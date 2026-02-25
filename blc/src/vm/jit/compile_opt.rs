//! SRA (Scalar Replacement of Aggregates) and CoW (Clone-on-Write) optimizations.
//!
//! Methods on `FnCompileCtx` for detecting and compiling SRA-eligible record
//! bindings and CoW enum field updates.

use std::collections::HashMap;

use cranelift_codegen::ir::{BlockArg, InstBuilder, types};
use cranelift_frontend::Variable;
use cranelift_module::Module;

use super::compile::{CValue, FnCompileCtx};
use super::super::ir::{Expr, Pattern};

use super::helpers::NV_UNIT;

impl<'a, 'b, M: Module> FnCompileCtx<'a, 'b, M> {

    // -- SRA (Scalar Replacement of Aggregates) --

    /// Try to compile an expression as an SRA Let binding.
    /// Returns `true` if the expression was intercepted and compiled as scalar
    /// field variables (skipping the MakeRecord allocation), `false` otherwise.
    pub(super) fn try_compile_sra_let(
        &mut self,
        expr: &Expr,
        sra_names: &std::collections::HashSet<&str>,
    ) -> Result<bool, String> {
        let Expr::Let { pattern, value, .. } = expr else {
            return Ok(false);
        };
        let Pattern::Var(name) = pattern.as_ref() else {
            return Ok(false);
        };
        if !sra_names.contains(name.as_str()) {
            return Ok(false);
        }
        let Expr::MakeRecord(fields, _) = value.as_ref() else {
            return Ok(false);
        };
        let mut field_vars = HashMap::new();
        for (fname, fexpr) in fields {
            let val = self.compile_expr(fexpr)?;
            let var = self.new_var();
            self.builder.def_var(var, val);
            field_vars.insert(fname.clone(), var);
        }
        self.sra_records.insert(name.clone(), field_vars);
        Ok(true)
    }

    // -- Clone-on-Write enum optimization --

    /// Detect if fields represent a single-field-update pattern.
    /// Returns (field_index, new_value_expr) if exactly one field differs
    /// from the corresponding pattern binding.
    fn detect_single_field_update<'e>(
        fields: &'e [Expr],
        bindings: &[String],
    ) -> Option<(usize, &'e Expr)> {
        if fields.len() != bindings.len() {
            return None;
        }
        let mut update_idx = None;
        for (i, (field, binding)) in fields.iter().zip(bindings.iter()).enumerate() {
            if let Expr::Var(name, _) = field {
                if name == binding {
                    continue; // pass-through field
                }
            }
            // This field is different
            if update_idx.is_some() {
                return None; // more than one changed field
            }
            update_idx = Some(i);
        }
        update_idx.map(|i| (i, &fields[i]))
    }

    /// Check if an expression can be optimized as an enum field update
    /// (without generating code). Used to decide whether to enter the
    /// optimization path for if/else trees.
    fn can_enum_update(expr: &Expr, tag: &str, bindings: &[String]) -> bool {
        match expr {
            Expr::MakeEnum {
                tag: t, payload, ..
            } if t == tag => {
                if let Expr::MakeTuple(fields, _) = payload.as_ref() {
                    Self::detect_single_field_update(fields, bindings).is_some()
                } else {
                    false
                }
            }
            Expr::If {
                then_branch,
                else_branch,
                ..
            } => {
                Self::can_enum_update(then_branch, tag, bindings)
                    || else_branch
                        .as_ref()
                        .is_some_and(|e| Self::can_enum_update(e, tag, bindings))
            }
            _ => false,
        }
    }

    /// Try to compile an enum update body using CoW (clone-on-write).
    /// Returns Some(result_val) if optimized, None if not applicable.
    ///
    /// Pattern detected:
    ///   match subject { Tag(a, b, c) -> Tag(a, f(b), c) }
    /// Emits jit_enum_field_drop + jit_enum_field_set instead of full MakeEnum.
    pub(super) fn try_gen_enum_update(
        &mut self,
        body: &Expr,
        tag: &str,
        bindings: &[String],
        subj_var: Variable,
    ) -> Result<Option<CValue>, String> {
        match body {
            Expr::MakeEnum {
                tag: body_tag,
                payload,
                ..
            } if body_tag == tag => {
                if let Expr::MakeTuple(fields, _) = payload.as_ref() {
                    if let Some((field_idx, new_value)) =
                        Self::detect_single_field_update(fields, bindings)
                    {
                        let idx_val = self
                            .builder
                            .ins()
                            .iconst(types::I64, field_idx as i64);

                        // Null out the updated field to reduce its refcount
                        let subj = self.builder.use_var(subj_var);
                        self.call_helper_void("jit_enum_field_drop", &[subj, idx_val]);

                        // Compile the new value (pattern bindings now have refcount 1)
                        let new_val = self.compile_expr(new_value)?;

                        // In-place field update (or clone if shared)
                        let subj = self.builder.use_var(subj_var);
                        let result =
                            self.call_helper("jit_enum_field_set", &[subj, idx_val, new_val]);
                        return Ok(Some(result));
                    }
                }
                Ok(None)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                // Check if at least one branch is optimizable
                let then_opt = Self::can_enum_update(then_branch, tag, bindings);
                let else_opt = else_branch
                    .as_ref()
                    .is_some_and(|e| Self::can_enum_update(e, tag, bindings));
                if !then_opt && !else_opt {
                    return Ok(None);
                }

                let cond_val = self.compile_expr(condition)?;

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                self.builder.append_block_param(merge_block, types::I64);

                let cmp = self.is_truthy(cond_val);
                if self.rc_enabled {
                    self.emit_decref(cond_val);
                }
                self.builder
                    .ins()
                    .brif(cmp, then_block, &[], else_block, &[]);

                // Then branch
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_val = if then_opt {
                    if let Some(v) =
                        self.try_gen_enum_update(then_branch, tag, bindings, subj_var)?
                    {
                        v
                    } else {
                        self.compile_expr(then_branch)?
                    }
                } else {
                    self.compile_expr(then_branch)?
                };
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(then_val)]);

                // Else branch
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_val = if let Some(else_expr) = else_branch {
                    if else_opt {
                        if let Some(v) =
                            self.try_gen_enum_update(else_expr, tag, bindings, subj_var)?
                        {
                            v
                        } else {
                            self.compile_expr(else_expr)?
                        }
                    } else {
                        self.compile_expr(else_expr)?
                    }
                } else {
                    self.builder.ins().iconst(types::I64, NV_UNIT as i64)
                };
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(else_val)]);

                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(Some(self.builder.block_params(merge_block)[0]))
            }
            _ => Ok(None),
        }
    }

    /// Find SRA candidates in a block: Let bindings to MakeRecord where the
    /// variable never escapes (only used in GetField positions).
    ///
    /// Uses a single pass to collect all escaping variable names, then filters
    /// candidates by checking set membership. O(N) instead of O(N²).
    pub(super) fn find_sra_candidates(exprs: &[Expr]) -> Vec<(String, Vec<String>)> {
        // Single pass: collect all variable names that appear in escaping positions
        let mut escaping = std::collections::HashSet::new();
        for expr in exprs {
            Self::collect_escaping_vars(expr, &mut escaping);
        }

        let mut candidates = Vec::new();
        for expr in exprs {
            if let Expr::Let { pattern, value, .. } = expr {
                if let Pattern::Var(name) = pattern.as_ref() {
                    if let Expr::MakeRecord(fields, _) = value.as_ref() {
                        if !escaping.contains(name.as_str()) {
                            let field_names: Vec<String> =
                                fields.iter().map(|(k, _)| k.clone()).collect();
                            candidates.push((name.clone(), field_names));
                        }
                    }
                }
            }
        }
        candidates
    }

    /// Collect variable names that appear in escaping positions (anything other
    /// than GetField base). A single walk of the expression tree.
    fn collect_escaping_vars<'c>(expr: &'c Expr, escaping: &mut std::collections::HashSet<&'c str>) {
        match expr {
            Expr::Var(name, _) => {
                // Bare variable reference = escapes
                escaping.insert(name.as_str());
            }
            Expr::GetField { object, .. } => {
                // GetField on a bare var is a safe (non-escaping) use — skip it.
                // But recurse into nested expressions.
                if let Expr::Var(_, _) = object.as_ref() {
                    // Safe use: record.field — do NOT mark as escaping
                } else {
                    Self::collect_escaping_vars(object, escaping);
                }
            }
            Expr::UpdateRecord { base, updates, .. } => {
                // UpdateRecord base always escapes (immutability — see 1A fix)
                Self::collect_escaping_vars(base, escaping);
                for (_, v) in updates {
                    Self::collect_escaping_vars(v, escaping);
                }
            }
            // Recurse into all children
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::String(_)
            | Expr::Bool(_)
            | Expr::Unit
            | Expr::Hole
            | Expr::GetClosureVar(_) => {}
            Expr::BinOp { lhs, rhs, .. } | Expr::And(lhs, rhs) | Expr::Or(lhs, rhs) => {
                Self::collect_escaping_vars(lhs, escaping);
                Self::collect_escaping_vars(rhs, escaping);
            }
            Expr::UnaryOp { operand, .. } => Self::collect_escaping_vars(operand, escaping),
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                Self::collect_escaping_vars(condition, escaping);
                Self::collect_escaping_vars(then_branch, escaping);
                if let Some(e) = else_branch {
                    Self::collect_escaping_vars(e, escaping);
                }
            }
            Expr::Block(exprs, _) | Expr::MakeList(exprs, _) | Expr::MakeTuple(exprs, _) => {
                for e in exprs {
                    Self::collect_escaping_vars(e, escaping);
                }
            }
            Expr::Concat(parts) => {
                for p in parts {
                    Self::collect_escaping_vars(p, escaping);
                }
            }
            Expr::Let { value, .. } => Self::collect_escaping_vars(value, escaping),
            Expr::CallDirect { args, .. }
            | Expr::CallNative { args, .. }
            | Expr::TailCall { args, .. } => {
                for a in args {
                    Self::collect_escaping_vars(a, escaping);
                }
            }
            Expr::CallIndirect { callee, args, .. }
            | Expr::TailCallIndirect { callee, args, .. } => {
                Self::collect_escaping_vars(callee, escaping);
                for a in args {
                    Self::collect_escaping_vars(a, escaping);
                }
            }
            Expr::Match { subject, arms, .. } => {
                Self::collect_escaping_vars(subject, escaping);
                for arm in arms {
                    Self::collect_escaping_vars(&arm.body, escaping);
                }
            }
            Expr::MakeEnum { payload, .. } => Self::collect_escaping_vars(payload, escaping),
            Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => {
                for (_, v) in fields {
                    Self::collect_escaping_vars(v, escaping);
                }
            }
            Expr::MakeRange(a, b) => {
                Self::collect_escaping_vars(a, escaping);
                Self::collect_escaping_vars(b, escaping);
            }
            Expr::For { iterable, body, .. } => {
                Self::collect_escaping_vars(iterable, escaping);
                Self::collect_escaping_vars(body, escaping);
            }
            Expr::Try { expr, .. } => Self::collect_escaping_vars(expr, escaping),
            Expr::MakeClosure { captures, .. } => {
                for c in captures {
                    Self::collect_escaping_vars(c, escaping);
                }
            }
            Expr::Lambda { body, .. } => Self::collect_escaping_vars(body, escaping),
            Expr::WithHandlers { handlers, body, .. } => {
                for (_, methods) in handlers {
                    for (_, h) in methods {
                        Self::collect_escaping_vars(h, escaping);
                    }
                }
                Self::collect_escaping_vars(body, escaping);
            }
            Expr::HandleEffect { body, clauses, .. } => {
                Self::collect_escaping_vars(body, escaping);
                for clause in clauses {
                    Self::collect_escaping_vars(&clause.body, escaping);
                }
            }
            Expr::PerformEffect { args, .. } => {
                for a in args {
                    Self::collect_escaping_vars(a, escaping);
                }
            }
            Expr::Expect { actual, .. } => Self::collect_escaping_vars(actual, escaping),
        }
    }
}
