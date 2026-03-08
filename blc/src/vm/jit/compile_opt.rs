//! SRA (Scalar Replacement of Aggregates) and CoW (Clone-on-Write) optimizations.
//!
//! Methods on `FnCompileCtx` for detecting and compiling SRA-eligible record
//! bindings and CoW enum field updates.

use std::collections::HashMap;

use cranelift_codegen::ir::{BlockArg, InstBuilder, types};
use cranelift_frontend::Variable;
use cranelift_module::Module;

use super::super::ir::{Expr, Pattern};
use super::compile::{CValue, FnCompileCtx};

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
        match value.as_ref() {
            // SRA alias: let x = y where y is an SRA record — just alias the entries
            Expr::Var(src_name, _) if self.sra_records.contains_key(src_name.as_str()) => {
                let src_fields = self.sra_records.get(src_name).cloned().unwrap();
                self.sra_records.insert(name.clone(), src_fields);
                self.vars.remove(name);
                Ok(true)
            }
            Expr::MakeRecord(fields, _) | Expr::MakeStruct { fields, .. } => {
                let mut field_vars = HashMap::new();
                for (fname, fexpr) in fields {
                    let val = self.compile_expr(fexpr)?;
                    let var = self.new_var();
                    self.builder.def_var(var, val);
                    field_vars.insert(fname.clone(), var);
                }
                self.sra_records.insert(name.clone(), field_vars);
                // Remove vars entry so Var references use SRA materialization
                self.vars.remove(name);
                Ok(true)
            }
            Expr::UpdateRecord { base, updates, .. } => {
                // SRA UpdateRecord: copy all field vars from the base SRA
                // record, then override fields with the updated values.
                let base_name = if let Expr::Var(n, _) = base.as_ref() {
                    n.clone()
                } else {
                    return Ok(false);
                };
                let Some(base_fields) = self.sra_records.get(&base_name).cloned() else {
                    return Ok(false);
                };
                // Start with copies of base field vars
                let mut field_vars = HashMap::new();
                for (fname, &fvar) in &base_fields {
                    let val = self.builder.use_var(fvar);
                    let var = self.new_var();
                    self.builder.def_var(var, val);
                    field_vars.insert(fname.clone(), var);
                }
                // Override with updated fields
                for (fname, fexpr) in updates {
                    let val = self.compile_expr(fexpr)?;
                    let var = self.new_var();
                    self.builder.def_var(var, val);
                    field_vars.insert(fname.clone(), var);
                }
                self.sra_records.insert(name.clone(), field_vars);
                // Remove stale vars entry so Var references use SRA materialization
                self.vars.remove(name);
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    // -- SRA-aware tail call --

    /// Compile a self-recursive tail call with SRA: for params that have
    /// param_sra_original entries, directly rebind field variables instead
    /// of materializing boxed records. Non-SRA params are handled normally.
    pub(super) fn compile_sra_tail_call(
        &mut self,
        args: &[Expr],
        loop_header: cranelift_codegen::ir::Block,
    ) -> Result<(), String> {
        let param_vars = self.param_vars.clone();
        let param_sra = self.param_sra_original.clone();
        let ir_funcs = self.ir_functions;
        let func_name = &self.current_func_name;

        // Find the current function's param names
        let func_params: Vec<String> = ir_funcs
            .iter()
            .find(|f| f.name == *func_name)
            .map(|f| f.params.clone())
            .unwrap_or_default();

        // Phase 1: Compile all arg values BEFORE rebinding any variables.
        // For SRA args, read current SRA field values. For non-SRA args, compile normally.
        // Stash everything in temporary variables to avoid ordering issues.
        struct ArgInfo {
            is_sra: bool,
            // For SRA: field_name → stashed value
            field_vals: Vec<(String, CValue)>,
            // For non-SRA: single stashed value
            scalar_val: Option<CValue>,
        }

        let mut arg_infos: Vec<ArgInfo> = Vec::with_capacity(args.len());

        for (i, arg) in args.iter().enumerate() {
            let param_name = func_params.get(i).map(|s| s.as_str()).unwrap_or("");

            if let Some(orig_fields) = param_sra.get(param_name) {
                // This param is SRA'd. Check if the arg is also an SRA record.
                if let Expr::Var(arg_name, _) = arg {
                    if let Some(arg_sra) = self.sra_records.get(arg_name.as_str()).cloned() {
                        // Both param and arg are SRA: read field values
                        let mut field_vals = Vec::with_capacity(orig_fields.len());
                        for (fname, _orig_var) in orig_fields {
                            if let Some(&arg_fvar) = arg_sra.get(fname) {
                                let val = self.builder.use_var(arg_fvar);
                                let stashed = self.stash_in_var(val);
                                field_vals.push((fname.clone(), stashed));
                            }
                        }
                        arg_infos.push(ArgInfo {
                            is_sra: true,
                            field_vals,
                            scalar_val: None,
                        });
                        continue;
                    }
                }

                // Arg is not SRA — compile to boxed NValue, then extract fields
                let boxed_val = self.compile_expr(arg)?;
                let boxed_stashed = self.stash_in_var(boxed_val);
                let mut field_vals = Vec::with_capacity(orig_fields.len());

                // Sort fields alphabetically (same order as record storage)
                let mut sorted_fields: Vec<&String> = orig_fields.keys().collect();
                sorted_fields.sort();

                for (field_idx, fname) in sorted_fields.iter().enumerate() {
                    let idx_val = self
                        .builder
                        .ins()
                        .iconst(types::I64, field_idx as i64);
                    let field_val =
                        self.call_helper("jit_get_field_idx", &[boxed_stashed, idx_val]);
                    let stashed = self.stash_in_var(field_val);
                    field_vals.push(((*fname).clone(), stashed));
                }

                // Decref the boxed value (we've extracted what we need)
                if self.rc_enabled {
                    self.emit_decref(boxed_stashed);
                }

                arg_infos.push(ArgInfo {
                    is_sra: true,
                    field_vals,
                    scalar_val: None,
                });
            } else {
                // Non-SRA param: compile arg normally
                let val = self.compile_expr(arg)?;
                let stashed = self.stash_in_var(val);
                arg_infos.push(ArgInfo {
                    is_sra: false,
                    field_vals: Vec::new(),
                    scalar_val: Some(stashed),
                });
            }
        }

        // Phase 2: Rebind all variables. RC: decref old non-SRA param values first.
        if self.rc_enabled {
            for (i, info) in arg_infos.iter().enumerate() {
                if !info.is_sra {
                    if let Some(&pv) = param_vars.get(i) {
                        let old_val = self.builder.use_var(pv);
                        self.emit_decref(old_val);
                    }
                }
            }
        }

        // Phase 3: Write new values
        for (i, info) in arg_infos.iter().enumerate() {
            let param_name = func_params.get(i).map(|s| s.as_str()).unwrap_or("");

            if info.is_sra {
                // Rebind param SRA field vars
                if let Some(orig_fields) = param_sra.get(param_name) {
                    for (fname, stashed_val) in &info.field_vals {
                        if let Some(&orig_var) = orig_fields.get(fname) {
                            self.builder.def_var(orig_var, *stashed_val);
                        }
                    }
                }
                // Also update sra_records to point to the original vars
                // (in case any code between here and jump reads them)
                if let Some(orig_fields) = param_sra.get(param_name) {
                    self.sra_records
                        .insert(param_name.to_string(), orig_fields.clone());
                }
            } else if let Some(val) = info.scalar_val {
                // Rebind non-SRA param var
                if let Some(&pv) = param_vars.get(i) {
                    self.builder.def_var(pv, val);
                }
            }
        }

        self.builder.ins().jump(loop_header, &[]);

        let dead_block = self.builder.create_block();
        self.builder.switch_to_block(dead_block);
        self.builder.seal_block(dead_block);

        Ok(())
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
            if let Expr::Var(name, _) = field
                && name == binding
            {
                continue; // pass-through field
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
                if let Expr::MakeTuple(fields, _) = payload.as_ref()
                    && let Some((field_idx, new_value)) =
                        Self::detect_single_field_update(fields, bindings)
                {
                    let idx_val = self.builder.ins().iconst(types::I64, field_idx as i64);

                    // Null out the updated field to reduce its refcount
                    let subj = self.builder.use_var(subj_var);
                    self.call_helper_void("jit_enum_field_drop", &[subj, idx_val]);

                    // Compile the new value (pattern bindings now have refcount 1)
                    let new_val = self.compile_expr(new_value)?;

                    // In-place field update (or clone if shared)
                    let subj = self.builder.use_var(subj_var);
                    let result = self.call_helper("jit_enum_field_set", &[subj, idx_val, new_val]);
                    return Ok(Some(result));
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
    pub(super) fn find_sra_candidates_with_existing(
        exprs: &[Expr],
        existing_sra: &HashMap<String, HashMap<String, Variable>>,
        self_fn_name: Option<&str>,
    ) -> Vec<(String, Vec<String>)> {
        // Single pass: collect all variable names that appear in escaping positions
        let mut escaping = std::collections::HashSet::new();
        for expr in exprs {
            Self::collect_escaping_vars_ex(expr, &mut escaping, self_fn_name);
        }

        let mut candidates = Vec::new();
        // Track which names are SRA candidates (for chained UpdateRecord detection)
        let mut candidate_names = std::collections::HashSet::new();
        for expr in exprs {
            if let Expr::Let { pattern, value, .. } = expr
                && let Pattern::Var(name) = pattern.as_ref()
                && !escaping.contains(name.as_str())
            {
                match value.as_ref() {
                    Expr::MakeRecord(fields, _) => {
                        let field_names: Vec<String> =
                            fields.iter().map(|(k, _)| k.clone()).collect();
                        candidate_names.insert(name.clone());
                        candidates.push((name.clone(), field_names));
                    }
                    Expr::MakeStruct { fields, .. } => {
                        let field_names: Vec<String> =
                            fields.iter().map(|(k, _)| k.clone()).collect();
                        candidate_names.insert(name.clone());
                        candidates.push((name.clone(), field_names));
                    }
                    Expr::UpdateRecord { base, updates, .. } => {
                        // Chained SRA: { ..base, field: val } where base is
                        // an SRA candidate or a param SRA record. We inherit
                        // the base's field names and add/replace update fields.
                        if let Expr::Var(base_name, _) = base.as_ref() {
                            // Try block-local candidates first, then param SRA
                            let base_fields = if candidate_names.contains(base_name) {
                                candidates
                                    .iter()
                                    .find(|(n, _)| n == base_name)
                                    .map(|(_, f)| f.clone())
                            } else if let Some(param_fields) = existing_sra.get(base_name) {
                                Some(param_fields.keys().cloned().collect())
                            } else {
                                None
                            };
                            if let Some(mut field_names) = base_fields {
                                for (fname, _) in updates {
                                    if !field_names.contains(fname) {
                                        field_names.push(fname.clone());
                                    }
                                }
                                candidate_names.insert(name.clone());
                                candidates.push((name.clone(), field_names));
                            }
                        }
                    }
                    // Var alias: let x = y where y is an SRA candidate or param SRA
                    Expr::Var(src_name, _) => {
                        let src_fields = if candidate_names.contains(src_name) {
                            candidates
                                .iter()
                                .find(|(n, _)| n == src_name)
                                .map(|(_, f)| f.clone())
                        } else if let Some(param_fields) = existing_sra.get(src_name) {
                            Some(param_fields.keys().cloned().collect())
                        } else {
                            None
                        };
                        if let Some(field_names) = src_fields {
                            candidate_names.insert(name.clone());
                            candidates.push((name.clone(), field_names));
                        }
                    }
                    _ => {}
                }
            }
        }
        candidates
    }

    /// Collect variable names that appear in escaping positions (anything other
    /// than GetField base). A single walk of the expression tree.
    fn collect_escaping_vars_ex<'c>(
        expr: &'c Expr,
        escaping: &mut std::collections::HashSet<&'c str>,
        self_fn_name: Option<&str>,
    ) {
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
                    Self::collect_escaping_vars_ex(object, escaping, self_fn_name);
                }
            }
            Expr::UpdateRecord { base, updates, .. } => {
                if let Expr::Var(_, _) = base.as_ref() {
                    // Safe use: { ..record, field: val } — do NOT mark as escaping
                } else {
                    Self::collect_escaping_vars_ex(base, escaping, self_fn_name);
                }
                for (_, v) in updates {
                    Self::collect_escaping_vars_ex(v, escaping, self_fn_name);
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
                Self::collect_escaping_vars_ex(lhs, escaping, self_fn_name);
                Self::collect_escaping_vars_ex(rhs, escaping, self_fn_name);
            }
            Expr::UnaryOp { operand, .. } => Self::collect_escaping_vars_ex(operand, escaping, self_fn_name),
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                Self::collect_escaping_vars_ex(condition, escaping, self_fn_name);
                Self::collect_escaping_vars_ex(then_branch, escaping, self_fn_name);
                if let Some(e) = else_branch {
                    Self::collect_escaping_vars_ex(e, escaping, self_fn_name);
                }
            }
            Expr::Block(exprs, _) | Expr::MakeList(exprs, _) | Expr::MakeTuple(exprs, _) => {
                for e in exprs {
                    Self::collect_escaping_vars_ex(e, escaping, self_fn_name);
                }
            }
            Expr::Concat(parts) => {
                for p in parts {
                    Self::collect_escaping_vars_ex(p, escaping, self_fn_name);
                }
            }
            Expr::Let { value, .. } | Expr::Assign { value, .. } | Expr::FieldAssign { value, .. } => Self::collect_escaping_vars_ex(value, escaping, self_fn_name),
            // Self-recursive tail calls: bare Var args don't escape because
            // the SRA tail call can read their fields directly.
            Expr::TailCall { name, args, .. } if self_fn_name == Some(name.as_str()) => {
                for a in args {
                    if let Expr::Var(_, _) = a {
                        // Non-escaping: SRA tail call handles this
                    } else {
                        Self::collect_escaping_vars_ex(a, escaping, self_fn_name);
                    }
                }
            }
            Expr::CallDirect { args, .. }
            | Expr::CallNative { args, .. }
            | Expr::TailCall { args, .. } => {
                for a in args {
                    Self::collect_escaping_vars_ex(a, escaping, self_fn_name);
                }
            }
            Expr::CallIndirect { callee, args, .. }
            | Expr::TailCallIndirect { callee, args, .. } => {
                Self::collect_escaping_vars_ex(callee, escaping, self_fn_name);
                for a in args {
                    Self::collect_escaping_vars_ex(a, escaping, self_fn_name);
                }
            }
            Expr::Match { subject, arms, .. } => {
                Self::collect_escaping_vars_ex(subject, escaping, self_fn_name);
                for arm in arms {
                    Self::collect_escaping_vars_ex(&arm.body, escaping, self_fn_name);
                }
            }
            Expr::MakeEnum { payload, .. } => Self::collect_escaping_vars_ex(payload, escaping, self_fn_name),
            Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => {
                for (_, v) in fields {
                    Self::collect_escaping_vars_ex(v, escaping, self_fn_name);
                }
            }
            Expr::MakeRange(a, b) => {
                Self::collect_escaping_vars_ex(a, escaping, self_fn_name);
                Self::collect_escaping_vars_ex(b, escaping, self_fn_name);
            }
            Expr::For { iterable, body, .. } => {
                Self::collect_escaping_vars_ex(iterable, escaping, self_fn_name);
                Self::collect_escaping_vars_ex(body, escaping, self_fn_name);
            }
            Expr::Try { expr, .. } => Self::collect_escaping_vars_ex(expr, escaping, self_fn_name),
            Expr::MakeClosure { captures, .. } => {
                for c in captures {
                    Self::collect_escaping_vars_ex(c, escaping, self_fn_name);
                }
            }
            Expr::Lambda { body, .. } => Self::collect_escaping_vars_ex(body, escaping, self_fn_name),
            Expr::WithHandlers { handlers, body, .. } => {
                for (_, methods) in handlers {
                    for (_, h) in methods {
                        Self::collect_escaping_vars_ex(h, escaping, self_fn_name);
                    }
                }
                Self::collect_escaping_vars_ex(body, escaping, self_fn_name);
            }
            Expr::HandleEffect { body, clauses, .. } => {
                Self::collect_escaping_vars_ex(body, escaping, self_fn_name);
                for clause in clauses {
                    Self::collect_escaping_vars_ex(&clause.body, escaping, self_fn_name);
                }
            }
            Expr::PerformEffect { args, .. } => {
                for a in args {
                    Self::collect_escaping_vars_ex(a, escaping, self_fn_name);
                }
            }
            Expr::Expect { actual, .. } => Self::collect_escaping_vars_ex(actual, escaping, self_fn_name),
            Expr::Drop { body, .. } => Self::collect_escaping_vars_ex(body, escaping, self_fn_name),
            Expr::Reuse { alloc, .. } => Self::collect_escaping_vars_ex(alloc, escaping, self_fn_name),
        }
    }
}
