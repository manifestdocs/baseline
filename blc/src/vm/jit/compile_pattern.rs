//! Match/pattern compilation, for loops, and try expressions.
//!
//! Methods on `FnCompileCtx` for compiling match expressions, pattern tests,
//! variable binding, for loops, and try expressions.

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{BlockArg, InstBuilder, types};
use cranelift_frontend::Switch;
use cranelift_module::Module;

use super::compile::{CValue, FnCompileCtx};
use super::super::ir::{Expr, MatchArm, Pattern};
use super::super::nvalue::NValue;
use super::helpers::{NV_TRUE, NV_UNIT};

impl<'a, 'b, M: Module> FnCompileCtx<'a, 'b, M> {
    // -- Match compilation --

    pub(super) fn compile_match(&mut self, subject: &Expr, arms: &[MatchArm]) -> Result<CValue, String> {
        let subject_val = self.compile_expr(subject)?;

        if arms.is_empty() {
            return Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64));
        }

        // Check if this match is eligible for Switch dispatch
        if self.is_switch_eligible(arms) {
            return self.compile_match_switch(subject_val, arms);
        }

        self.compile_match_linear(subject_val, arms)
    }

    /// Check if a match can use Cranelift Switch dispatch.
    /// Eligible when all arms are Constructor patterns (with optional trailing Wildcard/Var),
    /// and all constructor tags have known integer IDs.
    fn is_switch_eligible(&self, arms: &[MatchArm]) -> bool {
        if arms.is_empty() {
            return false;
        }
        // Guards require fallthrough to next arm on failure, incompatible with switch
        if arms.iter().any(|arm| arm.guard.is_some()) {
            return false;
        }
        let mut has_constructor = false;
        for (i, arm) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;
            match &arm.pattern {
                Pattern::Constructor(tag, _) => {
                    if self.tags.get_id(tag).is_none() {
                        return false;
                    }
                    has_constructor = true;
                }
                Pattern::Wildcard | Pattern::Var(_) => {
                    // Only allowed as the last arm (default case)
                    if !is_last {
                        return false;
                    }
                }
                _ => return false, // Literal or Tuple — not switch-eligible
            }
        }
        has_constructor
    }

    /// Compile a match using Cranelift Switch (jump table / binary search).
    fn compile_match_switch(
        &mut self,
        subject_val: CValue,
        arms: &[MatchArm],
    ) -> Result<CValue, String> {
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        // Store subject in a variable for use across blocks
        let subj_var = self.new_var();
        self.builder.def_var(subj_var, subject_val);

        // Extract tag_id once via helper call
        let subj = self.builder.use_var(subj_var);
        let tag_id_val = self.call_helper("jit_enum_tag_id", &[subj]);

        // Build Switch + per-arm blocks
        let mut switch = Switch::new();
        let mut arm_blocks = Vec::new();
        let mut default_arm: Option<usize> = None;

        for (i, arm) in arms.iter().enumerate() {
            match &arm.pattern {
                Pattern::Constructor(tag, _) => {
                    let id = self.tags.get_id(tag).unwrap(); // checked in is_switch_eligible
                    let block = self.builder.create_block();
                    switch.set_entry(id as u128, block);
                    arm_blocks.push((i, block));
                }
                Pattern::Wildcard | Pattern::Var(_) => {
                    default_arm = Some(i);
                }
                _ => unreachable!("non-eligible pattern in switch"),
            }
        }

        // Default block: handles wildcard/var or returns unit
        let default_block = self.builder.create_block();
        switch.emit(self.builder, tag_id_val, default_block);

        // Compile each constructor arm body
        for (arm_idx, block) in &arm_blocks {
            self.builder.switch_to_block(*block);
            self.builder.seal_block(*block);
            let subj_again = self.builder.use_var(subj_var);
            if self.rc_enabled {
                self.push_rc_scope();
                self.bind_pattern_vars_rc(&arms[*arm_idx].pattern, subj_again)?;

                // Try CoW optimization for constructor patterns
                let mut body_val =
                    if let Pattern::Constructor(tag, sub_pats) = &arms[*arm_idx].pattern {
                        let bindings: Vec<String> = sub_pats
                            .iter()
                            .map(|p| match p {
                                Pattern::Var(name) => name.clone(),
                                _ => String::new(),
                            })
                            .collect();
                        if let Some(val) = self.try_gen_enum_update(
                            &arms[*arm_idx].body,
                            tag,
                            &bindings,
                            subj_var,
                        )? {
                            // CoW consumed subject ownership — null out subj_var
                            // so the merge-block decref is a no-op.
                            let unit_val =
                                self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                            self.builder.def_var(subj_var, unit_val);
                            val
                        } else {
                            self.compile_expr(&arms[*arm_idx].body)?
                        }
                    } else {
                        self.compile_expr(&arms[*arm_idx].body)?
                    };

                let ret_var = self.new_var();
                self.builder.def_var(ret_var, body_val);
                self.pop_rc_scope(Some(ret_var));
                body_val = self.builder.use_var(ret_var);
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(body_val)]);
            } else {
                self.bind_pattern_vars(&arms[*arm_idx].pattern, subj_again)?;
                let body_val = self.compile_expr(&arms[*arm_idx].body)?;
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(body_val)]);
            }
        }

        // Compile default arm
        self.builder.switch_to_block(default_block);
        self.builder.seal_block(default_block);
        if let Some(idx) = default_arm {
            let subj_again = self.builder.use_var(subj_var);
            if self.rc_enabled {
                self.push_rc_scope();
                self.bind_pattern_vars_rc(&arms[idx].pattern, subj_again)?;
                let mut body_val = self.compile_expr(&arms[idx].body)?;
                let ret_var = self.new_var();
                self.builder.def_var(ret_var, body_val);
                self.pop_rc_scope(Some(ret_var));
                body_val = self.builder.use_var(ret_var);
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(body_val)]);
            } else {
                self.bind_pattern_vars(&arms[idx].pattern, subj_again)?;
                let body_val = self.compile_expr(&arms[idx].body)?;
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(body_val)]);
            }
        } else {
            let unit_val = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
            self.builder
                .ins()
                .jump(merge_block, &[BlockArg::Value(unit_val)]);
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        // RC: decref the subject after all arms have merged
        if self.rc_enabled {
            let subj = self.builder.use_var(subj_var);
            self.emit_decref(subj);
        }
        Ok(self.builder.block_params(merge_block)[0])
    }

    /// Linear cascade match (original implementation, used as fallback).
    fn compile_match_linear(
        &mut self,
        subject_val: CValue,
        arms: &[MatchArm],
    ) -> Result<CValue, String> {
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        // Store subject in a variable so we can use it across blocks
        let subj_var = self.new_var();
        self.builder.def_var(subj_var, subject_val);

        for (i, arm) in arms.iter().enumerate() {
            let is_last = i == arms.len() - 1;
            let body_block = self.builder.create_block();
            let next_test = if is_last {
                None
            } else {
                Some(self.builder.create_block())
            };

            let subj = self.builder.use_var(subj_var);
            self.compile_pattern_test(&arm.pattern, subj, body_block, next_test, merge_block)?;

            // Body block
            self.builder.switch_to_block(body_block);
            self.builder.seal_block(body_block);
            let subj_again = self.builder.use_var(subj_var);
            if self.rc_enabled {
                self.push_rc_scope();
                self.bind_pattern_vars_rc(&arm.pattern, subj_again)?;
                // Guard check: if guard fails, jump to next arm
                if let Some(ref guard) = arm.guard {
                    let guard_val = self.compile_expr(guard)?;
                    let true_val = self.builder.ins().iconst(types::I64, NV_TRUE as i64);
                    let cmp = self.builder.ins().icmp(IntCC::Equal, guard_val, true_val);
                    let guard_fail = self.builder.create_block();
                    let guard_pass = self.builder.create_block();
                    self.builder
                        .ins()
                        .brif(cmp, guard_pass, &[], guard_fail, &[]);
                    // Guard fail: clean up scope and fall through to next
                    self.builder.switch_to_block(guard_fail);
                    self.builder.seal_block(guard_fail);
                    self.pop_rc_scope(None);
                    if let Some(next) = next_test {
                        self.builder.ins().jump(next, &[]);
                    } else {
                        let unit = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                        self.builder
                            .ins()
                            .jump(merge_block, &[BlockArg::Value(unit)]);
                    }
                    self.builder.switch_to_block(guard_pass);
                    self.builder.seal_block(guard_pass);
                }
                let mut body_val = self.compile_expr(&arm.body)?;
                let ret_var = self.new_var();
                self.builder.def_var(ret_var, body_val);
                self.pop_rc_scope(Some(ret_var));
                body_val = self.builder.use_var(ret_var);
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(body_val)]);
            } else {
                self.bind_pattern_vars(&arm.pattern, subj_again)?;
                // Guard check: if guard fails, jump to next arm
                if let Some(ref guard) = arm.guard {
                    let guard_val = self.compile_expr(guard)?;
                    let true_val = self.builder.ins().iconst(types::I64, NV_TRUE as i64);
                    let cmp = self.builder.ins().icmp(IntCC::Equal, guard_val, true_val);
                    let guard_fail = self.builder.create_block();
                    let guard_pass = self.builder.create_block();
                    self.builder
                        .ins()
                        .brif(cmp, guard_pass, &[], guard_fail, &[]);
                    // Guard fail: fall through to next arm
                    self.builder.switch_to_block(guard_fail);
                    self.builder.seal_block(guard_fail);
                    if let Some(next) = next_test {
                        self.builder.ins().jump(next, &[]);
                    } else {
                        let unit = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                        self.builder
                            .ins()
                            .jump(merge_block, &[BlockArg::Value(unit)]);
                    }
                    self.builder.switch_to_block(guard_pass);
                    self.builder.seal_block(guard_pass);
                }
                let body_val = self.compile_expr(&arm.body)?;
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(body_val)]);
            }

            if let Some(next) = next_test {
                self.builder.switch_to_block(next);
                self.builder.seal_block(next);
            }
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        // RC: decref the subject after all arms have merged
        if self.rc_enabled {
            let subj = self.builder.use_var(subj_var);
            self.emit_decref(subj);
        }
        Ok(self.builder.block_params(merge_block)[0])
    }

    /// Emit a pattern test. On match, jump to body_block. On mismatch, jump to
    /// next_test (or merge with unit if last arm).
    fn compile_pattern_test(
        &mut self,
        pattern: &Pattern,
        subject: CValue,
        body_block: cranelift_codegen::ir::Block,
        next_test: Option<cranelift_codegen::ir::Block>,
        merge_block: cranelift_codegen::ir::Block,
    ) -> Result<(), String> {
        match pattern {
            Pattern::Wildcard | Pattern::Var(_) => {
                // Always matches
                self.builder.ins().jump(body_block, &[]);
            }
            Pattern::Literal(lit_expr) => {
                let lit_val = self.compile_expr(lit_expr)?;
                let eq_result = self.call_helper("jit_values_equal", &[subject, lit_val]);
                let cmp = self.is_truthy(eq_result);
                let fallthrough = next_test.unwrap_or(merge_block);
                let fallthrough_args: Vec<BlockArg> = if next_test.is_none() {
                    vec![BlockArg::Value(
                        self.builder.ins().iconst(types::I64, NV_UNIT as i64),
                    )]
                } else {
                    vec![]
                };
                self.builder
                    .ins()
                    .brif(cmp, body_block, &[], fallthrough, &fallthrough_args);
            }
            Pattern::Constructor(tag, sub_patterns) => {
                // Use integer tag_id comparison when available (faster than string compare)
                let cmp = if let Some(id) = self.tags.get_id(tag) {
                    let tag_id_val = self.call_helper("jit_enum_tag_id", &[subject]);
                    let expected = self.builder.ins().iconst(types::I64, id as i64);
                    self.builder.ins().icmp(IntCC::Equal, tag_id_val, expected)
                } else {
                    let tag_nv = NValue::string(tag.as_str().into());
                    let tag_val = self.emit_heap_nvalue(tag_nv);
                    let eq_result = self.call_helper("jit_enum_tag_eq", &[subject, tag_val]);
                    self.is_truthy(eq_result)
                };

                if sub_patterns.is_empty() {
                    let fallthrough = next_test.unwrap_or(merge_block);
                    let fallthrough_args: Vec<BlockArg> = if next_test.is_none() {
                        vec![BlockArg::Value(
                            self.builder.ins().iconst(types::I64, NV_UNIT as i64),
                        )]
                    } else {
                        vec![]
                    };
                    self.builder
                        .ins()
                        .brif(cmp, body_block, &[], fallthrough, &fallthrough_args);
                } else {
                    // Need to check sub-patterns on the payload
                    let check_payload = self.builder.create_block();
                    let fallthrough = next_test.unwrap_or(merge_block);
                    let fallthrough_args: Vec<BlockArg> = if next_test.is_none() {
                        vec![BlockArg::Value(
                            self.builder.ins().iconst(types::I64, NV_UNIT as i64),
                        )]
                    } else {
                        vec![]
                    };
                    self.builder.ins().brif(
                        cmp,
                        check_payload,
                        &[],
                        fallthrough,
                        &fallthrough_args,
                    );

                    self.builder.switch_to_block(check_payload);
                    self.builder.seal_block(check_payload);

                    // For single sub-pattern, extract payload directly
                    if sub_patterns.len() == 1 {
                        // Sub-pattern is applied to the enum payload
                        // Just jump to body — binding happens in bind_pattern_vars
                        self.builder.ins().jump(body_block, &[]);
                    } else {
                        // Multiple sub-patterns → payload must be a tuple
                        self.builder.ins().jump(body_block, &[]);
                    }
                }
            }
            Pattern::Tuple(sub_patterns) => {
                // Tuples always match structurally.
                // For sub-patterns that need checking (literals, constructors),
                // we extract each element and test recursively.
                let has_complex = sub_patterns
                    .iter()
                    .any(|p| !matches!(p, Pattern::Var(_) | Pattern::Wildcard));
                if has_complex {
                    // Check each complex sub-pattern; mismatch falls through
                    let check_block = self.builder.create_block();
                    self.builder.ins().jump(check_block, &[]);
                    self.builder.switch_to_block(check_block);
                    self.builder.seal_block(check_block);

                    for (i, sub) in sub_patterns.iter().enumerate() {
                        if matches!(sub, Pattern::Var(_) | Pattern::Wildcard) {
                            continue;
                        }
                        let idx = self.builder.ins().iconst(types::I64, i as i64);
                        let elem = self.call_helper("jit_tuple_get", &[subject, idx]);
                        // For literal sub-patterns, test equality
                        if let Pattern::Literal(lit_expr) = sub {
                            let lit_val = self.compile_expr(lit_expr)?;
                            let eq = self.call_helper("jit_values_equal", &[elem, lit_val]);
                            let cmp = self.is_truthy(eq);
                            let next_check = self.builder.create_block();
                            let fallthrough = next_test.unwrap_or(merge_block);
                            let fallthrough_args: Vec<BlockArg> = if next_test.is_none() {
                                vec![BlockArg::Value(
                                    self.builder.ins().iconst(types::I64, NV_UNIT as i64),
                                )]
                            } else {
                                vec![]
                            };
                            self.builder.ins().brif(
                                cmp,
                                next_check,
                                &[],
                                fallthrough,
                                &fallthrough_args,
                            );
                            self.builder.switch_to_block(next_check);
                            self.builder.seal_block(next_check);
                        }
                        // Constructor sub-patterns inside tuples: check tag
                        if let Pattern::Constructor(tag, _) = sub {
                            let cmp = if let Some(id) = self.tags.get_id(tag) {
                                let tag_id_val = self.call_helper("jit_enum_tag_id", &[elem]);
                                let expected = self.builder.ins().iconst(types::I64, id as i64);
                                self.builder.ins().icmp(IntCC::Equal, tag_id_val, expected)
                            } else {
                                let tag_nv = NValue::string(tag.as_str().into());
                                let tag_val = self.emit_heap_nvalue(tag_nv);
                                let eq_result =
                                    self.call_helper("jit_enum_tag_eq", &[elem, tag_val]);
                                self.is_truthy(eq_result)
                            };
                            let next_check = self.builder.create_block();
                            let fallthrough = next_test.unwrap_or(merge_block);
                            let fallthrough_args: Vec<BlockArg> = if next_test.is_none() {
                                vec![BlockArg::Value(
                                    self.builder.ins().iconst(types::I64, NV_UNIT as i64),
                                )]
                            } else {
                                vec![]
                            };
                            self.builder.ins().brif(
                                cmp,
                                next_check,
                                &[],
                                fallthrough,
                                &fallthrough_args,
                            );
                            self.builder.switch_to_block(next_check);
                            self.builder.seal_block(next_check);
                        }
                    }
                    self.builder.ins().jump(body_block, &[]);
                } else {
                    self.builder.ins().jump(body_block, &[]);
                }
            }
            Pattern::Record(fields) => {
                // Check if any field has a literal/complex sub-pattern
                let has_complex = fields
                    .iter()
                    .any(|(_, p)| !matches!(p, Pattern::Var(_) | Pattern::Wildcard));
                if has_complex {
                    let check_block = self.builder.create_block();
                    self.builder.ins().jump(check_block, &[]);
                    self.builder.switch_to_block(check_block);
                    self.builder.seal_block(check_block);

                    for (field_name, sub) in fields {
                        if matches!(sub, Pattern::Var(_) | Pattern::Wildcard) {
                            continue;
                        }
                        // Extract the field value
                        let field_nv = NValue::string(field_name.as_str().into());
                        let field_val = self.emit_heap_nvalue(field_nv);
                        let field_val = self.stash_in_var(field_val);
                        let elem = self.call_helper("jit_get_field", &[subject, field_val]);

                        // For literal sub-patterns, test equality
                        if let Pattern::Literal(lit_expr) = sub {
                            let lit_val = self.compile_expr(lit_expr)?;
                            let eq = self.call_helper("jit_values_equal", &[elem, lit_val]);
                            let cmp = self.is_truthy(eq);
                            let next_check = self.builder.create_block();
                            let fallthrough = next_test.unwrap_or(merge_block);
                            let fallthrough_args: Vec<BlockArg> = if next_test.is_none() {
                                vec![BlockArg::Value(
                                    self.builder.ins().iconst(types::I64, NV_UNIT as i64),
                                )]
                            } else {
                                vec![]
                            };
                            self.builder.ins().brif(
                                cmp,
                                next_check,
                                &[],
                                fallthrough,
                                &fallthrough_args,
                            );
                            self.builder.switch_to_block(next_check);
                            self.builder.seal_block(next_check);
                        }
                        // Constructor sub-patterns inside records: check tag
                        if let Pattern::Constructor(tag, _) = sub {
                            let cmp = if let Some(id) = self.tags.get_id(tag) {
                                let tag_id_val =
                                    self.call_helper("jit_enum_tag_id", &[elem]);
                                let expected =
                                    self.builder.ins().iconst(types::I64, id as i64);
                                self.builder.ins().icmp(IntCC::Equal, tag_id_val, expected)
                            } else {
                                let tag_nv = NValue::string(tag.as_str().into());
                                let tag_val = self.emit_heap_nvalue(tag_nv);
                                let eq_result =
                                    self.call_helper("jit_enum_tag_eq", &[elem, tag_val]);
                                self.is_truthy(eq_result)
                            };
                            let next_check = self.builder.create_block();
                            let fallthrough = next_test.unwrap_or(merge_block);
                            let fallthrough_args: Vec<BlockArg> = if next_test.is_none() {
                                vec![BlockArg::Value(
                                    self.builder.ins().iconst(types::I64, NV_UNIT as i64),
                                )]
                            } else {
                                vec![]
                            };
                            self.builder.ins().brif(
                                cmp,
                                next_check,
                                &[],
                                fallthrough,
                                &fallthrough_args,
                            );
                            self.builder.switch_to_block(next_check);
                            self.builder.seal_block(next_check);
                        }
                    }
                    self.builder.ins().jump(body_block, &[]);
                } else {
                    // All fields are simple Var/Wildcard bindings — always matches
                    self.builder.ins().jump(body_block, &[]);
                }
            }
            Pattern::List(elems, rest) => {
                // List pattern: check length >= required elements
                let len_nv = self.call_helper("jit_list_length", &[subject]);
                let len_raw = self.untag_int(len_nv);
                let required = self.builder.ins().iconst(types::I64, elems.len() as i64);

                // If rest binding: len >= required. If no rest: len == required.
                let cmp = if rest.is_some() {
                    self.builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, len_raw, required)
                } else {
                    self.builder.ins().icmp(IntCC::Equal, len_raw, required)
                };

                let check_elems = self.builder.create_block();
                let fallthrough = next_test.unwrap_or(merge_block);
                let fallthrough_args: Vec<BlockArg> = if next_test.is_none() {
                    vec![BlockArg::Value(
                        self.builder.ins().iconst(types::I64, NV_UNIT as i64),
                    )]
                } else {
                    vec![]
                };
                self.builder.ins().brif(
                    cmp,
                    check_elems,
                    &[],
                    fallthrough,
                    &fallthrough_args,
                );

                self.builder.switch_to_block(check_elems);
                self.builder.seal_block(check_elems);

                // Check each element sub-pattern that needs testing (literals, constructors)
                for (i, sub) in elems.iter().enumerate() {
                    if matches!(sub, Pattern::Var(_) | Pattern::Wildcard) {
                        continue;
                    }
                    let idx = self.builder.ins().iconst(types::I64, i as i64);
                    let idx_tagged = self.tag_int(idx);
                    let elem = self.call_helper("jit_list_get", &[subject, idx_tagged]);
                    if let Pattern::Literal(lit_expr) = sub {
                        let lit_val = self.compile_expr(lit_expr)?;
                        let eq = self.call_helper("jit_values_equal", &[elem, lit_val]);
                        let c = self.is_truthy(eq);
                        let next_check = self.builder.create_block();
                        let ft = next_test.unwrap_or(merge_block);
                        let ft_args: Vec<BlockArg> = if next_test.is_none() {
                            vec![BlockArg::Value(
                                self.builder.ins().iconst(types::I64, NV_UNIT as i64),
                            )]
                        } else {
                            vec![]
                        };
                        self.builder.ins().brif(c, next_check, &[], ft, &ft_args);
                        self.builder.switch_to_block(next_check);
                        self.builder.seal_block(next_check);
                    }
                }
                self.builder.ins().jump(body_block, &[]);
            }
        }
        Ok(())
    }

    /// Bind variables from a match pattern to the subject value.
    fn bind_pattern_vars(&mut self, pattern: &Pattern, subject: CValue) -> Result<(), String> {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Var(name) => {
                let var = self.new_var();
                self.builder.def_var(var, subject);
                self.vars.insert(name.clone(), var);
            }
            Pattern::Literal(_) => {}
            Pattern::Constructor(_, sub_patterns) => {
                if !sub_patterns.is_empty() {
                    // Extract fields directly from flat enum payload
                    for (i, sub) in sub_patterns.iter().enumerate() {
                        if matches!(sub, Pattern::Wildcard) {
                            continue; // skip extraction for wildcards — no binding needed
                        }
                        let idx = self.builder.ins().iconst(types::I64, i as i64);
                        let elem = self.call_helper("jit_enum_field_get", &[subject, idx]);
                        self.bind_pattern_vars(sub, elem)?;
                    }
                }
            }
            Pattern::Tuple(sub_patterns) => {
                for (i, sub) in sub_patterns.iter().enumerate() {
                    if matches!(sub, Pattern::Wildcard) {
                        continue; // skip extraction for wildcards — no binding needed
                    }
                    let idx = self.builder.ins().iconst(types::I64, i as i64);
                    let elem = self.call_helper("jit_tuple_get", &[subject, idx]);
                    self.bind_pattern_vars(sub, elem)?;
                }
            }
            Pattern::Record(fields) => {
                for (field_name, sub_pat) in fields {
                    let field_nv = NValue::string(field_name.as_str().into());
                    let field_val = self.emit_heap_nvalue(field_nv);
                    let elem = self.call_helper("jit_get_field", &[subject, field_val]);
                    self.bind_pattern_vars(sub_pat, elem)?;
                }
            }
            Pattern::List(elems, rest) => {
                // Bind each element pattern to list[i]
                for (i, sub) in elems.iter().enumerate() {
                    if matches!(sub, Pattern::Wildcard) {
                        continue;
                    }
                    let idx = self.builder.ins().iconst(types::I64, i as i64);
                    let idx_tagged = self.tag_int(idx);
                    let elem = self.call_helper("jit_list_get", &[subject, idx_tagged]);
                    self.bind_pattern_vars(sub, elem)?;
                }
                // Bind rest variable to tail
                if let Some(rest_name) = rest {
                    let start = self.builder.ins().iconst(types::I64, elems.len() as i64);
                    let tail = self.call_helper("jit_list_tail", &[subject, start]);
                    let var = self.new_var();
                    self.builder.def_var(var, tail);
                    self.vars.insert(rest_name.clone(), var);
                }
            }
        }
        Ok(())
    }

    /// Bind match-pattern variables with RC tracking.
    /// Like bind_pattern_vars but tracks bound variables in the current RC scope.
    /// Does NOT decref the subject — that's handled separately in the merge block.
    fn bind_pattern_vars_rc(&mut self, pattern: &Pattern, subject: CValue) -> Result<(), String> {
        if !self.rc_enabled {
            return self.bind_pattern_vars(pattern, subject);
        }
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Var(name) => {
                let var = self.new_var();
                self.builder.def_var(var, subject);
                self.vars.insert(name.clone(), var);
                self.rc_track_var(var);
            }
            Pattern::Literal(_) => {}
            Pattern::Constructor(_, sub_patterns) => {
                if !sub_patterns.is_empty() {
                    // Extract fields directly from flat enum payload
                    for (i, sub) in sub_patterns.iter().enumerate() {
                        if matches!(sub, Pattern::Wildcard) {
                            continue; // skip extraction for wildcards — avoids leaked owned clone
                        }
                        let idx = self.builder.ins().iconst(types::I64, i as i64);
                        let elem = self.call_helper("jit_enum_field_get", &[subject, idx]);
                        self.bind_pattern_vars_rc(sub, elem)?;
                    }
                }
            }
            Pattern::Tuple(sub_patterns) => {
                for (i, sub) in sub_patterns.iter().enumerate() {
                    if matches!(sub, Pattern::Wildcard) {
                        continue; // skip extraction for wildcards — avoids leaked owned clone
                    }
                    let idx = self.builder.ins().iconst(types::I64, i as i64);
                    let elem = self.call_helper("jit_tuple_get", &[subject, idx]);
                    self.bind_pattern_vars_rc(sub, elem)?;
                }
            }
            Pattern::Record(fields) => {
                for (field_name, sub_pat) in fields {
                    let field_nv = NValue::string(field_name.as_str().into());
                    let field_val = self.emit_heap_nvalue(field_nv);
                    let elem = self.call_helper("jit_get_field", &[subject, field_val]);
                    self.bind_pattern_vars_rc(sub_pat, elem)?;
                }
            }
            Pattern::List(elems, rest) => {
                // Bind each element pattern to list[i] with RC tracking
                for (i, sub) in elems.iter().enumerate() {
                    if matches!(sub, Pattern::Wildcard) {
                        continue;
                    }
                    let idx = self.builder.ins().iconst(types::I64, i as i64);
                    let idx_tagged = self.tag_int(idx);
                    let elem = self.call_helper("jit_list_get", &[subject, idx_tagged]);
                    self.bind_pattern_vars_rc(sub, elem)?;
                }
                // Bind rest variable to tail with RC tracking
                if let Some(rest_name) = rest {
                    let start = self.builder.ins().iconst(types::I64, elems.len() as i64);
                    let tail = self.call_helper("jit_list_tail", &[subject, start]);
                    let var = self.new_var();
                    self.builder.def_var(var, tail);
                    self.vars.insert(rest_name.clone(), var);
                    self.rc_track_var(var);
                }
            }
        }
        Ok(())
    }

    /// Bind a let-pattern (Var, Wildcard, Tuple, Constructor).
    fn bind_pattern(&mut self, pattern: &Pattern, val: CValue) -> Result<(), String> {
        match pattern {
            Pattern::Var(name) => {
                let var = self.new_var();
                self.builder.def_var(var, val);
                self.vars.insert(name.clone(), var);
            }
            Pattern::Wildcard => {}
            Pattern::Tuple(sub_patterns) => {
                for (i, sub) in sub_patterns.iter().enumerate() {
                    let idx = self.builder.ins().iconst(types::I64, i as i64);
                    let elem = self.call_helper("jit_tuple_get", &[val, idx]);
                    self.bind_pattern(sub, elem)?;
                }
            }
            Pattern::Constructor(_, sub_patterns) => {
                if !sub_patterns.is_empty() {
                    // Extract fields directly from flat enum payload
                    for (i, sub) in sub_patterns.iter().enumerate() {
                        let idx = self.builder.ins().iconst(types::I64, i as i64);
                        let elem = self.call_helper("jit_enum_field_get", &[val, idx]);
                        self.bind_pattern(sub, elem)?;
                    }
                }
            }
            Pattern::Record(fields) => {
                for (field_name, sub_pat) in fields {
                    let field_nv = NValue::string(field_name.as_str().into());
                    let field_val = self.emit_heap_nvalue(field_nv);
                    let elem = self.call_helper("jit_get_field", &[val, field_val]);
                    self.bind_pattern(sub_pat, elem)?;
                }
            }
            Pattern::List(elems, rest) => {
                for (i, sub) in elems.iter().enumerate() {
                    if matches!(sub, Pattern::Wildcard) {
                        continue;
                    }
                    let idx = self.builder.ins().iconst(types::I64, i as i64);
                    let idx_tagged = self.tag_int(idx);
                    let elem = self.call_helper("jit_list_get", &[val, idx_tagged]);
                    self.bind_pattern(sub, elem)?;
                }
                if let Some(rest_name) = rest {
                    let start = self.builder.ins().iconst(types::I64, elems.len() as i64);
                    let tail = self.call_helper("jit_list_tail", &[val, start]);
                    let var = self.new_var();
                    self.builder.def_var(var, tail);
                    self.vars.insert(rest_name.clone(), var);
                }
            }
            _ => return Err(format!("Unsupported pattern in let binding: {:?}", pattern)),
        }
        Ok(())
    }

    /// Bind a let-pattern with RC tracking: tracks bound variables in the
    /// current RC scope and decrefs wildcard/unused values.
    pub(super) fn bind_pattern_rc(&mut self, pattern: &Pattern, val: CValue) -> Result<(), String> {
        if !self.rc_enabled {
            return self.bind_pattern(pattern, val);
        }
        match pattern {
            Pattern::Var(name) => {
                let var = self.new_var();
                self.builder.def_var(var, val);
                self.vars.insert(name.clone(), var);
                self.rc_track_var(var);
            }
            Pattern::Wildcard => {
                // Value is unused — decref it
                self.emit_decref(val);
            }
            Pattern::Tuple(sub_patterns) => {
                for (i, sub) in sub_patterns.iter().enumerate() {
                    let idx = self.builder.ins().iconst(types::I64, i as i64);
                    let elem = self.call_helper("jit_tuple_get", &[val, idx]);
                    // jit_tuple_get increfs the element, so we own it
                    self.bind_pattern_rc(sub, elem)?;
                }
                // Decref the tuple container after extracting elements
                self.emit_decref(val);
            }
            Pattern::Constructor(_, sub_patterns) => {
                if sub_patterns.is_empty() {
                    // Enum with no payload — decref it
                    self.emit_decref(val);
                } else {
                    // Extract fields directly from flat enum payload
                    for (i, sub) in sub_patterns.iter().enumerate() {
                        let idx = self.builder.ins().iconst(types::I64, i as i64);
                        let elem = self.call_helper("jit_enum_field_get", &[val, idx]);
                        self.bind_pattern_rc(sub, elem)?;
                    }
                    // Decref the enum container
                    self.emit_decref(val);
                }
            }
            Pattern::Record(fields) => {
                for (field_name, sub_pat) in fields {
                    let field_nv = NValue::string(field_name.as_str().into());
                    let field_val = self.emit_heap_nvalue(field_nv);
                    let elem = self.call_helper("jit_get_field", &[val, field_val]);
                    self.bind_pattern_rc(sub_pat, elem)?;
                }
                self.emit_decref(val);
            }
            Pattern::List(elems, rest) => {
                for (i, sub) in elems.iter().enumerate() {
                    if matches!(sub, Pattern::Wildcard) {
                        continue;
                    }
                    let idx = self.builder.ins().iconst(types::I64, i as i64);
                    let idx_tagged = self.tag_int(idx);
                    let elem = self.call_helper("jit_list_get", &[val, idx_tagged]);
                    self.bind_pattern_rc(sub, elem)?;
                }
                if let Some(rest_name) = rest {
                    let start = self.builder.ins().iconst(types::I64, elems.len() as i64);
                    let tail = self.call_helper("jit_list_tail", &[val, start]);
                    let var = self.new_var();
                    self.builder.def_var(var, tail);
                    self.vars.insert(rest_name.clone(), var);
                    self.rc_track_var(var);
                }
                self.emit_decref(val);
            }
            _ => return Err(format!("Unsupported pattern in let binding: {:?}", pattern)),
        }
        Ok(())
    }

    // -- For loop compilation --

    pub(super) fn compile_for(
        &mut self,
        binding: &str,
        iterable: &Expr,
        body: &Expr,
    ) -> Result<CValue, String> {
        let list_val = self.compile_expr(iterable)?;

        // Get list length
        let len_nv = self.call_helper("jit_list_length", &[list_val]);
        let len = self.untag_int(len_nv);

        // Store list in a variable
        let list_var = self.new_var();
        self.builder.def_var(list_var, list_val);

        // Index variable starts at 0
        let idx_var = self.new_var();
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(idx_var, zero);

        // RC: initialize binding var to NV_UNIT in preheader (for decref on first iteration)
        let bind_var = self.new_var();
        let unit_val = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
        self.builder.def_var(bind_var, unit_val);
        self.vars.insert(binding.to_string(), bind_var);

        // Loop header
        let loop_head = self.builder.create_block();
        let loop_body = self.builder.create_block();
        let loop_exit = self.builder.create_block();

        self.builder.ins().jump(loop_head, &[]);
        self.builder.switch_to_block(loop_head);
        // Don't seal yet — back edge from loop body

        let idx = self.builder.use_var(idx_var);
        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, idx, len);
        self.builder.ins().brif(cmp, loop_body, &[], loop_exit, &[]);

        // Loop body
        self.builder.switch_to_block(loop_body);
        self.builder.seal_block(loop_body);

        // RC: decref previous iteration's binding value
        if self.rc_enabled {
            let old_bind = self.builder.use_var(bind_var);
            self.emit_decref(old_bind);
        }

        let list = self.builder.use_var(list_var);
        let idx_tagged = self.tag_int(idx);
        let elem = self.call_helper("jit_list_get", &[list, idx_tagged]);

        // Update binding variable with new element
        self.builder.def_var(bind_var, elem);

        // Compile body (result is discarded)
        let body_val = self.compile_expr(body)?;

        // RC: decref discarded body result
        if self.rc_enabled {
            self.emit_decref(body_val);
        }

        // Increment index
        let cur_idx = self.builder.use_var(idx_var);
        let one = self.builder.ins().iconst(types::I64, 1);
        let next_idx = self.builder.ins().iadd(cur_idx, one);
        self.builder.def_var(idx_var, next_idx);

        self.builder.ins().jump(loop_head, &[]);

        // Seal loop header now
        self.builder.seal_block(loop_head);

        // Exit
        self.builder.switch_to_block(loop_exit);
        self.builder.seal_block(loop_exit);

        // RC: decref the last iteration's binding and the list itself
        if self.rc_enabled {
            let last_bind = self.builder.use_var(bind_var);
            self.emit_decref(last_bind);
            let list_final = self.builder.use_var(list_var);
            self.emit_decref(list_final);
        }

        Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64))
    }

    // -- Try expression compilation --

    pub(super) fn compile_try(&mut self, inner: &Expr) -> Result<CValue, String> {
        let val = self.compile_expr(inner)?;

        // Check if the value is Err or None → early return
        let is_err_val = self.call_helper("jit_is_err", &[val]);
        let is_none_val = self.call_helper("jit_is_none", &[val]);

        // Combine: should_return = is_err || is_none
        let one = self.builder.ins().iconst(types::I64, 1);
        let err_bit = self.builder.ins().band(is_err_val, one);
        let none_bit = self.builder.ins().band(is_none_val, one);
        let should_return = self.builder.ins().bor(err_bit, none_bit);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self
            .builder
            .ins()
            .icmp(IntCC::NotEqual, should_return, zero);

        let unwrap_block = self.builder.create_block();
        let return_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(cmp, return_block, &[], unwrap_block, &[]);

        // Return block: propagate the Err/None value
        self.builder.switch_to_block(return_block);
        self.builder.seal_block(return_block);
        self.builder.ins().return_(&[val]);

        // Unwrap block: extract Ok/Some payload
        self.builder.switch_to_block(unwrap_block);
        self.builder.seal_block(unwrap_block);
        let payload = self.call_helper("jit_enum_payload", &[val]);
        // RC: jit_enum_payload borrows val and returns owned payload;
        // decref the container now that we've extracted the payload
        if self.rc_enabled {
            self.emit_decref(val);
        }
        Ok(payload)
    }
}
