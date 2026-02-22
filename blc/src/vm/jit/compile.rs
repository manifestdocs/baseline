//! Function compilation context and expression codegen.
//!
//! `FnCompileCtx` holds per-function state during JIT compilation and provides
//! methods for compiling IR expressions to Cranelift IR.

use std::collections::HashMap;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{AbiParam, BlockArg, InstBuilder, StackSlotData, StackSlotKind, types};
use cranelift_codegen::isa::CallConv;
use cranelift_frontend::{FunctionBuilder, Switch, Variable};
use cranelift_module::{DataId, FuncId, Module};

use super::super::ir::{BinOp, Expr, IrFunction, MatchArm, Pattern, TagRegistry, UnaryOp};
use super::super::natives::NativeRegistry;
use super::super::nvalue::{NValue, PAYLOAD_MASK, TAG_BOOL, TAG_INT};
use super::super::value::RcStr;
use super::analysis::expr_only_refs_params;
use super::helpers::{NV_FALSE, NV_TRUE, NV_UNIT};
use crate::analysis::types::Type;

// ---------------------------------------------------------------------------
// Function compile context
// ---------------------------------------------------------------------------

pub(super) struct FnCompileCtx<'a, 'b, M: Module> {
    pub(super) builder: &'a mut FunctionBuilder<'b>,
    pub(super) func_ids: &'a [Option<FuncId>],
    pub(super) module: &'a mut M,
    pub(super) vars: HashMap<String, Variable>,
    pub(super) next_var: u32,
    pub(super) func_names: &'a HashMap<String, usize>,
    pub(super) ir_functions: &'a [IrFunction],
    pub(super) current_func_name: String,
    pub(super) param_vars: Vec<Variable>,
    pub(super) loop_header: Option<cranelift_codegen::ir::Block>,
    pub(super) heap_roots: &'a mut Vec<NValue>,
    pub(super) helper_ids: &'a HashMap<&'a str, FuncId>,
    pub(super) natives: Option<&'a NativeRegistry>,
    pub(super) ptr_type: cranelift_codegen::ir::Type,
    /// Per-function unboxed flags (indexed by function index).
    pub(super) unboxed_flags: &'a [bool],
    /// Compile-time enum tag → integer ID mapping.
    pub(super) tags: &'a TagRegistry,
    /// SRA: record variables that have been scalar-replaced.
    /// Maps record variable name → (field name → Cranelift Variable).
    pub(super) sra_records: HashMap<String, HashMap<String, Variable>>,
    /// AOT string constants: maps string content → DataId in the object module.
    /// When Some, strings are loaded from global data instead of baked as heap pointers.
    pub(super) aot_strings: Option<&'a HashMap<String, DataId>>,
    /// AOT native function IDs: maps qualified name → FuncId for direct calls.
    /// When Some, CallNative emits direct calls instead of going through NativeRegistry.
    pub(super) aot_native_ids: Option<&'a HashMap<String, FuncId>>,
    /// Whether RC codegen is enabled (scope-based incref/decref).
    pub(super) rc_enabled: bool,
    /// Stack of scope frames tracking owned variables for RC cleanup.
    /// Each frame is a list of Variables whose values should be decref'd on scope exit.
    pub(super) rc_scope_stack: Vec<Vec<Variable>>,
    /// Calling convention for Baseline functions (Tail for JIT, Fast for AOT).
    pub(super) func_call_conv: CallConv,
}

pub(super) type CValue = cranelift_codegen::ir::Value;

impl<'a, 'b, M: Module> FnCompileCtx<'a, 'b, M> {
    pub(super) fn new_var(&mut self) -> Variable {
        let var = self.builder.declare_var(types::I64);
        self.next_var += 1;
        var
    }

    // -- RC scope helpers --

    /// Push a new RC scope frame. Variables tracked in this frame will be
    /// decref'd when the scope is popped.
    pub(super) fn push_rc_scope(&mut self) {
        if self.rc_enabled {
            self.rc_scope_stack.push(Vec::new());
        }
    }

    /// Track a variable as owned in the current RC scope.
    pub(super) fn rc_track_var(&mut self, var: Variable) {
        if self.rc_enabled {
            if let Some(frame) = self.rc_scope_stack.last_mut() {
                frame.push(var);
            }
        }
    }

    /// Pop an RC scope frame, emitting decref for all tracked variables
    /// except `keep` (the return value variable, if any).
    pub(super) fn pop_rc_scope(&mut self, keep: Option<Variable>) {
        if !self.rc_enabled {
            return;
        }
        if let Some(frame) = self.rc_scope_stack.pop() {
            for var in &frame {
                if keep == Some(*var) {
                    continue;
                }
                let val = self.builder.use_var(*var);
                self.emit_decref(val);
            }
        }
    }

    /// Emit an incref call. Returns the same value (for chaining).
    pub(super) fn emit_incref(&mut self, val: CValue) -> CValue {
        self.call_helper("jit_rc_incref", &[val])
    }

    /// Emit a decref call.
    fn emit_decref(&mut self, val: CValue) {
        self.call_helper_void("jit_rc_decref", &[val]);
    }

    /// Call a runtime helper that returns no useful value.
    fn call_helper_void(&mut self, name: &str, args: &[CValue]) {
        let func_id = self.helper_ids[name];
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        self.builder.ins().call(func_ref, args);
    }

    // -- NaN-boxing helpers --

    /// Emit a NaN-boxed NValue constant.
    fn emit_nvalue(&mut self, nv: NValue) -> CValue {
        if nv.is_heap() {
            // BigInt, or any other heap-allocated value: must keep rooted
            return self.emit_heap_nvalue(nv);
        }
        let bits = nv.raw() as i64;
        self.builder.ins().iconst(types::I64, bits)
    }

    /// Emit a NaN-boxed NValue for a heap object, keeping it rooted.
    fn emit_heap_nvalue(&mut self, nv: NValue) -> CValue {
        let bits = nv.raw() as i64;
        self.heap_roots.push(nv);
        let val = self.builder.ins().iconst(types::I64, bits);
        // RC: heap_roots owns one ref; incref so the function gets its own.
        if self.rc_enabled {
            self.emit_incref(val)
        } else {
            val
        }
    }

    /// Emit a string NValue: AOT uses global data + jit_make_string, JIT bakes the pointer.
    fn emit_string_value(&mut self, s: &str) -> Result<CValue, String> {
        if let Some(aot_strings) = self.aot_strings {
            let data_id = aot_strings
                .get(s)
                .ok_or_else(|| format!("String not in AOT data: {:?}", s))?;
            let gv = self
                .module
                .declare_data_in_func(*data_id, self.builder.func);
            let addr = self.builder.ins().global_value(self.ptr_type, gv);
            let len = self.builder.ins().iconst(types::I64, s.len() as i64);
            Ok(self.call_helper("jit_make_string", &[addr, len]))
        } else {
            let nv = NValue::string(s.into());
            Ok(self.emit_heap_nvalue(nv))
        }
    }

    /// Untag an Int: extract the 48-bit signed payload from NaN-boxed int.
    /// ishl 16, sshr 16
    fn untag_int(&mut self, val: CValue) -> CValue {
        let shifted_left = self.builder.ins().ishl_imm(val, 16);
        self.builder.ins().sshr_imm(shifted_left, 16)
    }

    /// Tag a raw i64 as a NaN-boxed Int: band with PAYLOAD_MASK, bor with TAG_INT.
    /// Only correct when the value is guaranteed to fit in 48-bit signed range.
    fn tag_int(&mut self, val: CValue) -> CValue {
        let mask = self.builder.ins().iconst(types::I64, PAYLOAD_MASK as i64);
        let masked = self.builder.ins().band(val, mask);
        let tag = self.builder.ins().iconst(types::I64, TAG_INT as i64);
        self.builder.ins().bor(masked, tag)
    }

    /// Tag an i64 arithmetic result with overflow checking.
    /// If the value fits in 48-bit signed range, tags inline (fast path).
    /// Otherwise calls jit_int_from_i64 to allocate a BigInt (slow path).
    fn tag_int_checked(&mut self, val: CValue) -> CValue {
        // Sign-extend from 48 bits and compare with original
        let sext = self.builder.ins().ishl_imm(val, 16);
        let sext = self.builder.ins().sshr_imm(sext, 16);
        let fits = self.builder.ins().icmp(IntCC::Equal, val, sext);

        let inline_block = self.builder.create_block();
        let overflow_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        self.builder
            .ins()
            .brif(fits, inline_block, &[], overflow_block, &[]);

        self.builder.switch_to_block(inline_block);
        self.builder.seal_block(inline_block);
        let fast = self.tag_int(val);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(fast)]);

        self.builder.switch_to_block(overflow_block);
        self.builder.seal_block(overflow_block);
        let slow = self.call_helper("jit_int_from_i64", &[val]);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(slow)]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        self.builder.block_params(merge_block)[0]
    }

    /// Tag a Cranelift i8 comparison result as a NaN-boxed Bool.
    fn tag_bool(&mut self, cmp: CValue) -> CValue {
        let extended = self.builder.ins().uextend(types::I64, cmp);
        let tag = self.builder.ins().iconst(types::I64, TAG_BOOL as i64);
        self.builder.ins().bor(extended, tag)
    }

    /// Check if a NaN-boxed value is truthy (bit 0 set, works for NaN-boxed bools).
    fn is_truthy(&mut self, val: CValue) -> CValue {
        let one = self.builder.ins().iconst(types::I64, 1);
        let bit = self.builder.ins().band(val, one);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().icmp(IntCC::NotEqual, bit, zero)
    }

    /// Spill values to a stack slot and return the address.
    fn spill_to_stack(&mut self, values: &[CValue]) -> CValue {
        let size = (values.len() * 8) as u32;
        let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            size,
            3, // 8-byte alignment
        ));
        for (i, &v) in values.iter().enumerate() {
            let offset = (i * 8) as i32;
            self.builder.ins().stack_store(v, slot, offset);
        }
        self.builder.ins().stack_addr(self.ptr_type, slot, 0)
    }

    /// Call a runtime helper function by name.
    fn call_helper(&mut self, name: &str, args: &[CValue]) -> CValue {
        let func_id = self.helper_ids[name];
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, args);
        self.builder.inst_results(call)[0]
    }

    // -- Base-case speculation --

    /// Check if an expression is simple enough to inline for base-case speculation.
    /// Recognizes scalars, variables, and binary/unary ops over simple operands
    /// (e.g., `a + b`, `n - 1`).
    fn is_simple_base_case(e: &Expr) -> bool {
        match e {
            Expr::Var(_, _) | Expr::Int(_) | Expr::Bool(_) | Expr::Unit => true,
            Expr::BinOp { lhs, rhs, .. } => {
                Self::is_simple_base_case(lhs) && Self::is_simple_base_case(rhs)
            }
            Expr::UnaryOp { operand, .. } => Self::is_simple_base_case(operand),
            _ => false,
        }
    }

    fn try_speculate_call(
        &mut self,
        name: &str,
        ir_args: &[Expr],
    ) -> Result<Option<CValue>, String> {
        let func_idx = match self.func_names.get(name) {
            Some(&idx) => idx,
            None => return Ok(None),
        };
        if self.func_ids[func_idx].is_none() {
            return Ok(None);
        }
        let func = &self.ir_functions[func_idx];

        // Look for base case in then_branch OR else_branch.
        // base_in_then: true = base case is then_branch (branch to base when cond is truthy)
        //               false = base case is else_branch (branch to base when cond is falsy)
        let (cond_expr, base_expr, base_in_then) = match &func.body {
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                if Self::is_simple_base_case(then_branch) {
                    (condition.as_ref(), then_branch.as_ref(), true)
                } else if let Some(else_br) = else_branch
                    && Self::is_simple_base_case(else_br)
                {
                    (condition.as_ref(), else_br.as_ref(), false)
                } else {
                    return Ok(None);
                }
            }
            _ => return Ok(None),
        };

        if !expr_only_refs_params(cond_expr, &func.params) {
            return Ok(None);
        }
        if !expr_only_refs_params(base_expr, &func.params) {
            return Ok(None);
        }

        let arg_vals: Vec<CValue> = ir_args
            .iter()
            .map(|a| self.compile_expr(a))
            .collect::<Result<_, _>>()?;

        let mut saved_vars = HashMap::new();
        for (i, param_name) in func.params.iter().enumerate() {
            let var = self.new_var();
            self.builder.def_var(var, arg_vals[i]);
            if let Some(old) = self.vars.insert(param_name.clone(), var) {
                saved_vars.insert(param_name.clone(), old);
            }
        }

        let cond_val = self.compile_expr(cond_expr)?;
        let base_val = self.compile_expr(base_expr)?;

        for param_name in &func.params {
            self.vars.remove(param_name);
        }
        for (name, old_var) in saved_vars {
            self.vars.insert(name, old_var);
        }

        let call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        let cmp = self.is_truthy(cond_val);
        if base_in_then {
            // Base case in then: branch to base when cond is true
            self.builder.ins().brif(
                cmp,
                merge_block,
                &[BlockArg::Value(base_val)],
                call_block,
                &[],
            );
        } else {
            // Base case in else: branch to base when cond is false
            self.builder.ins().brif(
                cmp,
                call_block,
                &[],
                merge_block,
                &[BlockArg::Value(base_val)],
            );
        }

        self.builder.switch_to_block(call_block);
        self.builder.seal_block(call_block);
        let func_id = self.func_ids[func_idx].unwrap();
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let callee_unboxed = func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
        let call_result = if callee_unboxed {
            // Callee expects raw i64: untag args, tag result
            let untagged_args: Vec<CValue> = arg_vals.iter().map(|&v| self.untag_int(v)).collect();
            let call = self.builder.ins().call(func_ref, &untagged_args);
            let raw = self.builder.inst_results(call)[0];
            self.tag_int_checked(raw)
        } else {
            let call = self.builder.ins().call(func_ref, &arg_vals);
            self.builder.inst_results(call)[0]
        };
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(call_result)]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(Some(self.builder.block_params(merge_block)[0]))
    }

    // -- Unboxed expression compiler (scalar-only fast path) --
    //
    // Values are raw i64: ints as plain signed integers, bools as 0/1.
    // No NaN-boxing overhead. Boxing only happens at function boundaries.

    pub(super) fn compile_expr_unboxed(&mut self, expr: &Expr) -> Result<CValue, String> {
        match expr {
            Expr::Int(n) => Ok(self.builder.ins().iconst(types::I64, *n)),

            Expr::Float(f) => {
                // Float in unboxed mode: store as raw bits
                Ok(self.builder.ins().iconst(types::I64, f.to_bits() as i64))
            }

            Expr::Bool(b) => Ok(self.builder.ins().iconst(types::I64, *b as i64)),

            Expr::Unit => Ok(self.builder.ins().iconst(types::I64, 0)),

            Expr::Var(name, _) => {
                if let Some(&var) = self.vars.get(name) {
                    Ok(self.builder.use_var(var))
                } else {
                    Err(format!("Unknown variable: {}", name))
                }
            }

            Expr::BinOp { op, lhs, rhs, .. } => {
                let l = self.compile_expr_unboxed(lhs)?;
                let r = self.compile_expr_unboxed(rhs)?;
                match op {
                    BinOp::Add => Ok(self.builder.ins().iadd(l, r)),
                    BinOp::Sub => Ok(self.builder.ins().isub(l, r)),
                    BinOp::Mul => Ok(self.builder.ins().imul(l, r)),
                    BinOp::Div => Ok(self.builder.ins().sdiv(l, r)),
                    BinOp::Mod => Ok(self.builder.ins().srem(l, r)),
                    BinOp::Eq => {
                        let cmp = self.builder.ins().icmp(IntCC::Equal, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Ne => {
                        let cmp = self.builder.ins().icmp(IntCC::NotEqual, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Lt => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Gt => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThan, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Le => {
                        let cmp = self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::Ge => {
                        let cmp = self
                            .builder
                            .ins()
                            .icmp(IntCC::SignedGreaterThanOrEqual, l, r);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                    BinOp::ListConcat => Err("ListConcat cannot be JIT-compiled".into()),
                }
            }

            Expr::UnaryOp { op, operand, .. } => {
                let val = self.compile_expr_unboxed(operand)?;
                match op {
                    UnaryOp::Neg => Ok(self.builder.ins().ineg(val)),
                    UnaryOp::Not => {
                        let zero = self.builder.ins().iconst(types::I64, 0);
                        let cmp = self.builder.ins().icmp(IntCC::Equal, val, zero);
                        Ok(self.builder.ins().uextend(types::I64, cmp))
                    }
                }
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_val = self.compile_expr_unboxed(condition)?;

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                self.builder.append_block_param(merge_block, types::I64);

                // In unboxed mode, truthy = != 0
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, cond_val, zero);
                self.builder
                    .ins()
                    .brif(cmp, then_block, &[], else_block, &[]);

                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_val = self.compile_expr_unboxed(then_branch)?;
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(then_val)]);

                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_val = if let Some(else_expr) = else_branch {
                    self.compile_expr_unboxed(else_expr)?
                } else {
                    self.builder.ins().iconst(types::I64, 0)
                };
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(else_val)]);

                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(self.builder.block_params(merge_block)[0])
            }

            Expr::Let { pattern, value, .. } => {
                let val = self.compile_expr_unboxed(value)?;
                // Only Var patterns in scalar-only functions
                if let Pattern::Var(name) = pattern.as_ref() {
                    let var = self.new_var();
                    self.builder.def_var(var, val);
                    self.vars.insert(name.clone(), var);
                }
                Ok(self.builder.ins().iconst(types::I64, 0))
            }

            Expr::Block(exprs, _) => {
                let mut result = self.builder.ins().iconst(types::I64, 0);
                for e in exprs {
                    result = self.compile_expr_unboxed(e)?;
                }
                Ok(result)
            }

            Expr::CallDirect { name, args, .. } => {
                // Try base-case speculation first (unboxed mode)
                if let Some(val) = self.try_speculate_call_unboxed(name, args)? {
                    return Ok(val);
                }

                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let callee_unboxed =
                            func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

                        if callee_unboxed {
                            // Unboxed → unboxed: pass raw values, receive raw result
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| self.compile_expr_unboxed(a))
                                .collect::<Result<Vec<_>, _>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            Ok(self.builder.inst_results(call)[0])
                        } else {
                            // Unboxed → boxed: tag args, untag result
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| {
                                    let raw = self.compile_expr_unboxed(a)?;
                                    Ok(self.tag_int_checked(raw))
                                })
                                .collect::<Result<Vec<_>, String>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            let boxed_result = self.builder.inst_results(call)[0];
                            Ok(self.untag_int(boxed_result))
                        }
                    } else {
                        Err(format!(
                            "Function '{}' not JIT-compiled (fallback required)",
                            name
                        ))
                    }
                } else {
                    Err(format!("Unknown function: {}", name))
                }
            }

            Expr::TailCall { name, args, .. } => {
                if name == &self.current_func_name
                    && let Some(loop_header) = self.loop_header
                {
                    // Self-recursive tail call in unboxed mode: pass raw values
                    let arg_vals: Vec<CValue> = args
                        .iter()
                        .map(|a| self.compile_expr_unboxed(a))
                        .collect::<Result<Vec<_>, _>>()?;

                    for (var, val) in self.param_vars.iter().zip(arg_vals.iter()) {
                        self.builder.def_var(*var, *val);
                    }

                    self.builder.ins().jump(loop_header, &[]);

                    let dead_block = self.builder.create_block();
                    self.builder.switch_to_block(dead_block);
                    self.builder.seal_block(dead_block);

                    return Ok(self.builder.ins().iconst(types::I64, 0));
                }

                // Non-self tail call → regular call
                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let callee_unboxed =
                            func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

                        if callee_unboxed {
                            // Both unboxed — use return_call for guaranteed TCE
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| self.compile_expr_unboxed(a))
                                .collect::<Result<Vec<_>, _>>()?;
                            self.builder.ins().return_call(func_ref, &arg_vals);

                            let dead_block = self.builder.create_block();
                            self.builder.switch_to_block(dead_block);
                            self.builder.seal_block(dead_block);
                            Ok(self.builder.ins().iconst(types::I64, 0))
                        } else {
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| {
                                    let raw = self.compile_expr_unboxed(a)?;
                                    Ok(self.tag_int_checked(raw))
                                })
                                .collect::<Result<Vec<_>, String>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            let boxed_result = self.builder.inst_results(call)[0];
                            Ok(self.untag_int(boxed_result))
                        }
                    } else {
                        Err(format!(
                            "Function '{}' not JIT-compiled (fallback required)",
                            name
                        ))
                    }
                } else {
                    Err(format!("Unknown function: {}", name))
                }
            }

            Expr::And(a, b) => {
                let a_val = self.compile_expr_unboxed(a)?;
                let zero_val = self.builder.ins().iconst(types::I64, 0);

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                let cmp = self.builder.ins().icmp(IntCC::NotEqual, a_val, zero_val);
                self.builder
                    .ins()
                    .brif(cmp, eval_b, &[], merge, &[BlockArg::Value(zero_val)]);

                self.builder.switch_to_block(eval_b);
                self.builder.seal_block(eval_b);
                let b_val = self.compile_expr_unboxed(b)?;
                self.builder.ins().jump(merge, &[BlockArg::Value(b_val)]);

                self.builder.switch_to_block(merge);
                self.builder.seal_block(merge);
                Ok(self.builder.block_params(merge)[0])
            }

            Expr::Or(a, b) => {
                let a_val = self.compile_expr_unboxed(a)?;
                let one_val = self.builder.ins().iconst(types::I64, 1);

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, a_val, zero);
                self.builder
                    .ins()
                    .brif(cmp, merge, &[BlockArg::Value(one_val)], eval_b, &[]);

                self.builder.switch_to_block(eval_b);
                self.builder.seal_block(eval_b);
                let b_val = self.compile_expr_unboxed(b)?;
                self.builder.ins().jump(merge, &[BlockArg::Value(b_val)]);

                self.builder.switch_to_block(merge);
                self.builder.seal_block(merge);
                Ok(self.builder.block_params(merge)[0])
            }

            _ => Err(format!("Unsupported expression in unboxed JIT: {:?}", expr)),
        }
    }

    /// Base-case speculation for unboxed mode.
    /// Same logic as try_speculate_call but using raw unboxed values.
    fn try_speculate_call_unboxed(
        &mut self,
        name: &str,
        ir_args: &[Expr],
    ) -> Result<Option<CValue>, String> {
        let func_idx = match self.func_names.get(name) {
            Some(&idx) => idx,
            None => return Ok(None),
        };
        if self.func_ids[func_idx].is_none() {
            return Ok(None);
        }
        // Only speculate on unboxed callees
        if func_idx >= self.unboxed_flags.len() || !self.unboxed_flags[func_idx] {
            return Ok(None);
        }
        let func = &self.ir_functions[func_idx];

        // Look for base case in then_branch OR else_branch
        let (cond_expr, base_expr, base_in_then) = match &func.body {
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                if Self::is_simple_base_case(then_branch) {
                    (condition.as_ref(), then_branch.as_ref(), true)
                } else if let Some(else_br) = else_branch
                    && Self::is_simple_base_case(else_br)
                {
                    (condition.as_ref(), else_br.as_ref(), false)
                } else {
                    return Ok(None);
                }
            }
            _ => return Ok(None),
        };

        if !expr_only_refs_params(cond_expr, &func.params) {
            return Ok(None);
        }
        if !expr_only_refs_params(base_expr, &func.params) {
            return Ok(None);
        }

        // Compile args in unboxed mode
        let arg_vals: Vec<CValue> = ir_args
            .iter()
            .map(|a| self.compile_expr_unboxed(a))
            .collect::<Result<_, _>>()?;

        // Temporarily bind callee params to arg values
        let mut saved_vars = HashMap::new();
        for (i, param_name) in func.params.iter().enumerate() {
            let var = self.new_var();
            self.builder.def_var(var, arg_vals[i]);
            if let Some(old) = self.vars.insert(param_name.clone(), var) {
                saved_vars.insert(param_name.clone(), old);
            }
        }

        // Compile condition and base case in unboxed mode
        let cond_val = self.compile_expr_unboxed(cond_expr)?;
        let base_val = self.compile_expr_unboxed(base_expr)?;

        // Restore vars
        for param_name in &func.params {
            self.vars.remove(param_name);
        }
        for (name, old_var) in saved_vars {
            self.vars.insert(name, old_var);
        }

        let call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        // Unboxed truthy check: != 0
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::NotEqual, cond_val, zero);
        if base_in_then {
            self.builder.ins().brif(
                cmp,
                merge_block,
                &[BlockArg::Value(base_val)],
                call_block,
                &[],
            );
        } else {
            self.builder.ins().brif(
                cmp,
                call_block,
                &[],
                merge_block,
                &[BlockArg::Value(base_val)],
            );
        }

        self.builder.switch_to_block(call_block);
        self.builder.seal_block(call_block);

        // Call the unboxed function: pass raw args, receive raw result
        let func_id = self.func_ids[func_idx].unwrap();
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, &arg_vals);
        let call_result = self.builder.inst_results(call)[0];
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(call_result)]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(Some(self.builder.block_params(merge_block)[0]))
    }

    // -- Main expression compiler --

    pub(super) fn compile_expr(&mut self, expr: &Expr) -> Result<CValue, String> {
        match expr {
            Expr::Int(n) => Ok(self.emit_nvalue(NValue::int(*n))),

            Expr::Float(f) => Ok(self.emit_nvalue(NValue::float(*f))),

            Expr::Bool(b) => {
                let bits = if *b { NV_TRUE } else { NV_FALSE };
                Ok(self.builder.ins().iconst(types::I64, bits as i64))
            }

            Expr::Unit => Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64)),

            Expr::String(s) => self.emit_string_value(s),

            Expr::Var(name, _) => {
                if let Some(&var) = self.vars.get(name) {
                    let val = self.builder.use_var(var);
                    // RC: incref on read — scope still owns the original
                    if self.rc_enabled {
                        Ok(self.emit_incref(val))
                    } else {
                        Ok(val)
                    }
                } else if let Some(&func_idx) = self.func_names.get(name) {
                    // Function reference as NaN-boxed Function value
                    Ok(self.emit_nvalue(NValue::function(func_idx)))
                } else {
                    Err(format!("Unknown variable: {}", name))
                }
            }

            Expr::BinOp { op, lhs, rhs, ty } => {
                let is_int = matches!(ty, Some(Type::Int));
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;

                if is_int {
                    // Use runtime helpers for arithmetic ops to correctly handle
                    // BigInt overflow (values exceeding 48-bit inline range).
                    // Comparisons that don't produce overflow can use inline ops
                    // when both inputs are guaranteed to be inline ints.
                    match op {
                        BinOp::Add => Ok(self.call_helper("jit_int_add", &[lhs_val, rhs_val])),
                        BinOp::Sub => Ok(self.call_helper("jit_int_sub", &[lhs_val, rhs_val])),
                        BinOp::Mul => Ok(self.call_helper("jit_int_mul", &[lhs_val, rhs_val])),
                        BinOp::Div => Ok(self.call_helper("jit_int_div", &[lhs_val, rhs_val])),
                        BinOp::Mod => Ok(self.call_helper("jit_int_mod", &[lhs_val, rhs_val])),
                        BinOp::Eq => Ok(self.call_helper("jit_values_equal", &[lhs_val, rhs_val])),
                        BinOp::Ne => {
                            let eq = self.call_helper("jit_values_equal", &[lhs_val, rhs_val]);
                            let one = self.builder.ins().iconst(types::I64, 1);
                            Ok(self.builder.ins().bxor(eq, one))
                        }
                        BinOp::Lt => Ok(self.call_helper("jit_int_lt", &[lhs_val, rhs_val])),
                        BinOp::Gt => Ok(self.call_helper("jit_int_gt", &[lhs_val, rhs_val])),
                        BinOp::Le => Ok(self.call_helper("jit_int_le", &[lhs_val, rhs_val])),
                        BinOp::Ge => Ok(self.call_helper("jit_int_ge", &[lhs_val, rhs_val])),
                        BinOp::ListConcat => {
                            Ok(self.call_helper("jit_list_concat", &[lhs_val, rhs_val]))
                        }
                    }
                } else {
                    // Non-int: compare raw NaN-boxed bits for equality/ordering,
                    // or use runtime helpers for complex types.
                    // For simple cases (Bool comparison), raw bit comparison works.
                    match op {
                        BinOp::Eq => {
                            let result = self.call_helper("jit_values_equal", &[lhs_val, rhs_val]);
                            Ok(result)
                        }
                        BinOp::Ne => {
                            let eq = self.call_helper("jit_values_equal", &[lhs_val, rhs_val]);
                            // Flip: if eq is NV_TRUE, return NV_FALSE and vice versa
                            let one = self.builder.ins().iconst(types::I64, 1);
                            Ok(self.builder.ins().bxor(eq, one))
                        }
                        // For non-int arithmetic, use runtime helpers (handles BigInt)
                        BinOp::Add => Ok(self.call_helper("jit_int_add", &[lhs_val, rhs_val])),
                        BinOp::Sub => Ok(self.call_helper("jit_int_sub", &[lhs_val, rhs_val])),
                        BinOp::Mul => Ok(self.call_helper("jit_int_mul", &[lhs_val, rhs_val])),
                        BinOp::Div => Ok(self.call_helper("jit_int_div", &[lhs_val, rhs_val])),
                        BinOp::Mod => Ok(self.call_helper("jit_int_mod", &[lhs_val, rhs_val])),
                        // For ordering comparisons without type annotation, use inline
                        // NaN-boxed comparison (works correctly for inline ints; for BigInt
                        // the type checker should provide ty=Int so the is_int path is used).
                        BinOp::Lt => {
                            let a = self.untag_int(lhs_val);
                            let b = self.untag_int(rhs_val);
                            let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, a, b);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Gt => {
                            let a = self.untag_int(lhs_val);
                            let b = self.untag_int(rhs_val);
                            let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThan, a, b);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Le => {
                            let a = self.untag_int(lhs_val);
                            let b = self.untag_int(rhs_val);
                            let cmp = self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, a, b);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Ge => {
                            let a = self.untag_int(lhs_val);
                            let b = self.untag_int(rhs_val);
                            let cmp =
                                self.builder
                                    .ins()
                                    .icmp(IntCC::SignedGreaterThanOrEqual, a, b);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::ListConcat => {
                            Ok(self.call_helper("jit_list_concat", &[lhs_val, rhs_val]))
                        }
                    }
                }
            }

            Expr::UnaryOp { op, operand, .. } => {
                let val = self.compile_expr(operand)?;
                match op {
                    UnaryOp::Neg => Ok(self.call_helper("jit_int_neg", &[val])),
                    UnaryOp::Not => {
                        // Check bit 0 (truthy check), invert
                        let one = self.builder.ins().iconst(types::I64, 1);
                        let bit = self.builder.ins().band(val, one);
                        let zero = self.builder.ins().iconst(types::I64, 0);
                        let is_falsy = self.builder.ins().icmp(IntCC::Equal, bit, zero);
                        Ok(self.tag_bool(is_falsy))
                    }
                }
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_val = self.compile_expr(condition)?;

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                self.builder.append_block_param(merge_block, types::I64);

                let cmp = self.is_truthy(cond_val);
                // RC: is_truthy borrows cond_val; decref the owned value now
                if self.rc_enabled {
                    self.emit_decref(cond_val);
                }
                self.builder
                    .ins()
                    .brif(cmp, then_block, &[], else_block, &[]);

                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_val = self.compile_expr(then_branch)?;
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(then_val)]);

                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_val = if let Some(else_expr) = else_branch {
                    self.compile_expr(else_expr)?
                } else {
                    self.builder.ins().iconst(types::I64, NV_UNIT as i64)
                };
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(else_val)]);

                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(self.builder.block_params(merge_block)[0])
            }

            Expr::Let { pattern, value, .. } => {
                let val = self.compile_expr(value)?;
                self.bind_pattern_rc(pattern, val)?;
                Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64))
            }

            Expr::Block(exprs, _) => {
                // SRA: find non-escaping record bindings and track them
                let sra_candidates = Self::find_sra_candidates(exprs);
                let sra_names: std::collections::HashSet<&str> =
                    sra_candidates.iter().map(|(n, _)| n.as_str()).collect();

                self.push_rc_scope();
                let mut result = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                for (idx, e) in exprs.iter().enumerate() {
                    let is_last = idx == exprs.len() - 1;
                    // SRA: intercept Let bindings to MakeRecord for candidates
                    if self.try_compile_sra_let(e, &sra_names)? {
                        result = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                        continue;
                    }
                    result = self.compile_expr(e)?;
                    // RC: decref intermediate results (not the last expression)
                    if self.rc_enabled && !is_last && !matches!(e, Expr::Let { .. }) {
                        self.emit_decref(result);
                    }
                }
                // RC: store result in a variable so pop_rc_scope can exclude it
                if self.rc_enabled {
                    let result_var = self.new_var();
                    self.builder.def_var(result_var, result);
                    self.pop_rc_scope(Some(result_var));
                    result = self.builder.use_var(result_var);
                } else {
                    self.pop_rc_scope(None);
                }
                Ok(result)
            }

            Expr::CallDirect { name, args, .. } => {
                if let Some(val) = self.try_speculate_call(name, args)? {
                    return Ok(val);
                }

                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let callee_unboxed =
                            func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

                        if callee_unboxed {
                            // Boxed → unboxed: untag args, tag result
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| {
                                    let boxed = self.compile_expr(a)?;
                                    Ok(self.untag_int(boxed))
                                })
                                .collect::<Result<Vec<_>, String>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            let raw_result = self.builder.inst_results(call)[0];
                            Ok(self.tag_int_checked(raw_result))
                        } else {
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| self.compile_expr(a))
                                .collect::<Result<Vec<_>, _>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            Ok(self.builder.inst_results(call)[0])
                        }
                    } else {
                        Err(format!(
                            "Function '{}' not JIT-compiled (fallback required)",
                            name
                        ))
                    }
                } else {
                    Err(format!("Unknown function: {}", name))
                }
            }

            Expr::TailCall { name, args, .. } => {
                if name == &self.current_func_name
                    && let Some(loop_header) = self.loop_header
                {
                    let arg_vals: Vec<CValue> = args
                        .iter()
                        .map(|a| self.compile_expr(a))
                        .collect::<Result<Vec<_>, _>>()?;

                    // RC: decref old parameter values before overwriting
                    if self.rc_enabled {
                        let pvars: Vec<Variable> = self.param_vars.clone();
                        for pv in &pvars {
                            let old_val = self.builder.use_var(*pv);
                            self.emit_decref(old_val);
                        }
                    }

                    for (var, val) in self.param_vars.iter().zip(arg_vals.iter()) {
                        self.builder.def_var(*var, *val);
                    }

                    self.builder.ins().jump(loop_header, &[]);

                    let dead_block = self.builder.create_block();
                    self.builder.switch_to_block(dead_block);
                    self.builder.seal_block(dead_block);

                    return Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64));
                }

                // Non-self tail call → regular call
                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let callee_unboxed =
                            func_idx < self.unboxed_flags.len() && self.unboxed_flags[func_idx];
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

                        if callee_unboxed {
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| {
                                    let boxed = self.compile_expr(a)?;
                                    Ok(self.untag_int(boxed))
                                })
                                .collect::<Result<Vec<_>, String>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            let raw_result = self.builder.inst_results(call)[0];
                            Ok(self.tag_int_checked(raw_result))
                        } else {
                            // Both boxed — use return_call for guaranteed TCE
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| self.compile_expr(a))
                                .collect::<Result<Vec<_>, _>>()?;
                            self.builder.ins().return_call(func_ref, &arg_vals);

                            let dead_block = self.builder.create_block();
                            self.builder.switch_to_block(dead_block);
                            self.builder.seal_block(dead_block);
                            Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64))
                        }
                    } else {
                        Err(format!(
                            "Function '{}' not JIT-compiled (fallback required)",
                            name
                        ))
                    }
                } else {
                    Err(format!("Unknown function: {}", name))
                }
            }

            Expr::And(a, b) => {
                let a_val = self.compile_expr(a)?;
                let false_val = self.builder.ins().iconst(types::I64, NV_FALSE as i64);
                let cmp = self.is_truthy(a_val);
                // RC: is_truthy borrows a_val; decref the owned value now
                if self.rc_enabled {
                    self.emit_decref(a_val);
                }

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                self.builder
                    .ins()
                    .brif(cmp, eval_b, &[], merge, &[BlockArg::Value(false_val)]);

                self.builder.switch_to_block(eval_b);
                self.builder.seal_block(eval_b);
                let b_val = self.compile_expr(b)?;
                self.builder.ins().jump(merge, &[BlockArg::Value(b_val)]);

                self.builder.switch_to_block(merge);
                self.builder.seal_block(merge);
                Ok(self.builder.block_params(merge)[0])
            }

            Expr::Or(a, b) => {
                let a_val = self.compile_expr(a)?;
                let true_val = self.builder.ins().iconst(types::I64, NV_TRUE as i64);
                let cmp = self.is_truthy(a_val);
                // RC: is_truthy borrows a_val; decref the owned value now
                if self.rc_enabled {
                    self.emit_decref(a_val);
                }

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                self.builder
                    .ins()
                    .brif(cmp, merge, &[BlockArg::Value(true_val)], eval_b, &[]);

                self.builder.switch_to_block(eval_b);
                self.builder.seal_block(eval_b);
                let b_val = self.compile_expr(b)?;
                self.builder.ins().jump(merge, &[BlockArg::Value(b_val)]);

                self.builder.switch_to_block(merge);
                self.builder.seal_block(merge);
                Ok(self.builder.block_params(merge)[0])
            }

            // -- Phase 2: CallNative + Concat --
            Expr::CallNative {
                module: mod_name,
                method,
                args,
                ..
            } => {
                let qualified = format!("{}.{}", mod_name, method);

                // HOF path: compile inline (works for both JIT and AOT)
                if Self::is_inline_hof(&qualified) {
                    return self.compile_hof(&qualified, args);
                }

                // AOT path: direct call to extern "C" symbol in libbaseline_rt
                if let Some(native_ids) = self.aot_native_ids {
                    let func_id = native_ids
                        .get(&qualified)
                        .ok_or_else(|| format!("AOT: unsupported native: {}", qualified))?;

                    let arg_vals: Vec<CValue> = args
                        .iter()
                        .map(|a| self.compile_expr(a))
                        .collect::<Result<Vec<_>, _>>()?;

                    let args_addr = self.spill_to_stack(&arg_vals);
                    let count_val = self.builder.ins().iconst(types::I64, arg_vals.len() as i64);

                    let func_ref = self
                        .module
                        .declare_func_in_func(*func_id, self.builder.func);
                    let call = self.builder.ins().call(func_ref, &[args_addr, count_val]);
                    return Ok(self.builder.inst_results(call)[0]);
                }

                // JIT path: dispatch through NativeRegistry
                let registry = self.natives.ok_or("No native registry for JIT")?;
                let native_id = registry
                    .lookup(&qualified)
                    .ok_or_else(|| format!("Unknown native: {}", qualified))?;
                let is_owning = self.rc_enabled && registry.is_owning(native_id);

                let arg_vals: Vec<CValue> = if is_owning {
                    // Owning (CoW) dispatch: "move" the first arg (container)
                    // instead of copying it. This avoids the incref that would
                    // prevent in-place mutation in the native.
                    let mut vals = Vec::with_capacity(args.len());
                    for (i, a) in args.iter().enumerate() {
                        if i == 0 {
                            if let Expr::Var(name, _) = a {
                                // Move: read without incref, then null out the var
                                if let Some(&var) = self.vars.get(name.as_str()) {
                                    let val = self.builder.use_var(var);
                                    let unit =
                                        self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                                    self.builder.def_var(var, unit);
                                    vals.push(val);
                                    continue;
                                }
                            }
                        }
                        vals.push(self.compile_expr(a)?);
                    }
                    vals
                } else {
                    args.iter()
                        .map(|a| self.compile_expr(a))
                        .collect::<Result<Vec<_>, _>>()?
                };

                let args_addr = self.spill_to_stack(&arg_vals);
                let registry_ptr = self
                    .builder
                    .ins()
                    .iconst(self.ptr_type, registry as *const NativeRegistry as i64);
                let id_val = self.builder.ins().iconst(types::I64, native_id as i64);
                let count_val = self.builder.ins().iconst(types::I64, arg_vals.len() as i64);

                let helper = if is_owning {
                    "jit_call_native_owning"
                } else {
                    "jit_call_native"
                };
                Ok(self.call_helper(
                    helper,
                    &[registry_ptr, id_val, args_addr, count_val],
                ))
            }

            Expr::Concat(parts) => {
                let part_vals: Vec<CValue> = parts
                    .iter()
                    .map(|e| self.compile_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;

                let addr = self.spill_to_stack(&part_vals);
                let count = self
                    .builder
                    .ins()
                    .iconst(types::I64, part_vals.len() as i64);
                Ok(self.call_helper("jit_concat", &[addr, count]))
            }

            // -- Phase 3: Constructors --
            Expr::MakeEnum { tag, payload, .. } => {
                if let Some(id) = self.tags.get_id(tag) {
                    // Multi-arg: MakeEnum { payload: MakeTuple([...]) } → flat enum
                    if let Expr::MakeTuple(items, _) = payload.as_ref() {
                        let item_vals: Vec<CValue> = items
                            .iter()
                            .map(|e| self.compile_expr(e))
                            .collect::<Result<Vec<_>, _>>()?;
                        let tag_val = self.emit_string_value(tag)?;
                        let id_val = self.builder.ins().iconst(types::I64, id as i64);
                        let addr = self.spill_to_stack(&item_vals);
                        let count =
                            self.builder.ins().iconst(types::I64, item_vals.len() as i64);
                        return Ok(self.call_helper(
                            "jit_make_enum_flat",
                            &[tag_val, id_val, addr, count],
                        ));
                    }
                    // Nullary: MakeEnum { payload: Unit } → baked constant
                    if matches!(payload.as_ref(), Expr::Unit) {
                        let tag_str: RcStr = tag.as_str().into();
                        let nv = NValue::enum_val_flat(tag_str, id, vec![]);
                        return Ok(self.emit_heap_nvalue(nv));
                    }
                    // Single-arg: existing path
                    let tag_val = self.emit_string_value(tag)?;
                    let payload_val = self.compile_expr(payload)?;
                    let id_val = self.builder.ins().iconst(types::I64, id as i64);
                    Ok(self.call_helper("jit_make_enum_with_id", &[tag_val, id_val, payload_val]))
                } else {
                    let tag_val = self.emit_string_value(tag)?;
                    let payload_val = self.compile_expr(payload)?;
                    Ok(self.call_helper("jit_make_enum", &[tag_val, payload_val]))
                }
            }

            Expr::MakeStruct { name, fields, .. } => {
                let name_val = self.emit_string_value(name)?;

                let mut pair_vals = Vec::with_capacity(fields.len() * 2);
                for (fname, fexpr) in fields {
                    let key_val = self.emit_string_value(fname)?;
                    let val = self.compile_expr(fexpr)?;
                    pair_vals.push(key_val);
                    pair_vals.push(val);
                }

                let addr = self.spill_to_stack(&pair_vals);
                let count = self.builder.ins().iconst(types::I64, fields.len() as i64);
                Ok(self.call_helper("jit_make_struct", &[name_val, addr, count]))
            }

            Expr::MakeList(items, _) => {
                let item_vals: Vec<CValue> = items
                    .iter()
                    .map(|e| self.compile_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;

                let addr = self.spill_to_stack(&item_vals);
                let count = self
                    .builder
                    .ins()
                    .iconst(types::I64, item_vals.len() as i64);
                Ok(self.call_helper("jit_make_list", &[addr, count]))
            }

            Expr::MakeRecord(fields, _) => {
                let mut pair_vals = Vec::with_capacity(fields.len() * 2);
                for (fname, fexpr) in fields {
                    let key_val = self.emit_string_value(fname)?;
                    let val = self.compile_expr(fexpr)?;
                    pair_vals.push(key_val);
                    pair_vals.push(val);
                }

                let addr = self.spill_to_stack(&pair_vals);
                let count = self.builder.ins().iconst(types::I64, fields.len() as i64);
                Ok(self.call_helper("jit_make_record", &[addr, count]))
            }

            Expr::MakeTuple(items, _) => {
                let item_vals: Vec<CValue> = items
                    .iter()
                    .map(|e| self.compile_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;

                let addr = self.spill_to_stack(&item_vals);
                let count = self
                    .builder
                    .ins()
                    .iconst(types::I64, item_vals.len() as i64);
                Ok(self.call_helper("jit_make_tuple", &[addr, count]))
            }

            Expr::MakeRange(start, end) => {
                let start_val = self.compile_expr(start)?;
                let end_val = self.compile_expr(end)?;
                Ok(self.call_helper("jit_make_range", &[start_val, end_val]))
            }

            Expr::UpdateRecord { base, updates, .. } => {
                // SRA: if base is a scalar-replaced record, update field variables directly
                if let Expr::Var(name, _) = base.as_ref() {
                    if self.sra_records.contains_key(name.as_str()) {
                        // Compile updated field values and def_var them
                        for (fname, fexpr) in updates {
                            let val = self.compile_expr(fexpr)?;
                            if let Some(field_vars) = self.sra_records.get(name.as_str()) {
                                if let Some(&var) = field_vars.get(fname.as_str()) {
                                    self.builder.def_var(var, val);
                                }
                            }
                        }
                        return Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64));
                    }
                }
                let base_val = self.compile_expr(base)?;

                let mut pair_vals = Vec::with_capacity(updates.len() * 2);
                for (fname, fexpr) in updates {
                    let key_val = self.emit_string_value(fname)?;
                    let val = self.compile_expr(fexpr)?;
                    pair_vals.push(key_val);
                    pair_vals.push(val);
                }

                let addr = self.spill_to_stack(&pair_vals);
                let count = self.builder.ins().iconst(types::I64, updates.len() as i64);
                Ok(self.call_helper("jit_update_record", &[base_val, addr, count]))
            }

            Expr::GetField { object, field, .. } => {
                // SRA: if object is a scalar-replaced record, read field variable directly
                if let Expr::Var(name, _) = object.as_ref() {
                    if let Some(field_vars) = self.sra_records.get(name.as_str()) {
                        if let Some(&var) = field_vars.get(field.as_str()) {
                            return Ok(self.builder.use_var(var));
                        }
                    }
                }
                let obj_val = self.compile_expr(object)?;
                let field_val = self.emit_string_value(field)?;
                let result = self.call_helper("jit_get_field", &[obj_val, field_val]);
                // RC: jit_get_field borrows both args; decref them
                if self.rc_enabled {
                    self.emit_decref(obj_val);
                    self.emit_decref(field_val);
                }
                Ok(result)
            }

            // -- Phase 4: Match --
            Expr::Match { subject, arms, .. } => self.compile_match(subject, arms),

            // -- Phase 5: For, Try --
            Expr::For {
                binding,
                iterable,
                body,
            } => self.compile_for(binding, iterable, body),

            Expr::Try { expr: inner, .. } => self.compile_try(inner),

            Expr::MakeClosure { func_idx, captures } => {
                if captures.is_empty() {
                    // Zero-capture: emit NValue::function directly (no heap allocation)
                    Ok(self.emit_heap_nvalue(NValue::function(*func_idx)))
                } else {
                    // Compile each capture expression
                    let cap_vals: Vec<CValue> = captures
                        .iter()
                        .map(|c| self.compile_expr(c))
                        .collect::<Result<Vec<_>, _>>()?;
                    let addr = self.spill_to_stack(&cap_vals);
                    let idx_val = self.builder.ins().iconst(types::I64, *func_idx as i64);
                    let count_val = self.builder.ins().iconst(types::I64, captures.len() as i64);
                    Ok(self.call_helper("jit_make_closure", &[idx_val, addr, count_val]))
                }
            }

            Expr::GetClosureVar(idx) => {
                // __closure is the first parameter (param_vars[0])
                let closure_param = self.builder.use_var(self.param_vars[0]);
                let idx_val = self.builder.ins().iconst(types::I64, *idx as i64);
                Ok(self.call_helper("jit_closure_upvalue", &[closure_param, idx_val]))
            }

            Expr::CallIndirect { callee, args, .. } => {
                let callee_val = self.compile_expr(callee)?;
                let compiled_args: Vec<CValue> = args
                    .iter()
                    .map(|a| self.compile_expr(a))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = self.compile_call_value(callee_val, &compiled_args)?;
                // RC: decref the callee value after the call completes
                if self.rc_enabled {
                    self.emit_decref(callee_val);
                }
                Ok(result)
            }

            Expr::TailCallIndirect { callee, args, .. } => {
                let callee_val = self.compile_expr(callee)?;
                let compiled_args: Vec<CValue> = args
                    .iter()
                    .map(|a| self.compile_expr(a))
                    .collect::<Result<Vec<_>, _>>()?;

                let use_tail = self.func_call_conv == CallConv::Tail;

                // Check if closure or plain function
                let is_closure = self.call_helper("jit_is_closure", &[callee_val]);
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(IntCC::NotEqual, is_closure, zero);

                let closure_block = self.builder.create_block();
                let function_block = self.builder.create_block();
                self.builder
                    .ins()
                    .brif(cmp, closure_block, &[], function_block, &[]);

                if use_tail {
                    // Tail CC: use return_call_indirect for proper tail calls
                    // Closure path
                    self.builder.switch_to_block(closure_block);
                    self.builder.seal_block(closure_block);
                    let fn_ptr_c = self.call_helper("jit_closure_fn_ptr", &[callee_val]);
                    let zero_c = self.builder.ins().iconst(types::I64, 0);
                    let ptr_ok_c = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_c, zero_c);
                    let call_closure_block = self.builder.create_block();
                    let null_closure_block = self.builder.create_block();
                    self.builder.ins().brif(
                        ptr_ok_c,
                        call_closure_block,
                        &[],
                        null_closure_block,
                        &[],
                    );

                    self.builder.switch_to_block(null_closure_block);
                    self.builder.seal_block(null_closure_block);
                    let unit_c = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                    self.builder.ins().return_(&[unit_c]);

                    self.builder.switch_to_block(call_closure_block);
                    self.builder.seal_block(call_closure_block);
                    let mut cargs = vec![callee_val];
                    cargs.extend(&compiled_args);
                    let closure_sig = self.build_indirect_sig(args.len() + 1);
                    let sig_ref_c = self.builder.import_signature(closure_sig);
                    self.builder
                        .ins()
                        .return_call_indirect(sig_ref_c, fn_ptr_c, &cargs);

                    // Function path
                    self.builder.switch_to_block(function_block);
                    self.builder.seal_block(function_block);
                    let fn_ptr_f = self.call_helper("jit_function_fn_ptr", &[callee_val]);
                    if self.rc_enabled {
                        self.emit_decref(callee_val);
                    }
                    let zero_f = self.builder.ins().iconst(types::I64, 0);
                    let ptr_ok_f = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_f, zero_f);
                    let call_func_block = self.builder.create_block();
                    let null_func_block = self.builder.create_block();
                    self.builder
                        .ins()
                        .brif(ptr_ok_f, call_func_block, &[], null_func_block, &[]);

                    self.builder.switch_to_block(null_func_block);
                    self.builder.seal_block(null_func_block);
                    let unit_f = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                    self.builder.ins().return_(&[unit_f]);

                    self.builder.switch_to_block(call_func_block);
                    self.builder.seal_block(call_func_block);
                    let func_sig = self.build_indirect_sig(args.len());
                    let sig_ref_f = self.builder.import_signature(func_sig);
                    self.builder
                        .ins()
                        .return_call_indirect(sig_ref_f, fn_ptr_f, &compiled_args);

                    let dead_block = self.builder.create_block();
                    self.builder.switch_to_block(dead_block);
                    self.builder.seal_block(dead_block);
                    Ok(self.builder.ins().iconst(types::I64, 0))
                } else {
                    // Non-Tail CC (AOT Fast): use call_indirect + return_
                    let merge_block = self.builder.create_block();
                    self.builder.append_block_param(merge_block, types::I64);

                    // Closure path
                    self.builder.switch_to_block(closure_block);
                    self.builder.seal_block(closure_block);
                    let fn_ptr_c = self.call_helper("jit_closure_fn_ptr", &[callee_val]);
                    let zero_c = self.builder.ins().iconst(types::I64, 0);
                    let ptr_ok_c = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_c, zero_c);
                    let call_closure_block = self.builder.create_block();
                    let null_closure_block = self.builder.create_block();
                    self.builder.ins().brif(
                        ptr_ok_c,
                        call_closure_block,
                        &[],
                        null_closure_block,
                        &[],
                    );

                    self.builder.switch_to_block(null_closure_block);
                    self.builder.seal_block(null_closure_block);
                    let unit_c = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(unit_c)]);

                    self.builder.switch_to_block(call_closure_block);
                    self.builder.seal_block(call_closure_block);
                    let mut cargs = vec![callee_val];
                    cargs.extend(&compiled_args);
                    let closure_sig = self.build_indirect_sig(args.len() + 1);
                    let sig_ref_c = self.builder.import_signature(closure_sig);
                    let inst_c = self
                        .builder
                        .ins()
                        .call_indirect(sig_ref_c, fn_ptr_c, &cargs);
                    let result_c = self.builder.inst_results(inst_c)[0];
                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(result_c)]);

                    // Function path
                    self.builder.switch_to_block(function_block);
                    self.builder.seal_block(function_block);
                    let fn_ptr_f = self.call_helper("jit_function_fn_ptr", &[callee_val]);
                    if self.rc_enabled {
                        self.emit_decref(callee_val);
                    }
                    let zero_f = self.builder.ins().iconst(types::I64, 0);
                    let ptr_ok_f = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_f, zero_f);
                    let call_func_block = self.builder.create_block();
                    let null_func_block = self.builder.create_block();
                    self.builder
                        .ins()
                        .brif(ptr_ok_f, call_func_block, &[], null_func_block, &[]);

                    self.builder.switch_to_block(null_func_block);
                    self.builder.seal_block(null_func_block);
                    let unit_f = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(unit_f)]);

                    self.builder.switch_to_block(call_func_block);
                    self.builder.seal_block(call_func_block);
                    let func_sig = self.build_indirect_sig(args.len());
                    let sig_ref_f = self.builder.import_signature(func_sig);
                    let inst_f =
                        self.builder
                            .ins()
                            .call_indirect(sig_ref_f, fn_ptr_f, &compiled_args);
                    let result_f = self.builder.inst_results(inst_f)[0];
                    self.builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(result_f)]);

                    // Merge
                    self.builder.switch_to_block(merge_block);
                    self.builder.seal_block(merge_block);
                    let result = self.builder.block_params(merge_block)[0];
                    self.builder.ins().return_(&[result]);

                    let dead_block = self.builder.create_block();
                    self.builder.switch_to_block(dead_block);
                    self.builder.seal_block(dead_block);
                    Ok(self.builder.ins().iconst(types::I64, 0))
                }
            }

            // Remaining unsupported constructs
            _ => Err(format!("Unsupported expression in JIT: {:?}", expr)),
        }
    }

    /// Build a Cranelift signature for indirect calls with `n_params` I64 params.
    fn build_indirect_sig(&self, n_params: usize) -> cranelift_codegen::ir::Signature {
        let mut sig = self.module.make_signature();
        sig.call_conv = self.func_call_conv;
        for _ in 0..n_params {
            sig.params.push(AbiParam::new(types::I64));
        }
        sig.returns.push(AbiParam::new(types::I64));
        sig
    }

    // -- Indirect call dispatch (closure-vs-function) --

    /// Call a value that may be a closure or plain function.
    /// Handles: is_closure check → branch → closure path / function path → merge.
    fn compile_call_value(
        &mut self,
        callee_val: CValue,
        args: &[CValue],
    ) -> Result<CValue, String> {
        let is_closure = self.call_helper("jit_is_closure", &[callee_val]);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(IntCC::NotEqual, is_closure, zero);

        let closure_block = self.builder.create_block();
        let function_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);
        self.builder
            .ins()
            .brif(cmp, closure_block, &[], function_block, &[]);

        // Closure path: call_indirect(sig_N+1, fn_ptr, [closure, args...])
        self.builder.switch_to_block(closure_block);
        self.builder.seal_block(closure_block);
        let fn_ptr_c = self.call_helper("jit_closure_fn_ptr", &[callee_val]);
        let zero_c = self.builder.ins().iconst(types::I64, 0);
        let ptr_ok_c = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_c, zero_c);
        let call_closure_block = self.builder.create_block();
        let null_closure_block = self.builder.create_block();
        self.builder
            .ins()
            .brif(ptr_ok_c, call_closure_block, &[], null_closure_block, &[]);

        self.builder.switch_to_block(null_closure_block);
        self.builder.seal_block(null_closure_block);
        let unit_c = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(unit_c)]);

        self.builder.switch_to_block(call_closure_block);
        self.builder.seal_block(call_closure_block);
        let mut cargs = vec![callee_val];
        cargs.extend(args);
        let closure_sig = self.build_indirect_sig(args.len() + 1);
        let sig_ref_c = self.builder.import_signature(closure_sig);
        let result_c = self
            .builder
            .ins()
            .call_indirect(sig_ref_c, fn_ptr_c, &cargs);
        let ret_c = self.builder.inst_results(result_c)[0];
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(ret_c)]);

        // Function path: call_indirect(sig_N, fn_ptr, [args...])
        self.builder.switch_to_block(function_block);
        self.builder.seal_block(function_block);
        let fn_ptr_f = self.call_helper("jit_function_fn_ptr", &[callee_val]);
        let zero_f = self.builder.ins().iconst(types::I64, 0);
        let ptr_ok_f = self.builder.ins().icmp(IntCC::NotEqual, fn_ptr_f, zero_f);
        let call_func_block = self.builder.create_block();
        let null_func_block = self.builder.create_block();
        self.builder
            .ins()
            .brif(ptr_ok_f, call_func_block, &[], null_func_block, &[]);

        self.builder.switch_to_block(null_func_block);
        self.builder.seal_block(null_func_block);
        let unit_f = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(unit_f)]);

        self.builder.switch_to_block(call_func_block);
        self.builder.seal_block(call_func_block);
        let func_sig = self.build_indirect_sig(args.len());
        let sig_ref_f = self.builder.import_signature(func_sig);
        let result_f = self.builder.ins().call_indirect(sig_ref_f, fn_ptr_f, args);
        let ret_f = self.builder.inst_results(result_f)[0];
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(ret_f)]);

        // Merge
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(self.builder.block_params(merge_block)[0])
    }

    // -- HOF inline compilation --

    /// Check if a qualified native name is a HOF we compile inline.
    fn is_inline_hof(qualified: &str) -> bool {
        matches!(
            qualified,
            "List.map"
                | "List.filter"
                | "List.fold"
                | "List.find"
                | "Option.map"
                | "Result.map"
                | "Result.map_err"
        )
    }

    /// Compile a HOF call inline (no ABI bridging needed).
    fn compile_hof(&mut self, qualified: &str, args: &[Expr]) -> Result<CValue, String> {
        match qualified {
            "List.map" => self.compile_hof_list_map(args),
            "List.filter" => self.compile_hof_list_filter(args),
            "List.fold" => self.compile_hof_list_fold(args),
            "List.find" => self.compile_hof_list_find(args),
            "Option.map" => self.compile_hof_option_map(args),
            "Result.map" => self.compile_hof_result_map(args),
            "Result.map_err" => self.compile_hof_result_map_err(args),
            _ => Err(format!("Unknown inline HOF: {}", qualified)),
        }
    }

    /// List.map(list, fn) — loop: get item → call fn → store result → build list
    fn compile_hof_list_map(&mut self, args: &[Expr]) -> Result<CValue, String> {
        if args.len() != 2 {
            return Err("List.map requires 2 arguments".into());
        }
        let list_val = self.compile_expr(&args[0])?;
        let fn_val = self.compile_expr(&args[1])?;

        let len = self.call_helper("jit_list_length_raw", &[list_val]);
        let buf = self.call_helper("jit_alloc_buf", &[len]);

        // Loop: i = 0; while i < len
        let i_var = self.new_var();
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(i_var, zero);

        let header = self.builder.create_block();
        let body = self.builder.create_block();
        let done = self.builder.create_block();

        self.builder.ins().jump(header, &[]);
        self.builder.switch_to_block(header);

        let i = self.builder.use_var(i_var);
        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, i, len);
        self.builder.ins().brif(cmp, body, &[], done, &[]);

        self.builder.switch_to_block(body);
        self.builder.seal_block(body);

        let i = self.builder.use_var(i_var);
        let item = self.call_helper("jit_list_get_raw", &[list_val, i]);
        let result = self.compile_call_value(fn_val, &[item])?;

        // Store result at buf[i*8]
        let i = self.builder.use_var(i_var);
        let offset = self.builder.ins().imul_imm(i, 8);
        let addr = self.builder.ins().iadd(buf, offset);
        self.builder
            .ins()
            .store(cranelift_codegen::ir::MemFlags::new(), result, addr, 0);

        // i++
        let i = self.builder.use_var(i_var);
        let one = self.builder.ins().iconst(types::I64, 1);
        let next_i = self.builder.ins().iadd(i, one);
        self.builder.def_var(i_var, next_i);
        self.builder.ins().jump(header, &[]);

        self.builder.switch_to_block(done);
        self.builder.seal_block(done);
        self.builder.seal_block(header);

        let result = self.call_helper("jit_build_list_from_buf", &[buf, len]);
        // RC: decref input list and function after building result
        if self.rc_enabled {
            self.emit_decref(list_val);
            self.emit_decref(fn_val);
        }
        Ok(result)
    }

    /// List.filter(list, fn) — loop: get item → call fn → if truthy, store → build list
    fn compile_hof_list_filter(&mut self, args: &[Expr]) -> Result<CValue, String> {
        if args.len() != 2 {
            return Err("List.filter requires 2 arguments".into());
        }
        let list_val = self.compile_expr(&args[0])?;
        let fn_val = self.compile_expr(&args[1])?;

        let len = self.call_helper("jit_list_length_raw", &[list_val]);
        let buf = self.call_helper("jit_alloc_buf", &[len]);

        let i_var = self.new_var();
        let out_var = self.new_var();
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(i_var, zero);
        self.builder.def_var(out_var, zero);

        let header = self.builder.create_block();
        let body = self.builder.create_block();
        let store_block = self.builder.create_block();
        let skip_block = self.builder.create_block();
        let done = self.builder.create_block();

        self.builder.ins().jump(header, &[]);
        self.builder.switch_to_block(header);

        let i = self.builder.use_var(i_var);
        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, i, len);
        self.builder.ins().brif(cmp, body, &[], done, &[]);

        self.builder.switch_to_block(body);
        self.builder.seal_block(body);

        let i = self.builder.use_var(i_var);
        let item = self.call_helper("jit_list_get_raw", &[list_val, i]);
        let pred_result = self.compile_call_value(fn_val, &[item])?;
        let is_true = self.call_helper("jit_is_truthy", &[pred_result]);
        let zero_cmp = self.builder.ins().iconst(types::I64, 0);
        let truthy = self.builder.ins().icmp(IntCC::NotEqual, is_true, zero_cmp);
        self.builder
            .ins()
            .brif(truthy, store_block, &[], skip_block, &[]);

        // Store path
        self.builder.switch_to_block(store_block);
        self.builder.seal_block(store_block);
        let out_idx = self.builder.use_var(out_var);
        let offset = self.builder.ins().imul_imm(out_idx, 8);
        let addr = self.builder.ins().iadd(buf, offset);
        self.builder
            .ins()
            .store(cranelift_codegen::ir::MemFlags::new(), item, addr, 0);
        let one = self.builder.ins().iconst(types::I64, 1);
        let next_out = self.builder.ins().iadd(out_idx, one);
        self.builder.def_var(out_var, next_out);
        self.builder.ins().jump(skip_block, &[]);

        // Skip / continue
        self.builder.switch_to_block(skip_block);
        self.builder.seal_block(skip_block);
        let i = self.builder.use_var(i_var);
        let one = self.builder.ins().iconst(types::I64, 1);
        let next_i = self.builder.ins().iadd(i, one);
        self.builder.def_var(i_var, next_i);
        self.builder.ins().jump(header, &[]);

        self.builder.switch_to_block(done);
        self.builder.seal_block(done);
        self.builder.seal_block(header);

        let out_count = self.builder.use_var(out_var);
        let result = self.call_helper("jit_build_list_from_buf", &[buf, out_count]);
        // RC: decref input list and function after building result
        if self.rc_enabled {
            self.emit_decref(list_val);
            self.emit_decref(fn_val);
        }
        Ok(result)
    }

    /// List.fold(list, init, fn) — loop: get item → call fn(acc, item) → update acc
    fn compile_hof_list_fold(&mut self, args: &[Expr]) -> Result<CValue, String> {
        if args.len() != 3 {
            return Err("List.fold requires 3 arguments".into());
        }
        let list_val = self.compile_expr(&args[0])?;
        let init_val = self.compile_expr(&args[1])?;
        let fn_val = self.compile_expr(&args[2])?;

        let len = self.call_helper("jit_list_length_raw", &[list_val]);

        let i_var = self.new_var();
        let acc_var = self.new_var();
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(i_var, zero);
        self.builder.def_var(acc_var, init_val);

        let header = self.builder.create_block();
        let body = self.builder.create_block();
        let done = self.builder.create_block();

        self.builder.ins().jump(header, &[]);
        self.builder.switch_to_block(header);

        let i = self.builder.use_var(i_var);
        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, i, len);
        self.builder.ins().brif(cmp, body, &[], done, &[]);

        self.builder.switch_to_block(body);
        self.builder.seal_block(body);

        let i = self.builder.use_var(i_var);
        let item = self.call_helper("jit_list_get_raw", &[list_val, i]);
        let acc = self.builder.use_var(acc_var);
        let new_acc = self.compile_call_value(fn_val, &[acc, item])?;
        self.builder.def_var(acc_var, new_acc);

        let i = self.builder.use_var(i_var);
        let one = self.builder.ins().iconst(types::I64, 1);
        let next_i = self.builder.ins().iadd(i, one);
        self.builder.def_var(i_var, next_i);
        self.builder.ins().jump(header, &[]);

        self.builder.switch_to_block(done);
        self.builder.seal_block(done);
        self.builder.seal_block(header);

        // RC: decref input list and function after fold completes
        if self.rc_enabled {
            self.emit_decref(list_val);
            self.emit_decref(fn_val);
        }
        Ok(self.builder.use_var(acc_var))
    }

    /// List.find(list, fn) — loop: get item → call fn → if truthy, return Some(item)
    fn compile_hof_list_find(&mut self, args: &[Expr]) -> Result<CValue, String> {
        if args.len() != 2 {
            return Err("List.find requires 2 arguments".into());
        }
        let list_val = self.compile_expr(&args[0])?;
        let fn_val = self.compile_expr(&args[1])?;

        let len = self.call_helper("jit_list_length_raw", &[list_val]);

        let i_var = self.new_var();
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.def_var(i_var, zero);

        let header = self.builder.create_block();
        let body = self.builder.create_block();
        let found_block = self.builder.create_block();
        let done = self.builder.create_block();
        self.builder.append_block_param(done, types::I64);

        self.builder.ins().jump(header, &[]);
        self.builder.switch_to_block(header);

        let i = self.builder.use_var(i_var);
        let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, i, len);
        // If not found, jump to done with None
        let none_val = self.call_helper("jit_make_none", &[]);
        self.builder
            .ins()
            .brif(cmp, body, &[], done, &[BlockArg::Value(none_val)]);

        self.builder.switch_to_block(body);
        self.builder.seal_block(body);

        let i = self.builder.use_var(i_var);
        let item = self.call_helper("jit_list_get_raw", &[list_val, i]);
        let pred_result = self.compile_call_value(fn_val, &[item])?;
        let is_true = self.call_helper("jit_is_truthy", &[pred_result]);
        let zero_cmp = self.builder.ins().iconst(types::I64, 0);
        let truthy = self.builder.ins().icmp(IntCC::NotEqual, is_true, zero_cmp);

        let cont_block = self.builder.create_block();
        self.builder
            .ins()
            .brif(truthy, found_block, &[], cont_block, &[]);

        // Found: return Some(item)
        self.builder.switch_to_block(found_block);
        self.builder.seal_block(found_block);
        let some_val = self.call_helper("jit_make_some", &[item]);
        self.builder.ins().jump(done, &[BlockArg::Value(some_val)]);

        // Continue loop
        self.builder.switch_to_block(cont_block);
        self.builder.seal_block(cont_block);
        let i = self.builder.use_var(i_var);
        let one = self.builder.ins().iconst(types::I64, 1);
        let next_i = self.builder.ins().iadd(i, one);
        self.builder.def_var(i_var, next_i);
        self.builder.ins().jump(header, &[]);

        self.builder.switch_to_block(done);
        self.builder.seal_block(done);
        self.builder.seal_block(header);

        // RC: decref input list and function after find completes
        if self.rc_enabled {
            self.emit_decref(list_val);
            self.emit_decref(fn_val);
        }
        Ok(self.builder.block_params(done)[0])
    }

    /// Option.map(opt, fn) — if Some: call fn(payload) → Some(result); else None
    fn compile_hof_option_map(&mut self, args: &[Expr]) -> Result<CValue, String> {
        if args.len() != 2 {
            return Err("Option.map requires 2 arguments".into());
        }
        let opt_val = self.compile_expr(&args[0])?;
        let fn_val = self.compile_expr(&args[1])?;

        let is_none = self.call_helper("jit_is_none", &[opt_val]);
        let cmp = self.is_truthy(is_none);

        let none_block = self.builder.create_block();
        let some_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        self.builder
            .ins()
            .brif(cmp, none_block, &[], some_block, &[]);

        // None path: pass through
        self.builder.switch_to_block(none_block);
        self.builder.seal_block(none_block);
        let none_result = self.call_helper("jit_make_none", &[]);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(none_result)]);

        // Some path: extract payload, call fn, wrap in Some
        self.builder.switch_to_block(some_block);
        self.builder.seal_block(some_block);
        let payload = self.call_helper("jit_enum_payload", &[opt_val]);
        let mapped = self.compile_call_value(fn_val, &[payload])?;
        let some_result = self.call_helper("jit_make_some", &[mapped]);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(some_result)]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        // RC: decref input option and function after map completes
        if self.rc_enabled {
            self.emit_decref(opt_val);
            self.emit_decref(fn_val);
        }
        Ok(self.builder.block_params(merge_block)[0])
    }

    /// Result.map(res, fn) — if Ok: call fn(payload) → Ok(result); else pass-through
    fn compile_hof_result_map(&mut self, args: &[Expr]) -> Result<CValue, String> {
        if args.len() != 2 {
            return Err("Result.map requires 2 arguments".into());
        }
        let res_val = self.compile_expr(&args[0])?;
        let fn_val = self.compile_expr(&args[1])?;

        let is_err = self.call_helper("jit_is_err", &[res_val]);
        let cmp = self.is_truthy(is_err);

        let err_block = self.builder.create_block();
        let ok_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        self.builder.ins().brif(cmp, err_block, &[], ok_block, &[]);

        // Err path: pass through as-is
        self.builder.switch_to_block(err_block);
        self.builder.seal_block(err_block);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(res_val)]);

        // Ok path: extract payload, call fn, wrap in Ok
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);
        let payload = self.call_helper("jit_enum_payload", &[res_val]);
        let mapped = self.compile_call_value(fn_val, &[payload])?;
        let ok_result = self.call_helper("jit_make_ok", &[mapped]);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(ok_result)]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        // RC: decref input result and function after map completes
        if self.rc_enabled {
            self.emit_decref(res_val);
            self.emit_decref(fn_val);
        }
        Ok(self.builder.block_params(merge_block)[0])
    }

    /// Result.map_err(res, fn) — if Err: call fn(payload) → Err(result); else pass-through
    fn compile_hof_result_map_err(&mut self, args: &[Expr]) -> Result<CValue, String> {
        if args.len() != 2 {
            return Err("Result.map_err requires 2 arguments".into());
        }
        let res_val = self.compile_expr(&args[0])?;
        let fn_val = self.compile_expr(&args[1])?;

        let is_err = self.call_helper("jit_is_err", &[res_val]);
        let cmp = self.is_truthy(is_err);

        let err_block = self.builder.create_block();
        let ok_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        self.builder.ins().brif(cmp, err_block, &[], ok_block, &[]);

        // Ok path: pass through as-is
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(res_val)]);

        // Err path: extract payload, call fn, wrap in Err
        self.builder.switch_to_block(err_block);
        self.builder.seal_block(err_block);
        let payload = self.call_helper("jit_enum_payload", &[res_val]);
        let mapped = self.compile_call_value(fn_val, &[payload])?;
        let err_result = self.call_helper("jit_make_err", &[mapped]);
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(err_result)]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        if self.rc_enabled {
            self.emit_decref(res_val);
            self.emit_decref(fn_val);
        }
        Ok(self.builder.block_params(merge_block)[0])
    }

    /// Create MemFlags with `readonly` set.
    ///
    /// Use this for loads from immutable data (constant pools, frozen record fields).
    /// Tells Cranelift the memory won't change, enabling load-load reordering and CSE.
    ///
    /// NOTE: Currently unused because all field access goes through opaque runtime
    /// helpers. When direct memory loads are added (bypassing `jit_get_field`), every
    /// load from immutable data MUST use these flags.
    #[allow(dead_code)]
    fn readonly_mem_flags() -> cranelift_codegen::ir::MemFlags {
        let mut flags = cranelift_codegen::ir::MemFlags::new();
        flags.set_readonly();
        flags.set_aligned();
        flags
    }

    // -- Match compilation --

    fn compile_match(&mut self, subject: &Expr, arms: &[MatchArm]) -> Result<CValue, String> {
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
                // Tuples always match structurally, but sub-patterns might need checking.
                // For simplicity, tuples always match (sub-patterns are just bindings).
                // More complex tuple patterns with nested literals would need recursive checking.
                let has_complex = sub_patterns
                    .iter()
                    .any(|p| !matches!(p, Pattern::Var(_) | Pattern::Wildcard));
                if has_complex {
                    // TODO: complex tuple pattern matching
                    self.builder.ins().jump(body_block, &[]);
                } else {
                    self.builder.ins().jump(body_block, &[]);
                }
            }
            Pattern::Record(_) => {
                // Record patterns always match structurally
                self.builder.ins().jump(body_block, &[]);
            }
            Pattern::List(_, _) => {
                return Err("List patterns not supported in JIT".into());
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
                        let idx = self.builder.ins().iconst(types::I64, i as i64);
                        let elem = self.call_helper("jit_enum_field_get", &[subject, idx]);
                        self.bind_pattern_vars(sub, elem)?;
                    }
                }
            }
            Pattern::Tuple(sub_patterns) => {
                for (i, sub) in sub_patterns.iter().enumerate() {
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
            Pattern::List(_, _) => {
                return Err("List patterns not supported in JIT".into());
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
                        let idx = self.builder.ins().iconst(types::I64, i as i64);
                        let elem = self.call_helper("jit_enum_field_get", &[subject, idx]);
                        self.bind_pattern_vars_rc(sub, elem)?;
                    }
                }
            }
            Pattern::Tuple(sub_patterns) => {
                for (i, sub) in sub_patterns.iter().enumerate() {
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
            Pattern::List(_, _) => {
                return Err("List patterns not supported in JIT".into());
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
            _ => return Err("Unsupported pattern in let binding".into()),
        }
        Ok(())
    }

    /// Bind a let-pattern with RC tracking: tracks bound variables in the
    /// current RC scope and decrefs wildcard/unused values.
    fn bind_pattern_rc(&mut self, pattern: &Pattern, val: CValue) -> Result<(), String> {
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
            _ => return Err("Unsupported pattern in let binding".into()),
        }
        Ok(())
    }

    // -- For loop compilation --

    fn compile_for(
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

    fn compile_try(&mut self, inner: &Expr) -> Result<CValue, String> {
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

    // -- SRA (Scalar Replacement of Aggregates) --

    /// Check if a variable only appears in GetField or UpdateRecord base positions.
    /// Returns false (escapes) if the variable is used in any other context.
    fn var_escapes_in_expr(expr: &Expr, name: &str) -> bool {
        match expr {
            Expr::Var(n, _) if n == name => true, // bare use = escape
            Expr::GetField { object, .. } => {
                // GetField on our var is fine — but check if var appears elsewhere in field subtree
                if let Expr::Var(n, _) = object.as_ref() {
                    if n == name {
                        return false; // this is a safe use
                    }
                }
                Self::var_escapes_in_expr(object, name)
            }
            Expr::UpdateRecord { base, updates, .. } => {
                // UpdateRecord with our var as base is fine
                let base_escapes = if let Expr::Var(n, _) = base.as_ref() {
                    if n == name {
                        false // safe use
                    } else {
                        Self::var_escapes_in_expr(base, name)
                    }
                } else {
                    Self::var_escapes_in_expr(base, name)
                };
                if base_escapes {
                    return true;
                }
                updates
                    .iter()
                    .any(|(_, v)| Self::var_escapes_in_expr(v, name))
            }
            // Recurse into all children
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::String(_)
            | Expr::Bool(_)
            | Expr::Unit
            | Expr::Hole => false,
            Expr::Var(_, _) => false, // different name
            Expr::BinOp { lhs, rhs, .. } => {
                Self::var_escapes_in_expr(lhs, name) || Self::var_escapes_in_expr(rhs, name)
            }
            Expr::UnaryOp { operand, .. } => Self::var_escapes_in_expr(operand, name),
            Expr::And(l, r) | Expr::Or(l, r) => {
                Self::var_escapes_in_expr(l, name) || Self::var_escapes_in_expr(r, name)
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                Self::var_escapes_in_expr(condition, name)
                    || Self::var_escapes_in_expr(then_branch, name)
                    || else_branch
                        .as_ref()
                        .is_some_and(|e| Self::var_escapes_in_expr(e, name))
            }
            Expr::Block(exprs, _) => exprs.iter().any(|e| Self::var_escapes_in_expr(e, name)),
            Expr::Let { value, .. } => Self::var_escapes_in_expr(value, name),
            Expr::CallDirect { args, .. }
            | Expr::CallNative { args, .. }
            | Expr::TailCall { args, .. } => {
                args.iter().any(|a| Self::var_escapes_in_expr(a, name))
            }
            Expr::CallIndirect { callee, args, .. }
            | Expr::TailCallIndirect { callee, args, .. } => {
                Self::var_escapes_in_expr(callee, name)
                    || args.iter().any(|a| Self::var_escapes_in_expr(a, name))
            }
            Expr::MakeEnum { payload, .. } => Self::var_escapes_in_expr(payload, name),
            Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => fields
                .iter()
                .any(|(_, v)| Self::var_escapes_in_expr(v, name)),
            Expr::MakeList(items, _) | Expr::MakeTuple(items, _) => {
                items.iter().any(|e| Self::var_escapes_in_expr(e, name))
            }
            Expr::MakeRange(s, e) => {
                Self::var_escapes_in_expr(s, name) || Self::var_escapes_in_expr(e, name)
            }
            Expr::Match { subject, arms, .. } => {
                Self::var_escapes_in_expr(subject, name)
                    || arms
                        .iter()
                        .any(|a| Self::var_escapes_in_expr(&a.body, name))
            }
            Expr::For { iterable, body, .. } => {
                Self::var_escapes_in_expr(iterable, name) || Self::var_escapes_in_expr(body, name)
            }
            Expr::Lambda { body, .. } => Self::var_escapes_in_expr(body, name),
            Expr::Try { expr, .. } => Self::var_escapes_in_expr(expr, name),
            Expr::Concat(parts) => parts.iter().any(|p| Self::var_escapes_in_expr(p, name)),
            Expr::MakeClosure { captures, .. } => {
                captures.iter().any(|c| Self::var_escapes_in_expr(c, name))
            }
            Expr::GetClosureVar(_) => false,
            _ => true, // conservative: escape for anything else
        }
    }

    /// Try to compile an expression as an SRA Let binding.
    /// Returns `true` if the expression was intercepted and compiled as scalar
    /// field variables (skipping the MakeRecord allocation), `false` otherwise.
    fn try_compile_sra_let(
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
    fn try_gen_enum_update(
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
    /// variable never escapes (only used in GetField/UpdateRecord).
    pub(super) fn find_sra_candidates(exprs: &[Expr]) -> Vec<(String, Vec<String>)> {
        let mut candidates = Vec::new();
        for (i, expr) in exprs.iter().enumerate() {
            if let Expr::Let { pattern, value, .. } = expr {
                if let Pattern::Var(name) = pattern.as_ref() {
                    if let Expr::MakeRecord(fields, _) = value.as_ref() {
                        let field_names: Vec<String> =
                            fields.iter().map(|(k, _)| k.clone()).collect();
                        // Check if variable escapes in subsequent expressions
                        let escapes = exprs[i + 1..]
                            .iter()
                            .any(|e| Self::var_escapes_in_expr(e, name));
                        if !escapes {
                            candidates.push((name.clone(), field_names));
                        }
                    }
                }
            }
        }
        candidates
    }
}
