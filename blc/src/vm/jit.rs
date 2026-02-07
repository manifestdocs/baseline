//! Cranelift JIT compiler for Baseline IR.
//!
//! Compiles `IrFunction`s to native machine code. Functions that contain
//! unsupported constructs (closures, match, string ops, etc.) silently
//! fall back to the bytecode interpreter.

use std::collections::HashMap;

use cranelift_codegen::ir::{AbiParam, BlockArg, InstBuilder, types};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use super::ir::{BinOp, Expr, IrFunction, IrModule, UnaryOp};
use crate::analysis::types::Type;

// ---------------------------------------------------------------------------
// JIT Program
// ---------------------------------------------------------------------------

/// A JIT-compiled program. Holds the compiled module and a dispatch table
/// mapping function index → native function pointer.
pub struct JitProgram {
    /// Owns the compiled code pages (freed on drop).
    _module: JITModule,
    /// function index → native fn pointer (None = fell back to interpreter).
    dispatch: Vec<Option<*const u8>>,
    /// Entry function index.
    pub entry: usize,
}

// SAFETY: The JITModule owns memory-mapped code pages. The function pointers
// in `dispatch` point into those pages and remain valid for the lifetime of
// `_module`. We only call them on the same thread.
unsafe impl Send for JitProgram {}

impl JitProgram {
    /// Get the native function pointer for a given function index.
    /// Returns None if the function was not JIT-compiled.
    pub fn get_fn(&self, idx: usize) -> Option<*const u8> {
        self.dispatch.get(idx).copied().flatten()
    }

    /// Execute the entry function (must be a 0-arg function returning i64).
    pub fn run_entry(&self) -> Option<i64> {
        let ptr = self.get_fn(self.entry)?;
        // SAFETY: The entry function was compiled with signature () -> i64.
        // The pointer is valid for the lifetime of self._module.
        let func: fn() -> i64 = unsafe { std::mem::transmute(ptr) };
        Some(func())
    }
}

// ---------------------------------------------------------------------------
// JIT Compiler
// ---------------------------------------------------------------------------

/// Compiles an IrModule to native code via Cranelift.
pub fn compile(module: &IrModule, trace: bool) -> Result<JitProgram, String> {
    let mut flag_builder = settings::builder();
    flag_builder
        .set("opt_level", "speed")
        .map_err(|e| e.to_string())?;
    flag_builder
        .set("preserve_frame_pointers", "false")
        .map_err(|e| e.to_string())?;
    flag_builder
        .set("enable_probestack", "false")
        .map_err(|e| e.to_string())?;

    let isa_builder =
        cranelift_native::builder().map_err(|e| format!("Native ISA not available: {}", e))?;
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .map_err(|e| e.to_string())?;

    let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

    let mut jit_module = JITModule::new(builder);
    let ptr_type = jit_module.target_config().pointer_type();

    // Phase 1: Declare all functions so we can resolve cross-references
    let mut func_ids: Vec<Option<FuncId>> = Vec::with_capacity(module.functions.len());
    let mut compilable: Vec<bool> = Vec::with_capacity(module.functions.len());

    for func in &module.functions {
        if can_jit(func) {
            let sig = build_signature(&mut jit_module, func.params.len(), ptr_type);
            let id = jit_module
                .declare_function(&func.name, Linkage::Local, &sig)
                .map_err(|e| e.to_string())?;
            func_ids.push(Some(id));
            compilable.push(true);
        } else {
            func_ids.push(None);
            compilable.push(false);
            if trace {
                eprintln!("JIT: fallback for '{}' (unsupported constructs)", func.name);
            }
        }
    }

    // Phase 2: Compile each function
    let mut fb_ctx = FunctionBuilderContext::new();

    for (i, func) in module.functions.iter().enumerate() {
        if !compilable[i] {
            continue;
        }
        let func_id = func_ids[i].unwrap();
        let start = std::time::Instant::now();

        let mut cl_func = cranelift_codegen::ir::Function::new();
        cl_func.signature = build_signature(&mut jit_module, func.params.len(), ptr_type);

        let func_names: HashMap<String, usize> = module
            .functions
            .iter()
            .enumerate()
            .map(|(idx, f)| (f.name.clone(), idx))
            .collect();

        let compile_result = {
            let mut builder = FunctionBuilder::new(&mut cl_func, &mut fb_ctx);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Declare param variables (before creating ctx)
            let mut param_vars = Vec::new();
            let mut vars_map = HashMap::new();
            for (p, param_name) in func.params.iter().enumerate() {
                let var = builder.declare_var(types::I64);
                let param_val = builder.block_params(entry_block)[p];
                builder.def_var(var, param_val);
                vars_map.insert(param_name.clone(), var);
                param_vars.push(var);
            }

            // For tail-recursive functions: create a loop header block.
            // Self-tail-calls jump back here instead of making real calls.
            let is_tail_recursive = has_self_tail_call(&func.body, &func.name);
            let loop_header = if is_tail_recursive {
                let lh = builder.create_block();
                // Don't seal lh yet — tail-call sites will jump back to it
                builder.ins().jump(lh, &[]);
                builder.switch_to_block(lh);
                Some(lh)
            } else {
                None
            };

            let mut ctx = FnCompileCtx {
                builder: &mut builder,
                func_ids: &func_ids,
                module: &mut jit_module,
                vars: vars_map,
                next_var: param_vars.len() as u32,
                func_names: &func_names,
                ir_functions: &module.functions,
                current_func_name: func.name.clone(),
                param_vars,
                loop_header,
            };

            // Compile body
            let result = ctx.compile_expr(&func.body);
            let saved_loop_header = ctx.loop_header;
            drop(ctx); // Release borrow on builder

            match result {
                Ok(val) => {
                    builder.ins().return_(&[val]);
                    // Seal loop header now — all predecessors (entry + tail-call sites) are known
                    if let Some(lh) = saved_loop_header {
                        builder.seal_block(lh);
                    }
                    builder.finalize();
                    Ok(())
                }
                Err(e) => Err(e),
            }
        };

        match compile_result {
            Ok(()) => {}
            Err(e) => {
                if trace {
                    eprintln!("JIT: fallback for '{}': {}", func.name, e);
                }
                compilable[i] = false;
                continue;
            }
        }

        let mut ctx = cranelift_codegen::Context::for_function(cl_func);
        jit_module
            .define_function(func_id, &mut ctx)
            .map_err(|e| format!("JIT compile error in '{}': {}", func.name, e))?;

        if trace {
            let elapsed = start.elapsed();
            eprintln!(
                "JIT: compiled '{}' ({:.1}ms)",
                func.name,
                elapsed.as_secs_f64() * 1000.0
            );
        }
    }

    // Phase 3: Finalize and get function pointers
    jit_module
        .finalize_definitions()
        .map_err(|e| e.to_string())?;

    let mut dispatch: Vec<Option<*const u8>> = Vec::with_capacity(module.functions.len());
    for (i, _) in module.functions.iter().enumerate() {
        if compilable[i] {
            if let Some(func_id) = func_ids[i] {
                let ptr = jit_module.get_finalized_function(func_id);
                dispatch.push(Some(ptr));
            } else {
                dispatch.push(None);
            }
        } else {
            dispatch.push(None);
        }
    }

    Ok(JitProgram {
        _module: jit_module,
        dispatch,
        entry: module.entry,
    })
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn build_signature(
    module: &mut JITModule,
    param_count: usize,
    _ptr_type: cranelift_codegen::ir::Type,
) -> cranelift_codegen::ir::Signature {
    let mut sig = module.make_signature();
    sig.call_conv = CallConv::Fast;
    for _ in 0..param_count {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I64));
    sig
}

/// Check if a function can be JIT-compiled (no unsupported constructs).
fn can_jit(func: &IrFunction) -> bool {
    expr_can_jit(&func.body)
}

/// Check if an expression contains a self-tail-call to the given function name.
fn has_self_tail_call(expr: &Expr, name: &str) -> bool {
    match expr {
        Expr::TailCall {
            name: call_name, ..
        } => call_name == name,
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            has_self_tail_call(condition, name)
                || has_self_tail_call(then_branch, name)
                || else_branch
                    .as_ref()
                    .is_some_and(|e| has_self_tail_call(e, name))
        }
        Expr::Block(exprs, _) => exprs.iter().any(|e| has_self_tail_call(e, name)),
        Expr::Let { value, .. } => has_self_tail_call(value, name),
        Expr::BinOp { lhs, rhs, .. } => {
            has_self_tail_call(lhs, name) || has_self_tail_call(rhs, name)
        }
        Expr::UnaryOp { operand, .. } => has_self_tail_call(operand, name),
        Expr::And(a, b) | Expr::Or(a, b) => {
            has_self_tail_call(a, name) || has_self_tail_call(b, name)
        }
        Expr::CallDirect { args, .. } => args.iter().any(|a| has_self_tail_call(a, name)),
        _ => false,
    }
}

fn expr_can_jit(expr: &Expr) -> bool {
    match expr {
        Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::Unit => true,
        Expr::Var(_, _) => true,
        Expr::BinOp { lhs, rhs, .. } => expr_can_jit(lhs) && expr_can_jit(rhs),
        Expr::UnaryOp { operand, .. } => expr_can_jit(operand),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            expr_can_jit(condition)
                && expr_can_jit(then_branch)
                && else_branch.as_ref().is_none_or(|e| expr_can_jit(e))
        }
        Expr::CallDirect { args, .. } => args.iter().all(expr_can_jit),
        Expr::TailCall { args, .. } => args.iter().all(expr_can_jit),
        Expr::Let { value, .. } => expr_can_jit(value),
        Expr::Block(exprs, _) => exprs.iter().all(expr_can_jit),
        Expr::And(a, b) | Expr::Or(a, b) => expr_can_jit(a) && expr_can_jit(b),
        // Unsupported constructs
        Expr::String(_)
        | Expr::Concat(_)
        | Expr::CallNative { .. }
        | Expr::CallIndirect { .. }
        | Expr::Lambda { .. }
        | Expr::Match { .. }
        | Expr::For { .. }
        | Expr::Try { .. }
        | Expr::MakeEnum { .. }
        | Expr::MakeStruct { .. }
        | Expr::MakeList(_, _)
        | Expr::MakeRecord(_, _)
        | Expr::MakeTuple(_, _)
        | Expr::MakeRange(_, _)
        | Expr::UpdateRecord { .. }
        | Expr::GetField { .. } => false,
    }
}

/// Check if an expression only references the given parameter names
/// (no local variables or complex sub-expressions).
fn expr_only_refs_params(expr: &Expr, params: &[String]) -> bool {
    match expr {
        Expr::Var(name, _) => params.iter().any(|p| p == name),
        Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::Unit => true,
        Expr::BinOp { lhs, rhs, .. } => {
            expr_only_refs_params(lhs, params) && expr_only_refs_params(rhs, params)
        }
        Expr::UnaryOp { operand, .. } => expr_only_refs_params(operand, params),
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Function compile context
// ---------------------------------------------------------------------------

#[allow(dead_code)]
struct FnCompileCtx<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    func_ids: &'a [Option<FuncId>],
    module: &'a mut JITModule,
    vars: HashMap<String, Variable>,
    next_var: u32,
    func_names: &'a HashMap<String, usize>,
    /// All IR functions (for base-case speculation at call sites).
    ir_functions: &'a [IrFunction],
    /// Current function name (for detecting self-tail-calls).
    current_func_name: String,
    /// Parameter variables (for self-tail-call → loop conversion).
    param_vars: Vec<Variable>,
    /// Loop header block for tail-recursive functions.
    loop_header: Option<cranelift_codegen::ir::Block>,
}

impl<'a, 'b> FnCompileCtx<'a, 'b> {
    fn new_var(&mut self) -> Variable {
        let var = self.builder.declare_var(types::I64);
        self.next_var += 1;
        var
    }

    /// Base-case speculation: for functions with pattern `if cond then simple_base else ...`,
    /// inline the guard check at call sites to avoid function calls for base cases.
    /// Returns Some(value) if speculation was emitted, None if not applicable.
    fn try_speculate_call(
        &mut self,
        name: &str,
        ir_args: &[Expr],
    ) -> Result<Option<cranelift_codegen::ir::Value>, String> {
        let func_idx = match self.func_names.get(name) {
            Some(&idx) => idx,
            None => return Ok(None),
        };
        if self.func_ids[func_idx].is_none() {
            return Ok(None);
        }
        let func = &self.ir_functions[func_idx];

        // Extract base case: body must be If { cond, then: simple_val, else: ... }
        let (cond_expr, base_expr) = match &func.body {
            Expr::If {
                condition,
                then_branch,
                ..
            } => match then_branch.as_ref() {
                Expr::Var(_, _) | Expr::Int(_) | Expr::Bool(_) | Expr::Unit => {
                    (condition.as_ref(), then_branch.as_ref())
                }
                _ => return Ok(None),
            },
            _ => return Ok(None),
        };

        // Guard and base value must only reference params (no locals)
        if !expr_only_refs_params(cond_expr, &func.params) {
            return Ok(None);
        }
        if !expr_only_refs_params(base_expr, &func.params) {
            return Ok(None);
        }

        // Compile the IR arguments
        let arg_vals: Vec<cranelift_codegen::ir::Value> = ir_args
            .iter()
            .map(|a| self.compile_expr(a))
            .collect::<Result<_, _>>()?;

        // Temporarily bind param names to arg values
        let mut saved_vars = HashMap::new();
        for (i, param_name) in func.params.iter().enumerate() {
            let var = self.new_var();
            self.builder.def_var(var, arg_vals[i]);
            if let Some(old) = self.vars.insert(param_name.clone(), var) {
                saved_vars.insert(param_name.clone(), old);
            }
        }

        // Compile condition and base value with params bound to args
        let cond_val = self.compile_expr(cond_expr)?;
        let base_val = self.compile_expr(base_expr)?;

        // Restore vars
        for param_name in &func.params {
            self.vars.remove(param_name);
        }
        for (name, old_var) in saved_vars {
            self.vars.insert(name, old_var);
        }

        // Emit: if cond then base_val else call func(arg_vals)
        let call_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);

        let zero = self.builder.ins().iconst(types::I64, 0);
        let cmp = self.builder.ins().icmp(
            cranelift_codegen::ir::condcodes::IntCC::NotEqual,
            cond_val,
            zero,
        );
        self.builder.ins().brif(
            cmp,
            merge_block,
            &[BlockArg::Value(base_val)],
            call_block,
            &[],
        );

        // Call block: fall back to actual function call
        self.builder.switch_to_block(call_block);
        self.builder.seal_block(call_block);
        let func_id = self.func_ids[func_idx].unwrap();
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, &arg_vals);
        let call_result = self.builder.inst_results(call)[0];
        self.builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(call_result)]);

        // Merge
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(Some(self.builder.block_params(merge_block)[0]))
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<cranelift_codegen::ir::Value, String> {
        match expr {
            Expr::Int(n) => Ok(self.builder.ins().iconst(types::I64, *n)),

            Expr::Float(f) => {
                // Encode as NaN-boxed: the raw bits of the f64
                let bits = f.to_bits() as i64;
                Ok(self.builder.ins().iconst(types::I64, bits))
            }

            Expr::Bool(b) => {
                // NaN-boxed bool: use our encoding convention
                // true = NValue::bool(true), false = NValue::bool(false)
                // For integer fast path, just use 1/0
                let val = if *b { 1i64 } else { 0i64 };
                Ok(self.builder.ins().iconst(types::I64, val))
            }

            Expr::Unit => Ok(self.builder.ins().iconst(types::I64, 0)),

            Expr::Var(name, _) => {
                if let Some(&var) = self.vars.get(name) {
                    Ok(self.builder.use_var(var))
                } else if let Some(&func_idx) = self.func_names.get(name) {
                    // Function reference — return the function index as a value
                    // (won't be called directly, this is for passing as value)
                    Ok(self.builder.ins().iconst(types::I64, func_idx as i64))
                } else {
                    Err(format!("Unknown variable: {}", name))
                }
            }

            Expr::BinOp { op, lhs, rhs, ty } => {
                let is_int = matches!(ty, Some(Type::Int));
                let lhs_val = self.compile_expr(lhs)?;
                let rhs_val = self.compile_expr(rhs)?;

                if is_int {
                    match op {
                        BinOp::Add => Ok(self.builder.ins().iadd(lhs_val, rhs_val)),
                        BinOp::Sub => Ok(self.builder.ins().isub(lhs_val, rhs_val)),
                        BinOp::Mul => Ok(self.builder.ins().imul(lhs_val, rhs_val)),
                        BinOp::Div => Ok(self.builder.ins().sdiv(lhs_val, rhs_val)),
                        BinOp::Mod => Ok(self.builder.ins().srem(lhs_val, rhs_val)),
                        BinOp::Eq => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::Equal,
                                lhs_val,
                                rhs_val,
                            );
                            // Extend bool to i64 (0 or 1)
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Ne => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::NotEqual,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Lt => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::SignedLessThan,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Gt => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::SignedGreaterThan,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Le => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::SignedLessThanOrEqual,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Ge => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::SignedGreaterThanOrEqual,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                    }
                } else {
                    // Non-int path: for now, treat as i64 arithmetic (works for Bool comparisons)
                    match op {
                        BinOp::Add => Ok(self.builder.ins().iadd(lhs_val, rhs_val)),
                        BinOp::Sub => Ok(self.builder.ins().isub(lhs_val, rhs_val)),
                        BinOp::Mul => Ok(self.builder.ins().imul(lhs_val, rhs_val)),
                        BinOp::Div => Ok(self.builder.ins().sdiv(lhs_val, rhs_val)),
                        BinOp::Mod => Ok(self.builder.ins().srem(lhs_val, rhs_val)),
                        BinOp::Eq => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::Equal,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Ne => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::NotEqual,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Lt => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::SignedLessThan,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Gt => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::SignedGreaterThan,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Le => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::SignedLessThanOrEqual,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                        BinOp::Ge => {
                            let cmp = self.builder.ins().icmp(
                                cranelift_codegen::ir::condcodes::IntCC::SignedGreaterThanOrEqual,
                                lhs_val,
                                rhs_val,
                            );
                            Ok(self.builder.ins().uextend(types::I64, cmp))
                        }
                    }
                }
            }

            Expr::UnaryOp { op, operand, .. } => {
                let val = self.compile_expr(operand)?;
                match op {
                    UnaryOp::Neg => Ok(self.builder.ins().ineg(val)),
                    UnaryOp::Not => {
                        let zero = self.builder.ins().iconst(types::I64, 0);
                        let cmp = self.builder.ins().icmp(
                            cranelift_codegen::ir::condcodes::IntCC::Equal,
                            val,
                            zero,
                        );
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
                let cond_val = self.compile_expr(condition)?;

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                self.builder.append_block_param(merge_block, types::I64);

                // Branch on condition != 0
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(
                    cranelift_codegen::ir::condcodes::IntCC::NotEqual,
                    cond_val,
                    zero,
                );
                self.builder
                    .ins()
                    .brif(cmp, then_block, &[], else_block, &[]);

                // Then block
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_val = self.compile_expr(then_branch)?;
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(then_val)]);

                // Else block
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_val = if let Some(else_expr) = else_branch {
                    self.compile_expr(else_expr)?
                } else {
                    self.builder.ins().iconst(types::I64, 0) // Unit
                };
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(else_val)]);

                // Merge block
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(self.builder.block_params(merge_block)[0])
            }

            Expr::Let { pattern, value, .. } => {
                let val = self.compile_expr(value)?;
                match pattern.as_ref() {
                    super::ir::Pattern::Var(name) => {
                        let var = self.new_var();
                        self.builder.def_var(var, val);
                        self.vars.insert(name.clone(), var);
                    }
                    super::ir::Pattern::Wildcard => {
                        // Discard the value
                    }
                    _ => return Err("Unsupported pattern in let binding".into()),
                }
                // Let returns unit
                Ok(self.builder.ins().iconst(types::I64, 0))
            }

            Expr::Block(exprs, _) => {
                let mut result = self.builder.ins().iconst(types::I64, 0);
                for expr in exprs {
                    result = self.compile_expr(expr)?;
                }
                Ok(result)
            }

            Expr::CallDirect { name, args, .. } => {
                // Try base-case speculation: inline the guard check to avoid
                // function calls for base cases (e.g., tak's y >= x → z)
                if let Some(val) = self.try_speculate_call(name, args)? {
                    return Ok(val);
                }

                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
                        let arg_vals: Vec<cranelift_codegen::ir::Value> = args
                            .iter()
                            .map(|a| self.compile_expr(a))
                            .collect::<Result<Vec<_>, _>>()?;
                        let call = self.builder.ins().call(func_ref, &arg_vals);
                        Ok(self.builder.inst_results(call)[0])
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
                // Self-tail-call → loop: update params and jump to loop header
                if name == &self.current_func_name
                    && let Some(loop_header) = self.loop_header
                {
                    // Evaluate all new argument values first (before updating any vars)
                    let arg_vals: Vec<cranelift_codegen::ir::Value> = args
                        .iter()
                        .map(|a| self.compile_expr(a))
                        .collect::<Result<Vec<_>, _>>()?;

                    // Update param variables with new values
                    for (var, val) in self.param_vars.iter().zip(arg_vals.iter()) {
                        self.builder.def_var(*var, *val);
                    }

                    // Jump back to loop header (next iteration)
                    self.builder.ins().jump(loop_header, &[]);

                    // Switch to unreachable block so subsequent code has
                    // somewhere to emit into (Cranelift requires it)
                    let dead_block = self.builder.create_block();
                    self.builder.switch_to_block(dead_block);
                    self.builder.seal_block(dead_block);

                    // Return dummy value (this path is unreachable at runtime)
                    return Ok(self.builder.ins().iconst(types::I64, 0));
                }

                // Non-self tail call: compile as regular call
                if let Some(&func_idx) = self.func_names.get(name) {
                    if let Some(func_id) = self.func_ids[func_idx] {
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
                        let arg_vals: Vec<cranelift_codegen::ir::Value> = args
                            .iter()
                            .map(|a| self.compile_expr(a))
                            .collect::<Result<Vec<_>, _>>()?;
                        let call = self.builder.ins().call(func_ref, &arg_vals);
                        Ok(self.builder.inst_results(call)[0])
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
                // Short-circuit: if a is falsy, return 0; else evaluate b
                let a_val = self.compile_expr(a)?;
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(
                    cranelift_codegen::ir::condcodes::IntCC::NotEqual,
                    a_val,
                    zero,
                );

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                self.builder
                    .ins()
                    .brif(cmp, eval_b, &[], merge, &[BlockArg::Value(zero)]);

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
                let zero = self.builder.ins().iconst(types::I64, 0);
                let cmp = self.builder.ins().icmp(
                    cranelift_codegen::ir::condcodes::IntCC::NotEqual,
                    a_val,
                    zero,
                );

                let eval_b = self.builder.create_block();
                let merge = self.builder.create_block();
                self.builder.append_block_param(merge, types::I64);

                let one = self.builder.ins().iconst(types::I64, 1);
                self.builder
                    .ins()
                    .brif(cmp, merge, &[BlockArg::Value(one)], eval_b, &[]);

                self.builder.switch_to_block(eval_b);
                self.builder.seal_block(eval_b);
                let b_val = self.compile_expr(b)?;
                self.builder.ins().jump(merge, &[BlockArg::Value(b_val)]);

                self.builder.switch_to_block(merge);
                self.builder.seal_block(merge);
                Ok(self.builder.block_params(merge)[0])
            }

            _ => Err(format!("Unsupported expression in JIT: {:?}", expr)),
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::ir::{IrFunction, IrModule, Span};

    fn dummy_span() -> Span {
        Span {
            line: 0,
            col: 0,
            start_byte: 0,
            end_byte: 0,
        }
    }

    fn make_int(n: i64) -> Expr {
        Expr::Int(n)
    }

    fn make_var(name: &str) -> Expr {
        Expr::Var(name.to_string(), Some(Type::Int))
    }

    fn make_binop(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
        Expr::BinOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            ty: Some(Type::Int),
        }
    }

    fn make_bool_binop(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
        Expr::BinOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            ty: Some(Type::Bool),
        }
    }

    fn compile_and_run(module: &IrModule) -> i64 {
        let program = compile(module, false).expect("JIT compilation failed");
        program.run_entry().expect("Entry function not compiled")
    }

    #[test]
    fn jit_integer_constant() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: make_int(42),
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 42);
    }

    #[test]
    fn jit_add() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: make_binop(BinOp::Add, make_int(3), make_int(4)),
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 7);
    }

    #[test]
    fn jit_sub() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: make_binop(BinOp::Sub, make_int(10), make_int(3)),
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 7);
    }

    #[test]
    fn jit_mul() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: make_binop(BinOp::Mul, make_int(6), make_int(7)),
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 42);
    }

    #[test]
    fn jit_comparison_le() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: make_bool_binop(BinOp::Le, make_int(3), make_int(5)),
                ty: Some(Type::Bool),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 1); // true
    }

    #[test]
    fn jit_comparison_le_false() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: make_bool_binop(BinOp::Le, make_int(10), make_int(5)),
                ty: Some(Type::Bool),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 0); // false
    }

    #[test]
    fn jit_if_else() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::If {
                    condition: Box::new(make_bool_binop(BinOp::Le, make_int(1), make_int(5))),
                    then_branch: Box::new(make_int(100)),
                    else_branch: Some(Box::new(make_int(200))),
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 100);
    }

    #[test]
    fn jit_function_call() {
        // main calls double(21) where double(n) = n + n
        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "double".into(),
                        args: vec![make_int(21)],
                        ty: Some(Type::Int),
                    },
                    ty: Some(Type::Int),
                    span: dummy_span(),
                },
                IrFunction {
                    name: "double".into(),
                    params: vec!["n".into()],
                    body: make_binop(BinOp::Add, make_var("n"), make_var("n")),
                    ty: Some(Type::Int),
                    span: dummy_span(),
                },
            ],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 42);
    }

    #[test]
    fn jit_recursive_fib10() {
        // fib(n) = if n <= 1 then n else fib(n-1) + fib(n-2)
        // main = fib(10) = 55
        let fib_body = Expr::If {
            condition: Box::new(make_bool_binop(BinOp::Le, make_var("n"), make_int(1))),
            then_branch: Box::new(make_var("n")),
            else_branch: Some(Box::new(make_binop(
                BinOp::Add,
                Expr::CallDirect {
                    name: "fib".into(),
                    args: vec![make_binop(BinOp::Sub, make_var("n"), make_int(1))],
                    ty: Some(Type::Int),
                },
                Expr::CallDirect {
                    name: "fib".into(),
                    args: vec![make_binop(BinOp::Sub, make_var("n"), make_int(2))],
                    ty: Some(Type::Int),
                },
            ))),
            ty: Some(Type::Int),
        };

        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "fib".into(),
                    params: vec!["n".into()],
                    body: fib_body,
                    ty: Some(Type::Int),
                    span: dummy_span(),
                },
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "fib".into(),
                        args: vec![make_int(10)],
                        ty: Some(Type::Int),
                    },
                    ty: Some(Type::Int),
                    span: dummy_span(),
                },
            ],
            entry: 1,
        };
        assert_eq!(compile_and_run(&module), 55);
    }

    #[test]
    fn jit_let_binding() {
        // main = let x = 10; let y = 20; x + y
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Block(
                    vec![
                        Expr::Let {
                            pattern: Box::new(super::super::ir::Pattern::Var("x".into())),
                            value: Box::new(make_int(10)),
                            ty: Some(Type::Int),
                        },
                        Expr::Let {
                            pattern: Box::new(super::super::ir::Pattern::Var("y".into())),
                            value: Box::new(make_int(20)),
                            ty: Some(Type::Int),
                        },
                        make_binop(BinOp::Add, make_var("x"), make_var("y")),
                    ],
                    Some(Type::Int),
                ),
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 30);
    }

    #[test]
    fn jit_negation() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    operand: Box::new(make_int(42)),
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), -42);
    }

    #[test]
    fn jit_fallback_for_unsupported() {
        // A function with string operations should not be JIT-compiled
        let func = IrFunction {
            name: "greeting".into(),
            params: vec![],
            body: Expr::String("hello".into()),
            ty: None,
            span: dummy_span(),
        };
        assert!(!can_jit(&func));
    }

    #[test]
    fn jit_tail_call_as_regular_call() {
        // tail call compiled as regular call
        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "count".into(),
                    params: vec!["n".into()],
                    body: Expr::If {
                        condition: Box::new(make_bool_binop(BinOp::Le, make_var("n"), make_int(0))),
                        then_branch: Box::new(make_int(0)),
                        else_branch: Some(Box::new(Expr::TailCall {
                            name: "count".into(),
                            args: vec![make_binop(BinOp::Sub, make_var("n"), make_int(1))],
                            ty: Some(Type::Int),
                        })),
                        ty: Some(Type::Int),
                    },
                    ty: Some(Type::Int),
                    span: dummy_span(),
                },
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "count".into(),
                        args: vec![make_int(100)],
                        ty: Some(Type::Int),
                    },
                    ty: Some(Type::Int),
                    span: dummy_span(),
                },
            ],
            entry: 1,
        };
        assert_eq!(compile_and_run(&module), 0);
    }
}
