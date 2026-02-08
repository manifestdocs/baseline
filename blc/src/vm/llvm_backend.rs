//! LLVM JIT backend for the Baseline IR.
//!
//! Compiles IrModule → LLVM IR → optimized native code using inkwell.
//! All functions use i64 (raw unboxed integers). NaN-boxing is NOT used —
//! this backend targets pure-integer benchmarks for maximum performance.
//! For heap types (strings, lists, records), fall back to the Cranelift JIT.

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::IntType;
use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

use super::ir::{BinOp, Expr, IrFunction, IrModule, Pattern, UnaryOp};

type MainFunc = unsafe extern "C" fn() -> i64;

/// Compiled LLVM program, ready to execute.
pub struct LlvmProgram {
    /// Keeps the execution engine (and compiled code) alive.
    _engine: ExecutionEngine<'static>,
    /// The JIT-compiled entry function.
    entry_fn: JitFunction<'static, MainFunc>,
    /// Leaked context — lives for the duration of the program.
    _context: &'static Context,
}

impl LlvmProgram {
    /// Execute the entry function and return the result.
    pub fn run_entry(&self) -> i64 {
        unsafe { self.entry_fn.call() }
    }
}

/// Compile an IrModule to native code via LLVM.
///
/// Only supports scalar-only functions (Int, Bool, Unit, Float arithmetic).
/// Returns an error if unsupported IR features are encountered.
pub fn compile(ir_module: &IrModule, trace: bool) -> Result<LlvmProgram, String> {
    Target::initialize_native(&InitializationConfig::default())
        .map_err(|e| format!("Failed to initialize LLVM native target: {}", e))?;

    // Leak the context so it lives as long as the program.
    let context: &'static Context = Box::leak(Box::new(Context::create()));
    let module = context.create_module("baseline");
    let builder = context.create_builder();
    let i64_type = context.i64_type();

    // Build function name → index mapping
    let func_names: HashMap<String, usize> = ir_module
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| (f.name.clone(), i))
        .collect();

    // Declare all functions first (for forward references)
    let mut llvm_fns: Vec<FunctionValue<'static>> = Vec::new();
    for func in &ir_module.functions {
        let param_types: Vec<_> = func.params.iter().map(|_| i64_type.into()).collect();
        let fn_type = i64_type.fn_type(&param_types, false);
        let llvm_fn = module.add_function(&func.name, fn_type, None);
        llvm_fns.push(llvm_fn);
    }

    // Compile each function body
    for (i, func) in ir_module.functions.iter().enumerate() {
        compile_function(
            context, &builder, &module, i64_type, func, llvm_fns[i],
            &func_names, &llvm_fns, trace,
        )?;
    }

    // Verify the module
    module
        .verify()
        .map_err(|e| format!("LLVM verification failed: {}", e.to_string()))?;

    if trace {
        eprintln!("--- LLVM IR ---");
        eprintln!("{}", module.print_to_string().to_string());
    }

    // Create execution engine with aggressive codegen optimization.
    // LLVM's MCJIT applies its own optimization pipeline during compilation.
    let engine = module
        .create_jit_execution_engine(OptimizationLevel::Aggressive)
        .map_err(|e| format!("Failed to create LLVM JIT engine: {}", e.to_string()))?;

    // Get the entry function
    let entry_name = &ir_module.functions[ir_module.entry].name;
    let entry_fn: JitFunction<MainFunc> = unsafe {
        engine
            .get_function(entry_name)
            .map_err(|e| format!("Failed to get entry function '{}': {}", entry_name, e))?
    };

    if trace {
        eprintln!(
            "LLVM: compiled {} functions, entry='{}'",
            ir_module.functions.len(),
            entry_name
        );
    }

    Ok(LlvmProgram {
        _engine: engine,
        entry_fn,
        _context: context,
    })
}

/// Check if an expression contains a self-recursive tail call.
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

/// Per-function compilation context.
struct FnCtx<'a> {
    context: &'static Context,
    builder: &'a Builder<'static>,
    #[allow(dead_code)]
    module: &'a Module<'static>,
    i64_type: IntType<'static>,
    vars: HashMap<String, IntValue<'static>>,
    func_names: &'a HashMap<String, usize>,
    llvm_fns: &'a [FunctionValue<'static>],
    current_fn: FunctionValue<'static>,
    current_fn_name: String,
    /// For self-tail-call → loop: alloca'd parameter slots + loop header block
    param_allocas: Vec<PointerValue<'static>>,
    loop_header: Option<BasicBlock<'static>>,
}

/// Compile a single function, with self-tail-call → loop optimization.
#[allow(clippy::too_many_arguments)]
fn compile_function(
    context: &'static Context,
    builder: &Builder<'static>,
    module: &Module<'static>,
    i64_type: IntType<'static>,
    func: &IrFunction,
    llvm_fn: FunctionValue<'static>,
    func_names: &HashMap<String, usize>,
    llvm_fns: &[FunctionValue<'static>],
    trace: bool,
) -> Result<(), String> {
    let entry_block = context.append_basic_block(llvm_fn, "entry");
    builder.position_at_end(entry_block);

    let is_tail_recursive = has_self_tail_call(&func.body, &func.name);
    let mut param_allocas = Vec::new();
    let mut vars: HashMap<String, IntValue<'static>> = HashMap::new();

    if is_tail_recursive {
        // Allocate mutable slots for parameters (alloca in entry block)
        for (p, param_name) in func.params.iter().enumerate() {
            let alloca = builder
                .build_alloca(i64_type, param_name)
                .map_err(|e| e.to_string())?;
            let param_val = llvm_fn.get_nth_param(p as u32).unwrap().into_int_value();
            builder
                .build_store(alloca, param_val)
                .map_err(|e| e.to_string())?;
            param_allocas.push(alloca);
        }

        // Create loop header — reload params at top of each iteration
        let loop_block = context.append_basic_block(llvm_fn, "loop");
        builder
            .build_unconditional_branch(loop_block)
            .map_err(|e| e.to_string())?;
        builder.position_at_end(loop_block);

        for (p, param_name) in func.params.iter().enumerate() {
            let val = builder
                .build_load(i64_type, param_allocas[p], param_name)
                .map_err(|e| e.to_string())?
                .into_int_value();
            vars.insert(param_name.clone(), val);
        }

        let mut ctx = FnCtx {
            context,
            builder,
            module,
            i64_type,
            vars,
            func_names,
            llvm_fns,
            current_fn: llvm_fn,
            current_fn_name: func.name.clone(),
            param_allocas,
            loop_header: Some(loop_block),
        };

        let result = ctx.compile_expr(&func.body)?;
        builder
            .build_return(Some(&result))
            .map_err(|e| format!("LLVM return error: {}", e))?;
    } else {
        // Non-tail-recursive: simple parameter binding
        for (p, param_name) in func.params.iter().enumerate() {
            let param_val = llvm_fn.get_nth_param(p as u32).unwrap().into_int_value();
            vars.insert(param_name.clone(), param_val);
        }

        let mut ctx = FnCtx {
            context,
            builder,
            module,
            i64_type,
            vars,
            func_names,
            llvm_fns,
            current_fn: llvm_fn,
            current_fn_name: func.name.clone(),
            param_allocas: vec![],
            loop_header: None,
        };

        let result = ctx.compile_expr(&func.body)?;
        builder
            .build_return(Some(&result))
            .map_err(|e| format!("LLVM return error: {}", e))?;
    }

    if trace {
        let mode = if is_tail_recursive { " (tail→loop)" } else { "" };
        eprintln!(
            "LLVM: compiled '{}' ({} params){}",
            func.name,
            func.params.len(),
            mode,
        );
    }
    Ok(())
}

impl<'a> FnCtx<'a> {
    fn compile_expr(&mut self, expr: &Expr) -> Result<IntValue<'static>, String> {
        match expr {
            Expr::Int(n) => Ok(self.i64_type.const_int(*n as u64, true)),

            Expr::Bool(b) => Ok(self.i64_type.const_int(*b as u64, false)),

            Expr::Unit => Ok(self.i64_type.const_int(0, false)),

            Expr::Float(f) => {
                // Store float as raw bits in i64
                Ok(self.i64_type.const_int(f.to_bits(), false))
            }

            Expr::Var(name, _) => self
                .vars
                .get(name)
                .copied()
                .ok_or_else(|| format!("Undefined variable: {}", name)),

            Expr::BinOp { op, lhs, rhs, .. } => {
                let l = self.compile_expr(lhs)?;
                let r = self.compile_expr(rhs)?;
                match op {
                    BinOp::Add => self
                        .builder
                        .build_int_add(l, r, "add")
                        .map_err(|e| e.to_string()),
                    BinOp::Sub => self
                        .builder
                        .build_int_sub(l, r, "sub")
                        .map_err(|e| e.to_string()),
                    BinOp::Mul => self
                        .builder
                        .build_int_mul(l, r, "mul")
                        .map_err(|e| e.to_string()),
                    BinOp::Div => self
                        .builder
                        .build_int_signed_div(l, r, "div")
                        .map_err(|e| e.to_string()),
                    BinOp::Mod => self
                        .builder
                        .build_int_signed_rem(l, r, "mod")
                        .map_err(|e| e.to_string()),
                    BinOp::Eq => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, l, r, "eq")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "eq_ext")
                            .map_err(|e| e.to_string())
                    }
                    BinOp::Ne => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::NE, l, r, "ne")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "ne_ext")
                            .map_err(|e| e.to_string())
                    }
                    BinOp::Lt => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLT, l, r, "lt")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "lt_ext")
                            .map_err(|e| e.to_string())
                    }
                    BinOp::Gt => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGT, l, r, "gt")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "gt_ext")
                            .map_err(|e| e.to_string())
                    }
                    BinOp::Le => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLE, l, r, "le")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "le_ext")
                            .map_err(|e| e.to_string())
                    }
                    BinOp::Ge => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGE, l, r, "ge")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "ge_ext")
                            .map_err(|e| e.to_string())
                    }
                }
            }

            Expr::UnaryOp { op, operand, .. } => {
                let val = self.compile_expr(operand)?;
                match op {
                    UnaryOp::Neg => self
                        .builder
                        .build_int_neg(val, "neg")
                        .map_err(|e| e.to_string()),
                    UnaryOp::Not => {
                        let zero = self.i64_type.const_int(0, false);
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, val, zero, "not")
                            .map_err(|e| e.to_string())?;
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "not_ext")
                            .map_err(|e| e.to_string())
                    }
                }
            }

            Expr::And(lhs, rhs) => {
                // Short-circuit: if lhs is 0, return 0; else evaluate rhs
                let l = self.compile_expr(lhs)?;
                let zero = self.i64_type.const_int(0, false);
                let cond = self
                    .builder
                    .build_int_compare(IntPredicate::NE, l, zero, "and_cond")
                    .map_err(|e| e.to_string())?;

                let rhs_block = self.context.append_basic_block(self.current_fn, "and_rhs");
                let merge_block = self.context.append_basic_block(self.current_fn, "and_merge");

                self.builder
                    .build_conditional_branch(cond, rhs_block, merge_block)
                    .map_err(|e| e.to_string())?;

                let lhs_block = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block")?;

                self.builder.position_at_end(rhs_block);
                let r = self.compile_expr(rhs)?;
                let rhs_end = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block")?;
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(merge_block);
                let phi = self
                    .builder
                    .build_phi(self.i64_type, "and_result")
                    .map_err(|e| e.to_string())?;
                phi.add_incoming(&[(&zero, lhs_block), (&r, rhs_end)]);
                Ok(phi.as_basic_value().into_int_value())
            }

            Expr::Or(lhs, rhs) => {
                let l = self.compile_expr(lhs)?;
                let zero = self.i64_type.const_int(0, false);
                let cond = self
                    .builder
                    .build_int_compare(IntPredicate::NE, l, zero, "or_cond")
                    .map_err(|e| e.to_string())?;

                let rhs_block = self.context.append_basic_block(self.current_fn, "or_rhs");
                let merge_block = self.context.append_basic_block(self.current_fn, "or_merge");

                let lhs_block = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block")?;
                self.builder
                    .build_conditional_branch(cond, merge_block, rhs_block)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(rhs_block);
                let r = self.compile_expr(rhs)?;
                let rhs_end = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block")?;
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(|e| e.to_string())?;

                self.builder.position_at_end(merge_block);
                let phi = self
                    .builder
                    .build_phi(self.i64_type, "or_result")
                    .map_err(|e| e.to_string())?;
                phi.add_incoming(&[(&l, lhs_block), (&r, rhs_end)]);
                Ok(phi.as_basic_value().into_int_value())
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_val = self.compile_expr(condition)?;
                let zero = self.i64_type.const_int(0, false);
                let cond = self
                    .builder
                    .build_int_compare(IntPredicate::NE, cond_val, zero, "if_cond")
                    .map_err(|e| e.to_string())?;

                let then_block = self.context.append_basic_block(self.current_fn, "then");
                let else_block = self.context.append_basic_block(self.current_fn, "else");
                let merge_block = self.context.append_basic_block(self.current_fn, "merge");

                self.builder
                    .build_conditional_branch(cond, then_block, else_block)
                    .map_err(|e| e.to_string())?;

                // Then branch
                self.builder.position_at_end(then_block);
                let then_val = self.compile_expr(then_branch)?;
                let then_end = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block")?;
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(|e| e.to_string())?;

                // Else branch
                self.builder.position_at_end(else_block);
                let else_val = if let Some(eb) = else_branch {
                    self.compile_expr(eb)?
                } else {
                    self.i64_type.const_int(0, false)
                };
                let else_end = self
                    .builder
                    .get_insert_block()
                    .ok_or("No insert block")?;
                self.builder
                    .build_unconditional_branch(merge_block)
                    .map_err(|e| e.to_string())?;

                // Merge
                self.builder.position_at_end(merge_block);
                let phi = self
                    .builder
                    .build_phi(self.i64_type, "if_result")
                    .map_err(|e| e.to_string())?;
                phi.add_incoming(&[(&then_val, then_end), (&else_val, else_end)]);
                Ok(phi.as_basic_value().into_int_value())
            }

            Expr::Let { pattern, value, .. } => {
                let val = self.compile_expr(value)?;
                if let Pattern::Var(name) = pattern.as_ref() {
                    self.vars.insert(name.clone(), val);
                }
                // Let evaluates to the value
                Ok(val)
            }

            Expr::Block(exprs, _) => {
                let mut result = self.i64_type.const_int(0, false);
                for expr in exprs {
                    result = self.compile_expr(expr)?;
                }
                Ok(result)
            }

            Expr::CallDirect { name, args, .. } => {
                let func_idx = self
                    .func_names
                    .get(name)
                    .ok_or_else(|| format!("Unknown function: {}", name))?;
                let callee = self.llvm_fns[*func_idx];

                let arg_vals: Vec<BasicMetadataValueEnum<'static>> = args
                    .iter()
                    .map(|a| self.compile_expr(a).map(|v| v.into()))
                    .collect::<Result<_, _>>()?;

                let call = self
                    .builder
                    .build_call(callee, &arg_vals, "call")
                    .map_err(|e| e.to_string())?;

                match call.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(v) => Ok(v.into_int_value()),
                    inkwell::values::ValueKind::Instruction(_) => {
                        Err("Call returned void".to_string())
                    }
                }
            }

            Expr::TailCall { name, args, .. } => {
                if name == &self.current_fn_name
                    && let Some(loop_header) = self.loop_header
                {
                    // Self-recursive tail call → loop: store new args, branch to header
                    let arg_vals: Vec<IntValue<'static>> = args
                        .iter()
                        .map(|a| self.compile_expr(a))
                        .collect::<Result<_, _>>()?;

                    // Store all args first (before overwriting any, for correct semantics)
                    for (alloca, val) in self.param_allocas.iter().zip(arg_vals.iter()) {
                        self.builder
                            .build_store(*alloca, *val)
                            .map_err(|e| e.to_string())?;
                    }

                    self.builder
                        .build_unconditional_branch(loop_header)
                        .map_err(|e| e.to_string())?;

                    // Create unreachable block for LLVM's IR validity
                    let dead_block = self
                        .context
                        .append_basic_block(self.current_fn, "after_tailcall");
                    self.builder.position_at_end(dead_block);

                    Ok(self.i64_type.const_int(0, false))
                } else {
                    // Non-self tail call → regular call with tail hint
                    let func_idx = self
                        .func_names
                        .get(name)
                        .ok_or_else(|| format!("Unknown function: {}", name))?;
                    let callee = self.llvm_fns[*func_idx];

                    let arg_vals: Vec<BasicMetadataValueEnum<'static>> = args
                        .iter()
                        .map(|a| self.compile_expr(a).map(|v| v.into()))
                        .collect::<Result<_, _>>()?;

                    let call = self
                        .builder
                        .build_call(callee, &arg_vals, "tailcall")
                        .map_err(|e| e.to_string())?;

                    call.set_tail_call(true);

                    match call.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(v) => Ok(v.into_int_value()),
                        inkwell::values::ValueKind::Instruction(_) => {
                            Err("Tail call returned void".to_string())
                        }
                    }
                }
            }

            // Unsupported expressions — fall back to error
            Expr::String(_) => Err("LLVM backend: String not supported".into()),
            Expr::CallNative { module, method, .. } => Err(format!(
                "LLVM backend: native call {}.{} not supported",
                module, method
            )),
            Expr::CallIndirect { .. } => Err("LLVM backend: indirect calls not supported".into()),
            Expr::Lambda { .. } => Err("LLVM backend: lambdas not supported".into()),
            Expr::Match { .. } => Err("LLVM backend: match not supported".into()),
            Expr::For { .. } => Err("LLVM backend: for loops not supported".into()),
            Expr::Hole => Err("LLVM backend: typed holes not supported".into()),
            Expr::MakeEnum { .. } => Err("LLVM backend: enums not supported".into()),
            Expr::MakeStruct { .. } => Err("LLVM backend: structs not supported".into()),
            Expr::MakeList(_, _) => Err("LLVM backend: lists not supported".into()),
            Expr::MakeRecord(_, _) => Err("LLVM backend: records not supported".into()),
            Expr::MakeTuple(_, _) => Err("LLVM backend: tuples not supported".into()),
            Expr::MakeRange(_, _) => Err("LLVM backend: ranges not supported".into()),
            Expr::UpdateRecord { .. } => Err("LLVM backend: record update not supported".into()),
            Expr::GetField { .. } => Err("LLVM backend: field access not supported".into()),
            Expr::Try { .. } => Err("LLVM backend: try expression not supported".into()),
            Expr::Concat(_) => Err("LLVM backend: string concat not supported".into()),
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
            line: 1,
            col: 0,
            start_byte: 0,
            end_byte: 0,
        }
    }

    fn run_module(module: &IrModule) -> i64 {
        let program = compile(module, false).expect("LLVM compilation failed");
        program.run_entry()
    }

    fn make_module(body: Expr) -> IrModule {
        IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body,
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
        }
    }

    #[test]
    fn llvm_integer_constant() {
        let module = make_module(Expr::Int(42));
        assert_eq!(run_module(&module), 42);
    }

    #[test]
    fn llvm_negative_int() {
        let module = make_module(Expr::Int(-7));
        assert_eq!(run_module(&module), -7);
    }

    #[test]
    fn llvm_add() {
        let module = make_module(Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(Expr::Int(3)),
            rhs: Box::new(Expr::Int(4)),
            ty: None,
        });
        assert_eq!(run_module(&module), 7);
    }

    #[test]
    fn llvm_sub() {
        let module = make_module(Expr::BinOp {
            op: BinOp::Sub,
            lhs: Box::new(Expr::Int(10)),
            rhs: Box::new(Expr::Int(3)),
            ty: None,
        });
        assert_eq!(run_module(&module), 7);
    }

    #[test]
    fn llvm_mul() {
        let module = make_module(Expr::BinOp {
            op: BinOp::Mul,
            lhs: Box::new(Expr::Int(6)),
            rhs: Box::new(Expr::Int(7)),
            ty: None,
        });
        assert_eq!(run_module(&module), 42);
    }

    #[test]
    fn llvm_comparison_le() {
        let module = make_module(Expr::BinOp {
            op: BinOp::Le,
            lhs: Box::new(Expr::Int(3)),
            rhs: Box::new(Expr::Int(5)),
            ty: None,
        });
        assert_eq!(run_module(&module), 1);
    }

    #[test]
    fn llvm_if_else() {
        let module = make_module(Expr::If {
            condition: Box::new(Expr::Bool(true)),
            then_branch: Box::new(Expr::Int(42)),
            else_branch: Some(Box::new(Expr::Int(0))),
            ty: None,
        });
        assert_eq!(run_module(&module), 42);
    }

    #[test]
    fn llvm_if_else_false() {
        let module = make_module(Expr::If {
            condition: Box::new(Expr::Bool(false)),
            then_branch: Box::new(Expr::Int(42)),
            else_branch: Some(Box::new(Expr::Int(99))),
            ty: None,
        });
        assert_eq!(run_module(&module), 99);
    }

    #[test]
    fn llvm_let_binding() {
        let module = make_module(Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("x".into())),
                    value: Box::new(Expr::Int(10)),
                    ty: None,
                },
                Expr::BinOp {
                    op: BinOp::Add,
                    lhs: Box::new(Expr::Var("x".into(), None)),
                    rhs: Box::new(Expr::Int(5)),
                    ty: None,
                },
            ],
            None,
        ));
        assert_eq!(run_module(&module), 15);
    }

    #[test]
    fn llvm_function_call() {
        // fn double(x) = x + x; fn main() = double(21)
        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "double".into(),
                        args: vec![Expr::Int(21)],
                        ty: None,
                    },
                    ty: None,
                    span: dummy_span(),
                },
                IrFunction {
                    name: "double".into(),
                    params: vec!["x".into()],
                    body: Expr::BinOp {
                        op: BinOp::Add,
                        lhs: Box::new(Expr::Var("x".into(), None)),
                        rhs: Box::new(Expr::Var("x".into(), None)),
                        ty: None,
                    },
                    ty: None,
                    span: dummy_span(),
                },
            ],
            entry: 0,
        };
        assert_eq!(run_module(&module), 42);
    }

    #[test]
    fn llvm_recursive_fib10() {
        // fn fib(n) = if n <= 1 then n else fib(n-1) + fib(n-2)
        // fn main() = fib(10)
        let fib_body = Expr::If {
            condition: Box::new(Expr::BinOp {
                op: BinOp::Le,
                lhs: Box::new(Expr::Var("n".into(), None)),
                rhs: Box::new(Expr::Int(1)),
                ty: None,
            }),
            then_branch: Box::new(Expr::Var("n".into(), None)),
            else_branch: Some(Box::new(Expr::BinOp {
                op: BinOp::Add,
                lhs: Box::new(Expr::CallDirect {
                    name: "fib".into(),
                    args: vec![Expr::BinOp {
                        op: BinOp::Sub,
                        lhs: Box::new(Expr::Var("n".into(), None)),
                        rhs: Box::new(Expr::Int(1)),
                        ty: None,
                    }],
                    ty: None,
                }),
                rhs: Box::new(Expr::CallDirect {
                    name: "fib".into(),
                    args: vec![Expr::BinOp {
                        op: BinOp::Sub,
                        lhs: Box::new(Expr::Var("n".into(), None)),
                        rhs: Box::new(Expr::Int(2)),
                        ty: None,
                    }],
                    ty: None,
                }),
                ty: None,
            })),
            ty: None,
        };

        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "fib".into(),
                        args: vec![Expr::Int(10)],
                        ty: None,
                    },
                    ty: None,
                    span: dummy_span(),
                },
                IrFunction {
                    name: "fib".into(),
                    params: vec!["n".into()],
                    body: fib_body,
                    ty: None,
                    span: dummy_span(),
                },
            ],
            entry: 0,
        };
        assert_eq!(run_module(&module), 55);
    }

    #[test]
    fn llvm_tail_call() {
        // fn sum(n, acc) = if n <= 0 then acc else TailCall sum(n-1, acc+n)
        // fn main() = sum(100, 0)
        let sum_body = Expr::If {
            condition: Box::new(Expr::BinOp {
                op: BinOp::Le,
                lhs: Box::new(Expr::Var("n".into(), None)),
                rhs: Box::new(Expr::Int(0)),
                ty: None,
            }),
            then_branch: Box::new(Expr::Var("acc".into(), None)),
            else_branch: Some(Box::new(Expr::TailCall {
                name: "sum".into(),
                args: vec![
                    Expr::BinOp {
                        op: BinOp::Sub,
                        lhs: Box::new(Expr::Var("n".into(), None)),
                        rhs: Box::new(Expr::Int(1)),
                        ty: None,
                    },
                    Expr::BinOp {
                        op: BinOp::Add,
                        lhs: Box::new(Expr::Var("acc".into(), None)),
                        rhs: Box::new(Expr::Var("n".into(), None)),
                        ty: None,
                    },
                ],
                ty: None,
            })),
            ty: None,
        };

        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "sum".into(),
                        args: vec![Expr::Int(100), Expr::Int(0)],
                        ty: None,
                    },
                    ty: None,
                    span: dummy_span(),
                },
                IrFunction {
                    name: "sum".into(),
                    params: vec!["n".into(), "acc".into()],
                    body: sum_body,
                    ty: None,
                    span: dummy_span(),
                },
            ],
            entry: 0,
        };
        assert_eq!(run_module(&module), 5050);
    }

    #[test]
    fn llvm_tail_call_deep_recursion() {
        // Verify self-tail-call → loop: sum 1..1_000_000 without stack overflow
        // fn sum(n, acc) = if n <= 0 then acc else TailCall sum(n-1, acc+n)
        let sum_body = Expr::If {
            condition: Box::new(Expr::BinOp {
                op: BinOp::Le,
                lhs: Box::new(Expr::Var("n".into(), None)),
                rhs: Box::new(Expr::Int(0)),
                ty: None,
            }),
            then_branch: Box::new(Expr::Var("acc".into(), None)),
            else_branch: Some(Box::new(Expr::TailCall {
                name: "sum".into(),
                args: vec![
                    Expr::BinOp {
                        op: BinOp::Sub,
                        lhs: Box::new(Expr::Var("n".into(), None)),
                        rhs: Box::new(Expr::Int(1)),
                        ty: None,
                    },
                    Expr::BinOp {
                        op: BinOp::Add,
                        lhs: Box::new(Expr::Var("acc".into(), None)),
                        rhs: Box::new(Expr::Var("n".into(), None)),
                        ty: None,
                    },
                ],
                ty: None,
            })),
            ty: None,
        };

        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "sum".into(),
                        args: vec![Expr::Int(1_000_000), Expr::Int(0)],
                        ty: None,
                    },
                    ty: None,
                    span: dummy_span(),
                },
                IrFunction {
                    name: "sum".into(),
                    params: vec!["n".into(), "acc".into()],
                    body: sum_body,
                    ty: None,
                    span: dummy_span(),
                },
            ],
            entry: 0,
        };
        // 1 + 2 + ... + 1_000_000 = 500_000_500_000
        assert_eq!(run_module(&module), 500_000_500_000);
    }

    #[test]
    fn llvm_tak() {
        // Takeuchi function — heavily recursive, benefits from tail call optimization
        // tak(x, y, z) = if y < x then tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y)) else z
        let tak_body = Expr::If {
            condition: Box::new(Expr::BinOp {
                op: BinOp::Lt,
                lhs: Box::new(Expr::Var("y".into(), None)),
                rhs: Box::new(Expr::Var("x".into(), None)),
                ty: None,
            }),
            then_branch: Box::new(Expr::CallDirect {
                name: "tak".into(),
                args: vec![
                    Expr::CallDirect {
                        name: "tak".into(),
                        args: vec![
                            Expr::BinOp { op: BinOp::Sub, lhs: Box::new(Expr::Var("x".into(), None)), rhs: Box::new(Expr::Int(1)), ty: None },
                            Expr::Var("y".into(), None),
                            Expr::Var("z".into(), None),
                        ],
                        ty: None,
                    },
                    Expr::CallDirect {
                        name: "tak".into(),
                        args: vec![
                            Expr::BinOp { op: BinOp::Sub, lhs: Box::new(Expr::Var("y".into(), None)), rhs: Box::new(Expr::Int(1)), ty: None },
                            Expr::Var("z".into(), None),
                            Expr::Var("x".into(), None),
                        ],
                        ty: None,
                    },
                    Expr::CallDirect {
                        name: "tak".into(),
                        args: vec![
                            Expr::BinOp { op: BinOp::Sub, lhs: Box::new(Expr::Var("z".into(), None)), rhs: Box::new(Expr::Int(1)), ty: None },
                            Expr::Var("x".into(), None),
                            Expr::Var("y".into(), None),
                        ],
                        ty: None,
                    },
                ],
                ty: None,
            }),
            else_branch: Some(Box::new(Expr::Var("z".into(), None))),
            ty: None,
        };

        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "tak".into(),
                        args: vec![Expr::Int(18), Expr::Int(12), Expr::Int(6)],
                        ty: None,
                    },
                    ty: None,
                    span: dummy_span(),
                },
                IrFunction {
                    name: "tak".into(),
                    params: vec!["x".into(), "y".into(), "z".into()],
                    body: tak_body,
                    ty: None,
                    span: dummy_span(),
                },
            ],
            entry: 0,
        };
        assert_eq!(run_module(&module), 7);
    }

    #[test]
    fn llvm_negation() {
        let module = make_module(Expr::UnaryOp {
            op: UnaryOp::Neg,
            operand: Box::new(Expr::Int(42)),
            ty: None,
        });
        assert_eq!(run_module(&module), -42);
    }

    #[test]
    fn llvm_not() {
        let module = make_module(Expr::UnaryOp {
            op: UnaryOp::Not,
            operand: Box::new(Expr::Bool(true)),
            ty: None,
        });
        assert_eq!(run_module(&module), 0);
    }
}
