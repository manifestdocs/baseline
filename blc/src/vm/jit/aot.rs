//! Ahead-of-time compiler: emits native object files from Baseline IR.
//!
//! Supports all types compilable by the JIT (Int, Bool, Float, String, List,
//! Record, Enum, Tuple, Closure). Links against libbaseline_rt for runtime
//! helpers (heap allocation, string operations, etc.).

use std::collections::{HashMap, HashSet};
use std::path::Path;

use cranelift_codegen::ir::{AbiParam, InstBuilder, types};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use super::super::ir::{Expr, IrModule};
use super::super::nvalue::{PAYLOAD_MASK, TAG_INT};
use super::analysis::{can_jit, compute_unboxed_flags, has_self_tail_call};
use super::compile::FnCompileCtx;
use super::{HELPER_NAMES, make_helper_sig};

/// Compile an IrModule to a native object file (bytes).
pub fn compile_to_object(module: &IrModule, trace: bool) -> Result<Vec<u8>, String> {
    // Validate: all functions must be JIT-compilable
    for func in &module.functions {
        if !can_jit(func, None) {
            return Err(format!(
                "AOT: function '{}' uses unsupported constructs (not JIT-compilable)",
                func.name
            ));
        }
    }

    // Create ISA (same config as JIT)
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
    flag_builder
        .set("is_pic", "true")
        .map_err(|e| e.to_string())?;

    let isa_builder =
        cranelift_native::builder().map_err(|e| format!("Native ISA not available: {}", e))?;
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .map_err(|e| e.to_string())?;

    let obj_builder =
        ObjectBuilder::new(isa, "baseline_aot", cranelift_module::default_libcall_names())
            .map_err(|e| e.to_string())?;
    let mut obj_module = ObjectModule::new(obj_builder);
    let ptr_type = obj_module.target_config().pointer_type();

    let unboxed_flags = compute_unboxed_flags(module);

    // --- Declare runtime helpers as imports (linked from libbaseline_rt) ---
    let mut helper_ids: HashMap<&str, FuncId> = HashMap::new();
    for &name in HELPER_NAMES {
        // Skip jit_call_native — it depends on NativeRegistry (blc-only, not in rt lib)
        if name == "jit_call_native" {
            continue;
        }
        let sig = make_helper_sig(&mut obj_module, name, ptr_type);
        let id = obj_module
            .declare_function(name, Linkage::Import, &sig)
            .map_err(|e| e.to_string())?;
        helper_ids.insert(name, id);
    }

    // --- Collect string constants and create global data entries ---
    let mut all_strings = HashSet::new();
    for func in &module.functions {
        collect_expr_strings(&func.body, &mut all_strings);
    }

    let mut aot_strings: HashMap<String, DataId> = HashMap::new();
    for (i, s) in all_strings.iter().enumerate() {
        let data_name = format!("__bl_str_{}", i);
        let data_id = obj_module
            .declare_data(&data_name, Linkage::Local, false, false)
            .map_err(|e| e.to_string())?;
        let mut desc = DataDescription::new();
        desc.define(s.as_bytes().to_vec().into_boxed_slice());
        obj_module
            .define_data(data_id, &desc)
            .map_err(|e| e.to_string())?;
        aot_strings.insert(s.clone(), data_id);
    }

    if trace {
        eprintln!("AOT: {} unique string constants", aot_strings.len());
    }

    // --- Declare all functions (Tail CC, Local linkage) ---
    let mut func_ids: Vec<Option<FuncId>> = Vec::with_capacity(module.functions.len());

    for func in &module.functions {
        let mangled = format!("__bl_{}", func.name);
        let sig = build_aot_signature(&mut obj_module, func.params.len());
        let id = obj_module
            .declare_function(&mangled, Linkage::Local, &sig)
            .map_err(|e| e.to_string())?;
        func_ids.push(Some(id));
    }

    // --- Compile each function ---
    let mut fb_ctx = FunctionBuilderContext::new();
    let mut codegen_ctx = cranelift_codegen::Context::new();

    let func_names: HashMap<String, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, f)| (f.name.clone(), idx))
        .collect();

    for (i, func) in module.functions.iter().enumerate() {
        let func_id = func_ids[i].unwrap();
        let start = std::time::Instant::now();
        let is_unboxed = unboxed_flags[i];

        let mut cl_func = cranelift_codegen::ir::Function::new();
        cl_func.signature = build_aot_signature(&mut obj_module, func.params.len());

        let compile_result = {
            let mut fn_builder = FunctionBuilder::new(&mut cl_func, &mut fb_ctx);
            let entry_block = fn_builder.create_block();
            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let mut param_vars = Vec::new();
            let mut vars_map = HashMap::new();
            for (p, param_name) in func.params.iter().enumerate() {
                let var = fn_builder.declare_var(types::I64);
                let param_val = fn_builder.block_params(entry_block)[p];
                fn_builder.def_var(var, param_val);
                vars_map.insert(param_name.clone(), var);
                param_vars.push(var);
            }

            let is_tail_recursive = has_self_tail_call(&func.body, &func.name);
            let loop_header = if is_tail_recursive {
                let lh = fn_builder.create_block();
                fn_builder.ins().jump(lh, &[]);
                fn_builder.switch_to_block(lh);
                Some(lh)
            } else {
                None
            };

            let mut heap_roots = Vec::new();
            let mut ctx = FnCompileCtx {
                builder: &mut fn_builder,
                func_ids: &func_ids,
                module: &mut obj_module,
                vars: vars_map,
                next_var: param_vars.len() as u32,
                func_names: &func_names,
                ir_functions: &module.functions,
                current_func_name: func.name.clone(),
                param_vars,
                loop_header,
                heap_roots: &mut heap_roots,
                helper_ids: &helper_ids,
                natives: None,
                ptr_type,
                unboxed_flags: &unboxed_flags,
                tags: &module.tags,
                sra_records: HashMap::new(),
                aot_strings: Some(&aot_strings),
            };

            let result = if is_unboxed {
                ctx.compile_expr_unboxed(&func.body)
            } else {
                ctx.compile_expr(&func.body)
            };
            let saved_loop_header = ctx.loop_header;
            drop(ctx);

            match result {
                Ok(val) => {
                    fn_builder.ins().return_(&[val]);
                    if let Some(lh) = saved_loop_header {
                        fn_builder.seal_block(lh);
                    }
                    fn_builder.finalize();
                    Ok(())
                }
                Err(e) => Err(e),
            }
        };

        compile_result.map_err(|e| format!("AOT: compile error in '{}': {}", func.name, e))?;

        codegen_ctx.clear();
        codegen_ctx.func = cl_func;
        obj_module
            .define_function(func_id, &mut codegen_ctx)
            .map_err(|e| format!("AOT: codegen error in '{}': {}", func.name, e))?;

        if trace {
            let elapsed = start.elapsed();
            let mode = if is_unboxed { "unboxed" } else { "boxed" };
            eprintln!(
                "AOT: compiled '{}' [{}] ({:.1}ms)",
                func.name,
                mode,
                elapsed.as_secs_f64() * 1000.0
            );
        }
    }

    // --- Create main wrapper ---
    create_main_wrapper(
        &mut obj_module,
        &mut fb_ctx,
        &func_ids,
        &helper_ids,
        module.entry,
        unboxed_flags[module.entry],
        ptr_type,
        trace,
    )?;

    // Emit object bytes
    let obj_product = obj_module.finish();
    let bytes = obj_product.emit().map_err(|e| e.to_string())?;

    if trace {
        eprintln!("AOT: emitted {} byte object file", bytes.len());
    }

    Ok(bytes)
}

/// Link an object file into a standalone executable.
///
/// If `rt_lib_dir` is provided, links against `-lbaseline_rt` from that directory.
/// Otherwise, links as a standalone object (Phase 1 numeric-only programs).
pub fn link_executable(
    obj_bytes: &[u8],
    output: &Path,
    rt_lib_dir: Option<&Path>,
    trace: bool,
) -> Result<(), String> {
    let tmp_dir = std::env::temp_dir();
    let pid = std::process::id();
    let obj_path = tmp_dir.join(format!("baseline_aot_{}.o", pid));

    // Write object file (cleanup on drop)
    struct TempFile(std::path::PathBuf);
    impl Drop for TempFile {
        fn drop(&mut self) {
            let _ = std::fs::remove_file(&self.0);
        }
    }
    std::fs::write(&obj_path, obj_bytes)
        .map_err(|e| format!("Failed to write object file: {}", e))?;
    let _guard = TempFile(obj_path.clone());

    if trace {
        eprintln!(
            "AOT: linking {} -> {}",
            obj_path.display(),
            output.display()
        );
    }

    let mut cmd = std::process::Command::new("cc");
    cmd.arg("-o").arg(output).arg(&obj_path);

    if let Some(dir) = rt_lib_dir {
        cmd.arg("-L").arg(dir).arg("-lbaseline_rt");

        // Platform-specific system libraries
        if cfg!(target_os = "macos") {
            cmd.arg("-lSystem");
        } else {
            cmd.args(["-lc", "-ldl", "-lpthread", "-lm"]);
        }

        if trace {
            eprintln!("AOT: linking with libbaseline_rt from {}", dir.display());
        }
    }

    let status = cmd
        .status()
        .map_err(|e| format!("Failed to run linker (cc): {}", e))?;

    if !status.success() {
        return Err(format!("Linker failed with exit code: {}", status));
    }

    if trace {
        eprintln!("AOT: linked successfully: {}", output.display());
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Build a Tail-CC signature for AOT functions.
fn build_aot_signature(
    module: &mut ObjectModule,
    param_count: usize,
) -> cranelift_codegen::ir::Signature {
    let mut sig = module.make_signature();
    sig.call_conv = CallConv::Tail;
    for _ in 0..param_count {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I64));
    sig
}

/// Create the `main` function wrapper.
///
/// 1. Build function table on stack, initialize via jit_init_fn_table
/// 2. Call entry function → u64 result
/// 3. If entry is unboxed, NaN-box the raw i64 as Int
/// 4. Call jit_print_result(bits) to display the result
/// 5. Call jit_drain_arena() for cleanup
/// 6. Return 0
fn create_main_wrapper(
    obj_module: &mut ObjectModule,
    fb_ctx: &mut FunctionBuilderContext,
    func_ids: &[Option<FuncId>],
    helper_ids: &HashMap<&str, FuncId>,
    entry_idx: usize,
    entry_unboxed: bool,
    ptr_type: cranelift_codegen::ir::Type,
    trace: bool,
) -> Result<(), String> {
    let entry_func_id = func_ids[entry_idx]
        .ok_or_else(|| "AOT: entry function was not compiled".to_string())?;

    // Signature: (argc: i32, argv: ptr) -> i32
    let mut main_sig = obj_module.make_signature();
    main_sig.params.push(AbiParam::new(types::I32));
    main_sig.params.push(AbiParam::new(ptr_type));
    main_sig.returns.push(AbiParam::new(types::I32));

    let main_id = obj_module
        .declare_function("main", Linkage::Export, &main_sig)
        .map_err(|e| e.to_string())?;

    let mut cl_func = cranelift_codegen::ir::Function::new();
    cl_func.signature = main_sig;

    {
        let mut builder = FunctionBuilder::new(&mut cl_func, fb_ctx);
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        builder.seal_block(block);

        // 1. Build function table on the stack and initialize it
        let table_size = (func_ids.len() * 8) as u32;
        let fn_table_slot = builder.create_sized_stack_slot(
            cranelift_codegen::ir::StackSlotData::new(
                cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                table_size,
                3, // 8-byte aligned
            ),
        );
        let table_addr = builder.ins().stack_addr(ptr_type, fn_table_slot, 0);

        // Store each function's address into the table
        for (i, func_id_opt) in func_ids.iter().enumerate() {
            if let Some(fid) = func_id_opt {
                let func_ref = obj_module.declare_func_in_func(*fid, builder.func);
                let addr = builder.ins().func_addr(ptr_type, func_ref);
                builder
                    .ins()
                    .stack_store(addr, fn_table_slot, (i * 8) as i32);
            } else {
                let zero = builder.ins().iconst(ptr_type, 0);
                builder
                    .ins()
                    .stack_store(zero, fn_table_slot, (i * 8) as i32);
            }
        }

        let func_count = builder
            .ins()
            .iconst(types::I64, func_ids.len() as i64);

        let init_fn_id = helper_ids["jit_init_fn_table"];
        let init_fn_ref = obj_module.declare_func_in_func(init_fn_id, builder.func);
        builder.ins().call(init_fn_ref, &[table_addr, func_count]);

        // 2. Call entry function
        let entry_ref = obj_module.declare_func_in_func(entry_func_id, builder.func);
        let call = builder.ins().call(entry_ref, &[]);
        let raw_result = builder.inst_results(call)[0];

        // 3. If entry is unboxed, NaN-box the raw i64 as Int
        let result_bits = if entry_unboxed {
            let mask = builder.ins().iconst(types::I64, PAYLOAD_MASK as i64);
            let masked = builder.ins().band(raw_result, mask);
            let tag = builder.ins().iconst(types::I64, TAG_INT as i64);
            builder.ins().bor(masked, tag)
        } else {
            raw_result
        };

        // 4. Print the result
        let print_fn_id = helper_ids["jit_print_result"];
        let print_ref = obj_module.declare_func_in_func(print_fn_id, builder.func);
        builder.ins().call(print_ref, &[result_bits]);

        // 5. Write newline
        let mut write_sig = obj_module.make_signature();
        write_sig.params.push(AbiParam::new(types::I64));
        write_sig.params.push(AbiParam::new(ptr_type));
        write_sig.params.push(AbiParam::new(types::I64));
        write_sig.returns.push(AbiParam::new(types::I64));

        let write_id = obj_module
            .declare_function("write", Linkage::Import, &write_sig)
            .map_err(|e| e.to_string())?;
        let write_ref = obj_module.declare_func_in_func(write_id, builder.func);

        // Store newline in a 1-byte stack slot
        let nl_slot = builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            8,
            3,
        ));
        let nl_byte = builder.ins().iconst(types::I8, 10); // '\n'
        builder.ins().stack_store(nl_byte, nl_slot, 0);
        let nl_addr = builder.ins().stack_addr(ptr_type, nl_slot, 0);
        let fd_stdout = builder.ins().iconst(types::I64, 1);
        let one = builder.ins().iconst(types::I64, 1);
        builder.ins().call(write_ref, &[fd_stdout, nl_addr, one]);

        // 6. Drain arena
        let drain_fn_id = helper_ids["jit_drain_arena"];
        let drain_ref = obj_module.declare_func_in_func(drain_fn_id, builder.func);
        builder.ins().call(drain_ref, &[]);

        // 7. Return 0
        let ret_zero = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[ret_zero]);
        builder.finalize();
    }

    let mut ctx = cranelift_codegen::Context::for_function(cl_func);
    obj_module
        .define_function(main_id, &mut ctx)
        .map_err(|e| format!("AOT: main wrapper error: {}", e))?;

    if trace {
        eprintln!("AOT: created main wrapper (entry_unboxed={})", entry_unboxed);
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// String constant collection
// ---------------------------------------------------------------------------

/// Recursively collect all string constants from an expression tree.
/// This includes string literals, enum tag names, struct/record field names.
fn collect_expr_strings(expr: &Expr, strings: &mut HashSet<String>) {
    match expr {
        Expr::String(s) => {
            strings.insert(s.clone());
        }
        Expr::Concat(parts) => {
            for p in parts {
                collect_expr_strings(p, strings);
            }
        }
        Expr::MakeEnum { tag, payload, .. } => {
            strings.insert(tag.clone());
            collect_expr_strings(payload, strings);
        }
        Expr::MakeStruct { name, fields, .. } => {
            strings.insert(name.clone());
            for (fname, fexpr) in fields {
                strings.insert(fname.clone());
                collect_expr_strings(fexpr, strings);
            }
        }
        Expr::MakeRecord(fields, _) => {
            for (fname, fexpr) in fields {
                strings.insert(fname.clone());
                collect_expr_strings(fexpr, strings);
            }
        }
        Expr::UpdateRecord { base, updates, .. } => {
            collect_expr_strings(base, strings);
            for (fname, fexpr) in updates {
                strings.insert(fname.clone());
                collect_expr_strings(fexpr, strings);
            }
        }
        Expr::GetField { object, field, .. } => {
            strings.insert(field.clone());
            collect_expr_strings(object, strings);
        }
        // --- Recurse into children for all other variants ---
        Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole => {}
        Expr::Var(_, _) | Expr::GetClosureVar(_) => {}
        Expr::BinOp { lhs, rhs, .. } => {
            collect_expr_strings(lhs, strings);
            collect_expr_strings(rhs, strings);
        }
        Expr::UnaryOp { operand, .. } => {
            collect_expr_strings(operand, strings);
        }
        Expr::And(a, b) | Expr::Or(a, b) => {
            collect_expr_strings(a, strings);
            collect_expr_strings(b, strings);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_expr_strings(condition, strings);
            collect_expr_strings(then_branch, strings);
            if let Some(eb) = else_branch {
                collect_expr_strings(eb, strings);
            }
        }
        Expr::Match { subject, arms, .. } => {
            collect_expr_strings(subject, strings);
            for arm in arms {
                collect_expr_strings(&arm.body, strings);
            }
        }
        Expr::For { iterable, body, .. } => {
            collect_expr_strings(iterable, strings);
            collect_expr_strings(body, strings);
        }
        Expr::Let { value, .. } => {
            collect_expr_strings(value, strings);
        }
        Expr::Block(exprs, _) => {
            for e in exprs {
                collect_expr_strings(e, strings);
            }
        }
        Expr::Lambda { body, .. } => {
            collect_expr_strings(body, strings);
        }
        Expr::MakeClosure { captures, .. } => {
            for c in captures {
                collect_expr_strings(c, strings);
            }
        }
        Expr::CallDirect { args, .. }
        | Expr::TailCall { args, .. }
        | Expr::CallNative { args, .. } => {
            for a in args {
                collect_expr_strings(a, strings);
            }
        }
        Expr::CallIndirect { callee, args, .. }
        | Expr::TailCallIndirect { callee, args, .. } => {
            collect_expr_strings(callee, strings);
            for a in args {
                collect_expr_strings(a, strings);
            }
        }
        Expr::MakeList(items, _) | Expr::MakeTuple(items, _) => {
            for item in items {
                collect_expr_strings(item, strings);
            }
        }
        Expr::MakeRange(start, end) => {
            collect_expr_strings(start, strings);
            collect_expr_strings(end, strings);
        }
        Expr::Try { expr, .. } => {
            collect_expr_strings(expr, strings);
        }
        Expr::Expect { actual, .. } => {
            collect_expr_strings(actual, strings);
        }
        Expr::WithHandlers { handlers, body, .. } => {
            collect_expr_strings(body, strings);
            for (_, methods) in handlers {
                for (_, handler_expr) in methods {
                    collect_expr_strings(handler_expr, strings);
                }
            }
        }
        Expr::HandleEffect { body, .. } => {
            collect_expr_strings(body, strings);
        }
        Expr::PerformEffect { args, .. } => {
            for a in args {
                collect_expr_strings(a, strings);
            }
        }
    }
}
