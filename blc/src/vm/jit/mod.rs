//! Cranelift JIT compiler for Baseline IR.
//!
//! Compiles `IrFunction`s to native machine code using NaN-boxed NValue
//! representation throughout — the same encoding used by the bytecode VM.
//!
//! All Expr variants are supported. Heap-allocating operations (strings,
//! lists, records, enums, etc.) use `extern "C"` runtime helper functions
//! callable from JIT code.

mod analysis;
mod compile;
mod helpers;

#[cfg(feature = "aot")]
pub mod aot;

#[cfg(test)]
mod tests;

use std::collections::HashMap;

use cranelift_codegen::ir::{AbiParam, InstBuilder, types};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use super::ir::IrModule;
use super::natives::NativeRegistry;
use super::nvalue::NValue;

use analysis::{can_jit, compute_unboxed_flags, has_self_tail_call, is_scalar_only};
use compile::FnCompileCtx;
use helpers::*;

pub use helpers::jit_take_error;

/// A JIT-compiled program. Holds the compiled module and a dispatch table
/// mapping function index → native function pointer.
pub struct JitProgram {
    /// Owns the compiled code pages (freed on drop).
    _module: JITModule,
    /// function index → native fn pointer (None = fell back to interpreter).
    dispatch: Vec<Option<*const u8>>,
    /// Entry function index.
    pub entry: usize,
    /// Whether the entry function uses unboxed codegen (returns raw i64).
    entry_unboxed: bool,
    /// Heap roots: keeps Rc-based NValues alive for JIT code's lifetime.
    _heap_roots: Vec<NValue>,
    /// Entry wrapper pointer (platform-default CC, safe to call via transmute).
    /// All language functions use Tail CC; this wrapper bridges to the host ABI.
    entry_wrapper: *const u8,
    /// Whether RC codegen is enabled (scope-based incref/decref).
    rc_enabled: bool,
}

// SAFETY: JitProgram is Send because:
// 1. `_module` (JITModule) owns memory-mapped code pages that are immutable
//    after finalization — no mutable state is accessed post-compilation.
// 2. `dispatch` contains raw pointers to those code pages, which are valid
//    for the lifetime of `_module` and safe to read from any thread.
// 3. `_heap_roots` contains Rc<HeapObject> values — these are NOT Sync, so
//    JitProgram must NOT be Sync. However, moving the entire struct to another
//    thread (Send) is safe because Rc ownership transfers with the move.
// NOTE: JitProgram is NOT Sync. Do not add `unsafe impl Sync`.
unsafe impl Send for JitProgram {}

impl JitProgram {
    /// Get the native function pointer for a given function index.
    /// Returns None if the function was not JIT-compiled.
    pub fn get_fn(&self, idx: usize) -> Option<*const u8> {
        self.dispatch.get(idx).copied().flatten()
    }

    /// Execute the entry function, returning the result as NValue.
    ///
    /// Calls through the platform-CC entry wrapper, which internally calls
    /// the Tail-CC entry function. This avoids ABI mismatch on transmute.
    ///
    /// Heap values created during execution are tracked in a thread-local arena
    /// and freed after the return value is extracted.
    pub fn run_entry_nvalue(&self) -> Option<NValue> {
        if self.entry_wrapper.is_null() {
            return None;
        }
        let func: fn() -> u64 = unsafe { std::mem::transmute(self.entry_wrapper) };

        // Set up the function pointer table for indirect calls
        let fn_table: Vec<*const u8> = self
            .dispatch
            .iter()
            .map(|opt| opt.unwrap_or(std::ptr::null()))
            .collect();
        helpers::set_fn_table(fn_table);

        // Enable RC mode if this program was compiled with RC.
        // In RC mode, allocation helpers use mem::forget instead of arena push.
        if self.rc_enabled {
            helpers::jit_set_rc_mode(true);
        }

        // RAII guard: ensures fn_table and arena are cleaned up on panic.
        // On the normal path, we drop it explicitly after extracting the return value.
        let rc_enabled = self.rc_enabled;
        struct CleanupGuard {
            rc_enabled: bool,
        }
        impl Drop for CleanupGuard {
            fn drop(&mut self) {
                helpers::clear_fn_table();
                if self.rc_enabled {
                    helpers::jit_set_rc_mode(false);
                } else {
                    drop(helpers::jit_arena_drain());
                }
            }
        }
        let guard = CleanupGuard { rc_enabled };

        let raw = func();

        // Extract the return value BEFORE draining the arena.
        // borrow_from_raw increments the Arc refcount, so the return value
        // (and everything reachable from it) survives the arena drain.
        let result = if self.entry_unboxed {
            // Entry function returns raw i64 (not NaN-boxed), convert to NValue::Int
            NValue::int(raw as i64)
        } else {
            // SAFETY: raw is a valid NValue encoding produced by JIT code.
            // borrow_from_raw clones the value, bumping Arc refcount.
            unsafe { NValue::borrow_from_raw(raw) }
        };

        // RC mode: release the JIT code's ownership of the return value.
        // borrow_from_raw bumped the refcount, so we now have two refs:
        // one from jit_own (mem::forget) and one from borrow_from_raw.
        // Decref releases the JIT ownership; `result` keeps the Rust ownership.
        if self.rc_enabled && !self.entry_unboxed {
            jit_rc_decref(raw);
        }

        // Drop guard: clears fn_table and drains arena (frees intermediate heap values).
        // The return value survives because we already cloned it above.
        drop(guard);

        Some(result)
    }

    /// Execute the entry function (must be a 0-arg function returning i64).
    /// Legacy interface: extracts an Int from NaN-boxed result.
    pub fn run_entry(&self) -> Option<i64> {
        let nv = self.run_entry_nvalue()?;
        if nv.is_int() {
            Some(nv.as_int())
        } else if nv.is_bool() {
            Some(nv.as_bool() as i64)
        } else {
            Some(nv.raw() as i64)
        }
    }
}
/// Names of all runtime helper functions we register with Cranelift.
pub(super) const HELPER_NAMES: &[&str] = &[
    "jit_call_native",
    "jit_concat",
    "jit_make_enum",
    "jit_make_enum_with_id",
    "jit_make_enum_flat",
    "jit_make_tuple",
    "jit_make_list",
    "jit_make_record",
    "jit_make_struct",
    "jit_get_field",
    "jit_update_record",
    "jit_make_range",
    "jit_enum_tag_eq",
    "jit_enum_tag_id",
    "jit_enum_payload",
    "jit_enum_field_get",
    "jit_enum_field_drop",
    "jit_enum_field_set",
    "jit_tuple_get",
    "jit_list_length",
    "jit_list_get",
    "jit_is_err",
    "jit_is_none",
    "jit_values_equal",
    // Closure / CallIndirect / ListConcat helpers
    "jit_make_closure",
    "jit_closure_upvalue",
    "jit_is_closure",
    "jit_closure_fn_ptr",
    "jit_function_fn_ptr",
    "jit_list_concat",
    // AOT-specific helpers
    "jit_make_string",
    "jit_print_result",
    "jit_drain_arena",
    "jit_init_fn_table",
    // HOF support helpers
    "jit_list_length_raw",
    "jit_list_get_raw",
    "jit_alloc_buf",
    "jit_build_list_from_buf",
    "jit_make_some",
    "jit_make_none",
    "jit_make_ok",
    "jit_make_err",
    "jit_is_truthy",
    // Integer arithmetic helpers (handle BigInt overflow)
    "jit_int_from_i64",
    "jit_int_add",
    "jit_int_sub",
    "jit_int_mul",
    "jit_int_div",
    "jit_int_mod",
    "jit_int_neg",
    "jit_int_lt",
    "jit_int_le",
    "jit_int_gt",
    "jit_int_ge",
    // Reference counting helpers
    "jit_rc_incref",
    "jit_rc_decref",
    "jit_set_rc_mode_raw",
];

/// Compiles an IrModule to native code via Cranelift (RC enabled by default).
pub fn compile(module: &IrModule, trace: bool) -> Result<JitProgram, String> {
    compile_inner(module, trace, None, true)
}

/// Compiles an IrModule with RC-enabled codegen (alias for compile).
/// Allocation helpers use mem::forget (caller owns refcount via raw bits).
/// Codegen emits jit_rc_incref/jit_rc_decref at scope boundaries.
pub fn compile_rc(module: &IrModule, trace: bool) -> Result<JitProgram, String> {
    compile_inner(module, trace, None, true)
}

/// Compiles an IrModule to native code, optionally with a NativeRegistry
/// for CallNative support (RC enabled by default).
pub fn compile_with_natives(
    module: &IrModule,
    trace: bool,
    natives: Option<&NativeRegistry>,
) -> Result<JitProgram, String> {
    compile_inner(module, trace, natives, true)
}

fn compile_inner(
    module: &IrModule,
    trace: bool,
    natives: Option<&NativeRegistry>,
    rc_enabled: bool,
) -> Result<JitProgram, String> {
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

    let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

    // Register runtime helper symbols
    builder.symbol("jit_call_native", jit_call_native as *const u8);
    builder.symbol("jit_concat", jit_concat as *const u8);
    builder.symbol("jit_make_enum", jit_make_enum as *const u8);
    builder.symbol("jit_make_enum_with_id", jit_make_enum_with_id as *const u8);
    builder.symbol("jit_make_enum_flat", jit_make_enum_flat as *const u8);
    builder.symbol("jit_make_tuple", jit_make_tuple as *const u8);
    builder.symbol("jit_make_list", jit_make_list as *const u8);
    builder.symbol("jit_make_record", jit_make_record as *const u8);
    builder.symbol("jit_make_struct", jit_make_struct as *const u8);
    builder.symbol("jit_get_field", jit_get_field as *const u8);
    builder.symbol("jit_update_record", jit_update_record as *const u8);
    builder.symbol("jit_make_range", jit_make_range as *const u8);
    builder.symbol("jit_enum_tag_eq", jit_enum_tag_eq as *const u8);
    builder.symbol("jit_enum_tag_id", jit_enum_tag_id as *const u8);
    builder.symbol("jit_enum_payload", jit_enum_payload as *const u8);
    builder.symbol("jit_enum_field_get", jit_enum_field_get as *const u8);
    builder.symbol("jit_enum_field_drop", jit_enum_field_drop as *const u8);
    builder.symbol("jit_enum_field_set", jit_enum_field_set as *const u8);
    builder.symbol("jit_tuple_get", jit_tuple_get as *const u8);
    builder.symbol("jit_list_length", jit_list_length as *const u8);
    builder.symbol("jit_list_get", jit_list_get as *const u8);
    builder.symbol("jit_is_err", jit_is_err as *const u8);
    builder.symbol("jit_is_none", jit_is_none as *const u8);
    builder.symbol("jit_values_equal", jit_values_equal as *const u8);
    builder.symbol("jit_make_closure", jit_make_closure as *const u8);
    builder.symbol("jit_closure_upvalue", jit_closure_upvalue as *const u8);
    builder.symbol("jit_is_closure", jit_is_closure as *const u8);
    builder.symbol("jit_closure_fn_ptr", jit_closure_fn_ptr as *const u8);
    builder.symbol("jit_function_fn_ptr", jit_function_fn_ptr as *const u8);
    builder.symbol("jit_list_concat", jit_list_concat as *const u8);
    builder.symbol("jit_make_string", jit_make_string as *const u8);
    builder.symbol("jit_print_result", jit_print_result as *const u8);
    builder.symbol("jit_drain_arena", jit_drain_arena as *const u8);
    builder.symbol("jit_init_fn_table", jit_init_fn_table as *const u8);
    builder.symbol("jit_list_length_raw", jit_list_length_raw as *const u8);
    builder.symbol("jit_list_get_raw", jit_list_get_raw as *const u8);
    builder.symbol("jit_alloc_buf", jit_alloc_buf as *const u8);
    builder.symbol(
        "jit_build_list_from_buf",
        jit_build_list_from_buf as *const u8,
    );
    builder.symbol("jit_make_some", jit_make_some as *const u8);
    builder.symbol("jit_make_none", jit_make_none as *const u8);
    builder.symbol("jit_make_ok", jit_make_ok as *const u8);
    builder.symbol("jit_make_err", jit_make_err as *const u8);
    builder.symbol("jit_is_truthy", jit_is_truthy as *const u8);
    // Integer arithmetic helpers (handle BigInt overflow)
    builder.symbol("jit_int_from_i64", jit_int_from_i64 as *const u8);
    builder.symbol("jit_int_add", jit_int_add as *const u8);
    builder.symbol("jit_int_sub", jit_int_sub as *const u8);
    builder.symbol("jit_int_mul", jit_int_mul as *const u8);
    builder.symbol("jit_int_div", jit_int_div as *const u8);
    builder.symbol("jit_int_mod", jit_int_mod as *const u8);
    builder.symbol("jit_int_neg", jit_int_neg as *const u8);
    builder.symbol("jit_int_lt", jit_int_lt as *const u8);
    builder.symbol("jit_int_le", jit_int_le as *const u8);
    builder.symbol("jit_int_gt", jit_int_gt as *const u8);
    builder.symbol("jit_int_ge", jit_int_ge as *const u8);
    builder.symbol("jit_rc_incref", jit_rc_incref as *const u8);
    builder.symbol("jit_rc_decref", jit_rc_decref as *const u8);
    builder.symbol("jit_set_rc_mode_raw", jit_set_rc_mode_raw as *const u8);

    let mut jit_module = JITModule::new(builder);
    let ptr_type = jit_module.target_config().pointer_type();

    // Declare runtime helpers as imported functions
    let mut helper_ids: HashMap<&str, FuncId> = HashMap::new();
    for &name in HELPER_NAMES {
        let sig = make_helper_sig(&mut jit_module, name, ptr_type);
        let id = jit_module
            .declare_function(name, Linkage::Import, &sig)
            .map_err(|e| e.to_string())?;
        helper_ids.insert(name, id);
    }

    // Heap roots collected during compilation (string constants, tag names, etc.)
    let mut heap_roots: Vec<NValue> = Vec::new();

    // Compute unboxed flags (scalar-only functions with scalar-only callees)
    let unboxed_flags = compute_unboxed_flags(module);

    // Phase 1: Declare all functions
    let mut func_ids: Vec<Option<FuncId>> = Vec::with_capacity(module.functions.len());
    let mut compilable: Vec<bool> = Vec::with_capacity(module.functions.len());

    for func in &module.functions {
        let can = can_jit(func, natives);
        if can {
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
    let mut codegen_ctx = cranelift_codegen::Context::new();

    if trace {
        for (i, func) in module.functions.iter().enumerate() {
            eprintln!(
                "JIT: func[{}] '{}' scalar={} unboxed={} ty={:?}",
                i,
                func.name,
                is_scalar_only(func),
                unboxed_flags[i],
                func.ty
            );
        }
    }

    // Build name→index map once (used by all function compilations)
    let func_names: HashMap<String, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, f)| (f.name.clone(), idx))
        .collect();

    for (i, func) in module.functions.iter().enumerate() {
        if !compilable[i] {
            continue;
        }
        let func_id = func_ids[i].unwrap();
        let start = std::time::Instant::now();

        let mut cl_func = cranelift_codegen::ir::Function::new();
        cl_func.signature = build_signature(&mut jit_module, func.params.len(), ptr_type);

        let compile_result = {
            let mut fn_builder = FunctionBuilder::new(&mut cl_func, &mut fb_ctx);
            let entry_block = fn_builder.create_block();
            fn_builder.append_block_params_for_function_params(entry_block);
            fn_builder.switch_to_block(entry_block);
            fn_builder.seal_block(entry_block);

            let is_unboxed = unboxed_flags[i];
            let mut param_vars = Vec::new();
            let mut vars_map = HashMap::new();
            for (p, param_name) in func.params.iter().enumerate() {
                let var = fn_builder.declare_var(types::I64);
                let param_val = fn_builder.block_params(entry_block)[p];
                // Unboxed functions accept raw i64 params (no untagging needed).
                // Boxed functions accept NaN-boxed params (used as-is).
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

            let mut ctx = FnCompileCtx {
                builder: &mut fn_builder,
                func_ids: &func_ids,
                module: &mut jit_module,
                vars: vars_map,
                next_var: param_vars.len() as u32,
                func_names: &func_names,
                ir_functions: &module.functions,
                current_func_name: func.name.clone(),
                param_vars,
                loop_header,
                heap_roots: &mut heap_roots,
                helper_ids: &helper_ids,
                natives,
                ptr_type,
                unboxed_flags: &unboxed_flags,
                tags: &module.tags,
                sra_records: HashMap::new(),
                aot_strings: None,
                aot_native_ids: None,
                rc_enabled,
                rc_scope_stack: Vec::new(),
                func_call_conv: CallConv::Tail,
            };

            // RC: push function-level scope with parameter variables
            if rc_enabled && !is_unboxed {
                ctx.push_rc_scope();
                let pvars: Vec<Variable> = ctx.param_vars.clone();
                for pv in pvars {
                    ctx.rc_track_var(pv);
                }
            }

            let result = if is_unboxed {
                // Unboxed fast path: compile with raw i64 internally.
                // Returns raw i64 — caller is responsible for tagging if needed.
                ctx.compile_expr_unboxed(&func.body)
            } else {
                ctx.compile_expr(&func.body)
            };

            // RC: pop function-level scope, decref all tracked params.
            // The return value may alias a param, but Var reads emit incref,
            // so decrementing the param still leaves refcount >= 1 for the return.
            let result = if rc_enabled && !is_unboxed {
                match result {
                    Ok(ret_val) => {
                        ctx.pop_rc_scope(None);
                        Ok(ret_val)
                    }
                    err => {
                        ctx.pop_rc_scope(None);
                        err
                    }
                }
            } else {
                result
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

        codegen_ctx.clear();
        codegen_ctx.func = cl_func;
        jit_module
            .define_function(func_id, &mut codegen_ctx)
            .map_err(|e| format!("JIT compile error in '{}': {}", func.name, e))?;

        if trace {
            let elapsed = start.elapsed();
            let mode = if unboxed_flags[i] { "unboxed" } else { "boxed" };
            eprintln!(
                "JIT: compiled '{}' [{}] ({:.1}ms)",
                func.name,
                mode,
                elapsed.as_secs_f64() * 1000.0
            );
        }
    }

    // Phase 2.5: Create entry wrapper with platform-default calling convention.
    // All language functions use Tail CC for guaranteed tail call elimination.
    // The wrapper bridges to the host's C ABI so transmute to fn()->u64 works.
    let entry_wrapper_id: Option<FuncId> = if compilable.get(module.entry).copied().unwrap_or(false)
    {
        let entry_func_id = func_ids[module.entry].unwrap();

        // Platform-default CC (SystemV on Linux, AppleAarch64 on macOS ARM64)
        let mut wrapper_sig = jit_module.make_signature();
        wrapper_sig.returns.push(AbiParam::new(types::I64));

        let wrapper_id = jit_module
            .declare_function("__entry_wrapper", Linkage::Local, &wrapper_sig)
            .map_err(|e| e.to_string())?;

        let mut cl_func = cranelift_codegen::ir::Function::new();
        cl_func.signature = wrapper_sig;

        {
            let mut fn_builder = FunctionBuilder::new(&mut cl_func, &mut fb_ctx);
            let block = fn_builder.create_block();
            fn_builder.switch_to_block(block);
            fn_builder.seal_block(block);

            let func_ref = jit_module.declare_func_in_func(entry_func_id, fn_builder.func);
            let call = fn_builder.ins().call(func_ref, &[]);
            let result = fn_builder.inst_results(call)[0];
            fn_builder.ins().return_(&[result]);
            fn_builder.finalize();
        }

        let mut ctx = cranelift_codegen::Context::for_function(cl_func);
        jit_module
            .define_function(wrapper_id, &mut ctx)
            .map_err(|e| format!("JIT: entry wrapper error: {}", e))?;

        if trace {
            eprintln!("JIT: created entry wrapper (platform CC → Tail CC)");
        }

        Some(wrapper_id)
    } else {
        None
    };

    // Phase 3: Finalize and get function pointers
    jit_module
        .finalize_definitions()
        .map_err(|e| e.to_string())?;

    let mut dispatch: Vec<Option<*const u8>> = Vec::with_capacity(module.functions.len());
    for (i, _) in module.functions.iter().enumerate() {
        if compilable[i] {
            if let Some(fid) = func_ids[i] {
                let ptr = jit_module.get_finalized_function(fid);
                dispatch.push(Some(ptr));
            } else {
                dispatch.push(None);
            }
        } else {
            dispatch.push(None);
        }
    }

    let entry_wrapper_ptr = entry_wrapper_id
        .map(|id| jit_module.get_finalized_function(id))
        .unwrap_or(std::ptr::null());

    Ok(JitProgram {
        _module: jit_module,
        dispatch,
        entry: module.entry,
        entry_unboxed: unboxed_flags.get(module.entry).copied().unwrap_or(false),
        _heap_roots: heap_roots,
        entry_wrapper: entry_wrapper_ptr,
        rc_enabled,
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
    // Tail CC for all language-internal functions (per cranelift-guide.md §6, §10).
    // Enables guaranteed tail call elimination via return_call, at performance
    // parity with SystemV for non-tail calls. The host entry point uses a
    // platform-CC wrapper (__entry_wrapper) to avoid ABI mismatch on transmute.
    sig.call_conv = CallConv::Tail;
    for _ in 0..param_count {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I64));
    sig
}

/// Build a Cranelift signature for a named runtime helper.
pub(super) fn make_helper_sig<M: Module>(
    module: &mut M,
    name: &str,
    ptr_type: cranelift_codegen::ir::Type,
) -> cranelift_codegen::ir::Signature {
    let mut sig = module.make_signature();
    // Platform-default CC (SystemV on Linux x86_64, AppleAarch64 on macOS ARM64).
    // Matches the `extern "C"` ABI of our Rust runtime helper functions.

    match name {
        "jit_call_native" => {
            // (registry_ptr, id, args_ptr, count) -> u64
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_concat" | "jit_make_tuple" | "jit_make_list" => {
            // (items_ptr, count) -> u64
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_make_record" => {
            // (pairs_ptr, field_count) -> u64
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_make_struct" => {
            // (name_bits, pairs_ptr, field_count) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_make_enum" | "jit_make_range" | "jit_get_field" | "jit_enum_tag_eq"
        | "jit_values_equal" | "jit_list_get" | "jit_enum_field_get" => {
            // (a, b) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_make_enum_flat" => {
            // (tag_bits, tag_id, items_ptr, count) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_make_enum_with_id" | "jit_enum_field_set" => {
            // (a, b, c) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_enum_field_drop" => {
            // (enum_bits, field_idx) -> void
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
        }
        "jit_update_record" => {
            // (base, updates_ptr, count) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_enum_tag_id" | "jit_enum_payload" | "jit_list_length" | "jit_is_err"
        | "jit_is_none" | "jit_int_from_i64" | "jit_int_neg" => {
            // (val) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_tuple_get"
        | "jit_closure_upvalue"
        | "jit_list_concat"
        | "jit_int_add"
        | "jit_int_sub"
        | "jit_int_mul"
        | "jit_int_div"
        | "jit_int_mod"
        | "jit_int_lt"
        | "jit_int_le"
        | "jit_int_gt"
        | "jit_int_ge" => {
            // (a, b) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_make_closure" => {
            // (func_idx, captures_ptr, capture_count) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_is_closure" | "jit_closure_fn_ptr" | "jit_function_fn_ptr" => {
            // (val) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_make_string" => {
            // (ptr, len) -> u64
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_print_result" => {
            // (bits) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_drain_arena" => {
            // () -> u64
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_init_fn_table" => {
            // (table_ptr, count) -> u64
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        // HOF support helpers
        "jit_list_length_raw" => {
            // (list_bits) -> i64
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_list_get_raw" => {
            // (list_bits, index: i64) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_alloc_buf" => {
            // (count: i64) -> ptr
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(ptr_type));
        }
        "jit_build_list_from_buf" => {
            // (buf: ptr, count: i64) -> u64
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_make_some" | "jit_make_ok" | "jit_make_err" | "jit_is_truthy" => {
            // (val) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_make_none" => {
            // () -> u64
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_rc_incref" => {
            // (bits: i64) -> i64
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_rc_decref" => {
            // (bits: i64) -> void
            sig.params.push(AbiParam::new(types::I64));
        }
        "jit_set_rc_mode_raw" => {
            // (enabled: i64) -> void
            sig.params.push(AbiParam::new(types::I64));
        }
        _ => panic!("Unknown helper: {}", name),
    }
    sig
}
