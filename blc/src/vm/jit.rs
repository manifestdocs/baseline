//! Cranelift JIT compiler for Baseline IR.
//!
//! Compiles `IrFunction`s to native machine code using NaN-boxed NValue
//! representation throughout — the same encoding used by the bytecode VM.
//!
//! All Expr variants are supported. Heap-allocating operations (strings,
//! lists, records, enums, etc.) use `extern "C"` runtime helper functions
//! callable from JIT code.

use std::cell::RefCell;
use std::collections::HashMap;

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{AbiParam, BlockArg, InstBuilder, StackSlotData, StackSlotKind, types};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};

use super::ir::{BinOp, Expr, IrFunction, IrModule, MatchArm, Pattern, UnaryOp};
use super::natives::NativeRegistry;
use super::nvalue::{HeapObject, NValue, PAYLOAD_MASK, TAG_BOOL, TAG_INT, TAG_UNIT};
use crate::analysis::types::Type;

// ---------------------------------------------------------------------------
// NaN-boxing constants (mirrored from nvalue.rs for Cranelift codegen)
// ---------------------------------------------------------------------------

const NV_UNIT: u64 = TAG_UNIT;
const NV_TRUE: u64 = TAG_BOOL | 1;
const NV_FALSE: u64 = TAG_BOOL;
const MAX_RANGE_SIZE: i64 = 1_000_000;

// Thread-local error slot for JIT runtime helpers.
// Since extern "C" functions cannot return Result, runtime errors are stored
// here and checked after JIT execution returns.
thread_local! {
    static JIT_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

/// Set a JIT runtime error. The error will be checked after execution.
fn jit_set_error(msg: String) {
    JIT_ERROR.with(|e| *e.borrow_mut() = Some(msg));
}

/// Take and clear any pending JIT runtime error.
pub fn jit_take_error() -> Option<String> {
    JIT_ERROR.with(|e| e.borrow_mut().take())
}

// ---------------------------------------------------------------------------
// Runtime helper functions (extern "C", callable from JIT code)
// ---------------------------------------------------------------------------
//
// # Safety Invariants for JIT Callbacks
//
// These functions are called from JIT-generated machine code via function
// pointers. The JIT compiler guarantees the following invariants:
//
// 1. **Pointer Validity**: All pointer arguments point to valid memory that
//    remains live for the duration of the call.
//
// 2. **NValue Encoding**: All `u64` arguments representing NValues are valid
//    NaN-boxed encodings as produced by `NValue::raw()`.
//
// 3. **Ownership**: Unless documented otherwise, the caller retains ownership
//    of input NValues. Functions use `borrow_from_raw` for non-consuming reads.
//    Return values transfer ownership to the caller.
//
// 4. **Registry Lifetime**: The `registry` pointer remains valid for the
//    entire JIT execution (it's pinned by the Vm).
//
// 5. **Array Bounds**: The `count` parameter accurately reflects the number
//    of elements in the corresponding array pointer.

/// Call a native function by ID.
///
/// # Safety
///
/// - `registry` must point to a valid, pinned `NativeRegistry`
/// - `args` must point to `count` valid NValue-encoded u64s
/// - The caller retains ownership of the args; this function borrows them
/// - Returns an owned NValue (caller must eventually drop it)
extern "C" fn jit_call_native(
    registry: *const NativeRegistry,
    id: u64,
    args: *const u64,
    count: u64,
) -> u64 {
    // SAFETY: JIT compiler guarantees registry is a valid pointer to a live
    // NativeRegistry that is pinned for the duration of execution.
    let registry = unsafe { &*registry };
    // SAFETY: JIT compiler guarantees args points to count valid u64s.
    let arg_slice = unsafe { std::slice::from_raw_parts(args, count as usize) };
    // Convert raw u64s to NValues (without taking ownership — we just borrow)
    // SAFETY: Each bits value is a valid NValue encoding per JIT invariant.
    let nvalues: Vec<NValue> = arg_slice
        .iter()
        .map(|&bits| unsafe { NValue::borrow_from_raw(bits) })
        .collect();
    match registry.call(id as u16, &nvalues) {
        Ok(result) => {
            let bits = result.raw();
            std::mem::forget(result); // Caller takes ownership via raw bits
            bits
        }
        Err(e) => {
            jit_set_error(format!("Native function error: {}", e.0));
            NV_UNIT
        }
    }
}

/// Concatenate NValue parts into a new string.
///
/// # Safety
///
/// - `parts` must point to `count` valid NValue-encoded u64s
/// - Ownership of the parts is transferred to this function (consumed)
/// - Returns an owned NValue string
extern "C" fn jit_concat(parts: *const u64, count: u64) -> u64 {
    // SAFETY: JIT compiler guarantees parts points to count valid u64s.
    let slice = unsafe { std::slice::from_raw_parts(parts, count as usize) };
    let mut result = String::new();
    for &bits in slice {
        // SAFETY: Each bits value is a valid NValue encoding. We take ownership
        // here and forget after extracting the string representation.
        let nv = unsafe { NValue::from_raw(bits) };
        result.push_str(&nv.to_string());
        std::mem::forget(nv);
    }
    let nv = NValue::string(result.into());
    let bits = nv.raw();
    std::mem::forget(nv);
    bits
}

/// Build an enum variant from a tag (NaN-boxed string) and payload.
///
/// # Safety
///
/// - `tag_bits` must be a valid NValue encoding of a string
/// - `payload_bits` must be a valid NValue encoding
/// - Ownership is transferred; this function consumes both inputs
/// - Returns an owned NValue enum
extern "C" fn jit_make_enum(tag_bits: u64, payload_bits: u64) -> u64 {
    // SAFETY: JIT guarantees both are valid NValue encodings. We take ownership.
    let tag_nv = unsafe { NValue::from_raw(tag_bits) };
    let payload = unsafe { NValue::from_raw(payload_bits) };
    let Some(tag_str) = tag_nv.as_string().cloned() else {
        std::mem::forget(tag_nv);
        std::mem::forget(payload);
        jit_set_error("enum tag must be a string".to_string());
        return NV_UNIT;
    };
    let result = NValue::enum_val(tag_str, payload.clone());
    std::mem::forget(tag_nv);
    std::mem::forget(payload);
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Build a tuple from items.
///
/// # Safety
///
/// - `items` must point to `count` valid NValue-encoded u64s
/// - The caller retains ownership of items; this function borrows them
/// - Returns an owned NValue tuple
extern "C" fn jit_make_tuple(items: *const u64, count: u64) -> u64 {
    // SAFETY: JIT guarantees items points to count valid u64s.
    let slice = unsafe { std::slice::from_raw_parts(items, count as usize) };
    // SAFETY: Each bits value is valid. We borrow (clone) to preserve caller's ownership.
    let nvalues: Vec<NValue> = slice
        .iter()
        .map(|&bits| unsafe { NValue::borrow_from_raw(bits) })
        .collect();
    let result = NValue::tuple(nvalues);
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Build a list from items.
///
/// # Safety
///
/// - `items` must point to `count` valid NValue-encoded u64s
/// - The caller retains ownership of items; this function borrows them
/// - Returns an owned NValue list
extern "C" fn jit_make_list(items: *const u64, count: u64) -> u64 {
    // SAFETY: JIT guarantees items points to count valid u64s.
    let slice = unsafe { std::slice::from_raw_parts(items, count as usize) };
    // SAFETY: Each bits value is valid. We borrow (clone) to preserve caller's ownership.
    let nvalues: Vec<NValue> = slice
        .iter()
        .map(|&bits| unsafe { NValue::borrow_from_raw(bits) })
        .collect();
    let result = NValue::list(nvalues);
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Build a record from interleaved key/value pairs.
extern "C" fn jit_make_record(pairs: *const u64, count: u64) -> u64 {
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(pairs, n * 2) };
    let mut fields = Vec::with_capacity(n);
    for i in 0..n {
        let key_nv = unsafe { NValue::from_raw(slice[i * 2]) };
        let val_nv = unsafe { NValue::from_raw(slice[i * 2 + 1]) };
        let Some(key) = key_nv.as_string().cloned() else {
            std::mem::forget(key_nv);
            std::mem::forget(val_nv);
            jit_set_error("record key must be a string".to_string());
            return NV_UNIT;
        };
        let val = val_nv.clone();
        std::mem::forget(key_nv);
        std::mem::forget(val_nv);
        fields.push((key, val));
    }
    let result = NValue::record(fields);
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Build a named struct from interleaved key/value pairs.
extern "C" fn jit_make_struct(name_bits: u64, pairs: *const u64, count: u64) -> u64 {
    let name_nv = unsafe { NValue::from_raw(name_bits) };
    let Some(name) = name_nv.as_string().cloned() else {
        std::mem::forget(name_nv);
        jit_set_error("struct name must be a string".to_string());
        return NV_UNIT;
    };
    std::mem::forget(name_nv);

    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(pairs, n * 2) };
    let mut fields = Vec::with_capacity(n);
    for i in 0..n {
        let key_nv = unsafe { NValue::from_raw(slice[i * 2]) };
        let val_nv = unsafe { NValue::from_raw(slice[i * 2 + 1]) };
        let Some(key) = key_nv.as_string().cloned() else {
            std::mem::forget(key_nv);
            std::mem::forget(val_nv);
            jit_set_error("struct field key must be a string".to_string());
            return NV_UNIT;
        };
        let val = val_nv.clone();
        std::mem::forget(key_nv);
        std::mem::forget(val_nv);
        fields.push((key, val));
    }
    let result = NValue::struct_val(name, fields);
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Get a field from a record or struct by field name.
extern "C" fn jit_get_field(object_bits: u64, field_bits: u64) -> u64 {
    let obj = unsafe { NValue::from_raw(object_bits) };
    let field_nv = unsafe { NValue::from_raw(field_bits) };
    let Some(field_name) = field_nv.as_string() else {
        std::mem::forget(obj);
        std::mem::forget(field_nv);
        jit_set_error("field name must be a string".to_string());
        return NV_UNIT;
    };

    let result = if let Some(fields) = obj.as_record() {
        fields
            .iter()
            .find(|(k, _)| **k == **field_name)
            .map(|(_, v)| v.clone())
            .unwrap_or(NValue::unit())
    } else if obj.is_heap() {
        match obj.as_heap_ref() {
            HeapObject::Struct { fields, .. } => fields
                .iter()
                .find(|(k, _)| **k == **field_name)
                .map(|(_, v)| v.clone())
                .unwrap_or(NValue::unit()),
            _ => NValue::unit(),
        }
    } else {
        NValue::unit()
    };

    std::mem::forget(obj);
    std::mem::forget(field_nv);
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Update a record by merging in new fields.
extern "C" fn jit_update_record(base_bits: u64, updates: *const u64, count: u64) -> u64 {
    let base = unsafe { NValue::from_raw(base_bits) };
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(updates, n * 2) };

    let Some(base_fields) = base.as_record() else {
        std::mem::forget(base);
        jit_set_error("update_record on non-record".to_string());
        return NV_UNIT;
    };
    let mut fields: Vec<(super::value::RcStr, NValue)> = base_fields.clone();

    for i in 0..n {
        let key_nv = unsafe { NValue::from_raw(slice[i * 2]) };
        let val_nv = unsafe { NValue::from_raw(slice[i * 2 + 1]) };
        let Some(key) = key_nv.as_string().cloned() else {
            std::mem::forget(key_nv);
            std::mem::forget(val_nv);
            std::mem::forget(base);
            jit_set_error("record update key must be a string".to_string());
            return NV_UNIT;
        };
        let val = val_nv.clone();
        std::mem::forget(key_nv);
        std::mem::forget(val_nv);

        if let Some(existing) = fields.iter_mut().find(|(k, _)| *k == key) {
            existing.1 = val;
        } else {
            fields.push((key, val));
        }
    }

    std::mem::forget(base);
    let result = NValue::record(fields);
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Build a range (as a list of ints from start..end).
extern "C" fn jit_make_range(start_bits: u64, end_bits: u64) -> u64 {
    let start_nv = unsafe { NValue::from_raw(start_bits) };
    let end_nv = unsafe { NValue::from_raw(end_bits) };
    let start = start_nv.as_int();
    let end = end_nv.as_int();
    std::mem::forget(start_nv);
    std::mem::forget(end_nv);

    let size = end.saturating_sub(start);
    if size > MAX_RANGE_SIZE {
        jit_set_error(format!(
            "Range too large ({} elements, max {})",
            size, MAX_RANGE_SIZE
        ));
        return NV_UNIT;
    }

    let items: Vec<NValue> = (start..end).map(NValue::int).collect();
    let result = NValue::list(items);
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Check if an enum's tag matches an expected tag string.
extern "C" fn jit_enum_tag_eq(subject_bits: u64, expected_tag_bits: u64) -> u64 {
    let subject = unsafe { NValue::from_raw(subject_bits) };
    let expected = unsafe { NValue::from_raw(expected_tag_bits) };
    let Some(expected_str) = expected.as_string() else {
        std::mem::forget(subject);
        std::mem::forget(expected);
        jit_set_error("enum tag comparison requires a string".to_string());
        return NV_FALSE;
    };

    let matches = if let Some((tag, _)) = subject.as_enum() {
        **tag == **expected_str
    } else {
        false
    };

    std::mem::forget(subject);
    std::mem::forget(expected);
    if matches { NV_TRUE } else { NV_FALSE }
}

/// Extract the payload from an enum.
extern "C" fn jit_enum_payload(subject_bits: u64) -> u64 {
    let subject = unsafe { NValue::from_raw(subject_bits) };
    let result = if let Some((_, payload)) = subject.as_enum() {
        let cloned = payload.clone();
        std::mem::forget(subject);
        cloned
    } else {
        std::mem::forget(subject);
        NValue::unit()
    };
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Get element at index from a tuple.
extern "C" fn jit_tuple_get(tuple_bits: u64, index: u64) -> u64 {
    let tuple = unsafe { NValue::from_raw(tuple_bits) };
    let result = if tuple.is_heap() {
        match tuple.as_heap_ref() {
            HeapObject::Tuple(items) => {
                items.get(index as usize).cloned().unwrap_or(NValue::unit())
            }
            _ => NValue::unit(),
        }
    } else {
        NValue::unit()
    };
    std::mem::forget(tuple);
    let bits = result.raw();
    std::mem::forget(result);
    bits
}

/// Get length of a list.
extern "C" fn jit_list_length(list_bits: u64) -> u64 {
    let list = unsafe { NValue::from_raw(list_bits) };
    let len = list.as_list().map(|l| l.len()).unwrap_or(0);
    std::mem::forget(list);
    NValue::int(len as i64).raw()
}

/// Get element at index from a list.
extern "C" fn jit_list_get(list_bits: u64, index_bits: u64) -> u64 {
    let list = unsafe { NValue::from_raw(list_bits) };
    let idx_nv = unsafe { NValue::from_raw(index_bits) };
    let idx_i64 = idx_nv.as_int();
    std::mem::forget(idx_nv);

    if idx_i64 < 0 {
        let len = list.as_list().map(|l| l.len()).unwrap_or(0);
        std::mem::forget(list);
        jit_set_error(format!("Negative index {} (len {})", idx_i64, len));
        return NV_UNIT;
    }

    let idx = idx_i64 as usize;
    // Extract the element (or error info) while the borrow is active
    let outcome: Result<NValue, String> = match list.as_list() {
        Some(items) if idx >= items.len() => {
            Err(format!("Index {} out of bounds (len {})", idx, items.len()))
        }
        Some(items) => Ok(items[idx].clone()),
        None => Err("ListGet requires a List".to_string()),
    };
    std::mem::forget(list);
    match outcome {
        Ok(val) => {
            let bits = val.raw();
            std::mem::forget(val);
            bits
        }
        Err(msg) => {
            jit_set_error(msg);
            NV_UNIT
        }
    }
}

/// Check if a value is Err variant.
extern "C" fn jit_is_err(val_bits: u64) -> u64 {
    let val = unsafe { NValue::from_raw(val_bits) };
    let is_err = val.as_enum().is_some_and(|(tag, _)| &**tag == "Err");
    std::mem::forget(val);
    if is_err { NV_TRUE } else { NV_FALSE }
}

/// Check if a value is None variant.
extern "C" fn jit_is_none(val_bits: u64) -> u64 {
    let val = unsafe { NValue::from_raw(val_bits) };
    let is_none = val.as_enum().is_some_and(|(tag, _)| &**tag == "None");
    std::mem::forget(val);
    if is_none { NV_TRUE } else { NV_FALSE }
}

/// Compare two NValues for equality (for literal pattern matching).
extern "C" fn jit_values_equal(a_bits: u64, b_bits: u64) -> u64 {
    let a = unsafe { NValue::from_raw(a_bits) };
    let b = unsafe { NValue::from_raw(b_bits) };
    let eq = a == b;
    std::mem::forget(a);
    std::mem::forget(b);
    if eq { NV_TRUE } else { NV_FALSE }
}

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
    /// Whether the entry function uses unboxed codegen (returns raw i64).
    entry_unboxed: bool,
    /// Heap roots: keeps Rc-based NValues alive for JIT code's lifetime.
    _heap_roots: Vec<NValue>,
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
    pub fn run_entry_nvalue(&self) -> Option<NValue> {
        let ptr = self.get_fn(self.entry)?;
        let func: fn() -> u64 = unsafe { std::mem::transmute(ptr) };
        let raw = func();

        if self.entry_unboxed {
            // Entry function returns raw i64 (not NaN-boxed), convert to NValue::Int
            Some(NValue::int(raw as i64))
        } else {
            // Clone semantics: we create an NValue from raw bits. For heap values,
            // we need to bump the refcount since the JIT code doesn't manage Rc.
            let nv = unsafe { NValue::from_raw(raw) };
            let cloned = nv.clone();
            std::mem::forget(nv); // Don't drop — JIT heap_roots own the originals
            Some(cloned)
        }
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

// ---------------------------------------------------------------------------
// JIT Compiler
// ---------------------------------------------------------------------------

/// Names of all runtime helper functions we register with Cranelift.
const HELPER_NAMES: &[&str] = &[
    "jit_call_native",
    "jit_concat",
    "jit_make_enum",
    "jit_make_tuple",
    "jit_make_list",
    "jit_make_record",
    "jit_make_struct",
    "jit_get_field",
    "jit_update_record",
    "jit_make_range",
    "jit_enum_tag_eq",
    "jit_enum_payload",
    "jit_tuple_get",
    "jit_list_length",
    "jit_list_get",
    "jit_is_err",
    "jit_is_none",
    "jit_values_equal",
];

/// Compiles an IrModule to native code via Cranelift.
pub fn compile(module: &IrModule, trace: bool) -> Result<JitProgram, String> {
    compile_with_natives(module, trace, None)
}

/// Compiles an IrModule to native code, optionally with a NativeRegistry
/// for CallNative support.
pub fn compile_with_natives(
    module: &IrModule,
    trace: bool,
    natives: Option<&NativeRegistry>,
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
    builder.symbol("jit_make_tuple", jit_make_tuple as *const u8);
    builder.symbol("jit_make_list", jit_make_list as *const u8);
    builder.symbol("jit_make_record", jit_make_record as *const u8);
    builder.symbol("jit_make_struct", jit_make_struct as *const u8);
    builder.symbol("jit_get_field", jit_get_field as *const u8);
    builder.symbol("jit_update_record", jit_update_record as *const u8);
    builder.symbol("jit_make_range", jit_make_range as *const u8);
    builder.symbol("jit_enum_tag_eq", jit_enum_tag_eq as *const u8);
    builder.symbol("jit_enum_payload", jit_enum_payload as *const u8);
    builder.symbol("jit_tuple_get", jit_tuple_get as *const u8);
    builder.symbol("jit_list_length", jit_list_length as *const u8);
    builder.symbol("jit_list_get", jit_list_get as *const u8);
    builder.symbol("jit_is_err", jit_is_err as *const u8);
    builder.symbol("jit_is_none", jit_is_none as *const u8);
    builder.symbol("jit_values_equal", jit_values_equal as *const u8);

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
            };

            let result = if is_unboxed {
                // Unboxed fast path: compile with raw i64 internally.
                // Returns raw i64 — caller is responsible for tagging if needed.
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
            let mode = if unboxed_flags[i] { "unboxed" } else { "boxed" };
            eprintln!(
                "JIT: compiled '{}' [{}] ({:.1}ms)",
                func.name,
                mode,
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

    Ok(JitProgram {
        _module: jit_module,
        dispatch,
        entry: module.entry,
        entry_unboxed: unboxed_flags.get(module.entry).copied().unwrap_or(false),
        _heap_roots: heap_roots,
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

/// Build a Cranelift signature for a named runtime helper.
fn make_helper_sig(
    module: &mut JITModule,
    name: &str,
    ptr_type: cranelift_codegen::ir::Type,
) -> cranelift_codegen::ir::Signature {
    let mut sig = module.make_signature();
    sig.call_conv = CallConv::SystemV;

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
        | "jit_values_equal" | "jit_list_get" => {
            // (a, b) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_update_record" => {
            // (base, updates_ptr, count) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_enum_payload" | "jit_list_length" | "jit_is_err" | "jit_is_none" => {
            // (val) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        "jit_tuple_get" => {
            // (tuple, index) -> u64
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(types::I64));
        }
        _ => panic!("Unknown helper: {}", name),
    }
    sig
}

/// Check if a function can be JIT-compiled.
fn can_jit(func: &IrFunction, natives: Option<&NativeRegistry>) -> bool {
    expr_can_jit(&func.body, natives)
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
        Expr::Match { subject, arms, .. } => {
            has_self_tail_call(subject, name)
                || arms.iter().any(|a| has_self_tail_call(&a.body, name))
        }
        _ => false,
    }
}

fn expr_can_jit(expr: &Expr, natives: Option<&NativeRegistry>) -> bool {
    match expr {
        Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::Unit | Expr::String(_) => true,
        Expr::Hole => false, // Typed holes should not be JIT-compiled
        Expr::Var(_, _) => true,
        Expr::BinOp { lhs, rhs, .. } => expr_can_jit(lhs, natives) && expr_can_jit(rhs, natives),
        Expr::UnaryOp { operand, .. } => expr_can_jit(operand, natives),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            expr_can_jit(condition, natives)
                && expr_can_jit(then_branch, natives)
                && else_branch
                    .as_ref()
                    .is_none_or(|e| expr_can_jit(e, natives))
        }
        Expr::CallDirect { args, .. } => args.iter().all(|a| expr_can_jit(a, natives)),
        Expr::TailCall { args, .. } => args.iter().all(|a| expr_can_jit(a, natives)),
        Expr::Let { value, .. } => expr_can_jit(value, natives),
        Expr::Block(exprs, _) => exprs.iter().all(|e| expr_can_jit(e, natives)),
        Expr::And(a, b) | Expr::Or(a, b) => expr_can_jit(a, natives) && expr_can_jit(b, natives),
        Expr::Concat(parts) => parts.iter().all(|e| expr_can_jit(e, natives)),
        Expr::CallNative {
            args,
            module,
            method,
            ..
        } => {
            // Need natives registry to resolve the ID
            if natives.is_none() {
                return false;
            }
            let qualified = format!("{}.{}", module, method);
            let reg = natives.unwrap();
            if reg.lookup(&qualified).is_none() {
                return false;
            }
            // HOFs need VM — can't JIT them
            if let Some(id) = reg.lookup(&qualified)
                && reg.is_hof(id)
            {
                return false;
            }
            args.iter().all(|a| expr_can_jit(a, natives))
        }
        Expr::MakeEnum { payload, .. } => expr_can_jit(payload, natives),
        Expr::MakeStruct { fields, .. } => fields.iter().all(|(_, e)| expr_can_jit(e, natives)),
        Expr::MakeList(items, _) => items.iter().all(|e| expr_can_jit(e, natives)),
        Expr::MakeRecord(fields, _) => fields.iter().all(|(_, e)| expr_can_jit(e, natives)),
        Expr::MakeTuple(items, _) => items.iter().all(|e| expr_can_jit(e, natives)),
        Expr::MakeRange(a, b) => expr_can_jit(a, natives) && expr_can_jit(b, natives),
        Expr::UpdateRecord { base, updates, .. } => {
            expr_can_jit(base, natives) && updates.iter().all(|(_, e)| expr_can_jit(e, natives))
        }
        Expr::GetField { object, .. } => expr_can_jit(object, natives),
        Expr::Match { subject, arms, .. } => {
            expr_can_jit(subject, natives) && arms.iter().all(|a| arm_can_jit(a, natives))
        }
        Expr::For { iterable, body, .. } => {
            expr_can_jit(iterable, natives) && expr_can_jit(body, natives)
        }
        Expr::Try { expr, .. } => expr_can_jit(expr, natives),
        // Lambda, CallIndirect, and WithHandlers not yet JIT-supported
        Expr::Lambda { .. } | Expr::CallIndirect { .. } | Expr::WithHandlers { .. } => false,
    }
}

fn arm_can_jit(arm: &MatchArm, natives: Option<&NativeRegistry>) -> bool {
    expr_can_jit(&arm.body, natives)
}

// ---------------------------------------------------------------------------
// Scalar-only analysis for unboxed fast path
// ---------------------------------------------------------------------------

/// Check if a function body only uses scalar types (Int, Bool, Unit, Float).
/// Functions marked scalar-only can use unboxed codegen internally.
fn is_scalar_only(func: &IrFunction) -> bool {
    expr_is_scalar(&func.body)
}

fn expr_is_scalar(expr: &Expr) -> bool {
    match expr {
        Expr::Int(_) | Expr::Bool(_) | Expr::Unit | Expr::Float(_) => true,
        Expr::Hole => false,
        Expr::Var(_, _) => true,
        Expr::BinOp { lhs, rhs, .. } => expr_is_scalar(lhs) && expr_is_scalar(rhs),
        Expr::UnaryOp { operand, .. } => expr_is_scalar(operand),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            expr_is_scalar(condition)
                && expr_is_scalar(then_branch)
                && else_branch.as_ref().is_none_or(|e| expr_is_scalar(e))
        }
        Expr::Let { value, .. } => expr_is_scalar(value),
        Expr::Block(exprs, _) => exprs.iter().all(expr_is_scalar),
        Expr::CallDirect { args, .. } | Expr::TailCall { args, .. } => {
            args.iter().all(expr_is_scalar)
        }
        Expr::And(a, b) | Expr::Or(a, b) => expr_is_scalar(a) && expr_is_scalar(b),
        // Everything else (strings, lists, records, match, enums, etc.) → not scalar
        _ => false,
    }
}

/// Collect all function names called (directly or via tail call) from an expression.
fn collect_called_functions(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::CallDirect { name, args, .. } => {
            out.push(name.clone());
            for a in args {
                collect_called_functions(a, out);
            }
        }
        Expr::TailCall { name, args, .. } => {
            out.push(name.clone());
            for a in args {
                collect_called_functions(a, out);
            }
        }
        Expr::BinOp { lhs, rhs, .. } => {
            collect_called_functions(lhs, out);
            collect_called_functions(rhs, out);
        }
        Expr::UnaryOp { operand, .. } => collect_called_functions(operand, out),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_called_functions(condition, out);
            collect_called_functions(then_branch, out);
            if let Some(e) = else_branch {
                collect_called_functions(e, out);
            }
        }
        Expr::Let { value, .. } => collect_called_functions(value, out),
        Expr::Block(exprs, _) => {
            for e in exprs {
                collect_called_functions(e, out);
            }
        }
        Expr::And(a, b) | Expr::Or(a, b) => {
            collect_called_functions(a, out);
            collect_called_functions(b, out);
        }
        _ => {}
    }
}

/// Compute which functions can use unboxed codegen. A function is unboxed if:
///
/// 1. Its body is scalar-only (no strings, lists, records, etc.)
/// 2. Its return type is Int (the hot path for benchmarks)
/// 3. All functions it calls are also unboxed
///
/// Uses fixed-point iteration to handle mutual recursion.
fn compute_unboxed_flags(module: &IrModule) -> Vec<bool> {
    let func_names: HashMap<String, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, f)| (f.name.clone(), idx))
        .collect();

    // Start optimistic: all scalar-only non-entry functions are candidates.
    // The entry function stays boxed to preserve correct NaN-boxing for the caller.
    let mut unboxed: Vec<bool> = module
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| i != module.entry && is_scalar_only(f))
        .collect();

    // Fixed-point: remove functions whose callees aren't all unboxed
    loop {
        let mut changed = false;
        for (i, func) in module.functions.iter().enumerate() {
            if !unboxed[i] {
                continue;
            }
            let mut callees = Vec::new();
            collect_called_functions(&func.body, &mut callees);
            for callee_name in &callees {
                if let Some(&callee_idx) = func_names.get(callee_name)
                    && callee_idx != i
                    && !unboxed[callee_idx]
                {
                    // Self-recursion is fine, but calling a boxed function isn't
                    unboxed[i] = false;
                    changed = true;
                    break;
                }
            }
        }
        if !changed {
            break;
        }
    }
    unboxed
}

/// Check if an expression only references the given parameter names
/// (no local variables or complex sub-expressions).
fn expr_only_refs_params(expr: &Expr, params: &[String]) -> bool {
    match expr {
        Expr::Var(name, _) => params.iter().any(|p| p == name),
        Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole => true,
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

struct FnCompileCtx<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    func_ids: &'a [Option<FuncId>],
    module: &'a mut JITModule,
    vars: HashMap<String, Variable>,
    next_var: u32,
    func_names: &'a HashMap<String, usize>,
    ir_functions: &'a [IrFunction],
    current_func_name: String,
    param_vars: Vec<Variable>,
    loop_header: Option<cranelift_codegen::ir::Block>,
    heap_roots: &'a mut Vec<NValue>,
    helper_ids: &'a HashMap<&'a str, FuncId>,
    natives: Option<&'a NativeRegistry>,
    ptr_type: cranelift_codegen::ir::Type,
    /// Per-function unboxed flags (indexed by function index).
    unboxed_flags: &'a [bool],
}

type CValue = cranelift_codegen::ir::Value;

impl<'a, 'b> FnCompileCtx<'a, 'b> {
    fn new_var(&mut self) -> Variable {
        let var = self.builder.declare_var(types::I64);
        self.next_var += 1;
        var
    }

    // -- NaN-boxing helpers --

    /// Emit a NaN-boxed NValue constant.
    fn emit_nvalue(&mut self, nv: NValue) -> CValue {
        let bits = nv.raw() as i64;
        self.builder.ins().iconst(types::I64, bits)
    }

    /// Emit a NaN-boxed NValue for a heap object, keeping it rooted.
    fn emit_heap_nvalue(&mut self, nv: NValue) -> CValue {
        let bits = nv.raw() as i64;
        self.heap_roots.push(nv);
        self.builder.ins().iconst(types::I64, bits)
    }

    /// Untag an Int: extract the 48-bit signed payload from NaN-boxed int.
    /// ishl 16, sshr 16
    fn untag_int(&mut self, val: CValue) -> CValue {
        let shifted_left = self.builder.ins().ishl_imm(val, 16);
        self.builder.ins().sshr_imm(shifted_left, 16)
    }

    /// Tag a raw i64 as a NaN-boxed Int: band with PAYLOAD_MASK, bor with TAG_INT.
    fn tag_int(&mut self, val: CValue) -> CValue {
        let mask = self.builder.ins().iconst(types::I64, PAYLOAD_MASK as i64);
        let masked = self.builder.ins().band(val, mask);
        let tag = self.builder.ins().iconst(types::I64, TAG_INT as i64);
        self.builder.ins().bor(masked, tag)
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
                let is_simple = |e: &Expr| {
                    matches!(
                        e,
                        Expr::Var(_, _) | Expr::Int(_) | Expr::Bool(_) | Expr::Unit
                    )
                };
                if is_simple(then_branch) {
                    (condition.as_ref(), then_branch.as_ref(), true)
                } else if let Some(else_br) = else_branch
                    && is_simple(else_br)
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
            self.tag_int(raw)
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

    fn compile_expr_unboxed(&mut self, expr: &Expr) -> Result<CValue, String> {
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
                                    Ok(self.tag_int(raw))
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
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| self.compile_expr_unboxed(a))
                                .collect::<Result<Vec<_>, _>>()?;
                            let call = self.builder.ins().call(func_ref, &arg_vals);
                            Ok(self.builder.inst_results(call)[0])
                        } else {
                            let arg_vals: Vec<CValue> = args
                                .iter()
                                .map(|a| {
                                    let raw = self.compile_expr_unboxed(a)?;
                                    Ok(self.tag_int(raw))
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
                let is_simple = |e: &Expr| {
                    matches!(
                        e,
                        Expr::Var(_, _) | Expr::Int(_) | Expr::Bool(_) | Expr::Unit
                    )
                };
                if is_simple(then_branch) {
                    (condition.as_ref(), then_branch.as_ref(), true)
                } else if let Some(else_br) = else_branch
                    && is_simple(else_br)
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

    fn compile_expr(&mut self, expr: &Expr) -> Result<CValue, String> {
        match expr {
            Expr::Int(n) => Ok(self.emit_nvalue(NValue::int(*n))),

            Expr::Float(f) => Ok(self.emit_nvalue(NValue::float(*f))),

            Expr::Bool(b) => {
                let bits = if *b { NV_TRUE } else { NV_FALSE };
                Ok(self.builder.ins().iconst(types::I64, bits as i64))
            }

            Expr::Unit => Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64)),

            Expr::String(s) => {
                let nv = NValue::string(s.as_str().into());
                Ok(self.emit_heap_nvalue(nv))
            }

            Expr::Var(name, _) => {
                if let Some(&var) = self.vars.get(name) {
                    Ok(self.builder.use_var(var))
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
                    // Untag both operands
                    let l = self.untag_int(lhs_val);
                    let r = self.untag_int(rhs_val);
                    match op {
                        BinOp::Add => {
                            let res = self.builder.ins().iadd(l, r);
                            Ok(self.tag_int(res))
                        }
                        BinOp::Sub => {
                            let res = self.builder.ins().isub(l, r);
                            Ok(self.tag_int(res))
                        }
                        BinOp::Mul => {
                            let res = self.builder.ins().imul(l, r);
                            Ok(self.tag_int(res))
                        }
                        BinOp::Div => {
                            let res = self.builder.ins().sdiv(l, r);
                            Ok(self.tag_int(res))
                        }
                        BinOp::Mod => {
                            let res = self.builder.ins().srem(l, r);
                            Ok(self.tag_int(res))
                        }
                        BinOp::Eq => {
                            let cmp = self.builder.ins().icmp(IntCC::Equal, l, r);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Ne => {
                            let cmp = self.builder.ins().icmp(IntCC::NotEqual, l, r);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Lt => {
                            let cmp = self.builder.ins().icmp(IntCC::SignedLessThan, l, r);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Gt => {
                            let cmp = self.builder.ins().icmp(IntCC::SignedGreaterThan, l, r);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Le => {
                            let cmp = self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, l, r);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::Ge => {
                            let cmp =
                                self.builder
                                    .ins()
                                    .icmp(IntCC::SignedGreaterThanOrEqual, l, r);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::ListConcat => Err("ListConcat cannot be JIT-compiled".into()),
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
                        // For non-int arithmetic, untag and retag (handles mixed types)
                        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                            let l = self.untag_int(lhs_val);
                            let r = self.untag_int(rhs_val);
                            let res = match op {
                                BinOp::Add => self.builder.ins().iadd(l, r),
                                BinOp::Sub => self.builder.ins().isub(l, r),
                                BinOp::Mul => self.builder.ins().imul(l, r),
                                BinOp::Div => self.builder.ins().sdiv(l, r),
                                BinOp::Mod => self.builder.ins().srem(l, r),
                                _ => unreachable!(),
                            };
                            Ok(self.tag_int(res))
                        }
                        BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                            let l = self.untag_int(lhs_val);
                            let r = self.untag_int(rhs_val);
                            let cc = match op {
                                BinOp::Lt => IntCC::SignedLessThan,
                                BinOp::Gt => IntCC::SignedGreaterThan,
                                BinOp::Le => IntCC::SignedLessThanOrEqual,
                                BinOp::Ge => IntCC::SignedGreaterThanOrEqual,
                                _ => unreachable!(),
                            };
                            let cmp = self.builder.ins().icmp(cc, l, r);
                            Ok(self.tag_bool(cmp))
                        }
                        BinOp::ListConcat => Err("ListConcat cannot be JIT-compiled".into()),
                    }
                }
            }

            Expr::UnaryOp { op, operand, .. } => {
                let val = self.compile_expr(operand)?;
                match op {
                    UnaryOp::Neg => {
                        let raw = self.untag_int(val);
                        let negated = self.builder.ins().ineg(raw);
                        Ok(self.tag_int(negated))
                    }
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
                self.bind_pattern(pattern, val)?;
                Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64))
            }

            Expr::Block(exprs, _) => {
                let mut result = self.builder.ins().iconst(types::I64, NV_UNIT as i64);
                for e in exprs {
                    result = self.compile_expr(e)?;
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
                            Ok(self.tag_int(raw_result))
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
                            Ok(self.tag_int(raw_result))
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

            Expr::And(a, b) => {
                let a_val = self.compile_expr(a)?;
                let false_val = self.builder.ins().iconst(types::I64, NV_FALSE as i64);
                let cmp = self.is_truthy(a_val);

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
                let registry = self.natives.ok_or("No native registry for JIT")?;
                let native_id = registry
                    .lookup(&qualified)
                    .ok_or_else(|| format!("Unknown native: {}", qualified))?;

                let arg_vals: Vec<CValue> = args
                    .iter()
                    .map(|a| self.compile_expr(a))
                    .collect::<Result<Vec<_>, _>>()?;

                let args_addr = self.spill_to_stack(&arg_vals);
                let registry_ptr = self
                    .builder
                    .ins()
                    .iconst(self.ptr_type, registry as *const NativeRegistry as i64);
                let id_val = self.builder.ins().iconst(types::I64, native_id as i64);
                let count_val = self.builder.ins().iconst(types::I64, arg_vals.len() as i64);

                Ok(self.call_helper(
                    "jit_call_native",
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
                let tag_nv = NValue::string(tag.as_str().into());
                let tag_val = self.emit_heap_nvalue(tag_nv);
                let payload_val = self.compile_expr(payload)?;
                Ok(self.call_helper("jit_make_enum", &[tag_val, payload_val]))
            }

            Expr::MakeStruct { name, fields, .. } => {
                let name_nv = NValue::string(name.as_str().into());
                let name_val = self.emit_heap_nvalue(name_nv);

                let mut pair_vals = Vec::with_capacity(fields.len() * 2);
                for (fname, fexpr) in fields {
                    let key_nv = NValue::string(fname.as_str().into());
                    let key_val = self.emit_heap_nvalue(key_nv);
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
                    let key_nv = NValue::string(fname.as_str().into());
                    let key_val = self.emit_heap_nvalue(key_nv);
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
                let base_val = self.compile_expr(base)?;

                let mut pair_vals = Vec::with_capacity(updates.len() * 2);
                for (fname, fexpr) in updates {
                    let key_nv = NValue::string(fname.as_str().into());
                    let key_val = self.emit_heap_nvalue(key_nv);
                    let val = self.compile_expr(fexpr)?;
                    pair_vals.push(key_val);
                    pair_vals.push(val);
                }

                let addr = self.spill_to_stack(&pair_vals);
                let count = self.builder.ins().iconst(types::I64, updates.len() as i64);
                Ok(self.call_helper("jit_update_record", &[base_val, addr, count]))
            }

            Expr::GetField { object, field, .. } => {
                let obj_val = self.compile_expr(object)?;
                let field_nv = NValue::string(field.as_str().into());
                let field_val = self.emit_heap_nvalue(field_nv);
                Ok(self.call_helper("jit_get_field", &[obj_val, field_val]))
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

            // Lambda and CallIndirect still unsupported
            _ => Err(format!("Unsupported expression in JIT: {:?}", expr)),
        }
    }

    // -- Match compilation --

    fn compile_match(&mut self, subject: &Expr, arms: &[MatchArm]) -> Result<CValue, String> {
        let subject_val = self.compile_expr(subject)?;

        if arms.is_empty() {
            return Ok(self.builder.ins().iconst(types::I64, NV_UNIT as i64));
        }

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
            self.bind_pattern_vars(&arm.pattern, subj_again)?;
            let body_val = self.compile_expr(&arm.body)?;
            self.builder
                .ins()
                .jump(merge_block, &[BlockArg::Value(body_val)]);

            if let Some(next) = next_test {
                self.builder.switch_to_block(next);
                self.builder.seal_block(next);
            }
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
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
                let tag_nv = NValue::string(tag.as_str().into());
                let tag_val = self.emit_heap_nvalue(tag_nv);
                let eq_result = self.call_helper("jit_enum_tag_eq", &[subject, tag_val]);
                let cmp = self.is_truthy(eq_result);

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
                    let payload = self.call_helper("jit_enum_payload", &[subject]);
                    if sub_patterns.len() == 1 {
                        self.bind_pattern_vars(&sub_patterns[0], payload)?;
                    } else {
                        // Payload is a tuple — extract each element
                        for (i, sub) in sub_patterns.iter().enumerate() {
                            let idx = self.builder.ins().iconst(types::I64, i as i64);
                            let elem = self.call_helper("jit_tuple_get", &[payload, idx]);
                            self.bind_pattern_vars(sub, elem)?;
                        }
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
                if sub_patterns.is_empty() {
                    // Nothing to bind
                } else {
                    let payload = self.call_helper("jit_enum_payload", &[val]);
                    if sub_patterns.len() == 1 {
                        self.bind_pattern(&sub_patterns[0], payload)?;
                    } else {
                        for (i, sub) in sub_patterns.iter().enumerate() {
                            let idx = self.builder.ins().iconst(types::I64, i as i64);
                            let elem = self.call_helper("jit_tuple_get", &[payload, idx]);
                            self.bind_pattern(sub, elem)?;
                        }
                    }
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

        let list = self.builder.use_var(list_var);
        let idx_tagged = self.tag_int(idx);
        let elem = self.call_helper("jit_list_get", &[list, idx_tagged]);

        // Bind loop variable
        let bind_var = self.new_var();
        self.builder.def_var(bind_var, elem);
        self.vars.insert(binding.to_string(), bind_var);

        // Compile body (result is discarded)
        let _body_val = self.compile_expr(body)?;

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
        Ok(payload)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::ir::{IrFunction, IrModule, Span};
    use crate::vm::nvalue::NValue;

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

    /// Compile and run, returning the NaN-boxed result as NValue.
    fn compile_and_run_nvalue(module: &IrModule) -> NValue {
        let program = compile(module, false).expect("JIT compilation failed");
        program
            .run_entry_nvalue()
            .expect("Entry function not compiled")
    }

    /// Compile and run, extracting an i64 from NaN-boxed Int result.
    fn compile_and_run(module: &IrModule) -> i64 {
        let nv = compile_and_run_nvalue(module);
        assert!(nv.is_int(), "Expected Int, got: {}", nv);
        nv.as_int()
    }

    /// Compile and run, extracting a bool from NaN-boxed Bool result.
    fn compile_and_run_bool(module: &IrModule) -> bool {
        let nv = compile_and_run_nvalue(module);
        assert!(nv.is_bool(), "Expected Bool, got: {}", nv);
        nv.as_bool()
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
        assert!(compile_and_run_bool(&module));
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
        assert!(!compile_and_run_bool(&module));
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
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Block(
                    vec![
                        Expr::Let {
                            pattern: Box::new(Pattern::Var("x".into())),
                            value: Box::new(make_int(10)),
                            ty: Some(Type::Int),
                        },
                        Expr::Let {
                            pattern: Box::new(Pattern::Var("y".into())),
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
        // Lambda is still unsupported
        let func = IrFunction {
            name: "test".into(),
            params: vec![],
            body: Expr::Lambda {
                params: vec!["x".into()],
                body: Box::new(Expr::Var("x".into(), None)),
                ty: None,
            },
            ty: None,
            span: dummy_span(),
        };
        assert!(!can_jit(&func, None));
    }

    #[test]
    fn jit_tail_call_as_regular_call() {
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

    // -- Phase 1 additional tests --

    #[test]
    fn jit_nan_boxed_unit() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Unit,
                ty: Some(Type::Unit),
                span: dummy_span(),
            }],
            entry: 0,
        };
        let nv = compile_and_run_nvalue(&module);
        assert!(nv.is_unit());
    }

    #[test]
    fn jit_nan_boxed_bool_true() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Bool(true),
                ty: Some(Type::Bool),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert!(compile_and_run_bool(&module));
    }

    #[test]
    fn jit_nan_boxed_bool_false() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Bool(false),
                ty: Some(Type::Bool),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert!(!compile_and_run_bool(&module));
    }

    #[test]
    fn jit_negative_int() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: make_int(-100),
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), -100);
    }

    // -- Phase 2 tests --

    #[test]
    fn jit_string_literal() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::String("hello".into()),
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
        };
        let nv = compile_and_run_nvalue(&module);
        assert!(nv.is_heap());
        assert_eq!(nv.as_string().unwrap().as_ref(), "hello");
    }

    #[test]
    fn jit_call_native_math_abs() {
        let registry = NativeRegistry::new();
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::CallNative {
                    module: "Math".into(),
                    method: "abs".into(),
                    args: vec![make_int(-42)],
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        let program =
            compile_with_natives(&module, false, Some(&registry)).expect("JIT compilation failed");
        let nv = program
            .run_entry_nvalue()
            .expect("Entry function not compiled");
        assert!(nv.is_int());
        assert_eq!(nv.as_int(), 42);
    }

    // -- Phase 3 tests --

    #[test]
    fn jit_make_tuple() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::MakeTuple(vec![make_int(1), make_int(2), make_int(3)], None),
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
        };
        let nv = compile_and_run_nvalue(&module);
        assert!(nv.is_heap());
        match nv.as_heap_ref() {
            HeapObject::Tuple(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0].as_int(), 1);
                assert_eq!(items[1].as_int(), 2);
                assert_eq!(items[2].as_int(), 3);
            }
            _ => panic!("Expected tuple"),
        }
    }

    #[test]
    fn jit_make_list() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::MakeList(vec![make_int(10), make_int(20)], None),
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
        };
        let nv = compile_and_run_nvalue(&module);
        let items = nv.as_list().unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].as_int(), 10);
        assert_eq!(items[1].as_int(), 20);
    }

    #[test]
    fn jit_make_enum() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::MakeEnum {
                    tag: "Some".into(),
                    payload: Box::new(make_int(42)),
                    ty: None,
                },
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
        };
        let nv = compile_and_run_nvalue(&module);
        let (tag, payload) = nv.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Some");
        assert_eq!(payload.as_int(), 42);
    }

    #[test]
    fn jit_make_record() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::MakeRecord(
                    vec![("x".into(), make_int(1)), ("y".into(), make_int(2))],
                    None,
                ),
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
        };
        let nv = compile_and_run_nvalue(&module);
        let fields = nv.as_record().unwrap();
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].0.as_ref(), "x");
        assert_eq!(fields[0].1.as_int(), 1);
    }

    #[test]
    fn jit_get_field() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::GetField {
                    object: Box::new(Expr::MakeRecord(
                        vec![("x".into(), make_int(10)), ("y".into(), make_int(20))],
                        None,
                    )),
                    field: "y".into(),
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 20);
    }

    // -- Phase 4 tests --

    #[test]
    fn jit_match_wildcard() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Match {
                    subject: Box::new(make_int(42)),
                    arms: vec![MatchArm {
                        pattern: Pattern::Wildcard,
                        body: make_int(99),
                    }],
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 99);
    }

    #[test]
    fn jit_match_var() {
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Match {
                    subject: Box::new(make_int(42)),
                    arms: vec![MatchArm {
                        pattern: Pattern::Var("x".into()),
                        body: make_binop(BinOp::Add, make_var("x"), make_int(1)),
                    }],
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 43);
    }

    #[test]
    fn jit_match_constructor() {
        // match Some(42) { Some(v) => v, None => 0 }
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Match {
                    subject: Box::new(Expr::MakeEnum {
                        tag: "Some".into(),
                        payload: Box::new(make_int(42)),
                        ty: None,
                    }),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::Constructor(
                                "Some".into(),
                                vec![Pattern::Var("v".into())],
                            ),
                            body: Expr::Var("v".into(), Some(Type::Int)),
                        },
                        MatchArm {
                            pattern: Pattern::Constructor("None".into(), vec![]),
                            body: make_int(0),
                        },
                    ],
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 42);
    }

    #[test]
    fn jit_match_literal() {
        // match 2 { 1 => 10, 2 => 20, _ => 30 }
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Match {
                    subject: Box::new(make_int(2)),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::Literal(Box::new(make_int(1))),
                            body: make_int(10),
                        },
                        MatchArm {
                            pattern: Pattern::Literal(Box::new(make_int(2))),
                            body: make_int(20),
                        },
                        MatchArm {
                            pattern: Pattern::Wildcard,
                            body: make_int(30),
                        },
                    ],
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 20);
    }

    // -- Phase 5 tests --

    #[test]
    fn jit_for_loop() {
        // for x in [1, 2, 3] { ... } returns unit
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::For {
                    binding: "x".into(),
                    iterable: Box::new(Expr::MakeList(
                        vec![make_int(1), make_int(2), make_int(3)],
                        None,
                    )),
                    body: Box::new(Expr::Unit),
                },
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
        };
        let nv = compile_and_run_nvalue(&module);
        assert!(nv.is_unit());
    }

    #[test]
    fn jit_try_ok() {
        // let x = Ok(42)?; x
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Block(
                    vec![
                        Expr::Let {
                            pattern: Box::new(Pattern::Var("x".into())),
                            value: Box::new(Expr::Try {
                                expr: Box::new(Expr::MakeEnum {
                                    tag: "Ok".into(),
                                    payload: Box::new(make_int(42)),
                                    ty: None,
                                }),
                                ty: Some(Type::Int),
                            }),
                            ty: Some(Type::Int),
                        },
                        Expr::Var("x".into(), Some(Type::Int)),
                    ],
                    Some(Type::Int),
                ),
                ty: Some(Type::Int),
                span: dummy_span(),
            }],
            entry: 0,
        };
        assert_eq!(compile_and_run(&module), 42);
    }

    #[test]
    fn jit_try_err_propagates() {
        // Err("bad")? should early-return the Err
        let module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Block(
                    vec![
                        Expr::Try {
                            expr: Box::new(Expr::MakeEnum {
                                tag: "Err".into(),
                                payload: Box::new(Expr::String("bad".into())),
                                ty: None,
                            }),
                            ty: None,
                        },
                        make_int(999), // Should not reach here
                    ],
                    None,
                ),
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
        };
        let nv = compile_and_run_nvalue(&module);
        let (tag, _) = nv.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Err");
    }
}
