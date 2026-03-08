//! Runtime helper functions for JIT/AOT-generated code.
//!
//! These `extern "C"` functions are called from native code via function pointers
//! or linker symbols. They handle heap-allocating operations (strings, lists,
//! records, enums, closures) and indirect call dispatch.

use std::cell::{Cell, RefCell};

use crate::fiber::{self, Fiber, FiberState};
use crate::nvalue::{
    ALLOC_STATS, HeapObject, NValue, PAYLOAD_MASK, TAG_BOOL, TAG_HEAP, TAG_INT, TAG_MASK, TAG_UNIT,
};
use crate::rc::{self, Rc};
use crate::value::RcStr;

// ---------------------------------------------------------------------------
// NaN-boxing convenience constants (derived from nvalue.rs imports)
// ---------------------------------------------------------------------------

pub const NV_UNIT: u64 = TAG_UNIT;
pub const NV_TRUE: u64 = TAG_BOOL | 1;
pub const NV_FALSE: u64 = TAG_BOOL;
pub const MAX_RANGE_SIZE: i64 = 1_000_000;

// Compile-time assertions: verify NaN-boxing constants match NValue behavior.
const _: () = assert!(NV_UNIT == 0xFFFC_0000_0000_0000);
const _: () = assert!(NV_TRUE == 0xFFFB_0000_0000_0001);
const _: () = assert!(NV_FALSE == 0xFFFB_0000_0000_0000);

// Thread-local error slot for JIT runtime helpers.
thread_local! {
    static JIT_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

// Thread-local arena for heap values created during JIT execution.
thread_local! {
    static JIT_ARENA: RefCell<Vec<NValue>> = const { RefCell::new(Vec::new()) };
}

// Thread-local flag: when true, allocation helpers transfer ownership via
// mem::forget instead of pushing to the arena. RC codegen handles cleanup.
thread_local! {
    static JIT_RC_MODE: Cell<bool> = const { Cell::new(false) };
}

/// Enable or disable RC mode for JIT allocation helpers.
pub fn jit_set_rc_mode(enabled: bool) {
    JIT_RC_MODE.with(|c| c.set(enabled));
}

/// Enable or disable RC mode (extern "C" for AOT).
/// enabled != 0 → RC mode on, enabled == 0 → RC mode off.
#[unsafe(no_mangle)]
pub extern "C" fn jit_set_rc_mode_raw(enabled: u64) {
    jit_set_rc_mode(enabled != 0);
}

/// Transfer ownership of a heap NValue to JIT code.
/// In RC mode: forgets the Rust NValue (caller owns the refcount via raw bits).
/// In arena mode: pushes to the arena (arena keeps value alive until drain).
pub fn jit_own(val: NValue) -> u64 {
    let bits = val.raw();
    if JIT_RC_MODE.with(|c| c.get()) {
        std::mem::forget(val);
    } else {
        jit_arena_push(val);
    }
    bits
}

/// Convert raw bits to an NValue argument.
/// In RC mode: takes ownership (from_raw — caller transferred their ref).
/// In arena mode: borrows (borrow_from_raw — arena keeps values alive).
///
/// # Safety
/// `bits` must be a valid NValue encoding.
#[inline]
unsafe fn jit_take_arg(bits: u64) -> NValue {
    if JIT_RC_MODE.with(|c| c.get()) {
        unsafe { NValue::from_raw(bits) }
    } else {
        unsafe { NValue::borrow_from_raw(bits) }
    }
}

/// Set a JIT runtime error. The error will be checked after execution.
pub fn jit_set_error(msg: String) {
    JIT_ERROR.with(|e| *e.borrow_mut() = Some(msg));
}

/// Take and clear any pending JIT runtime error.
pub fn jit_take_error() -> Option<String> {
    JIT_ERROR.with(|e| e.borrow_mut().take())
}

/// Track a value in the JIT arena, keeping it alive for the duration of execution.
pub fn jit_arena_push(val: NValue) {
    JIT_ARENA.with(|a| a.borrow_mut().push(val));
}

/// Drain the JIT arena, returning all tracked values.
pub fn jit_arena_drain() -> Vec<NValue> {
    JIT_ARENA.with(|a| std::mem::take(&mut *a.borrow_mut()))
}

// Thread-local function pointer table for indirect calls.
thread_local! {
    static JIT_FN_TABLE: RefCell<Option<Vec<*const u8>>> = const { RefCell::new(None) };
}

pub fn set_fn_table(table: &[*const u8]) {
    JIT_FN_TABLE.with(|t| {
        let mut guard = t.borrow_mut();
        if let Some(existing) = guard.as_mut() {
            existing.clear();
            existing.extend_from_slice(table);
        } else {
            *guard = Some(table.to_vec());
        }
    });
}

pub fn clear_fn_table() {
    JIT_FN_TABLE.with(|t| *t.borrow_mut() = None);
}

// Thread-local per-arity trampolines: bridge platform CC → Tail CC.
// Each trampoline takes (fn_ptr, arg0, ..., argN) and does a Cranelift
// indirect call with Tail CC to the target function.
thread_local! {
    static JIT_TRAMPOLINES: Cell<[*const u8; 5]> = const { Cell::new([std::ptr::null(); 5]) };
}

pub fn set_trampolines(trampolines: [*const u8; 5]) {
    JIT_TRAMPOLINES.with(|t| t.set(trampolines));
}

pub fn clear_trampolines() {
    JIT_TRAMPOLINES.with(|t| t.set([std::ptr::null(); 5]));
}

// ---------------------------------------------------------------------------
// Runtime helper functions (extern "C", callable from JIT/AOT code)
// ---------------------------------------------------------------------------

/// Concatenate NValue parts into a new string.
#[unsafe(no_mangle)]
pub extern "C" fn jit_concat(parts: *const u64, count: u64) -> u64 {
    let slice = unsafe { std::slice::from_raw_parts(parts, count as usize) };
    // Estimate capacity from string lengths to avoid repeated reallocs
    let mut cap = 0usize;
    for &bits in slice {
        let nv = unsafe { NValue::borrow_from_raw(bits) };
        if let Some(s) = nv.as_string() {
            cap += s.len();
        } else {
            cap += 20; // conservative estimate for non-string values
        }
    }
    let mut result = String::with_capacity(cap);
    for &bits in slice {
        let nv = unsafe { jit_take_arg(bits) };
        result.push_str(&nv.to_string());
    }
    let nv = NValue::string(result.into());
    jit_own(nv)
}

/// Build an enum variant from a tag (NaN-boxed string) and payload.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_enum(tag_bits: u64, payload_bits: u64) -> u64 {
    let tag_nv = unsafe { jit_take_arg(tag_bits) };
    let payload = unsafe { jit_take_arg(payload_bits) };
    let Some(tag_str) = tag_nv.as_string().cloned() else {
        jit_set_error("enum tag must be a string".to_string());
        return NV_UNIT;
    };
    let result = NValue::enum_val(tag_str, payload);
    jit_own(result)
}

/// Build an enum variant with integer tag_id.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_enum_with_id(tag_bits: u64, tag_id: u64, payload_bits: u64) -> u64 {
    let tag_nv = unsafe { jit_take_arg(tag_bits) };
    let payload = unsafe { jit_take_arg(payload_bits) };
    let Some(tag_str) = tag_nv.as_string().cloned() else {
        jit_set_error("enum tag must be a string".to_string());
        return NV_UNIT;
    };
    let result = NValue::enum_val_with_id(tag_str, tag_id as u32, payload);
    jit_own(result)
}

/// Build a flat enum variant directly from individual field values (no Tuple intermediate).
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_enum_flat(
    tag_bits: u64,
    tag_id: u64,
    items: *const u64,
    count: u64,
) -> u64 {
    let tag_nv = unsafe { jit_take_arg(tag_bits) };
    let Some(tag_str) = tag_nv.as_string().cloned() else {
        jit_set_error("enum tag must be a string".to_string());
        return NV_UNIT;
    };
    let slice = unsafe { std::slice::from_raw_parts(items, count as usize) };
    let payload: Vec<NValue> = slice
        .iter()
        .map(|&bits| unsafe { jit_take_arg(bits) })
        .collect();
    let result = NValue::enum_val_flat(tag_str, tag_id as u32, payload);
    jit_own(result)
}

/// Get a field directly from a flat enum payload by index.
#[unsafe(no_mangle)]
pub extern "C" fn jit_enum_field_get(subject_bits: u64, index: u64) -> u64 {
    let subject = unsafe { NValue::borrow_from_raw(subject_bits) };
    if !subject.is_heap() {
        return NV_UNIT;
    }
    match subject.as_heap_ref() {
        HeapObject::Enum { payload, .. } => {
            let i = index as usize;
            if i < payload.len() {
                jit_own(payload[i].clone())
            } else {
                NV_UNIT
            }
        }
        _ => NV_UNIT,
    }
}

/// Extract all fields from a flat enum payload into a caller-provided buffer.
/// Writes `count` owned u64 values to `out_ptr`. More efficient than
/// calling `jit_enum_field_get` per field (single borrow_from_raw + match).
#[unsafe(no_mangle)]
pub extern "C" fn jit_enum_fields_get_all(subject_bits: u64, out_ptr: *mut u64, count: u64) {
    let n = count as usize;
    let subject = unsafe { NValue::borrow_from_raw(subject_bits) };
    if !subject.is_heap() {
        let out = unsafe { std::slice::from_raw_parts_mut(out_ptr, n) };
        for slot in out.iter_mut() {
            *slot = NV_UNIT;
        }
        return;
    }
    match subject.as_heap_ref() {
        HeapObject::Enum { payload, .. } => {
            let out = unsafe { std::slice::from_raw_parts_mut(out_ptr, n) };
            for (i, slot) in out.iter_mut().enumerate() {
                *slot = if i < payload.len() {
                    jit_own(payload[i].clone())
                } else {
                    NV_UNIT
                };
            }
        }
        _ => {
            let out = unsafe { std::slice::from_raw_parts_mut(out_ptr, n) };
            for slot in out.iter_mut() {
                *slot = NV_UNIT;
            }
        }
    }
}

/// Null out a field in a flat enum payload to reduce the field's refcount.
/// Used before a recursive call so the extracted binding becomes sole owner,
/// enabling cascading in-place updates (clone-on-write).
///
/// Uses raw pointer mutation to bypass Rc::get_mut(). This is safe because:
/// 1. The field was already extracted via jit_enum_field_get (clone) before this call
/// 2. Extra Arc refs are "ghost" refs from the JIT's RC incref-on-read model
/// 3. Those ghost refs only participate in decref at scope exit, never read the payload
/// 4. Execution is single-threaded (no data races)
#[unsafe(no_mangle)]
pub extern "C" fn jit_enum_field_drop(enum_bits: u64, field_idx: u64) {
    if enum_bits & TAG_MASK != TAG_HEAP {
        return;
    }
    let fi = field_idx as usize;
    let ptr = (enum_bits & PAYLOAD_MASK) as *mut HeapObject;
    // SAFETY: See doc comment above. We bypass Arc::get_mut and mutate directly.
    debug_assert!(!ptr.is_null(), "jit_enum_field_drop: null heap pointer");
    debug_assert!(
        unsafe { matches!(&*ptr, HeapObject::Enum { .. }) },
        "jit_enum_field_drop: expected Enum, got non-enum HeapObject"
    );
    unsafe {
        if let HeapObject::Enum { payload, .. } = &mut *ptr
            && fi < payload.len()
        {
            payload[fi] = NValue::unit();
        }
    }
}

/// Update a single field in a flat enum via in-place mutation.
/// Takes ownership of new_value_bits. The enum is mutated in place via raw pointer.
///
/// Uses raw pointer mutation to bypass Rc::get_mut(). This is safe because:
/// 1. jit_enum_field_drop already nulled the old field value
/// 2. Extra Arc refs are "ghost" refs from the JIT's RC incref-on-read model
/// 3. Those ghost refs only participate in decref at scope exit, never read the payload
/// 4. Execution is single-threaded (no data races)
///
/// Returns the same enum_bits (the Arc pointer is unchanged).
#[unsafe(no_mangle)]
pub extern "C" fn jit_enum_field_set(enum_bits: u64, field_idx: u64, new_value_bits: u64) -> u64 {
    if enum_bits & TAG_MASK != TAG_HEAP {
        return NV_UNIT;
    }
    let new_value = unsafe { jit_take_arg(new_value_bits) };
    let i = field_idx as usize;
    let ptr = (enum_bits & PAYLOAD_MASK) as *mut HeapObject;
    // SAFETY: See doc comment above. We bypass Arc::get_mut and mutate directly.
    debug_assert!(!ptr.is_null(), "jit_enum_field_set: null heap pointer");
    debug_assert!(
        unsafe { matches!(&*ptr, HeapObject::Enum { .. }) },
        "jit_enum_field_set: expected Enum, got non-enum HeapObject"
    );
    unsafe {
        if let HeapObject::Enum { payload, .. } = &mut *ptr
            && i < payload.len()
        {
            payload[i] = new_value;
        }
    }
    enum_bits
}

// ---------------------------------------------------------------------------
// Perceus reuse helpers
// ---------------------------------------------------------------------------

/// Drain inner fields of a HeapObject (decrefs children), leaving the Rc shell alive.
/// Returns the original NaN-boxed bits as a reuse token (0 if not a heap value).
/// The compiler has statically proven this is the last use.
///
/// SAFETY: Uses raw pointer mutation to clear the HeapObject's inner fields.
/// Same safety model as jit_enum_field_drop — ghost refs only decref, never read payload.
#[unsafe(no_mangle)]
pub extern "C" fn jit_drop_reuse(bits: u64) -> u64 {
    if bits & TAG_MASK != TAG_HEAP {
        return 0;
    }
    let ptr = (bits & PAYLOAD_MASK) as *mut HeapObject;
    // SAFETY: We drain the inner contents to decref children, but keep the Rc shell alive.
    // The compiler has proven this is the last use of the variable.
    // Use std::mem::take to replace fields with empty defaults — this properly
    // drops items AND frees Vec/RcStr buffers (unlike clear() which leaks buffers
    // when ptr::write overwrites the HeapObject without calling drop).
    unsafe {
        match &mut *ptr {
            HeapObject::Enum { tag, payload, .. } => {
                drop(std::mem::take(tag));
                drop(std::mem::take(payload));
            }
            HeapObject::Record(fields) | HeapObject::Struct { fields, .. } => {
                drop(std::mem::take(fields));
            }
            HeapObject::Tuple(items) | HeapObject::List(items) => {
                drop(std::mem::take(items));
            }
            HeapObject::String(s) => {
                drop(std::mem::take(s));
            }
            _ => {
                // Closure, Map, Set, etc. — not reusable, return 0
                return 0;
            }
        }
    }
    #[cfg(debug_assertions)]
    ALLOC_STATS
        .reuses
        .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    bits
}

/// Build an enum variant with integer tag_id, reusing an existing allocation if possible.
/// If reuse_bits == 0, falls back to fresh allocation.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_enum_reuse(
    reuse_bits: u64,
    tag_bits: u64,
    tag_id: u64,
    payload_bits: u64,
) -> u64 {
    if reuse_bits == 0 {
        return jit_make_enum_with_id(tag_bits, tag_id, payload_bits);
    }
    let tag_nv = unsafe { jit_take_arg(tag_bits) };
    let payload = unsafe { jit_take_arg(payload_bits) };
    let Some(tag_str) = tag_nv.as_string().cloned() else {
        jit_set_error("enum tag must be a string".to_string());
        return NV_UNIT;
    };
    let payload_vec = if payload.is_unit() {
        vec![]
    } else {
        vec![payload]
    };
    let new_obj = HeapObject::Enum {
        tag: tag_str,
        tag_id: tag_id as u32,
        payload: payload_vec,
    };
    let ptr = (reuse_bits & PAYLOAD_MASK) as *mut HeapObject;
    unsafe { std::ptr::write(ptr, new_obj) };
    reuse_bits
}

/// Build a flat enum variant with reuse.
/// If reuse_bits == 0, falls back to fresh allocation.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_enum_flat_reuse(
    reuse_bits: u64,
    tag_bits: u64,
    tag_id: u64,
    items: *const u64,
    count: u64,
) -> u64 {
    if reuse_bits == 0 {
        return jit_make_enum_flat(tag_bits, tag_id, items, count);
    }
    let tag_nv = unsafe { jit_take_arg(tag_bits) };
    let Some(tag_str) = tag_nv.as_string().cloned() else {
        jit_set_error("enum tag must be a string".to_string());
        return NV_UNIT;
    };
    let slice = unsafe { std::slice::from_raw_parts(items, count as usize) };
    let payload: Vec<NValue> = slice
        .iter()
        .map(|&bits| unsafe { jit_take_arg(bits) })
        .collect();
    let new_obj = HeapObject::Enum {
        tag: tag_str,
        tag_id: tag_id as u32,
        payload,
    };
    let ptr = (reuse_bits & PAYLOAD_MASK) as *mut HeapObject;
    unsafe { std::ptr::write(ptr, new_obj) };
    reuse_bits
}

/// Build a record with reuse.
/// If reuse_bits == 0, falls back to fresh allocation.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_record_reuse(reuse_bits: u64, pairs: *const u64, count: u64) -> u64 {
    if reuse_bits == 0 {
        return jit_make_record(pairs, count);
    }
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(pairs, n * 2) };
    let mut fields = Vec::with_capacity(n);
    for i in 0..n {
        let key_nv = unsafe { jit_take_arg(slice[i * 2]) };
        let val_nv = unsafe { jit_take_arg(slice[i * 2 + 1]) };
        let Some(key) = key_nv.as_string().cloned() else {
            jit_set_error("record key must be a string".to_string());
            return NV_UNIT;
        };
        fields.push((key, val_nv));
    }
    // Sort fields alphabetically for O(1) indexed access.
    fields.sort_by(|(a, _), (b, _)| a.cmp(b));
    let new_obj = HeapObject::Record(fields);
    let ptr = (reuse_bits & PAYLOAD_MASK) as *mut HeapObject;
    unsafe { std::ptr::write(ptr, new_obj) };
    reuse_bits
}

/// Build a tuple with reuse.
/// If reuse_bits == 0, falls back to fresh allocation.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_tuple_reuse(reuse_bits: u64, items: *const u64, count: u64) -> u64 {
    if reuse_bits == 0 {
        return jit_make_tuple(items, count);
    }
    let slice = unsafe { std::slice::from_raw_parts(items, count as usize) };
    let nvalues: Vec<NValue> = slice
        .iter()
        .map(|&bits| unsafe { jit_take_arg(bits) })
        .collect();
    let new_obj = HeapObject::Tuple(nvalues);
    let ptr = (reuse_bits & PAYLOAD_MASK) as *mut HeapObject;
    unsafe { std::ptr::write(ptr, new_obj) };
    reuse_bits
}

/// Extract the integer tag_id from a NaN-boxed enum value.
#[unsafe(no_mangle)]
pub extern "C" fn jit_enum_tag_id(subject_bits: u64) -> u64 {
    let nv = unsafe { NValue::borrow_from_raw(subject_bits) };
    if !nv.is_heap() {
        return u32::MAX as u64;
    }
    match nv.as_heap_ref() {
        HeapObject::Enum { tag_id, .. } => *tag_id as u64,
        _ => u32::MAX as u64,
    }
}

/// Build a tuple from items.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_tuple(items: *const u64, count: u64) -> u64 {
    let slice = unsafe { std::slice::from_raw_parts(items, count as usize) };
    let nvalues: Vec<NValue> = slice
        .iter()
        .map(|&bits| unsafe { jit_take_arg(bits) })
        .collect();
    let result = NValue::tuple(nvalues);
    jit_own(result)
}

/// Build a list from items.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_list(items: *const u64, count: u64) -> u64 {
    let slice = unsafe { std::slice::from_raw_parts(items, count as usize) };
    let nvalues: Vec<NValue> = slice
        .iter()
        .map(|&bits| unsafe { jit_take_arg(bits) })
        .collect();
    let result = NValue::list(nvalues);
    jit_own(result)
}

/// Build a record from interleaved key/value pairs.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_record(pairs: *const u64, count: u64) -> u64 {
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(pairs, n * 2) };
    let mut fields = Vec::with_capacity(n);
    for i in 0..n {
        let key_nv = unsafe { jit_take_arg(slice[i * 2]) };
        let val_nv = unsafe { jit_take_arg(slice[i * 2 + 1]) };
        let Some(key) = key_nv.as_string().cloned() else {
            jit_set_error("record key must be a string".to_string());
            return NV_UNIT;
        };
        fields.push((key, val_nv));
    }
    // NValue::record sorts fields alphabetically for O(1) indexed access.
    jit_own(NValue::record(fields))
}

/// Build a named struct from interleaved key/value pairs.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_struct(name_bits: u64, pairs: *const u64, count: u64) -> u64 {
    let name_nv = unsafe { jit_take_arg(name_bits) };
    let Some(name) = name_nv.as_string().cloned() else {
        jit_set_error("struct name must be a string".to_string());
        return NV_UNIT;
    };
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(pairs, n * 2) };
    let mut fields = Vec::with_capacity(n);
    for i in 0..n {
        let key_nv = unsafe { jit_take_arg(slice[i * 2]) };
        let val_nv = unsafe { jit_take_arg(slice[i * 2 + 1]) };
        let Some(key) = key_nv.as_string().cloned() else {
            jit_set_error("struct field key must be a string".to_string());
            return NV_UNIT;
        };
        fields.push((key, val_nv));
    }
    // NValue::struct_val sorts fields alphabetically for O(1) indexed access.
    jit_own(NValue::struct_val(name, fields))
}

/// Get a field from a record or struct by field name.
#[unsafe(no_mangle)]
pub extern "C" fn jit_get_field(object_bits: u64, field_bits: u64) -> u64 {
    let obj = unsafe { NValue::borrow_from_raw(object_bits) };
    let field_nv = unsafe { NValue::borrow_from_raw(field_bits) };
    let Some(field_name) = field_nv.as_string() else {
        jit_set_error("field name must be a string".to_string());
        return NV_UNIT;
    };

    let result = if let Some(fields) = obj.as_record() {
        find_field_index(fields, field_name)
            .map(|idx| fields[idx].1.clone())
            .unwrap_or_else(NValue::unit)
    } else if obj.is_heap() {
        match obj.as_heap_ref() {
            HeapObject::Struct { fields, .. } => find_field_index(fields, field_name)
                .map(|idx| fields[idx].1.clone())
                .unwrap_or_else(NValue::unit),
            _ => NValue::unit(),
        }
    } else {
        NValue::unit()
    };

    jit_own(result)
}

/// Get a field by compile-time index (O(1) access, no string comparison).
#[unsafe(no_mangle)]
pub extern "C" fn jit_get_field_idx(object_bits: u64, idx: u64) -> u64 {
    let obj = unsafe { NValue::borrow_from_raw(object_bits) };
    let i = idx as usize;
    let result = if let Some(fields) = obj.as_record() {
        fields
            .get(i)
            .map(|(_, v)| v.clone())
            .unwrap_or_else(NValue::unit)
    } else if obj.is_heap() {
        match obj.as_heap_ref() {
            HeapObject::Struct { fields, .. } => fields
                .get(i)
                .map(|(_, v)| v.clone())
                .unwrap_or_else(NValue::unit),
            _ => NValue::unit(),
        }
    } else {
        NValue::unit()
    };
    jit_own(result)
}

#[inline]
fn find_field_index(fields: &[(RcStr, NValue)], key: &RcStr) -> Option<usize> {
    fields
        .iter()
        .position(|(k, _)| rc::ptr_eq(k, key) || **k == **key)
}

#[inline]
fn apply_record_updates(
    fields: &mut Vec<(RcStr, NValue)>,
    updates: &[u64],
    count: usize,
    borrow_keys: bool,
    allow_new_fields: bool,
) -> Result<(), String> {
    for i in 0..count {
        let key_nv = if borrow_keys {
            unsafe { NValue::borrow_from_raw(updates[i * 2]) }
        } else {
            unsafe { jit_take_arg(updates[i * 2]) }
        };
        let val_nv = unsafe { jit_take_arg(updates[i * 2 + 1]) };
        let Some(key_ref) = key_nv.as_string() else {
            return Err("record update key must be a string".to_string());
        };
        if let Some(idx) = find_field_index(fields, key_ref) {
            fields[idx].1 = val_nv;
        } else if allow_new_fields {
            // Insert in sorted position to maintain alphabetical field order.
            let pos = fields.partition_point(|(k, _)| k.as_ref() < key_ref.as_ref());
            fields.insert(pos, (key_ref.clone(), val_nv));
        } else {
            return Err(format!("Record has no field '{}'", key_ref));
        }
    }
    Ok(())
}

/// Update a record or named struct by merging in new fields.
#[unsafe(no_mangle)]
pub extern "C" fn jit_update_record(base_bits: u64, updates: *const u64, count: u64) -> u64 {
    let mut base = unsafe { jit_take_arg(base_bits) };
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(updates, n * 2) };

    if !base.is_heap() {
        jit_set_error("update_record on non-record".to_string());
        return NV_UNIT;
    }

    // Fast path: unique ownership allows in-place update.
    if let Some(obj) = base.as_heap_mut() {
        let updated = match obj {
            HeapObject::Record(fields) => apply_record_updates(fields, slice, n, false, true),
            HeapObject::Struct { fields, .. } => {
                apply_record_updates(fields, slice, n, false, false)
            }
            _ => Err("update_record on non-record".to_string()),
        };
        match updated {
            Ok(()) => return jit_own(base),
            Err(msg) => {
                jit_set_error(msg);
                return NV_UNIT;
            }
        }
    }

    // Slow path: clone fields, apply updates.
    match base.as_heap_ref() {
        HeapObject::Record(base_fields) => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            if let Err(msg) = apply_record_updates(&mut fields, slice, n, false, true) {
                jit_set_error(msg);
                return NV_UNIT;
            }
            jit_own(NValue::from_heap_obj(HeapObject::Record(fields)))
        }
        HeapObject::Struct {
            name,
            fields: base_fields,
        } => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            if let Err(msg) = apply_record_updates(&mut fields, slice, n, false, false) {
                jit_set_error(msg);
                return NV_UNIT;
            }
            jit_own(NValue::from_heap_obj(HeapObject::Struct {
                name: name.clone(),
                fields,
            }))
        }
        _ => {
            jit_set_error("update_record on non-record".to_string());
            NV_UNIT
        }
    }
}

/// Update record with borrowed keys (JIT fast path avoiding key allocation).
#[unsafe(no_mangle)]
pub extern "C" fn jit_update_record_borrow_keys(
    base_bits: u64,
    updates: *const u64,
    count: u64,
) -> u64 {
    let mut base = unsafe { jit_take_arg(base_bits) };
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(updates, n * 2) };

    if !base.is_heap() {
        jit_set_error("update_record on non-record".to_string());
        return NV_UNIT;
    }

    // Fast path: unique ownership allows in-place update.
    if let Some(obj) = base.as_heap_mut() {
        let updated = match obj {
            HeapObject::Record(fields) => apply_record_updates(fields, slice, n, true, true),
            HeapObject::Struct { fields, .. } => {
                apply_record_updates(fields, slice, n, true, false)
            }
            _ => Err("update_record on non-record".to_string()),
        };
        match updated {
            Ok(()) => return jit_own(base),
            Err(msg) => {
                jit_set_error(msg);
                return NV_UNIT;
            }
        }
    }

    match base.as_heap_ref() {
        HeapObject::Record(base_fields) => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            if let Err(msg) = apply_record_updates(&mut fields, slice, n, true, true) {
                jit_set_error(msg);
                return NV_UNIT;
            }
            jit_own(NValue::from_heap_obj(HeapObject::Record(fields)))
        }
        HeapObject::Struct {
            name,
            fields: base_fields,
        } => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            if let Err(msg) = apply_record_updates(&mut fields, slice, n, true, false) {
                jit_set_error(msg);
                return NV_UNIT;
            }
            jit_own(NValue::from_heap_obj(HeapObject::Struct {
                name: name.clone(),
                fields,
            }))
        }
        _ => {
            jit_set_error("update_record on non-record".to_string());
            NV_UNIT
        }
    }
}

/// Update record by integer field indices (no string comparison).
/// Updates is interleaved [idx0, val0, idx1, val1, ...].
#[unsafe(no_mangle)]
pub extern "C" fn jit_update_record_indexed(
    base_bits: u64,
    updates: *const u64,
    count: u64,
) -> u64 {
    let mut base = unsafe { jit_take_arg(base_bits) };
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(updates, n * 2) };

    if !base.is_heap() {
        jit_set_error("update_record on non-record".to_string());
        return NV_UNIT;
    }

    // Fast path: unique ownership allows in-place update.
    if let Some(obj) = base.as_heap_mut() {
        let fields = match obj {
            HeapObject::Record(fields) | HeapObject::Struct { fields, .. } => fields,
            _ => {
                jit_set_error("update_record on non-record".to_string());
                return NV_UNIT;
            }
        };
        for i in 0..n {
            let idx = slice[i * 2] as usize;
            let val_nv = unsafe { jit_take_arg(slice[i * 2 + 1]) };
            if idx < fields.len() {
                fields[idx].1 = val_nv;
            }
        }
        return jit_own(base);
    }

    // Slow path: clone fields, apply indexed updates.
    match base.as_heap_ref() {
        HeapObject::Record(base_fields) => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            for i in 0..n {
                let idx = slice[i * 2] as usize;
                let val_nv = unsafe { jit_take_arg(slice[i * 2 + 1]) };
                if idx < fields.len() {
                    fields[idx].1 = val_nv;
                }
            }
            jit_own(NValue::from_heap_obj(HeapObject::Record(fields)))
        }
        HeapObject::Struct {
            name,
            fields: base_fields,
        } => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            for i in 0..n {
                let idx = slice[i * 2] as usize;
                let val_nv = unsafe { jit_take_arg(slice[i * 2 + 1]) };
                if idx < fields.len() {
                    fields[idx].1 = val_nv;
                }
            }
            jit_own(NValue::from_heap_obj(HeapObject::Struct {
                name: name.clone(),
                fields,
            }))
        }
        _ => {
            jit_set_error("update_record on non-record".to_string());
            NV_UNIT
        }
    }
}

/// Fixed-arity indexed update fast path for three fields.
/// Equivalent to `jit_update_record_indexed` with count=3 but avoids
/// interleaved stack array setup in JIT-generated code.
#[unsafe(no_mangle)]
pub extern "C" fn jit_update_record_indexed3(
    base_bits: u64,
    idx0: u64,
    val0_bits: u64,
    idx1: u64,
    val1_bits: u64,
    idx2: u64,
    val2_bits: u64,
) -> u64 {
    let mut base = unsafe { jit_take_arg(base_bits) };
    if !base.is_heap() {
        jit_set_error("update_record on non-record".to_string());
        return NV_UNIT;
    }
    let i0 = idx0 as usize;
    let i1 = idx1 as usize;
    let i2 = idx2 as usize;

    // Fast path: unique ownership allows in-place update.
    if let Some(obj) = base.as_heap_mut() {
        let fields = match obj {
            HeapObject::Record(fields) | HeapObject::Struct { fields, .. } => fields,
            _ => {
                jit_set_error("update_record on non-record".to_string());
                return NV_UNIT;
            }
        };
        let v0 = unsafe { jit_take_arg(val0_bits) };
        let v1 = unsafe { jit_take_arg(val1_bits) };
        let v2 = unsafe { jit_take_arg(val2_bits) };
        if i0 < fields.len() {
            fields[i0].1 = v0;
        }
        if i1 < fields.len() {
            fields[i1].1 = v1;
        }
        if i2 < fields.len() {
            fields[i2].1 = v2;
        }
        return jit_own(base);
    }

    // Slow path: clone fields, apply indexed updates.
    match base.as_heap_ref() {
        HeapObject::Record(base_fields) => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            let v0 = unsafe { jit_take_arg(val0_bits) };
            let v1 = unsafe { jit_take_arg(val1_bits) };
            let v2 = unsafe { jit_take_arg(val2_bits) };
            if i0 < fields.len() {
                fields[i0].1 = v0;
            }
            if i1 < fields.len() {
                fields[i1].1 = v1;
            }
            if i2 < fields.len() {
                fields[i2].1 = v2;
            }
            jit_own(NValue::from_heap_obj(HeapObject::Record(fields)))
        }
        HeapObject::Struct {
            name,
            fields: base_fields,
        } => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            let v0 = unsafe { jit_take_arg(val0_bits) };
            let v1 = unsafe { jit_take_arg(val1_bits) };
            let v2 = unsafe { jit_take_arg(val2_bits) };
            if i0 < fields.len() {
                fields[i0].1 = v0;
            }
            if i1 < fields.len() {
                fields[i1].1 = v1;
            }
            if i2 < fields.len() {
                fields[i2].1 = v2;
            }
            jit_own(NValue::from_heap_obj(HeapObject::Struct {
                name: name.clone(),
                fields,
            }))
        }
        _ => {
            jit_set_error("update_record on non-record".to_string());
            NV_UNIT
        }
    }
}

/// Fixed-arity indexed update for a single field.
/// Used by mutable field assignment: `b.field = val`.
#[unsafe(no_mangle)]
pub extern "C" fn jit_update_record_indexed1(
    base_bits: u64,
    idx0: u64,
    val0_bits: u64,
) -> u64 {
    let mut base = unsafe { jit_take_arg(base_bits) };
    if !base.is_heap() {
        jit_set_error("update_record on non-record".to_string());
        return NV_UNIT;
    }
    let i0 = idx0 as usize;

    // Fast path: unique ownership allows in-place update.
    if let Some(obj) = base.as_heap_mut() {
        let fields = match obj {
            HeapObject::Record(fields) | HeapObject::Struct { fields, .. } => fields,
            _ => {
                jit_set_error("update_record on non-record".to_string());
                return NV_UNIT;
            }
        };
        let v0 = unsafe { jit_take_arg(val0_bits) };
        if i0 < fields.len() {
            fields[i0].1 = v0;
        }
        return jit_own(base);
    }

    // Slow path: clone fields, apply update.
    match base.as_heap_ref() {
        HeapObject::Record(base_fields) => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            let v0 = unsafe { jit_take_arg(val0_bits) };
            if i0 < fields.len() {
                fields[i0].1 = v0;
            }
            jit_own(NValue::from_heap_obj(HeapObject::Record(fields)))
        }
        HeapObject::Struct {
            name,
            fields: base_fields,
        } => {
            let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();
            let v0 = unsafe { jit_take_arg(val0_bits) };
            if i0 < fields.len() {
                fields[i0].1 = v0;
            }
            jit_own(NValue::from_heap_obj(HeapObject::Struct {
                name: name.clone(),
                fields,
            }))
        }
        _ => {
            jit_set_error("update_record on non-record".to_string());
            NV_UNIT
        }
    }
}

/// Build a range (as a list of ints from start..end).
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_range(start_bits: u64, end_bits: u64) -> u64 {
    let start_nv = unsafe { jit_take_arg(start_bits) };
    let end_nv = unsafe { jit_take_arg(end_bits) };
    let start = start_nv.as_int();
    let end = end_nv.as_int();

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
    jit_own(result)
}

/// Check if an enum's tag matches an expected tag string.
#[unsafe(no_mangle)]
pub extern "C" fn jit_enum_tag_eq(subject_bits: u64, expected_tag_bits: u64) -> u64 {
    let subject = unsafe { NValue::borrow_from_raw(subject_bits) };
    let expected = unsafe { NValue::borrow_from_raw(expected_tag_bits) };
    let Some(expected_str) = expected.as_string() else {
        jit_set_error("enum tag comparison requires a string".to_string());
        return NV_FALSE;
    };

    let matches = if let Some((tag, _)) = subject.as_enum() {
        **tag == **expected_str
    } else {
        false
    };

    if matches { NV_TRUE } else { NV_FALSE }
}

/// Extract the payload from an enum.
#[unsafe(no_mangle)]
pub extern "C" fn jit_enum_payload(subject_bits: u64) -> u64 {
    let subject = unsafe { NValue::borrow_from_raw(subject_bits) };
    let result = if let Some((_, payload)) = subject.as_enum() {
        payload.clone()
    } else {
        NValue::unit()
    };
    jit_own(result)
}

/// Get element at index from a tuple.
#[unsafe(no_mangle)]
pub extern "C" fn jit_tuple_get(tuple_bits: u64, index: u64) -> u64 {
    let tuple = unsafe { NValue::borrow_from_raw(tuple_bits) };
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
    jit_own(result)
}

/// Get length of a list.
#[unsafe(no_mangle)]
pub extern "C" fn jit_list_length(list_bits: u64) -> u64 {
    let list = unsafe { NValue::borrow_from_raw(list_bits) };
    let len = list.as_list().map(|l| l.len()).unwrap_or(0);
    NValue::int(len as i64).raw()
}

/// Get element at index from a list.
#[unsafe(no_mangle)]
pub extern "C" fn jit_list_get(list_bits: u64, index_bits: u64) -> u64 {
    let list = unsafe { NValue::borrow_from_raw(list_bits) };
    let idx_nv = unsafe { NValue::borrow_from_raw(index_bits) };
    let idx_i64 = idx_nv.as_int();

    if idx_i64 < 0 {
        let len = list.as_list().map(|l| l.len()).unwrap_or(0);
        jit_set_error(format!("Negative index {} (len {})", idx_i64, len));
        return NV_UNIT;
    }

    let idx = idx_i64 as usize;
    let outcome: Result<NValue, String> = match list.as_list() {
        Some(items) if idx >= items.len() => {
            Err(format!("Index {} out of bounds (len {})", idx, items.len()))
        }
        Some(items) => Ok(items[idx].clone()),
        None => Err("ListGet requires a List".to_string()),
    };
    match outcome {
        Ok(val) => jit_own(val),
        Err(msg) => {
            jit_set_error(msg);
            NV_UNIT
        }
    }
}

/// Check if a value is Err variant.
#[unsafe(no_mangle)]
pub extern "C" fn jit_is_err(val_bits: u64) -> u64 {
    let val = unsafe { NValue::borrow_from_raw(val_bits) };
    let is_err = val.as_enum().is_some_and(|(tag, _)| &**tag == "Err");
    if is_err { NV_TRUE } else { NV_FALSE }
}

/// Check if a value is None variant.
#[unsafe(no_mangle)]
pub extern "C" fn jit_is_none(val_bits: u64) -> u64 {
    let val = unsafe { NValue::borrow_from_raw(val_bits) };
    let is_none = val.as_enum().is_some_and(|(tag, _)| &**tag == "None");
    if is_none { NV_TRUE } else { NV_FALSE }
}

/// Compare two NValues for equality.
#[unsafe(no_mangle)]
pub extern "C" fn jit_values_equal(a_bits: u64, b_bits: u64) -> u64 {
    let a = unsafe { NValue::borrow_from_raw(a_bits) };
    let b = unsafe { NValue::borrow_from_raw(b_bits) };
    if a == b { NV_TRUE } else { NV_FALSE }
}

/// Create a closure: allocate HeapObject::Closure with captured values.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_closure(func_idx: u64, captures_ptr: u64, capture_count: u64) -> u64 {
    let count = capture_count as usize;
    let upvalues: Vec<NValue> = if count == 0 {
        Vec::new()
    } else {
        let slice = unsafe { std::slice::from_raw_parts(captures_ptr as *const u64, count) };
        slice
            .iter()
            .map(|&bits| unsafe { jit_take_arg(bits) })
            .collect()
    };
    let result = NValue::closure(func_idx as usize, upvalues);
    jit_own(result)
}

/// Read a captured variable from a closure's upvalue array.
#[unsafe(no_mangle)]
pub extern "C" fn jit_closure_upvalue(closure_bits: u64, idx: u64) -> u64 {
    let nv = unsafe { NValue::borrow_from_raw(closure_bits) };
    if !nv.is_heap() {
        jit_set_error("GetClosureVar on non-heap value".to_string());
        return NV_UNIT;
    }
    match nv.as_heap_ref() {
        HeapObject::Closure { upvalues, .. } => {
            let i = idx as usize;
            if i < upvalues.len() {
                let val = upvalues[i].clone();
                jit_own(val)
            } else {
                jit_set_error(format!(
                    "Closure upvalue index {} out of bounds (len {})",
                    i,
                    upvalues.len()
                ));
                NV_UNIT
            }
        }
        _ => {
            jit_set_error("GetClosureVar on non-closure heap value".to_string());
            NV_UNIT
        }
    }
}

/// Check if a value is a closure.
#[unsafe(no_mangle)]
pub extern "C" fn jit_is_closure(val_bits: u64) -> u64 {
    let nv = unsafe { NValue::borrow_from_raw(val_bits) };
    if nv.is_heap() && matches!(nv.as_heap_ref(), HeapObject::Closure { .. }) {
        return 1;
    }
    0
}

/// Get the native function pointer for a closure's chunk_idx from the JIT fn table.
#[unsafe(no_mangle)]
pub extern "C" fn jit_closure_fn_ptr(closure_bits: u64) -> u64 {
    let nv = unsafe { NValue::borrow_from_raw(closure_bits) };
    if !nv.is_heap() {
        jit_set_error("closure_fn_ptr on non-heap value".to_string());
        return 0;
    }
    match nv.as_heap_ref() {
        HeapObject::Closure { chunk_idx, .. } => JIT_FN_TABLE.with(|table| {
            let guard = table.borrow();
            let tbl = guard.as_ref().unwrap();
            let ptr = tbl.get(*chunk_idx).copied().unwrap_or(std::ptr::null());
            if ptr.is_null() {
                jit_set_error(format!("Closure chunk_idx {} has no JIT code", chunk_idx));
                0
            } else {
                ptr as u64
            }
        }),
        _ => {
            jit_set_error("closure_fn_ptr on non-closure".to_string());
            0
        }
    }
}

/// Get the native function pointer for a Function value from the JIT fn table.
#[unsafe(no_mangle)]
pub extern "C" fn jit_function_fn_ptr(func_bits: u64) -> u64 {
    let nv = unsafe { NValue::borrow_from_raw(func_bits) };
    if nv.is_function() {
        let idx = (nv.raw() & PAYLOAD_MASK) as usize;
        JIT_FN_TABLE.with(|table| {
            let guard = table.borrow();
            let tbl = guard.as_ref().unwrap();
            let ptr = tbl.get(idx).copied().unwrap_or(std::ptr::null());
            if ptr.is_null() {
                jit_set_error(format!("Function idx {} has no JIT code", idx));
                0
            } else {
                ptr as u64
            }
        })
    } else {
        jit_set_error("function_fn_ptr on non-function value".to_string());
        0
    }
}

/// Concatenate two lists.
#[unsafe(no_mangle)]
pub extern "C" fn jit_list_concat(a_bits: u64, b_bits: u64) -> u64 {
    let a = unsafe { jit_take_arg(a_bits) };
    let b = unsafe { jit_take_arg(b_bits) };

    let a_items = a.as_list().cloned().unwrap_or_default();
    let b_items = b.as_list().cloned().unwrap_or_default();

    let mut combined = a_items;
    combined.extend(b_items);
    let result = NValue::list(combined);
    jit_own(result)
}

/// Concatenate two strings.
#[unsafe(no_mangle)]
pub extern "C" fn jit_string_concat(a_bits: u64, b_bits: u64) -> u64 {
    let a = unsafe { jit_take_arg(a_bits) };
    let b = unsafe { jit_take_arg(b_bits) };

    let mut result = a.to_string();
    result.push_str(&b.to_string());

    let result = NValue::string(result.into());
    jit_own(result)
}

/// Reverse a string (single allocation, no intermediate list).
#[unsafe(no_mangle)]
pub extern "C" fn jit_string_reverse(s_bits: u64) -> u64 {
    let s = unsafe { jit_take_arg(s_bits) };
    match s.as_string() {
        Some(st) => {
            let reversed: String = st.chars().rev().collect();
            jit_own(NValue::string(reversed.into()))
        }
        None => {
            jit_set_error("String.reverse: expected String".to_string());
            NV_UNIT
        }
    }
}

// ---------------------------------------------------------------------------
// AOT-specific helpers
// ---------------------------------------------------------------------------

/// Create a NaN-boxed string from a raw UTF-8 pointer and length.
/// Used by AOT to construct strings from global data sections.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_string(ptr: *const u8, len: u64) -> u64 {
    let bytes = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
    let s = std::str::from_utf8(bytes).unwrap_or("<invalid utf8>");
    let nv = NValue::string(s.into());
    jit_own(nv)
}

/// Print an NValue result to stdout (Display format, no trailing newline).
/// Used by AOT main wrapper to print the program result.
#[unsafe(no_mangle)]
pub extern "C" fn jit_print_result(bits: u64) -> u64 {
    use std::io::Write;
    let nv = unsafe { NValue::borrow_from_raw(bits) };
    if !nv.is_unit() {
        print!("{}", nv);
        let _ = std::io::stdout().flush();
    }
    NV_UNIT
}

// ---------------------------------------------------------------------------
// HOF support helpers (raw / non-NaN-boxed interfaces for inline compilation)
// ---------------------------------------------------------------------------

/// Get the raw (non-NaN-boxed) length of a list.
#[unsafe(no_mangle)]
pub extern "C" fn jit_list_length_raw(list_bits: u64) -> i64 {
    let list = unsafe { NValue::borrow_from_raw(list_bits) };
    list.as_list().map(|l| l.len() as i64).unwrap_or(0)
}

/// Get element at raw index from a list (returns NaN-boxed NValue).
#[unsafe(no_mangle)]
pub extern "C" fn jit_list_get_raw(list_bits: u64, index: i64) -> u64 {
    let list = unsafe { NValue::borrow_from_raw(list_bits) };
    match list.as_list() {
        Some(items) if (index as usize) < items.len() => {
            let val = items[index as usize].clone();
            jit_own(val)
        }
        _ => NV_UNIT,
    }
}

/// Get a sub-list starting from index `start` (tail/slice operation).
/// Returns a new list containing elements[start..].
/// Used by list pattern matching: `[h, ...t]` binds `t` to the tail.
#[unsafe(no_mangle)]
pub extern "C" fn jit_list_tail(list_bits: u64, start: u64) -> u64 {
    let list = unsafe { NValue::borrow_from_raw(list_bits) };
    let result = match list.as_list() {
        Some(items) => {
            let s = start as usize;
            if s >= items.len() {
                NValue::list(Vec::new())
            } else {
                NValue::list(items[s..].to_vec())
            }
        }
        None => NValue::list(Vec::new()),
    };
    jit_own(result)
}

/// Allocate a temporary u64 buffer of `count` elements on the heap.
#[unsafe(no_mangle)]
pub extern "C" fn jit_alloc_buf(count: i64) -> *mut u64 {
    let n = count.max(0) as usize;
    let mut buf = vec![0u64; n];
    let ptr = buf.as_mut_ptr();
    std::mem::forget(buf);
    ptr
}

/// Build a NaN-boxed list from a u64 buffer, then free the buffer.
#[unsafe(no_mangle)]
pub extern "C" fn jit_build_list_from_buf(buf: *mut u64, count: i64) -> u64 {
    let n = count.max(0) as usize;
    let items: Vec<NValue> = if buf.is_null() || n == 0 {
        Vec::new()
    } else {
        let slice = unsafe { std::slice::from_raw_parts(buf, n) };
        let v: Vec<NValue> = slice
            .iter()
            .map(|&bits| unsafe { jit_take_arg(bits) })
            .collect();
        // Free the buffer
        unsafe { drop(Vec::from_raw_parts(buf, n, n)) };
        v
    };
    let result = NValue::list(items);
    jit_own(result)
}

/// Construct Some(payload) enum value (tag_id=1, matching TagRegistry).
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_some(payload: u64) -> u64 {
    let p = unsafe { jit_take_arg(payload) };
    let result = NValue::enum_val_with_id("Some".into(), 1, p);
    jit_own(result)
}

/// Construct None enum value (tag_id=0, matching TagRegistry).
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_none() -> u64 {
    let result = NValue::enum_val_with_id("None".into(), 0, NValue::unit());
    jit_own(result)
}

/// Construct Ok(payload) enum value (tag_id=2, matching TagRegistry).
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_ok(payload: u64) -> u64 {
    let p = unsafe { jit_take_arg(payload) };
    let result = NValue::enum_val_with_id("Ok".into(), 2, p);
    jit_own(result)
}

/// Construct Err(payload) enum value (tag_id=3, matching TagRegistry).
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_err(payload: u64) -> u64 {
    let p = unsafe { jit_take_arg(payload) };
    let result = NValue::enum_val_with_id("Err".into(), 3, p);
    jit_own(result)
}

/// Check if a NaN-boxed value is truthy. Returns raw 1 or 0.
#[unsafe(no_mangle)]
pub extern "C" fn jit_is_truthy(val_bits: u64) -> u64 {
    let nv = unsafe { NValue::borrow_from_raw(val_bits) };
    let truthy = if nv.is_bool() {
        nv.as_bool()
    } else {
        // Non-bool: check bit 0
        (val_bits & 1) != 0
    };
    if truthy { 1 } else { 0 }
}

// ---------------------------------------------------------------------------
// Integer arithmetic helpers (handle BigInt overflow transparently)
// ---------------------------------------------------------------------------

/// Extract a full i64 from a NaN-boxed value (handles both inline int and BigInt).
#[inline(always)]
fn nv_as_any_int(bits: u64) -> Option<i64> {
    if bits & TAG_MASK == TAG_INT {
        // Inline int: sign-extend from 48 bits
        Some(((bits << 16) as i64) >> 16)
    } else if bits & TAG_MASK == TAG_HEAP {
        let nv = unsafe { NValue::borrow_from_raw(bits) };
        match nv.as_heap_ref() {
            HeapObject::BigInt(i) => Some(*i),
            _ => {
                jit_set_error("Type error: expected Int".to_string());
                None
            }
        }
    } else {
        jit_set_error("Type error: expected Int".to_string());
        None
    }
}

/// Convert a raw i64 arithmetic result into a properly NaN-boxed integer value.
/// If the result fits in the 48-bit inline range, returns TAG_INT | payload.
/// Otherwise, allocates a BigInt on the heap.
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_from_i64(val: i64) -> u64 {
    jit_own(NValue::int(val))
}

/// Add two NaN-boxed integers (handles BigInt inputs and overflow).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_add(a: u64, b: u64) -> u64 {
    let (Some(a), Some(b)) = (nv_as_any_int(a), nv_as_any_int(b)) else {
        return NV_UNIT;
    };
    match a.checked_add(b) {
        Some(result) => jit_own(NValue::int(result)),
        None => {
            jit_set_error("Integer overflow in addition".to_string());
            NV_UNIT
        }
    }
}

/// Subtract two NaN-boxed integers (handles BigInt inputs and overflow).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_sub(a: u64, b: u64) -> u64 {
    let (Some(a), Some(b)) = (nv_as_any_int(a), nv_as_any_int(b)) else {
        return NV_UNIT;
    };
    match a.checked_sub(b) {
        Some(result) => jit_own(NValue::int(result)),
        None => {
            jit_set_error("Integer overflow in subtraction".to_string());
            NV_UNIT
        }
    }
}

/// Multiply two NaN-boxed integers (handles BigInt inputs and overflow).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_mul(a: u64, b: u64) -> u64 {
    let (Some(a), Some(b)) = (nv_as_any_int(a), nv_as_any_int(b)) else {
        return NV_UNIT;
    };
    match a.checked_mul(b) {
        Some(result) => jit_own(NValue::int(result)),
        None => {
            jit_set_error("Integer overflow in multiplication".to_string());
            NV_UNIT
        }
    }
}

/// Divide two NaN-boxed integers (handles BigInt inputs).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_div(a: u64, b: u64) -> u64 {
    let Some(bv) = nv_as_any_int(b) else {
        return NV_UNIT;
    };
    if bv == 0 {
        jit_set_error("Division by zero".to_string());
        return NV_UNIT;
    }
    let Some(av) = nv_as_any_int(a) else {
        return NV_UNIT;
    };
    match av.checked_div(bv) {
        Some(result) => jit_own(NValue::int(result)),
        None => {
            jit_set_error("Integer overflow in division".to_string());
            NV_UNIT
        }
    }
}

/// Modulo two NaN-boxed integers (handles BigInt inputs).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_mod(a: u64, b: u64) -> u64 {
    let Some(bv) = nv_as_any_int(b) else {
        return NV_UNIT;
    };
    if bv == 0 {
        jit_set_error("Modulo by zero".to_string());
        return NV_UNIT;
    }
    let Some(av) = nv_as_any_int(a) else {
        return NV_UNIT;
    };
    match av.checked_rem(bv) {
        Some(result) => jit_own(NValue::int(result)),
        None => {
            jit_set_error("Integer overflow in modulo".to_string());
            NV_UNIT
        }
    }
}

/// Negate a NaN-boxed integer (handles BigInt input and overflow).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_neg(a: u64) -> u64 {
    let Some(a) = nv_as_any_int(a) else {
        return NV_UNIT;
    };
    match a.checked_neg() {
        Some(result) => jit_own(NValue::int(result)),
        None => {
            jit_set_error("Integer overflow in negation".to_string());
            NV_UNIT
        }
    }
}

/// Compare two NaN-boxed integers: a < b (handles BigInt inputs).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_lt(a: u64, b: u64) -> u64 {
    let (Some(a), Some(b)) = (nv_as_any_int(a), nv_as_any_int(b)) else {
        return NV_UNIT;
    };
    if a < b { NV_TRUE } else { NV_FALSE }
}

/// Compare two NaN-boxed integers: a <= b (handles BigInt inputs).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_le(a: u64, b: u64) -> u64 {
    let (Some(a), Some(b)) = (nv_as_any_int(a), nv_as_any_int(b)) else {
        return NV_UNIT;
    };
    if a <= b { NV_TRUE } else { NV_FALSE }
}

/// Compare two NaN-boxed integers: a > b (handles BigInt inputs).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_gt(a: u64, b: u64) -> u64 {
    let (Some(a), Some(b)) = (nv_as_any_int(a), nv_as_any_int(b)) else {
        return NV_UNIT;
    };
    if a > b { NV_TRUE } else { NV_FALSE }
}

/// Compare two NaN-boxed integers: a >= b (handles BigInt inputs).
#[unsafe(no_mangle)]
pub extern "C" fn jit_int_ge(a: u64, b: u64) -> u64 {
    let (Some(a), Some(b)) = (nv_as_any_int(a), nv_as_any_int(b)) else {
        return NV_UNIT;
    };
    if a >= b { NV_TRUE } else { NV_FALSE }
}

// ---------------------------------------------------------------------------
// Float arithmetic helpers
// ---------------------------------------------------------------------------

/// Float modulo (no Cranelift fmod instruction; uses Rust's f64 % operator).
#[unsafe(no_mangle)]
pub extern "C" fn jit_float_mod(a: u64, b: u64) -> u64 {
    let fa = f64::from_bits(a);
    let fb = f64::from_bits(b);
    (fa % fb).to_bits()
}

// ---------------------------------------------------------------------------
// Reference counting helpers (for RC-enabled codegen)
// ---------------------------------------------------------------------------

/// Increment the reference count of a NaN-boxed heap value.
/// No-op for non-heap values (Int, Bool, Unit, Function, Float).
/// Returns the same bits for chaining in Cranelift IR.
#[unsafe(no_mangle)]
pub extern "C" fn jit_rc_incref(bits: u64) -> u64 {
    if bits & TAG_MASK == TAG_HEAP {
        let ptr = (bits & PAYLOAD_MASK) as *const HeapObject;
        unsafe {
            rc::increment_strong_count(ptr);
        }
    }
    bits
}

/// Decrement the reference count of a NaN-boxed heap value.
/// No-op for non-heap values. If refcount reaches zero, the value is freed.
#[unsafe(no_mangle)]
pub extern "C" fn jit_rc_decref(bits: u64) {
    if bits & TAG_MASK == TAG_HEAP {
        jit_rc_decref_heap(bits);
    }
}

/// Decrement the reference count of a value already known to be heap-allocated.
/// The caller must have verified `bits & TAG_MASK == TAG_HEAP`.
/// This avoids the tag check when the JIT inlines the non-heap fast path.
#[unsafe(no_mangle)]
pub extern "C" fn jit_rc_decref_heap(bits: u64) {
    let ptr = (bits & PAYLOAD_MASK) as *const HeapObject;
    #[cfg(debug_assertions)]
    {
        let is_last = unsafe {
            let arc = Rc::from_raw(ptr);
            let count = Rc::strong_count(&arc);
            std::mem::forget(arc);
            count == 1
        };
        if is_last {
            crate::nvalue::record_free();
        }
    }
    unsafe {
        rc::decrement_strong_count(ptr);
    }
}

/// Drain the JIT arena (free all intermediate heap values).
/// Must be called AFTER extracting/printing the return value.
#[unsafe(no_mangle)]
pub extern "C" fn jit_drain_arena() -> u64 {
    drop(jit_arena_drain());
    NV_UNIT
}

/// Initialize the function pointer table from a global array.
/// table_ptr points to `count` function pointers (u64-sized entries).
#[unsafe(no_mangle)]
pub extern "C" fn jit_init_fn_table(table_ptr: *const u64, count: u64) -> u64 {
    let n = count as usize;
    let ptrs = if n == 0 || table_ptr.is_null() {
        Vec::new()
    } else {
        let slice = unsafe { std::slice::from_raw_parts(table_ptr, n) };
        slice.iter().map(|&v| v as *const u8).collect()
    };
    set_fn_table(&ptrs);
    NV_UNIT
}

// ---------------------------------------------------------------------------
// Fiber-based effect handler helpers
// ---------------------------------------------------------------------------

/// Thread-local handler context stack. Each entry represents an active
/// `handle ... with { ... }` scope. Nested handlers push new entries.
/// Handler keys and fns are stored here so both the dispatch loop and
/// jit_handler_resume can access them via short-lived borrows.
struct HandlerContext {
    fiber: Box<Fiber>,
    handler_keys: Vec<String>,
    handler_fns: Vec<u64>,
}

thread_local! {
    static HANDLER_CTX: RefCell<Vec<HandlerContext>> = const { RefCell::new(Vec::new()) };
}

/// What the dispatch loop should do next (determined by a short-lived borrow).
enum DispatchAction {
    Return(u64),
    CallHandler {
        handler_fn_bits: u64,
        args: Vec<u64>,
    },
    Error(String),
}

/// Called from within a fiber body when it hits a `perform Effect.method!(args)`.
/// Yields the effect key and args to the handler dispatch loop.
#[unsafe(no_mangle)]
pub extern "C" fn jit_fiber_perform(effect_key: u64, args_ptr: u64, args_count: u64) -> u64 {
    let key_nv = unsafe { NValue::borrow_from_raw(effect_key) };
    let key_str = match key_nv.as_string() {
        Some(s) => s.to_string(),
        None => {
            jit_set_error("jit_fiber_perform: effect_key is not a string".into());
            return NV_UNIT;
        }
    };

    let argc = args_count as usize;
    let args: Vec<u64> = if argc == 0 || args_ptr == 0 {
        vec![]
    } else {
        let ptr = args_ptr as *const u64;
        (0..argc).map(|i| unsafe { *ptr.add(i) }).collect()
    };

    fiber::fiber_yield(key_str, args)
}

/// Called from a handler clause body to resume the suspended fiber with a value.
/// Uses short-lived borrows so it can be called while the dispatch loop is active.
#[unsafe(no_mangle)]
pub extern "C" fn jit_handler_resume(value: u64) -> u64 {
    // Get fiber pointer (short-lived borrow released before resume runs body,
    // which allows nested handlers to borrow HANDLER_CTX).
    let fiber_ptr: *mut Fiber = HANDLER_CTX.with(|ctx| {
        let mut stack = ctx.borrow_mut();
        let handler = stack
            .last_mut()
            .expect("jit_handler_resume: no handler context");
        &mut *handler.fiber as *mut Fiber
    });
    // SAFETY: fiber is owned by HandlerContext on the stack, single-threaded,
    // and no other code accesses this fiber during resume.
    unsafe { (*fiber_ptr).resume(value) };

    // Check fiber state after resume (short-lived borrow).
    let completed = HANDLER_CTX.with(|ctx| {
        let stack = ctx.borrow();
        let handler = stack.last().unwrap();
        match &handler.fiber.state {
            FiberState::Completed(result) => Some(*result),
            FiberState::Yielded { .. } => None,
            _ => {
                jit_set_error("jit_handler_resume: fiber in unexpected state".into());
                Some(NV_UNIT)
            }
        }
    });

    match completed {
        Some(result) => result,
        None => dispatch_loop(),
    }
}

/// Shared dispatch loop used by both `jit_run_fiber_handler` (initial) and
/// `jit_handler_resume` (recursive, when fiber yields again after resume).
///
/// All HANDLER_CTX borrows are short-lived and released before calling handler
/// clauses, which avoids re-entrant RefCell panics.
fn dispatch_loop() -> u64 {
    loop {
        // Short-lived borrow: determine what to do next.
        let action = HANDLER_CTX.with(|ctx| {
            let stack = ctx.borrow();
            let handler = stack.last().unwrap();
            match &handler.fiber.state {
                FiberState::Completed(result) => DispatchAction::Return(*result),
                FiberState::Yielded { key, args } => {
                    let key = key.clone();
                    let args = args.clone();
                    let clause_idx = handler.handler_keys.iter().position(|k| k == &key);
                    match clause_idx {
                        Some(idx) => DispatchAction::CallHandler {
                            handler_fn_bits: handler.handler_fns[idx],
                            args,
                        },
                        None => DispatchAction::Error(format!(
                            "jit_run_fiber_handler: no handler for effect '{}'",
                            key
                        )),
                    }
                }
                FiberState::Ready => {
                    DispatchAction::Error("fiber in Ready state after resume".into())
                }
                FiberState::Aborted => DispatchAction::Error("fiber was aborted".into()),
            }
        }); // borrow dropped — safe to call handler clauses

        match action {
            DispatchAction::Return(result) => return result,
            DispatchAction::CallHandler {
                handler_fn_bits,
                args,
            } => {
                let result = call_handler_clause(handler_fn_bits, &args);

                // If the handler didn't call resume (abort pattern), the fiber
                // is still in Yielded state — mark it aborted.
                HANDLER_CTX.with(|ctx| {
                    let mut stack = ctx.borrow_mut();
                    let handler = stack.last_mut().unwrap();
                    if let FiberState::Yielded { .. } = &handler.fiber.state {
                        handler.fiber.state = FiberState::Aborted;
                    }
                });

                return result;
            }
            DispatchAction::Error(msg) => {
                jit_set_error(msg);
                return NV_UNIT;
            }
        }
    }
}

/// Run a fiber-based effect handler.
///
/// 1. Wraps the body in a fiber
/// 2. Runs the fiber until it completes or yields an effect
/// 3. On yield: matches the effect key to a handler clause and calls it
/// 4. Handler clause may call jit_handler_resume to continue the fiber
#[unsafe(no_mangle)]
pub extern "C" fn jit_run_fiber_handler(
    body_closure: u64,
    handler_keys_ptr: u64,
    handler_fns_ptr: u64,
    handler_count: u64,
) -> u64 {
    let n = handler_count as usize;

    let keys_ptr = handler_keys_ptr as *const u64;
    let fns_ptr = handler_fns_ptr as *const u64;
    let handler_keys: Vec<String> = (0..n)
        .map(|i| {
            let bits = unsafe { *keys_ptr.add(i) };
            let nv = unsafe { NValue::borrow_from_raw(bits) };
            nv.as_string().map(|s| s.to_string()).unwrap_or_default()
        })
        .collect();
    let handler_fns: Vec<u64> = (0..n).map(|i| unsafe { *fns_ptr.add(i) }).collect();

    // Store body closure in thread-local for the fiber entry to retrieve.
    FIBER_BODY_CLOSURE.with(|c| c.set(body_closure));

    let fiber = Fiber::new(fiber_body_entry, 0);

    // Push handler context (short-lived borrow).
    HANDLER_CTX.with(|ctx| {
        ctx.borrow_mut().push(HandlerContext {
            fiber,
            handler_keys,
            handler_fns,
        });
    });

    // Get fiber pointer (short-lived borrow released before resume runs body,
    // which allows nested handlers to borrow HANDLER_CTX).
    let fiber_ptr: *mut Fiber = HANDLER_CTX.with(|ctx| {
        let mut stack = ctx.borrow_mut();
        let handler = stack.last_mut().unwrap();
        &mut *handler.fiber as *mut Fiber
    });
    // SAFETY: fiber is owned by HandlerContext on the stack, single-threaded,
    // and no other code accesses this fiber during resume.
    unsafe { (*fiber_ptr).resume(0) };

    // Dispatch loop (uses only short-lived borrows internally).
    let result = dispatch_loop();

    // Pop handler context (short-lived borrow).
    HANDLER_CTX.with(|ctx| {
        ctx.borrow_mut().pop();
    });

    result
}

/// Call a handler clause function with the yielded args.
/// The handler function may be a closure (with captures) or a plain function.
fn call_handler_clause(handler_fn_bits: u64, args: &[u64]) -> u64 {
    // Get the function pointer from the JIT fn table.
    let nv = unsafe { NValue::borrow_from_raw(handler_fn_bits) };
    let (fn_ptr, is_closure) = if nv.is_heap() {
        match nv.as_heap_ref() {
            HeapObject::Closure { chunk_idx, .. } => {
                let ptr = get_fn_ptr_from_table(*chunk_idx);
                (ptr, true)
            }
            _ => {
                jit_set_error("call_handler_clause: expected function or closure".into());
                return NV_UNIT;
            }
        }
    } else if nv.is_function() {
        let idx = (nv.raw() & PAYLOAD_MASK) as usize;
        let ptr = get_fn_ptr_from_table(idx);
        (ptr, false)
    } else {
        jit_set_error("call_handler_clause: expected function or closure".into());
        return NV_UNIT;
    };

    if fn_ptr.is_null() {
        jit_set_error("call_handler_clause: fn_ptr is null".into());
        return NV_UNIT;
    }

    // Build the argument list. For closures, prepend the closure value itself.
    // Then append the effect args.
    call_jit_fn(fn_ptr, handler_fn_bits, is_closure, args)
}

/// Get a function pointer from the JIT fn table by chunk index.
fn get_fn_ptr_from_table(chunk_idx: usize) -> *const u8 {
    JIT_FN_TABLE.with(|table| {
        let guard = table.borrow();
        match guard.as_ref() {
            Some(tbl) => tbl.get(chunk_idx).copied().unwrap_or(std::ptr::null()),
            None => std::ptr::null(),
        }
    })
}

/// Call a JIT-compiled function with the given args.
/// Handles both closure calls (prepend closure value) and plain function calls.
///
/// JIT functions use Cranelift Tail CC. Calls go through per-arity trampolines
/// that bridge platform CC → Tail CC via Cranelift indirect calls.
fn call_jit_fn(fn_ptr: *const u8, fn_bits: u64, is_closure: bool, args: &[u64]) -> u64 {
    let total_args = if is_closure {
        1 + args.len()
    } else {
        args.len()
    };

    if total_args > 4 {
        jit_set_error(format!(
            "call_jit_fn: too many args ({}), max 4 supported",
            total_args
        ));
        return NV_UNIT;
    }

    let trampolines = JIT_TRAMPOLINES.with(|t| t.get());
    let trampoline = trampolines[total_args];

    if trampoline.is_null() {
        jit_set_error("call_jit_fn: trampoline not initialized".into());
        return NV_UNIT;
    }

    // Each trampoline takes (fn_ptr, arg0, ..., argN) -> u64
    // fn_ptr is a raw pointer, passed as the first argument.
    let fn_ptr_u64 = fn_ptr as u64;

    match total_args {
        0 => {
            let t: extern "C" fn(u64) -> u64 = unsafe { std::mem::transmute(trampoline) };
            t(fn_ptr_u64)
        }
        1 => {
            let t: extern "C" fn(u64, u64) -> u64 = unsafe { std::mem::transmute(trampoline) };
            if is_closure {
                t(fn_ptr_u64, fn_bits)
            } else {
                t(fn_ptr_u64, args[0])
            }
        }
        2 => {
            let t: extern "C" fn(u64, u64, u64) -> u64 = unsafe { std::mem::transmute(trampoline) };
            if is_closure {
                t(fn_ptr_u64, fn_bits, args[0])
            } else {
                t(fn_ptr_u64, args[0], args[1])
            }
        }
        3 => {
            let t: extern "C" fn(u64, u64, u64, u64) -> u64 =
                unsafe { std::mem::transmute(trampoline) };
            if is_closure {
                t(fn_ptr_u64, fn_bits, args[0], args[1])
            } else {
                t(fn_ptr_u64, args[0], args[1], args[2])
            }
        }
        4 => {
            let t: extern "C" fn(u64, u64, u64, u64, u64) -> u64 =
                unsafe { std::mem::transmute(trampoline) };
            if is_closure {
                t(fn_ptr_u64, fn_bits, args[0], args[1], args[2])
            } else {
                t(fn_ptr_u64, args[0], args[1], args[2], args[3])
            }
        }
        _ => unreachable!(),
    }
}

thread_local! {
    static FIBER_BODY_CLOSURE: Cell<u64> = const { Cell::new(0) };
}

/// Entry point for the fiber body. Called on the fiber stack by the trampoline.
/// Retrieves the body closure from the thread-local and calls it.
extern "C" fn fiber_body_entry(_arg: u64) -> u64 {
    let body_bits = FIBER_BODY_CLOSURE.with(|c| c.get());
    call_handler_clause(body_bits, &[])
}
