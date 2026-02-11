//! Runtime helper functions for JIT/AOT-generated code.
//!
//! These `extern "C"` functions are called from native code via function pointers
//! or linker symbols. They handle heap-allocating operations (strings, lists,
//! records, enums, closures) and indirect call dispatch.

use std::cell::RefCell;

use crate::nvalue::{HeapObject, NValue, PAYLOAD_MASK, TAG_BOOL, TAG_UNIT};
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

pub fn set_fn_table(table: Vec<*const u8>) {
    JIT_FN_TABLE.with(|t| *t.borrow_mut() = Some(table));
}

pub fn clear_fn_table() {
    JIT_FN_TABLE.with(|t| *t.borrow_mut() = None);
}

// ---------------------------------------------------------------------------
// Runtime helper functions (extern "C", callable from JIT/AOT code)
// ---------------------------------------------------------------------------

/// Concatenate NValue parts into a new string.
#[unsafe(no_mangle)]
pub extern "C" fn jit_concat(parts: *const u64, count: u64) -> u64 {
    let slice = unsafe { std::slice::from_raw_parts(parts, count as usize) };
    let mut result = String::new();
    for &bits in slice {
        let nv = unsafe { NValue::borrow_from_raw(bits) };
        result.push_str(&nv.to_string());
    }
    let nv = NValue::string(result.into());
    let bits = nv.raw();
    jit_arena_push(nv);
    bits
}

/// Build an enum variant from a tag (NaN-boxed string) and payload.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_enum(tag_bits: u64, payload_bits: u64) -> u64 {
    let tag_nv = unsafe { NValue::borrow_from_raw(tag_bits) };
    let payload = unsafe { NValue::borrow_from_raw(payload_bits) };
    let Some(tag_str) = tag_nv.as_string().cloned() else {
        jit_set_error("enum tag must be a string".to_string());
        return NV_UNIT;
    };
    let result = NValue::enum_val(tag_str, payload);
    let bits = result.raw();
    jit_arena_push(result);
    bits
}

/// Build an enum variant with integer tag_id.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_enum_with_id(tag_bits: u64, tag_id: u64, payload_bits: u64) -> u64 {
    let tag_nv = unsafe { NValue::borrow_from_raw(tag_bits) };
    let payload = unsafe { NValue::borrow_from_raw(payload_bits) };
    let Some(tag_str) = tag_nv.as_string().cloned() else {
        jit_set_error("enum tag must be a string".to_string());
        return NV_UNIT;
    };
    let result = NValue::enum_val_with_id(tag_str, tag_id as u32, payload);
    let bits = result.raw();
    jit_arena_push(result);
    bits
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
        .map(|&bits| unsafe { NValue::borrow_from_raw(bits) })
        .collect();
    let result = NValue::tuple(nvalues);
    let bits = result.raw();
    jit_arena_push(result);
    bits
}

/// Build a list from items.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_list(items: *const u64, count: u64) -> u64 {
    let slice = unsafe { std::slice::from_raw_parts(items, count as usize) };
    let nvalues: Vec<NValue> = slice
        .iter()
        .map(|&bits| unsafe { NValue::borrow_from_raw(bits) })
        .collect();
    let result = NValue::list(nvalues);
    let bits = result.raw();
    jit_arena_push(result);
    bits
}

/// Build a record from interleaved key/value pairs.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_record(pairs: *const u64, count: u64) -> u64 {
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(pairs, n * 2) };
    let mut fields = Vec::with_capacity(n);
    for i in 0..n {
        let key_nv = unsafe { NValue::borrow_from_raw(slice[i * 2]) };
        let val_nv = unsafe { NValue::borrow_from_raw(slice[i * 2 + 1]) };
        let Some(key) = key_nv.as_string().cloned() else {
            jit_set_error("record key must be a string".to_string());
            return NV_UNIT;
        };
        fields.push((key, val_nv));
    }
    let result = NValue::record(fields);
    let bits = result.raw();
    jit_arena_push(result);
    bits
}

/// Build a named struct from interleaved key/value pairs.
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_struct(name_bits: u64, pairs: *const u64, count: u64) -> u64 {
    let name_nv = unsafe { NValue::borrow_from_raw(name_bits) };
    let Some(name) = name_nv.as_string().cloned() else {
        jit_set_error("struct name must be a string".to_string());
        return NV_UNIT;
    };
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(pairs, n * 2) };
    let mut fields = Vec::with_capacity(n);
    for i in 0..n {
        let key_nv = unsafe { NValue::borrow_from_raw(slice[i * 2]) };
        let val_nv = unsafe { NValue::borrow_from_raw(slice[i * 2 + 1]) };
        let Some(key) = key_nv.as_string().cloned() else {
            jit_set_error("struct field key must be a string".to_string());
            return NV_UNIT;
        };
        fields.push((key, val_nv));
    }
    let result = NValue::struct_val(name, fields);
    let bits = result.raw();
    jit_arena_push(result);
    bits
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

    let bits = result.raw();
    jit_arena_push(result);
    bits
}

/// Update a record by merging in new fields.
#[unsafe(no_mangle)]
pub extern "C" fn jit_update_record(base_bits: u64, updates: *const u64, count: u64) -> u64 {
    let base = unsafe { NValue::borrow_from_raw(base_bits) };
    let n = count as usize;
    let slice = unsafe { std::slice::from_raw_parts(updates, n * 2) };

    let Some(base_fields) = base.as_record() else {
        jit_set_error("update_record on non-record".to_string());
        return NV_UNIT;
    };
    let mut fields: Vec<(RcStr, NValue)> = base_fields.clone();

    for i in 0..n {
        let key_nv = unsafe { NValue::borrow_from_raw(slice[i * 2]) };
        let val_nv = unsafe { NValue::borrow_from_raw(slice[i * 2 + 1]) };
        let Some(key) = key_nv.as_string().cloned() else {
            jit_set_error("record update key must be a string".to_string());
            return NV_UNIT;
        };

        if let Some(existing) = fields.iter_mut().find(|(k, _)| *k == key) {
            existing.1 = val_nv;
        } else {
            fields.push((key, val_nv));
        }
    }

    let result = NValue::record(fields);
    let bits = result.raw();
    jit_arena_push(result);
    bits
}

/// Build a range (as a list of ints from start..end).
#[unsafe(no_mangle)]
pub extern "C" fn jit_make_range(start_bits: u64, end_bits: u64) -> u64 {
    let start_nv = unsafe { NValue::borrow_from_raw(start_bits) };
    let end_nv = unsafe { NValue::borrow_from_raw(end_bits) };
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
    let bits = result.raw();
    jit_arena_push(result);
    bits
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
    let bits = result.raw();
    jit_arena_push(result);
    bits
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
    let bits = result.raw();
    jit_arena_push(result);
    bits
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
        Ok(val) => {
            let bits = val.raw();
            jit_arena_push(val);
            bits
        }
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
            .map(|&bits| unsafe { NValue::borrow_from_raw(bits) })
            .collect()
    };
    let result = NValue::closure(func_idx as usize, upvalues);
    let bits = result.raw();
    jit_arena_push(result);
    bits
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
                let bits = val.raw();
                jit_arena_push(val);
                bits
            } else {
                jit_set_error(format!("Closure upvalue index {} out of bounds (len {})", i, upvalues.len()));
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
    if nv.is_heap() {
        if matches!(nv.as_heap_ref(), HeapObject::Closure { .. }) {
            return 1;
        }
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
        HeapObject::Closure { chunk_idx, .. } => {
            JIT_FN_TABLE.with(|table| {
                let guard = table.borrow();
                let tbl = guard.as_ref().unwrap();
                let ptr = tbl.get(*chunk_idx).copied().unwrap_or(std::ptr::null());
                if ptr.is_null() {
                    jit_set_error(format!("Closure chunk_idx {} has no JIT code", chunk_idx));
                    0
                } else {
                    ptr as u64
                }
            })
        }
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
    let a = unsafe { NValue::borrow_from_raw(a_bits) };
    let b = unsafe { NValue::borrow_from_raw(b_bits) };

    let a_items = a.as_list().cloned().unwrap_or_default();
    let b_items = b.as_list().cloned().unwrap_or_default();

    let mut combined = a_items;
    combined.extend(b_items);
    let result = NValue::list(combined);
    let bits = result.raw();
    jit_arena_push(result);
    bits
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
    let bits = nv.raw();
    jit_arena_push(nv);
    bits
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
    set_fn_table(ptrs);
    NV_UNIT
}
