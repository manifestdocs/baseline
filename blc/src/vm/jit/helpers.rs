//! Runtime helper functions for JIT-generated code.
//!
//! Most helpers live in `baseline_rt::helpers`. This module re-exports them
//! and adds `jit_call_native` which depends on `NativeRegistry` (blc-only).

use super::super::natives::NativeRegistry;
use super::super::nvalue::NValue;
use std::ptr::NonNull;

// Re-export everything from baseline-rt helpers
pub use baseline_rt::helpers::*;

fn registry_ref<'a>(registry: *const NativeRegistry) -> Result<&'a NativeRegistry, &'static str> {
    let ptr = NonNull::new(registry.cast_mut()).ok_or("null NativeRegistry pointer")?;
    // SAFETY: pointer validity is guaranteed by the JIT caller contract.
    Ok(unsafe { ptr.as_ref() })
}

fn args_bits<'a>(args: *const u64, count: u64) -> Result<&'a [u64], &'static str> {
    let len = usize::try_from(count).map_err(|_| "argument count exceeds usize")?;
    if len == 0 {
        return Ok(&[]);
    }
    let ptr = NonNull::new(args.cast_mut()).ok_or("null args pointer with non-zero count")?;
    // SAFETY: pointer validity and length are guaranteed by the JIT caller contract.
    Ok(unsafe { std::slice::from_raw_parts(ptr.as_ptr(), len) })
}

fn native_id(id: u64) -> Result<u16, &'static str> {
    u16::try_from(id).map_err(|_| "native id exceeds u16 range")
}

/// Call a native function by ID (borrowing args).
///
/// # Safety
///
/// - `registry` must point to a valid, pinned `NativeRegistry`
/// - `args` must point to `count` valid NValue-encoded u64s
/// - The caller retains ownership of the args; this function borrows them
/// - Returns an owned NValue (caller must eventually drop it)
pub(super) extern "C" fn jit_call_native(
    registry: *const NativeRegistry,
    id: u64,
    args: *const u64,
    count: u64,
) -> u64 {
    let registry = match registry_ref(registry) {
        Ok(r) => r,
        Err(msg) => {
            jit_set_error(format!("Native function error: {}", msg));
            return NV_UNIT;
        }
    };
    let arg_slice = match args_bits(args, count) {
        Ok(s) => s,
        Err(msg) => {
            jit_set_error(format!("Native function error: {}", msg));
            return NV_UNIT;
        }
    };
    let id = match native_id(id) {
        Ok(id) => id,
        Err(msg) => {
            jit_set_error(format!("Native function error: {}", msg));
            return NV_UNIT;
        }
    };
    // Use stack buffer for small arg counts (most native calls have 1-3 args)
    let len = arg_slice.len();
    let result = if len <= 4 {
        let mut buf: [std::mem::MaybeUninit<NValue>; 4] =
            [const { std::mem::MaybeUninit::uninit() }; 4];
        for (i, &bits) in arg_slice.iter().enumerate() {
            buf[i].write(unsafe { NValue::borrow_from_raw(bits) });
        }
        // SAFETY: first `len` elements are initialized above
        let nvalues = unsafe { std::slice::from_raw_parts(buf.as_ptr() as *const NValue, len) };
        registry.call(id, nvalues)
    } else {
        let nvalues: Vec<NValue> = arg_slice
            .iter()
            .map(|&bits| unsafe { NValue::borrow_from_raw(bits) })
            .collect();
        registry.call(id, &nvalues)
    };
    match result {
        Ok(r) => jit_own(r),
        Err(e) => {
            jit_set_error(format!("Native function error: {}", e.0));
            NV_UNIT
        }
    }
}

/// Call a native function by ID, transferring ownership of args (CoW dispatch).
///
/// # Safety
///
/// - `registry` must point to a valid, pinned `NativeRegistry`
/// - `args` must point to `count` valid NValue-encoded u64s
/// - This function TAKES OWNERSHIP of each arg (consumes one refcount per arg)
/// - The caller must NOT decref/drop the args after this call
/// - Returns an owned NValue (caller must eventually drop it)
pub(super) extern "C" fn jit_call_native_owning(
    registry: *const NativeRegistry,
    id: u64,
    args: *const u64,
    count: u64,
) -> u64 {
    let registry = match registry_ref(registry) {
        Ok(r) => r,
        Err(msg) => {
            jit_set_error(format!("Native function error: {}", msg));
            return NV_UNIT;
        }
    };
    let arg_slice = match args_bits(args, count) {
        Ok(s) => s,
        Err(msg) => {
            jit_set_error(format!("Native function error: {}", msg));
            return NV_UNIT;
        }
    };
    let id = match native_id(id) {
        Ok(id) => id,
        Err(msg) => {
            jit_set_error(format!("Native function error: {}", msg));
            return NV_UNIT;
        }
    };
    // Pre-allocate Vec with known capacity to avoid growth reallocs
    let mut nvalues = Vec::with_capacity(arg_slice.len());
    nvalues.extend(
        arg_slice
            .iter()
            .map(|&bits| unsafe { NValue::from_raw(bits) }),
    );
    match registry.call_owning(id, nvalues) {
        Ok(result) => jit_own(result),
        Err(e) => {
            jit_set_error(format!("Native function error: {}", e.0));
            NV_UNIT
        }
    }
}
