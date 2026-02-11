//! Runtime helper functions for JIT-generated code.
//!
//! Most helpers live in `baseline_rt::helpers`. This module re-exports them
//! and adds `jit_call_native` which depends on `NativeRegistry` (blc-only).

use super::super::natives::NativeRegistry;
use super::super::nvalue::NValue;

// Re-export everything from baseline-rt helpers
pub use baseline_rt::helpers::*;

/// Call a native function by ID.
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
    let registry = unsafe { &*registry };
    let arg_slice = unsafe { std::slice::from_raw_parts(args, count as usize) };
    let nvalues: Vec<NValue> = arg_slice
        .iter()
        .map(|&bits| unsafe { NValue::borrow_from_raw(bits) })
        .collect();
    match registry.call(id as u16, &nvalues) {
        Ok(result) => {
            let bits = result.raw();
            jit_arena_push(result);
            bits
        }
        Err(e) => {
            jit_set_error(format!("Native function error: {}", e.0));
            NV_UNIT
        }
    }
}
