//! Reference-counting abstraction: Arc (default) or Rc (non-atomic-rc feature).
//!
//! Baseline is single-threaded at runtime, so non-atomic Rc is safe and faster.
//! The `non-atomic-rc` feature flag switches from Arc to Rc, eliminating atomic
//! operations on every clone/drop of heap-allocated NValues.
//!
//! When cross-fiber sharing is needed (structured concurrency), use the default
//! Arc mode which provides Send + Sync.
//!
//! ## Arc Usage Audit (BASEL-336)
//!
//! All NValue/Value heap allocations go through this abstraction. Known exceptions:
//!
//! - `NativeObject` (nvalue.rs) uses `std::sync::Arc` unconditionally — native
//!   objects may outlive the Baseline runtime (e.g., database connections).
//! - JIT helpers (helpers.rs) use thread-local state (JIT_FN_TABLE, JIT_ARENA,
//!   JIT_RC_MODE, JIT_ERROR) — safe for single-threaded execution only.
//!   `JitProgram` is marked `Send` but must not be called concurrently.
//! - The `NativeRegistry` Send+Sync assertion (mod.rs) ensures native functions
//!   are thread-safe for future cross-fiber sharing.
//!
//! When adding new native functions: use `RcStr` (from `value.rs`) for strings,
//! and `crate::rc::Rc` for other heap containers. Never import `std::sync::Arc`
//! directly — it bypasses the feature-flag abstraction.

#[cfg(not(feature = "non-atomic-rc"))]
pub use std::sync::Arc as Rc;

#[cfg(feature = "non-atomic-rc")]
pub use std::rc::Rc;

/// Increment the strong count of a raw pointer (mirrors Arc/Rc API).
///
/// # Safety
/// `ptr` must have been obtained from `Rc::into_raw` and the Rc must still be alive.
#[inline(always)]
pub unsafe fn increment_strong_count<T: ?Sized>(ptr: *const T) {
    #[cfg(not(feature = "non-atomic-rc"))]
    unsafe {
        std::sync::Arc::increment_strong_count(ptr);
    }
    #[cfg(feature = "non-atomic-rc")]
    unsafe {
        std::rc::Rc::increment_strong_count(ptr);
    }
}

/// Decrement the strong count of a raw pointer (mirrors Arc/Rc API).
///
/// # Safety
/// `ptr` must have been obtained from `Rc::into_raw` and the Rc must still be alive.
/// If this was the last strong reference, the value will be dropped and deallocated.
#[inline(always)]
pub unsafe fn decrement_strong_count<T: ?Sized>(ptr: *const T) {
    #[cfg(not(feature = "non-atomic-rc"))]
    unsafe {
        std::sync::Arc::decrement_strong_count(ptr);
    }
    #[cfg(feature = "non-atomic-rc")]
    unsafe {
        std::rc::Rc::decrement_strong_count(ptr);
    }
}
