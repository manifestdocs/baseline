use std::sync::Arc;

use super::super::nvalue::{TAG_HEAP, TAG_MASK};
use super::{HeapObject, NValue, NativeError};

/// Weak.downgrade(value) -> Weak<T>
/// Creates a weak reference from a heap-allocated value.
pub(super) fn native_weak_downgrade(args: &[NValue]) -> Result<NValue, NativeError> {
    let value = &args[0];
    let arc = value.as_arc().ok_or_else(|| {
        NativeError("Weak.downgrade: expected a heap-allocated value (not Int, Bool, etc.)".into())
    })?;
    let weak = Arc::downgrade(&arc);
    Ok(NValue::weak_ref(weak))
}

/// Weak.upgrade(weak) -> Option<T>
/// Attempts to upgrade a weak reference. Returns Some(value) if the referent
/// is still alive, None otherwise.
pub(super) fn native_weak_upgrade(args: &[NValue]) -> Result<NValue, NativeError> {
    let weak_val = &args[0];
    match weak_val.as_heap_ref() {
        HeapObject::WeakRef(weak) => {
            match weak.upgrade() {
                Some(arc) => {
                    // Reconstruct an NValue from the Arc without double-counting allocs.
                    // Arc::into_raw transfers ownership of one strong count into the NValue.
                    let ptr = Arc::into_raw(arc) as u64;
                    debug_assert!(ptr & TAG_MASK == 0);
                    let inner = unsafe { NValue::from_raw(TAG_HEAP | ptr) };
                    Ok(NValue::enum_val("Some".into(), inner))
                }
                None => Ok(NValue::enum_val("None".into(), NValue::unit())),
            }
        }
        _ => Err(NativeError(
            "Weak.upgrade: expected a Weak reference".into(),
        )),
    }
}
