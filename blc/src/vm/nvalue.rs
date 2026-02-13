//! NaN-boxed value type for the VM stack.
//!
//! Encodes common value types (Int, Float, Bool, Unit, Function) inline in 8 bytes,
//! with heap-allocated variants (String, List, Record, etc.) behind an Arc pointer.
//!
//! Encoding scheme (top 16 bits determine type):
//!   Float:    any f64 whose top 16 bits < 0xFFFA (includes normal NaN = 0x7FF8...)
//!   Int:      0xFFFA_PPPP_PPPP_PPPP  (P = 48-bit signed integer, sign-extended)
//!   Bool:     0xFFFB_0000_0000_000V  (V = 0 or 1)
//!   Unit:     0xFFFC_0000_0000_0000
//!   Function: 0xFFFD_PPPP_PPPP_PPPP  (P = chunk index)
//!   Heap:     0xFFFE_PPPP_PPPP_PPPP  (P = pointer to Arc<HeapObject> inner data)

use std::fmt;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use super::value::{RcStr, Value};

// ---------------------------------------------------------------------------
// Tag constants
// ---------------------------------------------------------------------------

pub const TAG_MASK: u64 = 0xFFFF_0000_0000_0000;
pub const PAYLOAD_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;
pub const TAG_THRESHOLD: u64 = 0xFFFA_0000_0000_0000; // values >= this are tagged

pub const TAG_INT: u64 = 0xFFFA_0000_0000_0000;
pub const TAG_BOOL: u64 = 0xFFFB_0000_0000_0000;
pub const TAG_UNIT: u64 = 0xFFFC_0000_0000_0000;
pub const TAG_FUNC: u64 = 0xFFFD_0000_0000_0000;
pub const TAG_HEAP: u64 = 0xFFFE_0000_0000_0000;

// ---------------------------------------------------------------------------
// Allocation Statistics — tracks heap object lifecycle
// ---------------------------------------------------------------------------

static ALLOC_STATS: AllocStats = AllocStats::new();

/// Global allocation statistics for heap-allocated NValues.
///
/// Counts actual heap object allocations (Arc::new) and deallocations
/// (Arc drop when strong_count == 1). Refcount bumps from Clone are
/// not counted, so `live = allocs - frees` gives the true number of
/// heap objects that have not been freed.
pub struct AllocStats {
    allocs: AtomicU64,
    frees: AtomicU64,
}

impl AllocStats {
    const fn new() -> Self {
        Self {
            allocs: AtomicU64::new(0),
            frees: AtomicU64::new(0),
        }
    }
}

/// Snapshot of heap allocation statistics.
#[derive(Debug, Clone, Copy)]
pub struct AllocSnapshot {
    pub allocs: u64,
    pub frees: u64,
    pub live: u64,
}

impl fmt::Display for AllocSnapshot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "heap allocs: {}  frees: {}  live: {}",
            self.allocs, self.frees, self.live
        )?;
        if self.live == 0 {
            write!(f, "  (no leaks detected)")
        } else {
            write!(f, "  \u{26A0} potential leak")
        }
    }
}

/// Returns a snapshot of current heap allocation statistics.
pub fn alloc_stats() -> AllocSnapshot {
    let allocs = ALLOC_STATS.allocs.load(Ordering::Relaxed);
    let frees = ALLOC_STATS.frees.load(Ordering::Relaxed);
    AllocSnapshot {
        allocs,
        frees,
        live: allocs.saturating_sub(frees),
    }
}

/// Reset allocation counters to zero. Useful for test isolation.
pub fn reset_alloc_stats() {
    ALLOC_STATS.allocs.store(0, Ordering::Relaxed);
    ALLOC_STATS.frees.store(0, Ordering::Relaxed);
}

// ---------------------------------------------------------------------------
// HeapObject — heap-allocated value variants
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum HeapObject {
    String(RcStr),
    List(Vec<NValue>),
    Record(Vec<(RcStr, NValue)>),
    Tuple(Vec<NValue>),
    Enum {
        tag: RcStr,
        tag_id: u32,
        payload: NValue,
    },
    Struct {
        name: RcStr,
        fields: Vec<(RcStr, NValue)>,
    },
    Closure {
        chunk_idx: usize,
        upvalues: Vec<NValue>,
    },
    /// Middleware chain continuation: handler + remaining middleware.
    /// When called with (request), invokes the remaining middleware chain.
    NativeMwNext {
        handler: NValue,
        remaining_mw: Vec<NValue>,
    },
    /// Map: association list of (key, value) pairs.
    Map(Vec<(NValue, NValue)>),
    /// Set: unique elements.
    Set(Vec<NValue>),
    /// Weak reference to a heap object (does not prevent deallocation).
    WeakRef(Box<std::sync::Weak<HeapObject>>),
    /// Integers that don't fit in 48-bit signed range.
    BigInt(i64),
    /// One-shot delimited continuation captured at an effect perform site.
    /// Stores the stack/frame/handler segment between handler boundary and perform.
    /// One-shot semantics enforced at the VM level (continuation is consumed on call).
    Continuation {
        stack_segment: Vec<NValue>,
        /// Frames stored as (chunk_idx, ip, base_slot, upvalue_idx) tuples
        /// because CallFrame is private to vm.rs.
        frame_segment: Vec<(u32, u32, u32, u32)>,
        upvalue_segment: Vec<Vec<NValue>>,
        handler_stack_depth: usize,
        /// Handler stack entries captured between boundary and perform site.
        handler_stack_segment: Vec<std::collections::HashMap<String, NValue>>,
        /// Handler boundaries captured between boundary index and current.
        /// Stored as (stack_depth, frame_depth, upvalue_depth, handler_stack_idx, return_ip).
        handler_boundary_segment: Vec<(usize, usize, usize, usize, usize)>,
        /// IP to resume execution at (past PerformEffect in the body).
        resume_ip: u32,
        /// Chunk index for the resume point.
        resume_chunk_idx: u32,
    },
}

impl PartialEq for HeapObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HeapObject::String(a), HeapObject::String(b)) => a == b,
            (HeapObject::List(a), HeapObject::List(b)) => a == b,
            (HeapObject::Record(a), HeapObject::Record(b)) => a == b,
            (HeapObject::Tuple(a), HeapObject::Tuple(b)) => a == b,
            // Compare tag + payload only — tag_id is an optimization hint
            (HeapObject::Enum { tag: t1, payload: p1, .. },
             HeapObject::Enum { tag: t2, payload: p2, .. }) => t1 == t2 && p1 == p2,
            (HeapObject::Struct { name: n1, fields: f1 },
             HeapObject::Struct { name: n2, fields: f2 }) => n1 == n2 && f1 == f2,
            (HeapObject::Closure { chunk_idx: c1, upvalues: u1 },
             HeapObject::Closure { chunk_idx: c2, upvalues: u2 }) => c1 == c2 && u1 == u2,
            (HeapObject::Map(a), HeapObject::Map(b)) => a == b,
            (HeapObject::Set(a), HeapObject::Set(b)) => a == b,
            (HeapObject::WeakRef(a), HeapObject::WeakRef(b)) => std::sync::Weak::ptr_eq(a, b),
            (HeapObject::BigInt(a), HeapObject::BigInt(b)) => a == b,
            _ => false,
        }
    }
}

// ---------------------------------------------------------------------------
// NValue — 8-byte NaN-boxed value
// ---------------------------------------------------------------------------

#[repr(transparent)]
pub struct NValue(u64);

// -- Raw access (for JIT interop) --

impl NValue {
    /// Get the raw u64 bits of this NValue (for JIT).
    #[inline(always)]
    pub fn raw(&self) -> u64 {
        self.0
    }

    /// Reconstruct an NValue from raw u64 bits (for JIT).
    ///
    /// # Safety
    ///
    /// The caller must ensure ALL of the following invariants:
    ///
    /// 1. **Valid Encoding**: `bits` must represent a valid NValue encoding:
    ///    - If `bits < TAG_THRESHOLD`: must be a valid f64 bit pattern
    ///    - If `bits & TAG_MASK == TAG_INT`: payload must be sign-extended 48-bit int
    ///    - If `bits & TAG_MASK == TAG_BOOL`: payload must be 0 or 1
    ///    - If `bits & TAG_MASK == TAG_FUNC`: payload must be valid chunk index
    ///    - If `bits & TAG_MASK == TAG_HEAP`: see heap invariants below
    ///
    /// 2. **Heap Pointer Validity** (when `bits & TAG_MASK == TAG_HEAP`):
    ///    - `bits & PAYLOAD_MASK` must be a valid pointer to an `Arc<HeapObject>`
    ///    - The pointer must have been obtained from `Arc::into_raw()`
    ///    - The `Arc` must still be live (refcount > 0)
    ///
    /// 3. **Ownership Transfer**: This function takes ownership of one refcount.
    ///    The caller must NOT drop the original raw bits unless they first
    ///    increment the refcount (e.g., via `borrow_from_raw`).
    ///
    /// # Panics
    ///
    /// Debug builds assert that heap pointers are 48-bit aligned.
    #[inline(always)]
    pub unsafe fn from_raw(bits: u64) -> Self {
        debug_assert!(
            bits & TAG_MASK != TAG_HEAP || (bits & PAYLOAD_MASK) & 0x7 == 0,
            "heap pointer must be 8-byte aligned"
        );
        NValue(bits)
    }

    /// Borrow an NValue from raw bits without taking ownership.
    /// Creates a clone (bumping Arc refcount for heap values) and forgets the
    /// temporary, so the caller's raw bits remain valid.
    ///
    /// This is the safe way to read an NValue from JIT-generated code without
    /// invalidating the original storage.
    ///
    /// # Safety
    ///
    /// Same invariants as `from_raw`:
    ///
    /// 1. `bits` must represent a valid NValue encoding
    /// 2. For heap values, the underlying `Arc<HeapObject>` must be live
    /// 3. The caller retains ownership of the original bits (no refcount transfer)
    ///
    /// # Example
    ///
    /// ```ignore
    /// // In JIT callback: read argument without consuming it
    /// let arg = unsafe { NValue::borrow_from_raw(arg_bits) };
    /// // arg_bits remains valid; arg is a new clone
    /// ```
    #[inline(always)]
    pub unsafe fn borrow_from_raw(bits: u64) -> Self {
        // SAFETY: Caller guarantees bits is valid. We create a temporary NValue,
        // clone it (incrementing refcount for heap), then forget the temp so we
        // don't decrement the refcount. The clone is the return value.
        let temp = unsafe { NValue::from_raw(bits) };
        let cloned = temp.clone();
        std::mem::forget(temp);
        cloned
    }
}

// -- Construction --

impl NValue {
    #[inline(always)]
    pub fn int(i: i64) -> Self {
        // 48-bit signed range: -(1<<47) to (1<<47)-1
        const MIN_INLINE: i64 = -(1i64 << 47);
        const MAX_INLINE: i64 = (1i64 << 47) - 1;
        if (MIN_INLINE..=MAX_INLINE).contains(&i) {
            NValue(TAG_INT | (i as u64 & PAYLOAD_MASK))
        } else {
            Self::from_heap(HeapObject::BigInt(i))
        }
    }

    #[inline(always)]
    pub fn float(f: f64) -> Self {
        let bits = f.to_bits();
        // Canonicalize NaN values that would collide with our tags
        if bits >= TAG_THRESHOLD {
            NValue(f64::NAN.to_bits())
        } else {
            NValue(bits)
        }
    }

    #[inline(always)]
    pub fn bool(b: bool) -> Self {
        NValue(TAG_BOOL | b as u64)
    }

    #[inline(always)]
    pub fn unit() -> Self {
        NValue(TAG_UNIT)
    }

    #[inline(always)]
    pub fn function(chunk_idx: usize) -> Self {
        debug_assert!(chunk_idx as u64 <= PAYLOAD_MASK);
        NValue(TAG_FUNC | chunk_idx as u64)
    }

    pub fn string(s: RcStr) -> Self {
        Self::from_heap(HeapObject::String(s))
    }

    pub fn list(items: Vec<NValue>) -> Self {
        Self::from_heap(HeapObject::List(items))
    }

    pub fn record(fields: Vec<(RcStr, NValue)>) -> Self {
        Self::from_heap(HeapObject::Record(fields))
    }

    pub fn tuple(items: Vec<NValue>) -> Self {
        Self::from_heap(HeapObject::Tuple(items))
    }

    pub fn enum_val(tag: RcStr, payload: NValue) -> Self {
        Self::from_heap(HeapObject::Enum { tag, tag_id: u32::MAX, payload })
    }

    pub fn enum_val_with_id(tag: RcStr, tag_id: u32, payload: NValue) -> Self {
        Self::from_heap(HeapObject::Enum { tag, tag_id, payload })
    }

    pub fn struct_val(name: RcStr, fields: Vec<(RcStr, NValue)>) -> Self {
        Self::from_heap(HeapObject::Struct { name, fields })
    }

    pub fn map(entries: Vec<(NValue, NValue)>) -> Self {
        Self::from_heap(HeapObject::Map(entries))
    }

    pub fn set(elems: Vec<NValue>) -> Self {
        Self::from_heap(HeapObject::Set(elems))
    }

    pub fn weak_ref(w: std::sync::Weak<HeapObject>) -> Self {
        Self::from_heap(HeapObject::WeakRef(Box::new(w)))
    }

    pub fn closure(chunk_idx: usize, upvalues: Vec<NValue>) -> Self {
        Self::from_heap(HeapObject::Closure {
            chunk_idx,
            upvalues,
        })
    }

    pub fn continuation(
        stack_segment: Vec<NValue>,
        frame_segment: Vec<(u32, u32, u32, u32)>,
        upvalue_segment: Vec<Vec<NValue>>,
        handler_stack_depth: usize,
        handler_stack_segment: Vec<std::collections::HashMap<String, NValue>>,
        handler_boundary_segment: Vec<(usize, usize, usize, usize, usize)>,
        resume_ip: u32,
        resume_chunk_idx: u32,
    ) -> Self {
        Self::from_heap(HeapObject::Continuation {
            stack_segment,
            frame_segment,
            upvalue_segment,
            handler_stack_depth,
            handler_stack_segment,
            handler_boundary_segment,
            resume_ip,
            resume_chunk_idx,
        })
    }

    /// Public constructor for HeapObject variants (used by middleware chain builder).
    pub fn from_heap_obj(obj: HeapObject) -> Self {
        Self::from_heap(obj)
    }

    fn from_heap(obj: HeapObject) -> Self {
        let rc = Arc::new(obj);
        let ptr = Arc::into_raw(rc) as u64;
        debug_assert!(ptr & TAG_MASK == 0, "heap pointer exceeds 48 bits");
        ALLOC_STATS.allocs.fetch_add(1, Ordering::Relaxed);
        NValue(TAG_HEAP | ptr)
    }
}

// -- Type checking --

impl NValue {
    #[inline(always)]
    pub fn is_float(&self) -> bool {
        self.0 < TAG_THRESHOLD
    }

    #[inline(always)]
    pub fn is_int(&self) -> bool {
        self.0 & TAG_MASK == TAG_INT
    }

    #[inline(always)]
    pub fn is_bool(&self) -> bool {
        self.0 & TAG_MASK == TAG_BOOL
    }

    #[inline(always)]
    pub fn is_unit(&self) -> bool {
        self.0 == TAG_UNIT
    }

    #[inline(always)]
    pub fn is_function(&self) -> bool {
        self.0 & TAG_MASK == TAG_FUNC
    }

    #[inline(always)]
    pub fn is_heap(&self) -> bool {
        self.0 & TAG_MASK == TAG_HEAP
    }

    /// Check if this is a number (int or float) — used for mixed arithmetic.
    #[inline(always)]
    pub fn is_number(&self) -> bool {
        self.is_int() || self.is_float()
    }

    #[inline(always)]
    pub fn is_truthy(&self) -> bool {
        if self.is_bool() {
            self.0 & 1 != 0
        } else if self.is_int() {
            self.0 & PAYLOAD_MASK != 0
        } else if self.is_unit() {
            false
        } else {
            // Float, heap objects, functions are truthy
            true
        }
    }

    /// Returns true if this is a BigInt (heap-allocated large integer).
    #[inline(always)]
    fn is_bigint(&self) -> bool {
        if !self.is_heap() {
            return false;
        }
        matches!(self.as_heap_ref(), HeapObject::BigInt(_))
    }

    /// Returns true if this is any kind of integer (inline or BigInt).
    #[inline(always)]
    pub fn is_any_int(&self) -> bool {
        self.is_int() || self.is_bigint()
    }
}

// -- Accessors --

impl NValue {
    /// Extract i64 from an inline Int. Caller must ensure is_int().
    #[inline(always)]
    pub fn as_int(&self) -> i64 {
        debug_assert!(self.is_int());
        // Sign-extend from 48 bits: shift left 16, arithmetic shift right 16
        ((self.0 << 16) as i64) >> 16
    }

    /// Extract i64 from any integer (inline or BigInt).
    #[inline(always)]
    pub fn as_any_int(&self) -> i64 {
        if self.is_int() {
            self.as_int()
        } else {
            match self.as_heap_ref() {
                HeapObject::BigInt(i) => *i,
                _ => panic!("as_any_int on non-integer"),
            }
        }
    }

    #[inline(always)]
    pub fn as_float(&self) -> f64 {
        debug_assert!(self.is_float());
        f64::from_bits(self.0)
    }

    /// Get f64 from either Float or Int (for mixed arithmetic).
    #[inline(always)]
    pub fn as_f64(&self) -> f64 {
        if self.is_float() {
            self.as_float()
        } else {
            self.as_any_int() as f64
        }
    }

    #[inline(always)]
    pub fn as_bool(&self) -> bool {
        debug_assert!(self.is_bool());
        self.0 & 1 != 0
    }

    #[inline(always)]
    pub fn as_function(&self) -> usize {
        debug_assert!(self.is_function());
        (self.0 & PAYLOAD_MASK) as usize
    }

    /// Get a reference to the underlying heap object.
    ///
    /// # Safety
    ///
    /// This method contains an unsafe block that dereferences a raw pointer.
    /// The safety is guaranteed by the NValue invariant: if `is_heap()` returns
    /// true, then `self.0 & PAYLOAD_MASK` is a valid pointer to a live
    /// `Arc<HeapObject>`. The debug_assert verifies this precondition.
    ///
    /// # Panics
    ///
    /// Debug builds panic if called on a non-heap value.
    #[inline(always)]
    pub fn as_heap_ref(&self) -> &HeapObject {
        debug_assert!(self.is_heap(), "as_heap_ref called on non-heap value");
        let ptr = (self.0 & PAYLOAD_MASK) as *const HeapObject;
        // SAFETY: The NValue invariant guarantees that if is_heap() is true,
        // the payload contains a valid pointer to a live Arc<HeapObject>.
        // The Arc's lifetime is tied to the NValue's lifetime via Clone/Drop.
        unsafe { &*ptr }
    }

    /// Safely get heap reference, returning None if not a heap value.
    #[inline]
    pub fn as_heap_ref_checked(&self) -> Option<&HeapObject> {
        if self.is_heap() {
            Some(self.as_heap_ref())
        } else {
            None
        }
    }

    /// Extract string reference from a heap String.
    #[inline]
    pub fn as_string(&self) -> Option<&RcStr> {
        if !self.is_heap() {
            return None;
        }
        match self.as_heap_ref() {
            HeapObject::String(s) => Some(s),
            _ => None,
        }
    }

    /// Extract list reference from a heap List.
    #[inline]
    pub fn as_list(&self) -> Option<&Vec<NValue>> {
        if !self.is_heap() {
            return None;
        }
        match self.as_heap_ref() {
            HeapObject::List(items) => Some(items),
            _ => None,
        }
    }

    /// Extract record fields reference from a heap Record.
    #[inline]
    pub fn as_record(&self) -> Option<&Vec<(RcStr, NValue)>> {
        if !self.is_heap() {
            return None;
        }
        match self.as_heap_ref() {
            HeapObject::Record(fields) => Some(fields),
            _ => None,
        }
    }

    /// Extract enum tag and payload from a heap Enum.
    #[inline]
    pub fn as_enum(&self) -> Option<(&RcStr, &NValue)> {
        if !self.is_heap() {
            return None;
        }
        match self.as_heap_ref() {
            HeapObject::Enum { tag, payload, .. } => Some((tag, payload)),
            _ => None,
        }
    }

    /// Extract string slice from a heap String.
    #[inline]
    pub fn as_str(&self) -> Option<&str> {
        self.as_string().map(|s| s.as_ref())
    }

    /// Check if this is a continuation value.
    #[inline]
    pub fn is_continuation(&self) -> bool {
        if !self.is_heap() {
            return false;
        }
        matches!(self.as_heap_ref(), HeapObject::Continuation { .. })
    }

    /// Reconstruct the Arc<HeapObject> from a heap NValue (increments strong count).
    /// Returns None for non-heap values.
    #[inline]
    pub fn as_arc(&self) -> Option<Arc<HeapObject>> {
        if !self.is_heap() {
            return None;
        }
        let ptr = (self.0 & PAYLOAD_MASK) as *const HeapObject;
        // SAFETY: is_heap() guarantees ptr is a valid Arc pointer.
        // We increment the strong count then reconstruct, so the caller
        // gets a new owned Arc without invalidating ours.
        unsafe {
            Arc::increment_strong_count(ptr);
            Some(Arc::from_raw(ptr))
        }
    }

    /// Extract tuple items reference from a heap Tuple.
    #[inline]
    pub fn as_tuple(&self) -> Option<&Vec<NValue>> {
        if !self.is_heap() {
            return None;
        }
        match self.as_heap_ref() {
            HeapObject::Tuple(items) => Some(items),
            _ => None,
        }
    }
}

// -- Clone: bump Arc for heap, copy bits for inline --

impl Clone for NValue {
    /// Clone an NValue.
    ///
    /// For inline values (int, float, bool, unit, function index), this is a
    /// simple bit copy. For heap values, we increment the Arc strong count.
    ///
    /// # Safety
    ///
    /// The unsafe block is sound because:
    /// 1. We only call `increment_strong_count` when `is_heap()` is true
    /// 2. The NValue invariant guarantees the pointer is a valid `Arc<HeapObject>`
    /// 3. The Arc is still live (this NValue holds a reference)
    #[inline(always)]
    fn clone(&self) -> Self {
        if self.is_heap() {
            let ptr = (self.0 & PAYLOAD_MASK) as *const HeapObject;
            // SAFETY: is_heap() guarantees ptr is a valid Arc pointer.
            // We're incrementing the count, not decrementing, so no risk of
            // use-after-free. The Arc will be decremented when the clone is dropped.
            unsafe {
                Arc::increment_strong_count(ptr);
            }
        }
        NValue(self.0)
    }
}

// -- Drop: decrement Arc for heap --

impl Drop for NValue {
    /// Drop an NValue.
    ///
    /// For inline values, this is a no-op. For heap values, we decrement the
    /// Arc strong count, potentially freeing the HeapObject.
    ///
    /// # Safety
    ///
    /// The unsafe block is sound because:
    /// 1. We only call `decrement_strong_count` when `is_heap()` is true
    /// 2. The NValue invariant guarantees the pointer is a valid `Arc<HeapObject>`
    /// 3. Each NValue owns exactly one strong count (incremented in clone/from_heap)
    /// 4. We never call decrement twice for the same NValue
    #[inline(always)]
    fn drop(&mut self) {
        if self.is_heap() {
            let ptr = (self.0 & PAYLOAD_MASK) as *const HeapObject;
            // Check if this drop will actually deallocate the object.
            // SAFETY: We reconstruct the Arc only to read strong_count,
            // then forget it so we don't double-drop.
            let is_last = unsafe {
                let arc = Arc::from_raw(ptr);
                let count = Arc::strong_count(&arc);
                std::mem::forget(arc);
                count == 1
            };
            if is_last {
                ALLOC_STATS.frees.fetch_add(1, Ordering::Relaxed);
            }
            // SAFETY: is_heap() guarantees ptr is a valid Arc pointer.
            // This NValue owns exactly one strong count, which we're releasing.
            // If this is the last reference, the HeapObject will be deallocated.
            unsafe {
                Arc::decrement_strong_count(ptr);
            }
        }
    }
}

// -- PartialEq --

impl PartialEq for NValue {
    fn eq(&self, other: &Self) -> bool {
        // Fast path: identical bits (works for Int, Bool, Unit, Function)
        if self.0 == other.0 {
            // But NaN != NaN per IEEE 754
            if self.is_float() {
                return !f64::from_bits(self.0).is_nan();
            }
            return true;
        }
        // Different bits — could still be equal for floats or heap objects
        if self.is_float() && other.is_float() {
            return self.as_float() == other.as_float();
        }
        if self.is_heap() && other.is_heap() {
            return self.as_heap_ref() == other.as_heap_ref();
        }
        false
    }
}

// -- Display --

impl fmt::Display for NValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_int() {
            write!(f, "{}", self.as_int())
        } else if self.is_float() {
            write!(f, "{}", self.as_float())
        } else if self.is_bool() {
            write!(f, "{}", self.as_bool())
        } else if self.is_unit() {
            write!(f, "()")
        } else if self.is_function() {
            write!(f, "<fn:{}>", self.as_function())
        } else {
            match self.as_heap_ref() {
                HeapObject::String(s) => write!(f, "{}", s),
                HeapObject::List(items) => {
                    let s: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                    write!(f, "[{}]", s.join(", "))
                }
                HeapObject::Record(fields) => {
                    let s: Vec<String> = fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .collect();
                    write!(f, "{{ {} }}", s.join(", "))
                }
                HeapObject::Tuple(items) => {
                    let s: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                    write!(f, "({})", s.join(", "))
                }
                HeapObject::Enum { tag, payload, .. } => {
                    if payload.is_unit() {
                        write!(f, "{}", tag)
                    } else {
                        write!(f, "{}({})", tag, payload)
                    }
                }
                HeapObject::Struct { name, fields } => {
                    let s: Vec<String> = fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .collect();
                    write!(f, "{} {{ {} }}", name, s.join(", "))
                }
                HeapObject::Closure { chunk_idx, .. } => {
                    write!(f, "<closure:{}>", chunk_idx)
                }
                HeapObject::NativeMwNext { .. } => {
                    write!(f, "<middleware-next>")
                }
                HeapObject::Map(entries) => {
                    let s: Vec<String> = entries
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .collect();
                    write!(f, "#{{{}}}", s.join(", "))
                }
                HeapObject::Set(elems) => {
                    let s: Vec<String> = elems.iter().map(|v| v.to_string()).collect();
                    write!(f, "Set({})", s.join(", "))
                }
                HeapObject::WeakRef(_) => write!(f, "<weak>"),
                HeapObject::BigInt(i) => write!(f, "{}", i),
                HeapObject::Continuation { .. } => write!(f, "<continuation>"),
            }
        }
    }
}

impl fmt::Debug for NValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NValue({})", self)
    }
}

// -- Conversion to/from Value --

impl NValue {
    pub fn to_value(&self) -> Value {
        if self.is_int() {
            return Value::Int(self.as_int());
        }
        if self.is_float() {
            return Value::Float(self.as_float());
        }
        if self.is_bool() {
            return Value::Bool(self.as_bool());
        }
        if self.is_unit() {
            return Value::Unit;
        }
        if self.is_function() {
            return Value::Function(self.as_function());
        }
        match self.as_heap_ref() {
            HeapObject::String(s) => Value::String(s.clone()),
            HeapObject::List(items) => {
                Value::List(Arc::new(items.iter().map(|v| v.to_value()).collect()))
            }
            HeapObject::Record(fields) => Value::Record(Arc::new(
                fields
                    .iter()
                    .map(|(k, v)| (k.clone(), v.to_value()))
                    .collect(),
            )),
            HeapObject::Tuple(items) => {
                Value::Tuple(Arc::new(items.iter().map(|v| v.to_value()).collect()))
            }
            HeapObject::Enum { tag, payload, .. } => {
                Value::Enum(tag.clone(), Arc::new(payload.to_value()))
            }
            HeapObject::Struct { name, fields } => Value::Struct(
                name.clone(),
                Arc::new(
                    fields
                        .iter()
                        .map(|(k, v)| (k.clone(), v.to_value()))
                        .collect(),
                ),
            ),
            HeapObject::Closure {
                chunk_idx,
                upvalues,
            } => Value::Closure {
                chunk_idx: *chunk_idx,
                upvalues: Arc::new(upvalues.iter().map(|v| v.to_value()).collect()),
            },
            HeapObject::NativeMwNext { .. } => Value::Unit,
            HeapObject::WeakRef(_) => Value::Unit,
            HeapObject::Continuation { .. } => Value::Unit,
            HeapObject::BigInt(i) => Value::Int(*i),
            HeapObject::Map(entries) => Value::Map(Arc::new(
                entries
                    .iter()
                    .map(|(k, v)| (k.to_value(), v.to_value()))
                    .collect(),
            )),
            HeapObject::Set(elems) => {
                Value::Set(Arc::new(elems.iter().map(|v| v.to_value()).collect()))
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_roundtrip() {
        for i in [
            -1i64,
            0,
            1,
            42,
            -42,
            1000000,
            -1000000,
            (1 << 47) - 1,
            -(1 << 47),
        ] {
            let v = NValue::int(i);
            assert!(v.is_int() || v.is_heap(), "failed for {}", i);
            assert_eq!(v.as_any_int(), i, "roundtrip failed for {}", i);
        }
    }

    #[test]
    fn big_int_roundtrip() {
        let big = i64::MAX;
        let v = NValue::int(big);
        assert!(v.is_heap(), "big int should be heap-allocated");
        assert_eq!(v.as_any_int(), big);
    }

    #[test]
    fn float_roundtrip() {
        for f in [0.0, 1.0, -1.0, 3.125, f64::INFINITY, f64::NEG_INFINITY] {
            let v = NValue::float(f);
            assert!(v.is_float());
            assert_eq!(v.as_float(), f);
        }
    }

    #[test]
    fn nan_is_float() {
        let v = NValue::float(f64::NAN);
        assert!(v.is_float());
        assert!(v.as_float().is_nan());
    }

    #[test]
    fn bool_roundtrip() {
        assert!(NValue::bool(true).as_bool());
        assert!(!NValue::bool(false).as_bool());
        assert!(NValue::bool(true).is_truthy());
        assert!(!NValue::bool(false).is_truthy());
    }

    #[test]
    fn unit() {
        let v = NValue::unit();
        assert!(v.is_unit());
        assert!(!v.is_truthy());
    }

    #[test]
    fn function_roundtrip() {
        let v = NValue::function(42);
        assert!(v.is_function());
        assert_eq!(v.as_function(), 42);
    }

    #[test]
    fn string_heap() {
        let v = NValue::string("hello".into());
        assert!(v.is_heap());
        match v.as_heap_ref() {
            HeapObject::String(s) => assert_eq!(&**s, "hello"),
            _ => panic!("expected String"),
        }
    }

    #[test]
    fn clone_heap_bumps_refcount() {
        let v1 = NValue::string("test".into());
        let v2 = v1.clone();
        // Both should display the same
        assert_eq!(v1.to_string(), "test");
        assert_eq!(v2.to_string(), "test");
        // Dropping one should not affect the other
        drop(v1);
        assert_eq!(v2.to_string(), "test");
    }

    #[test]
    fn equality() {
        assert_eq!(NValue::int(1), NValue::int(1));
        assert_ne!(NValue::int(1), NValue::int(2));
        assert_eq!(NValue::bool(true), NValue::bool(true));
        assert_ne!(NValue::bool(true), NValue::bool(false));
        assert_eq!(NValue::unit(), NValue::unit());
        assert_eq!(NValue::float(1.0), NValue::float(1.0));
        let nan = NValue::float(f64::NAN);
        assert_ne!(nan, nan.clone());
    }

    #[test]
    fn value_conversion_roundtrip() {
        let cases: Vec<(NValue, Value)> = vec![
            (NValue::int(42), Value::Int(42)),
            (NValue::float(3.125), Value::Float(3.125)),
            (NValue::bool(true), Value::Bool(true)),
            (NValue::unit(), Value::Unit),
            (NValue::function(5), Value::Function(5)),
            (NValue::string("hello".into()), Value::String("hello".into())),
        ];
        for (nv, expected) in &cases {
            let back = nv.to_value();
            assert_eq!(&back, expected, "roundtrip failed for {:?}", nv);
        }
    }

    #[test]
    fn size_is_8_bytes() {
        assert_eq!(std::mem::size_of::<NValue>(), 8);
    }

    #[test]
    fn alloc_free_balance() {
        let before = alloc_stats();
        {
            let _s1 = NValue::string("hello".into());
            let _s2 = NValue::string("world".into());
            let _l = NValue::list(vec![NValue::int(1), NValue::int(2)]);
        }
        let after = alloc_stats();
        let new_allocs = after.allocs - before.allocs;
        let new_frees = after.frees - before.frees;
        assert_eq!(new_allocs, new_frees, "allocs should equal frees after all values dropped");
    }

    #[test]
    fn alloc_counting_accuracy() {
        let before = alloc_stats();
        let _s1 = NValue::string("a".into());
        let _s2 = NValue::string("b".into());
        let _s3 = NValue::string("c".into());
        let after = alloc_stats();
        assert_eq!(after.allocs - before.allocs, 3, "3 strings should produce 3 allocs");
    }

    #[test]
    fn clone_does_not_double_count() {
        let before = alloc_stats();
        let s = NValue::string("shared".into());
        let _clone = s.clone();
        let after = alloc_stats();
        assert_eq!(after.allocs - before.allocs, 1, "clone should not increment alloc count");
    }

    #[test]
    fn leak_detection() {
        let before = alloc_stats();
        let leaked = NValue::string("leaked".into());
        std::mem::forget(leaked);
        let after = alloc_stats();
        let new_allocs = after.allocs - before.allocs;
        let new_frees = after.frees - before.frees;
        assert!(new_allocs > new_frees, "forgotten value should not be freed");
    }
}
