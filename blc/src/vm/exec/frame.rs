// ---------------------------------------------------------------------------
// Call Frame
// ---------------------------------------------------------------------------

use super::MAX_CALL_DEPTH;

// ---------------------------------------------------------------------------
// Frame flags
// ---------------------------------------------------------------------------

/// Bit flag in `PackedBase` indicating the frame was created by `Call` (has a
/// function value slot below args). Clear for `CallDirect` frames.
pub(crate) const FRAME_HAS_FUNC: u32 = 0x8000_0000;

/// Bit flag in `PackedBase` indicating the frame acts as the boundary for a
/// resumed continuation. When this frame pops (or hits PopHandler), it returns
/// control to the handler function that invoked `resume()`.
pub(crate) const FRAME_IS_CONT: u32 = 0x4000_0000;

/// Combined mask for all frame flags — bits 30 and 31.
pub(crate) const FRAME_FLAGS_MASK: u32 = FRAME_HAS_FUNC | FRAME_IS_CONT;

// ---------------------------------------------------------------------------
// PackedBase — type-safe base_slot with flag accessors
// ---------------------------------------------------------------------------

/// A `u32` that encodes both the stack slot index (bits 0–29) and frame flags
/// (bits 30–31). Using a newtype makes it impossible to forget the flag mask
/// when reading the slot index, and impossible to accidentally apply a raw `|`
/// without going through the builder methods.
///
/// # Memory layout
/// Same as `u32` (`#[repr(transparent)]`), so it is safe to store in
/// `Box<[CallFrame; N]>` and read back via `get_unchecked`.
#[derive(Copy, Clone, Default, Debug)]
#[repr(transparent)]
pub(crate) struct PackedBase(pub(crate) u32);

impl PackedBase {
    /// Create from a raw slot index with no flags set.
    #[inline(always)]
    pub(crate) fn from_slot(slot: usize) -> Self {
        PackedBase(slot as u32)
    }

    /// The actual stack slot index (all flag bits stripped).
    #[inline(always)]
    pub(crate) fn slot(self) -> usize {
        (self.0 & !FRAME_FLAGS_MASK) as usize
    }

    /// True if a function value sits below the args on the stack (`Call` frame).
    #[inline(always)]
    pub(crate) fn has_func(self) -> bool {
        self.0 & FRAME_HAS_FUNC != 0
    }

    /// True if this frame is the bottom-most frame of a resumed continuation.
    #[inline(always)]
    pub(crate) fn is_cont(self) -> bool {
        self.0 & FRAME_IS_CONT != 0
    }

    /// Return `self` with `FRAME_HAS_FUNC` set.
    #[inline(always)]
    pub(crate) fn with_has_func(self) -> Self {
        PackedBase(self.0 | FRAME_HAS_FUNC)
    }

    /// Return `self` with `FRAME_IS_CONT` set.
    #[inline(always)]
    pub(crate) fn with_is_cont(self) -> Self {
        PackedBase(self.0 | FRAME_IS_CONT)
    }

    /// Raw `u32` for serialization into continuation frame segments.
    #[inline(always)]
    pub(crate) fn raw(self) -> u32 {
        self.0
    }
}

// ---------------------------------------------------------------------------
// Call Frame
// ---------------------------------------------------------------------------

/// Compact call frame — 16 bytes.
/// Upvalues are stored in a separate side-stack (`Vm::upvalue_stack`)
/// indexed by `upvalue_idx`. `u32::MAX` means no upvalues (plain function).
///
/// `base_slot` packs the stack slot index and frame flags into one `u32`.
/// Use `PackedBase` methods to read the slot index or check flags.
#[derive(Copy, Clone)]
pub(crate) struct CallFrame {
    pub(crate) chunk_idx: u32,
    pub(crate) ip: u32,
    pub(crate) base_slot: PackedBase,
    pub(crate) upvalue_idx: u32,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            chunk_idx: 0,
            ip: 0,
            base_slot: PackedBase(0),
            upvalue_idx: u32::MAX,
        }
    }
}

// ---------------------------------------------------------------------------
// Frame Stack
// ---------------------------------------------------------------------------

/// Fixed-capacity frame stack. Avoids `Vec` capacity checks on every push/pop.
/// 16 KB total (1024 × 16 bytes), allocated once.
pub(crate) struct FrameStack {
    pub(crate) frames: Box<[CallFrame; MAX_CALL_DEPTH]>,
    pub(crate) len: usize,
}

impl FrameStack {
    pub(crate) fn new() -> Self {
        // SAFETY: CallFrame is Copy and all-zeros is a valid (default-like) state.
        // We only ever read indices < self.len, so uninit values are never observed.
        let frames = unsafe {
            let layout = std::alloc::Layout::new::<[CallFrame; MAX_CALL_DEPTH]>();
            let ptr = std::alloc::alloc_zeroed(layout);
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }
            Box::from_raw(ptr as *mut [CallFrame; MAX_CALL_DEPTH])
        };
        Self { frames, len: 0 }
    }

    #[inline(always)]
    pub(crate) fn push(&mut self, frame: CallFrame) {
        // SAFETY: Caller must ensure len < MAX_CALL_DEPTH (checked in Call/CallDirect).
        debug_assert!(self.len < MAX_CALL_DEPTH);
        unsafe {
            *self.frames.get_unchecked_mut(self.len) = frame;
        }
        self.len += 1;
    }

    #[inline(always)]
    pub(crate) fn pop(&mut self) -> CallFrame {
        debug_assert!(self.len > 0);
        self.len -= 1;
        // SAFETY: We just decremented len; the frame at that index was previously written.
        unsafe { *self.frames.get_unchecked(self.len) }
    }

    #[inline(always)]
    pub(crate) fn last(&self) -> &CallFrame {
        debug_assert!(self.len > 0);
        // SAFETY: len > 0, so len - 1 is in bounds.
        unsafe { self.frames.get_unchecked(self.len - 1) }
    }

    #[inline(always)]
    pub(crate) fn last_mut(&mut self) -> &mut CallFrame {
        debug_assert!(self.len > 0);
        let idx = self.len - 1;
        // SAFETY: len > 0, so idx is in bounds.
        unsafe { self.frames.get_unchecked_mut(idx) }
    }

    #[inline(always)]
    pub(crate) fn len(&self) -> usize {
        self.len
    }

    pub(crate) fn clear(&mut self) {
        self.len = 0;
    }
}
