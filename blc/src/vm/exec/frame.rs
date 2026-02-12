// ---------------------------------------------------------------------------
// Call Frame
// ---------------------------------------------------------------------------

use super::MAX_CALL_DEPTH;

/// Compact call frame — 16 bytes instead of 32.
/// Upvalues are stored in a separate side-stack (`Vm::upvalue_stack`)
/// indexed by `upvalue_idx`. `u32::MAX` means no upvalues (plain function).
///
/// `base_slot` encoding: bit 31 indicates whether a function value sits below
/// the args on the stack (set for `Call`, clear for `CallDirect`). The actual
/// slot index is `base_slot & 0x7FFF_FFFF`.
#[derive(Copy, Clone)]
pub(crate) struct CallFrame {
    pub(crate) chunk_idx: u32,
    pub(crate) ip: u32,
    pub(crate) base_slot: u32,
    pub(crate) upvalue_idx: u32,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            chunk_idx: 0,
            ip: 0,
            base_slot: 0,
            upvalue_idx: u32::MAX,
        }
    }
}

/// Bit flag in `base_slot` indicating the frame was created by `Call` (has a
/// function value slot below args). Clear for `CallDirect` frames.
pub(crate) const FRAME_HAS_FUNC: u32 = 0x8000_0000;

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
