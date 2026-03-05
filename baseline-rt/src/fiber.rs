//! Minimal stackful coroutines for non-tail-resumptive effect handlers.
//!
//! A `Fiber` runs a function on a separate stack (mmap'd with a guard page).
//! The parent can resume the fiber, passing a value in; the fiber can yield
//! back to the parent with an effect key and arguments.
//!
//! ## Platform support
//!
//! Currently aarch64 only. The context switch is ~30 lines of inline assembly
//! saving/restoring callee-saved registers + SP.

use std::cell::Cell;

// ---------------------------------------------------------------------------
// Fiber state machine
// ---------------------------------------------------------------------------

/// The current state of a fiber.
#[derive(Debug)]
pub enum FiberState {
    /// Created but not yet started.
    Ready,
    /// Yielded an effect to the parent. Contains (effect_key, args).
    Yielded {
        key: String,
        args: Vec<u64>,
    },
    /// Fiber ran to completion with a return value.
    Completed(u64),
    /// Fiber was destroyed without completing (abort handler).
    Aborted,
}

// ---------------------------------------------------------------------------
// Context (callee-saved registers)
// ---------------------------------------------------------------------------

/// Saved CPU context for context switching. aarch64 callee-saved registers:
/// sp, x19-x28, x29 (fp), x30 (lr), d8-d15.
#[repr(C)]
struct FiberContext {
    regs: [u64; 22],
}

impl FiberContext {
    const fn zeroed() -> Self {
        FiberContext { regs: [0; 22] }
    }
}

// Register layout indices (aarch64):
// [0]  = sp
// [1]  = x19
// [2]  = x20
// [3]  = x21
// [4]  = x22
// [5]  = x23
// [6]  = x24
// [7]  = x25
// [8]  = x26
// [9]  = x27
// [10] = x28
// [11] = x29 (fp)
// [12] = x30 (lr)
// [13..20] = d8-d15 (stored as u64 via fmov)

// ---------------------------------------------------------------------------
// Fiber struct
// ---------------------------------------------------------------------------

/// A stackful coroutine.
pub struct Fiber {
    /// Saved context for this fiber (registers at yield/suspend point).
    fiber_ctx: FiberContext,
    /// Saved context for the parent (registers when we switch away).
    parent_ctx: FiberContext,
    /// mmap'd stack base (lowest address; guard page is at the bottom).
    stack: *mut u8,
    /// Total stack allocation size including guard page.
    stack_size: usize,
    /// Current state.
    pub state: FiberState,
    /// Slot for passing a value from parent → fiber on resume.
    transfer: u64,
    /// The entry function pointer and argument, consumed on first resume.
    entry: Option<(extern "C" fn(u64) -> u64, u64)>,
}

// SAFETY: Fiber is only used from a single thread (fiber parent and fiber
// body run sequentially, never concurrently). The raw pointers (stack) are
// owned exclusively.
unsafe impl Send for Fiber {}

// Default fiber stack size: 64 KB (generous for effect handlers).
const DEFAULT_STACK_SIZE: usize = 64 * 1024;
// Guard page size (one 4KB page).
const GUARD_PAGE_SIZE: usize = 4096;

// ---------------------------------------------------------------------------
// Stack allocation (mmap + guard page)
// ---------------------------------------------------------------------------

#[cfg(target_os = "macos")]
fn alloc_stack(size: usize) -> *mut u8 {
    use std::ptr;
    let total = size + GUARD_PAGE_SIZE;
    let ptr = unsafe {
        libc::mmap(
            ptr::null_mut(),
            total,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANON,
            -1,
            0,
        )
    };
    if ptr == libc::MAP_FAILED {
        panic!("fiber: mmap failed");
    }
    // Guard page at bottom (lowest address) — SIGSEGV on stack overflow.
    unsafe {
        libc::mprotect(ptr, GUARD_PAGE_SIZE, libc::PROT_NONE);
    }
    ptr as *mut u8
}

#[cfg(target_os = "linux")]
fn alloc_stack(size: usize) -> *mut u8 {
    use std::ptr;
    let total = size + GUARD_PAGE_SIZE;
    let ptr = unsafe {
        libc::mmap(
            ptr::null_mut(),
            total,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        )
    };
    if ptr == libc::MAP_FAILED {
        panic!("fiber: mmap failed");
    }
    unsafe {
        libc::mprotect(ptr, GUARD_PAGE_SIZE, libc::PROT_NONE);
    }
    ptr as *mut u8
}

fn dealloc_stack(ptr: *mut u8, size: usize) {
    let total = size + GUARD_PAGE_SIZE;
    unsafe {
        libc::munmap(ptr as *mut libc::c_void, total);
    }
}

// ---------------------------------------------------------------------------
// Thread-local: current fiber pointer (for fiber_yield to find its fiber)
// ---------------------------------------------------------------------------

thread_local! {
    static CURRENT_FIBER: Cell<*mut Fiber> = const { Cell::new(std::ptr::null_mut()) };
}

// ---------------------------------------------------------------------------
// Context switch (aarch64)
// ---------------------------------------------------------------------------

/// Switch from `from` context to `to` context.
///
/// Saves callee-saved registers into `from`, loads them from `to`, then
/// returns (to the resumed context's saved lr). Must be `#[naked]` so the
/// compiler does not generate a prologue/epilogue that would conflict with
/// our register save/restore.
#[cfg(target_arch = "aarch64")]
#[unsafe(naked)]
unsafe extern "C" fn switch_context(_from: *mut FiberContext, _to: *const FiberContext) {
    // x0 = from, x1 = to (C calling convention)
    core::arch::naked_asm!(
        // Save callee-saved registers to `from` (x0)
        "mov x9, sp",
        "str x9,  [x0, #0]",       // sp
        "stp x19, x20, [x0, #8]",
        "stp x21, x22, [x0, #24]",
        "stp x23, x24, [x0, #40]",
        "stp x25, x26, [x0, #56]",
        "stp x27, x28, [x0, #72]",
        "stp x29, x30, [x0, #88]",  // fp, lr
        // Save SIMD callee-saved (d8-d15)
        "stp d8,  d9,  [x0, #104]",
        "stp d10, d11, [x0, #120]",
        "stp d12, d13, [x0, #136]",
        "stp d14, d15, [x0, #152]",

        // Load callee-saved registers from `to` (x1)
        "ldr x9,  [x1, #0]",       // sp
        "mov sp, x9",
        "ldp x19, x20, [x1, #8]",
        "ldp x21, x22, [x1, #24]",
        "ldp x23, x24, [x1, #40]",
        "ldp x25, x26, [x1, #56]",
        "ldp x27, x28, [x1, #72]",
        "ldp x29, x30, [x1, #88]",  // fp, lr
        // Restore SIMD callee-saved (d8-d15)
        "ldp d8,  d9,  [x1, #104]",
        "ldp d10, d11, [x1, #120]",
        "ldp d12, d13, [x1, #136]",
        "ldp d14, d15, [x1, #152]",

        "ret",
    );
}

#[cfg(not(target_arch = "aarch64"))]
unsafe extern "C" fn switch_context(_from: *mut FiberContext, _to: *const FiberContext) {
    unimplemented!("fiber: context switch not implemented for this architecture. Only aarch64 is supported.");
}

// ---------------------------------------------------------------------------
// Fiber trampoline (first entry point on the fiber stack)
// ---------------------------------------------------------------------------

/// Trampoline that runs on the fiber's stack. Calls the entry function,
/// stores the result, and switches back to the parent.
extern "C" fn fiber_trampoline(fiber_ptr: *mut Fiber) {
    let fiber = unsafe { &mut *fiber_ptr };

    // Take the entry function (consumed once).
    let (entry_fn, arg) = fiber.entry.take().expect("fiber: entry already consumed");
    let result = entry_fn(arg);

    // Mark completed and switch back to parent.
    fiber.state = FiberState::Completed(result);
    unsafe {
        switch_context(
            &mut fiber.fiber_ctx as *mut FiberContext,
            &fiber.parent_ctx as *const FiberContext,
        );
    }
    // Unreachable: parent will destroy the fiber.
    unreachable!("fiber: returned from completed fiber");
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

impl Fiber {
    /// Create a new fiber that will run `entry_fn(arg)` on a fresh stack.
    pub fn new(entry_fn: extern "C" fn(u64) -> u64, arg: u64) -> Box<Fiber> {
        Self::with_stack_size(entry_fn, arg, DEFAULT_STACK_SIZE)
    }

    /// Create a new fiber with a custom stack size.
    pub fn with_stack_size(
        entry_fn: extern "C" fn(u64) -> u64,
        arg: u64,
        stack_size: usize,
    ) -> Box<Fiber> {
        let stack = alloc_stack(stack_size);

        let mut fiber = Box::new(Fiber {
            fiber_ctx: FiberContext::zeroed(),
            parent_ctx: FiberContext::zeroed(),
            stack,
            stack_size,
            state: FiberState::Ready,
            transfer: 0,
            entry: Some((entry_fn, arg)),
        });

        // Set up the initial context so the first switch_context lands at
        // fiber_trampoline. Stack grows down on aarch64.
        let stack_top = unsafe { stack.add(stack_size + GUARD_PAGE_SIZE) };
        // Align to 16 bytes (ABI requirement).
        let stack_top = (stack_top as usize & !0xF) as *mut u8;

        // Reserve space for the trampoline's stack frame (16 bytes min).
        let initial_sp = unsafe { stack_top.sub(16) } as u64;

        fiber.fiber_ctx.regs[0] = initial_sp;                                // sp
        fiber.fiber_ctx.regs[12] = fiber_trampoline as *const () as u64;      // lr (x30)
        fiber.fiber_ctx.regs[11] = initial_sp;                               // fp (x29)

        // Pass fiber pointer as x0 to the trampoline. We store it in x19
        // (first callee-saved) and the trampoline reads it from there.
        // Actually, we need x0 = fiber_ptr when trampoline is called.
        // Since switch_context does `ret` (jumps to lr), and aarch64 ABI
        // says x0 is caller-saved, we need a different approach.
        //
        // Solution: use a shim that reads x19 → x0, then calls the real
        // trampoline. Store fiber_ptr in x19.
        fiber.fiber_ctx.regs[1] = &*fiber as *const Fiber as u64;  // x19 = fiber_ptr
        fiber.fiber_ctx.regs[12] = fiber_entry_shim as *const () as u64; // lr = shim

        fiber
    }

    /// Resume the fiber, passing `value` to it. Returns a reference to the
    /// fiber's new state (Yielded, Completed, or Aborted).
    ///
    /// For the first resume, `value` is ignored (the fiber hasn't yielded yet).
    pub fn resume(&mut self, value: u64) -> &FiberState {
        match &self.state {
            FiberState::Completed(_) | FiberState::Aborted => {
                panic!("one-shot continuation violated: fiber already {:?}", self.state);
            }
            _ => {}
        }

        self.transfer = value;

        // Set thread-local so fiber_yield can find us.
        let prev = CURRENT_FIBER.with(|c| {
            let old = c.get();
            c.set(self as *mut Fiber);
            old
        });

        unsafe {
            switch_context(
                &mut self.parent_ctx as *mut FiberContext,
                &self.fiber_ctx as *const FiberContext,
            );
        }

        // Restore previous fiber (for nested fibers).
        CURRENT_FIBER.with(|c| c.set(prev));

        &self.state
    }

    /// Read the value passed by the parent on the most recent resume.
    /// Called from within the fiber after yielding.
    pub fn take_transfer(&self) -> u64 {
        self.transfer
    }
}

impl Drop for Fiber {
    fn drop(&mut self) {
        if !self.stack.is_null() {
            dealloc_stack(self.stack, self.stack_size);
            self.stack = std::ptr::null_mut();
        }
    }
}

/// Shim: move x19 (fiber pointer, saved by new()) into x0, then call
/// fiber_trampoline.
#[cfg(target_arch = "aarch64")]
#[unsafe(naked)]
extern "C" fn fiber_entry_shim() {
    core::arch::naked_asm!(
        "mov x0, x19",
        "b {trampoline}",
        trampoline = sym fiber_trampoline,
    );
}

#[cfg(not(target_arch = "aarch64"))]
extern "C" fn fiber_entry_shim() {
    unimplemented!("fiber: entry shim not implemented for this architecture");
}

// ---------------------------------------------------------------------------
// fiber_yield: called FROM the fiber to yield to parent
// ---------------------------------------------------------------------------

/// Yield from the current fiber to its parent, sending an effect key and args.
/// Returns the value passed by the parent on the next `resume()`.
///
/// # Safety
/// Must be called from within a fiber (CURRENT_FIBER must be set).
pub fn fiber_yield(key: String, args: Vec<u64>) -> u64 {
    let fiber_ptr = CURRENT_FIBER.with(|c| c.get());
    assert!(!fiber_ptr.is_null(), "fiber_yield called outside a fiber");
    let fiber = unsafe { &mut *fiber_ptr };

    fiber.state = FiberState::Yielded { key, args };

    unsafe {
        switch_context(
            &mut fiber.fiber_ctx as *mut FiberContext,
            &fiber.parent_ctx as *const FiberContext,
        );
    }

    // When we get here, parent called resume(value) — read the transfer slot.
    fiber.take_transfer()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    extern "C" fn simple_add(arg: u64) -> u64 {
        arg + 100
    }

    #[test]
    fn fiber_runs_to_completion() {
        let mut fiber = Fiber::new(simple_add, 42);
        let state = fiber.resume(0);
        match state {
            FiberState::Completed(val) => assert_eq!(*val, 142),
            other => panic!("expected Completed, got {:?}", std::mem::discriminant(other)),
        }
    }

    extern "C" fn yielding_fn(_arg: u64) -> u64 {
        // Yield once with key "E.get" and no args, receive a value back.
        let val = fiber_yield("E.get".to_string(), vec![]);
        val + 1
    }

    #[test]
    fn fiber_yield_and_resume() {
        let mut fiber = Fiber::new(yielding_fn, 0);

        // First resume starts the fiber.
        let state = fiber.resume(0);
        match state {
            FiberState::Yielded { key, args } => {
                assert_eq!(key, "E.get");
                assert!(args.is_empty());
            }
            other => panic!("expected Yielded, got {:?}", std::mem::discriminant(other)),
        }

        // Resume with value 99.
        let state = fiber.resume(99);
        match state {
            FiberState::Completed(val) => assert_eq!(*val, 100), // 99 + 1
            other => panic!("expected Completed, got {:?}", std::mem::discriminant(other)),
        }
    }

    extern "C" fn multi_yield(_arg: u64) -> u64 {
        let a = fiber_yield("E.first".to_string(), vec![10, 20]);
        let b = fiber_yield("E.second".to_string(), vec![]);
        a + b
    }

    #[test]
    fn fiber_multiple_yields() {
        let mut fiber = Fiber::new(multi_yield, 0);

        let state = fiber.resume(0);
        match state {
            FiberState::Yielded { key, args } => {
                assert_eq!(key, "E.first");
                assert_eq!(args, &[10, 20]);
            }
            _ => panic!("expected first yield"),
        }

        let state = fiber.resume(5);
        match state {
            FiberState::Yielded { key, args } => {
                assert_eq!(key, "E.second");
                assert!(args.is_empty());
            }
            _ => panic!("expected second yield"),
        }

        let state = fiber.resume(7);
        match state {
            FiberState::Completed(val) => assert_eq!(*val, 12), // 5 + 7
            _ => panic!("expected Completed"),
        }
    }

    #[test]
    fn fiber_abort_drops_cleanly() {
        let mut fiber = Fiber::new(yielding_fn, 0);
        let state = fiber.resume(0);
        assert!(matches!(state, FiberState::Yielded { .. }));
        // Drop fiber without resuming — tests clean deallocation.
        fiber.state = FiberState::Aborted;
        drop(fiber);
    }

    #[test]
    #[should_panic(expected = "one-shot continuation violated")]
    fn fiber_one_shot_enforcement() {
        let mut fiber = Fiber::new(simple_add, 42);
        let state = fiber.resume(0);
        assert!(matches!(state, FiberState::Completed(_)));
        // Second resume on completed fiber must panic.
        fiber.resume(0);
    }
}
