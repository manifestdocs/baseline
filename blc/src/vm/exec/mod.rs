mod dispatch_arith;
mod dispatch_call;
mod dispatch_cmp;
mod dispatch_data;
mod dispatch_effects;
pub(crate) mod frame;
mod hof;
mod http_helpers;
mod server;
#[cfg(test)]
mod tests;

use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use super::chunk::{Chunk, CompileError, Op, Program};
use super::natives::NativeRegistry;
use super::nvalue::{HeapObject, NValue};
use super::value::Value;

// ---------------------------------------------------------------------------
// Safety & Resource Limits
// ---------------------------------------------------------------------------
//
// The VM enforces several limits to prevent resource exhaustion attacks:
//
// 1. MAX_CALL_DEPTH: Prevents stack overflow from deep/infinite recursion
// 2. MAX_RANGE_SIZE: Prevents memory exhaustion from large range expressions
// 3. MAX_INSTRUCTIONS: Prevents infinite loops (checked every N iterations)
// 4. MAX_STACK_SIZE: Prevents stack exhaustion from pathological programs
// 5. MAX_STRING_SIZE: Prevents memory exhaustion from string operations
// 6. MAX_LIST_SIZE: Prevents memory exhaustion from list operations
//
// SAFETY note: The unsafe stack operations below (pop_unchecked, pop2_unchecked,
// get_unchecked) are sound because the compiler guarantees stack invariants:
// every pop has a matching push, and local slot indices are always valid.

/// Maximum call stack depth (prevents stack overflow from recursion).
/// 8192 frames × 16 bytes = 128 KB. Enables recursive algorithms on
/// data structures up to ~8K depth (e.g., unbalanced BSTs with 8K nodes).
const MAX_CALL_DEPTH: usize = 8192;

/// Maximum range iteration size (prevents memory exhaustion from `1..1000000000`)
const MAX_RANGE_SIZE: i64 = 1_000_000;

/// Maximum instructions before timeout check (for infinite loop protection)
/// Set to 0 to disable instruction counting (default for non-sandboxed execution)
const MAX_INSTRUCTIONS: u64 = 0; // 0 = unlimited, set to e.g. 100_000_000 for sandboxed

/// Instruction check interval (only check timeout every N instructions)
const INSTRUCTION_CHECK_INTERVAL: u64 = 10_000;

/// Maximum stack size in values (prevents pathological stack growth)
const MAX_STACK_SIZE: usize = 65_536;

/// Maximum string size in bytes (prevents memory exhaustion from concatenation)
#[allow(dead_code)]
const MAX_STRING_SIZE: usize = 100 * 1024 * 1024; // 100 MB

/// Maximum list size in elements (prevents memory exhaustion)
#[allow(dead_code)]
const MAX_LIST_SIZE: usize = 10_000_000; // 10M elements

use frame::{CallFrame, FrameStack, PackedBase};

// ---------------------------------------------------------------------------
// VM
// ---------------------------------------------------------------------------

/// Tracks VM state at handler installation for continuation capture.
pub(crate) struct HandlerBoundary {
    pub(crate) stack_depth: usize,
    pub(crate) frame_depth: usize,
    pub(crate) upvalue_depth: usize,
    pub(crate) handler_stack_idx: usize,
    /// IP to jump to when the handler returns without calling resume (abort).
    /// Set from PushResumableHandler's skip offset.
    pub(crate) return_ip: usize,
    /// Base slot of the frame that installed this handler. Used to determine
    /// the start of the stack segment to capture when building a continuation.
    /// Stored explicitly to avoid unsafe frame-stack arithmetic at capture time.
    pub(crate) original_base_slot: usize,
}

/// The VM execution engine. Each fiber gets its own `Vm` instance.
///
/// Intentionally !Send due to `Rc` in upvalue_stack — fibers share the
/// `Program` (which IS Send+Sync) but each runs its own Vm on a single thread.
pub struct Vm {
    pub(crate) stack: Vec<NValue>,
    pub(crate) frames: FrameStack,
    pub(crate) upvalue_stack: Vec<Rc<Vec<NValue>>>,
    pub(crate) natives: NativeRegistry,
    /// Handler stack for `with { Effect: handler } body` — maps effect module name → handler record
    pub(crate) handler_stack: Vec<HashMap<String, NValue>>,
    /// Boundaries for resumable handlers (for continuation capture).
    pub(crate) handler_boundaries: Vec<HandlerBoundary>,
    /// Instruction counter for timeout/limit checking (0 = unlimited)
    instruction_limit: u64,
    /// Current instruction count
    instruction_count: u64,
    /// Shared program reference for fiber spawning (set via `execute_program_arc`).
    pub(crate) program: Option<Arc<Program>>,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

/// Return type for opcode dispatch helpers.
///
/// Allows helpers to signal control flow changes back to the main loop.
/// Variants correspond to `continue` (Restart) and `return` (Return) in the
/// original monolithic match, while Continue means "fall through to next op".
pub(crate) enum DispatchResult {
    /// Continue to the next instruction (default).
    Continue,
    /// Restart the main loop with updated frame state.
    /// Used by TailCall, handler dispatch, and continuation resumption.
    Restart {
        ip: usize,
        chunk_idx: usize,
        base_slot: usize,
    },
    /// Return from run_frames with a final value.
    Return(NValue),
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            stack: Vec::with_capacity(4096),
            frames: FrameStack::new(),
            upvalue_stack: Vec::new(),
            natives: NativeRegistry::new(),
            handler_stack: Vec::new(),
            handler_boundaries: Vec::new(),
            instruction_limit: MAX_INSTRUCTIONS,
            instruction_count: 0,
            program: None,
        }
    }

    /// Reset VM state for reuse (e.g., thread-local VM between requests).
    /// Preserves allocations but clears all runtime state.
    pub fn reset(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.upvalue_stack.clear();
        self.handler_stack.clear();
        self.handler_boundaries.clear();
        self.instruction_count = 0;
    }

    /// Create a VM with a specific instruction limit for sandboxed execution.
    ///
    /// # Arguments
    /// * `limit` - Maximum instructions to execute (0 = unlimited)
    pub fn with_instruction_limit(limit: u64) -> Self {
        let mut vm = Self::new();
        vm.instruction_limit = limit;
        vm
    }

    /// Get a reference to the native function registry (for compiler lookups).
    pub fn natives(&self) -> &NativeRegistry {
        &self.natives
    }

    /// Execute a single chunk of bytecode (backward-compatible API).
    pub fn execute(&mut self, chunk: &Chunk) -> Result<Value, CompileError> {
        self.stack.clear();
        self.frames.clear();
        self.upvalue_stack.clear();
        self.handler_stack.clear();
        self.handler_boundaries.clear();
        self.instruction_count = 0;
        self.frames.push(CallFrame {
            chunk_idx: 0,
            ip: 0,
            base_slot: PackedBase::from_slot(0),
            upvalue_idx: u32::MAX,
        });
        let result = self.run(std::slice::from_ref(chunk))?;
        Ok(result.to_value())
    }

    /// Execute a multi-chunk program starting at the entry point,
    /// storing an `Arc<Program>` for fiber spawning.
    pub fn execute_program_arc(&mut self, program: Arc<Program>) -> Result<Value, CompileError> {
        self.program = Some(program.clone());
        self.execute_program(&program)
    }

    /// Execute a multi-chunk program starting at the entry point.
    pub fn execute_program(&mut self, program: &Program) -> Result<Value, CompileError> {
        self.stack.clear();
        self.frames.clear();
        self.upvalue_stack.clear();
        self.handler_stack.clear();
        self.handler_boundaries.clear();
        self.instruction_count = 0;
        self.frames.push(CallFrame {
            chunk_idx: program.entry as u32,
            ip: 0,
            base_slot: PackedBase::from_slot(0),
            upvalue_idx: u32::MAX,
        });
        let result = self.run(&program.chunks)?;
        Ok(result.to_value())
    }

    /// Execute a specific chunk within a multi-chunk program.
    /// Used by the VM test runner to execute individual test expression chunks.
    pub fn execute_chunk_at(
        &mut self,
        chunks: &[Chunk],
        chunk_idx: usize,
    ) -> Result<Value, CompileError> {
        self.stack.clear();
        self.frames.clear();
        self.upvalue_stack.clear();
        self.handler_boundaries.clear();
        self.instruction_count = 0;
        self.frames.push(CallFrame {
            chunk_idx: chunk_idx as u32,
            ip: 0,
            base_slot: PackedBase::from_slot(0),
            upvalue_idx: u32::MAX,
        });
        let result = self.run(chunks)?;
        Ok(result.to_value())
    }

    /// Re-entrant call for a function or closure NValue.
    /// Used by the server runtime to dispatch HTTP handlers from within a
    /// running VM. Preserves existing VM state (stack, frames).
    pub fn call_value(
        &mut self,
        func: &NValue,
        args: &[NValue],
        chunks: &[Chunk],
    ) -> Result<NValue, CompileError> {
        self.call_nvalue(func, args, chunks, 0, 0)
    }

    /// Main dispatch loop — shared by execute and execute_program.
    fn run(&mut self, chunks: &[Chunk]) -> Result<NValue, CompileError> {
        self.run_frames(chunks, 0)
    }

    /// Dispatch loop that stops when frame count drops to `base_depth`.
    ///
    /// Performance: This is the hottest code in the VM. Key optimizations:
    /// - NaN-boxed values (8 bytes) instead of enum (24 bytes) for fast stack ops
    /// - Frame state (chunk_idx, ip, base_slot) kept in local variables
    /// - Source map only consulted in error paths
    /// - Unsafe stack access eliminates bounds checks
    /// - Instruction limit checked only every INSTRUCTION_CHECK_INTERVAL ops
    pub(crate) fn run_frames(
        &mut self,
        chunks: &[Chunk],
        base_depth: usize,
    ) -> Result<NValue, CompileError> {
        let frame = self.frames.last();
        let mut ip = frame.ip as usize;
        let mut chunk_idx = frame.chunk_idx as usize;
        let mut base_slot = frame.base_slot.slot();
        let mut chunk = &chunks[chunk_idx];

        // SAFETY: Every chunk is guaranteed to end with Op::Return (see
        // Chunk::ensure_return). The Return handler exits the loop, so we
        // never read past the end of the code vector.
        debug_assert!(
            chunk.code.last() == Some(&Op::Return),
            "chunk must end with Return (sentinel)"
        );

        // Local counter for periodic limit/safety checking (avoids field access on every op)
        let mut local_count: u64 = 0;
        let check_interval = INSTRUCTION_CHECK_INTERVAL;
        let has_limit = self.instruction_limit > 0;

        loop {
            // Periodic safety checks (instruction limit + stack size)
            local_count += 1;
            if local_count >= check_interval {
                if has_limit {
                    self.instruction_count += local_count;
                    if self.instruction_count > self.instruction_limit {
                        let (line, col) = chunk
                            .source_map
                            .get(ip.saturating_sub(1))
                            .copied()
                            .unwrap_or((0, 0));
                        return Err(self.error(
                            format!(
                                "Execution limit exceeded: {} instructions (limit: {})",
                                self.instruction_count, self.instruction_limit
                            ),
                            line,
                            col,
                        ));
                    }
                }

                // Stack size check — only every check_interval ops
                if self.stack.len() > MAX_STACK_SIZE {
                    let (line, col) = chunk
                        .source_map
                        .get(ip.saturating_sub(1))
                        .copied()
                        .unwrap_or((0, 0));
                    return Err(self.error(
                        format!(
                            "Stack overflow: {} values (limit: {})",
                            self.stack.len(),
                            MAX_STACK_SIZE
                        ),
                        line,
                        col,
                    ));
                }

                local_count = 0;
            }

            let op = unsafe { *chunk.code.get_unchecked(ip) };
            ip += 1;

            match op {
                Op::LoadConst(idx) => {
                    // SAFETY: The compiler assigns constant indices; they are
                    // always within bounds.  Using get_unchecked removes the
                    // branch from the hot loop.
                    let constant = unsafe { chunk.constants.get_unchecked(idx as usize) };
                    // For inline NValues (int, float, bool, unit, function)
                    // .clone() is a simple bit-copy — no Arc refcount bump.
                    // This fast path avoids even calling the Clone impl for
                    // the most common constant types.
                    self.stack.push(constant.clone());
                }

                Op::LoadSmallInt(n) => {
                    self.stack.push(NValue::int(n as i64));
                }

                // ============================================================
                // HOT PATH: int arithmetic inlined to avoid double dispatch
                // ============================================================
                Op::AddInt => {
                    let (b, a) = self.pop2_fast();
                    self.stack
                        .push(NValue::int(a.as_any_int().wrapping_add(b.as_any_int())));
                }
                Op::SubInt => {
                    let (b, a) = self.pop2_fast();
                    self.stack
                        .push(NValue::int(a.as_any_int().wrapping_sub(b.as_any_int())));
                }
                Op::MulInt => {
                    let (b, a) = self.pop2_fast();
                    self.stack
                        .push(NValue::int(a.as_any_int().wrapping_mul(b.as_any_int())));
                }
                Op::DivInt => {
                    let (b, a) = self.pop2_fast();
                    let bi = b.as_any_int();
                    if bi == 0 {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error("Division by zero".into(), line, col));
                    }
                    self.stack.push(NValue::int(a.as_any_int() / bi));
                }
                Op::ModInt => {
                    let (b, a) = self.pop2_fast();
                    let bi = b.as_any_int();
                    if bi == 0 {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error("Modulo by zero".into(), line, col));
                    }
                    self.stack.push(NValue::int(a.as_any_int() % bi));
                }

                // ============================================================
                // HOT PATH: int comparisons inlined
                // ============================================================
                Op::GtInt => {
                    let (b, a) = self.pop2_fast();
                    self.stack
                        .push(NValue::bool(a.as_any_int() > b.as_any_int()));
                }
                Op::GeInt => {
                    let (b, a) = self.pop2_fast();
                    self.stack
                        .push(NValue::bool(a.as_any_int() >= b.as_any_int()));
                }
                Op::LtInt => {
                    let (b, a) = self.pop2_fast();
                    self.stack
                        .push(NValue::bool(a.as_any_int() < b.as_any_int()));
                }
                Op::LeInt => {
                    let (b, a) = self.pop2_fast();
                    self.stack
                        .push(NValue::bool(a.as_any_int() <= b.as_any_int()));
                }
                Op::Eq => {
                    let (b, a) = self.pop2_fast();
                    self.stack.push(NValue::bool(a == b));
                }
                Op::Ne => {
                    let (b, a) = self.pop2_fast();
                    self.stack.push(NValue::bool(a != b));
                }

                // Generic arithmetic/comparison → dispatch functions (cold path)
                Op::Add
                | Op::Sub
                | Op::Mul
                | Op::Div
                | Op::Mod
                | Op::Negate
                | Op::Not
                | Op::Concat => {
                    self.dispatch_arithmetic(&op, chunk, ip)?;
                }

                Op::Lt | Op::Gt | Op::Le | Op::Ge => {
                    self.dispatch_comparison(&op, chunk, ip)?;
                }

                Op::GetLocal(slot) => {
                    let idx = base_slot + slot as usize;
                    debug_assert!(idx < self.stack.len());
                    let val = unsafe { self.stack.get_unchecked(idx) }.clone();
                    self.stack.push(val);
                }

                Op::SetLocal(slot) => {
                    let idx = base_slot + slot as usize;
                    debug_assert!(idx < self.stack.len());
                    let val = unsafe { self.stack.get_unchecked(self.stack.len() - 1) }.clone();
                    unsafe {
                        *self.stack.get_unchecked_mut(idx) = val;
                    }
                }

                Op::PopN(n) => {
                    let count = n as usize;
                    let new_len = self.stack.len().saturating_sub(count);
                    self.stack.truncate(new_len);
                }

                Op::CloseScope(n) => {
                    let count = n as usize;
                    if count > 0 && !self.stack.is_empty() {
                        let top = self.pop_fast();
                        let new_len = self.stack.len().saturating_sub(count);
                        self.stack.truncate(new_len);
                        self.stack.push(top);
                    }
                }

                Op::Pop => {
                    self.stack.pop();
                }

                Op::Jump(offset) => {
                    ip += offset as usize;
                }

                Op::JumpIfFalse(offset) => {
                    let v = self.pop_fast();
                    if !v.is_truthy() {
                        ip += offset as usize;
                    }
                }

                Op::JumpIfTrue(offset) => {
                    if let Some(v) = self.stack.last()
                        && v.is_truthy()
                    {
                        ip += offset as usize;
                    }
                }

                Op::JumpBack(offset) => {
                    ip -= offset as usize;
                }

                Op::MakeRange
                | Op::ListGet
                | Op::ListLen
                | Op::MakeList(_)
                | Op::ListConcat
                | Op::ListTailFrom(_)
                | Op::MakeRecord(_)
                | Op::GetField(_)
                | Op::GetFieldIdx(_, _)
                | Op::MakeTuple(_)
                | Op::TupleGet(_)
                | Op::MakeEnum(_)
                | Op::MakeStruct(_)
                | Op::EnumTag
                | Op::EnumPayload
                | Op::UpdateRecord(_) => {
                    self.dispatch_data_structures(&op, chunk, ip)?;
                }

                // ============================================================
                // HOT PATH: CallDirect inlined (most common call form)
                // ============================================================
                Op::CallDirect(target_chunk, arg_count) => {
                    if self.frames.len() >= MAX_CALL_DEPTH {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "Stack overflow: maximum call depth exceeded".into(),
                            line,
                            col,
                        ));
                    }
                    let n = arg_count as usize;
                    let new_base = PackedBase::from_slot(self.stack.len() - n);

                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;

                    self.frames.push(CallFrame {
                        chunk_idx: target_chunk as u32,
                        ip: 0,
                        base_slot: new_base,
                        upvalue_idx: u32::MAX,
                    });
                    ip = 0;
                    chunk_idx = target_chunk as usize;
                    base_slot = new_base.slot();
                    chunk = &chunks[chunk_idx];
                    continue;
                }

                // ============================================================
                // HOT PATH: TailCall inlined (self-recursive loops)
                // ============================================================
                Op::TailCall(arg_count) => {
                    let n = arg_count as usize;
                    let args_start = self.stack.len() - n;
                    for i in 0..n {
                        let val = unsafe { self.stack.get_unchecked(args_start + i) }.clone();
                        unsafe {
                            *self.stack.get_unchecked_mut(base_slot + i) = val;
                        }
                    }
                    self.stack.truncate(base_slot + n);
                    ip = 0;
                    continue;
                }

                // ============================================================
                // HOT PATH: Return inlined (every function exit)
                // ============================================================
                Op::Return => {
                    let result = self.stack.pop().unwrap_or_else(NValue::unit);
                    let frame = self.frames.pop();

                    if frame.upvalue_idx != u32::MAX
                        && frame.upvalue_idx as usize == self.upvalue_stack.len() - 1
                    {
                        self.upvalue_stack.pop();
                    }

                    if self.frames.len() <= base_depth {
                        return Ok(result);
                    }

                    let base = frame.base_slot;
                    if base.has_func() {
                        self.stack.truncate(base.slot().saturating_sub(1));
                    } else {
                        self.stack.truncate(base.slot());
                    }
                    self.stack.push(result);

                    let caller = self.frames.last();
                    ip = caller.ip as usize;
                    chunk_idx = caller.chunk_idx as usize;
                    base_slot = caller.base_slot.slot();
                    chunk = &chunks[chunk_idx];
                    continue;
                }

                // -- Remaining function calls, closures, superinstructions --
                Op::Call(_)
                | Op::GetUpvalue(_)
                | Op::MakeClosure(_, _)
                | Op::CallNative(_, _)
                | Op::GetLocalSubInt(_, _)
                | Op::GetLocalLeInt(_, _)
                | Op::GetLocalAddInt(_, _)
                | Op::GetLocalLtInt(_, _)
                | Op::GetLocalMulInt(_, _)
                | Op::GetLocalGeInt(_, _)
                | Op::GetLocalGetField(_, _)
                | Op::GetLocalLeIntJumpIfFalse(_, _, _)
                | Op::GetLocalLtIntJumpIfFalse(_, _, _) => {
                    match self
                        .dispatch_call(&op, chunk, chunks, ip, chunk_idx, base_slot, base_depth)?
                    {
                        DispatchResult::Continue => {}
                        DispatchResult::Restart {
                            ip: new_ip,
                            chunk_idx: new_ci,
                            base_slot: new_bs,
                        } => {
                            ip = new_ip;
                            chunk_idx = new_ci;
                            base_slot = new_bs;
                            chunk = &chunks[chunk_idx];
                            continue;
                        }
                        DispatchResult::Return(val) => return Ok(val),
                    }
                }

                // -- Effect handlers --
                Op::PushHandler
                | Op::PushResumableHandler(_)
                | Op::PerformEffect(_, _)
                | Op::PopHandler
                | Op::Halt(_) => {
                    match self.dispatch_effects(
                        &op, chunk, chunks, ip, chunk_idx, base_slot, base_depth,
                    )? {
                        DispatchResult::Continue => {}
                        DispatchResult::Restart {
                            ip: new_ip,
                            chunk_idx: new_ci,
                            base_slot: new_bs,
                        } => {
                            ip = new_ip;
                            chunk_idx = new_ci;
                            base_slot = new_bs;
                            chunk = &chunks[chunk_idx];
                            continue;
                        }
                        DispatchResult::Return(val) => return Ok(val),
                    }
                }
            }
        }
    }

    // -- Helpers --

    #[inline(always)]
    pub(crate) fn pop_fast(&mut self) -> NValue {
        debug_assert!(!self.stack.is_empty());
        unsafe {
            let new_len = self.stack.len() - 1;
            self.stack.set_len(new_len);
            std::ptr::read(self.stack.as_ptr().add(new_len))
        }
    }

    #[inline(always)]
    pub(crate) fn pop2_fast(&mut self) -> (NValue, NValue) {
        debug_assert!(self.stack.len() >= 2);
        unsafe {
            let new_len = self.stack.len() - 2;
            let ptr = self.stack.as_ptr();
            let b = std::ptr::read(ptr.add(new_len + 1));
            let a = std::ptr::read(ptr.add(new_len));
            self.stack.set_len(new_len);
            (b, a)
        }
    }

    /// Search the handler stack for an effect handler matching `effect_name` and `method_name`.
    ///
    /// Returns `(handler_fn, boundary_idx)` where `boundary_idx` is `Some` for
    /// resumable handlers and `None` for tail-resumptive ones.
    pub(crate) fn find_handler(
        &self,
        effect_name: &str,
        method_name: &str,
    ) -> Option<(NValue, Option<usize>)> {
        if let Some((hs_idx, frame)) = self.handler_stack.iter().enumerate().next_back() {
            let handler_record = frame.get(effect_name)?;
            if let HeapObject::Record(fields) = handler_record.as_heap_ref() {
                for (k, v) in fields {
                    if k.as_ref() == method_name {
                        let boundary_idx = self
                            .handler_boundaries
                            .iter()
                            .position(|b| b.handler_stack_idx == hs_idx);
                        return Some((v.clone(), boundary_idx));
                    }
                }
            }
            return None;
        }
        None
    }

    pub(crate) fn error(&self, message: String, line: usize, col: usize) -> CompileError {
        CompileError { message, line, col }
    }
}
