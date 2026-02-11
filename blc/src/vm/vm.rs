use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use super::chunk::{Chunk, CompileError, Op, Program};
use super::natives::NativeRegistry;
use super::nvalue::{HeapObject, NValue};
use super::value::{RcStr, Value};

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

/// Maximum call stack depth (prevents stack overflow from recursion)
const MAX_CALL_DEPTH: usize = 1024;

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
const MAX_STRING_SIZE: usize = 100 * 1024 * 1024; // 100 MB

/// Maximum list size in elements (prevents memory exhaustion)
const MAX_LIST_SIZE: usize = 10_000_000; // 10M elements


// ---------------------------------------------------------------------------
// Call Frame
// ---------------------------------------------------------------------------

/// Compact call frame — 16 bytes instead of 32.
/// Upvalues are stored in a separate side-stack (`Vm::upvalue_stack`)
/// indexed by `upvalue_idx`. `u32::MAX` means no upvalues (plain function).
///
/// `base_slot` encoding: bit 31 indicates whether a function value sits below
/// the args on the stack (set for `Call`, clear for `CallDirect`). The actual
/// slot index is `base_slot & 0x7FFF_FFFF`.
#[derive(Copy, Clone)]
struct CallFrame {
    chunk_idx: u32,
    ip: u32,
    base_slot: u32,
    upvalue_idx: u32,
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
const FRAME_HAS_FUNC: u32 = 0x8000_0000;

// ---------------------------------------------------------------------------
// Frame Stack
// ---------------------------------------------------------------------------

/// Fixed-capacity frame stack. Avoids `Vec` capacity checks on every push/pop.
/// 16 KB total (1024 × 16 bytes), allocated once.
struct FrameStack {
    frames: Box<[CallFrame; MAX_CALL_DEPTH]>,
    len: usize,
}

impl FrameStack {
    fn new() -> Self {
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
    fn push(&mut self, frame: CallFrame) {
        // SAFETY: Caller must ensure len < MAX_CALL_DEPTH (checked in Call/CallDirect).
        debug_assert!(self.len < MAX_CALL_DEPTH);
        unsafe {
            *self.frames.get_unchecked_mut(self.len) = frame;
        }
        self.len += 1;
    }

    #[inline(always)]
    fn pop(&mut self) -> CallFrame {
        debug_assert!(self.len > 0);
        self.len -= 1;
        // SAFETY: We just decremented len; the frame at that index was previously written.
        unsafe { *self.frames.get_unchecked(self.len) }
    }

    #[inline(always)]
    fn last(&self) -> &CallFrame {
        debug_assert!(self.len > 0);
        // SAFETY: len > 0, so len - 1 is in bounds.
        unsafe { self.frames.get_unchecked(self.len - 1) }
    }

    #[inline(always)]
    fn last_mut(&mut self) -> &mut CallFrame {
        debug_assert!(self.len > 0);
        let idx = self.len - 1;
        // SAFETY: len > 0, so idx is in bounds.
        unsafe { self.frames.get_unchecked_mut(idx) }
    }

    #[inline(always)]
    fn len(&self) -> usize {
        self.len
    }

    fn clear(&mut self) {
        self.len = 0;
    }
}

// ---------------------------------------------------------------------------
// VM
// ---------------------------------------------------------------------------

/// Tracks VM state at handler installation for continuation capture.
struct HandlerBoundary {
    stack_depth: usize,
    frame_depth: usize,
    upvalue_depth: usize,
    handler_stack_idx: usize,
    /// IP to jump to when the handler returns without calling resume (abort).
    /// Set from PushResumableHandler's skip offset.
    return_ip: usize,
}

pub struct Vm {
    stack: Vec<NValue>,
    frames: FrameStack,
    upvalue_stack: Vec<Rc<Vec<NValue>>>,
    natives: NativeRegistry,
    /// Handler stack for `with { Effect: handler } body` — maps effect module name → handler record
    handler_stack: Vec<HashMap<String, NValue>>,
    /// Boundaries for resumable handlers (for continuation capture).
    handler_boundaries: Vec<HandlerBoundary>,
    /// Instruction counter for timeout/limit checking (0 = unlimited)
    instruction_limit: u64,
    /// Current instruction count
    instruction_count: u64,
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
enum DispatchResult {
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
        }
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
            base_slot: 0,
            upvalue_idx: u32::MAX,
        });
        let result = self.run(std::slice::from_ref(chunk))?;
        Ok(result.to_value())
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
            base_slot: 0,
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
            base_slot: 0,
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
    fn run_frames(&mut self, chunks: &[Chunk], base_depth: usize) -> Result<NValue, CompileError> {
        let frame = self.frames.last();
        let mut ip = frame.ip as usize;
        let mut chunk_idx = frame.chunk_idx as usize;
        let mut base_slot = (frame.base_slot & !FRAME_HAS_FUNC) as usize;
        let mut chunk = &chunks[chunk_idx];

        // SAFETY: Every chunk is guaranteed to end with Op::Return (see
        // Chunk::ensure_return). The Return handler exits the loop, so we
        // never read past the end of the code vector.
        debug_assert!(
            chunk.code.last() == Some(&Op::Return),
            "chunk must end with Return (sentinel)"
        );

        // Local counter for periodic limit checking (avoids field access on every op)
        let mut local_count: u64 = 0;
        let check_interval = INSTRUCTION_CHECK_INTERVAL;
        let has_limit = self.instruction_limit > 0;

        loop {
            // Periodic instruction limit check (only if limit is set)
            if has_limit {
                local_count += 1;
                if local_count >= check_interval {
                    self.instruction_count += local_count;
                    local_count = 0;
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
            }

            // Stack size check (less frequent than instruction check)
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

            let op = unsafe { *chunk.code.get_unchecked(ip) };
            ip += 1;

            match op {
                Op::LoadConst(idx) => {
                    let val = chunk.constants[idx as usize].clone();
                    self.stack.push(val);
                }

                Op::LoadSmallInt(n) => {
                    self.stack.push(NValue::int(n as i64));
                }

                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod
                | Op::Negate | Op::AddInt | Op::SubInt | Op::Not
                | Op::MulInt | Op::DivInt | Op::ModInt | Op::GtInt | Op::GeInt
                | Op::Concat => {
                    self.dispatch_arithmetic(&op, chunk, ip)?;
                }

                Op::Eq | Op::Ne | Op::Lt | Op::Gt | Op::Le | Op::Ge
                | Op::LtInt | Op::LeInt => {
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

                Op::MakeRange | Op::ListGet | Op::ListLen | Op::MakeList(_)
                | Op::ListConcat | Op::MakeRecord(_) | Op::GetField(_)
                | Op::MakeTuple(_) | Op::TupleGet(_) | Op::MakeEnum(_)
                | Op::MakeStruct(_) | Op::EnumTag | Op::EnumPayload
                | Op::UpdateRecord(_) => {
                    self.dispatch_data_structures(&op, chunk, ip)?;
                }

                // -- Function calls, closures, superinstructions --
                Op::Call(_) | Op::CallDirect(_, _) | Op::GetUpvalue(_)
                | Op::MakeClosure(_, _) | Op::CallNative(_, _)
                | Op::GetLocalSubInt(_, _) | Op::GetLocalLeInt(_, _)
                | Op::GetLocalAddInt(_, _) | Op::GetLocalLtInt(_, _)
                | Op::GetLocalLeIntJumpIfFalse(_, _, _) | Op::GetLocalLtIntJumpIfFalse(_, _, _)
                | Op::TailCall(_) => {
                    match self.dispatch_call(&op, chunk, chunks, ip, chunk_idx, base_slot, base_depth)? {
                        DispatchResult::Continue => {}
                        DispatchResult::Restart { ip: new_ip, chunk_idx: new_ci, base_slot: new_bs } => {
                            ip = new_ip;
                            chunk_idx = new_ci;
                            base_slot = new_bs;
                            chunk = &chunks[chunk_idx];
                            continue;
                        }
                        DispatchResult::Return(val) => return Ok(val),
                    }
                }

                // -- Effect handlers and control flow --
                Op::PushHandler | Op::PushResumableHandler(_)
                | Op::PerformEffect(_, _) | Op::PopHandler
                | Op::Halt(_) | Op::Return => {
                    match self.dispatch_effects(&op, chunk, chunks, ip, chunk_idx, base_slot, base_depth)? {
                        DispatchResult::Continue => {}
                        DispatchResult::Restart { ip: new_ip, chunk_idx: new_ci, base_slot: new_bs } => {
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
    fn pop_fast(&mut self) -> NValue {
        debug_assert!(!self.stack.is_empty());
        unsafe {
            let new_len = self.stack.len() - 1;
            self.stack.set_len(new_len);
            std::ptr::read(self.stack.as_ptr().add(new_len))
        }
    }

    #[inline(always)]
    fn pop2_fast(&mut self) -> (NValue, NValue) {
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

    // -- Dispatch helpers --
    //
    // Each helper handles a group of related opcodes. All are #[inline(always)]
    // so the compiler inlines them into the main dispatch loop — zero overhead.

    /// Arithmetic and logic opcodes: Add, Sub, Mul, Div, Mod, Negate, Not,
    /// AddInt, SubInt, MulInt, DivInt, ModInt, GtInt, GeInt, Concat.
    #[inline(always)]
    fn dispatch_arithmetic(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        ip: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::Add => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    self.stack
                        .push(NValue::int(a.as_int().wrapping_add(b.as_int())));
                } else if a.is_number() && b.is_number() {
                    self.stack.push(NValue::float(a.as_f64() + b.as_f64()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot add {} and {}", a, b), line, col));
                }
            }
            Op::Sub => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    self.stack
                        .push(NValue::int(a.as_int().wrapping_sub(b.as_int())));
                } else if a.is_number() && b.is_number() {
                    self.stack.push(NValue::float(a.as_f64() - b.as_f64()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("Cannot subtract {} from {}", b, a),
                        line,
                        col,
                    ));
                }
            }
            Op::Mul => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    self.stack
                        .push(NValue::int(a.as_int().wrapping_mul(b.as_int())));
                } else if a.is_number() && b.is_number() {
                    self.stack.push(NValue::float(a.as_f64() * b.as_f64()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("Cannot multiply {} and {}", a, b),
                        line,
                        col,
                    ));
                }
            }
            Op::Div => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    if b.as_int() == 0 {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error("Division by zero".into(), line, col));
                    }
                    self.stack.push(NValue::int(a.as_int() / b.as_int()));
                } else if a.is_number() && b.is_number() {
                    self.stack.push(NValue::float(a.as_f64() / b.as_f64()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot divide {} by {}", a, b), line, col));
                }
            }
            Op::Mod => {
                let (b, a) = self.pop2_fast();
                if a.is_int() && b.is_int() {
                    if b.as_int() == 0 {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error("Modulo by zero".into(), line, col));
                    }
                    self.stack.push(NValue::int(a.as_int() % b.as_int()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot modulo {} by {}", a, b), line, col));
                }
            }
            Op::Negate => {
                let v = self.pop_fast();
                if v.is_int() {
                    self.stack.push(NValue::int(-v.as_int()));
                } else if v.is_float() {
                    self.stack.push(NValue::float(-v.as_float()));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot negate {}", v), line, col));
                }
            }
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
            Op::Not => {
                let v = self.pop_fast();
                self.stack.push(NValue::bool(!v.is_truthy()));
            }
            Op::Concat => {
                let (b, a) = self.pop2_fast();
                // Fast path: both strings — avoid Display overhead
                if a.is_heap()
                    && b.is_heap()
                    && let (HeapObject::String(sa), HeapObject::String(sb)) =
                        (a.as_heap_ref(), b.as_heap_ref())
                {
                    let mut s = String::with_capacity(sa.len() + sb.len());
                    s.push_str(sa);
                    s.push_str(sb);
                    self.stack.push(NValue::string(s.into()));
                    return Ok(DispatchResult::Continue);
                }
                // Slow path: mixed types via Display
                let result = NValue::string(format!("{}{}", a, b).into());
                self.stack.push(result);
            }
            _ => unreachable!("dispatch_arithmetic called with non-arithmetic op"),
        }
        Ok(DispatchResult::Continue)
    }

    /// Comparison opcodes: Eq, Ne, Lt, Gt, Le, Ge, LtInt, LeInt.
    #[inline(always)]
    fn dispatch_comparison(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        ip: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::Eq => {
                let (b, a) = self.pop2_fast();
                self.stack.push(NValue::bool(a == b));
            }
            Op::Ne => {
                let (b, a) = self.pop2_fast();
                self.stack.push(NValue::bool(a != b));
            }
            Op::Lt => {
                let (b, a) = self.pop2_fast();
                match self.compare_lt(&a, &b) {
                    Some(result) => self.stack.push(NValue::bool(result)),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot compare {} and {}", a, b),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::Gt => {
                let (b, a) = self.pop2_fast();
                match self.compare_lt(&b, &a) {
                    Some(result) => self.stack.push(NValue::bool(result)),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot compare {} and {}", a, b),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::Le => {
                let (b, a) = self.pop2_fast();
                match self.compare_lt(&a, &b) {
                    Some(lt) => self.stack.push(NValue::bool(lt || a == b)),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot compare {} and {}", a, b),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::Ge => {
                let (b, a) = self.pop2_fast();
                match self.compare_lt(&b, &a) {
                    Some(lt) => self.stack.push(NValue::bool(lt || a == b)),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot compare {} and {}", a, b),
                            line,
                            col,
                        ));
                    }
                }
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
            _ => unreachable!("dispatch_comparison called with non-comparison op"),
        }
        Ok(DispatchResult::Continue)
    }

    /// Data structure opcodes: MakeRange, ListGet, ListLen, MakeList, ListConcat,
    /// MakeRecord, GetField, MakeTuple, TupleGet, MakeEnum, MakeStruct,
    /// EnumTag, EnumPayload, UpdateRecord.
    #[inline(always)]
    fn dispatch_data_structures(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        ip: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::MakeRange => {
                let (end_val, start_val) = self.pop2_fast();
                if start_val.is_any_int() && end_val.is_any_int() {
                    let start = start_val.as_any_int();
                    let end = end_val.as_any_int();
                    let size = end - start;
                    if size > MAX_RANGE_SIZE {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!(
                                "Range too large ({} elements, max {})",
                                size, MAX_RANGE_SIZE
                            ),
                            line,
                            col,
                        ));
                    }
                    let list: Vec<NValue> = (start..end).map(NValue::int).collect();
                    self.stack.push(NValue::list(list));
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("Range requires integers, got {} and {}", start_val, end_val),
                        line,
                        col,
                    ));
                }
            }
            Op::ListGet => {
                let (idx_val, list_val) = self.pop2_fast();
                if list_val.is_heap() && idx_val.is_any_int() {
                    if let HeapObject::List(items) = list_val.as_heap_ref() {
                        let idx_i64 = idx_val.as_any_int();
                        if idx_i64 < 0 {
                            let (line, col) = chunk.source_map[ip - 1];
                            return Err(self.error(
                                format!("Negative index {} (len {})", idx_i64, items.len()),
                                line,
                                col,
                            ));
                        }
                        let idx = idx_i64 as usize;
                        if idx >= items.len() {
                            let (line, col) = chunk.source_map[ip - 1];
                            return Err(self.error(
                                format!("Index {} out of bounds (len {})", idx, items.len()),
                                line,
                                col,
                            ));
                        }
                        self.stack.push(items[idx].clone());
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!(
                                "ListGet requires List and Int, got {} and {}",
                                list_val, idx_val
                            ),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!(
                            "ListGet requires List and Int, got {} and {}",
                            list_val, idx_val
                        ),
                        line,
                        col,
                    ));
                }
            }
            Op::ListLen => {
                let list_val = self.pop_fast();
                if list_val.is_heap() {
                    if let HeapObject::List(items) = list_val.as_heap_ref() {
                        self.stack.push(NValue::int(items.len() as i64));
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("ListLen requires List, got {}", list_val),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("ListLen requires List, got {}", list_val),
                        line,
                        col,
                    ));
                }
            }
            Op::MakeList(n) => {
                let count = *n as usize;
                let start = self.stack.len() - count;
                let items: Vec<NValue> = self.stack.drain(start..).collect();
                self.stack.push(NValue::list(items));
            }
            Op::ListConcat => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let r_items = right.as_list().unwrap();
                let l_items = left.as_list().unwrap();
                let mut result = l_items.clone();
                result.extend_from_slice(r_items);
                self.stack.push(NValue::list(result));
            }
            Op::MakeRecord(n) => {
                let count = *n as usize;
                let start = self.stack.len() - count * 2;
                let pairs: Vec<NValue> = self.stack.drain(start..).collect();
                let mut fields = Vec::with_capacity(count);
                for pair in pairs.chunks(2) {
                    if pair[0].is_heap() {
                        if let HeapObject::String(key) = pair[0].as_heap_ref() {
                            fields.push((key.clone(), pair[1].clone()));
                        } else {
                            let (line, col) = chunk.source_map[ip - 1];
                            return Err(self.error(
                                format!("Record key must be String, got {}", pair[0]),
                                line,
                                col,
                            ));
                        }
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Record key must be String, got {}", pair[0]),
                            line,
                            col,
                        ));
                    }
                }
                self.stack.push(NValue::record(fields));
            }
            Op::GetField(name_idx) => {
                let field_name = match chunk.constants[*name_idx as usize].as_string() {
                    Some(s) => s.clone(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "GetField constant must be String".into(),
                            line,
                            col,
                        ));
                    }
                };
                let record = self.pop_fast();
                if !record.is_heap() {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("Cannot access field '{}' on {}", field_name, record),
                        line,
                        col,
                    ));
                }
                match record.as_heap_ref() {
                    HeapObject::Record(fields) | HeapObject::Struct { fields, .. } => {
                        match fields.iter().find(|(k, _)| *k == field_name) {
                            Some((_, v)) => self.stack.push(v.clone()),
                            None => {
                                let (line, col) = chunk.source_map[ip - 1];
                                return Err(self.error(
                                    format!("Record has no field '{}'", field_name),
                                    line,
                                    col,
                                ));
                            }
                        }
                    }
                    _ => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("Cannot access field '{}' on {}", field_name, record),
                            line,
                            col,
                        ));
                    }
                }
            }
            Op::MakeTuple(n) => {
                let count = *n as usize;
                let start = self.stack.len() - count;
                let items: Vec<NValue> = self.stack.drain(start..).collect();
                self.stack.push(NValue::tuple(items));
            }
            Op::TupleGet(idx) => {
                let tuple = self.pop_fast();
                if tuple.is_heap() {
                    if let HeapObject::Tuple(items) = tuple.as_heap_ref() {
                        let i = *idx as usize;
                        if i >= items.len() {
                            let (line, col) = chunk.source_map[ip - 1];
                            return Err(self.error(
                                format!(
                                    "Tuple index {} out of bounds (len {})",
                                    i,
                                    items.len()
                                ),
                                line,
                                col,
                            ));
                        }
                        self.stack.push(items[i].clone());
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("TupleGet requires Tuple, got {}", tuple),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("TupleGet requires Tuple, got {}", tuple),
                        line,
                        col,
                    ));
                }
            }
            Op::MakeEnum(tag_idx) => {
                let tag = match chunk.constants[*tag_idx as usize].as_string() {
                    Some(s) => s.clone(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "MakeEnum constant must be String".into(),
                            line,
                            col,
                        ));
                    }
                };
                let payload = self.pop_fast();
                self.stack.push(NValue::enum_val(tag, payload));
            }
            Op::MakeStruct(tag_idx) => {
                let tag = match chunk.constants[*tag_idx as usize].as_string() {
                    Some(s) => s.clone(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "MakeStruct constant must be String".into(),
                            line,
                            col,
                        ));
                    }
                };
                let record = self.pop_fast();
                if record.is_heap() {
                    if let HeapObject::Record(fields) = record.as_heap_ref() {
                        self.stack.push(NValue::struct_val(tag, fields.clone()));
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("MakeStruct requires Record, got {}", record),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("MakeStruct requires Record, got {}", record),
                        line,
                        col,
                    ));
                }
            }
            Op::EnumTag => {
                let val = self.pop_fast();
                if val.is_heap() {
                    if let HeapObject::Enum { tag, .. } = val.as_heap_ref() {
                        self.stack.push(NValue::string(tag.clone()));
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("EnumTag requires Enum, got {}", val),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("EnumTag requires Enum, got {}", val),
                        line,
                        col,
                    ));
                }
            }
            Op::EnumPayload => {
                let val = self.pop_fast();
                if val.is_heap() {
                    if let HeapObject::Enum { payload, .. } = val.as_heap_ref() {
                        self.stack.push(payload.clone());
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("EnumPayload requires Enum, got {}", val),
                            line,
                            col,
                        ));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("EnumPayload requires Enum, got {}", val),
                        line,
                        col,
                    ));
                }
            }
            Op::UpdateRecord(n) => {
                let count = *n as usize;
                let start = self.stack.len() - count * 2;
                let updates: Vec<NValue> = self.stack.drain(start..).collect();
                let base = self.pop_fast();
                if !base.is_heap() {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("UpdateRecord requires Record, got {}", base),
                        line,
                        col,
                    ));
                }
                match base.as_heap_ref() {
                    HeapObject::Record(fields) => {
                        let mut new_fields = fields.clone();
                        for pair in updates.chunks(2) {
                            if pair[0].is_heap()
                                && let HeapObject::String(key) = pair[0].as_heap_ref()
                            {
                                if let Some(existing) =
                                    new_fields.iter_mut().find(|(k, _)| *k == *key)
                                {
                                    existing.1 = pair[1].clone();
                                } else {
                                    let (line, col) = chunk.source_map[ip - 1];
                                    return Err(self.error(
                                        format!("Record has no field '{}'", key),
                                        line,
                                        col,
                                    ));
                                }
                            }
                        }
                        self.stack.push(NValue::record(new_fields));
                    }
                    HeapObject::Struct { name, fields } => {
                        let mut new_fields = fields.clone();
                        for pair in updates.chunks(2) {
                            if pair[0].is_heap()
                                && let HeapObject::String(key) = pair[0].as_heap_ref()
                            {
                                if let Some(existing) =
                                    new_fields.iter_mut().find(|(k, _)| *k == *key)
                                {
                                    existing.1 = pair[1].clone();
                                } else {
                                    let (line, col) = chunk.source_map[ip - 1];
                                    return Err(self.error(
                                        format!("Record has no field '{}'", key),
                                        line,
                                        col,
                                    ));
                                }
                            }
                        }
                        self.stack
                            .push(NValue::struct_val(name.clone(), new_fields));
                    }
                    _ => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            format!("UpdateRecord requires Record, got {}", base),
                            line,
                            col,
                        ));
                    }
                }
            }
            _ => unreachable!("dispatch_data_structures called with non-data-structure op"),
        }
        Ok(DispatchResult::Continue)
    }

    /// Function call opcodes: Call, CallDirect, CallNative, TailCall,
    /// GetUpvalue, MakeClosure, and superinstructions.
    #[inline(always)]
    fn dispatch_call(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        chunks: &[Chunk],
        ip: usize,
        chunk_idx: usize,
        base_slot: usize,
        base_depth: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::Call(arg_count) => {
                if self.frames.len() >= MAX_CALL_DEPTH {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        "Stack overflow: maximum call depth exceeded".into(),
                        line,
                        col,
                    ));
                }
                let n = *arg_count as usize;
                let func_pos = self.stack.len() - n - 1;
                let func = unsafe { self.stack.get_unchecked(func_pos) };

                // Check for continuation call first
                if func.is_continuation() {
                    let (ss, fs, us, hs, hbs, r_ip, r_ci) = match func.as_heap_ref() {
                        HeapObject::Continuation {
                            stack_segment,
                            frame_segment,
                            upvalue_segment,
                            handler_stack_segment,
                            handler_boundary_segment,
                            resume_ip,
                            resume_chunk_idx,
                            ..
                        } => (
                            stack_segment.clone(),
                            frame_segment.clone(),
                            upvalue_segment.clone(),
                            handler_stack_segment.clone(),
                            handler_boundary_segment.clone(),
                            *resume_ip,
                            *resume_chunk_idx,
                        ),
                        _ => unreachable!(),
                    };
                    let resume_value = if n == 1 {
                        self.stack.pop().unwrap()
                    } else {
                        NValue::unit()
                    };

                    let handler_frame = self.frames.pop();
                    let raw_base = handler_frame.base_slot;
                    let has_func = raw_base & FRAME_HAS_FUNC != 0;
                    let handler_base = (raw_base & !FRAME_HAS_FUNC) as usize;
                    if has_func {
                        self.stack.truncate(handler_base - 1);
                    } else {
                        self.stack.truncate(handler_base);
                    }

                    if handler_frame.upvalue_idx != u32::MAX {
                        if handler_frame.upvalue_idx as usize == self.upvalue_stack.len() - 1 {
                            self.upvalue_stack.pop();
                        }
                    }

                    self.stack.extend(ss);

                    for &(ci, fip, fbs, fuv) in &fs {
                        self.frames.push(CallFrame {
                            chunk_idx: ci,
                            ip: fip,
                            base_slot: fbs,
                            upvalue_idx: fuv,
                        });
                    }

                    for uvs in us {
                        self.upvalue_stack.push(Rc::new(uvs));
                    }

                    for h in hs {
                        self.handler_stack.push(h);
                    }
                    for (sd, fd, ud, hsi, rip) in hbs {
                        self.handler_boundaries.push(HandlerBoundary {
                            stack_depth: sd,
                            frame_depth: fd,
                            upvalue_depth: ud,
                            handler_stack_idx: hsi,
                            return_ip: rip,
                        });
                    }

                    self.stack.push(resume_value);

                    let new_ip = r_ip as usize;
                    let new_ci = r_ci as usize;
                    let top = self.frames.last();
                    let new_bs = (top.base_slot & !FRAME_HAS_FUNC) as usize;
                    return Ok(DispatchResult::Restart {
                        ip: new_ip,
                        chunk_idx: new_ci,
                        base_slot: new_bs,
                    });
                }

                let (new_chunk_idx, new_upvalue_idx) = if func.is_function() {
                    (func.as_function(), u32::MAX)
                } else if func.is_heap() {
                    if let HeapObject::Closure {
                        chunk_idx: idx,
                        upvalues,
                    } = func.as_heap_ref()
                    {
                        let uv_idx = self.upvalue_stack.len() as u32;
                        self.upvalue_stack.push(Rc::new(upvalues.clone()));
                        (*idx, uv_idx)
                    } else {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(format!("Cannot call {}", func), line, col));
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(format!("Cannot call {}", func), line, col));
                };

                let caller = self.frames.last_mut();
                caller.ip = ip as u32;
                caller.chunk_idx = chunk_idx as u32;

                let new_base = (func_pos + 1) as u32;
                self.frames.push(CallFrame {
                    chunk_idx: new_chunk_idx as u32,
                    ip: 0,
                    base_slot: new_base | FRAME_HAS_FUNC,
                    upvalue_idx: new_upvalue_idx,
                });
                return Ok(DispatchResult::Restart {
                    ip: 0,
                    chunk_idx: new_chunk_idx,
                    base_slot: new_base as usize,
                });
            }

            Op::CallDirect(target_chunk, arg_count) => {
                if self.frames.len() >= MAX_CALL_DEPTH {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        "Stack overflow: maximum call depth exceeded".into(),
                        line,
                        col,
                    ));
                }
                let n = *arg_count as usize;
                let new_base = (self.stack.len() - n) as u32;

                let caller = self.frames.last_mut();
                caller.ip = ip as u32;
                caller.chunk_idx = chunk_idx as u32;

                self.frames.push(CallFrame {
                    chunk_idx: *target_chunk as u32,
                    ip: 0,
                    base_slot: new_base,
                    upvalue_idx: u32::MAX,
                });
                return Ok(DispatchResult::Restart {
                    ip: 0,
                    chunk_idx: *target_chunk as usize,
                    base_slot: new_base as usize,
                });
            }

            Op::GetUpvalue(idx) => {
                let frame = self.frames.last();
                let uv_idx = frame.upvalue_idx as usize;
                let val = self.upvalue_stack[uv_idx][*idx as usize].clone();
                self.stack.push(val);
            }

            Op::MakeClosure(new_chunk_idx, upvalue_count) => {
                let n = *upvalue_count as usize;
                let start = self.stack.len() - n;
                let upvalues: Vec<NValue> = self.stack.drain(start..).collect();
                self.stack
                    .push(NValue::closure(*new_chunk_idx as usize, upvalues));
            }

            Op::CallNative(fn_id, arg_count) => {
                let n = *arg_count as usize;

                // Effect handler interception
                if !self.handler_stack.is_empty() {
                    let name = self.natives.name(*fn_id);
                    if let Some(dot) = name.find('.') {
                        let module = &name[..dot];
                        let method = name[dot + 1..]
                            .strip_suffix('!')
                            .unwrap_or(&name[dot + 1..]);
                        let mut found_handler = None;
                        let mut found_boundary_idx = None;
                        for (hs_idx, frame) in
                            self.handler_stack.iter().enumerate().rev()
                        {
                            if let Some(handler_record) = frame.get(module) {
                                if let HeapObject::Record(fields) =
                                    handler_record.as_heap_ref()
                                {
                                    for (k, v) in fields {
                                        if k.as_ref() == method {
                                            found_handler = Some(v.clone());
                                            for (bi, b) in
                                                self.handler_boundaries.iter().enumerate()
                                            {
                                                if b.handler_stack_idx == hs_idx {
                                                    found_boundary_idx = Some(bi);
                                                    break;
                                                }
                                            }
                                            break;
                                        }
                                    }
                                }
                                break;
                            }
                        }
                        if let Some(handler_fn) = found_handler {
                            if let Some(bi) = found_boundary_idx {
                                // Resumable handler: capture continuation
                                let boundary = &self.handler_boundaries[bi];
                                let bd_stack = boundary.stack_depth;
                                let bd_frame = boundary.frame_depth;
                                let bd_upvalue = boundary.upvalue_depth;
                                let handler_depth = boundary.handler_stack_idx;
                                let return_ip = boundary.return_ip;

                                let resume_ip = ip as u32;
                                let resume_chunk_idx = chunk_idx as u32;

                                let caller = self.frames.last_mut();
                                caller.ip = return_ip as u32;
                                caller.chunk_idx = chunk_idx as u32;

                                let args_start = self.stack.len() - n;
                                let stack_segment: Vec<NValue> =
                                    self.stack[bd_stack..args_start].to_vec();

                                let frame_segment: Vec<(u32, u32, u32, u32)> = (bd_frame
                                    ..self.frames.len())
                                    .map(|i| {
                                        let f =
                                            unsafe { *self.frames.frames.get_unchecked(i) };
                                        (f.chunk_idx, f.ip, f.base_slot, f.upvalue_idx)
                                    })
                                    .collect();

                                let upvalue_segment: Vec<Vec<NValue>> = self.upvalue_stack
                                    [bd_upvalue..]
                                    .iter()
                                    .map(|rc| (**rc).clone())
                                    .collect();

                                let args: Vec<NValue> =
                                    self.stack.drain(args_start..).collect();

                                let handler_stack_segment: Vec<HashMap<String, NValue>> =
                                    self.handler_stack[handler_depth..].to_vec();
                                let handler_boundary_segment: Vec<(usize, usize, usize, usize, usize)> =
                                    self.handler_boundaries[bi..]
                                        .iter()
                                        .map(|b| (b.stack_depth, b.frame_depth, b.upvalue_depth, b.handler_stack_idx, b.return_ip))
                                        .collect();

                                self.stack.truncate(bd_stack);
                                self.frames.len = bd_frame;
                                self.upvalue_stack.truncate(bd_upvalue);
                                self.handler_stack.truncate(handler_depth);
                                self.handler_boundaries.truncate(bi);

                                let cont = NValue::continuation(
                                    stack_segment,
                                    frame_segment,
                                    upvalue_segment,
                                    handler_depth,
                                    handler_stack_segment,
                                    handler_boundary_segment,
                                    resume_ip,
                                    resume_chunk_idx,
                                );

                                for arg in args {
                                    self.stack.push(arg);
                                }
                                self.stack.push(cont);

                                let total_args = n + 1;
                                if handler_fn.is_function() {
                                    let fn_idx = handler_fn.as_function();
                                    let new_base =
                                        (self.stack.len() - total_args) as u32;
                                    self.frames.push(CallFrame {
                                        chunk_idx: fn_idx as u32,
                                        ip: 0,
                                        base_slot: new_base,
                                        upvalue_idx: u32::MAX,
                                    });
                                    return Ok(DispatchResult::Restart {
                                        ip: 0,
                                        chunk_idx: fn_idx,
                                        base_slot: new_base as usize,
                                    });
                                } else if let HeapObject::Closure {
                                    chunk_idx: cidx,
                                    upvalues,
                                } = handler_fn.as_heap_ref()
                                {
                                    let fn_idx = *cidx;
                                    let new_base =
                                        (self.stack.len() - total_args) as u32;
                                    self.upvalue_stack
                                        .push(Rc::new(upvalues.clone()));
                                    let uv_idx =
                                        (self.upvalue_stack.len() - 1) as u32;
                                    self.frames.push(CallFrame {
                                        chunk_idx: fn_idx as u32,
                                        ip: 0,
                                        base_slot: new_base,
                                        upvalue_idx: uv_idx,
                                    });
                                    return Ok(DispatchResult::Restart {
                                        ip: 0,
                                        chunk_idx: fn_idx,
                                        base_slot: new_base as usize,
                                    });
                                }
                            } else {
                                // Tail-resumptive (no boundary): call handler inline
                                let start = self.stack.len() - n;
                                if handler_fn.is_function() {
                                    let fn_idx = handler_fn.as_function();
                                    let caller = self.frames.last_mut();
                                    caller.ip = ip as u32;
                                    caller.chunk_idx = chunk_idx as u32;
                                    self.frames.push(CallFrame {
                                        chunk_idx: fn_idx as u32,
                                        ip: 0,
                                        base_slot: start as u32,
                                        upvalue_idx: u32::MAX,
                                    });
                                    return Ok(DispatchResult::Restart {
                                        ip: 0,
                                        chunk_idx: fn_idx,
                                        base_slot: start,
                                    });
                                } else if handler_fn.is_heap() {
                                    if let HeapObject::Closure {
                                        chunk_idx: cidx,
                                        upvalues,
                                    } = handler_fn.as_heap_ref()
                                    {
                                        let fn_idx = *cidx;
                                        let caller = self.frames.last_mut();
                                        caller.ip = ip as u32;
                                        caller.chunk_idx = chunk_idx as u32;
                                        self.upvalue_stack
                                            .push(Rc::new(upvalues.clone()));
                                        let uv_idx = self.upvalue_stack.len() - 1;
                                        self.frames.push(CallFrame {
                                            chunk_idx: fn_idx as u32,
                                            ip: 0,
                                            base_slot: start as u32,
                                            upvalue_idx: uv_idx as u32,
                                        });
                                        return Ok(DispatchResult::Restart {
                                            ip: 0,
                                            chunk_idx: fn_idx,
                                            base_slot: start,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }

                if self.natives.is_hof(*fn_id) {
                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;
                    let (line, col) = chunk.source_map[ip - 1];
                    self.dispatch_hof(*fn_id, n, chunks, line, col)?;
                } else if self.natives.is_server_listen(*fn_id) {
                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;
                    let (line, col) = chunk.source_map[ip - 1];

                    #[cfg(feature = "async-server")]
                    {
                        self.dispatch_server_listen_async(n, chunks, line, col)?;
                    }
                    #[cfg(not(feature = "async-server"))]
                    {
                        self.dispatch_server_listen(n, chunks, line, col)?;
                    }
                } else {
                    let start = self.stack.len() - n;
                    let result = self.natives.call(*fn_id, &self.stack[start..]);
                    let result = result.map_err(|e| {
                        let (line, col) = chunk.source_map[ip - 1];
                        self.error(format!("{}: {}", self.natives.name(*fn_id), e.0), line, col)
                    })?;
                    self.stack.truncate(start);
                    self.stack.push(result);
                }
            }

            // -- Superinstructions --
            Op::GetLocalSubInt(slot, k) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                self.stack.push(NValue::int(val.wrapping_sub(*k as i64)));
            }

            Op::GetLocalLeInt(slot, k) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                self.stack.push(NValue::bool(val <= *k as i64));
            }

            Op::GetLocalAddInt(slot, k) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                self.stack.push(NValue::int(val.wrapping_add(*k as i64)));
            }

            Op::GetLocalLtInt(slot, k) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                self.stack.push(NValue::bool(val < *k as i64));
            }

            Op::GetLocalLeIntJumpIfFalse(slot, k, offset) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                if val > *k as i64 {
                    return Ok(DispatchResult::Restart {
                        ip: ip + *offset as usize,
                        chunk_idx,
                        base_slot,
                    });
                }
            }

            Op::GetLocalLtIntJumpIfFalse(slot, k, offset) => {
                let idx = base_slot + *slot as usize;
                debug_assert!(idx < self.stack.len());
                let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                if val >= *k as i64 {
                    return Ok(DispatchResult::Restart {
                        ip: ip + *offset as usize,
                        chunk_idx,
                        base_slot,
                    });
                }
            }

            Op::TailCall(arg_count) => {
                let n = *arg_count as usize;
                let args_start = self.stack.len() - n;
                for i in 0..n {
                    let val = unsafe { self.stack.get_unchecked(args_start + i) }.clone();
                    unsafe {
                        *self.stack.get_unchecked_mut(base_slot + i) = val;
                    }
                }
                self.stack.truncate(base_slot + n);
                return Ok(DispatchResult::Restart {
                    ip: 0,
                    chunk_idx,
                    base_slot,
                });
            }

            _ => unreachable!("dispatch_call called with non-call op"),
        }
        Ok(DispatchResult::Continue)
    }

    /// Effect handler and control flow opcodes: PushHandler, PushResumableHandler,
    /// PerformEffect, PopHandler, Halt, Return.
    #[inline(always)]
    fn dispatch_effects(
        &mut self,
        op: &Op,
        chunk: &Chunk,
        chunks: &[Chunk],
        ip: usize,
        chunk_idx: usize,
        base_slot: usize,
        base_depth: usize,
    ) -> Result<DispatchResult, CompileError> {
        match op {
            Op::PushHandler => {
                let handler_record = self.stack.pop().unwrap();
                match handler_record.as_heap_ref() {
                    HeapObject::Record(fields) => {
                        let mut map = HashMap::new();
                        for (k, v) in fields {
                            map.insert(k.to_string(), v.clone());
                        }
                        self.handler_stack.push(map);
                    }
                    _ => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "PushHandler: expected Record".to_string(),
                            line,
                            col,
                        ));
                    }
                }
            }

            Op::PushResumableHandler(skip_offset) => {
                let handler_record = self.stack.pop().unwrap();
                match handler_record.as_heap_ref() {
                    HeapObject::Record(fields) => {
                        let mut map = HashMap::new();
                        for (k, v) in fields {
                            map.insert(k.to_string(), v.clone());
                        }
                        let handler_stack_idx = self.handler_stack.len();
                        self.handler_stack.push(map);
                        let return_ip = (ip - 1) + 1 + *skip_offset as usize;
                        self.handler_boundaries.push(HandlerBoundary {
                            stack_depth: self.stack.len(),
                            frame_depth: self.frames.len(),
                            upvalue_depth: self.upvalue_stack.len(),
                            handler_stack_idx,
                            return_ip,
                        });
                    }
                    _ => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "PushResumableHandler: expected Record".to_string(),
                            line,
                            col,
                        ));
                    }
                }
            }

            Op::PerformEffect(name_idx, arg_count) => {
                let key = match chunk.constants[*name_idx as usize].as_string() {
                    Some(s) => s.to_string(),
                    None => {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "PerformEffect: expected string key".to_string(),
                            line,
                            col,
                        ));
                    }
                };
                let n = *arg_count as usize;

                let (effect_name, method_name) = if let Some(dot) = key.find('.') {
                    (&key[..dot], &key[dot + 1..])
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("PerformEffect: invalid key '{}'", key),
                        line,
                        col,
                    ));
                };

                // Search handler_stack in reverse for matching effect
                let mut found_handler = None;
                let mut found_boundary_idx = None;
                for (hs_idx, frame) in self.handler_stack.iter().enumerate().rev() {
                    if let Some(handler_record) = frame.get(effect_name) {
                        if let HeapObject::Record(fields) = handler_record.as_heap_ref() {
                            for (k, v) in fields {
                                if k.as_ref() == method_name {
                                    found_handler = Some(v.clone());
                                    for (bi, b) in self.handler_boundaries.iter().enumerate() {
                                        if b.handler_stack_idx == hs_idx {
                                            found_boundary_idx = Some(bi);
                                            break;
                                        }
                                    }
                                    break;
                                }
                            }
                        }
                        break;
                    }
                }

                if let Some(handler_fn) = found_handler {
                    if let Some(bi) = found_boundary_idx {
                        // Resumable handler: capture continuation
                        let boundary = &self.handler_boundaries[bi];
                        let bd_stack = boundary.stack_depth;
                        let bd_frame = boundary.frame_depth;
                        let bd_upvalue = boundary.upvalue_depth;
                        let handler_depth = boundary.handler_stack_idx;
                        let return_ip = boundary.return_ip;

                        let resume_ip = ip as u32;
                        let resume_chunk_idx = chunk_idx as u32;

                        let caller = self.frames.last_mut();
                        caller.ip = return_ip as u32;
                        caller.chunk_idx = chunk_idx as u32;

                        let args_start = self.stack.len() - n;
                        let stack_segment: Vec<NValue> =
                            self.stack[bd_stack..args_start].to_vec();

                        let frame_segment: Vec<(u32, u32, u32, u32)> = (bd_frame
                            ..self.frames.len())
                            .map(|i| {
                                let f = unsafe { *self.frames.frames.get_unchecked(i) };
                                (f.chunk_idx, f.ip, f.base_slot, f.upvalue_idx)
                            })
                            .collect();

                        let upvalue_segment: Vec<Vec<NValue>> = self.upvalue_stack
                            [bd_upvalue..]
                            .iter()
                            .map(|rc| (**rc).clone())
                            .collect();

                        let args: Vec<NValue> = self.stack.drain(args_start..).collect();

                        let handler_stack_segment: Vec<HashMap<String, NValue>> =
                            self.handler_stack[handler_depth..].to_vec();
                        let handler_boundary_segment: Vec<(usize, usize, usize, usize, usize)> =
                            self.handler_boundaries[bi..]
                                .iter()
                                .map(|b| (b.stack_depth, b.frame_depth, b.upvalue_depth, b.handler_stack_idx, b.return_ip))
                                .collect();

                        self.stack.truncate(bd_stack);
                        self.frames.len = bd_frame;
                        self.upvalue_stack.truncate(bd_upvalue);
                        self.handler_stack.truncate(handler_depth);
                        self.handler_boundaries.truncate(bi);

                        let cont = NValue::continuation(
                            stack_segment,
                            frame_segment,
                            upvalue_segment,
                            handler_depth,
                            handler_stack_segment,
                            handler_boundary_segment,
                            resume_ip,
                            resume_chunk_idx,
                        );

                        for arg in args {
                            self.stack.push(arg);
                        }
                        self.stack.push(cont);

                        let total_args = n + 1;
                        if handler_fn.is_function() {
                            let fn_idx = handler_fn.as_function();
                            let new_base = (self.stack.len() - total_args) as u32;
                            self.frames.push(CallFrame {
                                chunk_idx: fn_idx as u32,
                                ip: 0,
                                base_slot: new_base,
                                upvalue_idx: u32::MAX,
                            });
                            return Ok(DispatchResult::Restart {
                                ip: 0,
                                chunk_idx: fn_idx,
                                base_slot: new_base as usize,
                            });
                        } else if let HeapObject::Closure {
                            chunk_idx: cidx,
                            upvalues,
                        } = handler_fn.as_heap_ref()
                        {
                            let fn_idx = *cidx;
                            let new_base = (self.stack.len() - total_args) as u32;
                            self.upvalue_stack.push(Rc::new(upvalues.clone()));
                            let uv_idx = (self.upvalue_stack.len() - 1) as u32;
                            self.frames.push(CallFrame {
                                chunk_idx: fn_idx as u32,
                                ip: 0,
                                base_slot: new_base,
                                upvalue_idx: uv_idx,
                            });
                            return Ok(DispatchResult::Restart {
                                ip: 0,
                                chunk_idx: fn_idx,
                                base_slot: new_base as usize,
                            });
                        }
                    } else {
                        // Tail-resumptive (no boundary): call handler inline
                        let start = self.stack.len() - n;
                        if handler_fn.is_function() {
                            let fn_idx = handler_fn.as_function();
                            let caller = self.frames.last_mut();
                            caller.ip = ip as u32;
                            caller.chunk_idx = chunk_idx as u32;
                            self.frames.push(CallFrame {
                                chunk_idx: fn_idx as u32,
                                ip: 0,
                                base_slot: start as u32,
                                upvalue_idx: u32::MAX,
                            });
                            return Ok(DispatchResult::Restart {
                                ip: 0,
                                chunk_idx: fn_idx,
                                base_slot: start,
                            });
                        } else if let HeapObject::Closure {
                            chunk_idx: cidx,
                            upvalues,
                        } = handler_fn.as_heap_ref()
                        {
                            let fn_idx = *cidx;
                            let caller = self.frames.last_mut();
                            caller.ip = ip as u32;
                            caller.chunk_idx = chunk_idx as u32;
                            self.upvalue_stack.push(Rc::new(upvalues.clone()));
                            let uv_idx = (self.upvalue_stack.len() - 1) as u32;
                            self.frames.push(CallFrame {
                                chunk_idx: fn_idx as u32,
                                ip: 0,
                                base_slot: start as u32,
                                upvalue_idx: uv_idx,
                            });
                            return Ok(DispatchResult::Restart {
                                ip: 0,
                                chunk_idx: fn_idx,
                                base_slot: start,
                            });
                        }
                    }
                } else {
                    let (line, col) = chunk.source_map[ip - 1];
                    return Err(self.error(
                        format!("Unhandled effect: {}", key),
                        line,
                        col,
                    ));
                }
            }

            Op::PopHandler => {
                if self.handler_stack.is_empty() {
                    // Handler was already consumed — nothing to pop.
                } else {
                    let hs_idx = self.handler_stack.len() - 1;
                    if let Some(bi) = self.handler_boundaries.last() {
                        if bi.handler_stack_idx == hs_idx {
                            self.handler_boundaries.pop();
                        }
                    }
                    self.handler_stack.pop();
                }
            }

            Op::Halt(idx) => {
                let msg = match chunk.constants[*idx as usize].as_string() {
                    Some(s) => s.to_string(),
                    None => "Runtime error".to_string(),
                };
                let (line, col) = chunk.source_map[ip - 1];
                return Err(self.error(msg, line, col));
            }

            Op::Return => {
                let result = self.stack.pop().unwrap_or_else(NValue::unit);
                let frame = self.frames.pop();

                if frame.upvalue_idx != u32::MAX {
                    if frame.upvalue_idx as usize == self.upvalue_stack.len() - 1 {
                        self.upvalue_stack.pop();
                    }
                }

                if self.frames.len() <= base_depth {
                    return Ok(DispatchResult::Return(result));
                }

                let raw_base = frame.base_slot;
                let has_func = raw_base & FRAME_HAS_FUNC != 0;
                let frame_base = (raw_base & !FRAME_HAS_FUNC) as usize;
                if has_func {
                    self.stack.truncate(frame_base - 1);
                } else {
                    self.stack.truncate(frame_base);
                }
                self.stack.push(result);

                let caller = self.frames.last();
                return Ok(DispatchResult::Restart {
                    ip: caller.ip as usize,
                    chunk_idx: caller.chunk_idx as usize,
                    base_slot: (caller.base_slot & !FRAME_HAS_FUNC) as usize,
                });
            }

            _ => unreachable!("dispatch_effects called with non-effect op"),
        }
        Ok(DispatchResult::Continue)
    }

    #[inline(always)]
    fn compare_lt(&self, a: &NValue, b: &NValue) -> Option<bool> {
        if a.is_int() && b.is_int() {
            Some(a.as_int() < b.as_int())
        } else if a.is_number() && b.is_number() {
            Some(a.as_f64() < b.as_f64())
        } else if a.is_heap() && b.is_heap() {
            match (a.as_heap_ref(), b.as_heap_ref()) {
                (HeapObject::String(x), HeapObject::String(y)) => Some(x < y),
                _ => None,
            }
        } else {
            None
        }
    }

    fn pop(&mut self, line: usize, col: usize) -> Result<NValue, CompileError> {
        self.stack
            .pop()
            .ok_or_else(|| self.error("Stack underflow".into(), line, col))
    }

    /// Execute a HOF native function by calling bytecode closures from within the VM loop.
    fn dispatch_hof(
        &mut self,
        fn_id: u16,
        arg_count: usize,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<(), CompileError> {
        let name = self.natives.name(fn_id);
        match name {
            "List.map" => {
                if arg_count != 2 {
                    return Err(self.error("List.map: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                if !list.is_heap() {
                    return Err(self.error("List.map: first arg must be List".into(), line, col));
                }
                let items = match list.as_heap_ref() {
                    HeapObject::List(v) => v.clone(),
                    _ => {
                        return Err(self.error(
                            "List.map: first arg must be List".into(),
                            line,
                            col,
                        ));
                    }
                };
                let mut results = Vec::with_capacity(items.len());
                for item in &items {
                    let result =
                        self.call_nvalue(&func, std::slice::from_ref(item), chunks, line, col)?;
                    results.push(result);
                }
                self.stack.push(NValue::list(results));
            }
            "List.filter" => {
                if arg_count != 2 {
                    return Err(self.error("List.filter: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                if !list.is_heap() {
                    return Err(self.error(
                        "List.filter: first arg must be List".into(),
                        line,
                        col,
                    ));
                }
                let items = match list.as_heap_ref() {
                    HeapObject::List(v) => v.clone(),
                    _ => {
                        return Err(self.error(
                            "List.filter: first arg must be List".into(),
                            line,
                            col,
                        ));
                    }
                };
                let mut results = Vec::new();
                for item in &items {
                    let result =
                        self.call_nvalue(&func, std::slice::from_ref(item), chunks, line, col)?;
                    if result.is_truthy() {
                        results.push(item.clone());
                    }
                }
                self.stack.push(NValue::list(results));
            }
            "List.fold" => {
                if arg_count != 3 {
                    return Err(self.error("List.fold: expected 3 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let initial = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                if !list.is_heap() {
                    return Err(self.error("List.fold: first arg must be List".into(), line, col));
                }
                let items = match list.as_heap_ref() {
                    HeapObject::List(v) => v.clone(),
                    _ => {
                        return Err(self.error(
                            "List.fold: first arg must be List".into(),
                            line,
                            col,
                        ));
                    }
                };
                let mut acc = initial;
                for item in &items {
                    acc = self.call_nvalue(&func, &[acc, item.clone()], chunks, line, col)?;
                }
                self.stack.push(acc);
            }
            "List.find" => {
                if arg_count != 2 {
                    return Err(self.error("List.find: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let list = self.pop(line, col)?;
                if !list.is_heap() {
                    return Err(self.error("List.find: first arg must be List".into(), line, col));
                }
                let items = match list.as_heap_ref() {
                    HeapObject::List(v) => v.clone(),
                    _ => {
                        return Err(self.error(
                            "List.find: first arg must be List".into(),
                            line,
                            col,
                        ));
                    }
                };
                let mut found = NValue::enum_val("None".into(), NValue::unit());
                for item in &items {
                    let result =
                        self.call_nvalue(&func, std::slice::from_ref(item), chunks, line, col)?;
                    if result.is_truthy() {
                        found = NValue::enum_val("Some".into(), item.clone());
                        break;
                    }
                }
                self.stack.push(found);
            }
            "Option.map" => {
                if arg_count != 2 {
                    return Err(self.error("Option.map: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let opt = self.pop(line, col)?;
                if !opt.is_heap() {
                    return Err(self.error(
                        "Option.map: first arg must be Option".into(),
                        line,
                        col,
                    ));
                }
                match opt.as_heap_ref() {
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Some" => {
                        let result = self.call_nvalue(
                            &func,
                            std::slice::from_ref(payload),
                            chunks,
                            line,
                            col,
                        )?;
                        self.stack.push(NValue::enum_val("Some".into(), result));
                    }
                    HeapObject::Enum { tag, .. } if &**tag == "None" => {
                        self.stack
                            .push(NValue::enum_val("None".into(), NValue::unit()));
                    }
                    _ => {
                        return Err(self.error(
                            "Option.map: first arg must be Option".into(),
                            line,
                            col,
                        ));
                    }
                }
            }
            "Result.map" => {
                if arg_count != 2 {
                    return Err(self.error("Result.map: expected 2 arguments".into(), line, col));
                }
                let func = self.pop(line, col)?;
                let res = self.pop(line, col)?;
                if !res.is_heap() {
                    return Err(self.error(
                        "Result.map: first arg must be Result".into(),
                        line,
                        col,
                    ));
                }
                match res.as_heap_ref() {
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Ok" => {
                        let result = self.call_nvalue(
                            &func,
                            std::slice::from_ref(payload),
                            chunks,
                            line,
                            col,
                        )?;
                        self.stack.push(NValue::enum_val("Ok".into(), result));
                    }
                    HeapObject::Enum { tag, payload, .. } if &**tag == "Err" => {
                        self.stack
                            .push(NValue::enum_val("Err".into(), payload.clone()));
                    }
                    _ => {
                        return Err(self.error(
                            "Result.map: first arg must be Result".into(),
                            line,
                            col,
                        ));
                    }
                }
            }
            _ => {
                return Err(self.error(format!("Unknown HOF: {}", name), line, col));
            }
        }
        Ok(())
    }

    /// Call a bytecode function/closure with given arguments (NValue version).
    ///
    /// This is the main entry point for invoking Baseline functions from Rust.
    pub fn call_nvalue(
        &mut self,
        func: &NValue,
        args: &[NValue],
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<NValue, CompileError> {
        if self.frames.len() >= MAX_CALL_DEPTH {
            return Err(self.error(
                "Stack overflow: maximum call depth exceeded".into(),
                line,
                col,
            ));
        }
        let base_depth = self.frames.len();
        let stack_base = self.stack.len();

        // Intercept NativeMwNext: when middleware calls next(req), dispatch
        // to the remaining middleware chain instead of bytecode execution.
        if func.is_heap() {
            if let HeapObject::NativeMwNext {
                handler,
                remaining_mw,
            } = func.as_heap_ref()
            {
                let handler = handler.clone();
                let remaining_mw = remaining_mw.clone();
                return self.call_mw_next(&handler, &remaining_mw, args, chunks, line, col);
            }
        }

        let (ci, uv_idx) = if func.is_function() {
            (func.as_function(), u32::MAX)
        } else if func.is_heap() {
            if let HeapObject::Closure {
                chunk_idx,
                upvalues,
            } = func.as_heap_ref()
            {
                let idx = self.upvalue_stack.len() as u32;
                self.upvalue_stack.push(Rc::new(upvalues.clone()));
                (*chunk_idx, idx)
            } else {
                return Err(self.error(format!("Cannot call {}", func), line, col));
            }
        } else {
            return Err(self.error(format!("Cannot call {}", func), line, col));
        };

        self.stack.push(func.clone());
        for arg in args {
            self.stack.push(arg.clone());
        }
        let bs = self.stack.len() - args.len();
        self.frames.push(CallFrame {
            chunk_idx: ci as u32,
            ip: 0,
            // Bit 31 set: function value sits at base_slot - 1
            base_slot: bs as u32 | FRAME_HAS_FUNC,
            upvalue_idx: uv_idx,
        });

        let result = self.run_frames(chunks, base_depth)?;
        self.stack.truncate(stack_base);
        Ok(result)
    }

    /// Dispatch Server.listen!(router, port) via Hyper+Tokio async server.
    /// Handles both Server.listen! and Server.listen_async! when async-server feature is enabled.
    #[cfg(feature = "async-server")]
    fn dispatch_server_listen_async(
        &mut self,
        arg_count: usize,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<(), CompileError> {
        if arg_count != 2 {
            return Err(self.error(
                format!(
                    "Server.listen! expects 2 arguments (router, port), got {}",
                    arg_count
                ),
                line,
                col,
            ));
        }

        let port_val = self.pop(line, col)?;
        let router_val = self.pop(line, col)?;

        if !port_val.is_any_int() {
            return Err(self.error(
                format!("Server.listen! port must be Int, got {}", port_val),
                line,
                col,
            ));
        }
        let port = port_val.as_any_int() as u16;

        let router_fields = match router_val.as_record() {
            Some(f) => f.clone(),
            None => {
                return Err(self.error(
                    format!(
                        "Server.listen! first argument must be a Router, got {}",
                        router_val
                    ),
                    line,
                    col,
                ));
            }
        };

        let middleware: Vec<NValue> = router_fields
            .iter()
            .find(|(k, _)| &**k == "middleware")
            .and_then(|(_, v)| v.as_list())
            .cloned()
            .unwrap_or_default();

        let sendable_mw: Vec<SendableHandler> = middleware
            .iter()
            .filter_map(SendableHandler::from_nvalue)
            .collect();

        let route_tree = crate::vm::hyper_server::AsyncRouteTree::from_nvalue(&router_val)
            .map_err(|e| self.error(format!("Failed to build route tree: {}", e), line, col))?;

        let chunks_vec = chunks.to_vec();

        let addr = std::net::SocketAddr::from(([0, 0, 0, 0], port));
        let ctx = std::sync::Arc::new(crate::vm::hyper_server::AsyncServerContext::with_executor(
            route_tree,
            chunks_vec,
            sendable_mw,
        ));
        let config = crate::vm::hyper_server::ServerConfig::default();

        let rt = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .map_err(|e| self.error(format!("Failed to create runtime: {}", e), line, col))?;

        rt.block_on(crate::vm::hyper_server::run_server_with_context(addr, ctx, config))
            .map_err(|e| self.error(format!("Server error: {}", e), line, col))?;

        self.stack.push(NValue::unit());
        Ok(())
    }

    /// Dispatch Server.listen!(router, port) — blocks in accept loop.
    /// Spawns a thread pool for concurrent request handling.
    fn dispatch_server_listen(
        &mut self,
        arg_count: usize,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<(), CompileError> {
        if arg_count != 2 {
            return Err(self.error(
                format!(
                    "Server.listen! expects 2 arguments (router, port), got {}",
                    arg_count
                ),
                line,
                col,
            ));
        }
        let port_val = self.pop(line, col)?;
        let router_val = self.pop(line, col)?;

        if !port_val.is_any_int() {
            return Err(self.error(
                format!("Server.listen! port must be Int, got {}", port_val),
                line,
                col,
            ));
        }
        let port = port_val.as_any_int() as u16;

        let router_fields = match router_val.as_record() {
            Some(f) => f.clone(),
            None => {
                return Err(self.error(
                    format!(
                        "Server.listen! first argument must be a Router, got {}",
                        router_val
                    ),
                    line,
                    col,
                ));
            }
        };

        let routes: Vec<NValue> = router_fields
            .iter()
            .find(|(k, _)| &**k == "routes")
            .and_then(|(_, v)| v.as_list())
            .cloned()
            .unwrap_or_default();

        let middleware: Vec<NValue> = router_fields
            .iter()
            .find(|(k, _)| &**k == "middleware")
            .and_then(|(_, v)| v.as_list())
            .cloned()
            .unwrap_or_default();

        // Serialize route tree + middleware + chunks to thread-safe form
        let sendable_routes = SendableRouteTree::from_nv_routes(&routes);
        let sendable_mw: Vec<SendableHandler> = middleware
            .iter()
            .filter_map(SendableHandler::from_nvalue)
            .collect();
        let sendable_chunks = SendableChunks::from_chunks(chunks);

        let addr = format!("0.0.0.0:{}", port);
        let server = tiny_http::Server::http(&addr).map_err(|e| {
            self.error(
                format!("Failed to start server on {}: {}", addr, e),
                line,
                col,
            )
        })?;

        let num_workers = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4);
        eprintln!(
            "[server] Listening on http://0.0.0.0:{} ({} workers)",
            port, num_workers
        );

        let server = Arc::new(server);
        let sendable_routes = Arc::new(sendable_routes);
        let sendable_mw = Arc::new(sendable_mw);
        let sendable_chunks = Arc::new(sendable_chunks);

        let mut handles = Vec::with_capacity(num_workers);
        for _ in 0..num_workers {
            let server = Arc::clone(&server);
            let s_routes = Arc::clone(&sendable_routes);
            let s_mw = Arc::clone(&sendable_mw);
            let s_chunks = Arc::clone(&sendable_chunks);

            handles.push(std::thread::spawn(move || {
                // Each thread gets its own VM + reconstructed NValue data
                let mut vm = Vm::new();
                let chunks = s_chunks.to_chunks();
                let route_tree = s_routes.to_nv_radix_tree();
                let middleware: Vec<NValue> = s_mw.iter().map(|h| h.to_nvalue()).collect();

                for request in server.incoming_requests() {
                    handle_request(&mut vm, request, &route_tree, &middleware, &chunks);
                }
            }));
        }

        // Block until all workers finish (server dropped = workers exit)
        for handle in handles {
            let _ = handle.join();
        }

        self.stack.push(NValue::unit());
        Ok(())
    }

    /// Apply middleware chain for VM server dispatch.
    /// Builds the chain inside-out: each middleware receives (request, next) where
    /// next is a closure wrapping the remaining middleware + handler.
    ///
    /// This is the main entry point for invoking handlers with middleware from Rust.
    pub fn apply_mw_chain(
        &mut self,
        middleware: &[NValue],
        handler: &NValue,
        request: &NValue,
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<NValue, CompileError> {
        if middleware.is_empty() {
            return self.call_nvalue(handler, &[request.clone()], chunks, line, col);
        }

        // Build the "next" chain inside-out: innermost = handler
        // Each layer wraps: fn(req) => middleware[i](req, next_inner)
        //
        // We use NativeMwNext heap objects to carry the chain. When the
        // middleware calls next(req), the VM intercepts it.
        let current_mw = &middleware[0];
        let remaining = &middleware[1..];

        // Build next as a closure that, when called with (req), invokes the
        // remaining middleware chain. We represent this as a NativeMwNext
        // heap object that the VM call_nvalue can intercept.
        let next = build_mw_next_nvalue(remaining, handler);

        self.call_nvalue(current_mw, &[request.clone(), next], chunks, line, col)
    }

    /// Handle a call to a NativeMwNext value (middleware chain continuation).
    fn call_mw_next(
        &mut self,
        handler: &NValue,
        remaining_mw: &[NValue],
        args: &[NValue],
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<NValue, CompileError> {
        if args.is_empty() {
            return Err(self.error(
                "Middleware next() requires a request argument".into(),
                line,
                col,
            ));
        }
        let request = &args[0];
        self.apply_mw_chain(remaining_mw, handler, request, chunks, line, col)
    }

    fn error(&self, message: String, line: usize, col: usize) -> CompileError {
        CompileError { message, line, col }
    }
}

// ---------------------------------------------------------------------------
// Middleware chain helper
// ---------------------------------------------------------------------------

/// Build a NativeMwNext NValue representing the continuation of a middleware chain.
fn build_mw_next_nvalue(remaining_mw: &[NValue], handler: &NValue) -> NValue {
    NValue::from_heap_obj(HeapObject::NativeMwNext {
        handler: handler.clone(),
        remaining_mw: remaining_mw.to_vec(),
    })
}

// ---------------------------------------------------------------------------
// Thread-pool server: handle a single request
// ---------------------------------------------------------------------------

fn handle_request(
    vm: &mut Vm,
    mut request: tiny_http::Request,
    route_tree: &NvRadixTree,
    middleware: &[NValue],
    chunks: &[Chunk],
) {
    let req_method = request.method().to_string();
    let raw_url = request.url().to_string();
    let mut req_body = String::new();
    if let Err(e) = request.as_reader().read_to_string(&mut req_body) {
        eprintln!("[server] Failed to read request body: {}", e);
        let response = tiny_http::Response::from_string("Internal Server Error")
            .with_status_code(tiny_http::StatusCode(500));
        let _ = request.respond(response);
        return;
    }

    let (req_path, query_record) = parse_url_query_nv(&raw_url);

    let req_headers: Vec<NValue> = request
        .headers()
        .iter()
        .map(|h| {
            NValue::tuple(vec![
                NValue::string(h.field.to_string().into()),
                NValue::string(h.value.to_string().into()),
            ])
        })
        .collect();

    let req_record = NValue::record(vec![
        ("body".into(), NValue::string(req_body.into())),
        ("headers".into(), NValue::list(req_headers)),
        ("method".into(), NValue::string(req_method.clone().into())),
        ("params".into(), NValue::record(Vec::new())),
        ("query".into(), query_record),
        ("url".into(), NValue::string(raw_url.into())),
    ]);

    let mut params = SmallParams::new();
    let (status, resp_headers, body) = match route_tree.find(&req_method, &req_path, &mut params) {
        Some(handler) => {
            let enriched = inject_params_nv(&req_record, params.as_slice());
            let result = if middleware.is_empty() {
                vm.call_nvalue(&handler, &[enriched], chunks, 0, 0)
            } else {
                vm.apply_mw_chain(middleware, &handler, &enriched, chunks, 0, 0)
            };
            match result {
                Ok(val) => extract_response_nv(&val),
                Err(e) => {
                    eprintln!("[server] Handler error: {}", e);
                    (500, Vec::new(), "Internal Server Error".to_string())
                }
            }
        }
        None => (404, Vec::new(), "Not Found".to_string()),
    };

    let mut response =
        tiny_http::Response::from_string(&body).with_status_code(tiny_http::StatusCode(status));
    for (name, value) in &resp_headers {
        if let Ok(header) = tiny_http::Header::from_bytes(name.as_bytes(), value.as_bytes()) {
            response = response.with_header(header);
        }
    }
    let _ = request.respond(response);
}

// ---------------------------------------------------------------------------
// Sendable types — thread-safe representations for cross-thread transfer
// ---------------------------------------------------------------------------

use super::radix::{RadixNode, RadixTree, SmallParams};
use super::sendable::{SendableHandler, SendableValue};

/// Thread-safe route tree node.
struct SendableNode {
    children: HashMap<String, SendableNode>,
    param: Option<(String, Box<SendableNode>)>,
    handlers: HashMap<String, SendableHandler>,
}

/// Thread-safe route tree built from NValue routes.
struct SendableRouteTree {
    root: SendableNode,
}

// SAFETY: SendableNode/SendableRouteTree contain only owned Strings and SendableHandlers.
unsafe impl Send for SendableRouteTree {}
unsafe impl Sync for SendableRouteTree {}

impl SendableNode {
    fn new() -> Self {
        SendableNode {
            children: HashMap::new(),
            param: None,
            handlers: HashMap::new(),
        }
    }

    fn to_nv_node(&self) -> RadixNode<NValue> {
        let mut node = RadixNode::default();
        for (seg, child) in &self.children {
            node.children.insert(seg.clone(), child.to_nv_node());
        }
        if let Some((name, child)) = &self.param {
            node.param = Some((name.clone(), Box::new(child.to_nv_node())));
        }
        for (method, handler) in &self.handlers {
            node.handlers.insert(method.clone(), handler.to_nvalue());
        }
        node
    }
}

impl SendableRouteTree {
    fn from_nv_routes(routes: &[NValue]) -> Self {
        let mut tree = SendableRouteTree {
            root: SendableNode::new(),
        };
        for route in routes {
            if let Some(fields) = route.as_record() {
                let method = fields
                    .iter()
                    .find(|(k, _)| &**k == "method")
                    .and_then(|(_, v)| v.as_string())
                    .map(|s| s.to_string());
                let path = fields
                    .iter()
                    .find(|(k, _)| &**k == "path")
                    .and_then(|(_, v)| v.as_string())
                    .map(|s| s.to_string());
                let handler = fields
                    .iter()
                    .find(|(k, _)| &**k == "handler")
                    .and_then(|(_, v)| SendableHandler::from_nvalue(v));

                if let (Some(m), Some(p), Some(h)) = (method, path, handler) {
                    tree.insert(&m, &p, h);
                }
            }
        }
        tree
    }

    fn insert(&mut self, method: &str, path: &str, handler: SendableHandler) {
        let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
        let mut node = &mut self.root;
        for seg in &segments {
            if let Some(name) = seg.strip_prefix(':') {
                if node.param.is_none() {
                    node.param = Some((name.to_string(), Box::new(SendableNode::new())));
                }
                node = node.param.as_mut().unwrap().1.as_mut();
            } else {
                node = node
                    .children
                    .entry(seg.to_string())
                    .or_insert_with(SendableNode::new);
            }
        }
        node.handlers.entry(method.to_string()).or_insert(handler);
    }

    fn to_nv_radix_tree(&self) -> NvRadixTree {
        NvRadixTree {
            root: self.root.to_nv_node(),
        }
    }
}

/// Thread-safe chunk representation using owned Strings instead of Rc<str>.
struct SendableChunks {
    chunks: Vec<SendableChunk>,
}

struct SendableChunk {
    code: Vec<Op>,
    constants: Vec<SendableValue>,
    source_map: Vec<(usize, usize)>,
}

// SAFETY: SendableChunks contains only owned data (Vec<Op>, Vec<SendableValue>).
unsafe impl Send for SendableChunks {}
unsafe impl Sync for SendableChunks {}

impl SendableChunks {
    fn from_chunks(chunks: &[Chunk]) -> Self {
        SendableChunks {
            chunks: chunks
                .iter()
                .map(|c| SendableChunk {
                    code: c.code.clone(),
                    constants: c
                        .constants
                        .iter()
                        .map(|v| SendableValue::from_nvalue(v))
                        .collect(),
                    source_map: c.source_map.clone(),
                })
                .collect(),
        }
    }

    fn to_chunks(&self) -> Vec<Chunk> {
        self.chunks
            .iter()
            .map(|sc| {
                let constants: Vec<NValue> = sc
                    .constants
                    .iter()
                    .map(|sv| sv.to_nvalue())
                    .collect();
                Chunk::from_parts(sc.code.clone(), constants, sc.source_map.clone())
            })
            .collect()
    }
}

// NvRadixTree is now RadixTree<NValue> from the radix module.
// Type alias for convenience.
type NvRadixTree = RadixTree<NValue>;

#[allow(dead_code)]
fn nv_radix_from_routes(routes: &[NValue]) -> NvRadixTree {
    let mut tree = NvRadixTree::new();
    for route in routes {
        if let Some(fields) = route.as_record() {
            let method = fields
                .iter()
                .find(|(k, _)| &**k == "method")
                .and_then(|(_, v)| v.as_string())
                .map(|s| s.to_string());
            let path = fields
                .iter()
                .find(|(k, _)| &**k == "path")
                .and_then(|(_, v)| v.as_string())
                .map(|s| s.to_string());
            let handler = fields
                .iter()
                .find(|(k, _)| &**k == "handler")
                .map(|(_, v)| v.clone());

            if let (Some(m), Some(p), Some(h)) = (method, path, handler) {
                tree.insert(&m, &p, h);
            }
        }
    }
    tree
}

// ---------------------------------------------------------------------------
// Server helpers for NValue-based request/response
// ---------------------------------------------------------------------------

fn parse_url_query_nv(url: &str) -> (String, NValue) {
    let (path, query_str) = match url.split_once('?') {
        Some((p, q)) => (p.to_string(), q),
        None => (url.to_string(), ""),
    };

    let mut query_fields: Vec<(RcStr, NValue)> = Vec::new();
    if !query_str.is_empty() {
        for pair in query_str.split('&') {
            if pair.is_empty() {
                continue;
            }
            let (key, value) = match pair.split_once('=') {
                Some((k, v)) => (k.to_string(), v.to_string()),
                None => (pair.to_string(), String::new()),
            };
            query_fields.push((RcStr::from(key.as_str()), NValue::string(value.into())));
        }
    }

    (path, NValue::record(query_fields))
}

fn inject_params_nv(req: &NValue, params: &[(String, String)]) -> NValue {
    let fields = match req.as_record() {
        Some(f) => f,
        None => return req.clone(),
    };
    let param_fields: Vec<(RcStr, NValue)> = params
        .iter()
        .map(|(k, v)| {
            (
                RcStr::from(k.as_str()),
                NValue::string(RcStr::from(v.as_str())),
            )
        })
        .collect();

    let mut new_fields: Vec<(RcStr, NValue)> = fields.clone();
    for (k, v) in &mut new_fields {
        if &**k == "params" {
            *v = NValue::record(param_fields.clone());
            return NValue::record(new_fields);
        }
    }
    new_fields.push(("params".into(), NValue::record(param_fields)));
    NValue::record(new_fields)
}

fn extract_response_nv(value: &NValue) -> (u16, Vec<(String, String)>, String) {
    if value.is_heap() {
        match value.as_heap_ref() {
            HeapObject::Enum { tag, payload, .. } if &**tag == "Ok" => {
                return extract_response_nv(payload);
            }
            HeapObject::Enum { tag, payload, .. } if &**tag == "Err" => {
                return (500, Vec::new(), format!("{}", payload));
            }
            _ => {}
        }
    }

    if let Some(fields) = value.as_record() {
        let has_status = fields.iter().any(|(k, _)| &**k == "status");
        if has_status {
            let status = fields
                .iter()
                .find(|(k, _)| &**k == "status")
                .and_then(|(_, v)| {
                    if v.is_any_int() {
                        Some(v.as_any_int() as u16)
                    } else {
                        None
                    }
                })
                .unwrap_or(200);

            let body = fields
                .iter()
                .find(|(k, _)| &**k == "body")
                .map(|(_, v)| match v.as_string() {
                    Some(s) => s.to_string(),
                    None => format!("{}", v),
                })
                .unwrap_or_default();

            let headers = fields
                .iter()
                .find(|(k, _)| &**k == "headers")
                .and_then(|(_, v)| v.as_list())
                .map(|items| {
                    items
                        .iter()
                        .filter_map(|item| {
                            if item.is_heap() {
                                if let HeapObject::Tuple(pair) = item.as_heap_ref() {
                                    if pair.len() == 2 {
                                        let k = pair[0].as_string()?.to_string();
                                        let v = pair[1].as_string()?.to_string();
                                        return Some((k, v));
                                    }
                                }
                            }
                            None
                        })
                        .collect()
                })
                .unwrap_or_default();

            return (status, headers, body);
        }
    }

    if let Some(s) = value.as_string() {
        return (200, Vec::new(), s.to_string());
    }

    (200, Vec::new(), format!("{}", value))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn run_chunk(chunk: Chunk) -> Value {
        let mut vm = Vm::new();
        vm.execute(&chunk).expect("VM error")
    }

    fn run_chunk_err(chunk: Chunk) -> CompileError {
        let mut vm = Vm::new();
        vm.execute(&chunk).expect_err("Expected VM error")
    }

    // -- Arithmetic --

    #[test]
    fn add_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(3));
        let b = c.add_constant(NValue::int(4));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Add, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(7));
    }

    #[test]
    fn sub_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(10));
        let b = c.add_constant(NValue::int(3));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Sub, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(7));
    }

    #[test]
    fn mul_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(6));
        let b = c.add_constant(NValue::int(7));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Mul, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(42));
    }

    #[test]
    fn div_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(15));
        let b = c.add_constant(NValue::int(3));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Div, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(5));
    }

    #[test]
    fn div_by_zero() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(1));
        let b = c.add_constant(NValue::int(0));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Div, 1, 0);
        c.emit(Op::Return, 1, 0);
        let err = run_chunk_err(c);
        assert!(err.message.contains("Division by zero"));
    }

    #[test]
    fn modulo() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(10));
        let b = c.add_constant(NValue::int(3));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Mod, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(1));
    }

    #[test]
    fn negate_int() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Negate, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(-5));
    }

    #[test]
    fn negate_float() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::float(3.125));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Negate, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Float(-3.125));
    }

    // -- Mixed int/float --

    #[test]
    fn add_int_float() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(1));
        let b = c.add_constant(NValue::float(2.5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Add, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Float(3.5));
    }

    // -- Comparisons --

    #[test]
    fn eq_same() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(5));
        let b = c.add_constant(NValue::int(5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Eq, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(true));
    }

    #[test]
    fn lt_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(3));
        let b = c.add_constant(NValue::int(5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Lt, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(true));
    }

    #[test]
    fn not_true() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::bool(true));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Not, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(false));
    }

    // -- String concat --

    #[test]
    fn concat_strings() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::string("hello ".into()));
        let b = c.add_constant(NValue::string("world".into()));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Concat, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::String("hello world".into()));
    }

    #[test]
    fn concat_int_to_string() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::string("count: ".into()));
        let b = c.add_constant(NValue::int(42));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Concat, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::String("count: 42".into()));
    }

    // -- Jumps --

    #[test]
    fn jump_unconditional() {
        let mut c = Chunk::new();
        let a = c.add_constant(NValue::int(1));
        let b = c.add_constant(NValue::int(99));
        c.emit(Op::Jump(1), 1, 0);
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(99));
    }

    #[test]
    fn jump_if_false_taken() {
        let mut c = Chunk::new();
        let f = c.add_constant(NValue::bool(false));
        let a = c.add_constant(NValue::int(1));
        let b = c.add_constant(NValue::int(2));
        c.emit(Op::LoadConst(f), 1, 0);
        c.emit(Op::JumpIfFalse(1), 1, 0);
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(2));
    }

    #[test]
    fn jump_if_false_not_taken() {
        let mut c = Chunk::new();
        let t = c.add_constant(NValue::bool(true));
        let a = c.add_constant(NValue::int(1));
        c.emit(Op::LoadConst(t), 1, 0);
        c.emit(Op::JumpIfFalse(1), 1, 0);
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(1));
    }

    // -- Compound expression: 1 + 2 * 3 = 7 --

    #[test]
    fn order_of_operations() {
        let mut c = Chunk::new();
        let one = c.add_constant(NValue::int(1));
        let two = c.add_constant(NValue::int(2));
        let three = c.add_constant(NValue::int(3));
        c.emit(Op::LoadConst(one), 1, 0);
        c.emit(Op::LoadConst(two), 1, 0);
        c.emit(Op::LoadConst(three), 1, 0);
        c.emit(Op::Mul, 1, 0);
        c.emit(Op::Add, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(7));
    }

    // -- Function calls --

    #[test]
    fn call_simple_function() {
        let mut caller = Chunk::new();
        let func = caller.add_constant(NValue::function(1));
        let arg = caller.add_constant(NValue::int(5));
        caller.emit(Op::LoadConst(func), 1, 0);
        caller.emit(Op::LoadConst(arg), 1, 0);
        caller.emit(Op::Call(1), 1, 0);
        caller.emit(Op::Return, 1, 0);

        let mut callee = Chunk::new();
        let two = callee.add_constant(NValue::int(2));
        callee.emit(Op::GetLocal(0), 1, 0);
        callee.emit(Op::LoadConst(two), 1, 0);
        callee.emit(Op::Mul, 1, 0);
        callee.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![caller, callee],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(10));
    }

    #[test]
    fn call_two_arg_function() {
        let mut caller = Chunk::new();
        let func = caller.add_constant(NValue::function(1));
        let a = caller.add_constant(NValue::int(3));
        let b = caller.add_constant(NValue::int(4));
        caller.emit(Op::LoadConst(func), 1, 0);
        caller.emit(Op::LoadConst(a), 1, 0);
        caller.emit(Op::LoadConst(b), 1, 0);
        caller.emit(Op::Call(2), 1, 0);
        caller.emit(Op::Return, 1, 0);

        let mut callee = Chunk::new();
        callee.emit(Op::GetLocal(0), 1, 0);
        callee.emit(Op::GetLocal(1), 1, 0);
        callee.emit(Op::Add, 1, 0);
        callee.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![caller, callee],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(7));
    }

    #[test]
    fn nested_calls() {
        let mut entry = Chunk::new();
        let add_fn = entry.add_constant(NValue::function(1));
        let dbl_fn = entry.add_constant(NValue::function(2));
        let three = entry.add_constant(NValue::int(3));
        let four = entry.add_constant(NValue::int(4));
        entry.emit(Op::LoadConst(add_fn), 1, 0);
        entry.emit(Op::LoadConst(dbl_fn), 1, 0);
        entry.emit(Op::LoadConst(three), 1, 0);
        entry.emit(Op::Call(1), 1, 0);
        entry.emit(Op::LoadConst(four), 1, 0);
        entry.emit(Op::Call(2), 1, 0);
        entry.emit(Op::Return, 1, 0);

        let mut add = Chunk::new();
        add.emit(Op::GetLocal(0), 1, 0);
        add.emit(Op::GetLocal(1), 1, 0);
        add.emit(Op::Add, 1, 0);
        add.emit(Op::Return, 1, 0);

        let mut dbl = Chunk::new();
        let two = dbl.add_constant(NValue::int(2));
        dbl.emit(Op::GetLocal(0), 1, 0);
        dbl.emit(Op::LoadConst(two), 1, 0);
        dbl.emit(Op::Mul, 1, 0);
        dbl.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![entry, add, dbl],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(10));
    }

    #[test]
    fn get_field_missing_returns_error() {
        let mut c = Chunk::new();
        let key = c.add_constant(NValue::string("x".into()));
        let val = c.add_constant(NValue::int(1));
        let bad_field = c.add_constant(NValue::string("y".into()));
        c.emit(Op::LoadConst(key), 1, 0);
        c.emit(Op::LoadConst(val), 1, 0);
        c.emit(Op::MakeRecord(1), 1, 0);
        c.emit(Op::GetField(bad_field), 1, 0);
        c.emit(Op::Return, 1, 0);
        let err = run_chunk_err(c);
        assert!(err.message.contains("no field 'y'"), "got: {}", err.message);
    }

    #[test]
    fn stack_overflow_detected() {
        let mut entry = Chunk::new();
        let func = entry.add_constant(NValue::function(1));
        entry.emit(Op::LoadConst(func), 1, 0);
        entry.emit(Op::Call(0), 1, 0);
        entry.emit(Op::Return, 1, 0);

        let mut recurse = Chunk::new();
        let self_func = recurse.add_constant(NValue::function(1));
        recurse.emit(Op::LoadConst(self_func), 1, 0);
        recurse.emit(Op::Call(0), 1, 0);
        recurse.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![entry, recurse],
            entry: 0,
        };
        let mut vm = Vm::new();
        let err = vm
            .execute_program(&program)
            .expect_err("Expected stack overflow");
        assert!(
            err.message.contains("Stack overflow"),
            "got: {}",
            err.message
        );
    }

    // -- Superinstruction tests --

    #[test]
    fn superinstruction_get_local_sub_int() {
        // Test that GetLocalSubInt produces the same result as the unfused sequence:
        // GetLocal(0) + LoadSmallInt(1) + SubInt
        let mut c_unfused = Chunk::new();
        let arg = c_unfused.add_constant(NValue::int(10));
        c_unfused.emit(Op::LoadConst(arg), 1, 0); // push 10 as local slot 0
        c_unfused.emit(Op::GetLocal(0), 1, 0);
        c_unfused.emit(Op::LoadSmallInt(1), 1, 0);
        c_unfused.emit(Op::SubInt, 1, 0);
        c_unfused.emit(Op::Return, 1, 0);
        let unfused_result = run_chunk(c_unfused);

        let mut c_fused = Chunk::new();
        let arg = c_fused.add_constant(NValue::int(10));
        c_fused.emit(Op::LoadConst(arg), 1, 0); // push 10 as local slot 0
        c_fused.emit(Op::GetLocalSubInt(0, 1), 1, 0);
        c_fused.emit(Op::Return, 1, 0);
        let fused_result = run_chunk(c_fused);

        assert_eq!(unfused_result, Value::Int(9));
        assert_eq!(fused_result, Value::Int(9));
        assert_eq!(unfused_result, fused_result);
    }

    #[test]
    fn superinstruction_get_local_sub_int_negative() {
        // Test subtraction with a negative small int (e.g., n - (-3) = n + 3)
        let mut c = Chunk::new();
        let arg = c.add_constant(NValue::int(5));
        c.emit(Op::LoadConst(arg), 1, 0);
        c.emit(Op::GetLocalSubInt(0, -3), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(8));
    }

    #[test]
    fn superinstruction_get_local_le_int() {
        // Test that GetLocalLeInt produces the same result as the unfused sequence:
        // GetLocal(0) + LoadSmallInt(1) + LeInt
        let mut c_unfused = Chunk::new();
        let arg = c_unfused.add_constant(NValue::int(0));
        c_unfused.emit(Op::LoadConst(arg), 1, 0);
        c_unfused.emit(Op::GetLocal(0), 1, 0);
        c_unfused.emit(Op::LoadSmallInt(1), 1, 0);
        c_unfused.emit(Op::LeInt, 1, 0);
        c_unfused.emit(Op::Return, 1, 0);
        let unfused_result = run_chunk(c_unfused);

        let mut c_fused = Chunk::new();
        let arg = c_fused.add_constant(NValue::int(0));
        c_fused.emit(Op::LoadConst(arg), 1, 0);
        c_fused.emit(Op::GetLocalLeInt(0, 1), 1, 0);
        c_fused.emit(Op::Return, 1, 0);
        let fused_result = run_chunk(c_fused);

        assert_eq!(unfused_result, Value::Bool(true));
        assert_eq!(fused_result, Value::Bool(true));
        assert_eq!(unfused_result, fused_result);
    }

    #[test]
    fn superinstruction_le_int_false_case() {
        // n = 5, check 5 <= 1 = false
        let mut c = Chunk::new();
        let arg = c.add_constant(NValue::int(5));
        c.emit(Op::LoadConst(arg), 1, 0);
        c.emit(Op::GetLocalLeInt(0, 1), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(false));
    }

    #[test]
    fn superinstruction_le_int_equal_case() {
        // n = 1, check 1 <= 1 = true
        let mut c = Chunk::new();
        let arg = c.add_constant(NValue::int(1));
        c.emit(Op::LoadConst(arg), 1, 0);
        c.emit(Op::GetLocalLeInt(0, 1), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(true));
    }

    #[test]
    fn superinstruction_in_function_call() {
        // Simulate fibonacci-like pattern: function with arg n,
        // compute n <= 1 and n - 1 using superinstructions
        let mut caller = Chunk::new();
        let func = caller.add_constant(NValue::function(1));
        let arg = caller.add_constant(NValue::int(5));
        caller.emit(Op::LoadConst(func), 1, 0);
        caller.emit(Op::LoadConst(arg), 1, 0);
        caller.emit(Op::Call(1), 1, 0);
        caller.emit(Op::Return, 1, 0);

        // Callee: takes n (slot 0), returns n - 2
        let mut callee = Chunk::new();
        callee.emit(Op::GetLocalSubInt(0, 2), 1, 0);
        callee.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![caller, callee],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(3));
    }

    #[test]
    fn superinstruction_fusion_produces_correct_result() {
        // Build a chunk with the unfused pattern, run fusion, verify same result
        let mut caller = Chunk::new();
        let func = caller.add_constant(NValue::function(1));
        let arg = caller.add_constant(NValue::int(7));
        caller.emit(Op::LoadConst(func), 1, 0);
        caller.emit(Op::LoadConst(arg), 1, 0);
        caller.emit(Op::Call(1), 1, 0);
        caller.emit(Op::Return, 1, 0);

        // Callee: GetLocal(0) + LoadSmallInt(3) + SubInt => should fuse
        let mut callee = Chunk::new();
        callee.emit(Op::GetLocal(0), 1, 0);
        callee.emit(Op::LoadSmallInt(3), 1, 0);
        callee.emit(Op::SubInt, 1, 0);
        callee.emit(Op::Return, 1, 0);

        let mut program = Program {
            chunks: vec![caller, callee],
            entry: 0,
        };
        program.optimize();

        // After optimization + compaction, callee chunk is [GetLocalSubInt, Return]
        assert_eq!(program.chunks[1].code[0], Op::GetLocalSubInt(0, 3));

        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(4));
    }

    // -- Tail call optimization tests --

    #[test]
    fn tail_call_factorial() {
        // fact(n, acc) = if n <= 1 then acc else fact(n-1, n*acc)
        // Entry: call fact(10, 1)
        let mut entry = Chunk::new();
        let func = entry.add_constant(NValue::function(1));
        let n = entry.add_constant(NValue::int(10));
        let acc = entry.add_constant(NValue::int(1));
        entry.emit(Op::LoadConst(func), 1, 0); // push function
        entry.emit(Op::LoadConst(n), 1, 0); // push 10
        entry.emit(Op::LoadConst(acc), 1, 0); // push 1
        entry.emit(Op::Call(2), 1, 0); // call fact(10, 1)
        entry.emit(Op::Return, 1, 0);

        // fact: slot 0 = n, slot 1 = acc
        // if n <= 1 then acc else TailCall(n-1, n*acc)
        let mut fact = Chunk::new();
        let one = fact.add_constant(NValue::int(1));
        // Check n <= 1
        fact.emit(Op::GetLocal(0), 2, 0); // push n
        fact.emit(Op::LoadConst(one), 2, 0); // push 1
        fact.emit(Op::Le, 2, 0); // n <= 1
        let then_jump = fact.emit(Op::JumpIfFalse(0), 2, 0);
        // Then: return acc
        fact.emit(Op::GetLocal(1), 3, 0); // push acc
        let else_jump = fact.emit(Op::Jump(0), 3, 0);
        // Else: TailCall(n-1, n*acc)
        fact.patch_jump(then_jump);
        fact.emit(Op::GetLocal(0), 4, 0); // push n
        fact.emit(Op::LoadConst(one), 4, 0); // push 1
        fact.emit(Op::Sub, 4, 0); // n - 1  (first arg)
        fact.emit(Op::GetLocal(0), 4, 0); // push n
        fact.emit(Op::GetLocal(1), 4, 0); // push acc
        fact.emit(Op::Mul, 4, 0); // n * acc  (second arg)
        fact.emit(Op::TailCall(2), 4, 0); // tail call with 2 args
        fact.patch_jump(else_jump);
        fact.emit(Op::Return, 5, 0);

        let program = Program {
            chunks: vec![entry, fact],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(3628800));
    }

    #[test]
    fn tail_call_deep_recursion() {
        // countdown(n) = if n <= 0 then 0 else countdown(n-1)
        // Tests that 100,000 iterations don't overflow the call stack
        let mut entry = Chunk::new();
        let func = entry.add_constant(NValue::function(1));
        let n = entry.add_constant(NValue::int(100_000));
        entry.emit(Op::LoadConst(func), 1, 0);
        entry.emit(Op::LoadConst(n), 1, 0);
        entry.emit(Op::Call(1), 1, 0);
        entry.emit(Op::Return, 1, 0);

        // countdown: slot 0 = n
        // if n <= 0 then 0 else TailCall(n-1)
        let mut countdown = Chunk::new();
        let zero = countdown.add_constant(NValue::int(0));
        let one_const = countdown.add_constant(NValue::int(1));
        // Check n <= 0
        countdown.emit(Op::GetLocal(0), 2, 0);
        countdown.emit(Op::LoadConst(zero), 2, 0);
        countdown.emit(Op::Le, 2, 0);
        let then_jump = countdown.emit(Op::JumpIfFalse(0), 2, 0);
        // Then: return 0
        countdown.emit(Op::LoadConst(zero), 3, 0);
        let else_jump = countdown.emit(Op::Jump(0), 3, 0);
        // Else: TailCall(n-1)
        countdown.patch_jump(then_jump);
        countdown.emit(Op::GetLocal(0), 4, 0);
        countdown.emit(Op::LoadConst(one_const), 4, 0);
        countdown.emit(Op::Sub, 4, 0);
        countdown.emit(Op::TailCall(1), 4, 0);
        countdown.patch_jump(else_jump);
        countdown.emit(Op::Return, 5, 0);

        let program = Program {
            chunks: vec![entry, countdown],
            entry: 0,
        };
        let mut vm = Vm::new();
        // Without TCO, 100,000 recursive calls would hit MAX_CALL_DEPTH (1024)
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(0));
    }

    #[test]
    fn superinstruction_get_local_add_int() {
        // Test that GetLocalAddInt produces same result as unfused sequence
        let mut c_unfused = Chunk::new();
        let arg = c_unfused.add_constant(NValue::int(10));
        c_unfused.emit(Op::LoadConst(arg), 1, 0);
        c_unfused.emit(Op::GetLocal(0), 1, 0);
        c_unfused.emit(Op::LoadSmallInt(3), 1, 0);
        c_unfused.emit(Op::AddInt, 1, 0);
        c_unfused.emit(Op::Return, 1, 0);
        let unfused = run_chunk(c_unfused);

        let mut c_fused = Chunk::new();
        let arg = c_fused.add_constant(NValue::int(10));
        c_fused.emit(Op::LoadConst(arg), 1, 0);
        c_fused.emit(Op::GetLocalAddInt(0, 3), 1, 0);
        c_fused.emit(Op::Return, 1, 0);
        let fused = run_chunk(c_fused);

        assert_eq!(unfused, Value::Int(13));
        assert_eq!(fused, Value::Int(13));
    }

    #[test]
    fn superinstruction_get_local_lt_int() {
        // Test that GetLocalLtInt produces same result as unfused sequence
        let mut c_unfused = Chunk::new();
        let arg = c_unfused.add_constant(NValue::int(3));
        c_unfused.emit(Op::LoadConst(arg), 1, 0);
        c_unfused.emit(Op::GetLocal(0), 1, 0);
        c_unfused.emit(Op::LoadSmallInt(5), 1, 0);
        c_unfused.emit(Op::LtInt, 1, 0);
        c_unfused.emit(Op::Return, 1, 0);
        let unfused = run_chunk(c_unfused);

        let mut c_fused = Chunk::new();
        let arg = c_fused.add_constant(NValue::int(3));
        c_fused.emit(Op::LoadConst(arg), 1, 0);
        c_fused.emit(Op::GetLocalLtInt(0, 5), 1, 0);
        c_fused.emit(Op::Return, 1, 0);
        let fused = run_chunk(c_fused);

        assert_eq!(unfused, Value::Bool(true));
        assert_eq!(fused, Value::Bool(true));
    }

    #[test]
    fn superinstruction_lt_int_false() {
        // 7 < 5 → false
        let mut c = Chunk::new();
        let arg = c.add_constant(NValue::int(7));
        c.emit(Op::LoadConst(arg), 1, 0);
        c.emit(Op::GetLocalLtInt(0, 5), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(false));
    }

    #[test]
    fn superinstruction_lt_int_equal() {
        // 5 < 5 → false (strictly less than)
        let mut c = Chunk::new();
        let arg = c.add_constant(NValue::int(5));
        c.emit(Op::LoadConst(arg), 1, 0);
        c.emit(Op::GetLocalLtInt(0, 5), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(false));
    }

    #[test]
    fn tail_call_sum_accumulator() {
        // sum(n, acc) = if n <= 0 then acc else sum(n-1, acc+n)
        // sum(100, 0) = 5050
        let mut entry = Chunk::new();
        let func = entry.add_constant(NValue::function(1));
        let n = entry.add_constant(NValue::int(100));
        let zero = entry.add_constant(NValue::int(0));
        entry.emit(Op::LoadConst(func), 1, 0);
        entry.emit(Op::LoadConst(n), 1, 0);
        entry.emit(Op::LoadConst(zero), 1, 0);
        entry.emit(Op::Call(2), 1, 0);
        entry.emit(Op::Return, 1, 0);

        // sum: slot 0 = n, slot 1 = acc
        let mut sum = Chunk::new();
        let zero_c = sum.add_constant(NValue::int(0));
        let one_c = sum.add_constant(NValue::int(1));
        // if n <= 0
        sum.emit(Op::GetLocal(0), 2, 0);
        sum.emit(Op::LoadConst(zero_c), 2, 0);
        sum.emit(Op::Le, 2, 0);
        let then_jump = sum.emit(Op::JumpIfFalse(0), 2, 0);
        // then acc
        sum.emit(Op::GetLocal(1), 3, 0);
        let else_jump = sum.emit(Op::Jump(0), 3, 0);
        // else TailCall(n-1, acc+n)
        sum.patch_jump(then_jump);
        sum.emit(Op::GetLocal(0), 4, 0);
        sum.emit(Op::LoadConst(one_c), 4, 0);
        sum.emit(Op::Sub, 4, 0); // n - 1
        sum.emit(Op::GetLocal(1), 4, 0);
        sum.emit(Op::GetLocal(0), 4, 0);
        sum.emit(Op::Add, 4, 0); // acc + n
        sum.emit(Op::TailCall(2), 4, 0);
        sum.patch_jump(else_jump);
        sum.emit(Op::Return, 5, 0);

        let program = Program {
            chunks: vec![entry, sum],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(5050));
    }

    // -- CallDirect tests --

    #[test]
    fn call_direct_simple() {
        // Entry calls callee via CallDirect (no function value on stack)
        let mut caller = Chunk::new();
        let arg = caller.add_constant(NValue::int(5));
        caller.emit(Op::LoadConst(arg), 1, 0);
        caller.emit(Op::CallDirect(1, 1), 1, 0);
        caller.emit(Op::Return, 1, 0);

        let mut callee = Chunk::new();
        let two = callee.add_constant(NValue::int(2));
        callee.emit(Op::GetLocal(0), 1, 0);
        callee.emit(Op::LoadConst(two), 1, 0);
        callee.emit(Op::Mul, 1, 0);
        callee.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![caller, callee],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(10));
    }

    #[test]
    fn call_direct_two_args() {
        let mut caller = Chunk::new();
        let a = caller.add_constant(NValue::int(3));
        let b = caller.add_constant(NValue::int(4));
        caller.emit(Op::LoadConst(a), 1, 0);
        caller.emit(Op::LoadConst(b), 1, 0);
        caller.emit(Op::CallDirect(1, 2), 1, 0);
        caller.emit(Op::Return, 1, 0);

        let mut callee = Chunk::new();
        callee.emit(Op::GetLocal(0), 1, 0);
        callee.emit(Op::GetLocal(1), 1, 0);
        callee.emit(Op::Add, 1, 0);
        callee.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![caller, callee],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(7));
    }

    #[test]
    fn call_direct_recursive_fib() {
        // fib(n) = if n <= 1 then n else fib(n-1) + fib(n-2)
        // Entry: CallDirect fib(10)
        let mut entry = Chunk::new();
        let arg = entry.add_constant(NValue::int(10));
        entry.emit(Op::LoadConst(arg), 1, 0);
        entry.emit(Op::CallDirect(1, 1), 1, 0);
        entry.emit(Op::Return, 1, 0);

        // fib: slot 0 = n
        let mut fib = Chunk::new();
        let one = fib.add_constant(NValue::int(1));
        let two = fib.add_constant(NValue::int(2));
        // n <= 1
        fib.emit(Op::GetLocal(0), 1, 0);
        fib.emit(Op::LoadConst(one), 1, 0);
        fib.emit(Op::Le, 1, 0);
        let then_jump = fib.emit(Op::JumpIfFalse(0), 1, 0);
        // then: return n
        fib.emit(Op::GetLocal(0), 1, 0);
        let else_jump = fib.emit(Op::Jump(0), 1, 0);
        // else: fib(n-1) + fib(n-2)
        fib.patch_jump(then_jump);
        fib.emit(Op::GetLocal(0), 1, 0);
        fib.emit(Op::LoadConst(one), 1, 0);
        fib.emit(Op::Sub, 1, 0); // n - 1
        fib.emit(Op::CallDirect(1, 1), 1, 0); // fib(n-1)
        fib.emit(Op::GetLocal(0), 1, 0);
        fib.emit(Op::LoadConst(two), 1, 0);
        fib.emit(Op::Sub, 1, 0); // n - 2
        fib.emit(Op::CallDirect(1, 1), 1, 0); // fib(n-2)
        fib.emit(Op::Add, 1, 0);
        fib.patch_jump(else_jump);
        fib.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![entry, fib],
            entry: 0,
        };
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(55));
    }

    #[test]
    fn call_direct_mixed_with_call() {
        // Test mixing CallDirect (known function) and Call (closure/indirect)
        let mut entry = Chunk::new();
        let func = entry.add_constant(NValue::function(2)); // indirect reference
        let arg = entry.add_constant(NValue::int(5));
        // Direct call to chunk 1
        entry.emit(Op::LoadConst(arg), 1, 0);
        entry.emit(Op::CallDirect(1, 1), 1, 0);
        // Indirect call to chunk 2 via Call
        entry.emit(Op::LoadConst(func), 1, 0);
        entry.emit(Op::Call(0), 1, 0); // Call with result from CallDirect as arg? No, Call(0) pops func, pushes result
        // Add both results  -- actually let's push result of direct call, then indirect call, then add
        entry.emit(Op::Add, 1, 0);
        entry.emit(Op::Return, 1, 0);

        // Chunk 1: double(n) = n * 2
        let mut double = Chunk::new();
        let two = double.add_constant(NValue::int(2));
        double.emit(Op::GetLocal(0), 1, 0);
        double.emit(Op::LoadConst(two), 1, 0);
        double.emit(Op::Mul, 1, 0);
        double.emit(Op::Return, 1, 0);

        // Chunk 2: returns 100
        let mut hundred = Chunk::new();
        let val = hundred.add_constant(NValue::int(100));
        hundred.emit(Op::LoadConst(val), 1, 0);
        hundred.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![entry, double, hundred],
            entry: 0,
        };
        let mut vm = Vm::new();
        // double(5) = 10, hundred() = 100, 10 + 100 = 110
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(110));
    }

    #[test]
    fn call_direct_stack_overflow() {
        // Ensure CallDirect still detects stack overflow
        let mut entry = Chunk::new();
        entry.emit(Op::CallDirect(1, 0), 1, 0);
        entry.emit(Op::Return, 1, 0);

        let mut recurse = Chunk::new();
        recurse.emit(Op::CallDirect(1, 0), 1, 0);
        recurse.emit(Op::Return, 1, 0);

        let program = Program {
            chunks: vec![entry, recurse],
            entry: 0,
        };
        let mut vm = Vm::new();
        let err = vm
            .execute_program(&program)
            .expect_err("Expected stack overflow");
        assert!(
            err.message.contains("Stack overflow"),
            "got: {}",
            err.message
        );
    }

    #[test]
    fn call_frame_size() {
        // AC-4.8: CallFrame must remain 16 bytes or smaller
        assert!(
            std::mem::size_of::<CallFrame>() <= 16,
            "CallFrame is {} bytes, must be <= 16",
            std::mem::size_of::<CallFrame>()
        );
    }
}
