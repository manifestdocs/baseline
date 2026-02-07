use std::rc::Rc;

use super::chunk::{Chunk, Op, Program};
use super::natives::NativeRegistry;
use super::nvalue::{HeapObject, NValue};
use super::value::Value;

// SAFETY note: The unsafe stack operations below (pop_unchecked, pop2_unchecked,
// get_unchecked) are sound because the compiler guarantees stack invariants:
// every pop has a matching push, and local slot indices are always valid.

const MAX_CALL_DEPTH: usize = 1024;
const MAX_RANGE_SIZE: i64 = 1_000_000;

// ---------------------------------------------------------------------------
// VM Error
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct VmError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}] {}", self.line, self.col, self.message)
    }
}

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
            let ptr = std::alloc::alloc_zeroed(layout) as *mut [CallFrame; MAX_CALL_DEPTH];
            Box::from_raw(ptr)
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

pub struct Vm {
    stack: Vec<NValue>,
    frames: FrameStack,
    upvalue_stack: Vec<Rc<Vec<NValue>>>,
    natives: NativeRegistry,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            stack: Vec::with_capacity(256),
            frames: FrameStack::new(),
            upvalue_stack: Vec::new(),
            natives: NativeRegistry::new(),
        }
    }

    /// Get a reference to the native function registry (for compiler lookups).
    pub fn natives(&self) -> &NativeRegistry {
        &self.natives
    }

    /// Execute a single chunk of bytecode (backward-compatible API).
    pub fn execute(&mut self, chunk: &Chunk) -> Result<Value, VmError> {
        self.stack.clear();
        self.frames.clear();
        self.upvalue_stack.clear();
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
    pub fn execute_program(&mut self, program: &Program) -> Result<Value, VmError> {
        self.stack.clear();
        self.frames.clear();
        self.upvalue_stack.clear();
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
    ) -> Result<Value, VmError> {
        self.stack.clear();
        self.frames.clear();
        self.upvalue_stack.clear();
        self.frames.push(CallFrame {
            chunk_idx: chunk_idx as u32,
            ip: 0,
            base_slot: 0,
            upvalue_idx: u32::MAX,
        });
        let result = self.run(chunks)?;
        Ok(result.to_value())
    }

    /// Main dispatch loop — shared by execute and execute_program.
    fn run(&mut self, chunks: &[Chunk]) -> Result<NValue, VmError> {
        self.run_frames(chunks, 0)
    }

    /// Dispatch loop that stops when frame count drops to `base_depth`.
    ///
    /// Performance: This is the hottest code in the VM. Key optimizations:
    /// - NaN-boxed values (8 bytes) instead of enum (24 bytes) for fast stack ops
    /// - Frame state (chunk_idx, ip, base_slot) kept in local variables
    /// - Source map only consulted in error paths
    /// - Unsafe stack access eliminates bounds checks
    fn run_frames(&mut self, chunks: &[Chunk], base_depth: usize) -> Result<NValue, VmError> {
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

        loop {
            let op = unsafe { *chunk.code.get_unchecked(ip) };
            ip += 1;

            match op {
                Op::LoadConst(idx) => {
                    let val = NValue::from_value(&chunk.constants[idx as usize]);
                    self.stack.push(val);
                }

                Op::LoadSmallInt(n) => {
                    self.stack.push(NValue::int(n as i64));
                }

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

                Op::Not => {
                    let v = self.pop_fast();
                    self.stack.push(NValue::bool(!v.is_truthy()));
                }

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
                        continue;
                    }
                    // Slow path: mixed types via Display
                    let result = NValue::string(format!("{}{}", a, b).into());
                    self.stack.push(result);
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
                            let idx = idx_val.as_any_int() as usize;
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
                    let count = n as usize;
                    let start = self.stack.len() - count;
                    let items: Vec<NValue> = self.stack.drain(start..).collect();
                    self.stack.push(NValue::list(items));
                }

                Op::MakeRecord(n) => {
                    let count = n as usize;
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
                    let field_name = match &chunk.constants[name_idx as usize] {
                        Value::String(s) => s.clone(),
                        _ => {
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
                    let count = n as usize;
                    let start = self.stack.len() - count;
                    let items: Vec<NValue> = self.stack.drain(start..).collect();
                    self.stack.push(NValue::tuple(items));
                }

                Op::TupleGet(idx) => {
                    let tuple = self.pop_fast();
                    if tuple.is_heap() {
                        if let HeapObject::Tuple(items) = tuple.as_heap_ref() {
                            let i = idx as usize;
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
                    let tag = match &chunk.constants[tag_idx as usize] {
                        Value::String(s) => s.clone(),
                        _ => {
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
                    let tag = match &chunk.constants[tag_idx as usize] {
                        Value::String(s) => s.clone(),
                        _ => {
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
                    let count = n as usize;
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

                // -- Functions --
                Op::Call(arg_count) => {
                    if self.frames.len() >= MAX_CALL_DEPTH {
                        let (line, col) = chunk.source_map[ip - 1];
                        return Err(self.error(
                            "Stack overflow: maximum call depth exceeded".into(),
                            line,
                            col,
                        ));
                    }
                    let n = arg_count as usize;
                    let func_pos = self.stack.len() - n - 1;
                    let func = unsafe { self.stack.get_unchecked(func_pos) };

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

                    // Save current frame's ip and chunk_idx (base_slot is already saved)
                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;

                    let new_base = (func_pos + 1) as u32;
                    self.frames.push(CallFrame {
                        chunk_idx: new_chunk_idx as u32,
                        ip: 0,
                        // Bit 31 set: this frame has a function value at base_slot - 1
                        base_slot: new_base | FRAME_HAS_FUNC,
                        upvalue_idx: new_upvalue_idx,
                    });
                    ip = 0;
                    chunk_idx = new_chunk_idx;
                    base_slot = new_base as usize;
                    chunk = &chunks[chunk_idx];
                    debug_assert!(chunk.code.last() == Some(&Op::Return));
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
                    let n = arg_count as usize;
                    let new_base = (self.stack.len() - n) as u32;

                    // Save current frame's ip and chunk_idx (base_slot is already saved)
                    let caller = self.frames.last_mut();
                    caller.ip = ip as u32;
                    caller.chunk_idx = chunk_idx as u32;

                    self.frames.push(CallFrame {
                        chunk_idx: target_chunk as u32,
                        ip: 0,
                        // Bit 31 clear: no function value slot
                        base_slot: new_base,
                        upvalue_idx: u32::MAX,
                    });
                    ip = 0;
                    chunk_idx = target_chunk as usize;
                    base_slot = new_base as usize;
                    chunk = &chunks[chunk_idx];
                    debug_assert!(chunk.code.last() == Some(&Op::Return));
                }

                Op::GetUpvalue(idx) => {
                    let frame = self.frames.last();
                    let uv_idx = frame.upvalue_idx as usize;
                    let val = self.upvalue_stack[uv_idx][idx as usize].clone();
                    self.stack.push(val);
                }

                Op::MakeClosure(new_chunk_idx, upvalue_count) => {
                    let n = upvalue_count as usize;
                    let start = self.stack.len() - n;
                    let upvalues: Vec<NValue> = self.stack.drain(start..).collect();
                    self.stack
                        .push(NValue::closure(new_chunk_idx as usize, upvalues));
                }

                Op::CallNative(fn_id, arg_count) => {
                    let n = arg_count as usize;
                    if self.natives.is_hof(fn_id) {
                        let caller = self.frames.last_mut();
                        caller.ip = ip as u32;
                        caller.chunk_idx = chunk_idx as u32;
                        let (line, col) = chunk.source_map[ip - 1];
                        self.dispatch_hof(fn_id, n, chunks, line, col)?;
                    } else {
                        let start = self.stack.len() - n;
                        let result = self.natives.call(fn_id, &self.stack[start..]);
                        let result = result.map_err(|e| {
                            let (line, col) = chunk.source_map[ip - 1];
                            self.error(format!("{}: {}", self.natives.name(fn_id), e.0), line, col)
                        })?;
                        self.stack.truncate(start);
                        self.stack.push(result);
                    }
                }

                // -- Superinstructions --
                Op::GetLocalSubInt(slot, k) => {
                    let idx = base_slot + slot as usize;
                    debug_assert!(idx < self.stack.len());
                    let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                    self.stack.push(NValue::int(val.wrapping_sub(k as i64)));
                }

                Op::GetLocalLeInt(slot, k) => {
                    let idx = base_slot + slot as usize;
                    debug_assert!(idx < self.stack.len());
                    let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                    self.stack.push(NValue::bool(val <= k as i64));
                }

                Op::GetLocalAddInt(slot, k) => {
                    let idx = base_slot + slot as usize;
                    debug_assert!(idx < self.stack.len());
                    let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                    self.stack.push(NValue::int(val.wrapping_add(k as i64)));
                }

                Op::GetLocalLtInt(slot, k) => {
                    let idx = base_slot + slot as usize;
                    debug_assert!(idx < self.stack.len());
                    let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                    self.stack.push(NValue::bool(val < k as i64));
                }

                Op::GetLocalLeIntJumpIfFalse(slot, k, offset) => {
                    let idx = base_slot + slot as usize;
                    debug_assert!(idx < self.stack.len());
                    let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                    if val > k as i64 {
                        ip += offset as usize;
                    }
                }

                Op::GetLocalLtIntJumpIfFalse(slot, k, offset) => {
                    let idx = base_slot + slot as usize;
                    debug_assert!(idx < self.stack.len());
                    let val = unsafe { self.stack.get_unchecked(idx) }.as_any_int();
                    if val >= k as i64 {
                        ip += offset as usize;
                    }
                }

                Op::TailCall(arg_count) => {
                    let n = arg_count as usize;
                    // Copy new args from the top of the stack over the current
                    // frame's argument slots. base_slot points to arg0 (slot 0).
                    // The function value sits at base_slot - 1 (caller's slot).
                    let args_start = self.stack.len() - n;
                    for i in 0..n {
                        let val = unsafe { self.stack.get_unchecked(args_start + i) }.clone();
                        unsafe {
                            *self.stack.get_unchecked_mut(base_slot + i) = val;
                        }
                    }
                    // Truncate the stack: keep only base_slot + n (the args)
                    self.stack.truncate(base_slot + n);
                    // Reset instruction pointer to beginning of current chunk
                    ip = 0;
                    continue;
                }

                Op::Return => {
                    let result = self.stack.pop().unwrap_or_else(NValue::unit);
                    let frame = self.frames.pop();

                    // Clean up upvalue_stack entry if this was a closure frame
                    if frame.upvalue_idx != u32::MAX {
                        // Pop from upvalue_stack if this was the last pushed entry
                        // (closures are always pushed/popped in stack order)
                        if frame.upvalue_idx as usize == self.upvalue_stack.len() - 1 {
                            self.upvalue_stack.pop();
                        }
                    }

                    if self.frames.len() <= base_depth {
                        return Ok(result);
                    }

                    // Remove callee's locals + args (+ function value if Call frame)
                    let raw_base = frame.base_slot;
                    let has_func = raw_base & FRAME_HAS_FUNC != 0;
                    let frame_base = (raw_base & !FRAME_HAS_FUNC) as usize;
                    if has_func {
                        self.stack.truncate(frame_base - 1);
                    } else {
                        self.stack.truncate(frame_base);
                    }
                    self.stack.push(result);

                    // Restore caller's frame state
                    let caller = self.frames.last();
                    ip = caller.ip as usize;
                    chunk_idx = caller.chunk_idx as usize;
                    base_slot = (caller.base_slot & !FRAME_HAS_FUNC) as usize;
                    chunk = &chunks[chunk_idx];
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

    fn pop(&mut self, line: usize, col: usize) -> Result<NValue, VmError> {
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
    ) -> Result<(), VmError> {
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
                    HeapObject::Enum { tag, payload } if &**tag == "Some" => {
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
                    HeapObject::Enum { tag, payload } if &**tag == "Ok" => {
                        let result = self.call_nvalue(
                            &func,
                            std::slice::from_ref(payload),
                            chunks,
                            line,
                            col,
                        )?;
                        self.stack.push(NValue::enum_val("Ok".into(), result));
                    }
                    HeapObject::Enum { tag, payload } if &**tag == "Err" => {
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
    fn call_nvalue(
        &mut self,
        func: &NValue,
        args: &[NValue],
        chunks: &[Chunk],
        line: usize,
        col: usize,
    ) -> Result<NValue, VmError> {
        if self.frames.len() >= MAX_CALL_DEPTH {
            return Err(self.error(
                "Stack overflow: maximum call depth exceeded".into(),
                line,
                col,
            ));
        }
        let base_depth = self.frames.len();
        let stack_base = self.stack.len();

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

    fn error(&self, message: String, line: usize, col: usize) -> VmError {
        VmError { message, line, col }
    }
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

    fn run_chunk_err(chunk: Chunk) -> VmError {
        let mut vm = Vm::new();
        vm.execute(&chunk).expect_err("Expected VM error")
    }

    // -- Arithmetic --

    #[test]
    fn add_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(3));
        let b = c.add_constant(Value::Int(4));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Add, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(7));
    }

    #[test]
    fn sub_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(10));
        let b = c.add_constant(Value::Int(3));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Sub, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(7));
    }

    #[test]
    fn mul_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(6));
        let b = c.add_constant(Value::Int(7));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Mul, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(42));
    }

    #[test]
    fn div_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(15));
        let b = c.add_constant(Value::Int(3));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Div, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(5));
    }

    #[test]
    fn div_by_zero() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(1));
        let b = c.add_constant(Value::Int(0));
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
        let a = c.add_constant(Value::Int(10));
        let b = c.add_constant(Value::Int(3));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Mod, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(1));
    }

    #[test]
    fn negate_int() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Negate, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(-5));
    }

    #[test]
    fn negate_float() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Float(3.125));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Negate, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Float(-3.125));
    }

    // -- Mixed int/float --

    #[test]
    fn add_int_float() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(1));
        let b = c.add_constant(Value::Float(2.5));
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
        let a = c.add_constant(Value::Int(5));
        let b = c.add_constant(Value::Int(5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Eq, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(true));
    }

    #[test]
    fn lt_integers() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Int(3));
        let b = c.add_constant(Value::Int(5));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Lt, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(true));
    }

    #[test]
    fn not_true() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::Bool(true));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::Not, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(false));
    }

    // -- String concat --

    #[test]
    fn concat_strings() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::String("hello ".into()));
        let b = c.add_constant(Value::String("world".into()));
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Concat, 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::String("hello world".into()));
    }

    #[test]
    fn concat_int_to_string() {
        let mut c = Chunk::new();
        let a = c.add_constant(Value::String("count: ".into()));
        let b = c.add_constant(Value::Int(42));
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
        let a = c.add_constant(Value::Int(1));
        let b = c.add_constant(Value::Int(99));
        c.emit(Op::Jump(1), 1, 0);
        c.emit(Op::LoadConst(a), 1, 0);
        c.emit(Op::LoadConst(b), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Int(99));
    }

    #[test]
    fn jump_if_false_taken() {
        let mut c = Chunk::new();
        let f = c.add_constant(Value::Bool(false));
        let a = c.add_constant(Value::Int(1));
        let b = c.add_constant(Value::Int(2));
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
        let t = c.add_constant(Value::Bool(true));
        let a = c.add_constant(Value::Int(1));
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
        let one = c.add_constant(Value::Int(1));
        let two = c.add_constant(Value::Int(2));
        let three = c.add_constant(Value::Int(3));
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
        let func = caller.add_constant(Value::Function(1));
        let arg = caller.add_constant(Value::Int(5));
        caller.emit(Op::LoadConst(func), 1, 0);
        caller.emit(Op::LoadConst(arg), 1, 0);
        caller.emit(Op::Call(1), 1, 0);
        caller.emit(Op::Return, 1, 0);

        let mut callee = Chunk::new();
        let two = callee.add_constant(Value::Int(2));
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
        let func = caller.add_constant(Value::Function(1));
        let a = caller.add_constant(Value::Int(3));
        let b = caller.add_constant(Value::Int(4));
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
        let add_fn = entry.add_constant(Value::Function(1));
        let dbl_fn = entry.add_constant(Value::Function(2));
        let three = entry.add_constant(Value::Int(3));
        let four = entry.add_constant(Value::Int(4));
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
        let two = dbl.add_constant(Value::Int(2));
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
        let key = c.add_constant(Value::String("x".into()));
        let val = c.add_constant(Value::Int(1));
        let bad_field = c.add_constant(Value::String("y".into()));
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
        let func = entry.add_constant(Value::Function(1));
        entry.emit(Op::LoadConst(func), 1, 0);
        entry.emit(Op::Call(0), 1, 0);
        entry.emit(Op::Return, 1, 0);

        let mut recurse = Chunk::new();
        let self_func = recurse.add_constant(Value::Function(1));
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
        let arg = c_unfused.add_constant(Value::Int(10));
        c_unfused.emit(Op::LoadConst(arg), 1, 0); // push 10 as local slot 0
        c_unfused.emit(Op::GetLocal(0), 1, 0);
        c_unfused.emit(Op::LoadSmallInt(1), 1, 0);
        c_unfused.emit(Op::SubInt, 1, 0);
        c_unfused.emit(Op::Return, 1, 0);
        let unfused_result = run_chunk(c_unfused);

        let mut c_fused = Chunk::new();
        let arg = c_fused.add_constant(Value::Int(10));
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
        let arg = c.add_constant(Value::Int(5));
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
        let arg = c_unfused.add_constant(Value::Int(0));
        c_unfused.emit(Op::LoadConst(arg), 1, 0);
        c_unfused.emit(Op::GetLocal(0), 1, 0);
        c_unfused.emit(Op::LoadSmallInt(1), 1, 0);
        c_unfused.emit(Op::LeInt, 1, 0);
        c_unfused.emit(Op::Return, 1, 0);
        let unfused_result = run_chunk(c_unfused);

        let mut c_fused = Chunk::new();
        let arg = c_fused.add_constant(Value::Int(0));
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
        let arg = c.add_constant(Value::Int(5));
        c.emit(Op::LoadConst(arg), 1, 0);
        c.emit(Op::GetLocalLeInt(0, 1), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(false));
    }

    #[test]
    fn superinstruction_le_int_equal_case() {
        // n = 1, check 1 <= 1 = true
        let mut c = Chunk::new();
        let arg = c.add_constant(Value::Int(1));
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
        let func = caller.add_constant(Value::Function(1));
        let arg = caller.add_constant(Value::Int(5));
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
        let func = caller.add_constant(Value::Function(1));
        let arg = caller.add_constant(Value::Int(7));
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
        let func = entry.add_constant(Value::Function(1));
        let n = entry.add_constant(Value::Int(10));
        let acc = entry.add_constant(Value::Int(1));
        entry.emit(Op::LoadConst(func), 1, 0); // push function
        entry.emit(Op::LoadConst(n), 1, 0); // push 10
        entry.emit(Op::LoadConst(acc), 1, 0); // push 1
        entry.emit(Op::Call(2), 1, 0); // call fact(10, 1)
        entry.emit(Op::Return, 1, 0);

        // fact: slot 0 = n, slot 1 = acc
        // if n <= 1 then acc else TailCall(n-1, n*acc)
        let mut fact = Chunk::new();
        let one = fact.add_constant(Value::Int(1));
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
        let func = entry.add_constant(Value::Function(1));
        let n = entry.add_constant(Value::Int(100_000));
        entry.emit(Op::LoadConst(func), 1, 0);
        entry.emit(Op::LoadConst(n), 1, 0);
        entry.emit(Op::Call(1), 1, 0);
        entry.emit(Op::Return, 1, 0);

        // countdown: slot 0 = n
        // if n <= 0 then 0 else TailCall(n-1)
        let mut countdown = Chunk::new();
        let zero = countdown.add_constant(Value::Int(0));
        let one_const = countdown.add_constant(Value::Int(1));
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
        let arg = c_unfused.add_constant(Value::Int(10));
        c_unfused.emit(Op::LoadConst(arg), 1, 0);
        c_unfused.emit(Op::GetLocal(0), 1, 0);
        c_unfused.emit(Op::LoadSmallInt(3), 1, 0);
        c_unfused.emit(Op::AddInt, 1, 0);
        c_unfused.emit(Op::Return, 1, 0);
        let unfused = run_chunk(c_unfused);

        let mut c_fused = Chunk::new();
        let arg = c_fused.add_constant(Value::Int(10));
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
        let arg = c_unfused.add_constant(Value::Int(3));
        c_unfused.emit(Op::LoadConst(arg), 1, 0);
        c_unfused.emit(Op::GetLocal(0), 1, 0);
        c_unfused.emit(Op::LoadSmallInt(5), 1, 0);
        c_unfused.emit(Op::LtInt, 1, 0);
        c_unfused.emit(Op::Return, 1, 0);
        let unfused = run_chunk(c_unfused);

        let mut c_fused = Chunk::new();
        let arg = c_fused.add_constant(Value::Int(3));
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
        let arg = c.add_constant(Value::Int(7));
        c.emit(Op::LoadConst(arg), 1, 0);
        c.emit(Op::GetLocalLtInt(0, 5), 1, 0);
        c.emit(Op::Return, 1, 0);
        assert_eq!(run_chunk(c), Value::Bool(false));
    }

    #[test]
    fn superinstruction_lt_int_equal() {
        // 5 < 5 → false (strictly less than)
        let mut c = Chunk::new();
        let arg = c.add_constant(Value::Int(5));
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
        let func = entry.add_constant(Value::Function(1));
        let n = entry.add_constant(Value::Int(100));
        let zero = entry.add_constant(Value::Int(0));
        entry.emit(Op::LoadConst(func), 1, 0);
        entry.emit(Op::LoadConst(n), 1, 0);
        entry.emit(Op::LoadConst(zero), 1, 0);
        entry.emit(Op::Call(2), 1, 0);
        entry.emit(Op::Return, 1, 0);

        // sum: slot 0 = n, slot 1 = acc
        let mut sum = Chunk::new();
        let zero_c = sum.add_constant(Value::Int(0));
        let one_c = sum.add_constant(Value::Int(1));
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
        let arg = caller.add_constant(Value::Int(5));
        caller.emit(Op::LoadConst(arg), 1, 0);
        caller.emit(Op::CallDirect(1, 1), 1, 0);
        caller.emit(Op::Return, 1, 0);

        let mut callee = Chunk::new();
        let two = callee.add_constant(Value::Int(2));
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
        let a = caller.add_constant(Value::Int(3));
        let b = caller.add_constant(Value::Int(4));
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
        let arg = entry.add_constant(Value::Int(10));
        entry.emit(Op::LoadConst(arg), 1, 0);
        entry.emit(Op::CallDirect(1, 1), 1, 0);
        entry.emit(Op::Return, 1, 0);

        // fib: slot 0 = n
        let mut fib = Chunk::new();
        let one = fib.add_constant(Value::Int(1));
        let two = fib.add_constant(Value::Int(2));
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
        let func = entry.add_constant(Value::Function(2)); // indirect reference
        let arg = entry.add_constant(Value::Int(5));
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
        let two = double.add_constant(Value::Int(2));
        double.emit(Op::GetLocal(0), 1, 0);
        double.emit(Op::LoadConst(two), 1, 0);
        double.emit(Op::Mul, 1, 0);
        double.emit(Op::Return, 1, 0);

        // Chunk 2: returns 100
        let mut hundred = Chunk::new();
        let val = hundred.add_constant(Value::Int(100));
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
