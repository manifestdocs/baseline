use super::value::Value;

// ---------------------------------------------------------------------------
// Opcodes
// ---------------------------------------------------------------------------

/// Bytecode instructions for the stack-based VM.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    /// Push a constant from the pool onto the stack.
    LoadConst(u16),
    /// Push a small integer directly (no constant pool lookup).
    LoadSmallInt(i16),

    // -- Arithmetic --
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Negate,

    // -- Type-specialized arithmetic (skip runtime type checks) --
    AddInt,
    SubInt,

    // -- Comparison --
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,

    // -- Type-specialized comparison --
    LtInt,
    LeInt,

    // -- Logic --
    Not,

    // -- String --
    Concat,

    // -- Variables --
    /// Push the value of a local variable (by slot index) onto the stack.
    GetLocal(u16),
    /// Set a local variable slot to the value on top of stack (does NOT pop).
    SetLocal(u16),
    /// Pop n values from the stack (for scope cleanup).
    PopN(u16),
    /// Remove n values from UNDER the top of stack (scope cleanup preserving result).
    /// Stack before: [... locals(n) result] → Stack after: [... result]
    CloseScope(u16),

    // -- Stack manipulation --
    Pop,

    // -- Control flow --
    /// Jump forward by offset (unconditional).
    Jump(u16),
    /// Jump forward by offset if top of stack is falsy (pops the value).
    JumpIfFalse(u16),
    /// Jump forward by offset if top of stack is truthy (does NOT pop — for short-circuit).
    JumpIfTrue(u16),

    /// Jump backward by offset (for loops).
    JumpBack(u16),

    // -- Data construction --
    /// Pop end and start from stack, push List of [start..end).
    MakeRange,
    /// Pop index and list from stack, push list[index].
    ListGet,
    /// Pop list from stack, push its length as Int.
    ListLen,
    /// Pop N values from stack, push as List.
    MakeList(u16),
    /// Pop N key-value pairs from stack, push as Record.
    /// Keys are Value::String constants from the pool. Stack: [key0, val0, key1, val1, ...] → Record
    MakeRecord(u16),
    /// Pop record from stack, push value of field (constant index for field name string).
    GetField(u16),
    /// Pop N values from stack, push as Tuple.
    MakeTuple(u16),
    /// Pop Tuple from stack, push element at index.
    TupleGet(u16),
    /// Pop value and tag from stack, push as Enum { tag, payload }.
    /// Tag is a constant index (string). Payload is the value (or Unit for nullary).
    MakeEnum(u16),
    /// Pop Record and tag from stack, push as Struct { name, fields }.
    /// Tag is a constant index (string).
    MakeStruct(u16),
    /// Pop Enum from stack, push its tag as String.
    EnumTag,
    /// Pop Enum from stack, push its payload.
    EnumPayload,
    /// Pop N key-value updates + base record, push new record with fields merged.
    /// Stack: [base_record, key0, val0, key1, val1, ...] → updated record
    UpdateRecord(u16),

    // -- Functions --
    /// Call a function with N arguments. Stack: [func, arg0, ..., argN-1] → [result]
    Call(u8),
    /// Get a captured upvalue by index (for closures).
    GetUpvalue(u8),
    /// Create a closure: pop N upvalues, bundle with chunk index.
    MakeClosure(u16, u8),

    // -- Native functions --
    /// Call a native function by ID with N arguments.
    /// Stack: [arg0, ..., argN-1] → [result]
    CallNative(u16, u8),

    // -- Termination --
    Return,
}

// ---------------------------------------------------------------------------
// Chunk
// ---------------------------------------------------------------------------

/// A compiled unit of bytecode with its constant pool and debug info.
#[derive(Debug, Default)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: Vec<Value>,
    /// Source locations: (line, col) per opcode index.
    pub source_map: Vec<(usize, usize)>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    /// Emit an opcode, recording its source location.
    pub fn emit(&mut self, op: Op, line: usize, col: usize) -> usize {
        let idx = self.code.len();
        self.code.push(op);
        self.source_map.push((line, col));
        idx
    }

    /// Add a constant to the pool, returning its index.
    /// Deduplicates: if an equal constant already exists, returns its index.
    pub fn add_constant(&mut self, value: Value) -> u16 {
        if let Some(idx) = self.constants.iter().position(|v| v == &value) {
            return idx as u16;
        }
        let idx = self.constants.len();
        self.constants.push(value);
        idx as u16
    }

    /// Rewrite all chunk-index references by an offset.
    /// Used when merging imported module chunks into a program.
    pub fn offset_chunk_refs(&mut self, offset: usize) {
        for constant in &mut self.constants {
            if let Value::Function(idx) = constant {
                *idx += offset;
            }
        }
        for op in &mut self.code {
            if let Op::MakeClosure(idx, _) = op {
                *idx += offset as u16;
            }
        }
    }

    /// Ensure the chunk ends with a Return opcode (sentinel return).
    /// The compiler calls this during finalization. The VM relies on this
    /// invariant to skip per-instruction bounds checks in the dispatch loop.
    pub fn ensure_return(&mut self) {
        if self.code.last() != Some(&Op::Return) {
            let len = self.code.len();
            let (line, col) = if len > 0 {
                self.source_map[len - 1]
            } else {
                (0, 0)
            };
            self.emit(Op::Return, line, col);
        }
    }

    /// Patch a jump instruction at `offset` with the correct jump distance.
    pub fn patch_jump(&mut self, offset: usize) {
        let jump_dist = (self.code.len() - offset - 1) as u16;
        match &mut self.code[offset] {
            Op::Jump(dist) => *dist = jump_dist,
            Op::JumpIfFalse(dist) => *dist = jump_dist,
            Op::JumpIfTrue(dist) => *dist = jump_dist,
            _ => panic!("patch_jump called on non-jump op"),
        }
    }
}

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

/// A compiled program: multiple chunks (one per function) plus an entry point.
#[derive(Debug)]
pub struct Program {
    pub chunks: Vec<Chunk>,
    /// Index of the entry-point chunk in `chunks`.
    pub entry: usize,
}

impl Program {
    /// Run peephole optimizations on all chunks.
    pub fn optimize(&mut self) {
        for chunk in &mut self.chunks {
            chunk.specialize_int_ops();
        }
    }
}

impl Chunk {
    /// Replace generic arithmetic/comparison ops with specialized int variants
    /// when at least one operand is a known integer (LoadSmallInt or LoadConst(Int)).
    /// Safe because the type checker validates operand types before execution.
    pub fn specialize_int_ops(&mut self) {
        let len = self.code.len();
        for i in 0..len {
            match self.code[i] {
                Op::Add | Op::Sub | Op::Lt | Op::Le => {
                    if self.has_int_operand(i) {
                        self.code[i] = match self.code[i] {
                            Op::Add => Op::AddInt,
                            Op::Sub => Op::SubInt,
                            Op::Lt => Op::LtInt,
                            Op::Le => Op::LeInt,
                            _ => unreachable!(),
                        };
                    }
                }
                _ => {}
            }
        }
    }

    /// Check if the binary op at `idx` has at least one known-int operand.
    /// Scans backwards to find the two push operations that feed this op.
    fn has_int_operand(&self, idx: usize) -> bool {
        let mut pushes_found = 0;
        let mut has_int = false;
        let mut i = idx;

        while i > 0 && pushes_found < 2 {
            i -= 1;
            match self.code[i] {
                Op::LoadSmallInt(_) | Op::AddInt | Op::SubInt => {
                    has_int = true;
                    pushes_found += 1;
                }
                Op::LoadConst(ci) => {
                    if matches!(self.constants.get(ci as usize), Some(Value::Int(_))) {
                        has_int = true;
                    }
                    pushes_found += 1;
                }
                Op::GetLocal(_) | Op::GetUpvalue(_) | Op::Negate
                | Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod
                | Op::Not | Op::Eq | Op::Ne | Op::Lt | Op::Gt | Op::Le | Op::Ge
                | Op::LtInt | Op::LeInt | Op::ListLen | Op::EnumTag | Op::EnumPayload
                | Op::Concat | Op::TupleGet(_) | Op::GetField(_) | Op::ListGet
                | Op::MakeRange | Op::MakeList(_) | Op::MakeRecord(_) | Op::MakeTuple(_)
                | Op::MakeEnum(_) | Op::MakeStruct(_) | Op::UpdateRecord(_)
                | Op::MakeClosure(_, _) => {
                    pushes_found += 1;
                }
                // Control flow or Call/CallNative — stop scanning
                _ => break,
            }
        }
        has_int && pushes_found >= 2
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emit_and_read_back() {
        let mut chunk = Chunk::new();
        let ci = chunk.add_constant(Value::Int(42));
        chunk.emit(Op::LoadConst(ci), 1, 0);
        chunk.emit(Op::Return, 1, 0);

        assert_eq!(chunk.code.len(), 2);
        assert_eq!(chunk.code[0], Op::LoadConst(0));
        assert_eq!(chunk.code[1], Op::Return);
        assert_eq!(chunk.constants[0], Value::Int(42));
    }

    #[test]
    fn add_constant_returns_sequential_indices() {
        let mut chunk = Chunk::new();
        assert_eq!(chunk.add_constant(Value::Int(1)), 0);
        assert_eq!(chunk.add_constant(Value::Int(2)), 1);
        assert_eq!(chunk.add_constant(Value::String("hi".into())), 2);
    }

    #[test]
    fn add_constant_deduplicates() {
        let mut chunk = Chunk::new();
        let a = chunk.add_constant(Value::Int(42));
        let b = chunk.add_constant(Value::Int(42));
        let c = chunk.add_constant(Value::String("hello".into()));
        let d = chunk.add_constant(Value::String("hello".into()));
        assert_eq!(a, b);
        assert_eq!(c, d);
        assert_eq!(chunk.constants.len(), 2);
    }

    #[test]
    fn source_map_tracks_locations() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::Add, 5, 10);
        chunk.emit(Op::Return, 6, 0);
        assert_eq!(chunk.source_map[0], (5, 10));
        assert_eq!(chunk.source_map[1], (6, 0));
    }

    #[test]
    fn patch_jump_updates_distance() {
        let mut chunk = Chunk::new();
        let jump_idx = chunk.emit(Op::JumpIfFalse(0), 1, 0);
        chunk.emit(Op::LoadConst(0), 1, 5);
        chunk.emit(Op::LoadConst(1), 1, 10);
        chunk.patch_jump(jump_idx);

        // Jump should skip 2 instructions (index 1 and 2)
        assert_eq!(chunk.code[0], Op::JumpIfFalse(2));
    }

    #[test]
    fn emit_returns_instruction_index() {
        let mut chunk = Chunk::new();
        assert_eq!(chunk.emit(Op::Add, 1, 0), 0);
        assert_eq!(chunk.emit(Op::Sub, 1, 0), 1);
        assert_eq!(chunk.emit(Op::Mul, 1, 0), 2);
    }

    #[test]
    fn ensure_return_appends_if_missing() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::Add, 1, 0);
        assert_ne!(chunk.code.last(), Some(&Op::Return));
        chunk.ensure_return();
        assert_eq!(chunk.code.last(), Some(&Op::Return));
        assert_eq!(chunk.code.len(), 2);
    }

    #[test]
    fn ensure_return_noop_if_present() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::Add, 1, 0);
        chunk.emit(Op::Return, 1, 0);
        chunk.ensure_return();
        assert_eq!(chunk.code.len(), 2); // no duplicate
    }

    #[test]
    fn ensure_return_on_empty_chunk() {
        let mut chunk = Chunk::new();
        chunk.ensure_return();
        assert_eq!(chunk.code.len(), 1);
        assert_eq!(chunk.code[0], Op::Return);
    }
}
