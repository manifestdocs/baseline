use super::value::Value;

// ---------------------------------------------------------------------------
// Opcodes
// ---------------------------------------------------------------------------

/// Bytecode instructions for the stack-based VM.
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    /// Push a constant from the pool onto the stack.
    LoadConst(u16),

    // -- Arithmetic --
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Negate,

    // -- Comparison --
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,

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
    pub fn add_constant(&mut self, value: Value) -> u16 {
        let idx = self.constants.len();
        self.constants.push(value);
        idx as u16
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
}
