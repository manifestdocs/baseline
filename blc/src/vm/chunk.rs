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
    MulInt,
    DivInt,
    ModInt,

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
    GtInt,
    GeInt,

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

    /// Call a statically-known function by chunk index with N arguments.
    /// Skips function-value lookup — the compiler knows the target at compile time.
    /// Stack: [arg0, ..., argN-1] → [result]
    CallDirect(u16, u8),

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
            match op {
                Op::MakeClosure(idx, _) | Op::CallDirect(idx, _) => {
                    *idx += offset as u16;
                }
                _ => {}
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
            chunk.fold_constants();
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
                Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod
                | Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                    if self.has_int_operand(i) {
                        self.code[i] = match self.code[i] {
                            Op::Add => Op::AddInt,
                            Op::Sub => Op::SubInt,
                            Op::Mul => Op::MulInt,
                            Op::Div => Op::DivInt,
                            Op::Mod => Op::ModInt,
                            Op::Lt => Op::LtInt,
                            Op::Le => Op::LeInt,
                            Op::Gt => Op::GtInt,
                            Op::Ge => Op::GeInt,
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
                Op::LoadSmallInt(_) | Op::AddInt | Op::SubInt
                | Op::MulInt | Op::DivInt | Op::ModInt => {
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
                | Op::LtInt | Op::LeInt | Op::GtInt | Op::GeInt
                | Op::ListLen | Op::EnumTag | Op::EnumPayload
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

    /// Evaluate constant expressions at compile time, replacing runtime
    /// computation with precomputed results.
    ///
    /// Patterns folded:
    /// - `LoadSmallInt(a), LoadSmallInt(b), <ArithInt>` -> result load
    /// - `LoadConst(Int(a)), LoadConst(Int(b)), <ArithInt>` -> result load
    /// - Mixed LoadSmallInt/LoadConst(Int) combinations
    /// - `LoadSmallInt(a), LoadSmallInt(b), <CmpInt>` -> `LoadConst(bool)`
    /// - `LoadConst(bool), Not` -> `LoadConst(!bool)`
    ///
    /// Folded sequences are replaced in-place: the operator position gets the
    /// result opcode, and the dead load positions become `Jump(0)` (no-ops).
    pub fn fold_constants(&mut self) {
        let len = self.code.len();
        if len < 2 {
            return;
        }

        // Boolean negation: LoadConst(bool), Not -> LoadConst(!bool)
        for i in 1..len {
            if self.code[i] != Op::Not {
                continue;
            }
            if let Op::LoadConst(ci) = self.code[i - 1] {
                match self.constants.get(ci as usize) {
                    Some(Value::Bool(b)) => {
                        let result_idx = self.add_constant(Value::Bool(!b));
                        let source_loc = self.source_map[i];
                        self.code[i - 1] = Op::Jump(0);
                        self.source_map[i - 1] = source_loc;
                        self.code[i] = Op::LoadConst(result_idx);
                    }
                    _ => {}
                }
            }
        }

        if len < 3 {
            return;
        }

        // Integer arithmetic and comparison folding
        for i in 2..len {
            let op = self.code[i];
            let is_arith = matches!(
                op,
                Op::AddInt | Op::SubInt | Op::MulInt | Op::DivInt | Op::ModInt
            );
            let is_cmp = matches!(
                op,
                Op::LtInt | Op::LeInt | Op::GtInt | Op::GeInt
            );

            if !is_arith && !is_cmp {
                continue;
            }

            // Extract the two integer operands from the preceding loads
            let (a_val, b_val) = match (self.code[i - 2], self.code[i - 1]) {
                (Op::LoadSmallInt(a), Op::LoadSmallInt(b)) => {
                    (a as i64, b as i64)
                }
                (Op::LoadSmallInt(a), Op::LoadConst(ci)) => {
                    match self.constants.get(ci as usize) {
                        Some(Value::Int(b)) => (a as i64, *b),
                        _ => continue,
                    }
                }
                (Op::LoadConst(ci), Op::LoadSmallInt(b)) => {
                    match self.constants.get(ci as usize) {
                        Some(Value::Int(a)) => (*a, b as i64),
                        _ => continue,
                    }
                }
                (Op::LoadConst(ci_a), Op::LoadConst(ci_b)) => {
                    match (
                        self.constants.get(ci_a as usize),
                        self.constants.get(ci_b as usize),
                    ) {
                        (Some(Value::Int(a)), Some(Value::Int(b))) => (*a, *b),
                        _ => continue,
                    }
                }
                _ => continue,
            };

            let source_loc = self.source_map[i];

            if is_arith {
                let result = match op {
                    Op::AddInt => a_val.wrapping_add(b_val),
                    Op::SubInt => a_val.wrapping_sub(b_val),
                    Op::MulInt => a_val.wrapping_mul(b_val),
                    Op::DivInt => {
                        if b_val == 0 {
                            continue; // preserve runtime division-by-zero error
                        }
                        a_val / b_val
                    }
                    Op::ModInt => {
                        if b_val == 0 {
                            continue; // preserve runtime modulo-by-zero error
                        }
                        a_val % b_val
                    }
                    _ => unreachable!(),
                };

                // Replace the two loads with no-ops, put result at the op position
                self.code[i - 2] = Op::Jump(0);
                self.source_map[i - 2] = source_loc;
                self.code[i - 1] = Op::Jump(0);
                self.source_map[i - 1] = source_loc;

                // If result fits in i16, use LoadSmallInt; otherwise add to constant pool
                if result >= i16::MIN as i64 && result <= i16::MAX as i64 {
                    self.code[i] = Op::LoadSmallInt(result as i16);
                } else {
                    let ci = self.add_constant(Value::Int(result));
                    self.code[i] = Op::LoadConst(ci);
                }
                self.source_map[i] = source_loc;
            } else {
                // Comparison
                let result = match op {
                    Op::LtInt => a_val < b_val,
                    Op::LeInt => a_val <= b_val,
                    Op::GtInt => a_val > b_val,
                    Op::GeInt => a_val >= b_val,
                    _ => unreachable!(),
                };

                let result_idx = self.add_constant(Value::Bool(result));
                self.code[i - 2] = Op::Jump(0);
                self.source_map[i - 2] = source_loc;
                self.code[i - 1] = Op::Jump(0);
                self.source_map[i - 1] = source_loc;
                self.code[i] = Op::LoadConst(result_idx);
                self.source_map[i] = source_loc;
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

    // -- Constant folding --

    #[test]
    fn fold_small_int_add() {
        // LoadSmallInt(2), LoadSmallInt(3), AddInt -> LoadSmallInt(5)
        let mut chunk = Chunk::new();
        chunk.emit(Op::LoadSmallInt(2), 1, 0);
        chunk.emit(Op::LoadSmallInt(3), 1, 5);
        chunk.emit(Op::AddInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fold_constants();

        assert_eq!(chunk.code[0], Op::Jump(0)); // nop (dead load)
        assert_eq!(chunk.code[1], Op::Jump(0)); // nop (dead load)
        assert_eq!(chunk.code[2], Op::LoadSmallInt(5)); // folded result
        assert_eq!(chunk.code[3], Op::Return);
        // Source location should be from the operator opcode
        assert_eq!(chunk.source_map[2], (1, 10));
    }

    #[test]
    fn fold_small_int_le_comparison() {
        // LoadSmallInt(1), LoadSmallInt(1), LeInt -> LoadConst(true)
        let mut chunk = Chunk::new();
        chunk.emit(Op::LoadSmallInt(1), 1, 0);
        chunk.emit(Op::LoadSmallInt(1), 1, 5);
        chunk.emit(Op::LeInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fold_constants();

        assert_eq!(chunk.code[0], Op::Jump(0));
        assert_eq!(chunk.code[1], Op::Jump(0));
        if let Op::LoadConst(ci) = chunk.code[2] {
            assert_eq!(chunk.constants[ci as usize], Value::Bool(true));
        } else {
            panic!("Expected LoadConst, got {:?}", chunk.code[2]);
        }
    }

    #[test]
    fn fold_small_int_lt_false() {
        // LoadSmallInt(5), LoadSmallInt(3), LtInt -> LoadConst(false)
        let mut chunk = Chunk::new();
        chunk.emit(Op::LoadSmallInt(5), 1, 0);
        chunk.emit(Op::LoadSmallInt(3), 1, 5);
        chunk.emit(Op::LtInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fold_constants();

        if let Op::LoadConst(ci) = chunk.code[2] {
            assert_eq!(chunk.constants[ci as usize], Value::Bool(false));
        } else {
            panic!("Expected LoadConst, got {:?}", chunk.code[2]);
        }
    }

    #[test]
    fn fold_const_pool_int_mul() {
        // LoadConst(Int(6)), LoadConst(Int(7)), MulInt -> LoadSmallInt(42)
        let mut chunk = Chunk::new();
        let a = chunk.add_constant(Value::Int(6));
        let b = chunk.add_constant(Value::Int(7));
        chunk.emit(Op::LoadConst(a), 1, 0);
        chunk.emit(Op::LoadConst(b), 1, 5);
        chunk.emit(Op::MulInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fold_constants();

        assert_eq!(chunk.code[2], Op::LoadSmallInt(42));
    }

    #[test]
    fn fold_mixed_small_and_const() {
        // LoadSmallInt(10), LoadConst(Int(3)), SubInt -> LoadSmallInt(7)
        let mut chunk = Chunk::new();
        let b = chunk.add_constant(Value::Int(3));
        chunk.emit(Op::LoadSmallInt(10), 1, 0);
        chunk.emit(Op::LoadConst(b), 1, 5);
        chunk.emit(Op::SubInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fold_constants();

        assert_eq!(chunk.code[2], Op::LoadSmallInt(7));
    }

    #[test]
    fn fold_not_true() {
        // LoadConst(true), Not -> LoadConst(false)
        let mut chunk = Chunk::new();
        let t = chunk.add_constant(Value::Bool(true));
        chunk.emit(Op::LoadConst(t), 1, 0);
        chunk.emit(Op::Not, 1, 5);
        chunk.emit(Op::Return, 1, 10);

        chunk.fold_constants();

        assert_eq!(chunk.code[0], Op::Jump(0));
        if let Op::LoadConst(ci) = chunk.code[1] {
            assert_eq!(chunk.constants[ci as usize], Value::Bool(false));
        } else {
            panic!("Expected LoadConst, got {:?}", chunk.code[1]);
        }
    }

    #[test]
    fn fold_not_false() {
        // LoadConst(false), Not -> LoadConst(true)
        let mut chunk = Chunk::new();
        let f = chunk.add_constant(Value::Bool(false));
        chunk.emit(Op::LoadConst(f), 1, 0);
        chunk.emit(Op::Not, 1, 5);
        chunk.emit(Op::Return, 1, 10);

        chunk.fold_constants();

        if let Op::LoadConst(ci) = chunk.code[1] {
            assert_eq!(chunk.constants[ci as usize], Value::Bool(true));
        } else {
            panic!("Expected LoadConst, got {:?}", chunk.code[1]);
        }
    }

    #[test]
    fn fold_div_by_zero_not_folded() {
        // LoadSmallInt(10), LoadSmallInt(0), DivInt -> NOT folded (preserve runtime error)
        let mut chunk = Chunk::new();
        chunk.emit(Op::LoadSmallInt(10), 1, 0);
        chunk.emit(Op::LoadSmallInt(0), 1, 5);
        chunk.emit(Op::DivInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fold_constants();

        // Should remain unchanged
        assert_eq!(chunk.code[0], Op::LoadSmallInt(10));
        assert_eq!(chunk.code[1], Op::LoadSmallInt(0));
        assert_eq!(chunk.code[2], Op::DivInt);
    }

    #[test]
    fn fold_large_result_uses_const_pool() {
        // LoadSmallInt(32000), LoadSmallInt(1000), AddInt -> result > i16::MAX, use LoadConst
        let mut chunk = Chunk::new();
        chunk.emit(Op::LoadSmallInt(32000), 1, 0);
        chunk.emit(Op::LoadSmallInt(1000), 1, 5);
        chunk.emit(Op::AddInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fold_constants();

        if let Op::LoadConst(ci) = chunk.code[2] {
            assert_eq!(chunk.constants[ci as usize], Value::Int(33000));
        } else {
            panic!("Expected LoadConst for large result, got {:?}", chunk.code[2]);
        }
    }

    #[test]
    fn fold_no_change_for_non_int_const() {
        // LoadConst(String), LoadSmallInt(1), AddInt -> NOT folded (first operand is String)
        let mut chunk = Chunk::new();
        let s = chunk.add_constant(Value::String("hello".into()));
        chunk.emit(Op::LoadConst(s), 1, 0);
        chunk.emit(Op::LoadSmallInt(1), 1, 5);
        chunk.emit(Op::AddInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fold_constants();

        // Should remain unchanged
        assert_eq!(chunk.code[0], Op::LoadConst(s));
        assert_eq!(chunk.code[1], Op::LoadSmallInt(1));
        assert_eq!(chunk.code[2], Op::AddInt);
    }
}
