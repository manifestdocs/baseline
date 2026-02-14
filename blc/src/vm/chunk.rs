use std::collections::HashMap;

use super::nvalue::NValue;
use super::value::RcStr;

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
    /// Pop two lists from stack, push concatenation.
    ListConcat,
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
    /// Direct call to a known function by chunk index. No function value on stack.
    /// Stack: [arg0, ..., argN-1] → [result]
    CallDirect(u16, u8),
    /// Tail call: reuse current frame for self-recursive calls.
    /// Overwrites args in-place and resets IP to 0. Stack: [arg0, ..., argN-1]
    TailCall(u8),
    /// Get a captured upvalue by index (for closures).
    GetUpvalue(u8),
    /// Create a closure: pop N upvalues, bundle with chunk index.
    MakeClosure(u16, u8),

    // -- Native functions --
    /// Call a native function by ID with N arguments.
    /// Stack: [arg0, ..., argN-1] → [result]
    CallNative(u16, u8),

    // -- Superinstructions (fused opcode sequences) --
    /// Fused GetLocal(slot) + LoadSmallInt(k) + SubInt.
    /// Pushes (local[slot] - k) onto the stack.
    GetLocalSubInt(u16, i16),
    /// Fused GetLocal(slot) + LoadSmallInt(k) + LeInt.
    /// Pushes (local[slot] <= k) as bool onto the stack.
    GetLocalLeInt(u16, i16),
    /// Fused GetLocal(slot) + LoadSmallInt(k) + AddInt.
    /// Pushes (local[slot] + k) onto the stack.
    GetLocalAddInt(u16, i16),
    /// Fused GetLocal(slot) + LoadSmallInt(k) + LtInt.
    /// Pushes (local[slot] < k) as bool onto the stack.
    GetLocalLtInt(u16, i16),

    /// Fused GetLocalLeInt(slot, k) + JumpIfFalse(offset).
    /// Tests local[slot] <= k and jumps if false, without pushing/popping a bool.
    GetLocalLeIntJumpIfFalse(u16, i16, u16),
    /// Fused GetLocalLtInt(slot, k) + JumpIfFalse(offset).
    /// Tests local[slot] < k and jumps if false, without pushing/popping a bool.
    GetLocalLtIntJumpIfFalse(u16, i16, u16),

    // -- Effect handlers --
    /// Push a handler map onto the handler stack. The record is on top of stack.
    /// N is the number of effect name/handler pairs. Stack: [handler_record] → []
    PushHandler,
    /// Pop the most recent handler map from the handler stack.
    PopHandler,
    /// Push a resumable handler (for `handle ... with { ... }`).
    /// Same as PushHandler but also records a handler boundary for continuation capture.
    /// The u16 is the offset from this instruction to the instruction after PopHandler,
    /// used to skip the body when a handler returns without calling resume (abort).
    PushResumableHandler(u16),
    /// Perform a user-defined effect operation.
    /// First arg: constant pool index for "Effect.method" key string.
    /// Second arg: number of effect arguments on the stack.
    PerformEffect(u16, u8),

    // -- Termination --
    /// Runtime error with message from constant pool.
    Halt(u16),
    Return,
}

// ---------------------------------------------------------------------------
// Chunk
// ---------------------------------------------------------------------------

/// Hashable key for constant pool deduplication.
/// Covers the common constant types; Float/complex types fall through to linear scan.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstKey {
    Int(i64),
    String(RcStr),
    Bool(bool),
    Unit,
    Function(usize),
}

impl ConstKey {
    fn from_nvalue(value: &NValue) -> Option<Self> {
        if value.is_int() {
            Some(ConstKey::Int(value.as_int()))
        } else if value.is_bool() {
            Some(ConstKey::Bool(value.as_bool()))
        } else if value.is_unit() {
            Some(ConstKey::Unit)
        } else if value.is_function() {
            Some(ConstKey::Function(value.as_function()))
        } else {
            value.as_string().map(|s| ConstKey::String(s.clone()))
            // Float, List, Record, etc. — rare in constant pools
        }
    }
}

/// A compiled unit of bytecode with its constant pool and debug info.
#[derive(Debug, Default, Clone)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub constants: Vec<NValue>,
    /// Source locations: (line, col) per opcode index.
    pub source_map: Vec<(usize, usize)>,
    /// Dedup index for O(1) constant pool lookups (common types only).
    const_index: HashMap<ConstKey, u16>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    /// Reconstruct a chunk from pre-built parts (used by thread-pool server).
    pub fn from_parts(
        code: Vec<Op>,
        constants: Vec<NValue>,
        source_map: Vec<(usize, usize)>,
    ) -> Self {
        // Rebuild the dedup index from constants
        let mut const_index = HashMap::new();
        for (i, v) in constants.iter().enumerate() {
            if let Some(key) = ConstKey::from_nvalue(v) {
                const_index.entry(key).or_insert(i as u16);
            }
        }
        Chunk {
            code,
            constants,
            source_map,
            const_index,
        }
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
    /// Uses a HashMap for O(1) lookup of common types (Int, String, Bool, Unit, Function).
    pub fn add_constant(&mut self, value: NValue) -> u16 {
        // Fast path: check the hash index for common types
        if let Some(key) = ConstKey::from_nvalue(&value) {
            if let Some(&idx) = self.const_index.get(&key) {
                return idx;
            }
            let idx = self.constants.len() as u16;
            self.const_index.insert(key, idx);
            self.constants.push(value);
            return idx;
        }
        // Slow path: linear scan for Float, List, Record, etc.
        if let Some(idx) = self.constants.iter().position(|v| v == &value) {
            return idx as u16;
        }
        let idx = self.constants.len() as u16;
        self.constants.push(value);
        idx
    }

    /// Rewrite all chunk-index references by an offset.
    /// Used when merging imported module chunks into a program.
    pub fn offset_chunk_refs(&mut self, offset: usize) {
        for constant in &mut self.constants {
            if constant.is_function() {
                *constant = NValue::function(constant.as_function() + offset);
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
    /// Panics in debug mode if the offset doesn't point to a jump instruction.
    pub fn patch_jump(&mut self, offset: usize) {
        let raw_dist = self.code.len() - offset - 1;
        let jump_dist = u16::try_from(raw_dist)
            .unwrap_or_else(|_| panic!("jump distance too large ({} > 65535)", raw_dist));
        match &mut self.code[offset] {
            Op::Jump(dist) => *dist = jump_dist,
            Op::JumpIfFalse(dist) => *dist = jump_dist,
            Op::JumpIfTrue(dist) => *dist = jump_dist,
            other => {
                debug_assert!(false, "patch_jump called on non-jump op: {:?}", other);
            }
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
            chunk.fuse_superinstructions();
            chunk.compact_nops();
            chunk.peephole_cleanup();
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
                Op::Add
                | Op::Sub
                | Op::Mul
                | Op::Div
                | Op::Mod
                | Op::Lt
                | Op::Le
                | Op::Gt
                | Op::Ge => {
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
                Op::LoadSmallInt(_)
                | Op::AddInt
                | Op::SubInt
                | Op::MulInt
                | Op::DivInt
                | Op::ModInt => {
                    has_int = true;
                    pushes_found += 1;
                }
                Op::LoadConst(ci) => {
                    if self.constants.get(ci as usize).is_some_and(|c| c.is_int()) {
                        has_int = true;
                    }
                    pushes_found += 1;
                }
                Op::GetLocal(_)
                | Op::GetUpvalue(_)
                | Op::Negate
                | Op::Add
                | Op::Sub
                | Op::Mul
                | Op::Div
                | Op::Mod
                | Op::Not
                | Op::Eq
                | Op::Ne
                | Op::Lt
                | Op::Gt
                | Op::Le
                | Op::Ge
                | Op::LtInt
                | Op::LeInt
                | Op::GtInt
                | Op::GeInt
                | Op::ListLen
                | Op::EnumTag
                | Op::EnumPayload
                | Op::Concat
                | Op::ListConcat
                | Op::TupleGet(_)
                | Op::GetField(_)
                | Op::ListGet
                | Op::MakeRange
                | Op::MakeList(_)
                | Op::MakeRecord(_)
                | Op::MakeTuple(_)
                | Op::MakeEnum(_)
                | Op::MakeStruct(_)
                | Op::UpdateRecord(_)
                | Op::MakeClosure(_, _)
                | Op::GetLocalSubInt(_, _)
                | Op::GetLocalLeInt(_, _)
                | Op::GetLocalAddInt(_, _)
                | Op::GetLocalLtInt(_, _) => {
                    pushes_found += 1;
                }
                // Control flow or Call/CallNative — stop scanning
                _ => break,
            }
        }
        has_int && pushes_found >= 2
    }

    /// Fuse common opcode triples into single superinstructions.
    ///
    /// Runs AFTER specialize_int_ops (operates on already-specialized opcodes).
    /// Patterns fused:
    ///   GetLocal(s) + LoadSmallInt(k) + SubInt  →  GetLocalSubInt(s, k)
    ///   GetLocal(s) + LoadSmallInt(k) + LeInt   →  GetLocalLeInt(s, k)
    ///
    /// Replaced opcodes become Jump(0) (no-op) to preserve indices and source map.
    pub fn fuse_superinstructions(&mut self) {
        let len = self.code.len();
        if len < 3 {
            return;
        }
        // Pass 1: fuse 3-instruction patterns (GetLocal + LoadSmallInt + Op)
        for i in 0..len - 2 {
            if let (Op::GetLocal(slot), Op::LoadSmallInt(k)) = (self.code[i], self.code[i + 1]) {
                let fused = match self.code[i + 2] {
                    Op::SubInt => Some(Op::GetLocalSubInt(slot, k)),
                    Op::LeInt => Some(Op::GetLocalLeInt(slot, k)),
                    Op::AddInt => Some(Op::GetLocalAddInt(slot, k)),
                    Op::LtInt => Some(Op::GetLocalLtInt(slot, k)),
                    _ => None,
                };
                if let Some(super_op) = fused {
                    let source_loc = self.source_map[i + 2];
                    self.code[i] = Op::Jump(0); // no-op
                    self.code[i + 1] = Op::Jump(0); // no-op
                    self.code[i + 2] = super_op;
                    self.source_map[i + 2] = source_loc;
                }
            }
        }
        // Pass 2: fuse 2-instruction patterns (compare-and-jump)
        // Must run after pass 1 since pass 1 creates the GetLocalLeInt/GetLocalLtInt instructions
        let len = self.code.len();
        if len < 2 {
            return;
        }
        for i in 0..len - 1 {
            match (self.code[i], self.code[i + 1]) {
                (Op::GetLocalLeInt(slot, k), Op::JumpIfFalse(offset)) => {
                    let source_loc = self.source_map[i + 1];
                    self.code[i] = Op::Jump(0); // no-op
                    self.code[i + 1] = Op::GetLocalLeIntJumpIfFalse(slot, k, offset);
                    self.source_map[i + 1] = source_loc;
                }
                (Op::GetLocalLtInt(slot, k), Op::JumpIfFalse(offset)) => {
                    let source_loc = self.source_map[i + 1];
                    self.code[i] = Op::Jump(0); // no-op
                    self.code[i + 1] = Op::GetLocalLtIntJumpIfFalse(slot, k, offset);
                    self.source_map[i + 1] = source_loc;
                }
                _ => {}
            }
        }
    }

    /// Remove `Jump(0)` no-ops left by superinstruction fusion and adjust all
    /// jump offsets accordingly. Also updates `source_map`.
    pub fn compact_nops(&mut self) {
        if self.code.is_empty() {
            return;
        }

        // Build old-index → new-index mapping
        let mut old_to_new: Vec<usize> = Vec::with_capacity(self.code.len());
        let mut new_idx = 0usize;
        for op in &self.code {
            old_to_new.push(new_idx);
            if *op != Op::Jump(0) {
                new_idx += 1;
            }
        }
        let new_len = new_idx;
        if new_len == self.code.len() {
            return; // nothing to compact
        }

        // Adjust jump offsets: a forward jump at old position `i` targeting
        // old position `i + 1 + offset` needs to become new_offset such that
        // new_pos + 1 + new_offset = old_to_new[old_target].
        // Similarly for JumpBack.
        let old_len = self.code.len();
        for i in 0..old_len {
            if self.code[i] == Op::Jump(0) {
                continue;
            }
            match &mut self.code[i] {
                Op::Jump(offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < old_len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::JumpIfFalse(offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < old_len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::JumpIfTrue(offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < old_len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::JumpBack(offset) => {
                    let old_target = i + 1 - *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = old_to_new[old_target];
                    *offset = (new_src + 1 - new_target) as u16;
                }
                Op::GetLocalLeIntJumpIfFalse(_, _, offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < old_len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::GetLocalLtIntJumpIfFalse(_, _, offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < old_len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::PushResumableHandler(offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < old_len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                _ => {}
            }
        }

        // Compact: remove Jump(0) entries from code and source_map
        let mut write = 0;
        for read in 0..old_len {
            if self.code[read] != Op::Jump(0) {
                self.code[write] = self.code[read];
                self.source_map[write] = self.source_map[read];
                write += 1;
            }
        }
        self.code.truncate(new_len);
        self.source_map.truncate(new_len);
    }

    /// Remove dead instruction pairs (e.g. `LoadConst + Pop`, `CloseScope(0)`).
    pub fn peephole_cleanup(&mut self) {
        let len = self.code.len();
        if len < 2 {
            return;
        }
        // Mark instructions for removal
        let mut remove = vec![false; len];
        for i in 0..len - 1 {
            // LoadConst immediately followed by Pop → dead pair
            if matches!(self.code[i], Op::LoadConst(_)) && self.code[i + 1] == Op::Pop {
                remove[i] = true;
                remove[i + 1] = true;
            }
        }
        for (i, r) in remove.iter_mut().enumerate() {
            if self.code[i] == Op::CloseScope(0) {
                *r = true;
            }
        }
        if !remove.iter().any(|&r| r) {
            return;
        }
        // Build mapping and compact (reuse same logic as compact_nops)
        let mut old_to_new: Vec<usize> = Vec::with_capacity(len);
        let mut new_idx = 0usize;
        for r in &remove {
            old_to_new.push(new_idx);
            if !r {
                new_idx += 1;
            }
        }
        let new_len = new_idx;

        // Adjust jump offsets
        for i in 0..len {
            if remove[i] {
                continue;
            }
            match &mut self.code[i] {
                Op::Jump(offset) if *offset > 0 => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::JumpIfFalse(offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::JumpIfTrue(offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::JumpBack(offset) => {
                    let old_target = i + 1 - *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = old_to_new[old_target];
                    *offset = (new_src + 1 - new_target) as u16;
                }
                Op::GetLocalLeIntJumpIfFalse(_, _, offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::GetLocalLtIntJumpIfFalse(_, _, offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                Op::PushResumableHandler(offset) => {
                    let old_target = i + 1 + *offset as usize;
                    let new_src = old_to_new[i];
                    let new_target = if old_target < len {
                        old_to_new[old_target]
                    } else {
                        new_len
                    };
                    *offset = (new_target - new_src - 1) as u16;
                }
                _ => {}
            }
        }

        // Compact
        let mut write = 0;
        for (read, &should_remove) in remove.iter().enumerate() {
            if !should_remove {
                self.code[write] = self.code[read];
                self.source_map[write] = self.source_map[read];
                write += 1;
            }
        }
        self.code.truncate(new_len);
        self.source_map.truncate(new_len);
    }
}

// ---------------------------------------------------------------------------
// Compile Error
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}] {}", self.line, self.col, self.message)
    }
}

// ---------------------------------------------------------------------------
// Compiled Test
// ---------------------------------------------------------------------------

/// A test expression compiled to a bytecode chunk.
pub struct CompiledTest {
    pub name: String,
    pub chunk_idx: usize,
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
    pub skip: bool,
}

/// A compiled program with inline test metadata.
pub struct TestProgram {
    pub program: Program,
    pub tests: Vec<CompiledTest>,
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
        let ci = chunk.add_constant(NValue::int(42));
        chunk.emit(Op::LoadConst(ci), 1, 0);
        chunk.emit(Op::Return, 1, 0);

        assert_eq!(chunk.code.len(), 2);
        assert_eq!(chunk.code[0], Op::LoadConst(0));
        assert_eq!(chunk.code[1], Op::Return);
        assert_eq!(chunk.constants[0], NValue::int(42));
    }

    #[test]
    fn add_constant_returns_sequential_indices() {
        let mut chunk = Chunk::new();
        assert_eq!(chunk.add_constant(NValue::int(1)), 0);
        assert_eq!(chunk.add_constant(NValue::int(2)), 1);
        assert_eq!(chunk.add_constant(NValue::string("hi".into())), 2);
    }

    #[test]
    fn add_constant_deduplicates() {
        let mut chunk = Chunk::new();
        let a = chunk.add_constant(NValue::int(42));
        let b = chunk.add_constant(NValue::int(42));
        let c = chunk.add_constant(NValue::string("hello".into()));
        let d = chunk.add_constant(NValue::string("hello".into()));
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

    // -- Superinstruction fusion tests --

    #[test]
    fn fuse_get_local_sub_int() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(0), 1, 0);
        chunk.emit(Op::LoadSmallInt(1), 1, 5);
        chunk.emit(Op::SubInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fuse_superinstructions();

        assert_eq!(chunk.code[0], Op::Jump(0)); // no-op
        assert_eq!(chunk.code[1], Op::Jump(0)); // no-op
        assert_eq!(chunk.code[2], Op::GetLocalSubInt(0, 1));
        assert_eq!(chunk.code[3], Op::Return);
    }

    #[test]
    fn fuse_get_local_le_int() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(2), 1, 0);
        chunk.emit(Op::LoadSmallInt(5), 1, 5);
        chunk.emit(Op::LeInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fuse_superinstructions();

        assert_eq!(chunk.code[0], Op::Jump(0));
        assert_eq!(chunk.code[1], Op::Jump(0));
        assert_eq!(chunk.code[2], Op::GetLocalLeInt(2, 5));
        assert_eq!(chunk.code[3], Op::Return);
    }

    #[test]
    fn fuse_preserves_source_map() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(0), 10, 0);
        chunk.emit(Op::LoadSmallInt(2), 10, 5);
        chunk.emit(Op::SubInt, 10, 10);
        chunk.emit(Op::Return, 11, 0);

        chunk.fuse_superinstructions();

        // The superinstruction should use the source location of SubInt (the last fused op)
        assert_eq!(chunk.source_map[2], (10, 10));
    }

    #[test]
    fn fuse_get_local_add_int() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(0), 1, 0);
        chunk.emit(Op::LoadSmallInt(1), 1, 5);
        chunk.emit(Op::AddInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fuse_superinstructions();

        assert_eq!(chunk.code[0], Op::Jump(0));
        assert_eq!(chunk.code[1], Op::Jump(0));
        assert_eq!(chunk.code[2], Op::GetLocalAddInt(0, 1));
        assert_eq!(chunk.code[3], Op::Return);
    }

    #[test]
    fn fuse_get_local_lt_int() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(3), 1, 0);
        chunk.emit(Op::LoadSmallInt(10), 1, 5);
        chunk.emit(Op::LtInt, 1, 10);
        chunk.emit(Op::Return, 1, 15);

        chunk.fuse_superinstructions();

        assert_eq!(chunk.code[0], Op::Jump(0));
        assert_eq!(chunk.code[1], Op::Jump(0));
        assert_eq!(chunk.code[2], Op::GetLocalLtInt(3, 10));
        assert_eq!(chunk.code[3], Op::Return);
    }

    #[test]
    fn fuse_does_not_match_non_pattern() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(0), 1, 0);
        chunk.emit(Op::LoadSmallInt(1), 1, 5);
        chunk.emit(Op::MulInt, 1, 10); // MulInt, not a fusable op
        chunk.emit(Op::Return, 1, 15);

        chunk.fuse_superinstructions();

        // Should remain unfused
        assert_eq!(chunk.code[0], Op::GetLocal(0));
        assert_eq!(chunk.code[1], Op::LoadSmallInt(1));
        assert_eq!(chunk.code[2], Op::MulInt);
    }

    #[test]
    fn fuse_multiple_patterns_in_sequence() {
        let mut chunk = Chunk::new();
        // Pattern 1: n <= 1
        chunk.emit(Op::GetLocal(0), 1, 0);
        chunk.emit(Op::LoadSmallInt(1), 1, 0);
        chunk.emit(Op::LeInt, 1, 0);
        // Some other op
        chunk.emit(Op::JumpIfFalse(3), 1, 0);
        // Pattern 2: n - 1
        chunk.emit(Op::GetLocal(0), 2, 0);
        chunk.emit(Op::LoadSmallInt(1), 2, 0);
        chunk.emit(Op::SubInt, 2, 0);
        chunk.emit(Op::Return, 2, 0);

        chunk.fuse_superinstructions();

        // Pass 1 fuses LeInt -> GetLocalLeInt and SubInt -> GetLocalSubInt
        // Pass 2 fuses GetLocalLeInt + JumpIfFalse -> GetLocalLeIntJumpIfFalse
        assert_eq!(chunk.code[3], Op::GetLocalLeIntJumpIfFalse(0, 1, 3));
        assert_eq!(chunk.code[6], Op::GetLocalSubInt(0, 1));
    }

    #[test]
    fn fuse_short_chunk_no_panic() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::Return, 1, 0);
        chunk.fuse_superinstructions(); // should not panic

        let mut chunk2 = Chunk::new();
        chunk2.emit(Op::GetLocal(0), 1, 0);
        chunk2.emit(Op::Return, 1, 0);
        chunk2.fuse_superinstructions(); // should not panic
    }

    // -- Compact NOP tests (AC-5.13) --

    #[test]
    fn compact_nops_middle() {
        // [Jump(0), Jump(0), GetLocalSubInt(0,1), Return]
        // After compaction: [GetLocalSubInt(0,1), Return]
        let mut chunk = Chunk::new();
        chunk.emit(Op::Jump(0), 1, 0);
        chunk.emit(Op::Jump(0), 1, 0);
        chunk.emit(Op::GetLocalSubInt(0, 1), 1, 5);
        chunk.emit(Op::Return, 1, 10);

        chunk.compact_nops();

        assert_eq!(chunk.code.len(), 2);
        assert_eq!(chunk.code[0], Op::GetLocalSubInt(0, 1));
        assert_eq!(chunk.code[1], Op::Return);
        assert_eq!(chunk.source_map[0], (1, 5));
        assert_eq!(chunk.source_map[1], (1, 10));
    }

    #[test]
    fn compact_nops_start() {
        // [Jump(0), LoadSmallInt(5), Return]
        let mut chunk = Chunk::new();
        chunk.emit(Op::Jump(0), 1, 0);
        chunk.emit(Op::LoadSmallInt(5), 1, 5);
        chunk.emit(Op::Return, 1, 10);

        chunk.compact_nops();

        assert_eq!(chunk.code.len(), 2);
        assert_eq!(chunk.code[0], Op::LoadSmallInt(5));
        assert_eq!(chunk.code[1], Op::Return);
    }

    #[test]
    fn compact_nops_adjusts_forward_jump() {
        // [Jump(0), Jump(0), GetLocalLeInt(0,1), JumpIfFalse(2), LoadSmallInt(1), Jump(0), GetLocal(0), Return]
        // The JumpIfFalse originally targets index 6 (4 + 1 + offset=2 - 1 = 6, wait let me recalc)
        // JumpIfFalse(2) at old index 3: target = 3 + 1 + 2 = 6 (GetLocal)
        // After removing 3 Jump(0)s (indices 0, 1, 5):
        // new code: [GetLocalLeInt, JumpIfFalse(?), LoadSmallInt, GetLocal, Return]
        // new index of JumpIfFalse = 1, target GetLocal at old 6 -> new 3
        // new offset = 3 - 1 - 1 = 1
        let mut chunk = Chunk::new();
        chunk.emit(Op::Jump(0), 1, 0);
        chunk.emit(Op::Jump(0), 1, 0);
        chunk.emit(Op::GetLocalLeInt(0, 1), 1, 0);
        chunk.emit(Op::JumpIfFalse(2), 1, 0); // target: old idx 6
        chunk.emit(Op::LoadSmallInt(1), 1, 0);
        chunk.emit(Op::Jump(0), 1, 0);
        chunk.emit(Op::GetLocal(0), 1, 0);
        chunk.emit(Op::Return, 1, 0);

        chunk.compact_nops();

        assert_eq!(chunk.code.len(), 5);
        assert_eq!(chunk.code[0], Op::GetLocalLeInt(0, 1));
        assert_eq!(chunk.code[1], Op::JumpIfFalse(1)); // adjusted offset
        assert_eq!(chunk.code[3], Op::GetLocal(0));
    }

    #[test]
    fn compact_nops_adjusts_backward_jump() {
        // [GetLocal(0), Jump(0), JumpBack(3)]
        // JumpBack(3) at old index 2: target = 2 + 1 - 3 = 0 (GetLocal)
        // After removing Jump(0) at index 1:
        // new code: [GetLocal(0), JumpBack(?)]
        // JumpBack at new index 1: target GetLocal at new 0
        // new offset = 1 + 1 - 0 = 2
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(0), 1, 0);
        chunk.emit(Op::Jump(0), 1, 0);
        chunk.emit(Op::JumpBack(3), 1, 0);

        chunk.compact_nops();

        assert_eq!(chunk.code.len(), 2);
        assert_eq!(chunk.code[0], Op::GetLocal(0));
        assert_eq!(chunk.code[1], Op::JumpBack(2));
    }

    #[test]
    fn compact_nops_empty_chunk() {
        let mut chunk = Chunk::new();
        chunk.compact_nops(); // should not panic
        assert_eq!(chunk.code.len(), 0);
    }

    #[test]
    fn compact_nops_no_ops_to_remove() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::LoadSmallInt(1), 1, 0);
        chunk.emit(Op::Return, 1, 0);

        let original_len = chunk.code.len();
        chunk.compact_nops();
        assert_eq!(chunk.code.len(), original_len); // unchanged
    }

    #[test]
    fn compact_nops_preserves_non_zero_jumps() {
        // Jump(1) should NOT be removed (only Jump(0) is a no-op)
        let mut chunk = Chunk::new();
        chunk.emit(Op::Jump(1), 1, 0); // skip next
        chunk.emit(Op::LoadSmallInt(1), 1, 0);
        chunk.emit(Op::LoadSmallInt(2), 1, 0);
        chunk.emit(Op::Return, 1, 0);

        chunk.compact_nops();

        assert_eq!(chunk.code.len(), 4); // nothing removed
        assert_eq!(chunk.code[0], Op::Jump(1));
    }

    // -- Fused compare-and-jump tests --

    #[test]
    fn fuse_le_int_jump_if_false() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(0), 1, 0);
        chunk.emit(Op::LoadSmallInt(1), 1, 0);
        chunk.emit(Op::LeInt, 1, 0);
        chunk.emit(Op::JumpIfFalse(5), 1, 0);
        chunk.emit(Op::Return, 1, 0);

        chunk.fuse_superinstructions();

        // Pass 1: indices 0,1,2 -> Jump(0), Jump(0), GetLocalLeInt(0,1)
        // Pass 2: indices 2,3 -> Jump(0), GetLocalLeIntJumpIfFalse(0,1,5)
        assert_eq!(chunk.code[3], Op::GetLocalLeIntJumpIfFalse(0, 1, 5));
    }

    #[test]
    fn fuse_lt_int_jump_if_false() {
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(0), 1, 0);
        chunk.emit(Op::LoadSmallInt(5), 1, 0);
        chunk.emit(Op::LtInt, 1, 0);
        chunk.emit(Op::JumpIfFalse(3), 1, 0);
        chunk.emit(Op::Return, 1, 0);

        chunk.fuse_superinstructions();

        assert_eq!(chunk.code[3], Op::GetLocalLtIntJumpIfFalse(0, 5, 3));
    }

    #[test]
    fn fuse_does_not_fuse_jump_if_true() {
        // AC-5.6: should NOT fuse with JumpIfTrue
        let mut chunk = Chunk::new();
        chunk.emit(Op::GetLocal(0), 1, 0);
        chunk.emit(Op::LoadSmallInt(1), 1, 0);
        chunk.emit(Op::LeInt, 1, 0);
        chunk.emit(Op::JumpIfTrue(5), 1, 0);
        chunk.emit(Op::Return, 1, 0);

        chunk.fuse_superinstructions();

        // Pass 1 fuses GetLocal+LoadSmallInt+LeInt into GetLocalLeInt
        // Pass 2 should NOT fuse GetLocalLeInt + JumpIfTrue
        assert_eq!(chunk.code[2], Op::GetLocalLeInt(0, 1));
        assert_eq!(chunk.code[3], Op::JumpIfTrue(5));
    }

    #[test]
    fn op_size_at_most_8_bytes() {
        // AC-5.14: Op enum must be at most 8 bytes
        assert!(
            std::mem::size_of::<Op>() <= 8,
            "Op is {} bytes, must be <= 8",
            std::mem::size_of::<Op>()
        );
    }

    // Compile-time assertions: Program and Chunk must be Send+Sync
    // for cross-fiber sharing in the structured concurrency runtime.
    const _: () = {
        fn assert_send_sync<T: Send + Sync>() {}
        fn check() {
            assert_send_sync::<super::Program>();
            assert_send_sync::<super::Chunk>();
        }
    };
}
