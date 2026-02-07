use std::collections::HashMap;
use tree_sitter::Node;

use crate::analysis::types::TypeMap;
use crate::analysis::types::Type;

use super::chunk::{Chunk, Op, Program};
use super::natives::NativeRegistry;
use super::value::Value;

// ---------------------------------------------------------------------------
// Compiler
// ---------------------------------------------------------------------------

/// String literal part — either plain text or an interpolation expression.
enum StringPart<'a> {
    Text(String),
    Interpolation(Node<'a>),
}

/// A local variable in the current scope.
#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: usize,
}

/// A captured variable from an enclosing scope.
#[derive(Debug, Clone)]
struct Upvalue {
    name: String,
    /// True if captured from enclosing scope's locals, false if from its upvalues.
    is_local: bool,
    /// Slot index (if local) or upvalue index (if upvalue) in enclosing scope.
    index: u16,
}

/// Saved compiler state for nested function/lambda compilation.
struct CompilerFrame {
    chunk: Chunk,
    locals: Vec<Local>,
    scope_depth: usize,
    upvalues: Vec<Upvalue>,
}

/// A test expression compiled to a bytecode chunk.
pub struct CompiledTest {
    pub name: String,
    pub function: Option<String>,
    pub chunk_idx: usize,
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

/// A compiled program with inline test metadata.
pub struct TestProgram {
    pub program: Program,
    pub tests: Vec<CompiledTest>,
}

pub struct Compiler<'a> {
    source: &'a str,
    chunk: Chunk,
    /// Local variable stack — mirrors the VM's value stack at compile time.
    locals: Vec<Local>,
    /// Current scope depth (0 = top-level).
    scope_depth: usize,
    /// Global function name → chunk index mapping.
    functions: HashMap<String, usize>,
    /// Finished function chunks (each function compiles to its own chunk).
    compiled_chunks: Vec<Chunk>,
    /// Stack of saved compiler state for nested function compilation.
    enclosing: Vec<CompilerFrame>,
    /// Upvalues captured by the current function being compiled.
    upvalues: Vec<Upvalue>,
    /// Native function registry for resolving Module.method calls.
    natives: &'a NativeRegistry,
    /// Optional type map from the static type checker.
    /// When present, enables type-directed opcode specialization.
    type_map: Option<TypeMap>,
    /// Name of the current top-level function being compiled (for TCO detection).
    current_fn_name: Option<String>,
    /// Whether the current expression is in tail position (last expr before Return).
    tail_position: bool,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str, natives: &'a NativeRegistry) -> Self {
        Compiler {
            source,
            chunk: Chunk::new(),
            locals: Vec::new(),
            scope_depth: 0,
            functions: HashMap::new(),
            compiled_chunks: Vec::new(),
            enclosing: Vec::new(),
            upvalues: Vec::new(),
            natives,
            type_map: None,
            current_fn_name: None,
            tail_position: false,
        }
    }

    /// Create a compiler with type information from the static type checker.
    /// Enables type-directed opcode specialization.
    pub fn new_with_type_map(source: &'a str, natives: &'a NativeRegistry, type_map: TypeMap) -> Self {
        let mut compiler = Self::new(source, natives);
        compiler.type_map = Some(type_map);
        compiler
    }

    /// Pre-populate the compiler with already-compiled chunks and function mappings.
    /// Used by the module compiler to inject imported module functions.
    pub fn set_pre_compiled(&mut self, chunks: Vec<Chunk>, functions: HashMap<String, usize>) {
        self.compiled_chunks = chunks;
        self.functions = functions;
    }

    /// Compile a tree-sitter expression node into bytecode.
    ///
    /// Tail position propagation: only `call_expression`, `if_expression`,
    /// `match_expression`, `block`, `parenthesized_expression`, and `literal`
    /// propagate tail position. All other expression types clear it.
    pub fn compile_expression(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let kind = node.kind();
        match kind {
            // --- Tail-position propagating types ---
            "call_expression" => {
                self.compile_call_expression(node)?;
            }
            "if_expression" => {
                self.compile_if(node)?;
            }
            "match_expression" => {
                self.compile_match(node)?;
            }
            "block" => {
                self.compile_block(node)?;
            }
            "parenthesized_expression" => {
                let inner = node.named_child(0).ok_or_else(|| {
                    self.error("Empty parenthesized expression".into(), node)
                })?;
                self.compile_expression(&inner)?;
            }
            "literal" => {
                let child = node.named_child(0).ok_or_else(|| {
                    self.error("Empty literal".into(), node)
                })?;
                self.compile_expression(&child)?;
            }
            // --- Non-tail types: clear tail_position ---
            other => {
                let was_tail = self.tail_position;
                self.tail_position = false;
                let result = self.compile_expression_leaf(node, other);
                self.tail_position = was_tail;
                result?;
            }
        }
        Ok(())
    }

    /// Compile leaf expression types that never propagate tail position.
    fn compile_expression_leaf(&mut self, node: &Node<'a>, kind: &str) -> Result<(), CompileError> {
        match kind {
            "integer_literal" => {
                let text = self.node_text(node);
                let val: i64 = text.parse().map_err(|_| {
                    self.error(format!("Invalid integer: {}", text), node)
                })?;
                if val >= i16::MIN as i64 && val <= i16::MAX as i64 {
                    self.emit(Op::LoadSmallInt(val as i16), node);
                } else {
                    let idx = self.chunk.add_constant(Value::Int(val));
                    self.emit(Op::LoadConst(idx), node);
                }
            }
            "float_literal" => {
                let text = self.node_text(node);
                let val: f64 = text.parse().map_err(|_| {
                    self.error(format!("Invalid float: {}", text), node)
                })?;
                let idx = self.chunk.add_constant(Value::Float(val));
                self.emit(Op::LoadConst(idx), node);
            }
            "boolean_literal" => {
                let text = self.node_text(node);
                let val = text == "true";
                let idx = self.chunk.add_constant(Value::Bool(val));
                self.emit(Op::LoadConst(idx), node);
            }
            "string_literal" => {
                self.compile_string_literal(node)?;
            }
            "tuple_expression" => {
                self.compile_tuple(node)?;
            }
            "unary_expression" => {
                self.compile_unary(node)?;
            }
            "binary_expression" => {
                self.compile_binary(node)?;
            }
            "identifier" => {
                self.compile_identifier(node)?;
            }
            "for_expression" => {
                self.compile_for(node)?;
            }
            "range_expression" => {
                self.compile_range(node)?;
            }
            "lambda" => {
                self.compile_lambda(node)?;
            }
            "pipe_expression" => {
                self.compile_pipe(node)?;
            }
            "list_expression" => {
                self.compile_list(node)?;
            }
            "record_expression" => {
                self.compile_record(node)?;
            }
            "field_expression" => {
                self.compile_field_access(node)?;
            }
            "struct_expression" => {
                self.compile_enum_constructor(node)?;
            }
            "type_identifier" => {
                self.compile_nullary_constructor(node)?;
            }
            "try_expression" => {
                self.compile_try(node)?;
            }
            "record_update" => {
                self.compile_record_update(node)?;
            }
            "line_comment" | "block_comment" => {
                // Comments are no-ops — skip them
            }
            _ => {
                return Err(self.error(
                    format!("Unsupported expression kind: {}", kind), node,
                ));
            }
        }
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Variables
    // -----------------------------------------------------------------------

    fn compile_identifier(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let name = self.node_text(node);
        // Search locals from innermost to outermost
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                self.emit(Op::GetLocal(i as u16), node);
                return Ok(());
            }
        }
        // Search already-captured upvalues
        for (i, uv) in self.upvalues.iter().enumerate() {
            if uv.name == name {
                self.emit(Op::GetUpvalue(i as u8), node);
                return Ok(());
            }
        }
        // Try to capture from enclosing scope
        if let Some(uv_idx) = self.resolve_upvalue(&name) {
            self.emit(Op::GetUpvalue(uv_idx as u8), node);
            return Ok(());
        }
        // Search global functions
        if let Some(&chunk_idx) = self.functions.get(&name) {
            let idx = self.chunk.add_constant(Value::Function(chunk_idx));
            self.emit(Op::LoadConst(idx), node);
            return Ok(());
        }
        Err(self.error(format!("Undefined variable: {}", name), node))
    }

    /// Try to resolve a variable name as an upvalue from enclosing scopes.
    /// Walks the full enclosing stack and threads upvalues through intermediates.
    fn resolve_upvalue(&mut self, name: &str) -> Option<usize> {
        if self.enclosing.is_empty() {
            return None;
        }
        let depth = self.enclosing.len();
        let mut found_at: Option<(usize, u16, bool)> = None; // (depth, index, is_local)

        // Walk from innermost enclosing frame outward
        for d in (0..depth).rev() {
            // Check locals at this depth
            for (slot, local) in self.enclosing[d].locals.iter().enumerate().rev() {
                if local.name == name {
                    found_at = Some((d, slot as u16, true));
                    break;
                }
            }
            if found_at.is_some() { break; }
            // Check upvalues at this depth
            for (i, uv) in self.enclosing[d].upvalues.iter().enumerate() {
                if uv.name == name {
                    found_at = Some((d, i as u16, false));
                    break;
                }
            }
            if found_at.is_some() { break; }
        }

        let (found_depth, mut index, mut is_local) = found_at?;

        // Thread upvalue through intermediate frames
        for d in (found_depth + 1)..depth {
            let uv_idx = self.enclosing[d].upvalues.len();
            self.enclosing[d].upvalues.push(Upvalue {
                name: name.to_string(),
                is_local,
                index,
            });
            index = uv_idx as u16;
            is_local = false;
        }

        // Add to current function's upvalues
        let uv_idx = self.upvalues.len();
        self.upvalues.push(Upvalue {
            name: name.to_string(),
            is_local,
            index,
        });
        Some(uv_idx)
    }

    fn declare_local(&mut self, name: &str) {
        self.locals.push(Local {
            name: name.to_string(),
            depth: self.scope_depth,
        });
    }

    // -----------------------------------------------------------------------
    // Blocks & Let bindings
    // -----------------------------------------------------------------------

    fn compile_block(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        self.begin_scope();
        let mut last_was_expr = false;

        // Collect non-comment children to determine which is last
        let mut children: Vec<Node<'a>> = Vec::new();
        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            if child.kind() != "line_comment" && child.kind() != "block_comment" {
                children.push(child);
            }
        }

        let was_tail = self.tail_position;
        let last_idx = children.len().saturating_sub(1);

        for (idx, child) in children.iter().enumerate() {
            // If the previous statement was an expression (not a let),
            // pop its value — only the last expression's value is kept.
            if last_was_expr {
                self.emit(Op::Pop, child);
            }

            // Only the last expression in a block is in tail position
            let is_last = idx == last_idx;
            self.tail_position = if is_last { was_tail } else { false };

            match child.kind() {
                "let_binding" => {
                    self.compile_let(child)?;
                    last_was_expr = false;
                }
                _ => {
                    self.compile_expression(child)?;
                    last_was_expr = true;
                }
            }
        }

        self.tail_position = was_tail;

        // If block is empty or ends with let, push Unit
        if !last_was_expr {
            let idx = self.chunk.add_constant(Value::Unit);
            self.emit(Op::LoadConst(idx), node);
        }

        self.end_scope(node);
        Ok(())
    }

    fn compile_let(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // Grammar: 'let' _pattern optional(type_annotation) '=' _expression
        let pattern = node.named_child(0).ok_or_else(|| {
            self.error("Let binding missing pattern".into(), node)
        })?;

        // Find the value expression — it's the last named child
        // (pattern is first, optional type_annotation is second, value is last)
        let value_node = node.named_child(node.named_child_count() - 1).ok_or_else(|| {
            self.error("Let binding missing value".into(), node)
        })?;

        // Compile the value — this pushes it onto the stack
        self.compile_expression(&value_node)?;

        if pattern.kind() == "tuple_pattern" {
            // let (a, b) = expr — destructure using TupleGet
            self.declare_local("__let_tuple");
            let tuple_slot = (self.locals.len() - 1) as u16;

            let mut pat_cursor = pattern.walk();
            let bindings: Vec<Node<'a>> =
                pattern.named_children(&mut pat_cursor).collect();

            for (i, binding) in bindings.iter().enumerate() {
                self.emit(Op::GetLocal(tuple_slot), node);
                self.emit(Op::TupleGet(i as u16), node);
                if binding.kind() == "identifier" {
                    let name = self.node_text(binding);
                    self.declare_local(&name);
                } else if binding.kind() == "wildcard_pattern" {
                    self.declare_local("_");
                }
            }
        } else {
            // Simple identifier pattern
            let name = self.node_text(&pattern);
            self.declare_local(&name);
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, node: &Node<'a>) {
        self.scope_depth -= 1;
        let mut pop_count = 0u16;
        while let Some(local) = self.locals.last() {
            if local.depth <= self.scope_depth {
                break;
            }
            self.locals.pop();
            pop_count += 1;
        }
        if pop_count > 0 {
            // CloseScope removes n locals from under the result value on top
            self.chunk.emit(Op::CloseScope(pop_count),
                node.start_position().row + 1,
                node.start_position().column);
        }
    }

    // -----------------------------------------------------------------------
    // If/Else
    // -----------------------------------------------------------------------

    fn compile_if(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // Grammar: 'if' _expression block ('else' (block | if_expression))?
        // Named children: condition, then-block, optional else
        let mut cursor = node.walk();
        let children: Vec<Node<'a>> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("If expression missing condition".into(), node));
        }

        let condition = &children[0];
        let then_block = &children[1];

        // Condition is never in tail position
        let was_tail = self.tail_position;
        self.tail_position = false;
        self.compile_expression(condition)?;
        self.tail_position = was_tail;

        // Jump over then-branch if false
        let then_jump = self.emit(Op::JumpIfFalse(0), node);

        // Both branches inherit tail position from the if expression
        self.compile_expression(then_block)?;

        if children.len() > 2 {
            // There's an else branch
            let else_jump = self.emit(Op::Jump(0), node);
            self.chunk.patch_jump(then_jump);

            let else_branch = &children[2];
            self.compile_expression(else_branch)?;

            self.chunk.patch_jump(else_jump);
        } else {
            // No else branch — push Unit for the false case
            let else_jump = self.emit(Op::Jump(0), node);
            self.chunk.patch_jump(then_jump);
            let idx = self.chunk.add_constant(Value::Unit);
            self.emit(Op::LoadConst(idx), node);
            self.chunk.patch_jump(else_jump);
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Match
    // -----------------------------------------------------------------------

    fn compile_match(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // Grammar: 'match' _expression match_arm+
        // match_arm: pattern '->' expression
        let mut cursor = node.walk();
        let children: Vec<Node<'a>> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("Match expression missing subject".into(), node));
        }

        let subject = &children[0];
        // Subject is not in tail position; arm bodies inherit tail position
        let was_tail = self.tail_position;
        self.tail_position = false;
        self.compile_expression(subject)?;
        self.tail_position = was_tail;
        // Subject is on stack — declare it as a temporary local so GetLocal works
        self.declare_local("__match_subject");

        let arms: Vec<&Node<'a>> = children[1..].iter()
            .filter(|c| c.kind() == "match_arm")
            .collect();

        if arms.is_empty() {
            return Err(self.error("Match expression has no arms".into(), node));
        }

        let mut end_jumps = Vec::new();
        let subject_slot = (self.locals.len() - 1) as u16;

        for arm in &arms {
            let mut arm_cursor = arm.walk();
            let arm_children: Vec<Node<'a>> = arm.named_children(&mut arm_cursor).collect();

            if arm_children.len() < 2 {
                return Err(self.error("Match arm missing pattern or body".into(), arm));
            }

            let pattern = &arm_children[0];
            let body = &arm_children[arm_children.len() - 1];

            match pattern.kind() {
                "wildcard_pattern" => {
                    self.compile_expression(body)?;
                    end_jumps.push(self.emit(Op::Jump(0), arm));
                }
                "identifier" => {
                    // Bind subject as a named local
                    self.emit(Op::GetLocal(subject_slot), arm);
                    self.begin_scope();
                    let name = self.node_text(pattern);
                    self.declare_local(&name);
                    self.compile_expression(body)?;
                    self.end_scope(arm);
                    end_jumps.push(self.emit(Op::Jump(0), arm));
                }
                "literal" | "integer_literal" | "float_literal"
                | "boolean_literal" | "string_literal" => {
                    // Duplicate subject, compile pattern, compare
                    self.emit(Op::GetLocal(subject_slot), arm);
                    self.compile_expression(pattern)?;
                    self.emit(Op::Eq, pattern);
                    let skip_jump = self.emit(Op::JumpIfFalse(0), arm);

                    self.compile_expression(body)?;
                    end_jumps.push(self.emit(Op::Jump(0), arm));

                    self.chunk.patch_jump(skip_jump);
                }
                "type_identifier" => {
                    // Nullary constructor pattern: None, Active, etc.
                    let tag = self.node_text(pattern);
                    self.emit(Op::GetLocal(subject_slot), arm);
                    self.emit(Op::EnumTag, arm);
                    let tag_const = self.chunk.add_constant(Value::String(tag.into()));
                    self.emit(Op::LoadConst(tag_const), arm);
                    self.emit(Op::Eq, arm);
                    let skip_jump = self.emit(Op::JumpIfFalse(0), arm);

                    self.compile_expression(body)?;
                    end_jumps.push(self.emit(Op::Jump(0), arm));

                    self.chunk.patch_jump(skip_jump);
                }
                "constructor_pattern" => {
                    // Constructor with payload: Some(v), Ok(val), Err(e)
                    let mut pat_cursor = pattern.walk();
                    let pat_children: Vec<Node<'a>> = pattern.named_children(&mut pat_cursor).collect();

                    if pat_children.is_empty() {
                        return Err(self.error("Constructor pattern missing tag".into(), pattern));
                    }

                    let tag_node = &pat_children[0];
                    let tag = self.node_text(tag_node);

                    // Check tag matches
                    self.emit(Op::GetLocal(subject_slot), arm);
                    self.emit(Op::EnumTag, arm);
                    let tag_const = self.chunk.add_constant(Value::String(tag.into()));
                    self.emit(Op::LoadConst(tag_const), arm);
                    self.emit(Op::Eq, arm);
                    let skip_jump = self.emit(Op::JumpIfFalse(0), arm);

                    // Tag matched — bind payload variables
                    self.begin_scope();
                    let binding_count = pat_children.len() - 1;
                    if binding_count == 1 {
                        // Single binding: Some(v), Ok(val), Circle(r)
                        self.emit(Op::GetLocal(subject_slot), arm);
                        self.emit(Op::EnumPayload, arm);
                        let binding = &pat_children[1];
                        if binding.kind() == "identifier" {
                            let name = self.node_text(binding);
                            self.declare_local(&name);
                        } else if binding.kind() == "wildcard_pattern" {
                            self.declare_local("_");
                        }
                    } else if binding_count > 1 {
                        // Multi-binding: Rectangle(w, h) — payload is a Tuple
                        self.emit(Op::GetLocal(subject_slot), arm);
                        self.emit(Op::EnumPayload, arm);
                        self.declare_local("__payload");
                        let payload_slot = (self.locals.len() - 1) as u16;

                        for i in 0..binding_count {
                            let binding = &pat_children[i + 1];
                            self.emit(Op::GetLocal(payload_slot), arm);
                            self.emit(Op::TupleGet(i as u16), arm);
                            if binding.kind() == "identifier" {
                                let name = self.node_text(binding);
                                self.declare_local(&name);
                            } else if binding.kind() == "wildcard_pattern" {
                                self.declare_local("_");
                            }
                        }
                    }

                    self.compile_expression(body)?;
                    self.end_scope(arm);
                    end_jumps.push(self.emit(Op::Jump(0), arm));

                    self.chunk.patch_jump(skip_jump);
                }
                "tuple_pattern" => {
                    // Tuple pattern: (a, b) -> body
                    self.begin_scope();
                    let mut pat_cursor = pattern.walk();
                    let pat_children: Vec<Node<'a>> =
                        pattern.named_children(&mut pat_cursor).collect();

                    for (i, binding) in pat_children.iter().enumerate() {
                        self.emit(Op::GetLocal(subject_slot), arm);
                        self.emit(Op::TupleGet(i as u16), arm);
                        if binding.kind() == "identifier" {
                            let name = self.node_text(binding);
                            self.declare_local(&name);
                        } else if binding.kind() == "wildcard_pattern" {
                            self.declare_local("_");
                        }
                    }

                    self.compile_expression(body)?;
                    self.end_scope(arm);
                    end_jumps.push(self.emit(Op::Jump(0), arm));
                }
                _ => {
                    return Err(self.error(
                        format!("Unsupported match pattern kind: {}", pattern.kind()),
                        pattern,
                    ));
                }
            }
        }

        // No arm matched — push Unit as default
        let idx = self.chunk.add_constant(Value::Unit);
        self.emit(Op::LoadConst(idx), node);

        // Patch end jumps to land here (after the default)
        for jump in &end_jumps {
            self.chunk.patch_jump(*jump);
        }

        // Clean up: remove the subject local, keep result on top
        self.locals.pop(); // remove __match_subject
        self.emit(Op::CloseScope(1), node); // remove subject from stack, keep result

        Ok(())
    }

    // -----------------------------------------------------------------------
    // For loops
    // -----------------------------------------------------------------------

    fn compile_for(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // Grammar: 'for' _pattern 'in' _expression 'do' _expression
        let mut cursor = node.walk();
        let children: Vec<Node<'a>> = node.named_children(&mut cursor).collect();

        if children.len() < 3 {
            return Err(self.error("For loop missing components".into(), node));
        }

        let pattern = &children[0];
        let iterable = &children[1];
        let body = &children[2];

        let var_name = self.node_text(pattern);

        // Compile the iterable — pushes a List onto the stack
        self.compile_expression(iterable)?;
        self.declare_local("__for_list");
        let list_slot = (self.locals.len() - 1) as u16;

        // Push initial index = 0
        let zero = self.chunk.add_constant(Value::Int(0));
        self.emit(Op::LoadConst(zero), node);
        self.declare_local("__for_idx");
        let idx_slot = (self.locals.len() - 1) as u16;

        // Loop start: check index < list.length
        let loop_start = self.chunk.code.len();

        // Get list length
        self.emit(Op::GetLocal(list_slot), node);
        self.emit(Op::ListLen, node);
        // Get index
        self.emit(Op::GetLocal(idx_slot), node);
        // Check: index < length
        self.emit(Op::Lt, node);
        let exit_jump = self.emit(Op::JumpIfFalse(0), node);

        // Get list[index]
        self.emit(Op::GetLocal(list_slot), node);
        self.emit(Op::GetLocal(idx_slot), node);
        self.emit(Op::ListGet, node);

        // Bind loop variable
        self.begin_scope();
        self.declare_local(&var_name);

        // Compile body
        self.compile_expression(body)?;
        // Pop body result (for loops don't produce values)
        self.emit(Op::Pop, node);

        self.end_scope(node);

        // Increment index: idx = idx + 1
        self.emit(Op::GetLocal(idx_slot), node);
        let one = self.chunk.add_constant(Value::Int(1));
        self.emit(Op::LoadConst(one), node);
        self.emit(Op::Add, node);
        self.emit(Op::SetLocal(idx_slot), node);
        self.emit(Op::Pop, node); // SetLocal doesn't pop, so pop the copy

        // Jump back to loop start
        let jump_back_dist = (self.chunk.code.len() - loop_start + 1) as u16;
        self.emit(Op::JumpBack(jump_back_dist), node);

        // Exit point
        self.chunk.patch_jump(exit_jump);

        // Clean up: remove list and index locals
        self.locals.pop(); // __for_idx
        self.locals.pop(); // __for_list
        self.emit(Op::PopN(2), node); // remove list and index from stack

        // For loops produce Unit
        let unit = self.chunk.add_constant(Value::Unit);
        self.emit(Op::LoadConst(unit), node);

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Range expressions
    // -----------------------------------------------------------------------

    fn compile_range(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // Grammar: _expression '..' _expression
        // For now, compile as constructing a list [start..end)
        // We need a MakeRange op
        let lhs = node.named_child(0).ok_or_else(|| {
            self.error("Range missing start".into(), node)
        })?;
        let rhs = node.named_child(1).ok_or_else(|| {
            self.error("Range missing end".into(), node)
        })?;

        self.compile_expression(&lhs)?;
        self.compile_expression(&rhs)?;
        self.emit(Op::MakeRange, node);
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Unary & Binary
    // -----------------------------------------------------------------------

    fn compile_unary(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // Constant folding: evaluate at compile time if possible
        if let Some(val) = self.try_eval_const(node) {
            self.emit_const_value(val, node);
            return Ok(());
        }

        let op_text = node.child(0)
            .map(|c| self.node_text(&c))
            .unwrap_or_default();
        let operand = node.named_child(0).ok_or_else(|| {
            self.error("Unary missing operand".into(), node)
        })?;

        // Operand of a unary op is not in tail position (the op still applies)
        let was_tail = self.tail_position;
        self.tail_position = false;
        self.compile_expression(&operand)?;
        self.tail_position = was_tail;

        match op_text.as_str() {
            "-" => self.emit(Op::Negate, node),
            "not" => self.emit(Op::Not, node),
            _ => return Err(self.error(
                format!("Unknown unary operator: {}", op_text), node,
            )),
        };
        Ok(())
    }

    /// Check if a node is known to produce an integer value at compile time.
    fn is_int_expr(&self, node: &Node) -> bool {
        match node.kind() {
            "integer_literal" => true,
            "unary_expression" => {
                // -integer_literal is still int
                node.named_child(0).map_or(false, |c| c.kind() == "integer_literal")
            }
            "binary_expression" => {
                // int +/-/*/%// int is still int
                if let Some(op) = node.child(1) {
                    let op_text = self.node_text(&op);
                    matches!(op_text.as_str(), "+" | "-" | "*" | "/" | "%")
                        && node.named_child(0).map_or(false, |c| self.is_int_expr(&c))
                        && node.named_child(1).map_or(false, |c| self.is_int_expr(&c))
                } else {
                    false
                }
            }
            // Variables could be int too, but we'd need type info for that
            _ => false,
        }
    }

    /// Try to evaluate a CST node as a compile-time constant.
    /// Returns None if the expression contains variables, calls, interpolation, etc.
    ///
    /// Uses [`eval_const_binary`] for binary sub-expressions.
    fn try_eval_const(&self, node: &Node) -> Option<Value> {
        match node.kind() {
            "integer_literal" => {
                self.node_text(node).parse::<i64>().ok().map(Value::Int)
            }
            "float_literal" => {
                self.node_text(node).parse::<f64>().ok().map(Value::Float)
            }
            "boolean_literal" => {
                Some(Value::Bool(self.node_text(node) == "true"))
            }
            "string_literal" if node.named_child_count() == 0 => {
                let text = &self.source[node.start_byte() + 1..node.end_byte() - 1];
                Some(Value::String(text.into()))
            }
            "parenthesized_expression" | "literal" => {
                node.named_child(0).and_then(|c| self.try_eval_const(&c))
            }
            "unary_expression" => {
                let op = node.child(0).map(|c| self.node_text(&c))?;
                let val = node.named_child(0).and_then(|c| self.try_eval_const(&c))?;
                match (op.as_str(), &val) {
                    ("-", Value::Int(n)) => Some(Value::Int(n.wrapping_neg())),
                    ("-", Value::Float(n)) => Some(Value::Float(-n)),
                    ("not", Value::Bool(b)) => Some(Value::Bool(!b)),
                    _ => None,
                }
            }
            "binary_expression" => {
                let a = node.named_child(0).and_then(|c| self.try_eval_const(&c))?;
                let op = self.node_text(&node.child(1)?);
                let b = node.named_child(1).and_then(|c| self.try_eval_const(&c))?;
                eval_const_binary(op.as_str(), &a, &b)
            }
            _ => None,
        }
    }

    /// Emit a single load instruction for a constant value.
    fn emit_const_value(&mut self, val: Value, node: &Node<'a>) {
        if let Value::Int(n) = val {
            if n >= i16::MIN as i64 && n <= i16::MAX as i64 {
                self.emit(Op::LoadSmallInt(n as i16), node);
                return;
            }
        }
        let idx = self.chunk.add_constant(val);
        self.emit(Op::LoadConst(idx), node);
    }

    fn compile_binary(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // Constant folding: evaluate at compile time if possible
        if let Some(val) = self.try_eval_const(node) {
            self.emit_const_value(val, node);
            return Ok(());
        }

        let lhs = node.child_by_field_name("left")
            .or_else(|| node.named_child(0))
            .ok_or_else(|| self.error("Binary missing lhs".into(), node))?;
        let rhs = node.child_by_field_name("right")
            .or_else(|| node.named_child(1))
            .ok_or_else(|| self.error("Binary missing rhs".into(), node))?;

        let op_node = node.child(1).ok_or_else(|| {
            self.error("Binary missing operator".into(), node)
        })?;
        let op_text = self.node_text(&op_node);

        match op_text.as_str() {
            "&&" => return self.compile_and(node, &lhs, &rhs),
            "||" => return self.compile_or(node, &lhs, &rhs),
            _ => {}
        }

        // Operands of a binary op are never in tail position
        let was_tail = self.tail_position;
        self.tail_position = false;
        self.compile_expression(&lhs)?;
        self.compile_expression(&rhs)?;
        self.tail_position = was_tail;

        // Use specialized int opcodes when both operands are statically known integers.
        // First check the type map from the static type checker (covers variables, calls, etc.),
        // then fall back to the syntactic is_int_expr heuristic for literal-only cases.
        let both_int = self.type_map.as_ref().map_or(false, |tm| {
            matches!(
                (tm.get(&lhs.start_byte()), tm.get(&rhs.start_byte())),
                (Some(Type::Int), Some(Type::Int))
            )
        }) || (self.is_int_expr(&lhs) && self.is_int_expr(&rhs));

        let op = match op_text.as_str() {
            "+" if both_int => Op::AddInt,
            "+" => Op::Add,
            "-" if both_int => Op::SubInt,
            "-" => Op::Sub,
            "*" if both_int => Op::MulInt,
            "*" => Op::Mul,
            "/" if both_int => Op::DivInt,
            "/" => Op::Div,
            "%" if both_int => Op::ModInt,
            "%" => Op::Mod,
            "==" => Op::Eq,
            "!=" => Op::Ne,
            "<" if both_int => Op::LtInt,
            "<" => Op::Lt,
            ">" if both_int => Op::GtInt,
            ">" => Op::Gt,
            "<=" if both_int => Op::LeInt,
            "<=" => Op::Le,
            ">=" if both_int => Op::GeInt,
            ">=" => Op::Ge,
            _ => return Err(self.error(
                format!("Unknown binary operator: {}", op_text), node,
            )),
        };

        self.emit(op, node);
        Ok(())
    }

    fn compile_and(&mut self, node: &Node<'a>, lhs: &Node<'a>, rhs: &Node<'a>) -> Result<(), CompileError> {
        self.compile_expression(lhs)?;
        let jump_idx = self.emit(Op::JumpIfFalse(0), node);
        self.compile_expression(rhs)?;
        let end_idx = self.emit(Op::Jump(0), node);
        self.chunk.patch_jump(jump_idx);
        let false_const = self.chunk.add_constant(Value::Bool(false));
        self.emit(Op::LoadConst(false_const), node);
        self.chunk.patch_jump(end_idx);
        Ok(())
    }

    fn compile_or(&mut self, node: &Node<'a>, lhs: &Node<'a>, rhs: &Node<'a>) -> Result<(), CompileError> {
        self.compile_expression(lhs)?;
        let jump_idx = self.emit(Op::JumpIfFalse(0), node);
        let true_const = self.chunk.add_constant(Value::Bool(true));
        self.emit(Op::LoadConst(true_const), node);
        let end_jump = self.emit(Op::Jump(0), node);
        self.chunk.patch_jump(jump_idx);
        self.compile_expression(rhs)?;
        self.chunk.patch_jump(end_jump);
        Ok(())
    }

    // -----------------------------------------------------------------------
    // String literals
    // -----------------------------------------------------------------------

    fn compile_string_literal(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let node_start = node.start_byte();
        let node_end = node.end_byte();
        let src = self.source.as_bytes();

        let mut parts: Vec<StringPart<'a>> = Vec::new();
        let mut cursor = node_start + 1;
        let content_end = node_end - 1;

        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            let child_start = child.start_byte();
            let child_end = child.end_byte();

            if cursor < child_start {
                let text = std::str::from_utf8(&src[cursor..child_start]).unwrap_or("");
                if !text.is_empty() {
                    parts.push(StringPart::Text(text.to_string()));
                }
            }

            match child.kind() {
                "interpolation" => {
                    parts.push(StringPart::Interpolation(child));
                }
                "escape_sequence" => {
                    let text = self.node_text(&child);
                    parts.push(StringPart::Text(unescape(&text)));
                }
                _ => {}
            }

            cursor = child_end;
        }

        if cursor < content_end {
            let text = std::str::from_utf8(&src[cursor..content_end]).unwrap_or("");
            if !text.is_empty() {
                parts.push(StringPart::Text(text.to_string()));
            }
        }

        if parts.is_empty() {
            let idx = self.chunk.add_constant(Value::String("".into()));
            self.emit(Op::LoadConst(idx), node);
            return Ok(());
        }

        let mut segment_count = 0;
        for part in &parts {
            match part {
                StringPart::Text(s) => {
                    let idx = self.chunk.add_constant(Value::String(s.as_str().into()));
                    self.emit(Op::LoadConst(idx), node);
                }
                StringPart::Interpolation(interp_node) => {
                    let expr = interp_node.named_child(0).ok_or_else(|| {
                        self.error("Empty interpolation".into(), interp_node)
                    })?;
                    self.compile_expression(&expr)?;
                }
            }
            if segment_count > 0 {
                self.emit(Op::Concat, node);
            }
            segment_count += 1;
        }

        let has_interpolation = parts.iter().any(|p| matches!(p, StringPart::Interpolation(_)));
        if segment_count == 1 && has_interpolation {
            let idx = self.chunk.add_constant(Value::String("".into()));
            self.emit(Op::LoadConst(idx), node);
            self.emit(Op::Concat, node);
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Program compilation
    // -----------------------------------------------------------------------

    /// Compile a full source file into a multi-chunk Program.
    /// Each function_def becomes its own chunk. Entry point is the last function.
    pub fn compile_program(mut self, root: &Node<'a>) -> Result<Program, CompileError> {
        // chunk_offset accounts for pre-compiled imported chunks
        let chunk_offset = self.compiled_chunks.len();

        // First pass: register all function names with their chunk indices
        let mut func_defs: Vec<Node<'a>> = Vec::new();
        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            if child.kind() == "function_def" {
                let name_node = child.child_by_field_name("name").unwrap();
                let name = self.node_text(&name_node);
                self.functions.insert(name, chunk_offset + func_defs.len());
                func_defs.push(child);
            }
        }

        if func_defs.is_empty() {
            return Err(CompileError {
                message: "No function definitions found".into(),
                line: 1,
                col: 0,
            });
        }

        // Pre-allocate chunk slots so named functions get stable indices.
        // Inner lambdas compiled during the second pass get indices after these.
        let num_funcs = func_defs.len();
        for _ in 0..num_funcs {
            self.compiled_chunks.push(Chunk::new());
        }

        // Second pass: compile each function body, replacing pre-allocated slots
        for (i, func_node) in func_defs.iter().enumerate() {
            let body = func_node.child_by_field_name("body").ok_or_else(|| {
                self.error("Function missing body".into(), func_node)
            })?;

            // Reset state for this function
            self.chunk = Chunk::new();
            self.locals.clear();
            self.scope_depth = 0;

            // Set current function name for TCO detection
            let name_node = func_node.child_by_field_name("name").unwrap();
            self.current_fn_name = Some(self.node_text(&name_node));

            // If body is a lambda, extract params and compile the inner body.
            // Otherwise it's a zero-arg function — compile body directly.
            if body.kind() == "lambda" {
                self.compile_lambda_body(&body)?;
            } else {
                self.tail_position = true;
                self.compile_expression(&body)?;
                self.tail_position = false;
            }

            self.current_fn_name = None;
            let chunk = self.finish_chunk();
            self.compiled_chunks[chunk_offset + i] = chunk;
        }

        // Determine entry point: require main!/main
        let entry = self.functions.get("main!")
            .or_else(|| self.functions.get("main"))
            .copied()
            .ok_or_else(|| CompileError {
                message: "No 'main' or 'main!' function found".into(),
                line: 1,
                col: 0,
            })?;
        Ok(Program {
            chunks: self.compiled_chunks,
            entry,
        })
    }

    /// Compile a module (no entry point required). Returns chunks and name→index map.
    pub fn compile_module(mut self, root: &Node<'a>)
        -> Result<(Vec<Chunk>, HashMap<String, usize>), CompileError>
    {
        // First pass: register all function names with their chunk indices
        let mut func_defs: Vec<Node<'a>> = Vec::new();
        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            if child.kind() == "function_def" {
                let name_node = child.child_by_field_name("name").unwrap();
                let name = self.node_text(&name_node);
                self.functions.insert(name, func_defs.len());
                func_defs.push(child);
            }
        }

        // Pre-allocate chunk slots
        let num_funcs = func_defs.len();
        for _ in 0..num_funcs {
            self.compiled_chunks.push(Chunk::new());
        }

        // Second pass: compile each function body
        for (i, func_node) in func_defs.iter().enumerate() {
            let body = func_node.child_by_field_name("body").ok_or_else(|| {
                self.error("Function missing body".into(), func_node)
            })?;

            self.chunk = Chunk::new();
            self.locals.clear();
            self.scope_depth = 0;

            // Set current function name for TCO detection
            let name_node = func_node.child_by_field_name("name").unwrap();
            self.current_fn_name = Some(self.node_text(&name_node));

            if body.kind() == "lambda" {
                self.compile_lambda_body(&body)?;
            } else {
                self.tail_position = true;
                self.compile_expression(&body)?;
                self.tail_position = false;
            }

            self.current_fn_name = None;
            let chunk = self.finish_chunk();
            self.compiled_chunks[i] = chunk;
        }

        Ok((self.compiled_chunks, self.functions))
    }

    /// Compile a full program plus all inline test expressions as additional chunks.
    pub fn compile_program_with_tests(mut self, root: &Node<'a>) -> Result<TestProgram, CompileError> {
        let chunk_offset = self.compiled_chunks.len();

        // First pass: register all function names with their chunk indices
        let mut func_defs: Vec<Node<'a>> = Vec::new();
        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            if child.kind() == "function_def" {
                let name_node = child.child_by_field_name("name").unwrap();
                let name = self.node_text(&name_node);
                self.functions.insert(name, chunk_offset + func_defs.len());
                func_defs.push(child);
            }
        }

        let num_funcs = func_defs.len();

        if num_funcs > 0 {
            // Pre-allocate chunk slots for named functions
            for _ in 0..num_funcs {
                self.compiled_chunks.push(Chunk::new());
            }

            // Second pass: compile each function body
            for (i, func_node) in func_defs.iter().enumerate() {
                let body = func_node.child_by_field_name("body").ok_or_else(|| {
                    self.error("Function missing body".into(), func_node)
                })?;

                self.chunk = Chunk::new();
                self.locals.clear();
                self.scope_depth = 0;

                // Set current function name for TCO detection
                let name_node = func_node.child_by_field_name("name").unwrap();
                self.current_fn_name = Some(self.node_text(&name_node));

                if body.kind() == "lambda" {
                    self.compile_lambda_body(&body)?;
                } else {
                    self.tail_position = true;
                    self.compile_expression(&body)?;
                    self.tail_position = false;
                }

                self.current_fn_name = None;
                let chunk = self.finish_chunk();
                self.compiled_chunks[chunk_offset + i] = chunk;
            }
        } else {
            // No functions — push a no-op entry chunk so test runner has valid chunks
            let mut noop = Chunk::new();
            let unit_idx = noop.add_constant(Value::Unit);
            noop.emit(Op::LoadConst(unit_idx), 1, 0);
            noop.emit(Op::Return, 1, 0);
            self.compiled_chunks.push(noop);
        }

        // Third pass: collect and compile inline tests
        let mut compiled_tests = Vec::new();

        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            match child.kind() {
                "function_def" => {
                    let func_name = child
                        .child_by_field_name("name")
                        .map(|n| self.node_text(&n));

                    let mut child_cursor = child.walk();
                    for fc in child.children(&mut child_cursor) {
                        if fc.kind() == "where_block" {
                            let mut wb_cursor = fc.walk();
                            for test_node in fc.children(&mut wb_cursor) {
                                if test_node.kind() == "inline_test" {
                                    if let Some(ct) = self.compile_inline_test(&test_node, &func_name)? {
                                        compiled_tests.push(ct);
                                    }
                                }
                            }
                        }
                    }
                }
                "inline_test" => {
                    if let Some(ct) = self.compile_inline_test(&child, &None)? {
                        compiled_tests.push(ct);
                    }
                }
                _ => {}
            }
        }

        let entry = self.functions.get("main!")
            .or_else(|| self.functions.get("main"))
            .copied()
            .unwrap_or(chunk_offset);

        Ok(TestProgram {
            program: Program {
                chunks: self.compiled_chunks,
                entry,
            },
            tests: compiled_tests,
        })
    }

    /// Compile an inline test expression as a standalone chunk.
    fn compile_inline_test(
        &mut self,
        node: &Node<'a>,
        function: &Option<String>,
    ) -> Result<Option<CompiledTest>, CompileError> {
        let count = node.named_child_count();
        if count < 2 {
            return Ok(None);
        }

        // First named child is the string_literal (test name)
        let name_node = node.named_child(0).unwrap();
        let raw_name = self.node_text(&name_node);
        let name = raw_name
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or(&raw_name)
            .to_string();

        // Last named child is the expression
        let expr_node = node.named_child(count - 1).unwrap();

        // Compile the expression as a standalone chunk
        self.chunk = Chunk::new();
        self.locals.clear();
        self.scope_depth = 0;
        self.compile_expression(&expr_node)?;
        let chunk = self.finish_chunk();

        let chunk_idx = self.compiled_chunks.len();
        self.compiled_chunks.push(chunk);

        let start = node.start_position();
        let end = node.end_position();

        Ok(Some(CompiledTest {
            name,
            function: function.clone(),
            chunk_idx,
            line: start.row + 1,
            col: start.column + 1,
            end_line: end.row + 1,
            end_col: end.column + 1,
        }))
    }

    /// Compile a lambda node's parameters and body into the current chunk.
    /// Used by compile_program for function_def bodies.
    fn compile_lambda_body(&mut self, lambda_node: &Node<'a>) -> Result<(), CompileError> {
        let mut cursor = lambda_node.walk();
        let children: Vec<Node<'a>> = lambda_node.named_children(&mut cursor).collect();

        if children.is_empty() {
            // Empty lambda `||` — no params, no body (just Unit)
            let idx = self.chunk.add_constant(Value::Unit);
            self.emit(Op::LoadConst(idx), lambda_node);
            return Ok(());
        }

        // All named children except the last are parameters, last is the body
        let body = &children[children.len() - 1];
        let params = &children[..children.len() - 1];

        for param in params {
            let name = self.node_text(param);
            self.declare_local(&name);
        }

        // The body of a named function is in tail position (for TCO)
        let was_tail = self.tail_position;
        if self.current_fn_name.is_some() {
            self.tail_position = true;
        }
        let result = self.compile_expression(body);
        self.tail_position = was_tail;
        result
    }

    // -----------------------------------------------------------------------
    // Call expressions
    // -----------------------------------------------------------------------

    fn compile_call_expression(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let mut cursor = node.walk();
        let children: Vec<Node<'a>> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("Call expression missing callee".into(), node));
        }

        let callee = &children[0];
        let args = &children[1..];

        // Check if callee is a constructor (type_identifier = starts with uppercase)
        if callee.kind() == "type_identifier" {
            let tag = self.node_text(callee);
            // Compile the single argument as payload
            if args.len() == 1 {
                self.compile_expression(&args[0])?;
            } else if args.is_empty() {
                let idx = self.chunk.add_constant(Value::Unit);
                self.emit(Op::LoadConst(idx), node);
            } else {
                // Multiple args → wrap in tuple
                for arg in args {
                    self.compile_expression(arg)?;
                }
                self.emit(Op::MakeTuple(args.len() as u16), node);
            }
            let tag_idx = self.chunk.add_constant(Value::String(tag.into()));
            self.emit(Op::MakeEnum(tag_idx), node);
            return Ok(());
        }

        // TCO: detect self-recursive tail calls.
        // Conditions: callee is an identifier, matches current function name,
        // we're in tail position, and not inside a nested function/lambda.
        if self.tail_position && self.enclosing.is_empty() {
            if callee.kind() == "identifier" {
                let callee_name = self.node_text(callee);
                if let Some(ref fn_name) = self.current_fn_name {
                    if callee_name == *fn_name {
                        // Emit args (no function value), then TailCall
                        // Args are NOT in tail position themselves
                        let was_tail = self.tail_position;
                        self.tail_position = false;
                        for arg in args {
                            self.compile_expression(arg)?;
                        }
                        self.tail_position = was_tail;
                        self.emit(Op::TailCall(args.len() as u8), node);
                        return Ok(());
                    }
                }
            }
        }

        // Arguments are never in tail position
        let was_tail = self.tail_position;
        self.tail_position = false;

        // Check if callee is Module.method (field_expression with type_identifier object)
        if callee.kind() == "field_expression" {
            if let Some(qualified) = self.try_resolve_native(callee) {
                if let Some(fn_id) = self.natives.lookup(&qualified) {
                    // Emit args, then CallNative
                    for arg in args {
                        self.compile_expression(arg)?;
                    }
                    self.tail_position = was_tail;
                    self.emit(Op::CallNative(fn_id, args.len() as u8), node);
                    return Ok(());
                }
                // Check imported module functions (e.g., Math.add)
                if let Some(&chunk_idx) = self.functions.get(&qualified) {
                    let idx = self.chunk.add_constant(Value::Function(chunk_idx));
                    self.emit(Op::LoadConst(idx), node);
                    for arg in args {
                        self.compile_expression(arg)?;
                    }
                    self.tail_position = was_tail;
                    self.emit(Op::Call(args.len() as u8), node);
                    return Ok(());
                }
            }
        }

        // Regular function call
        self.compile_expression(callee)?;
        for arg in args {
            self.compile_expression(arg)?;
        }
        self.tail_position = was_tail;
        self.emit(Op::Call(args.len() as u8), node);
        Ok(())
    }

    /// Try to resolve a field_expression as a qualified native name like "Console.println!".
    /// Returns the qualified name if the object is a type_identifier (module name).
    fn try_resolve_native(&self, field_expr: &Node<'a>) -> Option<String> {
        let obj = field_expr.named_child(0)?;
        let method = field_expr.named_child(1)?;
        if obj.kind() == "type_identifier" {
            let module = self.node_text(&obj);
            let method_name = self.node_text(&method);
            // Handle effectful methods: the `!` is part of the identifier in grammar
            Some(format!("{}.{}", module, method_name))
        } else {
            None
        }
    }

    // -----------------------------------------------------------------------
    // Lambda expressions (inline)
    // -----------------------------------------------------------------------

    fn compile_lambda(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let mut cursor = node.walk();
        let children: Vec<Node<'a>> = node.named_children(&mut cursor).collect();

        // Extract params and body
        let (params, body) = if children.is_empty() {
            return Err(self.error("Lambda missing body".into(), node));
        } else {
            let body = &children[children.len() - 1];
            let params: Vec<String> = children[..children.len() - 1]
                .iter()
                .map(|n| self.node_text(n))
                .collect();
            (params, *body)
        };

        // Save current compiler state
        self.enter_function();

        // Declare parameters as locals
        for param in &params {
            self.declare_local(param);
        }

        // Compile the body
        self.compile_expression(&body)?;

        // Restore state and finalize the lambda's chunk
        let (func_chunk, captured) = self.leave_function();
        let chunk_idx = self.compiled_chunks.len();
        self.compiled_chunks.push(func_chunk);

        if captured.is_empty() {
            // Plain function — no captures
            let const_idx = self.chunk.add_constant(Value::Function(chunk_idx));
            self.emit(Op::LoadConst(const_idx), node);
        } else {
            // Closure — push each captured value, then MakeClosure
            for uv in &captured {
                if uv.is_local {
                    self.emit(Op::GetLocal(uv.index), node);
                } else {
                    self.emit(Op::GetUpvalue(uv.index as u8), node);
                }
            }
            self.emit(Op::MakeClosure(chunk_idx as u16, captured.len() as u8), node);
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Pipe operator
    // -----------------------------------------------------------------------

    fn compile_pipe(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let lhs = node.named_child(0).ok_or_else(|| {
            self.error("Pipe missing left operand".into(), node)
        })?;
        let rhs = node.named_child(1).ok_or_else(|| {
            self.error("Pipe missing right operand".into(), node)
        })?;

        if rhs.kind() == "call_expression" {
            // x |> f(a, b) → f(x, a, b)
            let mut cursor = rhs.walk();
            let rhs_children: Vec<Node<'a>> = rhs.named_children(&mut cursor).collect();

            if rhs_children.is_empty() {
                return Err(self.error("Pipe RHS call missing callee".into(), &rhs));
            }

            let callee = &rhs_children[0];

            // Check for native module call: x |> Module.method(a, b) → CallNative(x, a, b)
            if callee.kind() == "field_expression" {
                if let Some(qualified) = self.try_resolve_native(callee) {
                    if let Some(fn_id) = self.natives.lookup(&qualified) {
                        self.compile_expression(&lhs)?;    // pipe value as first arg
                        for arg in &rhs_children[1..] {
                            self.compile_expression(arg)?;
                        }
                        let arg_count = 1 + (rhs_children.len() - 1);
                        self.emit(Op::CallNative(fn_id, arg_count as u8), node);
                        return Ok(());
                    }
                    // Check imported module functions
                    if let Some(&chunk_idx) = self.functions.get(&qualified) {
                        let idx = self.chunk.add_constant(Value::Function(chunk_idx));
                        self.emit(Op::LoadConst(idx), node);
                        self.compile_expression(&lhs)?;
                        for arg in &rhs_children[1..] {
                            self.compile_expression(arg)?;
                        }
                        let arg_count = 1 + (rhs_children.len() - 1);
                        self.emit(Op::Call(arg_count as u8), node);
                        return Ok(());
                    }
                }
            }

            self.compile_expression(callee)?;  // push function
            self.compile_expression(&lhs)?;    // push pipe value as first arg
            for arg in &rhs_children[1..] {
                self.compile_expression(arg)?; // push remaining args
            }
            let arg_count = 1 + (rhs_children.len() - 1);
            self.emit(Op::Call(arg_count as u8), node);
        } else if rhs.kind() == "field_expression" {
            // x |> Module.method → CallNative(x) if native
            if let Some(qualified) = self.try_resolve_native(&rhs) {
                if let Some(fn_id) = self.natives.lookup(&qualified) {
                    self.compile_expression(&lhs)?;
                    self.emit(Op::CallNative(fn_id, 1), node);
                    return Ok(());
                }
                // Check imported module functions
                if let Some(&chunk_idx) = self.functions.get(&qualified) {
                    let idx = self.chunk.add_constant(Value::Function(chunk_idx));
                    self.emit(Op::LoadConst(idx), node);
                    self.compile_expression(&lhs)?;
                    self.emit(Op::Call(1), node);
                    return Ok(());
                }
            }
            // Not a native — regular call
            self.compile_expression(&rhs)?;
            self.compile_expression(&lhs)?;
            self.emit(Op::Call(1), node);
        } else {
            // x |> f → f(x)
            self.compile_expression(&rhs)?;   // push function
            self.compile_expression(&lhs)?;   // push arg
            self.emit(Op::Call(1), node);
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Data structures
    // -----------------------------------------------------------------------

    fn compile_list(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let count = node.named_child_count();
        for i in 0..count {
            let child = node.named_child(i).unwrap();
            self.compile_expression(&child)?;
        }
        self.emit(Op::MakeList(count as u16), node);
        Ok(())
    }

    fn compile_record(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let mut field_count = 0u16;
        for i in 0..node.named_child_count() {
            let field_init = node.named_child(i).unwrap();
            if field_init.kind() != "record_field_init" {
                continue;
            }
            let key_node = field_init.named_child(0).ok_or_else(|| {
                self.error("Record field missing key".into(), &field_init)
            })?;
            let val_node = field_init.named_child(1).ok_or_else(|| {
                self.error("Record field missing value".into(), &field_init)
            })?;
            // Push key as string constant
            let key_name = self.node_text(&key_node);
            let key_idx = self.chunk.add_constant(Value::String(key_name.into()));
            self.emit(Op::LoadConst(key_idx), &key_node);
            // Push value
            self.compile_expression(&val_node)?;
            field_count += 1;
        }
        self.emit(Op::MakeRecord(field_count), node);
        Ok(())
    }

    fn compile_field_access(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // field_expression: _expression '.' identifier
        let obj = node.named_child(0).ok_or_else(|| {
            self.error("Field access missing object".into(), node)
        })?;
        let field = node.named_child(1).ok_or_else(|| {
            self.error("Field access missing field name".into(), node)
        })?;
        self.compile_expression(&obj)?;
        let field_name = self.node_text(&field);
        let name_idx = self.chunk.add_constant(Value::String(field_name.into()));
        self.emit(Op::GetField(name_idx), node);
        Ok(())
    }

    fn compile_tuple(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let count = node.named_child_count();
        if count == 0 {
            // () is Unit
            let idx = self.chunk.add_constant(Value::Unit);
            self.emit(Op::LoadConst(idx), node);
        } else if count == 1 {
            // (expr) is parenthesized expression
            let inner = node.named_child(0).unwrap();
            self.compile_expression(&inner)?;
        } else {
            for i in 0..count {
                let child = node.named_child(i).unwrap();
                self.compile_expression(&child)?;
            }
            self.emit(Op::MakeTuple(count as u16), node);
        }
        Ok(())
    }

    fn compile_enum_constructor(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // struct_expression: type_identifier '{' record_field_init... '}'
        // For enums like Some(x), Ok(v) — grammar uses call_expression with type_identifier
        // This handles: TypeName { field: val } — not really enum, but named record
        // For now, treat as named record (same as record_expression but with a type tag)
        let type_node = node.named_child(0).ok_or_else(|| {
            self.error("Struct expression missing type".into(), node)
        })?;
        let type_name = self.node_text(&type_node);

        // If it has record fields, compile as tagged record
        let mut field_count = 0u16;
        for i in 1..node.named_child_count() {
            let field_init = node.named_child(i).unwrap();
            if field_init.kind() != "record_field_init" {
                continue;
            }
            let key_node = field_init.named_child(0).unwrap();
            let val_node = field_init.named_child(1).unwrap();
            let key_name = self.node_text(&key_node);
            let key_idx = self.chunk.add_constant(Value::String(key_name.into()));
            self.emit(Op::LoadConst(key_idx), &key_node);
            self.compile_expression(&val_node)?;
            field_count += 1;
        }
        if field_count > 0 {
            self.emit(Op::MakeRecord(field_count), node);
            let tag_idx = self.chunk.add_constant(Value::String(type_name.into()));
            self.emit(Op::MakeStruct(tag_idx), node);
        } else {
            let idx = self.chunk.add_constant(Value::Unit);
            self.emit(Op::LoadConst(idx), node);
            let tag_idx = self.chunk.add_constant(Value::String(type_name.into()));
            self.emit(Op::MakeEnum(tag_idx), node);
        }
        Ok(())
    }

    fn compile_nullary_constructor(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // type_identifier used as expression: None, True, etc.
        let name = self.node_text(node);
        let unit_idx = self.chunk.add_constant(Value::Unit);
        self.emit(Op::LoadConst(unit_idx), node);
        let tag_idx = self.chunk.add_constant(Value::String(name.into()));
        self.emit(Op::MakeEnum(tag_idx), node);
        Ok(())
    }

    fn compile_try(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // try_expression: _expression '?'
        // Semantics: if Err(e) or None, return early; else unwrap Ok(v)/Some(v) to v
        let expr = node.named_child(0).ok_or_else(|| {
            self.error("Try expression missing operand".into(), node)
        })?;
        self.compile_expression(&expr)?;

        // Store in a local for repeated access
        self.declare_local("__try_val");
        let val_slot = (self.locals.len() - 1) as u16;

        // Check tag == "Err"
        self.emit(Op::GetLocal(val_slot), node);
        self.emit(Op::EnumTag, node);
        let err_tag = self.chunk.add_constant(Value::String("Err".into()));
        self.emit(Op::LoadConst(err_tag), node);
        self.emit(Op::Eq, node);
        let not_err = self.emit(Op::JumpIfFalse(0), node);
        // It's Err — return the whole Err value
        self.emit(Op::GetLocal(val_slot), node);
        self.emit(Op::Return, node);
        self.chunk.patch_jump(not_err);

        // Check tag == "None"
        self.emit(Op::GetLocal(val_slot), node);
        self.emit(Op::EnumTag, node);
        let none_tag = self.chunk.add_constant(Value::String("None".into()));
        self.emit(Op::LoadConst(none_tag), node);
        self.emit(Op::Eq, node);
        let not_none = self.emit(Op::JumpIfFalse(0), node);
        // It's None — return None
        self.emit(Op::GetLocal(val_slot), node);
        self.emit(Op::Return, node);
        self.chunk.patch_jump(not_none);

        // It's Ok(v) or Some(v) — extract payload
        self.emit(Op::GetLocal(val_slot), node);
        self.emit(Op::EnumPayload, node);

        // Clean up the local (keep payload on top)
        self.locals.pop();
        self.emit(Op::CloseScope(1), node);

        Ok(())
    }

    fn compile_record_update(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // record_update: '{' '..' _expression (',' record_field_init)* '}'
        let mut cursor = node.walk();
        let children: Vec<Node<'a>> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("Record update missing base".into(), node));
        }

        // First named child is the base expression
        let base = &children[0];
        self.compile_expression(base)?;

        // Push field updates as key-value pairs
        let mut update_count = 0u16;
        for child in &children[1..] {
            if child.kind() == "record_field_init" {
                let key = child.named_child(0).unwrap();
                let val = child.named_child(1).unwrap();
                let key_name = self.node_text(&key);
                let key_idx = self.chunk.add_constant(Value::String(key_name.into()));
                self.emit(Op::LoadConst(key_idx), &key);
                self.compile_expression(&val)?;
                update_count += 1;
            }
        }

        self.emit(Op::UpdateRecord(update_count), node);
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Nested function helpers
    // -----------------------------------------------------------------------

    fn enter_function(&mut self) {
        self.enclosing.push(CompilerFrame {
            chunk: std::mem::replace(&mut self.chunk, Chunk::new()),
            locals: std::mem::take(&mut self.locals),
            scope_depth: self.scope_depth,
            upvalues: std::mem::take(&mut self.upvalues),
        });
        self.scope_depth = 0;
    }

    /// Finalize the inner function and restore enclosing state.
    /// Returns (chunk, upvalues) so the caller can emit MakeClosure if needed.
    fn leave_function(&mut self) -> (Chunk, Vec<Upvalue>) {
        let func_chunk = self.finish_chunk();
        let func_upvalues = std::mem::take(&mut self.upvalues);
        let frame = self.enclosing.pop().unwrap();
        self.chunk = frame.chunk;
        self.locals = frame.locals;
        self.scope_depth = frame.scope_depth;
        self.upvalues = frame.upvalues;
        (func_chunk, func_upvalues)
    }

    /// Finalize the current chunk (append Return if needed) without consuming self.
    fn finish_chunk(&mut self) -> Chunk {
        let mut chunk = std::mem::replace(&mut self.chunk, Chunk::new());
        chunk.ensure_return();
        chunk
    }

    // -----------------------------------------------------------------------
    // Finish
    // -----------------------------------------------------------------------

    pub fn finish(mut self) -> Chunk {
        self.chunk.ensure_return();
        self.chunk
    }

    /// Finish compilation as a Program (needed when lambdas create extra chunks).
    pub fn finish_as_program(mut self) -> Program {
        let main_chunk = self.finish_chunk();
        self.compiled_chunks.push(main_chunk);
        let entry = self.compiled_chunks.len() - 1;
        Program {
            chunks: self.compiled_chunks,
            entry,
        }
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    fn emit(&mut self, op: Op, node: &Node) -> usize {
        let line = node.start_position().row + 1;
        let col = node.start_position().column;
        self.chunk.emit(op, line, col)
    }

    fn node_text(&self, node: &Node) -> String {
        node.utf8_text(self.source.as_bytes()).unwrap_or("").to_string()
    }

}

// ---------------------------------------------------------------------------
// Constant folding
// ---------------------------------------------------------------------------

/// Evaluate a binary operation on two constant values at compile time.
fn eval_const_binary(op: &str, a: &Value, b: &Value) -> Option<Value> {
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => match op {
            "+" => Some(Value::Int(a.wrapping_add(*b))),
            "-" => Some(Value::Int(a.wrapping_sub(*b))),
            "*" => Some(Value::Int(a.wrapping_mul(*b))),
            "/" if *b != 0 => Some(Value::Int(a.wrapping_div(*b))),
            "%" if *b != 0 => Some(Value::Int(a.wrapping_rem(*b))),
            "==" => Some(Value::Bool(a == b)),
            "!=" => Some(Value::Bool(a != b)),
            "<" => Some(Value::Bool(a < b)),
            ">" => Some(Value::Bool(a > b)),
            "<=" => Some(Value::Bool(a <= b)),
            ">=" => Some(Value::Bool(a >= b)),
            _ => None,
        },
        (Value::Float(a), Value::Float(b)) => match op {
            "+" => Some(Value::Float(a + b)),
            "-" => Some(Value::Float(a - b)),
            "*" => Some(Value::Float(a * b)),
            "/" if *b != 0.0 => Some(Value::Float(a / b)),
            _ => None,
        },
        (Value::Bool(a), Value::Bool(b)) => match op {
            "==" => Some(Value::Bool(a == b)),
            "!=" => Some(Value::Bool(a != b)),
            _ => None,
        },
        (Value::String(a), Value::String(b)) => match op {
            "+" => {
                let mut r = String::with_capacity(a.len() + b.len());
                r.push_str(a);
                r.push_str(b);
                Some(Value::String(r.into()))
            }
            "==" => Some(Value::Bool(a == b)),
            "!=" => Some(Value::Bool(a != b)),
            _ => None,
        },
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn unescape(s: &str) -> String {
    match s {
        "\\n" => "\n".into(),
        "\\t" => "\t".into(),
        "\\r" => "\r".into(),
        "\\\\" => "\\".into(),
        "\\\"" => "\"".into(),
        "\\0" => "\0".into(),
        _ if s.len() == 2 && s.starts_with('\\') => s[1..].into(),
        _ => s.into(),
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

impl Compiler<'_> {
    fn error(&self, message: String, node: &Node) -> CompileError {
        CompileError {
            message,
            line: node.start_position().row + 1,
            col: node.start_position().column,
        }
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}] {}", self.line, self.col, self.message)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;
    use crate::vm::vm::Vm;

    /// Helper: wrap expr in a function def, parse, compile the body, execute.
    fn eval_expr(expr: &str) -> Value {
        let source = format!("x : () -> Unknown\nx = {}", expr);
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .expect("Error loading Baseline grammar");
        let tree = parser.parse(&source, None).expect("Parse failed");
        let root = tree.root_node();

        let func_def = root.named_child(0)
            .expect("No function def found");
        assert_eq!(func_def.kind(), "function_def",
            "Expected function_def, got {}. Tree: {}",
            func_def.kind(), root.to_sexp());
        let body = func_def.child_by_field_name("body")
            .expect("No body field in function_def");

        let natives = NativeRegistry::new();
        let mut compiler = Compiler::new(&source, &natives);
        compiler
            .compile_expression(&body)
            .unwrap_or_else(|e| panic!("Compile error: {}. Tree: {}", e, root.to_sexp()));
        let program = compiler.finish_as_program();

        let mut vm = Vm::new();
        vm.execute_program(&program).expect("VM error")
    }

    /// Helper: parse a full program, compile all function_defs, execute the last one.
    fn eval_program(source: &str) -> Value {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .expect("Error loading Baseline grammar");
        let tree = parser.parse(source, None).expect("Parse failed");
        let root = tree.root_node();

        let natives = NativeRegistry::new();
        let compiler = Compiler::new(source, &natives);
        let program = compiler
            .compile_program(&root)
            .unwrap_or_else(|e| panic!("Compile error: {}. Tree: {}", e, root.to_sexp()));

        let mut vm = Vm::new();
        vm.execute_program(&program).expect("VM error")
    }

    // ===== Literal tests =====

    #[test]
    fn compile_integer_literal() {
        assert_eq!(eval_expr("42"), Value::Int(42));
    }

    #[test]
    fn compile_float_literal() {
        assert_eq!(eval_expr("3.14"), Value::Float(3.14));
    }

    #[test]
    fn compile_bool_true() {
        assert_eq!(eval_expr("true"), Value::Bool(true));
    }

    #[test]
    fn compile_bool_false() {
        assert_eq!(eval_expr("false"), Value::Bool(false));
    }

    #[test]
    fn compile_string_literal() {
        assert_eq!(eval_expr("\"hello\""), Value::String("hello".into()));
    }

    #[test]
    fn compile_empty_string() {
        assert_eq!(eval_expr("\"\""), Value::String("".into()));
    }

    // ===== Arithmetic =====

    #[test]
    fn compile_addition() {
        assert_eq!(eval_expr("1 + 2"), Value::Int(3));
    }

    #[test]
    fn compile_subtraction() {
        assert_eq!(eval_expr("10 - 3"), Value::Int(7));
    }

    #[test]
    fn compile_multiplication() {
        assert_eq!(eval_expr("6 * 7"), Value::Int(42));
    }

    #[test]
    fn compile_division() {
        assert_eq!(eval_expr("15 / 3"), Value::Int(5));
    }

    #[test]
    fn compile_modulo() {
        assert_eq!(eval_expr("10 % 3"), Value::Int(1));
    }

    #[test]
    fn compile_precedence() {
        assert_eq!(eval_expr("1 + 2 * 3"), Value::Int(7));
    }

    #[test]
    fn compile_nested_arithmetic() {
        assert_eq!(eval_expr("(1 + 2) * 3"), Value::Int(9));
    }

    #[test]
    fn compile_complex_expression() {
        assert_eq!(eval_expr("(10 - 2) / (1 + 3)"), Value::Int(2));
    }

    // ===== Unary =====

    #[test]
    fn compile_negate() {
        assert_eq!(eval_expr("-5"), Value::Int(-5));
    }

    #[test]
    fn compile_not() {
        assert_eq!(eval_expr("not true"), Value::Bool(false));
    }

    #[test]
    fn compile_double_negate() {
        assert_eq!(eval_expr("- -5"), Value::Int(5));
    }

    // ===== Comparisons =====

    #[test]
    fn compile_eq() { assert_eq!(eval_expr("5 == 5"), Value::Bool(true)); }
    #[test]
    fn compile_ne() { assert_eq!(eval_expr("5 != 3"), Value::Bool(true)); }
    #[test]
    fn compile_lt() { assert_eq!(eval_expr("3 < 5"), Value::Bool(true)); }
    #[test]
    fn compile_gt() { assert_eq!(eval_expr("5 > 3"), Value::Bool(true)); }
    #[test]
    fn compile_le() { assert_eq!(eval_expr("3 <= 3"), Value::Bool(true)); }
    #[test]
    fn compile_ge() { assert_eq!(eval_expr("5 >= 5"), Value::Bool(true)); }

    // ===== Boolean short-circuit =====

    #[test]
    fn compile_and_both_true() {
        assert_eq!(eval_expr("true && true"), Value::Bool(true));
    }
    #[test]
    fn compile_and_short_circuit() {
        assert_eq!(eval_expr("false && true"), Value::Bool(false));
    }
    #[test]
    fn compile_or_both_false() {
        assert_eq!(eval_expr("false || false"), Value::Bool(false));
    }
    #[test]
    fn compile_or_short_circuit() {
        assert_eq!(eval_expr("true || false"), Value::Bool(true));
    }

    // ===== String interpolation =====

    #[test]
    fn compile_string_interpolation() {
        assert_eq!(eval_expr("\"${1 + 2}\""), Value::String("3".into()));
    }
    #[test]
    fn compile_string_interpolation_with_text() {
        assert_eq!(
            eval_expr("\"count: ${42}\""),
            Value::String("count: 42".into()),
        );
    }
    #[test]
    fn compile_mixed_interpolation() {
        assert_eq!(
            eval_expr("\"a${1}b${2}c\""),
            Value::String("a1b2c".into()),
        );
    }

    // ===== Let bindings & blocks =====

    #[test]
    fn compile_let_and_use() {
        // let x = 10, then x + 1
        assert_eq!(
            eval_expr("{\n  let x = 10\n  x + 1\n}"),
            Value::Int(11),
        );
    }

    #[test]
    fn compile_multiple_lets() {
        assert_eq!(
            eval_expr("{\n  let a = 3\n  let b = 4\n  a + b\n}"),
            Value::Int(7),
        );
    }

    #[test]
    fn compile_nested_blocks() {
        assert_eq!(
            eval_expr("{\n  let x = 1\n  let y = {\n    let z = 10\n    z + x\n  }\n  y\n}"),
            Value::Int(11),
        );
    }

    #[test]
    fn compile_variable_shadowing() {
        assert_eq!(
            eval_expr("{\n  let x = 1\n  let x = 2\n  x\n}"),
            Value::Int(2),
        );
    }

    // ===== If/Else =====

    #[test]
    fn compile_if_true_branch() {
        assert_eq!(
            eval_expr("if true then 1 else 2"),
            Value::Int(1),
        );
    }

    #[test]
    fn compile_if_false_branch() {
        assert_eq!(
            eval_expr("if false then 1 else 2"),
            Value::Int(2),
        );
    }

    #[test]
    fn compile_if_with_expression_condition() {
        assert_eq!(
            eval_expr("if 3 > 2 then 10 else 20"),
            Value::Int(10),
        );
    }

    #[test]
    fn compile_if_with_let() {
        assert_eq!(
            eval_expr("{\n  let x = 5\n  if x > 3 then x + 1 else 0\n}"),
            Value::Int(6),
        );
    }

    // ===== Match =====

    #[test]
    fn compile_match_literal() {
        assert_eq!(
            eval_expr("match 2\n  1 -> 10\n  2 -> 20\n  _ -> 99"),
            Value::Int(20),
        );
    }

    #[test]
    fn compile_match_wildcard() {
        assert_eq!(
            eval_expr("match 42\n  _ -> 99"),
            Value::Int(99),
        );
    }

    #[test]
    fn compile_match_binding() {
        assert_eq!(
            eval_expr("match 5\n  x -> x + 1"),
            Value::Int(6),
        );
    }

    #[test]
    fn compile_match_first_arm_wins() {
        assert_eq!(
            eval_expr("match 1\n  1 -> 10\n  1 -> 20\n  _ -> 99"),
            Value::Int(10),
        );
    }

    // ===== Range =====

    #[test]
    fn compile_range_expression() {
        assert_eq!(
            eval_expr("1..4"),
            Value::List(Rc::new(vec![Value::Int(1), Value::Int(2), Value::Int(3)])),
        );
    }

    // ===== For loops =====

    #[test]
    fn compile_for_loop_unit() {
        // For loop produces Unit
        assert_eq!(
            eval_expr("for x in 1..4 do x + 1"),
            Value::Unit,
        );
    }

    // ===== Named function definition + call =====

    #[test]
    fn compile_named_function_call() {
        let result = eval_program(
            "double : Int -> Int\n\
             double = |x| x * 2\n\
             main : () -> Int\n\
             main = double(5)"
        );
        assert_eq!(result, Value::Int(10));
    }

    #[test]
    fn compile_two_arg_function() {
        let result = eval_program(
            "add : (Int, Int) -> Int\n\
             add = |a, b| a + b\n\
             main : () -> Int\n\
             main = add(3, 4)"
        );
        assert_eq!(result, Value::Int(7));
    }

    #[test]
    fn compile_chained_function_calls() {
        let result = eval_program(
            "double : Int -> Int\n\
             double = |x| x * 2\n\
             inc : Int -> Int\n\
             inc = |x| x + 1\n\
             main : () -> Int\n\
             main = inc(double(3))"
        );
        assert_eq!(result, Value::Int(7));
    }

    #[test]
    fn compile_recursive_function() {
        let result = eval_program(
            "factorial : Int -> Int\n\
             factorial = |n| if n <= 1 then 1 else n * factorial(n - 1)\n\
             main : () -> Int\n\
             main = factorial(5)"
        );
        assert_eq!(result, Value::Int(120));
    }

    #[test]
    fn compile_zero_arg_function() {
        let result = eval_program(
            "answer : () -> Int\n\
             answer = 42\n\
             main : () -> Int\n\
             main = answer()"
        );
        assert_eq!(result, Value::Int(42));
    }

    // ===== Lambdas as arguments =====

    #[test]
    fn compile_lambda_as_value() {
        // Lambda assigned to let binding, then called
        let result = eval_expr(
            "{\n  let f = |x| x + 1\n  f(3)\n}"
        );
        assert_eq!(result, Value::Int(4));
    }

    #[test]
    fn compile_lambda_two_params() {
        let result = eval_expr(
            "{\n  let add = |a, b| a + b\n  add(3, 4)\n}"
        );
        assert_eq!(result, Value::Int(7));
    }

    #[test]
    fn compile_lambda_passed_to_function() {
        let result = eval_program(
            "apply : ((Int -> Int), Int) -> Int\n\
             apply = |f, x| f(x)\n\
             main : () -> Int\n\
             main = apply(|x| x * 3, 5)"
        );
        assert_eq!(result, Value::Int(15));
    }

    // ===== Pipe operator =====

    #[test]
    fn compile_pipe_simple() {
        let result = eval_program(
            "double : Int -> Int\n\
             double = |x| x * 2\n\
             main : () -> Int\n\
             main = 5 |> double"
        );
        assert_eq!(result, Value::Int(10));
    }

    #[test]
    fn compile_pipe_chain() {
        let result = eval_program(
            "double : Int -> Int\n\
             double = |x| x * 2\n\
             inc : Int -> Int\n\
             inc = |x| x + 1\n\
             main : () -> Int\n\
             main = 3 |> double |> inc"
        );
        assert_eq!(result, Value::Int(7));
    }

    #[test]
    fn compile_pipe_with_call_args() {
        let result = eval_program(
            "add : (Int, Int) -> Int\n\
             add = |a, b| a + b\n\
             main : () -> Int\n\
             main = 3 |> add(4)"
        );
        assert_eq!(result, Value::Int(7));
    }

    #[test]
    fn compile_pipe_to_lambda() {
        let result = eval_program(
            "main : () -> Int\n\
             main = 5 |> |x| x * 2"
        );
        assert_eq!(result, Value::Int(10));
    }

    // ===== Closures =====

    #[test]
    fn compile_closure_captures_local() {
        let result = eval_expr(
            "{\n  let x = 10\n  let f = |y| x + y\n  f(5)\n}"
        );
        assert_eq!(result, Value::Int(15));
    }

    #[test]
    fn compile_closure_captures_multiple() {
        let result = eval_expr(
            "{\n  let a = 1\n  let b = 2\n  let f = |c| a + b + c\n  f(3)\n}"
        );
        assert_eq!(result, Value::Int(6));
    }

    #[test]
    fn compile_closure_returned_from_function() {
        let result = eval_program(
            "make_adder : Int -> (Int -> Int)\n\
             make_adder = |n| |x| n + x\n\
             main : () -> Int\n\
             main = {\n  let add5 = make_adder(5)\n  add5(3)\n}"
        );
        assert_eq!(result, Value::Int(8));
    }

    #[test]
    fn compile_closure_as_callback() {
        let result = eval_program(
            "apply : ((Int -> Int), Int) -> Int\n\
             apply = |f, x| f(x)\n\
             main : () -> Int\n\
             main = {\n  let offset = 100\n  apply(|x| x + offset, 5)\n}"
        );
        assert_eq!(result, Value::Int(105));
    }

    // ===== List literals =====

    #[test]
    fn compile_list_literal() {
        assert_eq!(
            eval_expr("[1, 2, 3]"),
            Value::List(Rc::new(vec![Value::Int(1), Value::Int(2), Value::Int(3)])),
        );
    }

    #[test]
    fn compile_empty_list() {
        assert_eq!(eval_expr("[]"), Value::List(Rc::new(vec![])));
    }

    #[test]
    fn compile_list_with_expressions() {
        assert_eq!(
            eval_expr("[1 + 1, 2 * 3]"),
            Value::List(Rc::new(vec![Value::Int(2), Value::Int(6)])),
        );
    }

    // ===== Records =====

    #[test]
    fn compile_record_literal() {
        assert_eq!(
            eval_expr("{ x: 1, y: 2 }"),
            Value::Record(Rc::new(vec![
                ("x".into(), Value::Int(1)),
                ("y".into(), Value::Int(2)),
            ])),
        );
    }

    #[test]
    fn compile_record_field_access() {
        assert_eq!(
            eval_expr("{\n  let r = { x: 10, y: 20 }\n  r.x\n}"),
            Value::Int(10),
        );
    }

    #[test]
    fn compile_record_nested_field_access() {
        assert_eq!(
            eval_expr("{\n  let r = { x: 10, y: 20 }\n  r.x + r.y\n}"),
            Value::Int(30),
        );
    }

    // ===== Tuples =====

    #[test]
    fn compile_tuple_literal() {
        assert_eq!(
            eval_expr("(1, \"hello\", true)"),
            Value::Tuple(Rc::new(vec![Value::Int(1), Value::String("hello".into()), Value::Bool(true)])),
        );
    }

    #[test]
    fn compile_unit_tuple() {
        assert_eq!(eval_expr("()"), Value::Unit);
    }

    // ===== Enums =====

    #[test]
    fn compile_some_constructor() {
        assert_eq!(
            eval_expr("Some(42)"),
            Value::Enum("Some".into(), Rc::new(Value::Int(42))),
        );
    }

    #[test]
    fn compile_none_constructor() {
        assert_eq!(
            eval_expr("None"),
            Value::Enum("None".into(), Rc::new(Value::Unit)),
        );
    }

    #[test]
    fn compile_ok_constructor() {
        assert_eq!(
            eval_expr("Ok(10)"),
            Value::Enum("Ok".into(), Rc::new(Value::Int(10))),
        );
    }

    #[test]
    fn compile_err_constructor() {
        assert_eq!(
            eval_expr("Err(\"bad\")"),
            Value::Enum("Err".into(), Rc::new(Value::String("bad".into()))),
        );
    }

    // ===== Enum pattern matching =====

    #[test]
    fn compile_match_some() {
        assert_eq!(
            eval_expr("match Some(42)\n  Some(v) -> v + 1\n  None -> 0"),
            Value::Int(43),
        );
    }

    #[test]
    fn compile_match_none() {
        assert_eq!(
            eval_expr("match None\n  Some(v) -> v + 1\n  None -> 0"),
            Value::Int(0),
        );
    }

    #[test]
    fn compile_match_ok_err() {
        assert_eq!(
            eval_expr("match Ok(10)\n  Ok(v) -> v * 2\n  Err(e) -> 0"),
            Value::Int(20),
        );
    }

    // ===== Try operator =====

    #[test]
    fn compile_try_ok_unwraps() {
        let result = eval_program(
            "main : () -> Unknown\n\
             main = {\n  let x = Ok(42)\n  let v = x?\n  v + 1\n}"
        );
        assert_eq!(result, Value::Int(43));
    }

    #[test]
    fn compile_try_err_returns_early() {
        let result = eval_program(
            "main : () -> Unknown\n\
             main = {\n  let x = Err(\"oops\")\n  let v = x?\n  v + 1\n}"
        );
        assert_eq!(result, Value::Enum("Err".into(), Rc::new(Value::String("oops".into()))));
    }

    // ===== Record update =====

    #[test]
    fn compile_record_update() {
        assert_eq!(
            eval_expr("{\n  let r = { x: 1, y: 2 }\n  { ..r, x: 10 }\n}"),
            Value::Record(Rc::new(vec![("x".into(), Value::Int(10)), ("y".into(), Value::Int(2))])),
        );
    }

    // ===== Native function calls =====

    #[test]
    fn compile_string_length() {
        assert_eq!(
            eval_expr("String.length(\"hello\")"),
            Value::Int(5),
        );
    }

    #[test]
    fn compile_string_to_upper() {
        assert_eq!(
            eval_expr("String.to_upper(\"hello\")"),
            Value::String("HELLO".into()),
        );
    }

    #[test]
    fn compile_math_abs() {
        assert_eq!(eval_expr("Math.abs(-42)"), Value::Int(42));
    }

    #[test]
    fn compile_math_min() {
        assert_eq!(eval_expr("Math.min(3, 7)"), Value::Int(3));
    }

    #[test]
    fn compile_math_pow() {
        assert_eq!(eval_expr("Math.pow(2, 10)"), Value::Int(1024));
    }

    #[test]
    fn compile_list_length() {
        assert_eq!(eval_expr("List.length([1, 2, 3])"), Value::Int(3));
    }

    #[test]
    fn compile_list_head() {
        assert_eq!(
            eval_expr("List.head([10, 20])"),
            Value::Enum("Some".into(), Rc::new(Value::Int(10))),
        );
    }

    #[test]
    fn compile_list_reverse() {
        assert_eq!(
            eval_expr("List.reverse([1, 2, 3])"),
            Value::List(Rc::new(vec![Value::Int(3), Value::Int(2), Value::Int(1)])),
        );
    }

    #[test]
    fn compile_option_unwrap() {
        assert_eq!(eval_expr("Option.unwrap(Some(42))"), Value::Int(42));
    }

    #[test]
    fn compile_option_is_some() {
        assert_eq!(eval_expr("Option.is_some(Some(1))"), Value::Bool(true));
        assert_eq!(eval_expr("Option.is_none(None)"), Value::Bool(true));
    }

    #[test]
    fn compile_result_unwrap() {
        assert_eq!(eval_expr("Result.unwrap(Ok(10))"), Value::Int(10));
    }

    #[test]
    fn compile_result_is_ok() {
        assert_eq!(eval_expr("Result.is_ok(Ok(1))"), Value::Bool(true));
        assert_eq!(eval_expr("Result.is_err(Err(\"bad\"))"), Value::Bool(true));
    }

    #[test]
    fn compile_int_to_string() {
        assert_eq!(
            eval_expr("Int.to_string(42)"),
            Value::String("42".into()),
        );
    }

    #[test]
    fn compile_console_println() {
        // Console.println returns Unit
        assert_eq!(
            eval_expr("Console.println(\"test\")"),
            Value::Unit,
        );
    }

    // ===== Native calls with pipe =====

    #[test]
    fn compile_pipe_to_native() {
        assert_eq!(
            eval_expr("\"hello\" |> String.length"),
            Value::Int(5),
        );
    }

    #[test]
    fn compile_pipe_to_native_with_args() {
        assert_eq!(
            eval_expr("\"hello world\" |> String.contains(\"world\")"),
            Value::Bool(true),
        );
    }

    // ===== HOF native calls =====

    #[test]
    fn compile_list_map() {
        assert_eq!(
            eval_expr("List.map([1, 2, 3], |x| x * 2)"),
            Value::List(Rc::new(vec![Value::Int(2), Value::Int(4), Value::Int(6)])),
        );
    }

    #[test]
    fn compile_list_filter() {
        assert_eq!(
            eval_expr("List.filter([1, 2, 3, 4, 5], |x| x > 3)"),
            Value::List(Rc::new(vec![Value::Int(4), Value::Int(5)])),
        );
    }

    #[test]
    fn compile_list_fold() {
        assert_eq!(
            eval_expr("List.fold([1, 2, 3], 0, |acc, x| acc + x)"),
            Value::Int(6),
        );
    }

    #[test]
    fn compile_list_find() {
        assert_eq!(
            eval_expr("List.find([1, 2, 3], |x| x == 2)"),
            Value::Enum("Some".into(), Rc::new(Value::Int(2))),
        );
    }

    #[test]
    fn compile_list_find_not_found() {
        assert_eq!(
            eval_expr("List.find([1, 2, 3], |x| x > 10)"),
            Value::Enum("None".into(), Rc::new(Value::Unit)),
        );
    }

    #[test]
    fn compile_option_map_some() {
        assert_eq!(
            eval_expr("Option.map(Some(5), |x| x * 2)"),
            Value::Enum("Some".into(), Rc::new(Value::Int(10))),
        );
    }

    #[test]
    fn compile_option_map_none() {
        assert_eq!(
            eval_expr("Option.map(None, |x| x * 2)"),
            Value::Enum("None".into(), Rc::new(Value::Unit)),
        );
    }

    #[test]
    fn compile_result_map_ok() {
        assert_eq!(
            eval_expr("Result.map(Ok(5), |x| x + 1)"),
            Value::Enum("Ok".into(), Rc::new(Value::Int(6))),
        );
    }

    #[test]
    fn compile_result_map_err() {
        assert_eq!(
            eval_expr("Result.map(Err(\"bad\"), |x| x + 1)"),
            Value::Enum("Err".into(), Rc::new(Value::String("bad".into()))),
        );
    }

    // ===== HOFs with pipes =====

    #[test]
    fn compile_pipe_list_map() {
        assert_eq!(
            eval_expr("[1, 2, 3] |> List.map(|x| x * 10)"),
            Value::List(Rc::new(vec![Value::Int(10), Value::Int(20), Value::Int(30)])),
        );
    }

    #[test]
    fn compile_pipe_chain_natives() {
        assert_eq!(
            eval_expr("[3, 1, 2] |> List.reverse |> List.length"),
            Value::Int(3),
        );
    }

    // ===== HOFs with closures capturing variables =====

    #[test]
    fn compile_list_map_with_closure() {
        assert_eq!(
            eval_expr("{\n  let offset = 100\n  List.map([1, 2, 3], |x| x + offset)\n}"),
            Value::List(Rc::new(vec![Value::Int(101), Value::Int(102), Value::Int(103)])),
        );
    }

    // ===== Constant folding tests =====

    #[test]
    fn fold_int_addition() {
        assert_eq!(eval_expr("1 + 2"), Value::Int(3));
    }

    #[test]
    fn fold_chained_arithmetic() {
        assert_eq!(eval_expr("1 + 2 + 3"), Value::Int(6));
    }

    #[test]
    fn fold_int_multiplication() {
        assert_eq!(eval_expr("6 * 7"), Value::Int(42));
    }

    #[test]
    fn fold_int_comparison() {
        assert_eq!(eval_expr("5 < 10"), Value::Bool(true));
        assert_eq!(eval_expr("10 <= 10"), Value::Bool(true));
        assert_eq!(eval_expr("10 > 5"), Value::Bool(true));
    }

    #[test]
    fn fold_negation() {
        assert_eq!(eval_expr("-42"), Value::Int(-42));
    }

    #[test]
    fn fold_not() {
        assert_eq!(eval_expr("not true"), Value::Bool(false));
        assert_eq!(eval_expr("not false"), Value::Bool(true));
    }

    #[test]
    fn fold_div_by_zero_not_folded() {
        // Division by zero should NOT be folded — it will be a runtime error
        // but the compilation should succeed (the error happens at runtime)
        let source = "x : () -> Int\nx = 10 / 0";
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&tree_sitter_baseline::LANGUAGE.into()).unwrap();
        let tree = parser.parse(source, None).unwrap();
        let root = tree.root_node();
        let func_def = root.named_child(0).unwrap();
        let body = func_def.child_by_field_name("body").unwrap();
        let natives = NativeRegistry::new();
        let mut compiler = Compiler::new(source, &natives);
        // Should compile without error (folding skips div-by-zero)
        compiler.compile_expression(&body).unwrap();
    }

    #[test]
    fn fold_nested_expression() {
        // (2 + 3) * (4 - 1) = 5 * 3 = 15
        assert_eq!(eval_expr("(2 + 3) * (4 - 1)"), Value::Int(15));
    }

    // ===== Tail Call Optimization tests =====

    #[test]
    fn tco_factorial_accumulator() {
        // Tail-recursive factorial with accumulator
        let source = r#"
fact : (Int, Int) -> Int
fact = |n, acc| if n <= 1 then acc else fact(n - 1, n * acc)

main : () -> Int
main = fact(10, 1)
"#;
        assert_eq!(eval_program(source), Value::Int(3628800));
    }

    #[test]
    fn tco_emits_tail_call_opcode() {
        // Verify the compiler actually emits TailCall for a self-recursive call
        let source = r#"
countdown : (Int) -> Int
countdown = |n| if n <= 0 then 0 else countdown(n - 1)

main : () -> Int
main = countdown(5)
"#;
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .expect("Error loading Baseline grammar");
        let tree = parser.parse(source, None).expect("Parse failed");
        let root = tree.root_node();

        let natives = NativeRegistry::new();
        let compiler = Compiler::new(source, &natives);
        let program = compiler
            .compile_program(&root)
            .unwrap_or_else(|e| panic!("Compile error: {}", e));

        // Find the countdown function's chunk and check it contains TailCall
        let countdown_chunk = &program.chunks[0]; // first function defined
        let has_tail_call = countdown_chunk.code.iter().any(|op| matches!(op, Op::TailCall(_)));
        assert!(has_tail_call, "Expected TailCall opcode in countdown chunk, got: {:?}", countdown_chunk.code);
    }

    #[test]
    fn tco_deep_recursion_no_overflow() {
        // Without TCO, 50,000 iterations would blow the 1024-frame call stack
        let source = r#"
countdown : (Int) -> Int
countdown = |n| if n <= 0 then 0 else countdown(n - 1)

main : () -> Int
main = countdown(50000)
"#;
        assert_eq!(eval_program(source), Value::Int(0));
    }

    #[test]
    fn tco_non_tail_call_not_optimized() {
        // fib(n) = fib(n-1) + fib(n-2) is NOT tail recursive
        // (the + happens after the calls)
        let source = r#"
fib : (Int) -> Int
fib = |n| if n <= 1 then n else fib(n - 1) + fib(n - 2)

main : () -> Int
main = fib(10)
"#;
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .expect("Error loading Baseline grammar");
        let tree = parser.parse(source, None).expect("Parse failed");
        let root = tree.root_node();

        let natives = NativeRegistry::new();
        let compiler = Compiler::new(source, &natives);
        let program = compiler
            .compile_program(&root)
            .unwrap_or_else(|e| panic!("Compile error: {}", e));

        // fib's calls are NOT in tail position, so no TailCall should be emitted
        let fib_chunk = &program.chunks[0];
        let has_tail_call = fib_chunk.code.iter().any(|op| matches!(op, Op::TailCall(_)));
        assert!(!has_tail_call, "fib should NOT have TailCall opcode, got: {:?}", fib_chunk.code);

        // But it should still produce correct results
        let mut vm = Vm::new();
        assert_eq!(vm.execute_program(&program).unwrap(), Value::Int(55));
    }

    #[test]
    fn tco_match_tail_position() {
        // Tail calls in match arms should be optimized
        let source = r#"
count : (Int, Int) -> Int
count = |n, acc| match n
  0 -> acc
  _ -> count(n - 1, acc + 1)

main : () -> Int
main = count(100, 0)
"#;
        assert_eq!(eval_program(source), Value::Int(100));
    }
}
