use tree_sitter::Node;

use super::chunk::{Chunk, Op};
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

pub struct Compiler<'a> {
    source: &'a str,
    chunk: Chunk,
    /// Local variable stack — mirrors the VM's value stack at compile time.
    locals: Vec<Local>,
    /// Current scope depth (0 = top-level).
    scope_depth: usize,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Compiler {
            source,
            chunk: Chunk::new(),
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    /// Compile a tree-sitter expression node into bytecode.
    pub fn compile_expression(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let kind = node.kind();
        match kind {
            "integer_literal" => {
                let text = self.node_text(node);
                let val: i64 = text.parse().map_err(|_| {
                    self.error(format!("Invalid integer: {}", text), node)
                })?;
                let idx = self.chunk.add_constant(Value::Int(val));
                self.emit(Op::LoadConst(idx), node);
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
            "parenthesized_expression" | "tuple_expression" => {
                if node.named_child_count() == 1 {
                    let inner = node.named_child(0).unwrap();
                    self.compile_expression(&inner)?;
                } else {
                    return Err(self.error(
                        "Tuples not yet supported in VM".into(), node,
                    ));
                }
            }
            "unary_expression" => {
                self.compile_unary(node)?;
            }
            "binary_expression" => {
                self.compile_binary(node)?;
            }
            "literal" => {
                let child = node.named_child(0).ok_or_else(|| {
                    self.error("Empty literal".into(), node)
                })?;
                self.compile_expression(&child)?;
            }
            "identifier" => {
                self.compile_identifier(node)?;
            }
            "block" => {
                self.compile_block(node)?;
            }
            "if_expression" => {
                self.compile_if(node)?;
            }
            "match_expression" => {
                self.compile_match(node)?;
            }
            "for_expression" => {
                self.compile_for(node)?;
            }
            "range_expression" => {
                self.compile_range(node)?;
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
        Err(self.error(format!("Undefined variable: {}", name), node))
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

        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();

            // If the previous statement was an expression (not a let),
            // pop its value — only the last expression's value is kept.
            if last_was_expr {
                self.emit(Op::Pop, &child);
            }

            match child.kind() {
                "let_binding" => {
                    self.compile_let(&child)?;
                    last_was_expr = false;
                }
                _ => {
                    self.compile_expression(&child)?;
                    last_was_expr = true;
                }
            }
        }

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
        // For now, only support identifier patterns
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

        // Declare the local — its slot is the current stack position
        let name = self.node_text(&pattern);
        self.declare_local(&name);

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

        // Compile condition
        self.compile_expression(condition)?;

        // Jump over then-branch if false
        let then_jump = self.emit(Op::JumpIfFalse(0), node);

        // Compile then-branch
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
        self.compile_expression(subject)?;
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
        let op_text = node.child(0)
            .map(|c| self.node_text_raw(&c))
            .unwrap_or_default();
        let operand = node.named_child(0).ok_or_else(|| {
            self.error("Unary missing operand".into(), node)
        })?;

        self.compile_expression(&operand)?;

        match op_text.as_str() {
            "-" => self.emit(Op::Negate, node),
            "not" => self.emit(Op::Not, node),
            _ => return Err(self.error(
                format!("Unknown unary operator: {}", op_text), node,
            )),
        };
        Ok(())
    }

    fn compile_binary(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let lhs = node.child_by_field_name("left")
            .or_else(|| node.named_child(0))
            .ok_or_else(|| self.error("Binary missing lhs".into(), node))?;
        let rhs = node.child_by_field_name("right")
            .or_else(|| node.named_child(1))
            .ok_or_else(|| self.error("Binary missing rhs".into(), node))?;

        let op_node = node.child(1).ok_or_else(|| {
            self.error("Binary missing operator".into(), node)
        })?;
        let op_text = self.node_text_raw(&op_node);

        match op_text.as_str() {
            "&&" => return self.compile_and(node, &lhs, &rhs),
            "||" => return self.compile_or(node, &lhs, &rhs),
            _ => {}
        }

        self.compile_expression(&lhs)?;
        self.compile_expression(&rhs)?;

        let op = match op_text.as_str() {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "/" => Op::Div,
            "%" => Op::Mod,
            "==" => Op::Eq,
            "!=" => Op::Ne,
            "<" => Op::Lt,
            ">" => Op::Gt,
            "<=" => Op::Le,
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
                    let text = self.node_text_raw(&child);
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
            let idx = self.chunk.add_constant(Value::String(String::new()));
            self.emit(Op::LoadConst(idx), node);
            return Ok(());
        }

        let mut segment_count = 0;
        for part in &parts {
            match part {
                StringPart::Text(s) => {
                    let idx = self.chunk.add_constant(Value::String(s.clone()));
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
            let idx = self.chunk.add_constant(Value::String(String::new()));
            self.emit(Op::LoadConst(idx), node);
            self.emit(Op::Concat, node);
        }

        Ok(())
    }

    // -----------------------------------------------------------------------
    // Finish
    // -----------------------------------------------------------------------

    pub fn finish(mut self) -> Chunk {
        if self.chunk.code.last() != Some(&Op::Return) {
            let len = self.chunk.code.len();
            let (line, col) = if len > 0 {
                self.chunk.source_map[len - 1]
            } else {
                (0, 0)
            };
            self.chunk.emit(Op::Return, line, col);
        }
        self.chunk
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

    fn node_text_raw(&self, node: &Node) -> String {
        node.utf8_text(self.source.as_bytes()).unwrap_or("").to_string()
    }

    /// Current compile-time stack offset (number of values on stack).
    fn stack_offset(&self) -> usize {
        self.locals.len()
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

        let mut compiler = Compiler::new(&source);
        compiler
            .compile_expression(&body)
            .unwrap_or_else(|e| panic!("Compile error: {}. Tree: {}", e, root.to_sexp()));
        let chunk = compiler.finish();

        let mut vm = Vm::new();
        vm.execute(&chunk).expect("VM error")
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
            Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
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
}
