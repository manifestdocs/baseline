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

pub struct Compiler<'a> {
    source: &'a str,
    chunk: Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Compiler {
            source,
            chunk: Chunk::new(),
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
                // tuple_expression with 1 child is a parenthesized expression
                // (grammar ambiguity between (expr) and tuple)
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
                // literal wraps integer_literal, float_literal, etc.
                let child = node.named_child(0).ok_or_else(|| {
                    self.error("Empty literal".into(), node)
                })?;
                self.compile_expression(&child)?;
            }
            _ => {
                return Err(self.error(
                    format!("Unsupported expression kind: {}", kind), node,
                ));
            }
        }
        Ok(())
    }

    fn compile_unary(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        // Grammar: choice('not', '-') _expression
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
        // Grammar: _expression op _expression
        // Children: [lhs, op, rhs] — op is anonymous child at index 1
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

        // Short-circuit: && and ||
        match op_text.as_str() {
            "&&" => return self.compile_and(node, &lhs, &rhs),
            "||" => return self.compile_or(node, &lhs, &rhs),
            _ => {}
        }

        // Normal binary: compile both sides, then emit op
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

    /// Compile `&&` with short-circuit: if LHS is false, skip RHS.
    fn compile_and(&mut self, node: &Node<'a>, lhs: &Node<'a>, rhs: &Node<'a>) -> Result<(), CompileError> {
        self.compile_expression(lhs)?;
        // If lhs is false, the whole expression is false — skip rhs
        let jump_idx = self.emit(Op::JumpIfFalse(0), node);
        // LHS was truthy (and consumed by JumpIfFalse), evaluate RHS
        self.compile_expression(rhs)?;
        let end_idx = self.emit(Op::Jump(0), node);
        // Patch the false-jump: land here, push false
        self.chunk.patch_jump(jump_idx);
        let false_const = self.chunk.add_constant(Value::Bool(false));
        self.emit(Op::LoadConst(false_const), node);
        self.chunk.patch_jump(end_idx);
        Ok(())
    }

    /// Compile `||` with short-circuit: if LHS is true, skip RHS.
    fn compile_or(&mut self, node: &Node<'a>, lhs: &Node<'a>, rhs: &Node<'a>) -> Result<(), CompileError> {
        self.compile_expression(lhs)?;
        // If lhs is true, the whole expression is true — skip rhs
        // JumpIfTrue peeks (doesn't pop), so if we jump, lhs value stays on stack.
        // But we want a bool. Let's use JumpIfFalse instead:
        // If false, fall through to rhs. If true, jump past rhs.
        let jump_idx = self.emit(Op::JumpIfFalse(0), node);
        // LHS was truthy (consumed by JumpIfFalse), push true and jump over rhs
        let true_const = self.chunk.add_constant(Value::Bool(true));
        self.emit(Op::LoadConst(true_const), node);
        let end_jump = self.emit(Op::Jump(0), node);
        // Patch: LHS was falsy, evaluate RHS
        self.chunk.patch_jump(jump_idx);
        self.compile_expression(rhs)?;
        self.chunk.patch_jump(end_jump);
        Ok(())
    }

    /// Compile a string literal, handling interpolation segments.
    ///
    /// Tree-sitter doesn't create child nodes for plain text in strings.
    /// Text fragments exist in the byte gaps between children (quotes,
    /// interpolations, escape sequences). We walk children and extract
    /// text from the gaps.
    fn compile_string_literal(&mut self, node: &Node<'a>) -> Result<(), CompileError> {
        let node_start = node.start_byte();
        let node_end = node.end_byte();
        let src = self.source.as_bytes();

        // Collect named children (interpolation, escape_sequence)
        let mut parts: Vec<StringPart<'a>> = Vec::new();

        // Track cursor position (skip opening quote)
        let mut cursor = node_start + 1; // skip opening "
        let content_end = node_end - 1; // before closing "

        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            let child_start = child.start_byte();
            let child_end = child.end_byte();

            // Text gap before this child
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

        // Text gap after last child (before closing quote)
        if cursor < content_end {
            let text = std::str::from_utf8(&src[cursor..content_end]).unwrap_or("");
            if !text.is_empty() {
                parts.push(StringPart::Text(text.to_string()));
            }
        }

        // Empty string
        if parts.is_empty() {
            let idx = self.chunk.add_constant(Value::String(String::new()));
            self.emit(Op::LoadConst(idx), node);
            return Ok(());
        }

        // Emit code for each part, concatenating as we go
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

        // If single interpolation (no text), force to string via concat with ""
        let has_interpolation = parts.iter().any(|p| matches!(p, StringPart::Interpolation(_)));
        if segment_count == 1 && has_interpolation {
            let idx = self.chunk.add_constant(Value::String(String::new()));
            self.emit(Op::LoadConst(idx), node);
            self.emit(Op::Concat, node);
        }

        Ok(())
    }

    /// Finish compilation and return the chunk.
    pub fn finish(mut self) -> Chunk {
        // Ensure there's a Return at the end
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

    // -- Helpers --

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
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn unescape(s: &str) -> String {
    // s is like \n, \t, \\, etc.
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
        // Wrap in: x : () -> Unknown \n x = <expr>
        let source = format!("x : () -> Unknown\nx = {}", expr);
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .expect("Error loading Baseline grammar");
        let tree = parser.parse(&source, None).expect("Parse failed");
        let root = tree.root_node();

        // Extract the function body (field "body" of function_def)
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

    // -- Literal tests --

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

    // -- Arithmetic --

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
        // 1 + 2 * 3 = 7 (acceptance criteria)
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

    // -- Unary --

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

    // -- Comparisons --

    #[test]
    fn compile_eq() {
        assert_eq!(eval_expr("5 == 5"), Value::Bool(true));
    }

    #[test]
    fn compile_ne() {
        assert_eq!(eval_expr("5 != 3"), Value::Bool(true));
    }

    #[test]
    fn compile_lt() {
        assert_eq!(eval_expr("3 < 5"), Value::Bool(true));
    }

    #[test]
    fn compile_gt() {
        assert_eq!(eval_expr("5 > 3"), Value::Bool(true));
    }

    #[test]
    fn compile_le() {
        assert_eq!(eval_expr("3 <= 3"), Value::Bool(true));
    }

    #[test]
    fn compile_ge() {
        assert_eq!(eval_expr("5 >= 5"), Value::Bool(true));
    }

    // -- Boolean short-circuit --

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

    // -- String interpolation --

    #[test]
    fn compile_string_interpolation() {
        // This needs the expression to be parseable in isolation.
        // String interpolation "${1 + 2}" should produce "3"
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
}
