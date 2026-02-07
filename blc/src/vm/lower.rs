use std::collections::HashSet;
use tree_sitter::Node;

use crate::analysis::types::{Type, TypeMap};

use super::ir::*;
use super::natives::NativeRegistry;

// ---------------------------------------------------------------------------
// Lowering Error
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct LowerError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}] {}", self.line, self.col, self.message)
    }
}

// ---------------------------------------------------------------------------
// Lowerer
// ---------------------------------------------------------------------------

/// Lowers a tree-sitter CST into the IR. All desugaring happens here.
pub struct Lowerer<'a> {
    source: &'a str,
    type_map: Option<TypeMap>,
    natives: &'a NativeRegistry,
    /// Known top-level function names (for CallDirect resolution).
    functions: HashSet<String>,
    /// Name of the function currently being lowered (for TCO detection).
    current_fn_name: Option<String>,
    /// Whether the current expression is in tail position.
    tail_position: bool,
    /// Stack of enclosing function parameter sets (for upvalue/capture detection).
    scopes: Vec<HashSet<String>>,
}

impl<'a> Lowerer<'a> {
    pub fn new(source: &'a str, natives: &'a NativeRegistry, type_map: Option<TypeMap>) -> Self {
        Lowerer {
            source,
            type_map,
            natives,
            functions: HashSet::new(),
            current_fn_name: None,
            tail_position: false,
            scopes: Vec::new(),
        }
    }

    /// Provide pre-registered function names (e.g. from imported modules).
    pub fn add_functions(&mut self, names: impl IntoIterator<Item = String>) {
        self.functions.extend(names);
    }

    // -----------------------------------------------------------------------
    // Module lowering
    // -----------------------------------------------------------------------

    /// Lower a full source file into an IrModule.
    pub fn lower_module(&mut self, root: &Node) -> Result<IrModule, LowerError> {
        // First pass: collect function names
        let mut func_nodes: Vec<(String, usize)> = Vec::new();
        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            if child.kind() == "function_def"
                && let Some(name_node) = child.child_by_field_name("name")
            {
                let name = self.node_text(&name_node);
                self.functions.insert(name.clone());
                func_nodes.push((name, i));
            }
        }

        if func_nodes.is_empty() {
            return Err(LowerError {
                message: "No function definitions found".into(),
                line: 1,
                col: 0,
            });
        }

        // Second pass: lower each function
        let mut functions = Vec::new();
        for (name, child_idx) in &func_nodes {
            let child = root.named_child(*child_idx).unwrap();
            let func = self.lower_function_def(&child, name)?;
            functions.push(func);
        }

        // Find entry point
        let entry = functions
            .iter()
            .position(|f| f.name == "main!" || f.name == "main")
            .ok_or_else(|| LowerError {
                message: "No 'main' or 'main!' function found".into(),
                line: 1,
                col: 0,
            })?;
        // Prefer main! over main
        let entry = functions
            .iter()
            .position(|f| f.name == "main!")
            .unwrap_or(entry);

        Ok(IrModule { functions, entry })
    }

    /// Lower function definitions for a module (no entry point required).
    pub fn lower_module_functions(&mut self, root: &Node) -> Result<Vec<IrFunction>, LowerError> {
        // First pass: collect function names
        let mut func_nodes: Vec<(String, usize)> = Vec::new();
        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            if child.kind() == "function_def"
                && let Some(name_node) = child.child_by_field_name("name")
            {
                let name = self.node_text(&name_node);
                self.functions.insert(name.clone());
                func_nodes.push((name, i));
            }
        }

        let mut functions = Vec::new();
        for (name, child_idx) in &func_nodes {
            let child = root.named_child(*child_idx).unwrap();
            let func = self.lower_function_def(&child, name)?;
            functions.push(func);
        }

        Ok(functions)
    }

    fn lower_function_def(&mut self, node: &Node, name: &str) -> Result<IrFunction, LowerError> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| self.error("Function missing body".into(), node))?;

        let span = self.span(node);
        self.current_fn_name = Some(name.to_string());

        let (params, body) = if body_node.kind() == "lambda" {
            let mut cursor = body_node.walk();
            let children: Vec<Node> = body_node.named_children(&mut cursor).collect();
            if children.is_empty() {
                (vec![], Expr::Unit)
            } else {
                let body_child = &children[children.len() - 1];
                let params: Vec<String> = children[..children.len() - 1]
                    .iter()
                    .map(|n| self.node_text(n))
                    .collect();

                // Enter scope with params
                let param_set: HashSet<String> = params.iter().cloned().collect();
                self.scopes.push(param_set);
                self.tail_position = true;
                let body = self.lower_expression(body_child)?;
                self.tail_position = false;
                self.scopes.pop();
                (params, body)
            }
        } else {
            self.tail_position = true;
            let body = self.lower_expression(&body_node)?;
            self.tail_position = false;
            (vec![], body)
        };

        self.current_fn_name = None;

        // Look up function type from type_map
        let ty = self
            .type_map
            .as_ref()
            .and_then(|tm| tm.get(&node.start_byte()).cloned());

        Ok(IrFunction {
            name: name.to_string(),
            params,
            body,
            ty,
            span,
        })
    }

    // -----------------------------------------------------------------------
    // Expression lowering
    // -----------------------------------------------------------------------

    pub fn lower_expression(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let kind = node.kind();
        match kind {
            // -- Tail-position propagating --
            "call_expression" => self.lower_call(node),
            "if_expression" => self.lower_if(node),
            "match_expression" => self.lower_match(node),
            "block" => self.lower_block(node),
            "parenthesized_expression" => {
                let inner = node
                    .named_child(0)
                    .ok_or_else(|| self.error("Empty parenthesized expression".into(), node))?;
                self.lower_expression(&inner)
            }
            "literal" => {
                let child = node
                    .named_child(0)
                    .ok_or_else(|| self.error("Empty literal".into(), node))?;
                self.lower_expression(&child)
            }

            // -- Non-tail types --
            "integer_literal" => self.lower_integer(node),
            "float_literal" => self.lower_float(node),
            "boolean_literal" => self.lower_boolean(node),
            "string_literal" => self.lower_string_literal(node),
            "tuple_expression" => self.lower_tuple(node),
            "unary_expression" => self.lower_unary(node),
            "binary_expression" => self.lower_binary(node),
            "identifier" => self.lower_identifier(node),
            "for_expression" => self.lower_for(node),
            "range_expression" => self.lower_range(node),
            "lambda" => self.lower_lambda(node),
            "pipe_expression" => self.lower_pipe(node),
            "list_expression" => self.lower_list(node),
            "record_expression" => self.lower_record(node),
            "field_expression" => self.lower_field_access(node),
            "struct_expression" => self.lower_struct_expression(node),
            "type_identifier" => self.lower_nullary_constructor(node),
            "try_expression" => self.lower_try(node),
            "record_update" => self.lower_record_update(node),
            "let_binding" => self.lower_let(node),
            "line_comment" | "block_comment" => Ok(Expr::Unit),
            _ => Err(self.error(format!("Unsupported expression kind: {}", kind), node)),
        }
    }

    // -----------------------------------------------------------------------
    // Literals
    // -----------------------------------------------------------------------

    fn lower_integer(&self, node: &Node) -> Result<Expr, LowerError> {
        let text = self.node_text(node);
        let val: i64 = text
            .parse()
            .map_err(|_| self.error(format!("Invalid integer: {}", text), node))?;
        Ok(Expr::Int(val))
    }

    fn lower_float(&self, node: &Node) -> Result<Expr, LowerError> {
        let text = self.node_text(node);
        let val: f64 = text
            .parse()
            .map_err(|_| self.error(format!("Invalid float: {}", text), node))?;
        Ok(Expr::Float(val))
    }

    fn lower_boolean(&self, node: &Node) -> Result<Expr, LowerError> {
        let text = self.node_text(node);
        Ok(Expr::Bool(text == "true"))
    }

    // -----------------------------------------------------------------------
    // String literals (with interpolation desugaring)
    // -----------------------------------------------------------------------

    fn lower_string_literal(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let node_start = node.start_byte();
        let node_end = node.end_byte();
        let src = self.source.as_bytes();

        let mut parts: Vec<Expr> = Vec::new();
        let mut cursor = node_start + 1; // skip opening quote
        let content_end = node_end - 1; // skip closing quote

        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            let child_start = child.start_byte();
            let child_end = child.end_byte();

            if cursor < child_start {
                let text = std::str::from_utf8(&src[cursor..child_start]).unwrap_or("");
                if !text.is_empty() {
                    parts.push(Expr::String(text.to_string()));
                }
            }

            match child.kind() {
                "interpolation" => {
                    let expr_node = child
                        .named_child(0)
                        .ok_or_else(|| self.error("Empty interpolation".into(), &child))?;
                    parts.push(self.lower_expression(&expr_node)?);
                }
                "escape_sequence" => {
                    let text = self.node_text(&child);
                    parts.push(Expr::String(unescape(&text)));
                }
                _ => {}
            }

            cursor = child_end;
        }

        if cursor < content_end {
            let text = std::str::from_utf8(&src[cursor..content_end]).unwrap_or("");
            if !text.is_empty() {
                parts.push(Expr::String(text.to_string()));
            }
        }

        if parts.is_empty() {
            return Ok(Expr::String(String::new()));
        }

        // Check if there are any non-string parts (interpolations)
        let has_interpolation = parts.iter().any(|p| !matches!(p, Expr::String(_)));

        if !has_interpolation && parts.len() == 1 {
            // Plain string, no interpolation
            return Ok(parts.into_iter().next().unwrap());
        }

        if has_interpolation || parts.len() > 1 {
            Ok(Expr::Concat(parts))
        } else {
            Ok(parts.into_iter().next().unwrap())
        }
    }

    // -----------------------------------------------------------------------
    // Variables
    // -----------------------------------------------------------------------

    fn lower_identifier(&self, node: &Node) -> Result<Expr, LowerError> {
        let name = self.node_text(node);
        let ty = self
            .type_map
            .as_ref()
            .and_then(|tm| tm.get(&node.start_byte()).cloned());
        Ok(Expr::Var(name, ty))
    }

    // -----------------------------------------------------------------------
    // Unary & Binary
    // -----------------------------------------------------------------------

    fn lower_unary(&mut self, node: &Node) -> Result<Expr, LowerError> {
        // Constant folding
        if let Some(val) = self.try_eval_const(node) {
            return Ok(val);
        }

        let op_text = node
            .child(0)
            .map(|c| self.node_text(&c))
            .unwrap_or_default();
        let operand = node
            .named_child(0)
            .ok_or_else(|| self.error("Unary missing operand".into(), node))?;

        let was_tail = self.tail_position;
        self.tail_position = false;
        let inner = self.lower_expression(&operand)?;
        self.tail_position = was_tail;

        let op = match op_text.as_str() {
            "-" => UnaryOp::Neg,
            "not" => UnaryOp::Not,
            _ => return Err(self.error(format!("Unknown unary operator: {}", op_text), node)),
        };

        Ok(Expr::UnaryOp {
            op,
            operand: Box::new(inner),
            ty: None,
        })
    }

    fn lower_binary(&mut self, node: &Node) -> Result<Expr, LowerError> {
        // Constant folding
        if let Some(val) = self.try_eval_const(node) {
            return Ok(val);
        }

        let lhs_node = node
            .child_by_field_name("left")
            .or_else(|| node.named_child(0))
            .ok_or_else(|| self.error("Binary missing lhs".into(), node))?;
        let rhs_node = node
            .child_by_field_name("right")
            .or_else(|| node.named_child(1))
            .ok_or_else(|| self.error("Binary missing rhs".into(), node))?;
        let op_node = node
            .child(1)
            .ok_or_else(|| self.error("Binary missing operator".into(), node))?;
        let op_text = self.node_text(&op_node);

        // Short-circuit operators
        match op_text.as_str() {
            "&&" => {
                let lhs = self.lower_expression(&lhs_node)?;
                let rhs = self.lower_expression(&rhs_node)?;
                return Ok(Expr::And(Box::new(lhs), Box::new(rhs)));
            }
            "||" => {
                let lhs = self.lower_expression(&lhs_node)?;
                let rhs = self.lower_expression(&rhs_node)?;
                return Ok(Expr::Or(Box::new(lhs), Box::new(rhs)));
            }
            _ => {}
        }

        let was_tail = self.tail_position;
        self.tail_position = false;
        let lhs = self.lower_expression(&lhs_node)?;
        let rhs = self.lower_expression(&rhs_node)?;
        self.tail_position = was_tail;

        // Determine type for specialization
        let both_int = self.type_map.as_ref().is_some_and(|tm| {
            matches!(
                (
                    tm.get(&lhs_node.start_byte()),
                    tm.get(&rhs_node.start_byte())
                ),
                (Some(Type::Int), Some(Type::Int))
            )
        }) || (self.is_int_expr(&lhs_node) && self.is_int_expr(&rhs_node));

        let ty = if both_int { Some(Type::Int) } else { None };

        let op = match op_text.as_str() {
            "+" => BinOp::Add,
            "-" => BinOp::Sub,
            "*" => BinOp::Mul,
            "/" => BinOp::Div,
            "%" => BinOp::Mod,
            "==" => BinOp::Eq,
            "!=" => BinOp::Ne,
            "<" => BinOp::Lt,
            ">" => BinOp::Gt,
            "<=" => BinOp::Le,
            ">=" => BinOp::Ge,
            _ => return Err(self.error(format!("Unknown binary operator: {}", op_text), node)),
        };

        Ok(Expr::BinOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            ty,
        })
    }

    fn is_int_expr(&self, node: &Node) -> bool {
        match node.kind() {
            "integer_literal" => true,
            "unary_expression" => node
                .named_child(0)
                .is_some_and(|c| c.kind() == "integer_literal"),
            "binary_expression" => {
                if let Some(op) = node.child(1) {
                    let op_text = self.node_text(&op);
                    matches!(op_text.as_str(), "+" | "-" | "*" | "/" | "%")
                        && node.named_child(0).is_some_and(|c| self.is_int_expr(&c))
                        && node.named_child(1).is_some_and(|c| self.is_int_expr(&c))
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    // -----------------------------------------------------------------------
    // Constant folding
    // -----------------------------------------------------------------------

    fn try_eval_const(&self, node: &Node) -> Option<Expr> {
        match node.kind() {
            "integer_literal" => self.node_text(node).parse::<i64>().ok().map(Expr::Int),
            "float_literal" => self.node_text(node).parse::<f64>().ok().map(Expr::Float),
            "boolean_literal" => Some(Expr::Bool(self.node_text(node) == "true")),
            "string_literal" if node.named_child_count() == 0 => {
                let text = &self.source[node.start_byte() + 1..node.end_byte() - 1];
                Some(Expr::String(text.into()))
            }
            "parenthesized_expression" | "literal" => {
                node.named_child(0).and_then(|c| self.try_eval_const(&c))
            }
            "unary_expression" => {
                let op = node.child(0).map(|c| self.node_text(&c))?;
                let val = node.named_child(0).and_then(|c| self.try_eval_const(&c))?;
                match (op.as_str(), &val) {
                    ("-", Expr::Int(n)) => Some(Expr::Int(n.wrapping_neg())),
                    ("-", Expr::Float(n)) => Some(Expr::Float(-n)),
                    ("not", Expr::Bool(b)) => Some(Expr::Bool(!b)),
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

    // -----------------------------------------------------------------------
    // Blocks & Let
    // -----------------------------------------------------------------------

    fn lower_block(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let mut children: Vec<Node> = Vec::new();
        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            if child.kind() != "line_comment" && child.kind() != "block_comment" {
                children.push(child);
            }
        }

        let was_tail = self.tail_position;
        let last_idx = children.len().saturating_sub(1);
        let mut exprs = Vec::new();

        for (idx, child) in children.iter().enumerate() {
            let is_last = idx == last_idx;
            self.tail_position = if is_last { was_tail } else { false };
            exprs.push(self.lower_expression(child)?);
        }

        self.tail_position = was_tail;

        if exprs.is_empty() {
            return Ok(Expr::Unit);
        }

        Ok(Expr::Block(exprs, None))
    }

    fn lower_let(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let pattern_node = node
            .named_child(0)
            .ok_or_else(|| self.error("Let binding missing pattern".into(), node))?;

        let value_node = node
            .named_child(node.named_child_count() - 1)
            .ok_or_else(|| self.error("Let binding missing value".into(), node))?;

        let value = self.lower_expression(&value_node)?;

        let pattern = if pattern_node.kind() == "tuple_pattern" {
            let mut pat_cursor = pattern_node.walk();
            let bindings: Vec<Node> = pattern_node.named_children(&mut pat_cursor).collect();
            let mut pats = Vec::new();
            for binding in &bindings {
                if binding.kind() == "identifier" {
                    pats.push(Pattern::Var(self.node_text(binding)));
                } else if binding.kind() == "wildcard_pattern" {
                    pats.push(Pattern::Wildcard);
                }
            }
            Pattern::Tuple(pats)
        } else {
            let name = self.node_text(&pattern_node);
            Pattern::Var(name)
        };

        let ty = self
            .type_map
            .as_ref()
            .and_then(|tm| tm.get(&node.start_byte()).cloned());

        Ok(Expr::Let {
            pattern: Box::new(pattern),
            value: Box::new(value),
            ty,
        })
    }

    // -----------------------------------------------------------------------
    // If/Else
    // -----------------------------------------------------------------------

    fn lower_if(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("If expression missing condition".into(), node));
        }

        let was_tail = self.tail_position;
        self.tail_position = false;
        let condition = self.lower_expression(&children[0])?;
        self.tail_position = was_tail;

        let then_branch = self.lower_expression(&children[1])?;

        let else_branch = if children.len() > 2 {
            Some(Box::new(self.lower_expression(&children[2])?))
        } else {
            None
        };

        Ok(Expr::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
            ty: None,
        })
    }

    // -----------------------------------------------------------------------
    // Match
    // -----------------------------------------------------------------------

    fn lower_match(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("Match expression missing subject".into(), node));
        }

        let was_tail = self.tail_position;
        self.tail_position = false;
        let subject = self.lower_expression(&children[0])?;
        self.tail_position = was_tail;

        let arm_nodes: Vec<&Node> = children[1..]
            .iter()
            .filter(|c| c.kind() == "match_arm")
            .collect();

        if arm_nodes.is_empty() {
            return Err(self.error("Match expression has no arms".into(), node));
        }

        let mut arms = Vec::new();
        for arm_node in &arm_nodes {
            let mut arm_cursor = arm_node.walk();
            let arm_children: Vec<Node> = arm_node.named_children(&mut arm_cursor).collect();

            if arm_children.len() < 2 {
                return Err(self.error("Match arm missing pattern or body".into(), arm_node));
            }

            let pattern = self.lower_pattern(&arm_children[0])?;
            let body = self.lower_expression(&arm_children[arm_children.len() - 1])?;

            arms.push(MatchArm { pattern, body });
        }

        Ok(Expr::Match {
            subject: Box::new(subject),
            arms,
            ty: None,
        })
    }

    fn lower_pattern(&self, node: &Node) -> Result<Pattern, LowerError> {
        match node.kind() {
            "wildcard_pattern" => Ok(Pattern::Wildcard),
            "identifier" => Ok(Pattern::Var(self.node_text(node))),
            "literal" | "integer_literal" | "float_literal" | "boolean_literal"
            | "string_literal" => {
                let expr =
                    match node.kind() {
                        "integer_literal" => {
                            let text = self.node_text(node);
                            Expr::Int(text.parse().map_err(|_| {
                                self.error(format!("Invalid integer: {}", text), node)
                            })?)
                        }
                        "float_literal" => {
                            let text = self.node_text(node);
                            Expr::Float(text.parse().map_err(|_| {
                                self.error(format!("Invalid float: {}", text), node)
                            })?)
                        }
                        "boolean_literal" => Expr::Bool(self.node_text(node) == "true"),
                        "string_literal" => {
                            let text = &self.source[node.start_byte() + 1..node.end_byte() - 1];
                            Expr::String(text.into())
                        }
                        "literal" => {
                            let child = node
                                .named_child(0)
                                .ok_or_else(|| self.error("Empty literal pattern".into(), node))?;
                            return self.lower_pattern(&child);
                        }
                        _ => return Err(self.error("Unexpected pattern literal".into(), node)),
                    };
                Ok(Pattern::Literal(Box::new(expr)))
            }
            "type_identifier" => {
                // Nullary constructor pattern: None, Active, etc.
                let tag = self.node_text(node);
                Ok(Pattern::Constructor(tag, vec![]))
            }
            "constructor_pattern" => {
                let mut pat_cursor = node.walk();
                let pat_children: Vec<Node> = node.named_children(&mut pat_cursor).collect();

                if pat_children.is_empty() {
                    return Err(self.error("Constructor pattern missing tag".into(), node));
                }

                let tag = self.node_text(&pat_children[0]);
                let mut sub_patterns = Vec::new();
                for child in &pat_children[1..] {
                    sub_patterns.push(self.lower_pattern(child)?);
                }

                Ok(Pattern::Constructor(tag, sub_patterns))
            }
            "tuple_pattern" => {
                let mut pat_cursor = node.walk();
                let pat_children: Vec<Node> = node.named_children(&mut pat_cursor).collect();
                let mut pats = Vec::new();
                for child in &pat_children {
                    pats.push(self.lower_pattern(child)?);
                }
                Ok(Pattern::Tuple(pats))
            }
            _ => Err(self.error(format!("Unsupported pattern kind: {}", node.kind()), node)),
        }
    }

    // -----------------------------------------------------------------------
    // For loops
    // -----------------------------------------------------------------------

    fn lower_for(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.named_children(&mut cursor).collect();

        if children.len() < 3 {
            return Err(self.error("For loop missing components".into(), node));
        }

        let binding = self.node_text(&children[0]);
        let iterable = self.lower_expression(&children[1])?;
        let body = self.lower_expression(&children[2])?;

        Ok(Expr::For {
            binding,
            iterable: Box::new(iterable),
            body: Box::new(body),
        })
    }

    // -----------------------------------------------------------------------
    // Range
    // -----------------------------------------------------------------------

    fn lower_range(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let lhs = node
            .named_child(0)
            .ok_or_else(|| self.error("Range missing start".into(), node))?;
        let rhs = node
            .named_child(1)
            .ok_or_else(|| self.error("Range missing end".into(), node))?;

        let start = self.lower_expression(&lhs)?;
        let end = self.lower_expression(&rhs)?;

        Ok(Expr::MakeRange(Box::new(start), Box::new(end)))
    }

    // -----------------------------------------------------------------------
    // Calls (with resolution)
    // -----------------------------------------------------------------------

    fn lower_call(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("Call expression missing callee".into(), node));
        }

        let callee = &children[0];
        let arg_nodes = &children[1..];

        // Constructor call: type_identifier(args) → MakeEnum
        if callee.kind() == "type_identifier" {
            let tag = self.node_text(callee);
            let payload = if arg_nodes.len() == 1 {
                self.lower_expression(&arg_nodes[0])?
            } else if arg_nodes.is_empty() {
                Expr::Unit
            } else {
                let mut elems = Vec::new();
                for arg in arg_nodes {
                    elems.push(self.lower_expression(arg)?);
                }
                Expr::MakeTuple(elems, None)
            };
            return Ok(Expr::MakeEnum {
                tag,
                payload: Box::new(payload),
                ty: None,
            });
        }

        // TCO: self-recursive tail call
        if self.tail_position
            && callee.kind() == "identifier"
            && let Some(ref fn_name) = self.current_fn_name
        {
            let callee_name = self.node_text(callee);
            if callee_name == *fn_name {
                let was_tail = self.tail_position;
                self.tail_position = false;
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                return Ok(Expr::TailCall {
                    name: callee_name,
                    args,
                    ty: None,
                });
            }
        }

        let was_tail = self.tail_position;
        self.tail_position = false;

        // Native call: Module.method(args)
        if callee.kind() == "field_expression"
            && let Some((module, method, qualified)) = self.try_resolve_qualified(callee)
        {
            if self.natives.lookup(&qualified).is_some() {
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                return Ok(Expr::CallNative {
                    module,
                    method,
                    args,
                    ty: None,
                });
            }
            // Imported module function
            if self.functions.contains(&qualified) {
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                return Ok(Expr::CallDirect {
                    name: qualified,
                    args,
                    ty: None,
                });
            }
        }

        // Named function call
        if callee.kind() == "identifier" {
            let callee_name = self.node_text(callee);
            if self.functions.contains(&callee_name) {
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                return Ok(Expr::CallDirect {
                    name: callee_name,
                    args,
                    ty: None,
                });
            }
        }

        // Indirect call (closure/variable)
        let callee_expr = self.lower_expression(callee)?;
        let mut args = Vec::new();
        for arg in arg_nodes {
            args.push(self.lower_expression(arg)?);
        }
        self.tail_position = was_tail;

        Ok(Expr::CallIndirect {
            callee: Box::new(callee_expr),
            args,
            ty: None,
        })
    }

    fn try_resolve_qualified(&self, field_expr: &Node) -> Option<(String, String, String)> {
        let obj = field_expr.named_child(0)?;
        let method = field_expr.named_child(1)?;
        if obj.kind() == "type_identifier" {
            let module = self.node_text(&obj);
            let method_name = self.node_text(&method);
            let qualified = format!("{}.{}", module, method_name);
            Some((module, method_name, qualified))
        } else {
            None
        }
    }

    // -----------------------------------------------------------------------
    // Pipe operator (desugared into calls)
    // -----------------------------------------------------------------------

    fn lower_pipe(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let lhs_node = node
            .named_child(0)
            .ok_or_else(|| self.error("Pipe missing left operand".into(), node))?;
        let rhs_node = node
            .named_child(1)
            .ok_or_else(|| self.error("Pipe missing right operand".into(), node))?;

        let lhs = self.lower_expression(&lhs_node)?;

        if rhs_node.kind() == "call_expression" {
            // x |> f(a, b) → f(x, a, b)
            let mut cursor = rhs_node.walk();
            let rhs_children: Vec<Node> = rhs_node.named_children(&mut cursor).collect();

            if rhs_children.is_empty() {
                return Err(self.error("Pipe RHS call missing callee".into(), &rhs_node));
            }

            let callee = &rhs_children[0];
            let extra_args = &rhs_children[1..];

            // Check for native module call
            if callee.kind() == "field_expression"
                && let Some((module, method, qualified)) = self.try_resolve_qualified(callee)
            {
                if self.natives.lookup(&qualified).is_some() {
                    let mut args = vec![lhs];
                    for arg in extra_args {
                        args.push(self.lower_expression(arg)?);
                    }
                    return Ok(Expr::CallNative {
                        module,
                        method,
                        args,
                        ty: None,
                    });
                }
                // Imported module function
                if self.functions.contains(&qualified) {
                    let mut args = vec![lhs];
                    for arg in extra_args {
                        args.push(self.lower_expression(arg)?);
                    }
                    return Ok(Expr::CallDirect {
                        name: qualified,
                        args,
                        ty: None,
                    });
                }
            }

            // Named function
            if callee.kind() == "identifier" {
                let callee_name = self.node_text(callee);
                if self.functions.contains(&callee_name) {
                    let mut args = vec![lhs];
                    for arg in extra_args {
                        args.push(self.lower_expression(arg)?);
                    }
                    return Ok(Expr::CallDirect {
                        name: callee_name,
                        args,
                        ty: None,
                    });
                }
            }

            // Generic indirect call
            let callee_expr = self.lower_expression(callee)?;
            let mut args = vec![lhs];
            for arg in extra_args {
                args.push(self.lower_expression(arg)?);
            }
            Ok(Expr::CallIndirect {
                callee: Box::new(callee_expr),
                args,
                ty: None,
            })
        } else if rhs_node.kind() == "field_expression" {
            // x |> Module.method → CallNative/CallDirect(x)
            if let Some((module, method, qualified)) = self.try_resolve_qualified(&rhs_node) {
                if self.natives.lookup(&qualified).is_some() {
                    return Ok(Expr::CallNative {
                        module,
                        method,
                        args: vec![lhs],
                        ty: None,
                    });
                }
                if self.functions.contains(&qualified) {
                    return Ok(Expr::CallDirect {
                        name: qualified,
                        args: vec![lhs],
                        ty: None,
                    });
                }
            }
            // Not a native — regular indirect call
            let callee_expr = self.lower_expression(&rhs_node)?;
            Ok(Expr::CallIndirect {
                callee: Box::new(callee_expr),
                args: vec![lhs],
                ty: None,
            })
        } else {
            // x |> f → f(x)
            let rhs = self.lower_expression(&rhs_node)?;

            // Check if rhs is a known function var
            if let Expr::Var(ref name, _) = rhs
                && self.functions.contains(name)
            {
                return Ok(Expr::CallDirect {
                    name: name.clone(),
                    args: vec![lhs],
                    ty: None,
                });
            }

            Ok(Expr::CallIndirect {
                callee: Box::new(rhs),
                args: vec![lhs],
                ty: None,
            })
        }
    }

    // -----------------------------------------------------------------------
    // Lambda
    // -----------------------------------------------------------------------

    fn lower_lambda(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("Lambda missing body".into(), node));
        }

        let body_node = &children[children.len() - 1];
        let params: Vec<String> = children[..children.len() - 1]
            .iter()
            .map(|n| self.node_text(n))
            .collect();

        let param_set: HashSet<String> = params.iter().cloned().collect();
        self.scopes.push(param_set);
        let body = self.lower_expression(body_node)?;
        self.scopes.pop();

        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
            ty: None,
        })
    }

    // -----------------------------------------------------------------------
    // Data structures
    // -----------------------------------------------------------------------

    fn lower_list(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let mut elems = Vec::new();
        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            elems.push(self.lower_expression(&child)?);
        }
        Ok(Expr::MakeList(elems, None))
    }

    fn lower_record(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let mut fields = Vec::new();
        for i in 0..node.named_child_count() {
            let field_init = node.named_child(i).unwrap();
            if field_init.kind() != "record_field_init" {
                continue;
            }
            let key_node = field_init
                .named_child(0)
                .ok_or_else(|| self.error("Record field missing key".into(), &field_init))?;
            let val_node = field_init
                .named_child(1)
                .ok_or_else(|| self.error("Record field missing value".into(), &field_init))?;
            let key = self.node_text(&key_node);
            let val = self.lower_expression(&val_node)?;
            fields.push((key, val));
        }
        Ok(Expr::MakeRecord(fields, None))
    }

    fn lower_tuple(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let count = node.named_child_count();
        if count == 0 {
            return Ok(Expr::Unit);
        }
        if count == 1 {
            // Single element — parenthesized expression
            let inner = node.named_child(0).unwrap();
            return self.lower_expression(&inner);
        }
        let mut elems = Vec::new();
        for i in 0..count {
            let child = node.named_child(i).unwrap();
            elems.push(self.lower_expression(&child)?);
        }
        Ok(Expr::MakeTuple(elems, None))
    }

    fn lower_field_access(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let obj = node
            .named_child(0)
            .ok_or_else(|| self.error("Field access missing object".into(), node))?;
        let field = node
            .named_child(1)
            .ok_or_else(|| self.error("Field access missing field name".into(), node))?;

        let object = self.lower_expression(&obj)?;
        let field_name = self.node_text(&field);

        Ok(Expr::GetField {
            object: Box::new(object),
            field: field_name,
            ty: None,
        })
    }

    fn lower_struct_expression(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let type_node = node
            .named_child(0)
            .ok_or_else(|| self.error("Struct expression missing type".into(), node))?;
        let type_name = self.node_text(&type_node);

        let mut fields = Vec::new();
        for i in 1..node.named_child_count() {
            let field_init = node.named_child(i).unwrap();
            if field_init.kind() != "record_field_init" {
                continue;
            }
            let key_node = field_init.named_child(0).unwrap();
            let val_node = field_init.named_child(1).unwrap();
            let key = self.node_text(&key_node);
            let val = self.lower_expression(&val_node)?;
            fields.push((key, val));
        }

        if !fields.is_empty() {
            Ok(Expr::MakeStruct {
                name: type_name,
                fields,
                ty: None,
            })
        } else {
            // No fields → nullary enum constructor
            Ok(Expr::MakeEnum {
                tag: type_name,
                payload: Box::new(Expr::Unit),
                ty: None,
            })
        }
    }

    fn lower_nullary_constructor(&self, node: &Node) -> Result<Expr, LowerError> {
        let name = self.node_text(node);
        Ok(Expr::MakeEnum {
            tag: name,
            payload: Box::new(Expr::Unit),
            ty: None,
        })
    }

    // -----------------------------------------------------------------------
    // Try expression
    // -----------------------------------------------------------------------

    fn lower_try(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let expr_node = node
            .named_child(0)
            .ok_or_else(|| self.error("Try expression missing operand".into(), node))?;
        let expr = self.lower_expression(&expr_node)?;
        Ok(Expr::Try {
            expr: Box::new(expr),
            ty: None,
        })
    }

    // -----------------------------------------------------------------------
    // Record update
    // -----------------------------------------------------------------------

    fn lower_record_update(&mut self, node: &Node) -> Result<Expr, LowerError> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("Record update missing base".into(), node));
        }

        let base = self.lower_expression(&children[0])?;

        let mut updates = Vec::new();
        for child in &children[1..] {
            if child.kind() == "record_field_init" {
                let key = child.named_child(0).unwrap();
                let val = child.named_child(1).unwrap();
                let key_name = self.node_text(&key);
                let val_expr = self.lower_expression(&val)?;
                updates.push((key_name, val_expr));
            }
        }

        Ok(Expr::UpdateRecord {
            base: Box::new(base),
            updates,
            ty: None,
        })
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    fn node_text(&self, node: &Node) -> String {
        node.utf8_text(self.source.as_bytes())
            .unwrap_or("")
            .to_string()
    }

    fn span(&self, node: &Node) -> Span {
        Span {
            line: node.start_position().row + 1,
            col: node.start_position().column,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
        }
    }

    fn error(&self, message: String, node: &Node) -> LowerError {
        LowerError {
            message,
            line: node.start_position().row + 1,
            col: node.start_position().column,
        }
    }
}

// ---------------------------------------------------------------------------
// Constant folding helpers
// ---------------------------------------------------------------------------

fn eval_const_binary(op: &str, a: &Expr, b: &Expr) -> Option<Expr> {
    match (a, b) {
        (Expr::Int(a), Expr::Int(b)) => match op {
            "+" => Some(Expr::Int(a.wrapping_add(*b))),
            "-" => Some(Expr::Int(a.wrapping_sub(*b))),
            "*" => Some(Expr::Int(a.wrapping_mul(*b))),
            "/" if *b != 0 => Some(Expr::Int(a.wrapping_div(*b))),
            "%" if *b != 0 => Some(Expr::Int(a.wrapping_rem(*b))),
            "==" => Some(Expr::Bool(a == b)),
            "!=" => Some(Expr::Bool(a != b)),
            "<" => Some(Expr::Bool(a < b)),
            ">" => Some(Expr::Bool(a > b)),
            "<=" => Some(Expr::Bool(a <= b)),
            ">=" => Some(Expr::Bool(a >= b)),
            _ => None,
        },
        (Expr::Float(a), Expr::Float(b)) => match op {
            "+" => Some(Expr::Float(a + b)),
            "-" => Some(Expr::Float(a - b)),
            "*" => Some(Expr::Float(a * b)),
            "/" if *b != 0.0 => Some(Expr::Float(a / b)),
            _ => None,
        },
        (Expr::Bool(a), Expr::Bool(b)) => match op {
            "==" => Some(Expr::Bool(a == b)),
            "!=" => Some(Expr::Bool(a != b)),
            _ => None,
        },
        (Expr::String(a), Expr::String(b)) => match op {
            "+" => {
                let mut r = std::string::String::with_capacity(a.len() + b.len());
                r.push_str(a);
                r.push_str(b);
                Some(Expr::String(r))
            }
            "==" => Some(Expr::Bool(a == b)),
            "!=" => Some(Expr::Bool(a != b)),
            _ => None,
        },
        _ => None,
    }
}

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
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .expect("Error loading grammar");
        parser.parse(source, None).expect("Parse failed")
    }

    fn lower_expr(source: &str) -> Expr {
        let wrapped = format!("x : () -> Unknown\nx = {}", source);
        let tree = parse(&wrapped);
        let root = tree.root_node();
        let func_def = root.named_child(0).unwrap();
        let body = func_def.child_by_field_name("body").unwrap();
        let natives = NativeRegistry::new();
        let mut lowerer = Lowerer::new(&wrapped, &natives, None);
        lowerer.lower_expression(&body).unwrap()
    }

    #[test]
    fn lower_int_literal() {
        match lower_expr("42") {
            Expr::Int(42) => {}
            other => panic!("Expected Int(42), got {:?}", other),
        }
    }

    #[test]
    fn lower_float_literal() {
        match lower_expr("1.5") {
            Expr::Float(f) => assert!((f - 1.5).abs() < 1e-10),
            other => panic!("Expected Float(1.5), got {:?}", other),
        }
    }

    #[test]
    fn lower_bool_literal() {
        match lower_expr("true") {
            Expr::Bool(true) => {}
            other => panic!("Expected Bool(true), got {:?}", other),
        }
    }

    #[test]
    fn lower_string_literal_plain() {
        match lower_expr("\"hello\"") {
            Expr::String(s) => assert_eq!(s, "hello"),
            other => panic!("Expected String, got {:?}", other),
        }
    }

    #[test]
    fn lower_addition_const_fold() {
        match lower_expr("1 + 2") {
            Expr::Int(3) => {}
            other => panic!("Expected Int(3) from const fold, got {:?}", other),
        }
    }

    #[test]
    fn lower_pipe_desugars() {
        // x |> f desugars to a call
        let source =
            "double : Int -> Int\ndouble = |x| x * 2\nmain : () -> Int\nmain = 5 |> double";
        let tree = parse(source);
        let root = tree.root_node();
        let natives = NativeRegistry::new();
        let mut lowerer = Lowerer::new(source, &natives, None);
        let module = lowerer.lower_module(&root).unwrap();
        // main function body should be a call, not a pipe
        match &module.functions[module.entry].body {
            Expr::CallDirect { name, args, .. } => {
                assert_eq!(name, "double");
                assert_eq!(args.len(), 1);
            }
            other => panic!("Expected CallDirect, got {:?}", other),
        }
    }

    #[test]
    fn lower_some_constructor() {
        match lower_expr("Some(42)") {
            Expr::MakeEnum { tag, payload, .. } => {
                assert_eq!(tag, "Some");
                match *payload {
                    Expr::Int(42) => {}
                    other => panic!("Expected Int(42) payload, got {:?}", other),
                }
            }
            other => panic!("Expected MakeEnum, got {:?}", other),
        }
    }

    #[test]
    fn lower_none_constructor() {
        match lower_expr("None") {
            Expr::MakeEnum { tag, payload, .. } => {
                assert_eq!(tag, "None");
                match *payload {
                    Expr::Unit => {}
                    other => panic!("Expected Unit payload, got {:?}", other),
                }
            }
            other => panic!("Expected MakeEnum, got {:?}", other),
        }
    }

    #[test]
    fn lower_module_basic() {
        let source = "inc : Int -> Int\ninc = |x| x + 1\nmain : () -> Int\nmain = inc(5)";
        let tree = parse(source);
        let root = tree.root_node();
        let natives = NativeRegistry::new();
        let mut lowerer = Lowerer::new(source, &natives, None);
        let module = lowerer.lower_module(&root).unwrap();
        assert_eq!(module.functions.len(), 2);
        assert_eq!(module.functions[module.entry].name, "main");
    }
}
