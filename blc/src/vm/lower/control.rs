use tree_sitter::Node;

use crate::vm::chunk::CompileError;
use crate::vm::ir::*;

impl<'a> super::Lowerer<'a> {
    pub(super) fn lower_block(&mut self, node: &Node) -> Result<Expr, CompileError> {
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

    pub(super) fn lower_let(&mut self, node: &Node) -> Result<Expr, CompileError> {
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
        } else if pattern_node.kind() == "record_pattern" {
            self.lower_pattern(&pattern_node)?
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

    pub(super) fn lower_if(&mut self, node: &Node) -> Result<Expr, CompileError> {
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

    pub(super) fn lower_match(&mut self, node: &Node) -> Result<Expr, CompileError> {
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

            // Separate patterns, optional guard, and body.
            // The body is always the last child. The guard (if present) is the
            // second-to-last child with kind "match_guard". Everything before
            // the guard/body is a pattern (or-pattern arms have multiple).
            let body_idx = arm_children.len() - 1;
            let mut guard = None;
            let mut pattern_end = body_idx;
            for child in &arm_children[1..body_idx] {
                if child.kind() == "match_guard" {
                    let guard_expr_node = child.named_child(0)
                        .ok_or_else(|| self.error("Match guard missing expression".into(), child))?;
                    guard = Some(self.lower_expression(&guard_expr_node)?);
                    pattern_end = arm_children.iter().position(|c| c.id() == child.id()).unwrap();
                }
            }

            let body = self.lower_expression(&arm_children[body_idx])?;

            // Collect all pattern nodes (or-patterns have multiple)
            let pattern_nodes: Vec<&Node> = arm_children[..pattern_end]
                .iter()
                .filter(|c| c.kind() != "match_guard")
                .collect();

            // Desugar or-patterns: each alternative becomes a separate IR arm
            // sharing the same guard and body.
            for pat_node in &pattern_nodes {
                let pattern = self.lower_pattern(pat_node)?;
                arms.push(MatchArm { pattern, guard: guard.clone(), body: body.clone() });
            }
        }

        Ok(Expr::Match {
            subject: Box::new(subject),
            arms,
            ty: None,
        })
    }

    pub(super) fn lower_pattern(&self, node: &Node) -> Result<Pattern, CompileError> {
        match node.kind() {
            "wildcard_pattern" => Ok(Pattern::Wildcard),
            "identifier" => Ok(Pattern::Var(self.node_text(node))),
            "literal" | "integer_literal" | "float_literal" | "boolean_literal"
            | "string_literal" | "multiline_string_literal" | "raw_string_literal" | "raw_hash_string_literal" => {
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
                        "string_literal" | "multiline_string_literal" => {
                            self.extract_string_content(node)
                        }
                        "raw_string_literal" | "raw_hash_string_literal" => {
                            Expr::String(self.raw_string_content(node))
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
            "record_pattern" => {
                let mut pat_cursor = node.walk();
                let field_nodes: Vec<Node> = node.named_children(&mut pat_cursor).collect();
                let mut fields = Vec::new();
                for field_node in &field_nodes {
                    if field_node.kind() == "record_pattern_field" {
                        let named_count = field_node.named_child_count();
                        let name_node = field_node.child_by_field_name("name")
                            .ok_or_else(|| self.error("Record pattern field missing name".into(), field_node))?;
                        let name = self.node_text(&name_node);
                        if named_count > 1 {
                            // { x: pat } form
                            let pat_node = field_node.child_by_field_name("pattern")
                                .ok_or_else(|| self.error("Record pattern field missing pattern".into(), field_node))?;
                            let sub_pattern = self.lower_pattern(&pat_node)?;
                            fields.push((name, sub_pattern));
                        } else {
                            // { x } shorthand — binds to variable with same name
                            fields.push((name.clone(), Pattern::Var(name)));
                        }
                    }
                }
                Ok(Pattern::Record(fields))
            }
            _ => Err(self.error(format!("Unsupported pattern kind: {}", node.kind()), node)),
        }
    }

    pub(super) fn lower_for(&mut self, node: &Node) -> Result<Expr, CompileError> {
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

    pub(super) fn lower_range(&mut self, node: &Node) -> Result<Expr, CompileError> {
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
}
