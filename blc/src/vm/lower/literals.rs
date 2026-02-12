use tree_sitter::Node;

use crate::vm::chunk::CompileError;
use crate::vm::ir::*;

use super::helpers::unescape;

impl<'a> super::Lowerer<'a> {
    pub(super) fn lower_integer(&self, node: &Node) -> Result<Expr, CompileError> {
        let text = self.node_text(node);
        let val: i64 = crate::parse::parse_int_literal(&text)
            .ok_or_else(|| self.error(format!("Invalid integer: {}", text), node))?;
        Ok(Expr::Int(val))
    }

    pub(super) fn lower_float(&self, node: &Node) -> Result<Expr, CompileError> {
        let text = self.node_text(node);
        let val: f64 = text
            .parse()
            .map_err(|_| self.error(format!("Invalid float: {}", text), node))?;
        Ok(Expr::Float(val))
    }

    pub(super) fn lower_boolean(&self, node: &Node) -> Result<Expr, CompileError> {
        let text = self.node_text(node);
        Ok(Expr::Bool(text == "true"))
    }

    // -----------------------------------------------------------------------
    // String literals (with interpolation desugaring)
    // -----------------------------------------------------------------------

    pub(super) fn lower_string_literal(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let mut parts: Vec<Expr> = Vec::new();

        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            match child.kind() {
                "string_content" | "multiline_string_content" => {
                    let text = self.node_text(&child);
                    if !text.is_empty() {
                        parts.push(Expr::String(text));
                    }
                }
                "escape_sequence" => {
                    let text = self.node_text(&child);
                    parts.push(Expr::String(unescape(&text)));
                }
                "interpolation" => {
                    let expr_node = child
                        .named_child(0)
                        .ok_or_else(|| self.error("Empty interpolation".into(), &child))?;
                    parts.push(self.lower_expression(&expr_node)?);
                }
                _ => {} // string_start, string_end, etc.
            }
        }

        if parts.is_empty() {
            return Ok(Expr::String(String::new()));
        }

        let has_interpolation = parts.iter().any(|p| !matches!(p, Expr::String(_)));

        if !has_interpolation && parts.len() == 1 {
            return Ok(parts.into_iter().next().unwrap());
        }

        if has_interpolation || parts.len() > 1 {
            Ok(Expr::Concat(parts))
        } else {
            Ok(parts.into_iter().next().unwrap())
        }
    }

    pub(super) fn lower_raw_string(&self, node: &Node) -> Result<Expr, CompileError> {
        Ok(Expr::String(self.raw_string_content(node)))
    }

    /// Extract content from raw_string_literal or raw_hash_string_literal.
    /// Children are [start, content?, end] -- find the one with "content" in kind.
    pub(super) fn raw_string_content(&self, node: &Node) -> String {
        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            if child.kind().contains("content") {
                return child.utf8_text(self.source.as_bytes()).unwrap_or("").to_string();
            }
        }
        String::new()
    }

    /// Check if a string node contains interpolation children.
    pub(super) fn has_interpolation(&self, node: &Node) -> bool {
        for i in 0..node.named_child_count() {
            if let Some(child) = node.named_child(i) {
                if child.kind() == "interpolation" {
                    return true;
                }
            }
        }
        false
    }

    /// Extract the text content of a string literal (no interpolation).
    pub(super) fn extract_string_content(&self, node: &Node) -> Expr {
        let mut result = String::new();
        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            match child.kind() {
                "string_content" | "multiline_string_content" => {
                    result.push_str(&self.node_text(&child));
                }
                "escape_sequence" => {
                    result.push_str(&unescape(&self.node_text(&child)));
                }
                _ => {}
            }
        }
        Expr::String(result)
    }
}
