use tree_sitter::Node;

use crate::vm::chunk::CompileError;
use crate::vm::ir::*;

impl<'a> super::Lowerer<'a> {
    pub(super) fn node_text(&self, node: &Node) -> String {
        node.utf8_text(self.source.as_bytes())
            .unwrap_or("")
            .to_string()
    }

    pub(super) fn span(&self, node: &Node) -> Span {
        Span {
            line: node.start_position().row + 1,
            col: node.start_position().column,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
        }
    }

    pub(super) fn error(&self, message: String, node: &Node) -> CompileError {
        CompileError {
            message,
            line: node.start_position().row + 1,
            col: node.start_position().column,
        }
    }

    /// Extract parameter names from a function_def node.
    pub(super) fn extract_param_names(&self, func_def: &Node) -> Vec<String> {
        let mut params = Vec::new();
        if let Some(param_list) = func_def.child_by_field_name("params") {
            let mut cursor = param_list.walk();
            for child in param_list.named_children(&mut cursor) {
                if child.kind() == "param"
                    && let Some(name_node) = child.child_by_field_name("name")
                {
                    params.push(self.node_text(&name_node));
                }
            }
        }
        params
    }
}

// ---------------------------------------------------------------------------
// Standalone helpers (used across multiple files)
// ---------------------------------------------------------------------------

pub(super) fn unescape(s: &str) -> String {
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

pub(super) fn eval_const_binary(op: &str, a: &Expr, b: &Expr) -> Option<Expr> {
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
