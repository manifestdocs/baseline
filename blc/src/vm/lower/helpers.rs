use tree_sitter::Node;

use crate::vm::chunk::CompileError;
use crate::vm::ir::*;

impl<'a> super::Lowerer<'a> {
    pub(super) fn node_text(&self, node: &Node) -> String {
        node.utf8_text(self.source.as_bytes())
            .unwrap_or("")
            .to_string()
    }

    /// Extract a field name from an identifier or string_literal node.
    /// Returns the identifier text, or the string content (rejecting interpolation).
    pub(super) fn field_name_text(&self, node: &Node) -> Result<String, CompileError> {
        match node.kind() {
            "identifier" => Ok(self.node_text(node)),
            "string_literal" => {
                let mut name = String::new();
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    match child.kind() {
                        "string_content" => {
                            name.push_str(child.utf8_text(self.source.as_bytes()).unwrap_or(""));
                        }
                        "interpolation" => {
                            return Err(self.error(
                                "Interpolation is not allowed in quoted field names".into(),
                                &child,
                            ));
                        }
                        "escape_sequence" => {
                            name.push_str(&unescape(
                                child.utf8_text(self.source.as_bytes()).unwrap_or(""),
                            ));
                        }
                        _ => {}
                    }
                }
                Ok(name)
            }
            _ => Ok(self.node_text(node)),
        }
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

    /// Collect a simple enum definition (all variants nullary) into `enum_defs`.
    pub(super) fn collect_enum_def(&mut self, node: &Node) {
        let name_node = match node.child_by_field_name("name") {
            Some(n) => n,
            None => return,
        };
        let def_node = match node.child_by_field_name("def") {
            Some(n) => n,
            None => return,
        };
        if def_node.kind() != "variant_list" {
            return;
        }
        let name = self.node_text(&name_node);
        let mut variants = Vec::new();
        let mut all_nullary = true;
        let mut cursor = def_node.walk();
        for child in def_node.children(&mut cursor) {
            if child.kind() == "variant" {
                if let Some(vname_node) = child.child_by_field_name("name") {
                    let vname = self.node_text(&vname_node);
                    // A variant is nullary if the only type_identifier child is its name
                    let payload_count = (0..child.child_count())
                        .filter(|&i| {
                            let c = child.child(i).unwrap();
                            (c.kind() != "type_identifier" || c.id() != vname_node.id())
                                && c.kind() != "|"
                                && c.kind() != "("
                                && c.kind() != ")"
                                && c.kind() != ","
                                && c.kind() != "line_comment"
                                && c.kind() != "block_comment"
                        })
                        .count();
                    if payload_count > 0 {
                        all_nullary = false;
                        break;
                    }
                    variants.push(vname);
                }
            }
        }
        if all_nullary && !variants.is_empty() {
            self.enum_defs.insert(name, variants);
        }
    }

    /// Extract parameter names from a function_def node.
    pub(super) fn extract_param_names(&self, func_def: &Node) -> Vec<String> {
        let mut params = Vec::new();
        if let Some(param_list) = func_def.child_by_field_name("params") {
            let mut cursor = param_list.walk();
            for child in param_list.named_children(&mut cursor) {
                if child.kind() == "param" {
                    if let Some(name_node) = child.child_by_field_name("name") {
                         params.push(self.node_text(&name_node));
                    } else if let Some(pat_node) = child.child_by_field_name("pattern") {
                        // Simple identifier patterns support named args
                        if pat_node.kind() == "identifier" {
                            params.push(self.node_text(&pat_node));
                        } else {
                            // Complex patterns (record destructuring etc.) are positional-only
                            params.push("".to_string());
                        }
                    }
                }
            }
        }
        params
    }

    /// Recursively collect all variable names bound by a pattern.
    pub(super) fn collect_bound_names(&self, pattern: &Pattern) -> Vec<String> {
        let mut names = Vec::new();
        match pattern {
            Pattern::Var(name) => names.push(name.clone()),
            Pattern::Record(fields) => {
                for (_, sub_pat) in fields {
                    names.extend(self.collect_bound_names(sub_pat));
                }
            }
            Pattern::Tuple(sub_pats) | Pattern::Constructor(_, sub_pats) => {
                for sub_pat in sub_pats {
                    names.extend(self.collect_bound_names(sub_pat));
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) => {}
        }
        names
    }
}

// ---------------------------------------------------------------------------
// Standalone helpers (used across multiple files)
// ---------------------------------------------------------------------------

pub(super) fn to_snake_case(name: &str) -> String {
    let mut result = String::with_capacity(name.len() + 4);
    for (i, ch) in name.chars().enumerate() {
        if ch.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(ch.to_lowercase().next().unwrap());
        } else {
            result.push(ch);
        }
    }
    result
}

/// Generate inline IR for auto-derived enum methods (to_string, parse).
/// Returns `None` if the method is not recognized.
pub(super) fn generate_enum_method(
    enum_name: &str,
    method: &str,
    variants: &[String],
    args: Vec<Expr>,
) -> Option<Expr> {
    match method {
        "to_string" => {
            // Match on the enum value and return the snake_case string
            let subject = args.into_iter().next().unwrap_or(Expr::Unit);
            let arms: Vec<MatchArm> = variants
                .iter()
                .map(|v| MatchArm {
                    pattern: Pattern::Constructor(v.clone(), vec![]),
                    body: Expr::String(to_snake_case(v)),
                })
                .collect();
            Some(Expr::Match {
                subject: Box::new(subject),
                arms,
                ty: Some(crate::analysis::types::Type::String),
            })
        }
        "parse" => {
            // Chain of if-else comparing the string to each variant's snake_case name
            let input = args.into_iter().next().unwrap_or(Expr::Unit);
            // Build from last variant backwards
            let err_expr = Expr::MakeEnum {
                tag: "Err".to_string(),
                payload: Box::new(Expr::String(format!("invalid {} value", enum_name))),
                ty: None,
            };
            let result = variants.iter().rev().fold(err_expr, |else_branch, v| {
                let ok_expr = Expr::MakeEnum {
                    tag: "Ok".to_string(),
                    payload: Box::new(Expr::MakeEnum {
                        tag: v.clone(),
                        payload: Box::new(Expr::Unit),
                        ty: None,
                    }),
                    ty: None,
                };
                Expr::If {
                    condition: Box::new(Expr::BinOp {
                        op: BinOp::Eq,
                        lhs: Box::new(input.clone()),
                        rhs: Box::new(Expr::String(to_snake_case(v))),
                        ty: None,
                    }),
                    then_branch: Box::new(ok_expr),
                    else_branch: Some(Box::new(else_branch)),
                    ty: None,
                }
            });
            Some(result)
        }
        _ => None,
    }
}

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
