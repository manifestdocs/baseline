use tree_sitter::Node;

use crate::vm::chunk::CompileError;
use crate::vm::ir::*;

impl<'a> super::Lowerer<'a> {
    pub(super) fn lower_expect(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let actual_node = node
            .child_by_field_name("actual")
            .ok_or_else(|| self.error("expect missing actual".into(), node))?;
        let matcher_node = node
            .child_by_field_name("matcher")
            .ok_or_else(|| self.error("expect missing matcher".into(), node))?;

        let actual = self.lower_expression(&actual_node)?;

        let matcher_kind = matcher_node
            .child(0)
            .map(|c| self.node_text(&c))
            .unwrap_or_default();

        let make_enum_match = |subject: Expr, tag: &str| -> Expr {
            Expr::Match {
                subject: Box::new(subject),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor(tag.into(), vec![Pattern::Wildcard]),
                        guard: None,
                        body: Expr::Bool(true),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: Expr::Bool(false),
                    }
                ],
                ty: Some(crate::analysis::types::Type::Bool),
            }
        };

        match matcher_kind.as_str() {
            "to_equal" => {
                let expected = self.lower_matcher_arg(&matcher_node)?;
                Ok(Expr::BinOp {
                    op: BinOp::Eq,
                    lhs: Box::new(actual),
                    rhs: Box::new(expected),
                    ty: Some(crate::analysis::types::Type::Bool),
                })
            }
            "to_be_ok" => Ok(make_enum_match(actual, "Ok")),
            "to_be_some" => Ok(make_enum_match(actual, "Some")),
            "to_be_none" => Ok(make_enum_match(actual, "None")),
            "to_be_empty" => {
                Ok(Expr::BinOp {
                    op: BinOp::Eq,
                    lhs: Box::new(Expr::CallNative {
                        module: "List".into(),
                        method: "length".into(),
                        args: vec![actual],
                        ty: Some(crate::analysis::types::Type::Int),
                    }),
                    rhs: Box::new(Expr::Int(0)),
                    ty: Some(crate::analysis::types::Type::Bool),
                })
            }
            "to_have_length" => {
                let expected = self.lower_matcher_arg(&matcher_node)?;
                Ok(Expr::BinOp {
                    op: BinOp::Eq,
                    lhs: Box::new(Expr::CallNative {
                        module: "List".into(),
                        method: "length".into(),
                        args: vec![actual],
                        ty: Some(crate::analysis::types::Type::Int),
                    }),
                    rhs: Box::new(expected),
                    ty: Some(crate::analysis::types::Type::Bool),
                })
            }
            "to_contain" => {
                let expected = self.lower_matcher_arg(&matcher_node)?;
                Ok(Expr::CallNative {
                    module: "".into(),
                    method: "__test_contains".into(),
                    args: vec![actual, expected],
                    ty: Some(crate::analysis::types::Type::Bool),
                })
            }
            "to_start_with" => {
                let expected = self.lower_matcher_arg(&matcher_node)?;
                Ok(Expr::CallNative {
                    module: "String".into(),
                    method: "starts_with".into(),
                    args: vec![actual, expected],
                    ty: Some(crate::analysis::types::Type::Bool),
                })
            }
            "to_satisfy" => {
                let pred = self.lower_matcher_arg(&matcher_node)?;
                Ok(Expr::CallIndirect {
                    callee: Box::new(pred),
                    args: vec![actual],
                    ty: Some(crate::analysis::types::Type::Bool),
                })
            }
            "to_be" => {
                let pattern = if let Some(pat_node) = matcher_node.named_child(0) {
                    self.lower_pattern(&pat_node)?
                } else {
                    Pattern::Wildcard
                };
                Ok(Expr::Match {
                    subject: Box::new(actual),
                    arms: vec![
                        MatchArm {
                            pattern,
                            guard: None,
                            body: Expr::Bool(true),
                        },
                        MatchArm {
                            pattern: Pattern::Wildcard,
                            guard: None,
                            body: Expr::Bool(false),
                        }
                    ],
                    ty: Some(crate::analysis::types::Type::Bool),
                })
            }
            _ => {
                // Fallback: treat as equality
                let expected = self.lower_matcher_arg(&matcher_node)?;
                Ok(Expr::BinOp {
                    op: BinOp::Eq,
                    lhs: Box::new(actual),
                    rhs: Box::new(expected),
                    ty: Some(crate::analysis::types::Type::Bool),
                })
            }
        }
    }

    pub(super) fn lower_matcher_arg(&mut self, matcher_node: &Node) -> Result<Expr, CompileError> {
        if let Some(arg) = matcher_node.named_child(0) {
            self.lower_expression(&arg)
        } else {
            Ok(Expr::Unit)
        }
    }

    /// Lower `with { Effect: handler_expr } body` to WithHandlers IR.
    pub(super) fn lower_with_expression(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| self.error("with_expression missing body".into(), node))?;

        // Check for handler_map form (with { Effect: handler } body)
        if let Some(handler_map_node) = node.child_by_field_name("handlers") {
            let mut handlers: Vec<(String, Vec<(String, Expr)>)> = Vec::new();
            let mut cursor = handler_map_node.walk();
            for child in handler_map_node.named_children(&mut cursor) {
                if child.kind() == "handler_binding" {
                    let effect_name = child
                        .child_by_field_name("effect")
                        .map(|n| self.node_text(&n))
                        .unwrap_or_default();
                    let handler_expr_node =
                        child.child_by_field_name("handler").ok_or_else(|| {
                            self.error("handler_binding missing handler".into(), &child)
                        })?;
                    let handler_expr = self.lower_expression(&handler_expr_node)?;
                    // Single handler expression treated as the whole record
                    handlers.push((effect_name, vec![("__record__".to_string(), handler_expr)]));
                }
            }
            let body = self.lower_expression(&body_node)?;
            return Ok(Expr::WithHandlers {
                handlers,
                body: Box::new(body),
            });
        }

        // Legacy form: with effect_call block -- just lower the body
        self.lower_expression(&body_node)
    }

    /// Lower `handle body with { Effect.method!(args) -> handler_body }` to HandleEffect IR.
    pub(super) fn lower_handle_expression(&mut self, node: &Node) -> Result<Expr, CompileError> {
        use crate::vm::ir::HandlerClause;

        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| self.error("handle_expression missing body".into(), node))?;

        let mut clauses = Vec::new();

        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            if child.kind() == "handler_clause" {
                let effect_name = child
                    .child_by_field_name("effect")
                    .map(|n| self.node_text(&n))
                    .unwrap_or_default();
                let method_node = child.child_by_field_name("method").unwrap();
                let method_name = self.node_text(&method_node);
                let method_key = method_name
                    .strip_suffix('!')
                    .unwrap_or(&method_name)
                    .to_string();

                // Extract parameter names
                let mut params = Vec::new();
                let mut pcursor = child.walk();
                for pchild in child.named_children(&mut pcursor) {
                    if pchild.kind() == "identifier" && pchild.start_byte() > method_node.end_byte()
                    {
                        params.push(self.node_text(&pchild));
                    }
                }

                let handler_body_node = child.child_by_field_name("handler_body").unwrap();
                let handler_body = self.lower_expression(&handler_body_node)?;

                // Detect tail-resumptive: handler body is exactly `resume(expr)`.
                // If so, no continuation capture is needed.
                let is_tail_resumptive = Self::is_tail_resumptive_body(&handler_body);

                clauses.push(HandlerClause {
                    effect: effect_name,
                    method: method_key,
                    params,
                    body: handler_body,
                    is_tail_resumptive,
                });
            }
        }

        // Register handled effects so calls in the body are compiled as
        // PerformEffect rather than CallNative. The qualified key from
        // try_resolve_qualified includes the `!` suffix (e.g. "Console.println!"),
        // so we need to include it here to match.
        let mut added_effects = Vec::new();
        for clause in &clauses {
            let qualified = format!("{}.{}!", clause.effect, clause.method);
            if self.handled_effects.insert(qualified.clone()) {
                added_effects.push(qualified);
            }
        }

        let body = self.lower_expression(&body_node)?;

        // Restore handled_effects to previous state
        for eff in &added_effects {
            self.handled_effects.remove(eff);
        }

        Ok(Expr::HandleEffect {
            body: Box::new(body),
            clauses,
        })
    }

    /// Lower `restrict(Effect1, Effect2) { body }` — purely static, just evaluate body.
    ///
    /// Effect narrowing is enforced at analysis time by the effect checker.
    /// At the IR level, restrict is transparent — it's just the body expression.
    pub(super) fn lower_restrict_expression(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| self.error("restrict_expression missing body".into(), node))?;
        self.lower_expression(&body_node)
    }

    /// Check if a handler body is tail-resumptive (body is exactly `resume(expr)`).
    fn is_tail_resumptive_body(expr: &Expr) -> bool {
        match expr {
            Expr::CallIndirect { callee, args, .. } => {
                matches!(callee.as_ref(), Expr::Var(name, _) if name == "resume") && args.len() == 1
            }
            Expr::Block(stmts, _) if stmts.len() == 1 => Self::is_tail_resumptive_body(&stmts[0]),
            _ => false,
        }
    }
}
