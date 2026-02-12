use tree_sitter::Node;

use crate::analysis::types::Type;
use crate::vm::chunk::CompileError;
use crate::vm::ir::*;

use super::helpers::eval_const_binary;

impl<'a> super::Lowerer<'a> {
    pub(super) fn lower_identifier(&self, node: &Node) -> Result<Expr, CompileError> {
        let name = self.node_text(node);
        let ty = self
            .type_map
            .as_ref()
            .and_then(|tm| tm.get(&node.start_byte()).cloned());
        Ok(Expr::Var(name, ty))
    }

    pub(super) fn lower_unary(&mut self, node: &Node) -> Result<Expr, CompileError> {
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

    pub(super) fn lower_binary(&mut self, node: &Node) -> Result<Expr, CompileError> {
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
            "++" => BinOp::ListConcat,
            _ => return Err(self.error(format!("Unknown binary operator: {}", op_text), node)),
        };

        Ok(Expr::BinOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            ty,
        })
    }

    pub(super) fn is_int_expr(&self, node: &Node) -> bool {
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

    pub(super) fn try_eval_const(&self, node: &Node) -> Option<Expr> {
        match node.kind() {
            "integer_literal" => crate::parse::parse_int_literal(&self.node_text(node)).map(Expr::Int),
            "float_literal" => self.node_text(node).parse::<f64>().ok().map(Expr::Float),
            "boolean_literal" => Some(Expr::Bool(self.node_text(node) == "true")),
            "string_literal" | "multiline_string_literal"
                if !self.has_interpolation(node) =>
            {
                Some(self.extract_string_content(node))
            }
            "raw_string_literal" | "raw_hash_string_literal" => {
                Some(Expr::String(self.raw_string_content(node)))
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
}
