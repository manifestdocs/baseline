use std::collections::HashSet;
use tree_sitter::Node;

use crate::vm::chunk::CompileError;
use crate::vm::ir::*;

impl<'a> super::Lowerer<'a> {
    pub(super) fn lower_lambda(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("Lambda missing body".into(), node));
        }

        let body_node = &children[children.len() - 1];
        let mut params = Vec::new();
        let mut param_lets = Vec::new();
        let mut param_scope_names = HashSet::new();

        // The last child is the body. The preceding children are patterns.
        let pattern_nodes = &children[..children.len() - 1];

        for (i, pat_node) in pattern_nodes.iter().enumerate() {
            let pattern = self.lower_pattern(pat_node)?;
            if let Pattern::Var(name) = pattern {
                params.push(name.clone());
                param_scope_names.insert(name);
            } else {
                let synth_name = format!("$lparam{}", i);
                params.push(synth_name.clone());
                param_scope_names.insert(synth_name.clone());

                let bound = self.collect_bound_names(&pattern);
                for bn in bound {
                    param_scope_names.insert(bn);
                }

                param_lets.push(Expr::Let {
                    pattern: Box::new(pattern),
                    value: Box::new(Expr::Var(synth_name, None)),
                    ty: None,
                });
            }
        }

        self.scopes.push(param_scope_names);
        let mut body = self.lower_expression(body_node)?;
        self.scopes.pop();

        if !param_lets.is_empty() {
            if let Expr::Block(ref mut stmts, _) = body {
                let mut new_stmts = param_lets;
                new_stmts.append(stmts);
                *stmts = new_stmts;
            } else {
                let mut stmts = param_lets;
                stmts.push(body);
                body = Expr::Block(stmts, None);
            }
        }

        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
            ty: None,
        })
    }

    pub(super) fn lower_map_literal(&mut self, node: &Node) -> Result<Expr, CompileError> {
        // Desugar #{ k1: v1, k2: v2 } into Map.insert(Map.insert(Map.empty(), k1, v1), k2, v2)
        let was_tail = self.tail_position;
        self.tail_position = false;
        let mut result = Expr::CallNative {
            module: "Map".to_string(),
            method: "empty".to_string(),
            args: vec![],
            ty: None,
        };

        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            if child.kind() == "map_entry" {
                let key_node = child
                    .child_by_field_name("key")
                    .ok_or_else(|| self.error("Map entry missing key".into(), &child))?;
                let val_node = child
                    .child_by_field_name("value")
                    .ok_or_else(|| self.error("Map entry missing value".into(), &child))?;
                let key = self.lower_expression(&key_node)?;
                let val = self.lower_expression(&val_node)?;
                result = Expr::CallNative {
                    module: "Map".to_string(),
                    method: "insert".to_string(),
                    args: vec![result, key, val],
                    ty: None,
                };
            }
        }

        self.tail_position = was_tail;
        Ok(result)
    }

    pub(super) fn lower_set_literal(&mut self, node: &Node) -> Result<Expr, CompileError> {
        // Desugar #{ v1, v2 } into Set.insert(Set.insert(Set.empty(), v1), v2)
        let was_tail = self.tail_position;
        self.tail_position = false;
        let mut result = Expr::CallNative {
            module: "Set".to_string(),
            method: "empty".to_string(),
            args: vec![],
            ty: None,
        };

        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            let elem = self.lower_expression(&child)?;
            result = Expr::CallNative {
                module: "Set".to_string(),
                method: "insert".to_string(),
                args: vec![result, elem],
                ty: None,
            };
        }

        self.tail_position = was_tail;
        Ok(result)
    }

    pub(super) fn lower_list(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let was_tail = self.tail_position;
        self.tail_position = false;
        let mut elems = Vec::new();
        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            elems.push(self.lower_expression(&child)?);
        }
        self.tail_position = was_tail;
        Ok(Expr::MakeList(elems, None))
    }

    pub(super) fn lower_record(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let was_tail = self.tail_position;
        self.tail_position = false;
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
            let key = self.field_name_text(&key_node)?;
            let val = self.lower_expression(&val_node)?;
            fields.push((key, val));
        }
        self.tail_position = was_tail;
        Ok(Expr::MakeRecord(fields, None))
    }

    pub(super) fn lower_tuple(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let count = node.named_child_count();
        if count == 0 {
            return Ok(Expr::Unit);
        }
        if count == 1 {
            // Single element -- parenthesized expression
            let inner = node.named_child(0).unwrap();
            return self.lower_expression(&inner);
        }
        let was_tail = self.tail_position;
        self.tail_position = false;
        let mut elems = Vec::new();
        for i in 0..count {
            let child = node.named_child(i).unwrap();
            elems.push(self.lower_expression(&child)?);
        }
        self.tail_position = was_tail;
        Ok(Expr::MakeTuple(elems, None))
    }

    pub(super) fn lower_field_access(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let was_tail = self.tail_position;
        self.tail_position = false;

        let obj = node
            .named_child(0)
            .ok_or_else(|| self.error("Field access missing object".into(), node))?;
        let field = node
            .named_child(1)
            .ok_or_else(|| self.error("Field access missing field name".into(), node))?;

        let object = self.lower_expression(&obj)?;
        let field_name = self.node_text(&field);

        self.tail_position = was_tail;

        // Look up the resolved type of this field_expression from the type checker.
        let ty = self
            .type_map
            .as_ref()
            .and_then(|tm| tm.get(&node.start_byte()).cloned());

        Ok(Expr::GetField {
            object: Box::new(object),
            field: field_name,
            field_idx: None,
            ty,
        })
    }

    pub(super) fn lower_struct_expression(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let was_tail = self.tail_position;
        self.tail_position = false;

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
            let key = self.field_name_text(&key_node)?;
            let val = self.lower_expression(&val_node)?;
            fields.push((key, val));
        }

        self.tail_position = was_tail;

        if !fields.is_empty() {
            Ok(Expr::MakeStruct {
                name: type_name,
                fields,
                ty: None,
            })
        } else {
            // No fields -> nullary enum constructor
            Ok(Expr::MakeEnum {
                tag: type_name,
                payload: Box::new(Expr::Unit),
                ty: None,
            })
        }
    }

    pub(super) fn lower_nullary_constructor(&self, node: &Node) -> Result<Expr, CompileError> {
        let name = self.node_text(node);
        Ok(Expr::MakeEnum {
            tag: name,
            payload: Box::new(Expr::Unit),
            ty: None,
        })
    }

    pub(super) fn lower_try(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let expr_node = node
            .named_child(0)
            .ok_or_else(|| self.error("Try expression missing operand".into(), node))?;
        let expr = self.lower_expression(&expr_node)?;
        Ok(Expr::Try {
            expr: Box::new(expr),
            ty: None,
        })
    }

    pub(super) fn lower_record_update(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let was_tail = self.tail_position;
        self.tail_position = false;

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
                let key_name = self.field_name_text(&key)?;
                let val_expr = self.lower_expression(&val)?;
                updates.push((key_name, val_expr));
            }
        }

        self.tail_position = was_tail;

        Ok(Expr::UpdateRecord {
            base: Box::new(base),
            updates,
            ty: None,
        })
    }
}
