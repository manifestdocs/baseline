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

    pub(super) fn lower_map_literal(&mut self, node: &Node) -> Result<Expr, CompileError> {
        // Desugar #{ k1: v1, k2: v2 } into Map.insert(Map.insert(Map.empty(), k1, v1), k2, v2)
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

        Ok(result)
    }

    pub(super) fn lower_set_literal(&mut self, node: &Node) -> Result<Expr, CompileError> {
        // Desugar #{ v1, v2 } into Set.insert(Set.insert(Set.empty(), v1), v2)
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

        Ok(result)
    }

    pub(super) fn lower_list(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let mut elems = Vec::new();
        for i in 0..node.named_child_count() {
            let child = node.named_child(i).unwrap();
            elems.push(self.lower_expression(&child)?);
        }
        Ok(Expr::MakeList(elems, None))
    }

    pub(super) fn lower_record(&mut self, node: &Node) -> Result<Expr, CompileError> {
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
        let mut elems = Vec::new();
        for i in 0..count {
            let child = node.named_child(i).unwrap();
            elems.push(self.lower_expression(&child)?);
        }
        Ok(Expr::MakeTuple(elems, None))
    }

    pub(super) fn lower_field_access(&mut self, node: &Node) -> Result<Expr, CompileError> {
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

    pub(super) fn lower_struct_expression(&mut self, node: &Node) -> Result<Expr, CompileError> {
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
}
