use tree_sitter::Node;

use crate::vm::chunk::CompileError;
use crate::vm::ir::*;

impl<'a> super::Lowerer<'a> {
    /// Lower a full source file into an IrTestModule (functions + inline tests).
    pub fn lower_module_with_tests(&mut self, root: &Node) -> Result<IrTestModule, CompileError> {
        // First pass: collect function names and parameter names
        let mut func_nodes: Vec<(String, usize)> = Vec::new();
        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            if let Some(func_node) = self.unwrap_function_def(&child)
                && let Some(name_node) = func_node.child_by_field_name("name")
            {
                let name = self.node_text(&name_node);
                self.functions.insert(name.clone());
                let params = self.extract_param_names(&func_node);
                self.fn_params.insert(name.clone(), params);
                func_nodes.push((name, i));
            }
        }

        // Second pass: lower each function
        let mut functions = Vec::new();
        for (name, child_idx) in &func_nodes {
            let child = root.named_child(*child_idx).unwrap();
            let func_node = self.unwrap_function_def(&child).unwrap_or(child);
            let func = self.lower_function_def(&func_node, name)?;
            functions.push(func);
        }

        // Third pass: collect and lower inline tests
        let mut tests = Vec::new();
        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            let effective = if child.kind() == "spec_block" {
                self.unwrap_function_def(&child).unwrap_or(child)
            } else {
                child
            };

            match effective.kind() {
                "inline_test" => {
                    if let Some(t) = self.lower_inline_test(&effective)? {
                        tests.push(t);
                    }
                }
                "describe_block" => {
                    let has_only = self.has_focused_tests(&effective);
                    self.collect_describe_tests(&effective, "", has_only, &[], &[], &mut tests)?;
                }
                "test_section" => {
                    let mut section_cursor = effective.walk();
                    for section_child in effective.named_children(&mut section_cursor) {
                        match section_child.kind() {
                            "inline_test" => {
                                if let Some(t) = self.lower_inline_test(&section_child)? {
                                    tests.push(t);
                                }
                            }
                            "describe_block" => {
                                let has_only = self.has_focused_tests(&section_child);
                                self.collect_describe_tests(&section_child, "", has_only, &[], &[], &mut tests)?;
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }

        // Find entry point (optional for test programs)
        let entry = functions
            .iter()
            .position(|f| f.name == "main!")
            .or_else(|| functions.iter().position(|f| f.name == "main"))
            .unwrap_or(0);

        Ok(IrTestModule {
            functions,
            tests,
            entry,
            tags: TagRegistry::new(),
        })
    }

    /// Unwrap spec_block to extract the inner function_def.
    pub(super) fn unwrap_function_def<'b>(&self, node: &Node<'b>) -> Option<Node<'b>> {
        match node.kind() {
            "function_def" => Some(*node),
            "spec_block" => {
                for i in 0..node.named_child_count() {
                    let child = node.named_child(i).unwrap();
                    if child.kind() == "function_def" {
                        return Some(child);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Lower an inline test node into an IrTest.
    pub(super) fn lower_inline_test(
        &mut self,
        node: &Node,
    ) -> Result<Option<IrTest>, CompileError> {
        let count = node.named_child_count();
        if count < 2 {
            return Ok(None);
        }

        // First named child is the string_literal (test name)
        let name_node = node.named_child(0).unwrap();
        let raw_name = self.node_text(&name_node);
        let name = raw_name
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or(&raw_name)
            .to_string();

        // Last named child is the expression
        let expr_node = node.named_child(count - 1).unwrap();
        let body = self.lower_expression(&expr_node)?;

        let start = node.start_position();
        let end = node.end_position();

        Ok(Some(IrTest {
            name,
            body,
            line: start.row + 1,
            col: start.column + 1,
            end_line: end.row + 1,
            end_col: end.column + 1,
            skip: false,
        }))
    }

    /// Check if any it_block in the tree has .only modifier.
    pub(super) fn has_focused_tests(&self, node: &Node) -> bool {
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            if child.kind() == "it_block"
                && let Some(mod_node) = child.child_by_field_name("modifier")
            {
                let modifier = self.node_text(&mod_node);
                if modifier == ".only" {
                    return true;
                }
            }
            if child.kind() == "describe_block" && self.has_focused_tests(&child) {
                return true;
            }
        }
        false
    }

    /// Collect BDD tests from a describe_block, recursively handling nested blocks.
    pub(super) fn collect_describe_tests(
        &mut self,
        node: &Node,
        prefix: &str,
        has_only: bool,
        before_hooks: &[Node],
        after_hooks: &[Node],
        out: &mut Vec<IrTest>,
    ) -> Result<(), CompileError> {
        let name_node = node.child_by_field_name("name");
        let raw_name = name_node
            .map(|n| {
                let text = self.node_text(&n);
                text.strip_prefix('"')
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or(&text)
                    .to_string()
            })
            .unwrap_or_default();

        let full_name = if prefix.is_empty() {
            raw_name
        } else {
            format!("{} > {}", prefix, raw_name)
        };

        // Collect hooks at this level
        let mut local_before: Vec<Node> = before_hooks.to_vec();
        let mut local_after: Vec<Node> = after_hooks.to_vec();
        {
            let mut scan_cursor = node.walk();
            for child in node.named_children(&mut scan_cursor) {
                match child.kind() {
                    "before_each_block" => {
                        if let Some(expr) = child.named_child(0) {
                            local_before.push(expr);
                        }
                    }
                    "after_each_block" => {
                        if let Some(expr) = child.named_child(0) {
                            local_after.push(expr);
                        }
                    }
                    _ => {}
                }
            }
        }

        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            match child.kind() {
                "it_block" => {
                    if let Some(t) = self.lower_it_block(
                        &child, &full_name, has_only, &local_before, &local_after,
                    )? {
                        out.push(t);
                    }
                }
                "inline_test" => {
                    if let Some(mut t) = self.lower_inline_test(&child)? {
                        t.name = format!("{} > {}", full_name, t.name);
                        out.push(t);
                    }
                }
                "describe_block" => {
                    self.collect_describe_tests(
                        &child, &full_name, has_only, &local_before, &local_after, out,
                    )?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Lower a BDD it_block as a standalone test.
    pub(super) fn lower_it_block(
        &mut self,
        node: &Node,
        prefix: &str,
        has_only: bool,
        before_hooks: &[Node],
        after_hooks: &[Node],
    ) -> Result<Option<IrTest>, CompileError> {
        let name_node = match node.child_by_field_name("name") {
            Some(n) => n,
            None => return Ok(None),
        };
        let raw_name = self.node_text(&name_node);
        let name = raw_name
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or(&raw_name)
            .to_string();

        let full_name = if prefix.is_empty() {
            name
        } else {
            format!("{} > {}", prefix, name)
        };

        let modifier = node
            .child_by_field_name("modifier")
            .map(|n| self.node_text(&n));

        let skip = match modifier.as_deref() {
            Some(".skip") => true,
            Some(".only") => false,
            _ => has_only,
        };

        let body_node = match node.child_by_field_name("body") {
            Some(n) => n,
            None => return Ok(None),
        };

        // Build the test body: before_hooks + body + after_hooks
        let mut body_exprs = Vec::new();

        for hook_expr in before_hooks {
            body_exprs.push(self.lower_expression(hook_expr)?);
        }

        let test_body = self.lower_expression(&body_node)?;

        if after_hooks.is_empty() {
            body_exprs.push(test_body);
        } else {
            // Wrap in a let to preserve the result across after hooks
            body_exprs.push(Expr::Let {
                pattern: Box::new(Pattern::Var("__test_result__".into())),
                value: Box::new(test_body),
                ty: None,
            });

            for hook_expr in after_hooks.iter().rev() {
                body_exprs.push(self.lower_expression(hook_expr)?);
            }

            body_exprs.push(Expr::Var("__test_result__".into(), None));
        }

        let body = if body_exprs.len() == 1 {
            body_exprs.into_iter().next().unwrap()
        } else {
            Expr::Block(body_exprs, None)
        };

        let start = node.start_position();
        let end = node.end_position();

        Ok(Some(IrTest {
            name: full_name,
            body,
            line: start.row + 1,
            col: start.column + 1,
            end_line: end.row + 1,
            end_col: end.column + 1,
            skip,
        }))
    }
}
