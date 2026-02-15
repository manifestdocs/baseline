use std::collections::{HashMap, HashSet};
use tree_sitter::Node;

use crate::analysis::types::TypeMap;

use super::chunk::CompileError;
use super::ir::*;
use super::natives::NativeRegistry;

mod call;
mod control;
mod data;
mod effects;
mod helpers;
mod literals;
mod operators;
mod test_lower;
#[cfg(test)]
mod tests;

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
    /// Parameter names for known functions (for named argument resolution).
    fn_params: HashMap<String, Vec<String>>,
    /// Name of the function currently being lowered (for TCO detection).
    current_fn_name: Option<String>,
    /// Whether the current expression is in tail position.
    tail_position: bool,
    /// Stack of enclosing function parameter sets (for upvalue/capture detection).
    scopes: Vec<HashSet<String>>,
    /// Effect methods currently being handled by an enclosing `handle` expression.
    /// Stored as qualified names like "Console.println" so that native calls
    /// matching these keys are compiled as PerformEffect instead of CallNative.
    handled_effects: HashSet<String>,
    /// Simple enum definitions: enum name → variant names (nullary only).
    enum_defs: HashMap<String, Vec<String>>,
}

impl<'a> Lowerer<'a> {
    pub fn new(source: &'a str, natives: &'a NativeRegistry, type_map: Option<TypeMap>) -> Self {
        Lowerer {
            source,
            type_map,
            natives,
            functions: HashSet::new(),
            fn_params: HashMap::new(),
            current_fn_name: None,
            tail_position: false,
            scopes: Vec::new(),
            handled_effects: HashSet::new(),
            enum_defs: HashMap::new(),
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
    pub fn lower_module(&mut self, root: &Node) -> Result<IrModule, CompileError> {
        // First pass: collect function names and parameter names
        // Handles both top-level function_def and function_def wrapped in spec_block
        let mut func_nodes: Vec<(String, usize)> = Vec::new();
        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            let func = if child.kind() == "function_def" {
                Some(child)
            } else if child.kind() == "spec_block" {
                let mut found = None;
                for j in 0..child.named_child_count() {
                    let sc = child.named_child(j).unwrap();
                    if sc.kind() == "function_def" {
                        found = Some(sc);
                        break;
                    }
                }
                found
            } else {
                None
            };
            if let Some(func_node) = func
                && let Some(name_node) = func_node.child_by_field_name("name")
            {
                let name = self.node_text(&name_node);
                self.functions.insert(name.clone());
                let params = self.extract_param_names(&func_node);
                self.fn_params.insert(name.clone(), params);
                func_nodes.push((name, i));
            }
            // Collect simple enum definitions for auto-derived methods
            if child.kind() == "type_def" {
                self.collect_enum_def(&child);
            }
        }

        if func_nodes.is_empty() {
            return Err(CompileError {
                message: "No function definitions found".into(),
                line: 1,
                col: 0,
            });
        }

        // Second pass: lower each function
        let mut functions = Vec::new();
        for (name, child_idx) in &func_nodes {
            let child = root.named_child(*child_idx).unwrap();
            // Unwrap spec_block to get to the function_def
            let func_node = if child.kind() == "spec_block" {
                let mut found = child;
                for j in 0..child.named_child_count() {
                    let sc = child.named_child(j).unwrap();
                    if sc.kind() == "function_def" {
                        found = sc;
                        break;
                    }
                }
                found
            } else {
                child
            };
            let func = self.lower_function_def(&func_node, name)?;
            functions.push(func);
        }

        // Find entry point
        let entry = functions
            .iter()
            .position(|f| f.name == "main!" || f.name == "main")
            .ok_or_else(|| CompileError {
                message: "No 'main' or 'main!' function found".into(),
                line: 1,
                col: 0,
            })?;
        // Prefer main! over main
        let entry = functions
            .iter()
            .position(|f| f.name == "main!")
            .unwrap_or(entry);

        Ok(IrModule { functions, entry, tags: TagRegistry::new() })
    }

    /// Lower function definitions for a module (no entry point required).
    pub fn lower_module_functions(&mut self, root: &Node) -> Result<Vec<IrFunction>, CompileError> {
        // First pass: collect function names (unwrap spec_block)
        let mut func_nodes: Vec<(String, usize)> = Vec::new();
        for i in 0..root.named_child_count() {
            let child = root.named_child(i).unwrap();
            let func = if child.kind() == "function_def" {
                Some(child)
            } else if child.kind() == "spec_block" {
                let mut found = None;
                for j in 0..child.named_child_count() {
                    let sc = child.named_child(j).unwrap();
                    if sc.kind() == "function_def" {
                        found = Some(sc);
                        break;
                    }
                }
                found
            } else {
                None
            };
            if let Some(func_node) = func
                && let Some(name_node) = func_node.child_by_field_name("name")
            {
                let name = self.node_text(&name_node);
                self.functions.insert(name.clone());
                func_nodes.push((name, i));
            }
            if child.kind() == "type_def" {
                self.collect_enum_def(&child);
            }
        }

        let mut functions = Vec::new();
        for (name, child_idx) in &func_nodes {
            let child = root.named_child(*child_idx).unwrap();
            let func_node = if child.kind() == "spec_block" {
                let mut found = child;
                for j in 0..child.named_child_count() {
                    let sc = child.named_child(j).unwrap();
                    if sc.kind() == "function_def" {
                        found = sc;
                        break;
                    }
                }
                found
            } else {
                child
            };
            let func = self.lower_function_def(&func_node, name)?;
            functions.push(func);
        }

        Ok(functions)
    }

    fn lower_function_def(&mut self, node: &Node, name: &str) -> Result<IrFunction, CompileError> {
        let body_node = node
            .child_by_field_name("body")
            .ok_or_else(|| self.error("Function missing body".into(), node))?;

        let span = self.span(node);
        self.current_fn_name = Some(name.to_string());

        // Extract params from param_list
        let mut params = Vec::new();
        if let Some(param_list) = node.child_by_field_name("params") {
            let mut cursor = param_list.walk();
            for param in param_list.named_children(&mut cursor) {
                if param.kind() == "param"
                    && let Some(name_node) = param.child_by_field_name("name")
                {
                    params.push(self.node_text(&name_node));
                }
            }
        }

        // Enter scope with params and lower body
        let param_set: HashSet<String> = params.iter().cloned().collect();
        self.scopes.push(param_set);
        self.tail_position = true;
        let body = self.lower_expression(&body_node)?;
        self.tail_position = false;
        self.scopes.pop();

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

    pub fn lower_expression(&mut self, node: &Node) -> Result<Expr, CompileError> {
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
            "string_literal" | "multiline_string_literal" => self.lower_string_literal(node),
            "raw_string_literal" | "raw_hash_string_literal" => self.lower_raw_string(node),
            "tuple_expression" => self.lower_tuple(node),
            "unary_expression" => self.lower_unary(node),
            "binary_expression" => self.lower_binary(node),
            "identifier" | "effect_identifier" => self.lower_identifier(node),
            "for_expression" => self.lower_for(node),
            "hole_expression" => Ok(Expr::Hole),
            "named_argument" => {
                // Lower the expression part of a named argument
                let count = node.named_child_count();
                if count > 0 {
                    self.lower_expression(&node.named_child(count - 1).unwrap())
                } else {
                    Ok(Expr::Unit)
                }
            }
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
            "with_expression" => self.lower_with_expression(node),
            "handle_expression" => self.lower_handle_expression(node),
            "restrict_expression" => self.lower_restrict_expression(node),
            "map_literal" => self.lower_map_literal(node),
            "set_literal" => self.lower_set_literal(node),
            "map_entry" => Ok(Expr::Unit),
            "expect_expression" => self.lower_expect(node),
            "matcher" => Ok(Expr::Bool(true)),
            "describe_block" | "it_block" | "before_each_block" | "after_each_block" => {
                Ok(Expr::Unit)
            }
            "handler_map" | "handler_binding" | "handler_clause" => Ok(Expr::Unit),
            "line_comment" | "block_comment" => Ok(Expr::Unit),
            _ => Err(self.error(format!("Unsupported expression kind: {}", kind), node)),
        }
    }
}
