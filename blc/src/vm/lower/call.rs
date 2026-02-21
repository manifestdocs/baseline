use std::collections::HashMap;
use tree_sitter::Node;

use crate::vm::chunk::CompileError;
use crate::vm::ir::*;

impl<'a> super::Lowerer<'a> {
    pub(super) fn lower_call(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.named_children(&mut cursor).collect();

        if children.is_empty() {
            return Err(self.error("Call expression missing callee".into(), node));
        }

        let callee = &children[0];
        let arg_nodes = &children[1..];

        // `resume(val)` -- kept as CallIndirect so it flows through to the VM.
        // The VM handles resume by calling the continuation value.

        // Constructor call: type_identifier(args) -> MakeEnum
        // Constructor arguments are never in tail position — they produce
        // the payload for a data constructor, not the function's return value.
        if callee.kind() == "type_identifier" {
            let tag = self.node_text(callee);
            let was_tail = self.tail_position;
            self.tail_position = false;
            let payload = if arg_nodes.len() == 1 {
                self.lower_expression(&arg_nodes[0])?
            } else if arg_nodes.is_empty() {
                Expr::Unit
            } else {
                let mut elems = Vec::new();
                for arg in arg_nodes {
                    elems.push(self.lower_expression(arg)?);
                }
                Expr::MakeTuple(elems, None)
            };
            self.tail_position = was_tail;
            return Ok(Expr::MakeEnum {
                tag,
                payload: Box::new(payload),
                ty: None,
            });
        }

        // TCO: self-recursive tail call
        if self.tail_position
            && (callee.kind() == "identifier" || callee.kind() == "effect_identifier")
            && let Some(ref fn_name) = self.current_fn_name
        {
            let callee_name = self.node_text(callee);
            if callee_name == *fn_name {
                let was_tail = self.tail_position;
                self.tail_position = false;
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                return Ok(Expr::TailCall {
                    name: callee_name,
                    args,
                    ty: None,
                });
            }
        }

        let was_tail = self.tail_position;
        self.tail_position = false;

        // Trait method call: check if the type checker resolved a mangled name for this callee
        if callee.kind() == "field_expression" {
            let mangled_name = self.type_map.as_ref().and_then(|tm| {
                let key = callee.start_byte();
                if let Some(ty) = tm.get(&key) {
                    if let crate::analysis::types::Type::Module(mangled) = ty {
                        if mangled.contains('$') {
                            return Some(mangled.clone());
                        }
                    }
                }
                None
            });
            if let Some(mangled) = mangled_name {
                // Dictionary passing: __dict$TraitName$method$ParamName markers
                // mean we're inside a bounded generic body — call through hidden param
                if mangled.starts_with("__dict$") {
                    // Parse: __dict$TraitName$method$ParamName
                    let parts: Vec<&str> = mangled.splitn(4, '$').collect();
                    if parts.len() >= 3 {
                        let trait_name = parts[1];
                        let method = parts[2];
                        let hidden_param = format!("__{}_{}", trait_name, method);
                        let mut args = Vec::new();
                        for arg in arg_nodes {
                            args.push(self.lower_expression(arg)?);
                        }
                        self.tail_position = was_tail;
                        return Ok(Expr::CallIndirect {
                            callee: Box::new(Expr::Var(hidden_param, None)),
                            args,
                            ty: None,
                        });
                    }
                }
                // Concrete trait dispatch: call the mangled function directly
                if self.functions.contains(&mangled) {
                    let mut args = Vec::new();
                    for arg in arg_nodes {
                        args.push(self.lower_expression(arg)?);
                    }
                    self.tail_position = was_tail;
                    return Ok(Expr::CallDirect {
                        name: mangled,
                        args,
                        ty: None,
                    });
                }
            }
        }

        // Row.decode(row, TypeName) — rewrite type arg into field spec constant
        if callee.kind() == "field_expression"
            && let Some((ref _mod, ref _meth, ref qualified)) = self.try_resolve_qualified(callee)
        {
            if qualified == "Row.decode" && arg_nodes.len() == 2 {
                if let Some(struct_type) = self.type_map.as_ref().and_then(|tm| tm.get(&node.start_byte())) {
                    if let crate::analysis::types::Type::Struct(name, fields) = struct_type.clone() {
                        let row_expr = self.lower_expression(&arg_nodes[0])?;
                        let field_spec = build_field_spec_expr(&fields);
                        let name_expr = Expr::String(name.clone());
                        self.tail_position = was_tail;
                        return Ok(Expr::CallNative {
                            module: "Row".to_string(),
                            method: "decode".to_string(),
                            args: vec![row_expr, field_spec, name_expr],
                            ty: None,
                        });
                    }
                }
            }

        }

        // Native call: Module.method(args)
        if callee.kind() == "field_expression"
            && let Some((module, method, qualified)) = self.try_resolve_qualified(callee)
        {
            if self.natives.lookup(&qualified).is_some()
                && !self.handled_effects.contains(&qualified)
            {
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                return Ok(Expr::CallNative {
                    module,
                    method,
                    args,
                    ty: None,
                });
            }
            // Imported module function
            if self.functions.contains(&qualified) {
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                return Ok(Expr::CallDirect {
                    name: qualified,
                    args,
                    ty: None,
                });
            }
            // Auto-derived enum methods: Enum.to_string / Enum.parse
            if let Some(variants) = self.enum_defs.get(&module).cloned() {
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                let result = super::helpers::generate_enum_method(
                    &module, &method, &variants, args,
                );
                if let Some(expr) = result {
                    return Ok(expr);
                }
            }
            // User-defined effect: Module.method!(args) where Module is not native
            if method.ends_with('!') {
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                let method_key = method.strip_suffix('!').unwrap_or(&method).to_string();
                return Ok(Expr::PerformEffect {
                    effect: module,
                    method: method_key,
                    args,
                    ty: None,
                });
            }
        }

        // Standalone native call (e.g. scope!)
        if callee.kind() == "effect_identifier" || callee.kind() == "identifier" {
            let callee_name = self.node_text(callee);
            if self.natives.lookup(&callee_name).is_some()
                && !self.handled_effects.contains(&callee_name)
            {
                let mut args = Vec::new();
                for arg in arg_nodes {
                    args.push(self.lower_expression(arg)?);
                }
                self.tail_position = was_tail;
                // Standalone natives have no module prefix
                return Ok(Expr::CallNative {
                    module: String::new(),
                    method: callee_name,
                    args,
                    ty: None,
                });
            }
        }

        // Named function call (identifier or effect_identifier like scope!)
        if callee.kind() == "identifier" || callee.kind() == "effect_identifier" {
            let callee_name = self.node_text(callee);
            if self.functions.contains(&callee_name) {
                let mut args = self.resolve_named_call_args(&callee_name.clone(), arg_nodes)?;
                // Append hidden dictionary args for bounded generic calls
                self.append_dict_args(node, &mut args);
                self.tail_position = was_tail;
                return Ok(Expr::CallDirect {
                    name: callee_name,
                    args,
                    ty: None,
                });
            }
        }

        // Indirect call (closure/variable)
        let callee_expr = self.lower_expression(callee)?;
        let mut args = Vec::new();
        for arg in arg_nodes {
            args.push(self.lower_expression(arg)?);
        }
        self.tail_position = was_tail;

        // Emit TailCallIndirect when in tail position (enables return_call_indirect in JIT)
        if was_tail {
            Ok(Expr::TailCallIndirect {
                callee: Box::new(callee_expr),
                args,
                ty: None,
            })
        } else {
            Ok(Expr::CallIndirect {
                callee: Box::new(callee_expr),
                args,
                ty: None,
            })
        }
    }

    pub(super) fn try_resolve_qualified(&self, field_expr: &Node) -> Option<(String, String, String)> {
        let obj = field_expr.named_child(0)?;
        let method = field_expr.named_child(1)?;
        let module = if obj.kind() == "type_identifier" {
            self.node_text(&obj)
        } else if obj.kind() == "identifier" {
            // Resolve short module aliases (e.g. "str" -> "String")
            let name = self.node_text(&obj);
            if let Some(canonical) = crate::prelude::module_alias(&name) {
                canonical.to_string()
            } else {
                return None;
            }
        } else {
            return None;
        };
        let method_name = self.node_text(&method);
        let qualified = format!("{}.{}", module, method_name);
        Some((module, method_name, qualified))
    }

    // -----------------------------------------------------------------------
    // Pipe operator (desugared into calls)
    // -----------------------------------------------------------------------

    pub(super) fn lower_pipe(&mut self, node: &Node) -> Result<Expr, CompileError> {
        let lhs_node = node
            .named_child(0)
            .ok_or_else(|| self.error("Pipe missing left operand".into(), node))?;
        let rhs_node = node
            .named_child(1)
            .ok_or_else(|| self.error("Pipe missing right operand".into(), node))?;

        let lhs = self.lower_expression(&lhs_node)?;

        if rhs_node.kind() == "call_expression" {
            // x |> f(a, b) -> f(x, a, b)
            let mut cursor = rhs_node.walk();
            let rhs_children: Vec<Node> = rhs_node.named_children(&mut cursor).collect();

            if rhs_children.is_empty() {
                return Err(self.error("Pipe RHS call missing callee".into(), &rhs_node));
            }

            let callee = &rhs_children[0];
            let extra_args = &rhs_children[1..];

            // Check for native module call
            if callee.kind() == "field_expression"
                && let Some((module, method, qualified)) = self.try_resolve_qualified(callee)
            {
                if self.natives.lookup(&qualified).is_some() {
                    let mut args = vec![lhs];
                    for arg in extra_args {
                        args.push(self.lower_expression(arg)?);
                    }
                    return Ok(Expr::CallNative {
                        module,
                        method,
                        args,
                        ty: None,
                    });
                }
                // Imported module function
                if self.functions.contains(&qualified) {
                    let mut args = vec![lhs];
                    for arg in extra_args {
                        args.push(self.lower_expression(arg)?);
                    }
                    return Ok(Expr::CallDirect {
                        name: qualified,
                        args,
                        ty: None,
                    });
                }
            }

            // Named function
            if callee.kind() == "identifier" {
                let callee_name = self.node_text(callee);
                if self.functions.contains(&callee_name) {
                    let mut args = vec![lhs];
                    for arg in extra_args {
                        args.push(self.lower_expression(arg)?);
                    }
                    return Ok(Expr::CallDirect {
                        name: callee_name,
                        args,
                        ty: None,
                    });
                }
            }

            // Generic indirect call
            let callee_expr = self.lower_expression(callee)?;
            let mut args = vec![lhs];
            for arg in extra_args {
                args.push(self.lower_expression(arg)?);
            }
            Ok(Expr::CallIndirect {
                callee: Box::new(callee_expr),
                args,
                ty: None,
            })
        } else if rhs_node.kind() == "field_expression" {
            // x |> Module.method -> CallNative/CallDirect(x)
            if let Some((module, method, qualified)) = self.try_resolve_qualified(&rhs_node) {
                if self.natives.lookup(&qualified).is_some() {
                    return Ok(Expr::CallNative {
                        module,
                        method,
                        args: vec![lhs],
                        ty: None,
                    });
                }
                if self.functions.contains(&qualified) {
                    return Ok(Expr::CallDirect {
                        name: qualified,
                        args: vec![lhs],
                        ty: None,
                    });
                }
            }
            // Not a native -- regular indirect call
            let callee_expr = self.lower_expression(&rhs_node)?;
            Ok(Expr::CallIndirect {
                callee: Box::new(callee_expr),
                args: vec![lhs],
                ty: None,
            })
        } else {
            // x |> f -> f(x)
            let rhs = self.lower_expression(&rhs_node)?;

            // Check if rhs is a known function var
            if let Expr::Var(ref name, _) = rhs
                && self.functions.contains(name)
            {
                return Ok(Expr::CallDirect {
                    name: name.clone(),
                    args: vec![lhs],
                    ty: None,
                });
            }

            Ok(Expr::CallIndirect {
                callee: Box::new(rhs),
                args: vec![lhs],
                ty: None,
            })
        }
    }

    /// Resolve named arguments in call args, reordering by parameter names.
    /// Returns args in positional order, or the original args if no named args present.
    pub(super) fn resolve_named_call_args(
        &mut self,
        fn_name: &str,
        arg_nodes: &[Node],
    ) -> Result<Vec<Expr>, CompileError> {
        let has_named = arg_nodes.iter().any(|a| a.kind() == "named_argument");
        if !has_named {
            // All positional -- lower as-is
            let mut args = Vec::new();
            for arg in arg_nodes {
                args.push(self.lower_expression(arg)?);
            }
            return Ok(args);
        }

        let param_names = self.fn_params.get(fn_name).cloned().unwrap_or_default();

        // Split into positional and named
        let mut positional = Vec::new();
        let mut named: Vec<(String, Node)> = Vec::new();
        for arg in arg_nodes {
            if arg.kind() == "named_argument" {
                if let Some(name_node) = arg.child_by_field_name("name") {
                    let name = self.node_text(&name_node);
                    let count = arg.named_child_count();
                    let expr_node = arg.named_child(count - 1).unwrap();
                    named.push((name, expr_node));
                }
            } else {
                positional.push(*arg);
            }
        }

        // Build result array
        let total = param_names.len().max(positional.len() + named.len());
        let mut result: Vec<Option<Expr>> = vec![None; total];

        // Place positional args
        for (i, arg) in positional.iter().enumerate() {
            result[i] = Some(self.lower_expression(arg)?);
        }

        // Place named args by matching parameter names
        for (name, expr_node) in &named {
            if let Some(pos) = param_names.iter().position(|p| p == name) {
                result[pos] = Some(self.lower_expression(expr_node)?);
            } else {
                // Unknown param name -- lower in order (type checker will catch it)
                let lowered = self.lower_expression(expr_node)?;
                if let Some(slot) = result.iter().position(|r| r.is_none()) {
                    result[slot] = Some(lowered);
                }
            }
        }

        // Fill any remaining None slots with Unit
        Ok(result
            .into_iter()
            .map(|r| r.unwrap_or(Expr::Unit))
            .collect())
    }

    /// Append hidden dictionary arguments for a bounded generic call site.
    /// Reads dict_map entries at the call_expression's start_byte, and for
    /// each trait bound's methods, appends Expr::Var(mangled_impl_name).
    /// Entries are sorted by trait name then method name for deterministic order.
    pub(super) fn append_dict_args(&self, call_node: &Node, args: &mut Vec<Expr>) {
        if let Some(entries) = self.dict_map.get(&call_node.start_byte()) {
            // Sort entries by trait name for deterministic parameter ordering
            let mut sorted_entries: Vec<&crate::analysis::types::DictEntry> =
                entries.iter().collect();
            sorted_entries.sort_by_key(|e| &e.trait_name);
            for entry in sorted_entries {
                let mut methods: Vec<&(String, String)> = entry.methods.iter().collect();
                methods.sort_by_key(|(name, _)| name.clone());
                for (_method_name, mangled_impl) in methods {
                    args.push(Expr::Var(mangled_impl.clone(), None));
                }
            }
        }
    }
}

/// Convert a struct field type to a type tag string for the Row.decode field spec.
fn type_to_tag(ty: &crate::analysis::types::Type) -> &'static str {
    use crate::analysis::types::Type;
    match ty {
        Type::Int => "Int",
        Type::Float => "Float",
        Type::String => "String",
        Type::Bool => "Bool",
        Type::Enum(name, variants) if name == "Option" => {
            if let Some((_, payload)) = variants.iter().find(|(tag, _)| tag == "Some") {
                if payload.len() == 1 {
                    match &payload[0] {
                        Type::Int => "Option<Int>",
                        Type::Float => "Option<Float>",
                        Type::String => "Option<String>",
                        Type::Bool => "Option<Bool>",
                        _ => "String", // fallback
                    }
                } else {
                    "String"
                }
            } else {
                "String"
            }
        }
        _ => "String", // fallback
    }
}

/// Build a field spec expression: List<List<String>> like [["id","Int"],["name","String"],...].
/// Fields are sorted by name for deterministic ordering.
fn build_field_spec_expr(fields: &HashMap<String, crate::analysis::types::Type>) -> Expr {
    let mut sorted_fields: Vec<_> = fields.iter().collect();
    sorted_fields.sort_by_key(|(name, _)| (*name).clone());

    let entries: Vec<Expr> = sorted_fields
        .into_iter()
        .map(|(name, ty)| {
            let tag = type_to_tag(ty);
            Expr::MakeList(
                vec![
                    Expr::String(name.clone()),
                    Expr::String(tag.to_string()),
                ],
                None,
            )
        })
        .collect();
    Expr::MakeList(entries, None)
}
