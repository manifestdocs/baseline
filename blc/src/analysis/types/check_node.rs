use super::check_lambda::{bind_pattern, check_lambda_with_expected};
use super::check_pattern::{check_match_exhaustiveness, check_pattern};
use super::symbol_table::SymbolTable;
use super::type_compat::{detect_implicit_type_params, extract_explicit_type_params, extract_type_param_bounds, types_compatible};
use super::type_def::Type;
use super::type_parse::{call_arg_expr, infer_expected_type, parse_type};
use crate::diagnostics::{Diagnostic, Location, Severity, Suggestion};
use super::super::infer::{InferCtx, UserGenericSchema};
use std::collections::HashSet;
use tree_sitter::Node;

/// Check if a type can be decoded from a SQL row.
/// Compatible types: Int, Float, String, Bool, Option<Int>, Option<String>, Option<Float>, Option<Bool>.
fn is_decode_compatible(ty: &Type) -> bool {
    match ty {
        Type::Int | Type::Float | Type::String | Type::Bool => true,
        Type::Enum(name, variants) if name == "Option" => {
            // Option<T> where T is a scalar type
            if let Some((_, payload)) = variants.iter().find(|(tag, _)| tag == "Some") {
                payload.len() == 1 && matches!(payload[0], Type::Int | Type::Float | Type::String | Type::Bool)
            } else {
                false
            }
        }
        Type::Unknown => true, // Allow Unknown to not cascade errors
        _ => false,
    }
}

/// Extract a field name from an identifier or string_literal node.
/// For identifiers, returns the text directly.
/// For string_literal nodes, concatenates string_content children and rejects interpolation.
pub(super) fn extract_field_name(
    node: &Node,
    source: &str,
    file: &str,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<String> {
    match node.kind() {
        "identifier" => Some(node.utf8_text(source.as_bytes()).unwrap().to_string()),
        "string_literal" => {
            let mut name = String::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                match child.kind() {
                    "string_content" => {
                        name.push_str(child.utf8_text(source.as_bytes()).unwrap());
                    }
                    "interpolation" => {
                        diagnostics.push(Diagnostic {
                            code: "TYP_030".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, &child),
                            message: "Interpolation is not allowed in quoted field names".to_string(),
                            context: "Use a plain string literal for field names.".to_string(),
                            suggestions: vec![],
                        });
                        return None;
                    }
                    // string_start, string_end, escape_sequence — skip delimiters, allow escapes
                    "escape_sequence" => {
                        name.push_str(child.utf8_text(source.as_bytes()).unwrap());
                    }
                    _ => {}
                }
            }
            Some(name)
        }
        _ => Some(node.utf8_text(source.as_bytes()).unwrap().to_string()),
    }
}

/// Check a single `inline_test` node: the expression must type-check to Bool.
pub(super) fn check_inline_test(
    test_node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // inline_test: "test" string_literal "=" _expression
    // The expression is the last named child
    let count = test_node.named_child_count();
    if count == 0 {
        return;
    }
    let expr_node = test_node.named_child(count - 1).unwrap();
    // Skip string_literal (the test name)
    if expr_node.kind() == "string_literal" {
        return;
    }
    let test_type = check_node(&expr_node, source, file, symbols, diagnostics);
    if test_type != Type::Bool && test_type != Type::Unknown {
        diagnostics.push(Diagnostic {
            code: "TYP_026".to_string(),
            severity: Severity::Error,
            location: Location::from_node(file,&expr_node),
            message: format!("Inline test expression must be Bool, found {}", test_type),
            context: "Test expressions should evaluate to true or false.".to_string(),
            suggestions: vec![],
        });
    }
}


/// Extract a qualified name like "List.map" from a field_expression node.
pub(super) fn extract_qualified_name(node: &Node, source: &str) -> Option<String> {
    if node.kind() == "field_expression" {
        let obj = node.named_child(0)?;
        let field = node.named_child(1)?;
        let obj_name = obj.utf8_text(source.as_bytes()).ok()?;
        let field_name = field.utf8_text(source.as_bytes()).ok()?;
        Some(format!("{}.{}", obj_name, field_name))
    } else {
        None
    }
}

pub(super) fn check_node(
    node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) -> Type {
    let ty = check_node_inner(node, source, file, symbols, diagnostics);
    // Record the resolved type for this node (used by the VM compiler for specialization).
    // Don't overwrite trait dispatch entries (mangled names stored as Type::Module with '$').
    if !matches!(symbols.type_map.get(&node.start_byte()), Some(Type::Module(m)) if m.contains('$')) {
        symbols.type_map.insert(node.start_byte(), ty.clone());
    }
    ty
}

fn check_node_inner(
    node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) -> Type {
    let k = node.kind();

    match k {
        "source_file" | "module_decl" => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_node(&child, source, file, symbols, diagnostics);
            }
            Type::Unit
        }
        "prelude_decl" | "import_decl" => Type::Unit,
        "spec_block" => {
            // Walk into spec_block to type-check the wrapped definition
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                if child.kind() == "function_def" || child.kind() == "type_def" || child.kind() == "effect_def" {
                    check_node(&child, source, file, symbols, diagnostics);
                }
                // spec_attribute children are ignored by the type checker
            }
            Type::Unit
        }
        "spec_attribute" | "spec_decl" | "given_clause" | "returns_clause"
        | "requires_clause" | "ensures_clause" | "assume_clause"
        | "pure_attribute" | "total_attribute" => Type::Unit,
        "try_expression" => {
            // expr? — unwraps Option<T> to T or Result<T,E> to T
            let inner = check_node(
                &node.named_child(0).unwrap(),
                source,
                file,
                symbols,
                diagnostics,
            );
            match &inner {
                Type::Enum(name, variants) if name == "Option" => variants
                    .iter()
                    .find(|(v, _)| v == "Some")
                    .and_then(|(_, payloads)| payloads.first().cloned())
                    .unwrap_or(Type::Unknown),
                Type::Enum(name, variants) if name == "Result" => variants
                    .iter()
                    .find(|(v, _)| v == "Ok")
                    .and_then(|(_, payloads)| payloads.first().cloned())
                    .unwrap_or(Type::Unknown),
                _ => Type::Unknown,
            }
        }
        "effect_def" => {
            // effect_def: seq(..., 'effect', $.type_identifier, ...)
            // Manual search since field access creates panic
            let mut name = String::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "type_identifier" {
                    name = child.utf8_text(source.as_bytes()).unwrap().to_string();
                    break;
                }
            }
            if !name.is_empty() {
                symbols.insert_type(name.clone(), Type::Module(name));
            }
            Type::Unit
        }
        "trait_def" => {
            // Already handled in collect_signatures — validate supertraits here
            check_trait_def(node, source, file, symbols, diagnostics);
            Type::Unit
        }
        "impl_block" => {
            check_impl_block(node, source, file, symbols, diagnostics);
            Type::Unit
        }
        "type_def" => {
            // type Point = { x: Int, y: Int }
            // type Status = | Active | Inactive | Pending(String)
            let name_node = node
                .child_by_field_name("name")
                .unwrap_or_else(|| node.child(1).unwrap());
            let name = name_node
                .utf8_text(source.as_bytes())
                .unwrap_or("Unknown")
                .to_string();

            // Def node is after '=' — use field if available, else positional
            let def_node_candidate = node
                .child_by_field_name("def")
                .unwrap_or_else(|| node.child(3).unwrap());

            if def_node_candidate.kind() == "variant_list" {
                // Sum type / enum
                let mut variants = Vec::new();
                let mut cursor = def_node_candidate.walk();
                for child in def_node_candidate.children(&mut cursor) {
                    if child.kind() == "variant" {
                        let vname_node = child.child_by_field_name("name").unwrap();
                        let vname = vname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                        let mut payload_types = Vec::new();

                        // Collect payload types: variant children after name that are type exprs
                        let vcount = child.child_count();
                        for vi in 0..vcount {
                            let vc = child.child(vi).unwrap();
                            if (vc.kind() != "type_identifier" || vc.id() != vname_node.id())
                                && vc.kind() != "|"
                                && vc.kind() != "("
                                && vc.kind() != ")"
                                && vc.kind() != ","
                            {
                                let pt = parse_type(&vc, source, symbols);
                                if pt != Type::Unknown {
                                    payload_types.push(pt);
                                }
                            }
                        }

                        variants.push((vname, payload_types));
                    }
                }

                let enum_type = Type::Enum(name.clone(), variants.clone());
                symbols.insert_type(name.clone(), enum_type.clone());

                // Register constructors as functions or values in the symbol table
                for (vname, payload) in &variants {
                    if payload.is_empty() {
                        // Nullary constructor: Active -> Enum type directly
                        symbols.insert(vname.clone(), enum_type.clone());
                    } else {
                        // Constructor with payload: Some(T) -> function T -> Enum
                        symbols.insert(
                            vname.clone(),
                            Type::Function(payload.clone(), Box::new(enum_type.clone())),
                        );
                    }
                }
            } else {
                let ty = parse_type(&def_node_candidate, source, symbols);

                // Check for refinement clause (e.g., `type Email = String where ...`)
                let has_refinement = {
                    let mut cursor2 = node.walk();
                    node.children(&mut cursor2)
                        .any(|c| c.kind() == "refinement_clause")
                };

                // If it's a record, wrap it in a Struct with the name
                let defined_type = match ty {
                    Type::Record(fields, _) => Type::Struct(name.clone(), fields),
                    _ if has_refinement => Type::Refined(Box::new(ty), name.clone()),
                    _ => ty,
                };

                symbols.insert_type(name, defined_type);
            }
            Type::Unit
        }
        "function_def" => {
            let name_node = node.child_by_field_name("name").unwrap();
            let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();

            // Detect type parameters (explicit or implicit)
            let mut type_param_names = extract_explicit_type_params(node, source);
            if type_param_names.is_empty() {
                type_param_names = detect_implicit_type_params(node, source, symbols);
            }
            let type_param_set: HashSet<String> = type_param_names.iter().cloned().collect();

            // Build function type with TypeParam for generic params
            let function_type =
                super::parse_function_type_with_params(node, source, symbols, &type_param_set);

            symbols.insert(name.clone(), function_type.clone());

            // Register user generic schema if type params present
            let bounds = extract_type_param_bounds(node, source);
            if !type_param_names.is_empty() {
                symbols.user_generic_schemas.insert(
                    name.clone(),
                    UserGenericSchema {
                        type_param_names,
                        fn_type: function_type.clone(),
                        bounds: bounds.clone(),
                    },
                );
            }

            if let Type::Function(arg_types, ret_type) = &function_type {
                symbols.enter_scope();

                // Set current_bounds for trait call resolution inside bounded generic bodies
                let prev_bounds = std::mem::replace(&mut symbols.current_bounds, bounds);

                // Bind params from param_list and register param names
                let mut param_names = Vec::new();
                if let Some(params) = node.child_by_field_name("params") {
                    let mut cursor = params.walk();
                    let mut i = 0;
                    for param in params.named_children(&mut cursor) {
                        if param.kind() == "param" {
                            let param_type = if i < arg_types.len() {
                                arg_types[i].clone()
                            } else {
                                Type::Unknown
                            };

                            if let Some(name_node) = param.child_by_field_name("name") {
                                let arg_name =
                                    name_node.utf8_text(source.as_bytes()).unwrap().to_string();
                                symbols.insert(arg_name.clone(), param_type.clone());
                                param_names.push(arg_name);
                            } else if let Some(pat_node) = param.child_by_field_name("pattern") {
                                // Bind pattern variables
                                bind_pattern(&pat_node, param_type, source, symbols);
                                
                                // Collect param name for named args if simple identifier
                                if pat_node.kind() == "identifier" {
                                    let arg_name = pat_node.utf8_text(source.as_bytes()).unwrap().to_string();
                                    param_names.push(arg_name);
                                } else {
                                    // Complex pattern parameters cannot be targeted by named arguments
                                    param_names.push("".to_string());
                                }
                            }
                            i += 1;
                        }
                    }
                }
                symbols.insert_fn_params(name.clone(), param_names);

                let body_node = node.child_by_field_name("body").unwrap();
                let body_type = check_node(&body_node, source, file, symbols, diagnostics);
                if !types_compatible(&body_type, ret_type) && **ret_type != Type::Unit {
                    let suggestions = crate::diagnostic_render::suggest_type_coercion(
                        &format!("{}", ret_type),
                        &format!("{}", body_type),
                    );
                    diagnostics.push(Diagnostic {
                        code: "TYP_006".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&body_node),
                        message: format!(
                            "Function `{}` declares return type {}, body returns {}",
                            name, ret_type, body_type
                        ),
                        context: "Function body return type must match signature.".to_string(),
                        suggestions,
                    });
                }

                symbols.current_bounds = prev_bounds;
                symbols.exit_scope();
            }

            Type::Unit
        }
        "inline_test" => {
            // Top-level inline test: test "name" = expr
            check_inline_test(node, source, file, symbols, diagnostics);
            Type::Unit
        }
        "test_section" => {
            // @test section containing inline_test and describe_block items
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                match child.kind() {
                    "inline_test" => {
                        check_inline_test(&child, source, file, symbols, diagnostics);
                    }
                    "describe_block" => {
                        check_node(&child, source, file, symbols, diagnostics);
                    }
                    _ => {}
                }
            }
            Type::Unit
        }
        "describe_block" => {
            // BDD describe/context block — type-check all items within
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                check_node(&child, source, file, symbols, diagnostics);
            }
            Type::Unit
        }
        "it_block" => {
            // BDD it block — body should be Bool (like inline_test)
            if let Some(body) = node.child_by_field_name("body") {
                let body_type = check_node(&body, source, file, symbols, diagnostics);
                if body_type != Type::Bool && body_type != Type::Unknown {
                    diagnostics.push(Diagnostic {
                        code: "TYP_026".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&body),
                        message: format!("Test expression must be Bool, found {}", body_type),
                        context: "it block body should evaluate to true or false.".to_string(),
                        suggestions: vec![],
                    });
                }
            }
            Type::Unit
        }
        "before_each_block" | "after_each_block" => {
            // Hook expressions — type-check body
            let count = node.named_child_count();
            if count > 0
                && let Some(expr) = node.named_child(count - 1)
            {
                check_node(&expr, source, file, symbols, diagnostics);
            }
            Type::Unit
        }
        "expect_expression" => {
            // expect <actual> <matcher> — type-check actual, return Bool
            if let Some(actual) = node.child_by_field_name("actual") {
                check_node(&actual, source, file, symbols, diagnostics);
            }
            if let Some(matcher) = node.child_by_field_name("matcher") {
                check_node(&matcher, source, file, symbols, diagnostics);
            }
            Type::Bool
        }
        "matcher" => {
            // Matcher expressions — type-check any sub-expressions
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                check_node(&child, source, file, symbols, diagnostics);
            }
            Type::Bool
        }
        "map_literal" => {
            // #{ key: value, ... } — infer Map<K, V> from entries
            let mut key_type = Type::Unknown;
            let mut val_type = Type::Unknown;
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                if child.kind() == "map_entry" {
                    if let Some(k) = child.child_by_field_name("key") {
                        let kt = check_node(&k, source, file, symbols, diagnostics);
                        if key_type == Type::Unknown {
                            key_type = kt;
                        }
                    }
                    if let Some(v) = child.child_by_field_name("value") {
                        let vt = check_node(&v, source, file, symbols, diagnostics);
                        if val_type == Type::Unknown {
                            val_type = vt;
                        }
                    }
                }
            }
            Type::Map(Box::new(key_type), Box::new(val_type))
        }
        "map_entry" => {
            // Handled within map_literal
            Type::Unit
        }
        "set_literal" => {
            // #{ val1, val2, ... } — infer Set<T> from first element
            let mut elem_type = Type::Unknown;
            let mut cursor = node.walk();
            for child in node.named_children(&mut cursor) {
                let t = check_node(&child, source, file, symbols, diagnostics);
                if elem_type == Type::Unknown {
                    elem_type = t;
                }
            }
            Type::Set(Box::new(elem_type))
        }
        "let_binding" => {
            // Grammar: let pattern [: type_annotation] = expression
            // named_child(0) = pattern, last named_child = expression
            // type_annotation accessed via field name "type"
            let named_count = node.named_child_count();
            if let Some(pattern) = node.named_child(0) {
                let expr_node = node.named_child(named_count - 1).unwrap();
                let expr_type = check_node(&expr_node, source, file, symbols, diagnostics);

                // Check type annotation if present
                if let Some(ann_node) = node.child_by_field_name("type") {
                    // type_annotation is `: Type`, the type_expr is its named child
                    if let Some(type_node) = ann_node.named_child(0) {
                        let declared_type = parse_type(&type_node, source, symbols);
                        if !types_compatible(&expr_type, &declared_type) {
                            let suggestions = crate::diagnostic_render::suggest_type_coercion(
                                &format!("{}", declared_type),
                                &format!("{}", expr_type),
                            );
                            diagnostics.push(Diagnostic {
                                code: "TYP_021".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,&expr_node),
                                message: format!(
                                    "Binding type mismatch: declared {}, found {}",
                                    declared_type, expr_type
                                ),
                                context: "Expression type must match declared type annotation."
                                    .to_string(),
                                suggestions,
                            });
                        }
                        bind_pattern(&pattern, declared_type, source, symbols);
                    } else {
                        bind_pattern(&pattern, expr_type, source, symbols);
                    }
                } else {
                    bind_pattern(&pattern, expr_type, source, symbols);
                }
            }
            Type::Unit
        }
        "call_expression" => {
            let func_node = node.named_child(0).unwrap();
            let func_type = check_node(&func_node, source, file, symbols, diagnostics);

            // Named argument validation
            let total_children = node.named_child_count();
            let params_provided = total_children.saturating_sub(1);

            // Check: no positional args after named args, no duplicate names
            let mut seen_named = false;
            let mut named_names: Vec<String> = Vec::new();
            for i in 1..total_children {
                let arg = node.named_child(i).unwrap();
                if arg.kind() == "named_argument" {
                    seen_named = true;
                    if let Some(name_node) = arg.child_by_field_name("name") {
                        let name = name_node.utf8_text(source.as_bytes()).unwrap_or("").to_string();
                        if named_names.contains(&name) {
                            diagnostics.push(Diagnostic {
                                code: "TYP_031".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,&arg),
                                message: format!("Duplicate named argument: {}", name),
                                context: "Each named argument may only appear once.".to_string(),
                                suggestions: vec![],
                            });
                        }
                        named_names.push(name);
                    }
                } else if seen_named {
                    diagnostics.push(Diagnostic {
                        code: "TYP_030".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&arg),
                        message: "Positional argument after named argument".to_string(),
                        context: "All positional arguments must come before named arguments.".to_string(),
                        suggestions: vec![],
                    });
                }
            }

            // Resolve named args: look up function param names, validate names,
            // and build a mapping from call arg index -> parameter position.
            let fn_name = extract_qualified_name(&func_node, source).or_else(|| {
                let k = func_node.kind();
                if k == "identifier" || k == "effect_identifier" {
                    func_node.utf8_text(source.as_bytes()).ok().map(|s| s.to_string())
                } else {
                    None
                }
            });
            let fn_param_names = fn_name
                .as_ref()
                .and_then(|n| symbols.lookup_fn_params(n))
                .cloned();

            // arg_position_map[i] = which parameter position arg i fills
            // Default: identity mapping (positional)
            let mut arg_position_map: Vec<usize> = (0..params_provided).collect();
            if !named_names.is_empty() {
                if let Some(ref param_names) = fn_param_names {
                    // Check for unknown named args
                    for named in &named_names {
                        if !param_names.contains(named) {
                            // Find closest match for suggestion
                            let suggestions: Vec<String> = param_names.iter()
                                .filter(|p| !named_names.contains(p))
                                .cloned()
                                .collect();
                            let suggestion_text = if suggestions.is_empty() {
                                String::new()
                            } else {
                                format!(" Did you mean: {}?", suggestions.join(", "))
                            };
                            diagnostics.push(Diagnostic {
                                code: "TYP_032".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file, node),
                                message: format!("Unknown named argument: {}.{}", named, suggestion_text),
                                context: format!(
                                    "Available parameters: {}",
                                    param_names.join(", ")
                                ),
                                suggestions: vec![],
                            });
                        }
                    }

                    // Build position mapping: positional args stay in order,
                    // named args map to their parameter position
                    let mut positional_idx = 0;
                    for i in 0..params_provided {
                        let arg = node.named_child(i + 1).unwrap();
                        if arg.kind() == "named_argument" {
                            if let Some(name_node) = arg.child_by_field_name("name") {
                                let name = name_node.utf8_text(source.as_bytes()).unwrap_or("");
                                if let Some(pos) = param_names.iter().position(|p| p == name) {
                                    arg_position_map[i] = pos;
                                }
                            }
                        } else {
                            arg_position_map[i] = positional_idx;
                            positional_idx += 1;
                        }
                    }
                }
            }

            // STY_001: suggest pipe for nested single-arg calls like f(g(x))
            if params_provided == 1 {
                let single_arg = node.named_child(1).unwrap();
                if single_arg.kind() == "call_expression" {
                    diagnostics.push(Diagnostic {
                        code: "STY_001".to_string(),
                        severity: Severity::Warning,
                        location: Location::from_node(file,node),
                        message: "Nested call could use pipe syntax".to_string(),
                        context: "Consider rewriting f(g(x)) as x |> g |> f for readability."
                            .to_string(),
                        suggestions: vec![Suggestion {
                            strategy: "rewrite".to_string(),
                            description: "Use pipe operator |> instead of nested calls".to_string(),
                            confidence: None,
                            patch: None,
                        }],
                    });
                }
            }

            // Row.decode(row, TypeName) — automatic row-to-record mapping
            if let Some(qualified) = extract_qualified_name(&func_node, source) {
                if qualified == "Row.decode" {
                    if params_provided != 2 {
                        diagnostics.push(Diagnostic {
                            code: "TYP_040".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, node),
                            message: "Row.decode expects 2 arguments: (row, TypeName)".to_string(),
                            context: "Usage: Row.decode(row, User)".to_string(),
                            suggestions: vec![],
                        });
                        return Type::Unknown;
                    }
                    // Type-check first arg: must be Row
                    let row_arg = node.named_child(1).unwrap();
                    let row_arg_expr = call_arg_expr(&row_arg);
                    let row_type = check_node(&row_arg_expr, source, file, symbols, diagnostics);
                    if row_type != Type::Row && row_type != Type::Unknown {
                        diagnostics.push(Diagnostic {
                            code: "TYP_040".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, &row_arg_expr),
                            message: format!("Row.decode: first argument must be Row, found {}", row_type),
                            context: "Pass a Row value from a database query.".to_string(),
                            suggestions: vec![],
                        });
                    }
                    // Second arg: type_identifier → resolve struct
                    let type_arg = node.named_child(2).unwrap();
                    let type_name = type_arg.utf8_text(source.as_bytes()).unwrap();
                    if let Some(Type::Struct(name, fields)) = symbols.lookup_type(type_name).cloned() {
                        // Validate all fields are decode-compatible
                        for (fname, ftype) in &fields {
                            if !is_decode_compatible(ftype) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_042".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &type_arg),
                                    message: format!(
                                        "Row.decode: field '{}' has type {} which cannot be decoded from SQL",
                                        fname, ftype
                                    ),
                                    context: "Decodable types: Int, Float, String, Bool, Option<Int>, Option<String>".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        }
                        // Store struct type in TypeMap for the lowerer
                        symbols.type_map.insert(
                            node.start_byte(),
                            Type::Struct(name.clone(), fields.clone()),
                        );
                        return Type::Struct(name, fields);
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_041".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, &type_arg),
                            message: format!("Row.decode: '{}' is not a known struct type", type_name),
                            context: "The second argument must be a struct type name.".to_string(),
                            suggestions: vec![],
                        });
                        return Type::Unknown;
                    }
                }

                // Sqlite.query_as! / Postgres.query_as! / Mysql.query_as!
                // Sqlite.query_one_as! / Postgres.query_one_as! / Mysql.query_one_as!
                let is_query_as = matches!(
                    qualified.as_str(),
                    "Sqlite.query_as!" | "Postgres.query_as!" | "Mysql.query_as!"
                );
                let is_query_one_as = matches!(
                    qualified.as_str(),
                    "Sqlite.query_one_as!" | "Postgres.query_one_as!" | "Mysql.query_one_as!"
                );
                if is_query_as || is_query_one_as {
                    if params_provided != 3 {
                        let fn_name = if is_query_as { "query_as!" } else { "query_one_as!" };
                        diagnostics.push(Diagnostic {
                            code: "TYP_007".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, node),
                            message: format!(
                                "{} expects 3 arguments: (TypeName, sql, params)",
                                fn_name
                            ),
                            context: format!("Usage: {}(User, \"SELECT ...\", [])", fn_name),
                            suggestions: vec![],
                        });
                        return Type::Unknown;
                    }
                    // First arg: type_identifier → resolve struct
                    let type_arg = node.named_child(1).unwrap();
                    let type_name = type_arg.utf8_text(source.as_bytes()).unwrap();
                    let struct_type = symbols.lookup_type(type_name).cloned();
                    if let Some(Type::Struct(name, fields)) = struct_type {
                        // Validate fields
                        for (fname, ftype) in &fields {
                            if !is_decode_compatible(ftype) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_042".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &type_arg),
                                    message: format!(
                                        "{}: field '{}' has type {} which cannot be decoded from SQL",
                                        qualified, fname, ftype
                                    ),
                                    context: "Decodable types: Int, Float, String, Bool, Option<Int>, Option<String>".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        }
                        // Type-check sql and params args
                        let sql_arg = node.named_child(2).unwrap();
                        let sql_expr = call_arg_expr(&sql_arg);
                        let sql_type = check_node(&sql_expr, source, file, symbols, diagnostics);
                        if sql_type != Type::String && sql_type != Type::Unknown {
                            diagnostics.push(Diagnostic {
                                code: "TYP_008".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file, &sql_expr),
                                message: format!("Expected String for sql argument, found {}", sql_type),
                                context: "SQL query must be a String.".to_string(),
                                suggestions: vec![],
                            });
                        }
                        let params_arg = node.named_child(3).unwrap();
                        let params_expr = call_arg_expr(&params_arg);
                        let _params_type = check_node(&params_expr, source, file, symbols, diagnostics);

                        // Store struct type in TypeMap for the lowerer
                        symbols.type_map.insert(
                            node.start_byte(),
                            Type::Struct(name.clone(), fields.clone()),
                        );

                        if is_query_as {
                            return Type::List(Box::new(Type::Struct(name, fields)));
                        } else {
                            return Type::Enum(
                                "Option".to_string(),
                                vec![
                                    ("Some".to_string(), vec![Type::Struct(name, fields)]),
                                    ("None".to_string(), vec![]),
                                ],
                            );
                        }
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_041".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, &type_arg),
                            message: format!("{}: '{}' is not a known struct type", qualified, type_name),
                            context: "The first argument must be a struct type name.".to_string(),
                            suggestions: vec![],
                        });
                        return Type::Unknown;
                    }
                }
            }

            // Trait method call: TraitName.method(value, args...)
            if let Some(qualified) = extract_qualified_name(&func_node, source) {
                let parts: Vec<&str> = qualified.splitn(2, '.').collect();
                if parts.len() == 2 {
                    let maybe_trait = parts[0];
                    let method = parts[1];
                    let is_trait = symbols.lookup_trait(maybe_trait).is_some();
                    if is_trait && params_provided >= 1 {
                        // Infer type of first argument to determine which impl to use
                        let first_arg_node = node.named_child(1).unwrap();
                        let first_arg_expr = call_arg_expr(&first_arg_node);
                        let first_arg_type = check_node(&first_arg_expr, source, file, symbols, diagnostics);

                        // Type-check remaining args
                        for i in 2..=params_provided {
                            if let Some(arg_node) = node.named_child(i) {
                                let arg_expr = call_arg_expr(&arg_node);
                                check_node(&arg_expr, source, file, symbols, diagnostics);
                            }
                        }

                        // If first arg is a TypeParam, check trait bounds (dictionary passing)
                        if let Type::TypeParam(ref param_name) = first_arg_type {
                            let has_bound = symbols.current_bounds
                                .get(param_name)
                                .map_or(false, |bs| bs.contains(&maybe_trait.to_string()));
                            if has_bound {
                                // Store a dict marker so the lowerer uses the hidden parameter
                                let dict_marker = format!("__dict${}${}${}", maybe_trait, method, param_name);
                                symbols.type_map.insert(
                                    func_node.start_byte(),
                                    Type::Module(dict_marker),
                                );
                                // Return type: look up trait method sig, substitute Self -> TypeParam
                                let trait_methods = symbols.lookup_trait(maybe_trait)
                                    .map(|td| td.methods.clone())
                                    .unwrap_or_default();
                                let ret_type = trait_methods.iter()
                                    .find(|(n, _)| n == method)
                                    .map(|(_, ty)| {
                                        substitute_self(ty, &first_arg_type)
                                    })
                                    .and_then(|ty| {
                                        if let Type::Function(_, ret) = ty { Some(*ret) } else { None }
                                    })
                                    .unwrap_or(Type::Unknown);
                                return ret_type;
                            }
                            // Fall through to TRT_004 if no bound
                        }

                        let type_key = SymbolTable::type_key(&first_arg_type);

                        if let Some(trait_impl) = symbols.lookup_trait_impl(maybe_trait, &type_key) {
                            if let Some(mangled) = trait_impl.methods.get(method) {
                                let mangled_clone = mangled.clone();
                                // Store mangled name in type_map for the call site
                                // so the lowerer can resolve it
                                symbols.type_map.insert(
                                    func_node.start_byte(),
                                    Type::Module(mangled_clone.clone()),
                                );

                                // Get return type from the mangled function
                                let ret_type = if let Some(Type::Function(_, ret)) = symbols.lookup(&mangled_clone) {
                                    *ret.clone()
                                } else {
                                    Type::Unknown
                                };
                                return ret_type;
                            } else {
                                diagnostics.push(Diagnostic {
                                    code: "TRT_004".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &func_node),
                                    message: format!(
                                        "No method `{}` in impl of `{}` for `{}`",
                                        method, maybe_trait, type_key
                                    ),
                                    context: "Trait method not found in implementation.".to_string(),
                                    suggestions: vec![],
                                });
                                return Type::Unknown;
                            }
                        } else {
                            diagnostics.push(Diagnostic {
                                code: "TRT_004".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file, &func_node),
                                message: format!(
                                    "No impl of `{}` for `{}`",
                                    maybe_trait, type_key
                                ),
                                context: "Implement the trait for this type first.".to_string(),
                                suggestions: vec![],
                            });
                            return Type::Unknown;
                        }
                    }
                }
            }

            // Try generic schema inference for module methods (List.map) and constructors (Some, Ok)
            let schema_name = extract_qualified_name(&func_node, source).or_else(|| {
                let k = func_node.kind();
                if k == "identifier" || k == "type_identifier" {
                    func_node
                        .utf8_text(source.as_bytes())
                        .ok()
                        .map(|s| s.to_string())
                } else {
                    None
                }
            });
            // Try builtin generic schema first, then user-defined generic schema
            let builtin_schema = schema_name
                .as_ref()
                .and_then(|qn| symbols.lookup_generic_schema(qn));
            let user_schema = if builtin_schema.is_none() {
                schema_name
                    .as_ref()
                    .and_then(|qn| symbols.lookup_user_generic_schema(qn))
            } else {
                None
            };

            if builtin_schema.is_some() || user_schema.is_some() {
                // Capture bounds info before borrowing for instantiation
                let schema_bounds = user_schema.map(|us| {
                    (us.bounds.clone(), us.type_param_names.clone())
                });

                let mut ctx = InferCtx::new();
                let (instantiated, param_vars) = if let Some(schema) = builtin_schema {
                    ((schema.build)(&mut ctx), Vec::new())
                } else {
                    let us = user_schema.unwrap();
                    // Build mapping from param names to fresh vars (for bound checking)
                    let mut mapping = std::collections::HashMap::new();
                    let mut pvars = Vec::new();
                    for name in &us.type_param_names {
                        let v = ctx.fresh_var();
                        pvars.push((name.clone(), v.clone()));
                        mapping.insert(name.clone(), v);
                    }
                    let instantiated = crate::analysis::infer::substitute_type_params(&us.fn_type, &mapping);
                    (instantiated, pvars)
                };
                if let Type::Function(schema_params, schema_ret) = instantiated {
                    // Check arg count
                    if params_provided != schema_params.len() {
                        diagnostics.push(Diagnostic {
                            code: "TYP_007".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,node),
                            message: format!(
                                "Function call expects {} arguments, found {}",
                                schema_params.len(),
                                params_provided
                            ),
                            context: "Argument count mismatch.".to_string(),
                            suggestions: vec![],
                        });
                    }

                    // Unify each arg with the schema param, using lambda inference for HOFs
                    for (i, schema_param) in schema_params
                        .iter()
                        .enumerate()
                        .take(std::cmp::min(params_provided, schema_params.len()))
                    {
                        let raw_arg = node.named_child(i + 1).unwrap();
                        let arg_expr = call_arg_expr(&raw_arg);

                        // For lambda args with a Function-typed schema param, use
                        // check_lambda_with_expected with the resolved param types
                        let arg_type = if arg_expr.kind() == "lambda" {
                            let resolved_param = ctx.apply(schema_param);
                            if let Type::Function(ref expected_params, ref expected_ret) =
                                resolved_param
                            {
                                check_lambda_with_expected(
                                    &arg_expr,
                                    expected_params,
                                    expected_ret,
                                    source,
                                    file,
                                    symbols,
                                    diagnostics,
                                )
                            } else {
                                check_node(&arg_expr, source, file, symbols, diagnostics)
                            }
                        } else {
                            check_node(&arg_expr, source, file, symbols, diagnostics)
                        };

                        let _ = ctx.unify(&arg_type, schema_param);
                    }

                    // After unification: validate trait bounds and populate dict_map
                    if let Some((bounds, _param_names)) = schema_bounds {
                        if !bounds.is_empty() {
                            let mut dict_entries = Vec::new();
                            for (param_name, var) in &param_vars {
                                let resolved_type = ctx.apply(var);
                                if let Some(bound_traits) = bounds.get(param_name) {
                                    let resolved_key = SymbolTable::type_key(&resolved_type);
                                    for trait_name in bound_traits {
                                        if let Some(trait_impl) = symbols.lookup_trait_impl(trait_name, &resolved_key) {
                                            // Concrete type: pass the mangled impl functions
                                            let methods: Vec<(String, String)> = trait_impl.methods.iter()
                                                .map(|(k, v)| (k.clone(), v.clone()))
                                                .collect();
                                            dict_entries.push(super::symbol_table::DictEntry {
                                                trait_name: trait_name.clone(),
                                                methods,
                                            });
                                        } else if let Type::TypeParam(ref tp_name) = resolved_type {
                                            // TypeParam: check if caller has the same bound
                                            let caller_has_bound = symbols.current_bounds
                                                .get(tp_name)
                                                .map_or(false, |bs| bs.contains(trait_name));
                                            if caller_has_bound {
                                                // Forward caller's hidden params as dict entries.
                                                // Use hidden param names like "__Show_show".
                                                let trait_methods = symbols.lookup_trait(trait_name)
                                                    .map(|td| td.methods.clone())
                                                    .unwrap_or_default();
                                                let methods: Vec<(String, String)> = trait_methods.iter()
                                                    .map(|(mn, _)| (mn.clone(), format!("__{}_{}", trait_name, mn)))
                                                    .collect();
                                                dict_entries.push(super::symbol_table::DictEntry {
                                                    trait_name: trait_name.clone(),
                                                    methods,
                                                });
                                            } else {
                                                diagnostics.push(Diagnostic {
                                                    code: "TRT_011".to_string(),
                                                    severity: Severity::Error,
                                                    location: Location::from_node(file, node),
                                                    message: format!(
                                                        "Type `{}` does not implement `{}` required by bound on `{}`",
                                                        resolved_type, trait_name, param_name
                                                    ),
                                                    context: "Trait bound not satisfied.".to_string(),
                                                    suggestions: vec![],
                                                });
                                            }
                                        } else if resolved_type != Type::Unknown {
                                            diagnostics.push(Diagnostic {
                                                code: "TRT_011".to_string(),
                                                severity: Severity::Error,
                                                location: Location::from_node(file, node),
                                                message: format!(
                                                    "Type `{}` does not implement `{}` required by bound on `{}`",
                                                    resolved_type, trait_name, param_name
                                                ),
                                                context: "Trait bound not satisfied.".to_string(),
                                                suggestions: vec![],
                                            });
                                        }
                                    }
                                }
                            }
                            if !dict_entries.is_empty() {
                                symbols.dict_map.insert(node.start_byte(), dict_entries);
                            }
                        }
                    }

                    return ctx.apply(&schema_ret);
                }
            }

            if let Type::Function(arg_types, ret_type) = func_type {
                if params_provided != arg_types.len() {
                    // Allow partial application in pipe contexts
                    let in_pipe = node
                        .parent()
                        .map(|p| p.kind() == "pipe_expression")
                        .unwrap_or(false);
                    if params_provided < arg_types.len() && in_pipe {
                        // Pipe partial application: pipe inserts at position 0,
                        // so provided args fill positions 1..N.
                        // Type-check provided args against offset positions.
                        let offset = arg_types.len() - params_provided;
                        for (i, expected_arg_type) in arg_types
                            .iter()
                            .skip(offset)
                            .enumerate()
                        {
                            let raw_arg = node.named_child(i + 1).unwrap();
                            let arg_expr = call_arg_expr(&raw_arg);

                            let arg_type = if arg_expr.kind() == "lambda" {
                                if let Type::Function(ref expected_params, ref expected_ret) =
                                    *expected_arg_type
                                {
                                    check_lambda_with_expected(
                                        &arg_expr,
                                        expected_params,
                                        expected_ret,
                                        source,
                                        file,
                                        symbols,
                                        diagnostics,
                                    )
                                } else {
                                    check_node(&arg_expr, source, file, symbols, diagnostics)
                                }
                            } else {
                                check_node(&arg_expr, source, file, symbols, diagnostics)
                            };

                            if !types_compatible(&arg_type, expected_arg_type) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_008".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &arg_expr),
                                    message: format!(
                                        "Argument {} mismatch: expected {}, found {}",
                                        i + offset + 1,
                                        expected_arg_type,
                                        arg_type
                                    ),
                                    context: "Argument type must match function signature.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        }
                        let remaining = arg_types[..offset].to_vec();
                        return Type::Function(remaining, ret_type);
                    }

                    // Allow builtins with optional extra args (e.g. Log.info! accepts
                    // an optional record for structured fields).
                    let is_variadic_builtin = fn_name.as_ref().map_or(false, |n| {
                        matches!(n.as_str(),
                            "Log.info!" | "Log.warn!" | "Log.error!" | "Log.debug!"
                            | "Log.info" | "Log.warn" | "Log.error" | "Log.debug"
                        )
                    });
                    if is_variadic_builtin && params_provided > arg_types.len() {
                        // Type-check the declared args, ignore extras (handled at runtime)
                        for (i, expected) in arg_types.iter().enumerate() {
                            let raw_arg = node.named_child(i + 1).unwrap();
                            let arg_expr = call_arg_expr(&raw_arg);
                            let arg_type = check_node(&arg_expr, source, file, symbols, diagnostics);
                            if !types_compatible(&arg_type, expected) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_008".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &arg_expr),
                                    message: format!(
                                        "Argument {} mismatch: expected {}, found {}",
                                        i + 1, expected, arg_type
                                    ),
                                    context: "Argument type must match function signature.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        }
                        // Type-check extra args without type constraints
                        for i in arg_types.len()..params_provided {
                            let raw_arg = node.named_child(i + 1).unwrap();
                            let arg_expr = call_arg_expr(&raw_arg);
                            check_node(&arg_expr, source, file, symbols, diagnostics);
                        }
                        return *ret_type;
                    }

                    diagnostics.push(Diagnostic {
                        code: "TYP_007".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,node),
                        message: format!(
                            "Function call expects {} arguments, found {}",
                            arg_types.len(),
                            params_provided
                        ),
                        context: "Argument count mismatch.".to_string(),
                        suggestions: vec![],
                    });
                }

                for i in 0..std::cmp::min(params_provided, arg_types.len()) {
                    let raw_arg = node.named_child(i + 1).unwrap();
                    let arg_expr = call_arg_expr(&raw_arg);

                    // Resolve which parameter position this arg fills
                    let param_pos = arg_position_map.get(i).copied().unwrap_or(i);
                    let expected_arg_type = arg_types
                        .get(param_pos)
                        .unwrap_or(arg_types.get(i).unwrap_or(&Type::Unknown));

                    // Lambda argument inference: if expected type is Function and arg is
                    // a lambda, check the lambda body with expected param types injected.
                    let arg_type = if arg_expr.kind() == "lambda" {
                        if let Type::Function(ref expected_params, ref expected_ret) =
                            *expected_arg_type
                        {
                            check_lambda_with_expected(
                                &arg_expr,
                                expected_params,
                                expected_ret,
                                source,
                                file,
                                symbols,
                                diagnostics,
                            )
                        } else {
                            check_node(&arg_expr, source, file, symbols, diagnostics)
                        }
                    } else {
                        check_node(&arg_expr, source, file, symbols, diagnostics)
                    };

                    if !types_compatible(&arg_type, expected_arg_type) {
                        diagnostics.push(Diagnostic {
                            code: "TYP_008".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&arg_expr),
                            message: format!(
                                "Argument {} mismatch: expected {}, found {}",
                                param_pos + 1,
                                expected_arg_type,
                                arg_type
                            ),
                            context: "Argument type must match function signature.".to_string(),
                            suggestions: vec![],
                        });
                    }
                }
                *ret_type
            } else if func_type == Type::Unknown {
                Type::Unknown
            } else {
                diagnostics.push(Diagnostic {
                    code: "TYP_009".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&func_node),
                    message: format!("Called expression is not a function, it is {}", func_type),
                    context: "Only functions can be called.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "with_expression" => {
            // with Console.println! { body } -> (T, List<String>)
            // Effect field is child 0, body block is child 1
            let body_node = node
                .child_by_field_name("body")
                .unwrap_or_else(|| node.named_child(1).unwrap());
            let body_type = check_node(&body_node, source, file, symbols, diagnostics);
            Type::Tuple(vec![body_type, Type::List(Box::new(Type::String))])
        }
        "restrict_expression" => {
            // restrict(Effect1, Effect2) { body } — type is the body's type.
            // Effect narrowing is enforced by the effect checker, not the type checker.
            if let Some(body_node) = node.child_by_field_name("body") {
                check_node(&body_node, source, file, symbols, diagnostics)
            } else {
                Type::Unknown
            }
        }
        "struct_expression" => {
            let type_name_node = node.named_child(0).unwrap();
            let type_name = type_name_node.utf8_text(source.as_bytes()).unwrap();

            if let Some(Type::Struct(name, fields)) = symbols.lookup_type(type_name).cloned() {
                let mut initialized_fields = std::collections::HashSet::new();

                let count = node.named_child_count();
                for i in 1..count {
                    let field_init = node.named_child(i).unwrap();
                    let fname_node = field_init.named_child(0).unwrap();
                    let fname = match extract_field_name(&fname_node, source, file, diagnostics) {
                        Some(n) => n,
                        None => continue,
                    };

                    let fexpr_node = field_init.named_child(1).unwrap();
                    let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);

                    if let Some(expected_type) = fields.get(&fname) {
                        if !types_compatible(&ftype, expected_type) {
                            diagnostics.push(Diagnostic {
                                code: "TYP_010".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,&fexpr_node),
                                message: format!(
                                    "Field `{}` expects {}, found {}",
                                    fname, expected_type, ftype
                                ),
                                context: "Struct field type mismatch.".to_string(),
                                suggestions: vec![],
                            });
                        }
                        initialized_fields.insert(fname);
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_011".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&fname_node),
                            message: format!("Struct `{}` has no field `{}`", name, fname),
                            context: "Field not defined in struct.".to_string(),
                            suggestions: vec![],
                        });
                    }
                }

                for required_field in fields.keys() {
                    if !initialized_fields.contains(required_field) {
                        diagnostics.push(Diagnostic {
                            code: "TYP_012".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,node),
                            message: format!("Missing field `{}`", required_field),
                            context: "All struct fields must be initialized.".to_string(),
                            suggestions: vec![],
                        });
                    }
                }

                Type::Struct(name, fields)
            } else {
                diagnostics.push(Diagnostic {
                    code: "TYP_013".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&type_name_node),
                    message: format!("Unknown type `{}`", type_name),
                    context: "Type must be defined before use.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "record_expression" => {
            // { x: 1, y: 2 } — anonymous record literal
            let mut fields = std::collections::HashMap::new();
            let count = node.named_child_count();
            for i in 0..count {
                let field_init = node.named_child(i).unwrap();
                if field_init.kind() == "record_field_init" {
                    let fname_node = field_init.named_child(0).unwrap();
                    if let Some(fname) = extract_field_name(&fname_node, source, file, diagnostics) {
                        let fexpr_node = field_init.named_child(1).unwrap();
                        let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);
                        fields.insert(fname, ftype);
                    }
                }
            }
            Type::Record(fields, None)
        }
        "record_update" => {
            // { ..base, field: newValue }
            // named_child(0) = base expression, named_child(1..) = record_field_init overrides
            let base_node = node.named_child(0).unwrap();
            let base_type = check_node(&base_node, source, file, symbols, diagnostics);

            match base_type {
                Type::Struct(ref name, ref fields) => {
                    let count = node.named_child_count();
                    for i in 1..count {
                        let field_init = node.named_child(i).unwrap();
                        let fname_node = field_init.named_child(0).unwrap();
                        let fname = match extract_field_name(&fname_node, source, file, diagnostics) {
                            Some(n) => n,
                            None => continue,
                        };
                        let fexpr_node = field_init.named_child(1).unwrap();
                        let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);

                        if let Some(expected_type) = fields.get(&fname) {
                            if !types_compatible(&ftype, expected_type) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_028".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file,&fexpr_node),
                                    message: format!(
                                        "Field `{}` expects {}, found {}",
                                        fname, expected_type, ftype
                                    ),
                                    context: "Record update field type mismatch.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        } else {
                            diagnostics.push(Diagnostic {
                                code: "TYP_029".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,&fname_node),
                                message: format!("Struct `{}` has no field `{}`", name, fname),
                                context: "Field not defined in struct.".to_string(),
                                suggestions: vec![],
                            });
                        }
                    }
                    base_type.clone()
                }
                Type::Record(ref fields, _) => {
                    // Records are open: updates can override existing fields
                    // or extend the record with new fields.
                    let mut result_fields = fields.clone();
                    let count = node.named_child_count();
                    for i in 1..count {
                        let field_init = node.named_child(i).unwrap();
                        let fname_node = field_init.named_child(0).unwrap();
                        let fname = match extract_field_name(&fname_node, source, file, diagnostics) {
                            Some(n) => n,
                            None => continue,
                        };
                        let fexpr_node = field_init.named_child(1).unwrap();
                        let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);

                        if let Some(expected_type) = fields.get(&fname) {
                            if !types_compatible(&ftype, expected_type) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_028".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file,&fexpr_node),
                                    message: format!(
                                        "Field `{}` expects {}, found {}",
                                        fname, expected_type, ftype
                                    ),
                                    context: "Record update field type mismatch.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        }
                        // Insert or update the field in the result type
                        result_fields.insert(fname, ftype);
                    }
                    Type::Record(result_fields, None)
                }
                Type::Unknown => Type::Unknown,
                _ => {
                    diagnostics.push(Diagnostic {
                        code: "TYP_027".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&base_node),
                        message: format!(
                            "Record update requires a record or struct, found {}",
                            base_type
                        ),
                        context: "Only records and structs support update syntax.".to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown
                }
            }
        }
        "field_expression" => {
            let obj_node = node.named_child(0).unwrap();
            let field_node = node.named_child(1).unwrap();
            let field_name = field_node.utf8_text(source.as_bytes()).unwrap();

            let obj_type = check_node(&obj_node, source, file, symbols, diagnostics);

            match obj_type {
                Type::Struct(name, fields) => {
                    if let Some(ty) = fields.get(field_name) {
                        ty.clone()
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_014".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&field_node),
                            message: format!("Struct `{}` has no field `{}`", name, field_name),
                            context: "Field access error.".to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    }
                }
                Type::Record(ref fields, _) => {
                    if let Some(ty) = fields.get(field_name) {
                        ty.clone()
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_014".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&field_node),
                            message: format!("Record has no field `{}`", field_name),
                            context: "Field access error.".to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    }
                }
                Type::Module(ref module_name) => {
                    let qualified = format!("{}.{}", module_name, field_name);
                    if let Some(ty) = symbols.lookup_module_method(&qualified) {
                        ty.clone()
                    } else if symbols.is_user_module(module_name) {
                        diagnostics.push(Diagnostic {
                            code: "IMP_004".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, &field_node),
                            message: format!(
                                "Symbol `{}` not found in module `{}`",
                                field_name, module_name
                            ),
                            context: "The symbol may be private (not exported) or does not exist.".to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    } else {
                        // Builtin module — allow unknown methods for forward compat
                        Type::Unknown
                    }
                }
                Type::Enum(ref enum_name, ref variants) => {
                    // Auto-derived methods for simple enums (all variants nullary)
                    let all_nullary = variants.iter().all(|(_, payload)| payload.is_empty());
                    if all_nullary {
                        match field_name {
                            "to_string" => {
                                Type::Function(
                                    vec![obj_type.clone()],
                                    Box::new(Type::String),
                                )
                            }
                            "parse" => {
                                let result_type = Type::Enum(
                                    "Result".to_string(),
                                    vec![
                                        ("Ok".to_string(), vec![obj_type.clone()]),
                                        ("Err".to_string(), vec![Type::String]),
                                    ],
                                );
                                Type::Function(
                                    vec![Type::String],
                                    Box::new(result_type),
                                )
                            }
                            _ => {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_015".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &field_node),
                                    message: format!(
                                        "Enum `{}` has no method `{}`",
                                        enum_name, field_name
                                    ),
                                    context: "Simple enums support to_string and parse.".to_string(),
                                    suggestions: vec![],
                                });
                                Type::Unknown
                            }
                        }
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_015".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, &field_node),
                            message: format!(
                                "Enum `{}` has payload variants; auto-derived methods require all variants to be nullary",
                                enum_name
                            ),
                            context: "Only simple enums (no payloads) support to_string and parse.".to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    }
                }
                Type::Unknown => Type::Unknown,
                _ => {
                    diagnostics.push(Diagnostic {
                        code: "TYP_015".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&obj_node),
                        message: format!("Type {} excludes field access", obj_type),
                        context: "Only Structs, Records, and Modules support field access."
                            .to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown
                }
            }
        }
        "block" => {
            symbols.enter_scope();
            let mut last_type = Type::Unit;
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "let_binding" {
                    check_node(&child, source, file, symbols, diagnostics);
                    last_type = Type::Unit;
                } else if child.kind().ends_with("_expression")
                    || child.kind() == "identifier"
                    || child.kind().ends_with("_literal")
                    || child.kind() == "literal"
                {
                    last_type = check_node(&child, source, file, symbols, diagnostics);
                }
            }
            symbols.exit_scope();
            last_type
        }
        "identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap();
            if let Some(ty) = symbols.lookup(name) {
                ty.clone()
            } else {
                let known_names: Vec<String> = symbols
                    .visible_bindings()
                    .iter()
                    .map(|(n, _)| n.clone())
                    .collect();
                let suggestions =
                    crate::diagnostic_render::suggest_similar_identifier(name, &known_names);
                diagnostics.push(Diagnostic {
                    code: "TYP_002".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,node),
                    message: format!("Undefined variable `{}`", name),
                    context: "Variable must be defined before use.".to_string(),
                    suggestions,
                });
                Type::Unknown
            }
        }
        "effect_identifier" | "effectful_identifier" => {
            // TODO: Lookup in a real Effect Registry for return types.
            // For now, assume most effects return Unit or Unknown to allow flow.
            Type::Unknown
        }
        "named_argument" => {
            // Named argument: name: expression — type-check the expression
            let count = node.named_child_count();
            if count > 0 {
                check_node(&node.named_child(count - 1).unwrap(), source, file, symbols, diagnostics)
            } else {
                Type::Unknown
            }
        }
        "hole_expression" => {
            // Typed hole: ?? — report expected type and available bindings
            let expected = infer_expected_type(node, source, symbols);
            let bindings = symbols.visible_bindings();

            let mut msg = format!("Typed hole (??) — expected type: {}", expected);
            if !bindings.is_empty() {
                msg.push_str("\n  Available bindings:");
                for (name, ty) in &bindings {
                    msg.push_str(&format!("\n    {}: {}", name, ty));
                }
            }

            diagnostics.push(Diagnostic {
                code: "HOLE_001".to_string(),
                severity: Severity::Warning,
                location: Location::from_node(file,node),
                message: msg,
                context: "Replace ?? with an expression of the expected type.".to_string(),
                suggestions: vec![],
            });

            expected
        }
        "literal" | "parenthesized_expression" => {
            if let Some(child) = node.named_child(0) {
                check_node(&child, source, file, symbols, diagnostics)
            } else {
                Type::Unit
            }
        }
        "integer_literal" => Type::Int,
        "float_literal" => Type::Float,
        "string_literal" | "multiline_string_literal" => {
            // Recurse into interpolation children to type-check embedded expressions
            let named_count = node.named_child_count();
            for i in 0..named_count {
                let child = node.named_child(i).unwrap();
                if child.kind() == "interpolation"
                    && let Some(expr) = child.named_child(0)
                {
                    check_node(&expr, source, file, symbols, diagnostics);
                }
            }
            Type::String
        }
        "raw_string_literal" | "raw_hash_string_literal" => Type::String,
        "boolean_literal" => Type::Bool,
        "binary_expression" => {
            let left_type = check_node(
                &node.named_child(0).unwrap(),
                source,
                file,
                symbols,
                diagnostics,
            );
            let right_type = check_node(
                &node.named_child(1).unwrap(),
                source,
                file,
                symbols,
                diagnostics,
            );
            let op_str = node.child(1).unwrap().utf8_text(source.as_bytes()).unwrap();

            match op_str {
                "+" | "-" | "*" | "/" | "%" => {
                    if left_type == Type::Int && right_type == Type::Int {
                        Type::Int
                    } else if left_type == Type::Float && right_type == Type::Float {
                        Type::Float
                    } else if (left_type == Type::Int && right_type == Type::Float)
                        || (left_type == Type::Float && right_type == Type::Int)
                    {
                        // Int/Float promotion: mixed arithmetic produces Float
                        Type::Float
                    } else {
                        if left_type != Type::Unknown && right_type != Type::Unknown {
                            diagnostics.push(Diagnostic {
                                code: "TYP_001".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file,node),
                                message: format!("Binary operator `{}` requires matching Int or Float operands, found {} and {}", op_str, left_type, right_type),
                                context: "Arithmetic operations require matching numeric types.".to_string(),
                                suggestions: vec![],
                            });
                        }
                        Type::Unknown
                    }
                }
                "++" => {
                    // List concatenation: List<T> ++ List<T> -> List<T>
                    match (&left_type, &right_type) {
                        (Type::List(inner_l), Type::List(inner_r)) => {
                            if types_compatible(inner_l, inner_r) {
                                left_type.clone()
                            } else if **inner_l == Type::Unknown {
                                right_type.clone()
                            } else {
                                Type::List(inner_l.clone())
                            }
                        }
                        (Type::List(_), Type::Unknown) | (Type::Unknown, Type::List(_)) | (Type::Unknown, Type::Unknown) => {
                            if matches!(left_type, Type::List(_)) { left_type.clone() } else { right_type.clone() }
                        }
                        _ => {
                            if left_type != Type::Unknown && right_type != Type::Unknown {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_001".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file,node),
                                    message: format!("Operator `++` requires List operands, found {} and {}", left_type, right_type),
                                    context: "The ++ operator concatenates two lists of the same type.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                            Type::Unknown
                        }
                    }
                }
                "==" | "!=" | "<" | ">" | "<=" | ">=" => Type::Bool,
                _ => Type::Unknown,
            }
        }
        "if_expression" => {
            let cond = node.named_child(0).unwrap();
            let cond_type = check_node(&cond, source, file, symbols, diagnostics);

            if cond_type != Type::Bool && cond_type != Type::Unknown {
                diagnostics.push(Diagnostic {
                    code: "TYP_003".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&cond),
                    message: format!("If condition must be Boolean, found {}", cond_type),
                    context: "Control flow conditions must evaluate to true or false.".to_string(),
                    suggestions: vec![],
                });
            }

            let then_branch = node.named_child(1).unwrap();
            let then_type = check_node(&then_branch, source, file, symbols, diagnostics);

            if let Some(else_branch) = node.named_child(2) {
                let else_type = check_node(&else_branch, source, file, symbols, diagnostics);

                if !types_compatible(&then_type, &else_type) {
                    let suggestions = crate::diagnostic_render::suggest_type_coercion(
                        &format!("{}", then_type),
                        &format!("{}", else_type),
                    );
                    diagnostics.push(Diagnostic {
                        code: "TYP_004".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&else_branch),
                        message: format!(
                            "If branches match mismatch: then is {}, else is {}",
                            then_type, else_type
                        ),
                        context: "Both branches of an if expression must return the same type."
                            .to_string(),
                        suggestions,
                    });
                    Type::Unknown
                } else {
                    then_type
                }
            } else {
                Type::Unit
            }
        }
        "tuple_expression" => {
            let count = node.named_child_count();
            if count == 0 {
                Type::Unit
            } else {
                let elems: Vec<Type> = (0..count)
                    .map(|i| {
                        check_node(
                            &node.named_child(i).unwrap(),
                            source,
                            file,
                            symbols,
                            diagnostics,
                        )
                    })
                    .collect();
                if elems.len() == 1 {
                    // (expr) is parenthesized, not a 1-tuple
                    elems.into_iter().next().unwrap()
                } else {
                    Type::Tuple(elems)
                }
            }
        }
        "list_expression" => {
            // [1, 2, 3]
            let mut element_type = Type::Unknown;
            let mut first = true;

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "[" || child.kind() == "]" || child.kind() == "," {
                    continue;
                }

                let ty = check_node(&child, source, file, symbols, diagnostics);

                if first {
                    element_type = ty;
                    first = false;
                } else if !types_compatible(&ty, &element_type) {
                    diagnostics.push(Diagnostic {
                        code: "TYP_016".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&child),
                        message: format!(
                            "List element type mismatch: expected {}, found {}",
                            element_type, ty
                        ),
                        context: "All elements in a list must have the same type.".to_string(),
                        suggestions: vec![],
                    });
                }
            }

            Type::List(Box::new(element_type))
        }
        "lambda" => {
            // |x| body
            let mut arg_types = Vec::new();

            // Last named child is body, others are args
            let count = node.named_child_count();
            symbols.enter_scope();

            for i in 0..count - 1 {
                let arg = node.named_child(i).unwrap();
                if arg.kind() == "identifier" {
                    let name = arg.utf8_text(source.as_bytes()).unwrap().to_string();
                    symbols.insert(name, Type::Unknown);
                    arg_types.push(Type::Unknown);
                }
            }

            let body = node.named_child(count - 1).unwrap();
            let body_type = check_node(&body, source, file, symbols, diagnostics);

            symbols.exit_scope();

            Type::Function(arg_types, Box::new(body_type))
        }
        "match_expression" => {
            let expr_node = node.named_child(0).unwrap();
            let expr_type = check_node(&expr_node, source, file, symbols, diagnostics);

            let mut ret_type = Type::Unknown;
            let mut first = true;

            let count = node.named_child_count();
            for i in 1..count {
                let arm = node.named_child(i).unwrap();
                // Body is always the last child (after optional guard and ->)
                let body = arm.child(arm.child_count() - 1).unwrap();

                // Collect all pattern nodes (or-patterns have multiple separated by |)
                let mut arm_cursor2 = arm.walk();
                let pat_nodes: Vec<tree_sitter::Node> = arm.named_children(&mut arm_cursor2)
                    .filter(|c| c.kind() != "match_guard" && c.id() != body.id())
                    .collect();

                symbols.enter_scope();
                for pat in &pat_nodes {
                    check_pattern(pat, &expr_type, source, symbols, diagnostics);
                }

                // Type-check guard expression if present
                let mut arm_cursor = arm.walk();
                for arm_child in arm.children(&mut arm_cursor) {
                    if arm_child.kind() == "match_guard" {
                        if let Some(guard_expr) = arm_child.named_child(0) {
                            let guard_type = check_node(&guard_expr, source, file, symbols, diagnostics);
                            if guard_type != Type::Bool && guard_type != Type::Unknown {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_003".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &guard_expr),
                                    message: format!("Match guard must be Boolean, found {}", guard_type),
                                    context: "Guard expressions must evaluate to true or false.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        }
                    }
                }

                let body_type = check_node(&body, source, file, symbols, diagnostics);
                symbols.exit_scope();

                if first {
                    ret_type = body_type;
                    first = false;
                } else if !types_compatible(&body_type, &ret_type) {
                    diagnostics.push(Diagnostic {
                        code: "TYP_017".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file, &body),
                        message: format!(
                            "Match arm mismatch: expected {}, found {}",
                            ret_type, body_type
                        ),
                        context: "All match arms must return same type.".to_string(),
                        suggestions: vec![],
                    });
                }
            }
            check_match_exhaustiveness(node, &expr_type, source, file, diagnostics);
            ret_type
        }
        "pipe_expression" => {
            let left_node = node.named_child(0).unwrap();
            let right_node = node.named_child(1).unwrap();

            let left_type = check_node(&left_node, source, file, symbols, diagnostics);

            // When RHS is a call_expression like `List.filter(pred)`, treat the
            // piped value as the first argument: `items |> List.filter(pred)` becomes
            // `List.filter(items, pred)`.
            if right_node.kind() == "call_expression" {
                let func_node = right_node.named_child(0).unwrap();
                let explicit_arg_count = right_node.named_child_count() - 1;

                // Resolve function name for schema lookup
                let func_name = extract_qualified_name(&func_node, source).or_else(|| {
                    let k = func_node.kind();
                    if k == "identifier" || k == "effect_identifier" {
                        func_node.utf8_text(source.as_bytes()).ok().map(|s| s.to_string())
                    } else {
                        None
                    }
                });

                // Try generic schema first (covers List.map, List.filter, etc.)
                let schema = func_name.as_ref()
                    .and_then(|n| symbols.lookup_generic_schema(n));
                let user_schema = if schema.is_none() {
                    func_name.as_ref()
                        .and_then(|n| symbols.lookup_user_generic_schema(n))
                } else {
                    None
                };

                if schema.is_some() || user_schema.is_some() {
                    let mut ctx = InferCtx::new();
                    let instantiated = if let Some(s) = schema {
                        (s.build)(&mut ctx)
                    } else {
                        let us = user_schema.unwrap();
                        let mut mapping = std::collections::HashMap::new();
                        for name in &us.type_param_names {
                            mapping.insert(name.clone(), ctx.fresh_var());
                        }
                        crate::analysis::infer::substitute_type_params(&us.fn_type, &mapping)
                    };
                    if let Type::Function(params, ret) = instantiated {
                        let expected_total = params.len();
                        if explicit_arg_count + 1 != expected_total {
                            diagnostics.push(Diagnostic {
                                code: "TYP_007".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file, &right_node),
                                message: format!(
                                    "Pipe call expects {} arguments (including piped value), found {}",
                                    expected_total, explicit_arg_count + 1
                                ),
                                context: "Argument count mismatch in pipe call.".to_string(),
                                suggestions: vec![],
                            });
                            Type::Unknown
                        } else {
                            // Unify piped value with first param
                            let _ = ctx.unify(&left_type, &params[0]);
                            // Unify explicit args with remaining params
                            for i in 0..explicit_arg_count {
                                let arg_node = right_node.named_child(i + 1).unwrap();
                                let arg_expr = call_arg_expr(&arg_node);
                                // For lambda args, use expected type for inference
                                let expected = &params[i + 1];
                                let arg_type = if arg_expr.kind() == "lambda" {
                                    let expected_applied = ctx.apply(expected);
                                    if let Type::Function(ref exp_params, ref exp_ret) = expected_applied {
                                        check_lambda_with_expected(
                                            &arg_expr, exp_params, exp_ret,
                                            source, file, symbols, diagnostics,
                                        )
                                    } else {
                                        check_node(&arg_expr, source, file, symbols, diagnostics)
                                    }
                                } else {
                                    check_node(&arg_expr, source, file, symbols, diagnostics)
                                };
                                let _ = ctx.unify(&arg_type, expected);
                            }
                            ctx.apply(&ret)
                        }
                    } else {
                        Type::Unknown
                    }
                } else {
                    // Non-generic function: look up type directly
                    let func_type = check_node(&func_node, source, file, symbols, diagnostics);
                    if let Type::Function(params, ret) = func_type {
                        let expected_total = params.len();
                        if explicit_arg_count + 1 == expected_total {
                            // Validate piped value against first param
                            if !types_compatible(&left_type, &params[0]) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_019".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &left_node),
                                    message: format!(
                                        "Pipe argument mismatch: expected {}, found {}",
                                        params[0], left_type
                                    ),
                                    context: "Piped value type must match first parameter.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                            // Type-check explicit args
                            for i in 0..explicit_arg_count {
                                let arg_node = right_node.named_child(i + 1).unwrap();
                                let arg_expr = call_arg_expr(&arg_node);
                                let arg_type = check_node(&arg_expr, source, file, symbols, diagnostics);
                                if !types_compatible(&arg_type, &params[i + 1]) {
                                    diagnostics.push(Diagnostic {
                                        code: "TYP_019".to_string(),
                                        severity: Severity::Error,
                                        location: Location::from_node(file, &arg_expr),
                                        message: format!(
                                            "Argument mismatch: expected {}, found {}",
                                            params[i + 1], arg_type
                                        ),
                                        context: "Argument type must match function signature.".to_string(),
                                        suggestions: vec![],
                                    });
                                }
                            }
                            *ret
                        } else if explicit_arg_count == 0 && params.len() == 1 {
                            // `x |> f()` — same as `x |> f`
                            if !types_compatible(&left_type, &params[0]) {
                                diagnostics.push(Diagnostic {
                                    code: "TYP_019".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &left_node),
                                    message: format!(
                                        "Pipe argument mismatch: expected {}, found {}",
                                        params[0], left_type
                                    ),
                                    context: "Argument type must match function signature.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                            *ret
                        } else {
                            diagnostics.push(Diagnostic {
                                code: "TYP_007".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file, &right_node),
                                message: format!(
                                    "Pipe call expects {} arguments (including piped value), found {}",
                                    params.len(), explicit_arg_count + 1
                                ),
                                context: "Argument count mismatch in pipe call.".to_string(),
                                suggestions: vec![],
                            });
                            Type::Unknown
                        }
                    } else if func_type == Type::Unknown {
                        // Type-check explicit args for side effects
                        for i in 0..explicit_arg_count {
                            let arg_node = right_node.named_child(i + 1).unwrap();
                            let arg_expr = call_arg_expr(&arg_node);
                            check_node(&arg_expr, source, file, symbols, diagnostics);
                        }
                        Type::Unknown
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_020".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, &right_node),
                            message: format!("Pipe target is not a function, it is {}", func_type),
                            context: "Left side must be piped into a function.".to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    }
                }
            } else {
                // RHS is a bare function reference (no call): x |> f
                let right_type = check_node(&right_node, source, file, symbols, diagnostics);

                if let Type::Function(args, ret) = right_type {
                    if args.len() != 1 {
                        diagnostics.push(Diagnostic {
                            code: "TYP_018".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, &right_node),
                            message: format!("Pipe function expects 1 argument, found {}", args.len()),
                            context: "Bare pipe target must be a unary function. Use `|> f(arg)` for multi-arg functions.".to_string(),
                            suggestions: vec![],
                        });
                    } else if !types_compatible(&left_type, &args[0]) {
                        diagnostics.push(Diagnostic {
                            code: "TYP_019".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file, &left_node),
                            message: format!(
                                "Pipe argument mismatch: expected {}, found {}",
                                args[0], left_type
                            ),
                            context: "Argument type must match function signature.".to_string(),
                            suggestions: vec![],
                        });
                    }
                    *ret
                } else if right_type == Type::Unknown {
                    Type::Unknown
                } else {
                    diagnostics.push(Diagnostic {
                        code: "TYP_020".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file, &right_node),
                        message: format!("Pipe target is not a function, it is {}", right_type),
                        context: "Left side must be piped into a function.".to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown
                }
            }
        }
        "type_identifier" => {
            // In expression context, could be a constructor, module, or type reference
            let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
            // First check if it's a constructor in the value namespace
            if let Some(ty) = symbols.lookup(&name) {
                ty.clone()
            } else if let Some(ty) = symbols.lookup_type(&name) {
                ty.clone()
            } else {
                // Assume it might be a module not yet defined or external
                Type::Module(name)
            }
        }
        "for_expression" => {
            // for <pattern> in <collection> do <body>
            let pat_node = node.named_child(0).unwrap();
            let collection_node = node.named_child(1).unwrap();
            let body_node = node.named_child(2).unwrap();

            let collection_type = check_node(&collection_node, source, file, symbols, diagnostics);

            let element_type = match &collection_type {
                Type::List(inner) => *inner.clone(),
                Type::Unknown => Type::Unknown,
                _ => {
                    diagnostics.push(Diagnostic {
                        code: "TYP_023".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&collection_node),
                        message: format!("For loop requires a List, found {}", collection_type),
                        context: "The collection in a for..in must be a List.".to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown
                }
            };

            symbols.enter_scope();
            bind_pattern(&pat_node, element_type, source, symbols);
            let body_type = check_node(&body_node, source, file, symbols, diagnostics);
            symbols.exit_scope();

            // For loops are for side effects only. If the body returns a non-Unit type,
            // warn: use List.map for transformations (One Way Principle).
            if body_type != Type::Unit && body_type != Type::Unknown {
                diagnostics.push(Diagnostic {
                    code: "TYP_033".to_string(),
                    severity: Severity::Warning,
                    location: Location::from_node(file,&body_node),
                    message: format!(
                        "For loop body returns {}, but for loops are for side effects only",
                        body_type
                    ),
                    context: "Use List.map() to transform data, for loops to perform effects.".to_string(),
                    suggestions: vec![Suggestion {
                        strategy: "replace".to_string(),
                        description: "Use List.map() for transformations".to_string(),
                        confidence: None,
                        patch: None,
                    }],
                });
            }

            Type::Unit
        }
        "range_expression" => {
            // start..end — both must be Int, produces List<Int>
            let start_node = node.named_child(0).unwrap();
            let end_node = node.named_child(1).unwrap();

            let start_type = check_node(&start_node, source, file, symbols, diagnostics);
            let end_type = check_node(&end_node, source, file, symbols, diagnostics);

            if start_type != Type::Int && start_type != Type::Unknown {
                diagnostics.push(Diagnostic {
                    code: "TYP_024".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&start_node),
                    message: format!("Range operand must be Int, found {}", start_type),
                    context: "Range expressions require Int operands.".to_string(),
                    suggestions: vec![],
                });
            }
            if end_type != Type::Int && end_type != Type::Unknown {
                diagnostics.push(Diagnostic {
                    code: "TYP_024".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&end_node),
                    message: format!("Range operand must be Int, found {}", end_type),
                    context: "Range expressions require Int operands.".to_string(),
                    suggestions: vec![],
                });
            }

            Type::List(Box::new(Type::Int))
        }
        "unary_expression" => {
            // !expr or -expr
            let op = node.child(0).unwrap().utf8_text(source.as_bytes()).unwrap();
            let operand_node = node.named_child(0).unwrap();
            let operand_type = check_node(&operand_node, source, file, symbols, diagnostics);

            match op {
                "not" => {
                    if operand_type != Type::Bool && operand_type != Type::Unknown {
                        diagnostics.push(Diagnostic {
                            code: "TYP_025".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&operand_node),
                            message: format!("Logical NOT requires Bool, found {}", operand_type),
                            context: "The not operator can only be applied to Bool values."
                                .to_string(),
                            suggestions: vec![],
                        });
                    }
                    Type::Bool
                }
                "-" => {
                    if operand_type != Type::Int
                        && operand_type != Type::Float
                        && operand_type != Type::Unknown
                    {
                        diagnostics.push(Diagnostic {
                            code: "TYP_025".to_string(),
                            severity: Severity::Error,
                            location: Location::from_node(file,&operand_node),
                            message: format!(
                                "Negation requires Int or Float, found {}",
                                operand_type
                            ),
                            context: "The - operator can only be applied to numeric values."
                                .to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    } else {
                        operand_type
                    }
                }
                _ => Type::Unknown,
            }
        }
        _ => {
            Type::Unit // simplified
        }
    }
}

/// Find a function_def node at a given byte offset by walking the tree.
fn find_function_def_at_byte<'a>(root: &Node<'a>, byte: usize) -> Option<Node<'a>> {
    let mut cursor = root.walk();
    for child in root.named_children(&mut cursor) {
        if child.kind() == "trait_def" {
            let mut inner = child.walk();
            for tc in child.named_children(&mut inner) {
                if tc.kind() == "function_def" && tc.start_byte() == byte {
                    return Some(tc);
                }
            }
        }
    }
    None
}

/// Substitute all occurrences of TypeParam("Self") with the concrete target type.
fn substitute_self(ty: &Type, target: &Type) -> Type {
    match ty {
        Type::TypeParam(name) if name == "Self" => target.clone(),
        Type::Function(args, ret) => {
            let new_args = args.iter().map(|a| substitute_self(a, target)).collect();
            let new_ret = substitute_self(ret, target);
            Type::Function(new_args, Box::new(new_ret))
        }
        Type::List(inner) => Type::List(Box::new(substitute_self(inner, target))),
        Type::Tuple(elems) => Type::Tuple(elems.iter().map(|e| substitute_self(e, target)).collect()),
        Type::Enum(name, variants) => {
            let new_variants = variants.iter().map(|(n, payloads)| {
                (n.clone(), payloads.iter().map(|p| substitute_self(p, target)).collect())
            }).collect();
            Type::Enum(name.clone(), new_variants)
        }
        _ => ty.clone(),
    }
}

/// Validate a trait definition: check supertraits exist and detect cycles.
fn check_trait_def(
    node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let trait_name_node = match node.child_by_field_name("name") {
        Some(n) => n,
        None => return,
    };
    let trait_name = trait_name_node.utf8_text(source.as_bytes()).unwrap().to_string();

    // Clone supertraits to avoid borrow conflicts
    let supertraits = match symbols.lookup_trait(&trait_name) {
        Some(td) => td.supertraits.clone(),
        None => return,
    };

    // TRT_008: Unknown supertrait
    if let Some(supertrait_list) = node.child_by_field_name("supertraits") {
        let mut scursor = supertrait_list.walk();
        for st_child in supertrait_list.named_children(&mut scursor) {
            if st_child.kind() == "type_identifier" {
                let st_name = st_child.utf8_text(source.as_bytes()).unwrap().to_string();
                if symbols.lookup_trait(&st_name).is_none() {
                    diagnostics.push(Diagnostic {
                        code: "TRT_008".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file, &st_child),
                        message: format!("Unknown supertrait `{}`", st_name),
                        context: "Supertrait must be defined before use.".to_string(),
                        suggestions: vec![],
                    });
                }
            }
        }
    }

    // TRT_009: Cycle detection — check if trait_name appears in its own transitive supertraits
    let all_supers = symbols.all_supertraits(&trait_name);
    if all_supers.contains(&trait_name) {
        diagnostics.push(Diagnostic {
            code: "TRT_009".to_string(),
            severity: Severity::Error,
            location: Location::from_node(file, &trait_name_node),
            message: format!("Cyclic trait inheritance involving `{}`", trait_name),
            context: "Trait inheritance must form a DAG (no cycles).".to_string(),
            suggestions: vec![],
        });
    }
    drop(supertraits);
}

/// Type-check an impl block: validate trait exists, check method signatures, check bodies.
fn check_impl_block(
    node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let trait_name_node = match node.child_by_field_name("trait_name") {
        Some(n) => n,
        None => return,
    };
    let trait_name = trait_name_node.utf8_text(source.as_bytes()).unwrap().to_string();

    let target_type_node = match node.child_by_field_name("target_type") {
        Some(n) => n,
        None => return,
    };
    let target_type = super::type_parse::parse_type(&target_type_node, source, symbols);
    let type_key = SymbolTable::type_key(&target_type);

    // TRT_001: Unknown trait — clone data to avoid borrow conflicts with symbols
    let (trait_methods, default_methods) = match symbols.lookup_trait(&trait_name) {
        Some(td) => (td.methods.clone(), td.default_method_bytes.clone()),
        None => {
            diagnostics.push(Diagnostic {
                code: "TRT_001".to_string(),
                severity: Severity::Error,
                location: Location::from_node(file, &trait_name_node),
                message: format!("Unknown trait `{}`", trait_name),
                context: "Trait must be defined before implementing.".to_string(),
                suggestions: vec![],
            });
            return;
        }
    };

    // TRT_003: Duplicate impl check
    // Note: collect_impl_block already stored the first impl; we check here for diagnostics
    // We detect duplicate by counting how many impl_block nodes have the same trait+type
    // (collect_signatures skips duplicates, so the impl is already stored for the first one)

    // TRT_007: Cannot impl trait for non-concrete type
    match &target_type {
        Type::TypeParam(_) | Type::Var(_) | Type::Unknown => {
            diagnostics.push(Diagnostic {
                code: "TRT_007".to_string(),
                severity: Severity::Error,
                location: Location::from_node(file, &target_type_node),
                message: format!("Cannot impl trait for non-concrete type `{}`", target_type),
                context: "Trait implementations require a concrete type.".to_string(),
                suggestions: vec![],
            });
            return;
        }
        _ => {}
    }

    // TRT_010: Check all transitive supertraits are implemented
    let all_supers = symbols.all_supertraits(&trait_name);
    for super_name in &all_supers {
        if symbols.lookup_trait_impl(super_name, &type_key).is_none() {
            diagnostics.push(Diagnostic {
                code: "TRT_010".to_string(),
                severity: Severity::Error,
                location: Location::from_node(file, node),
                message: format!(
                    "Impl `{}` for `{}` requires impl `{}` for `{}`",
                    trait_name, type_key, super_name, type_key
                ),
                context: "All supertraits must be implemented.".to_string(),
                suggestions: vec![],
            });
        }
    }

    // Collect implemented method names
    let mut impl_method_names: Vec<String> = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() == "function_def" {
            if let Some(name_node) = child.child_by_field_name("name") {
                let method_name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();

                // TRT_006: Extra method not in trait
                if !trait_methods.iter().any(|(n, _)| n == &method_name) {
                    diagnostics.push(Diagnostic {
                        code: "TRT_006".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file, &name_node),
                        message: format!(
                            "Extra method `{}` not in trait `{}`",
                            method_name, trait_name
                        ),
                        context: format!(
                            "Trait `{}` defines: {}",
                            trait_name,
                            trait_methods.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>().join(", ")
                        ),
                        suggestions: vec![],
                    });
                    impl_method_names.push(method_name);
                    continue;
                }

                // Type-check the method body using the mangled name
                let mangled = format!("{}${}${}", trait_name, type_key, method_name);
                let fn_type = symbols.lookup(&mangled).cloned();

                if let Some(Type::Function(ref arg_types, ref ret_type)) = fn_type {
                    // TRT_005: Check method signature matches trait
                    if let Some((_, trait_fn_type)) = trait_methods.iter().find(|(n, _)| n == &method_name) {
                        let substituted = substitute_self(trait_fn_type, &target_type);
                        if let Type::Function(ref expected_args, ref expected_ret) = substituted {
                            let sig_matches = arg_types.len() == expected_args.len()
                                && arg_types.iter().zip(expected_args.iter()).all(|(a, e)| {
                                    super::type_compat::types_compatible(a, e)
                                })
                                && super::type_compat::types_compatible(ret_type, expected_ret);

                            if !sig_matches {
                                diagnostics.push(Diagnostic {
                                    code: "TRT_005".to_string(),
                                    severity: Severity::Error,
                                    location: Location::from_node(file, &name_node),
                                    message: format!(
                                        "Method `{}` signature doesn't match trait definition: expected {}, found {}",
                                        method_name, substituted, fn_type.as_ref().unwrap()
                                    ),
                                    context: "Impl method signature must match the trait definition.".to_string(),
                                    suggestions: vec![],
                                });
                            }
                        }
                    }

                    // Type-check the function body
                    symbols.enter_scope();

                    if let Some(params) = child.child_by_field_name("params") {
                        let mut pcursor = params.walk();
                        let mut i = 0;
                        for param in params.named_children(&mut pcursor) {
                            if param.kind() == "param" {
                                // Grammar uses field('pattern', ...) not field('name', ...)
                                let name_node = param.child_by_field_name("name")
                                    .or_else(|| param.child_by_field_name("pattern"));
                                if let Some(pn) = name_node {
                                    let arg_name = pn.utf8_text(source.as_bytes()).unwrap().to_string();
                                    if i < arg_types.len() {
                                        symbols.insert(arg_name, arg_types[i].clone());
                                    }
                                }
                                i += 1;
                            }
                        }
                    }

                    if let Some(body_node) = child.child_by_field_name("body") {
                        let body_type = check_node(&body_node, source, file, symbols, diagnostics);
                        if !super::type_compat::types_compatible(&body_type, ret_type) && **ret_type != Type::Unit {
                            diagnostics.push(Diagnostic {
                                code: "TYP_006".to_string(),
                                severity: Severity::Error,
                                location: Location::from_node(file, &body_node),
                                message: format!(
                                    "Function `{}` declares return type {}, body returns {}",
                                    method_name, ret_type, body_type
                                ),
                                context: "Function body return type must match signature.".to_string(),
                                suggestions: vec![],
                            });
                        }
                    }

                    symbols.exit_scope();
                }

                impl_method_names.push(method_name);
            }
        }
    }

    // TRT_002: Missing methods (only for methods without defaults)
    for (method_name, _) in &trait_methods {
        if !impl_method_names.contains(method_name) && !default_methods.contains_key(method_name) {
            diagnostics.push(Diagnostic {
                code: "TRT_002".to_string(),
                severity: Severity::Error,
                location: Location::from_node(file, node),
                message: format!(
                    "Missing method `{}` in impl of `{}` for `{}`",
                    method_name, trait_name, type_key
                ),
                context: "All trait methods must be implemented.".to_string(),
                suggestions: vec![],
            });
        }
    }

    // Type-check default method bodies in the context of this concrete type
    for (method_name, default_byte) in &default_methods {
        if impl_method_names.contains(method_name) {
            continue; // Overridden by impl, skip
        }
        // Find the CST node for the default method body
        let mangled = format!("{}${}${}", trait_name, type_key, method_name);
        let fn_type = symbols.lookup(&mangled).cloned();
        if let Some(Type::Function(ref arg_types, ref _ret_type)) = fn_type {
            // Find the default function_def node by byte offset
            let tcursor = node.walk();
            // We need to find the function_def in the TRAIT node, not the impl node
            // The default_byte refers to a byte in the trait_def
            // We'll search all top-level nodes for it
            let root = {
                let mut n = *node;
                while let Some(p) = n.parent() {
                    n = p;
                }
                n
            };
            // Search for the function_def at the given byte offset in the source tree
            if let Some(default_node) = find_function_def_at_byte(&root, *default_byte) {
                symbols.enter_scope();

                // Bind parameters with concrete types
                if let Some(params) = default_node.child_by_field_name("params") {
                    let mut pcursor = params.walk();
                    let mut i = 0;
                    for param in params.named_children(&mut pcursor) {
                        if param.kind() == "param" {
                            // Grammar uses field('pattern', ...) not field('name', ...)
                            let name_node = param.child_by_field_name("name")
                                .or_else(|| param.child_by_field_name("pattern"));
                            if let Some(pn) = name_node {
                                let arg_name = pn.utf8_text(source.as_bytes()).unwrap().to_string();
                                if i < arg_types.len() {
                                    symbols.insert(arg_name, arg_types[i].clone());
                                }
                            }
                            i += 1;
                        }
                    }
                }

                if let Some(body_node) = default_node.child_by_field_name("body") {
                    let _body_type = check_node(&body_node, source, file, symbols, diagnostics);
                }

                symbols.exit_scope();
            }
            drop(tcursor);
        }
    }
}
