use super::check_lambda::{bind_pattern, check_lambda_with_expected};
use super::check_pattern::{check_match_exhaustiveness, check_pattern};
use super::symbol_table::SymbolTable;
use super::type_compat::{detect_implicit_type_params, extract_explicit_type_params, types_compatible};
use super::type_def::Type;
use super::type_parse::{call_arg_expr, infer_expected_type, parse_type};
use crate::diagnostics::{Diagnostic, Location, Severity, Suggestion};
use super::super::infer::{InferCtx, UserGenericSchema};
use std::collections::HashSet;
use tree_sitter::Node;

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
    symbols.type_map.insert(node.start_byte(), ty.clone());
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
            if !type_param_names.is_empty() {
                symbols.user_generic_schemas.insert(
                    name.clone(),
                    UserGenericSchema {
                        type_param_names,
                        fn_type: function_type.clone(),
                    },
                );
            }

            if let Type::Function(arg_types, ret_type) = &function_type {
                symbols.enter_scope();

                // Bind params from param_list
                if let Some(params) = node.child_by_field_name("params") {
                    let mut cursor = params.walk();
                    let mut i = 0;
                    for param in params.named_children(&mut cursor) {
                        if param.kind() == "param"
                            && let Some(name_node) = param.child_by_field_name("name")
                        {
                            let arg_name =
                                name_node.utf8_text(source.as_bytes()).unwrap().to_string();
                            if i < arg_types.len() {
                                symbols.insert(arg_name, arg_types[i].clone());
                            }
                            i += 1;
                        }
                    }
                }

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
                let mut ctx = InferCtx::new();
                let instantiated = if let Some(schema) = builtin_schema {
                    (schema.build)(&mut ctx)
                } else {
                    user_schema.unwrap().instantiate(&mut ctx)
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
                        // Partial application: return function for remaining params
                        let remaining = arg_types[params_provided..].to_vec();
                        return Type::Function(remaining, ret_type);
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

                for (i, expected_arg_type) in arg_types
                    .iter()
                    .enumerate()
                    .take(std::cmp::min(params_provided, arg_types.len()))
                {
                    let raw_arg = node.named_child(i + 1).unwrap();
                    let arg_expr = call_arg_expr(&raw_arg);

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
                                i + 1,
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
        "struct_expression" => {
            let type_name_node = node.named_child(0).unwrap();
            let type_name = type_name_node.utf8_text(source.as_bytes()).unwrap();

            if let Some(Type::Struct(name, fields)) = symbols.lookup_type(type_name).cloned() {
                let mut initialized_fields = std::collections::HashSet::new();

                let count = node.named_child_count();
                for i in 1..count {
                    let field_init = node.named_child(i).unwrap();
                    let fname_node = field_init.child(0).unwrap();
                    let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();

                    let fexpr_node = field_init.child(2).unwrap();
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
                    let fname_node = field_init.child(0).unwrap();
                    let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                    let fexpr_node = field_init.child(2).unwrap();
                    let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);
                    fields.insert(fname, ftype);
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
                        let fname_node = field_init.child(0).unwrap();
                        let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                        let fexpr_node = field_init.child(2).unwrap();
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
                    let count = node.named_child_count();
                    for i in 1..count {
                        let field_init = node.named_child(i).unwrap();
                        let fname_node = field_init.child(0).unwrap();
                        let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                        let fexpr_node = field_init.child(2).unwrap();
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
                                message: format!("Record has no field `{}`", fname),
                                context: "Field not defined in record.".to_string(),
                                suggestions: vec![],
                            });
                        }
                    }
                    base_type.clone()
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
                    } else {
                        // Unknown method — allow for forward compat / effect-only checking
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
                let pat = arm.child(0).unwrap();
                let body = arm.child(2).unwrap();

                symbols.enter_scope();
                check_pattern(&pat, &expr_type, source, symbols, diagnostics);

                let body_type = check_node(&body, source, file, symbols, diagnostics);
                symbols.exit_scope();

                if first {
                    ret_type = body_type;
                    first = false;
                } else if !types_compatible(&body_type, &ret_type) {
                    diagnostics.push(Diagnostic {
                        code: "TYP_017".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&body),
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
            let right_type = check_node(&right_node, source, file, symbols, diagnostics);

            if let Type::Function(args, ret) = right_type {
                if args.len() != 1 {
                    diagnostics.push(Diagnostic {
                        code: "TYP_018".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&right_node),
                        message: format!("Pipe function expects 1 argument, found {}", args.len()),
                        context: "Pipe operator expects a unary function.".to_string(),
                        suggestions: vec![],
                    });
                } else if !types_compatible(&left_type, &args[0]) {
                    diagnostics.push(Diagnostic {
                        code: "TYP_019".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&left_node),
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
                    location: Location::from_node(file,&right_node),
                    message: format!("Pipe target is not a function, it is {}", right_type),
                    context: "Left side must be piped into a function.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
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
