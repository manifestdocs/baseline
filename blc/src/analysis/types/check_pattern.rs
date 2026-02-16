use super::symbol_table::SymbolTable;
use super::type_def::{PatternCoverage, Type};
use crate::diagnostics::{Diagnostic, Location, Patch, Severity, Suggestion};
use std::collections::HashSet;
use tree_sitter::Node;

#[allow(clippy::only_used_in_recursion)]
pub(super) fn check_pattern(
    node: &Node,
    expected_type: &Type,
    source: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match node.kind() {
        "wildcard_pattern" => {}
        "identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
            symbols.insert(name, expected_type.clone());
        }
        "type_identifier" => {
            // Nullary constructor pattern: None, Active
            // Verify it's a valid constructor for the expected type
        }
        "constructor_pattern" => {
            // Constructor pattern: Some(v), Pending(msg)
            let ctor_name_node = node.child(0).unwrap();
            let ctor_name = ctor_name_node.utf8_text(source.as_bytes()).unwrap();

            // Extract payload types from the expected enum type if available,
            // otherwise fall back to the constructor's signature in the symbol table.
            let payload_types: Vec<Type> = if let Type::Enum(_, variants) = expected_type {
                variants
                    .iter()
                    .find(|(name, _)| name == ctor_name)
                    .map(|(_, payloads)| payloads.clone())
                    .unwrap_or_default()
            } else {
                symbols
                    .lookup(ctor_name)
                    .and_then(|ty| match ty {
                        Type::Function(params, _) => Some(params.clone()),
                        _ => None,
                    })
                    .unwrap_or_default()
            };

            // Bind each sub-pattern to its payload type
            let mut pattern_idx = 0;
            let child_count = node.child_count();
            for ci in 0..child_count {
                let child = node.child(ci).unwrap();
                if child.kind() != "type_identifier"
                    && child.kind() != "("
                    && child.kind() != ")"
                    && child.kind() != ","
                    && pattern_idx < payload_types.len()
                {
                    check_pattern(
                        &child,
                        &payload_types[pattern_idx],
                        source,
                        symbols,
                        diagnostics,
                    );
                    pattern_idx += 1;
                }
            }
        }
        "literal" => {
            if let Some(child) = node.named_child(0) {
                check_pattern(&child, expected_type, source, symbols, diagnostics);
            }
        }
        "tuple_pattern" => {
            let count = node.named_child_count();
            if let Type::Tuple(elem_types) = expected_type {
                for i in 0..count {
                    let sub_pat = node.named_child(i).unwrap();
                    let elem_ty = if i < elem_types.len() {
                        elem_types[i].clone()
                    } else {
                        Type::Unknown
                    };
                    check_pattern(&sub_pat, &elem_ty, source, symbols, diagnostics);
                }
            } else {
                // Expected type is not a tuple — bind sub-patterns as Unknown
                for i in 0..count {
                    let sub_pat = node.named_child(i).unwrap();
                    check_pattern(&sub_pat, &Type::Unknown, source, symbols, diagnostics);
                }
            }
        }
        "record_pattern" => {
            let count = node.named_child_count();
            for i in 0..count {
                let field_node = node.named_child(i).unwrap();
                if field_node.kind() == "record_pattern_field" {
                    let name_node = field_node.child_by_field_name("name");
                    let name = name_node
                        .map(|n| n.utf8_text(source.as_bytes()).unwrap().to_string())
                        .unwrap_or_default();

                    // Resolve field type from expected record type
                    let field_type = match expected_type {
                        Type::Record(fields, _) => fields
                            .iter()
                            .find(|(f, _)| *f == &name)
                            .map(|(_, t)| t.clone())
                            .unwrap_or(Type::Unknown),
                        _ => Type::Unknown,
                    };

                    // Check sub-pattern if { x: pat }, otherwise bind shorthand { x }
                    if let Some(pat_node) = field_node.child_by_field_name("pattern") {
                        check_pattern(&pat_node, &field_type, source, symbols, diagnostics);
                    } else {
                        // Shorthand: { x } — bind x with field's type
                        symbols.insert(name, field_type);
                    }
                }
            }
        }
        "integer_literal" => {
            // Basic check
        }
        _ => {}
    }
}

/// Extract what a pattern covers at the top level.
pub(super) fn extract_pattern_coverage(node: &Node, source: &str) -> Option<PatternCoverage> {
    match node.kind() {
        "wildcard_pattern" => Some(PatternCoverage::CatchAll),
        "identifier" => Some(PatternCoverage::CatchAll),
        "type_identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
            Some(PatternCoverage::Variant(name))
        }
        "constructor_pattern" => {
            let ctor = node.child(0).unwrap();
            let name = ctor.utf8_text(source.as_bytes()).unwrap().to_string();
            Some(PatternCoverage::Variant(name))
        }
        "literal" => {
            if let Some(child) = node.named_child(0) {
                extract_pattern_coverage(&child, source)
            } else {
                None
            }
        }
        "boolean_literal" => {
            let text = node.utf8_text(source.as_bytes()).unwrap();
            Some(PatternCoverage::BoolLiteral(text == "true"))
        }
        _ => None,
    }
}

/// Check whether a match expression covers all variants of the matched type.
pub(super) fn check_match_exhaustiveness(
    node: &Node,
    expr_type: &Type,
    source: &str,
    file: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let count = node.named_child_count();
    let mut has_catch_all = false;
    let mut covered_variants: HashSet<String> = HashSet::new();
    let mut covered_true = false;
    let mut covered_false = false;

    for i in 1..count {
        let arm = node.named_child(i).unwrap();

        // Guarded arms don't count for exhaustiveness — the compiler
        // cannot prove a guard will be true, so a guarded catch-all
        // (e.g. `_ if cond -> ...`) does not cover the pattern.
        let has_guard = {
            let mut cursor = arm.walk();
            arm.children(&mut cursor)
                .any(|c| c.kind() == "match_guard")
        };
        if has_guard {
            continue;
        }

        // Collect all pattern nodes in this arm (or-patterns have multiple)
        let body = arm.child(arm.child_count() - 1).unwrap();
        let mut pat_cursor = arm.walk();
        let pat_nodes: Vec<Node> = arm.named_children(&mut pat_cursor)
            .filter(|c| c.kind() != "match_guard" && c.id() != body.id())
            .collect();

        for pat in &pat_nodes {
            if let Some(coverage) = extract_pattern_coverage(pat, source) {
                match coverage {
                    PatternCoverage::CatchAll => {
                        has_catch_all = true;
                    }
                    PatternCoverage::Variant(name) => {
                        covered_variants.insert(name);
                    }
                    PatternCoverage::BoolLiteral(val) => {
                        if val {
                            covered_true = true;
                        } else {
                            covered_false = true;
                        }
                    }
                }
            }
        }
    }

    if has_catch_all {
        return;
    }

    let (missing_desc, type_name) = match expr_type {
        Type::Enum(name, variants) => {
            let all: HashSet<String> = variants.iter().map(|(v, _)| v.clone()).collect();
            let missing: Vec<String> = all.difference(&covered_variants).cloned().collect();
            if missing.is_empty() {
                return;
            }
            (missing, name.clone())
        }
        Type::Bool => {
            if covered_true && covered_false {
                return;
            }
            let mut missing = Vec::new();
            if !covered_true {
                missing.push("true".to_string());
            }
            if !covered_false {
                missing.push("false".to_string());
            }
            (missing, "Bool".to_string())
        }
        Type::Int | Type::String | Type::Float => (
            vec!["_".to_string()],
            match expr_type {
                Type::Int => "Int".to_string(),
                Type::String => "String".to_string(),
                Type::Float => "Float".to_string(),
                _ => unreachable!(),
            },
        ),
        _ => return,
    };

    let missing_str = missing_desc.join(", ");
    let last_arm = node.named_child(count - 1).unwrap();
    let insert_line = last_arm.end_position().row + 2; // 1-indexed, after last arm

    diagnostics.push(Diagnostic {
        code: "TYP_022".to_string(),
        severity: Severity::Error,
        location: Location::from_node(file,node),
        message: format!(
            "Non-exhaustive match on '{}': missing variant(s) {}",
            type_name, missing_str
        ),
        context: "All variants must be handled, or add a wildcard '_' arm.".to_string(),
        suggestions: vec![
            Suggestion {
                strategy: "add_missing_arms".to_string(),
                description: format!("Add arms for {}", missing_str),
                confidence: None,
                patch: Some(Patch {
                    start_line: insert_line,
                    original_text: None,
                    replacement_text: Some(
                        missing_desc
                            .iter()
                            .map(|v| format!("    {} -> todo", v))
                            .collect::<Vec<_>>()
                            .join("\n"),
                    ),
                    operation: Some("insert".to_string()),
                }),
            },
            Suggestion {
                strategy: "add_wildcard".to_string(),
                description: "Add a wildcard '_' arm".to_string(),
                confidence: None,
                patch: Some(Patch {
                    start_line: insert_line,
                    original_text: None,
                    replacement_text: Some("    _ -> todo".to_string()),
                    operation: Some("insert".to_string()),
                }),
            },
        ],
    });
}
