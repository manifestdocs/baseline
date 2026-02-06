//! Effect checking pass.
//!
//! This module implements Level 1 verification from the Technical Specification:
//! capability checking for effectful function calls.
//!
//! The algorithm:
//! 1. Find all function declarations and extract their declared effect sets
//! 2. Walk each function body looking for effectful calls (identifiers ending in `!`)
//! 3. For each effectful call, infer the required effect from the callee
//! 4. Check if the required effect is in the function's declared effect set
//! 5. Emit CAP_XXX diagnostics for violations

use tree_sitter::{Node, Tree};

use crate::diagnostics::{Diagnostic, Location, Patch, Severity, Suggestion};

/// Check effects in a parsed syntax tree.
///
/// Returns a list of diagnostics for any effect violations found.
pub fn check_effects(tree: &Tree, source: &str, file: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let root = tree.root_node();

    // Recurse function definitions
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
         if child.kind() == "function_def" {
             check_function(child, source, file, &mut diagnostics);
         }
    }

    diagnostics
}

/// Check a single function declaration for effect violations.
fn check_function(node: Node, source: &str, file: &str, diagnostics: &mut Vec<Diagnostic>) {
    // Extract function name
    let func_name = find_function_name(node, source).unwrap_or("<unknown>");

    // Extract declared effects from the type annotation
    let declared_effects = extract_declared_effects(node, source);

    // 4. Find the function body
    // We try named child "body" first, then fallback to last child
    let mut body_node = node.child_by_field_name("body");
    
    if body_node.is_none() {
         let count = node.named_child_count();
         if count > 0 {
             // Assume last child is body (after = or just the block)
             body_node = node.named_child(count - 1);
         }
    }

    if let Some(body) = body_node {
        check_node_for_effects(
            body,
            source,
            file,
            &func_name,
            &declared_effects,
            node,
            diagnostics,
        );
    }
}

/// Find the function name from a function_def node.
fn find_function_name<'a>(node: Node<'a>, source: &'a str) -> Option<&'a str> {
    // The grammar defines: field('name', $._name) twice in function_def.
    // child_by_field_name("name") returns the first occurrence.
    if let Some(name_node) = node.child_by_field_name("name") {
        return name_node.utf8_text(source.as_bytes()).ok();
    }
    // Fallback: search children directly
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "identifier" | "effectful_identifier" => {
                return child.utf8_text(source.as_bytes()).ok();
            }
            _ => continue,
        }
    }
    None
}

/// Extract declared effects from a function's type annotation.
///
/// Looks for patterns like `func : Type -> {Http, Db} Result`
fn extract_declared_effects(node: Node, source: &str) -> Vec<String> {
    let mut effects = Vec::new();

    // Find the signature child
    if let Some(sig) = node.child_by_field_name("signature") {
        collect_effects_from_type(sig, source, &mut effects);
        return effects;
    }

    // Fallback search
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "type_signature" || child.kind() == "type_annotation" {
            collect_effects_from_type(child, source, &mut effects);
        }
    }

    effects
}

/// Recursively collect effect names from a type expression.
fn collect_effects_from_type(node: Node, source: &str, effects: &mut Vec<String>) {
    if node.kind() == "effect_set" {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "type_identifier" {
                if let Ok(name) = child.utf8_text(source.as_bytes()) {
                    effects.push(name.to_string());
                }
            }
        }
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_effects_from_type(child, source, effects);
    }
}

/// Recursively check a node and its children for effectful calls.
fn check_node_for_effects(
    node: Node,
    source: &str,
    file: &str,
    func_name: &str,
    declared_effects: &[String],
    func_node: Node,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // DEBUG: Log everything
    // eprintln!("DEBUG: visiting node kind: {}", node.kind());
    if node.kind() == "call_expression" {
         // eprintln!("DEBUG: visiting call_expression: {}", node.to_sexp());
    }

    // Check for effectful identifiers (calls ending in !)
    if node.kind() == "effect_identifier" {
        if let Ok(call_name) = node.utf8_text(source.as_bytes()) {
            check_effectful_call(
                node,
                call_name,
                source,
                file,
                func_name,
                declared_effects,
                func_node,
                diagnostics,
            );
        }
    }

    // Check for field expressions (Log.info!)
    if node.kind() == "field_expression" {
        // field_expression: expr . id
        let obj = node.named_child(0);
        let field = node.named_child(1);
        
        if let (Some(obj_node), Some(field_node)) = (obj, field) {
            if field_node.kind() == "effect_identifier" {
                if let Ok(effect_name) = obj_node.utf8_text(source.as_bytes()) {
                     if let Ok(method_name) = field_node.utf8_text(source.as_bytes()) {
                         // We have Effect.method!
                         // Construct call name for reporting? Or just use Effect
                         // required_effect is inferred from Effect name (obj) if possible, 
                         // or we can rely on existing inference from method name?
                         // "Log.info!" -> infer from "Log"? 
                         // infer_required_effect("Log.info!") -> "Log"
                         
                         let full_name = format!("{}.{}", effect_name, method_name);
                          check_effectful_call(
                                node,
                                &full_name,
                                source,
                                file,
                                func_name,
                                declared_effects,
                                func_node,
                                diagnostics,
                            );
                            // Don't recurse to avoid double reporting
                            return;
                     }
                }
            }
        }
    }

    // Check for qualified effectful calls like Http.get! (Legacy/Alternative)
    if node.kind() == "qualified_identifier" {
        if let Some(effect_call) = extract_qualified_effect_call(node, source) {
            check_effectful_call(
                node,
                &effect_call,
                source,
                file,
                func_name,
                declared_effects,
                func_node,
                diagnostics,
            );
        }
        // Don't recurse into qualified identifiers to avoid checking the inner effectful_identifier separately
        return;
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        check_node_for_effects(
            child,
            source,
            file,
            func_name,
            declared_effects,
            func_node,
            diagnostics,
        );
    }
}

/// Extract effect name and method from a qualified identifier like `Http.get!`.
fn extract_qualified_effect_call(node: Node, source: &str) -> Option<String> {
    let text = node.utf8_text(source.as_bytes()).ok()?;
    if text.ends_with('!') {
        Some(text.to_string())
    } else {
        None
    }
}

/// Check if an effectful call is allowed given the declared effects.
fn check_effectful_call(
    node: Node,
    call_name: &str,
    source: &str,
    file: &str,
    func_name: &str,
    declared_effects: &[String],
    func_node: Node,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Infer the required effect from the call
    let required_effect = infer_required_effect(call_name);

    // Check if the required effect is in the declared set
    let has_effect = declared_effects
        .iter()
        .any(|e| e.eq_ignore_ascii_case(&required_effect));

    if !has_effect {
        let start = node.start_position();
        let end = node.end_position();

        // Build the suggestion patch
        let (patch, _original_text) = build_effect_patch(func_node, source, &required_effect);

        diagnostics.push(Diagnostic {
            code: "CAP_001".to_string(),
            severity: Severity::Error,
            location: Location {
                file: file.to_string(),
                line: start.row + 1,
                col: start.column + 1,
                end_line: Some(end.row + 1),
                end_col: Some(end.column + 1),
            },
            message: format!("Unauthorized Side Effect: '{}'", call_name),
            context: format!(
                "Function '{}' declares effects {{{}}}, but calls '{}' which requires {{{}}}.",
                func_name,
                declared_effects.join(", "),
                call_name,
                required_effect
            ),
            suggestions: vec![
                Suggestion {
                    strategy: "escalate_capability".to_string(),
                    description: format!("Add {} to the function signature", required_effect),
                    patch: Some(patch),
                },
                Suggestion {
                    strategy: "remove_call".to_string(),
                    description: "Delete the unauthorized call".to_string(),
                    patch: Some(Patch {
                        start_line: start.row + 1,
                        original_text: Some(call_name.to_string()),
                        replacement_text: None,
                        operation: Some("delete".to_string()),
                    }),
                },
            ],
        });
    }
}

/// Infer the required effect from an effectful call name.
///
/// For `Http.get!` returns "Http"
/// For `log!` returns "Log" (capitalized)
fn infer_required_effect(call_name: &str) -> String {
    // Handle qualified names like Http.get!
    if let Some(dot_pos) = call_name.find('.') {
        let module = &call_name[..dot_pos];
        // Server.listen! requires the Http effect (starting a server is an Http capability)
        return match module {
            "Server" => "Http".to_string(),
            _ => module.to_string(),
        };
    }

    // Handle simple names
    let name = call_name.trim_end_matches('!');
    
    // Check Known Effects Registry first
    match name {
        "print" | "println" | "eprint" | "read_line" => return "Console".to_string(),
        "now" | "sleep" => return "Time".to_string(),
        "random" | "uuid" => return "Random".to_string(),
        "read_top_secrets" => return "Fs".to_string(), // Example for testing
        "log" | "info" | "warn" | "error" => return "Log".to_string(),
        _ => {}
    }

    // Fallback: Capitalize the first letter (heuristic)
    let mut chars: Vec<char> = name.chars().collect();
    if let Some(first) = chars.first_mut() {
        *first = first.to_ascii_uppercase();
    }
    chars.into_iter().collect()
}

/// Build a patch to add an effect to the function signature.
fn build_effect_patch(func_node: Node, source: &str, new_effect: &str) -> (Patch, String) {
    let func_line = func_node.start_position().row + 1;

    // Get the original signature line
    let lines: Vec<&str> = source.lines().collect();
    let original_line = lines
        .get(func_node.start_position().row)
        .unwrap_or(&"")
        .to_string();

    // Try to find existing effect set and add to it
    let replacement = if let Some(effect_start) = original_line.find('{') {
        if let Some(effect_end) = original_line.find('}') {
            let existing = &original_line[effect_start + 1..effect_end];
            let new_effects = format!("{}, {}", existing, new_effect);
            original_line.replace(
                &original_line[effect_start..=effect_end],
                &format!("{{{}}}", new_effects),
            )
        } else {
            original_line.clone()
        }
    } else {
        // No existing effects - need to add effect set before return type
        // This is a simplified heuristic
        if let Some(arrow_pos) = original_line.find("->") {
            let (before, after) = original_line.split_at(arrow_pos);
            format!("{}{{{}}}{}", before.trim_end(), new_effect, after)
        } else {
            original_line.clone()
        }
    };

    (
        Patch {
            start_line: func_line,
            original_text: Some(original_line.clone()),
            replacement_text: Some(replacement),
            operation: None,
        },
        original_line,
    )
}
