//! Effect checking pass.
//!
//! This module implements capability checking for effectful function calls.
//!
//! The algorithm:
//! 1. Find all function declarations and extract their declared effect sets
//! 2. Walk each function body looking for effectful calls (identifiers ending in `!`)
//! 3. For each effectful call, infer the required effect from the callee
//! 4. Check if the required effect is in the function's declared effect set
//! 5. Build a call graph of user-defined function calls
//! 6. Compute transitive effect closure via fixed-point iteration
//! 7. Verify that each function's declared effects cover its transitive closure
//! 8. Emit CAP_XXX diagnostics for violations
//!
//! ## Ambient Effects
//!
//! Low-risk observability effects (Log, Time, Random) are "ambient": they are
//! allowed without explicit declaration. This prevents the logging tax where
//! every function in a realistic codebase would need `{Log}` in its signature.
//!
//! ## Effect Inference
//!
//! Functions without explicit effect annotations get their effects inferred
//! bottom-up from their body. Only functions with explicit `{Effect}` declarations
//! or `@pure` annotations are checked against their declaration.
//!
//! ## Restrict Blocks
//!
//! `restrict(Effect1, Effect2) { body }` narrows the allowed effects within
//! a lexical scope. This is the compile-time inverse of `handle`: instead of
//! dynamically intercepting effects, it statically limits permissions.

use std::collections::{HashMap, HashSet};

use tree_sitter::{Node, Tree};

use crate::diagnostics::{Diagnostic, Location, Patch, Severity, Suggestion};

/// Effects that are allowed without explicit declaration.
///
/// These are low-risk observability effects that would otherwise infect every
/// function signature in a realistic codebase. The runtime `handle` blocks can
/// still intercept these effects if needed (e.g., for testing).
///
/// Random is NOT ambient: it affects determinism, which matters for property-based
/// testing and reproducibility.
const AMBIENT_EFFECTS: &[&str] = &["Log", "Time"];

/// Check if an effect is ambient (allowed without declaration).
fn is_ambient_effect(effect: &str) -> bool {
    AMBIENT_EFFECTS
        .iter()
        .any(|e| e.eq_ignore_ascii_case(effect))
}

/// Check if a top-level node (function_def or spec_block) has a `@pure` annotation.
fn has_pure_annotation(node: Node) -> bool {
    if node.kind() == "spec_block" {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "spec_attribute" {
                let mut inner = child.walk();
                for attr in child.children(&mut inner) {
                    if attr.kind() == "pure_attribute" {
                        return true;
                    }
                }
            }
            if child.kind() == "pure_attribute" {
                return true;
            }
        }
    }
    false
}

/// Determine the effect declaration mode for a function.
///
/// - `Declared(effects)` — function has explicit `{Effect}` annotations; check inferred ⊆ declared.
/// - `Pure` — function has `@pure`; any non-ambient effect is a violation.
/// - `Inferred` — no annotation; effects are inferred bottom-up, no checking needed.
#[derive(Debug)]
enum EffectMode {
    Declared(Vec<String>),
    Pure,
    Inferred,
}

fn determine_effect_mode(func_node: Node, top_level_node: Node, source: &str) -> EffectMode {
    if has_pure_annotation(top_level_node) {
        return EffectMode::Pure;
    }
    let declared = extract_declared_effects(func_node, source);
    if !declared.is_empty() {
        EffectMode::Declared(declared)
    } else {
        // Check if the function has an explicit empty effect set vs no annotation at all.
        // If the function has `-> Type` with no `{...}`, it's Inferred.
        // If it has `-> {} Type` (empty braces), that would be Pure, but the grammar
        // requires at least one type_identifier in effect_set, so empty isn't possible.
        EffectMode::Inferred
    }
}

/// Extract function_def from a top-level child, unwrapping spec_block if needed.
fn extract_function_def<'a>(node: Node<'a>) -> Option<Node<'a>> {
    match node.kind() {
        "function_def" => Some(node),
        "spec_block" => {
            let count = node.named_child_count();
            for i in 0..count {
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

/// Check effects in a parsed syntax tree.
///
/// Returns a list of diagnostics for any effect violations found.
pub fn check_effects(tree: &Tree, source: &str, file: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let root = tree.root_node();

    // Collect all function names defined in this module
    let mut defined_functions: HashSet<String> = HashSet::new();
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if let Some(func) = extract_function_def(child)
            && let Some(name) = find_function_name(func, source)
        {
            defined_functions.insert(name.to_string());
        }
    }

    // Phase 1: Direct effect checking + build call graph and direct effects map.
    // Effect mode determines checking behavior:
    //   Declared(effects) — check direct calls against declared set
    //   Pure — check direct calls against empty set
    //   Inferred — skip direct checking entirely
    let mut effect_mode_map: HashMap<String, EffectMode> = HashMap::new();
    let mut direct_effects_map: HashMap<String, HashSet<String>> = HashMap::new();
    let mut call_graph: HashMap<String, HashSet<String>> = HashMap::new();

    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if let Some(func) = extract_function_def(child) {
            let mode = determine_effect_mode(func, child, source);

            // Only check direct effects for functions with explicit declarations
            match &mode {
                EffectMode::Declared(_) | EffectMode::Pure => {
                    check_function(func, &mode, source, file, &mut diagnostics);
                }
                EffectMode::Inferred => {
                    // No direct checking — effects will be inferred
                }
            }

            // Build call graph data (always, even for inferred functions)
            if let Some(name) = find_function_name(func, source) {
                let name = name.to_string();

                let mut direct_effects = HashSet::new();
                let mut callees = HashSet::new();

                if let Some(body) = get_function_body(func) {
                    collect_call_graph_info(
                        body,
                        source,
                        &defined_functions,
                        &mut direct_effects,
                        &mut callees,
                    );
                }

                direct_effects_map.insert(name.clone(), direct_effects);
                call_graph.insert(name.clone(), callees);
                effect_mode_map.insert(name, mode);
            }
        }
    }

    // Phase 2: Compute transitive effect closure via fixed-point iteration
    let transitive_effects = compute_transitive_effects(&direct_effects_map, &call_graph);

    // Phase 3: Check transitive effects against declared/pure effects.
    // Inferred functions are skipped — their transitive set IS their effect signature.
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if let Some(func) = extract_function_def(child)
            && let Some(name) = find_function_name(func, source)
        {
            let name_str = name.to_string();
            let mode = effect_mode_map.get(&name_str);

            match mode {
                Some(EffectMode::Inferred) | None => {
                    // No transitive checking for inferred functions
                }
                Some(EffectMode::Declared(_)) | Some(EffectMode::Pure) => {
                    check_transitive_effects(
                        func,
                        &name_str,
                        &mode.unwrap(),
                        source,
                        file,
                        &transitive_effects,
                        &call_graph,
                        &direct_effects_map,
                        &mut diagnostics,
                    );
                }
            }
        }
    }

    diagnostics
}

/// Get the body node of a function_def.
fn get_function_body(node: Node) -> Option<Node> {
    let mut body_node = node.child_by_field_name("body");
    if body_node.is_none() {
        let count = node.named_child_count();
        if count > 0 {
            body_node = node.named_child(count - 1);
        }
    }
    body_node
}

/// Collect call graph information from a function body.
///
/// Walks the body to find:
/// - Direct effect requirements (from `!` calls)
/// - Calls to user-defined functions (for transitive analysis)
fn collect_call_graph_info(
    node: Node,
    source: &str,
    defined_functions: &HashSet<String>,
    direct_effects: &mut HashSet<String>,
    callees: &mut HashSet<String>,
) {
    // Check for effectful identifiers (calls ending in !)
    if node.kind() == "effect_identifier"
        && let Ok(call_name) = node.utf8_text(source.as_bytes())
    {
        let effect = infer_required_effect(call_name);
        direct_effects.insert(effect);
    }

    // Check for field expressions (Log.info!)
    if node.kind() == "field_expression" {
        let obj = node.named_child(0);
        let field = node.named_child(1);
        if let (Some(obj_node), Some(field_node)) = (obj, field)
            && field_node.kind() == "effect_identifier"
            && let Ok(effect_name) = obj_node.utf8_text(source.as_bytes())
            && let Ok(method_name) = field_node.utf8_text(source.as_bytes())
        {
            let full_name = format!("{}.{}", effect_name, method_name);
            let effect = infer_required_effect(&full_name);
            direct_effects.insert(effect);
        }
        // Don't recurse to avoid double-counting
        return;
    }

    // Check for qualified effectful calls
    if node.kind() == "qualified_identifier" {
        if let Some(effect_call) = extract_qualified_effect_call(node, source) {
            let effect = infer_required_effect(&effect_call);
            direct_effects.insert(effect);
        }
        return;
    }

    // Check for call_expression to user-defined functions
    if node.kind() == "call_expression" {
        // The first named child is the callee expression
        if let Some(callee) = node.named_child(0) {
            if callee.kind() == "identifier"
                && let Ok(callee_name) = callee.utf8_text(source.as_bytes())
                && defined_functions.contains(callee_name)
            {
                callees.insert(callee_name.to_string());
            }
            // Also check effectful identifiers used as callee in call_expression
            // e.g., `foo!(x)` — the callee is an effect_identifier
            if callee.kind() == "effect_identifier"
                && let Ok(callee_name) = callee.utf8_text(source.as_bytes())
            {
                let base_name = callee_name.trim_end_matches('!');
                if defined_functions.contains(callee_name) {
                    callees.insert(callee_name.to_string());
                } else if defined_functions.contains(base_name) {
                    callees.insert(base_name.to_string());
                }
            }
        }
    }

    // with_expression and handle_expression suppress their named effects
    if node.kind() == "with_expression" || node.kind() == "handle_expression" {
        // The body is handled; don't collect effects from it
        return;
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_call_graph_info(child, source, defined_functions, direct_effects, callees);
    }
}

/// Compute transitive effects for all functions via fixed-point iteration.
///
/// Starting from each function's direct effects, propagate effects through the call graph
/// until no new effects are added (fixed point reached). Handles mutual recursion.
fn compute_transitive_effects(
    direct_effects_map: &HashMap<String, HashSet<String>>,
    call_graph: &HashMap<String, HashSet<String>>,
) -> HashMap<String, HashSet<String>> {
    // Initialize transitive effects with direct effects
    let mut transitive: HashMap<String, HashSet<String>> = HashMap::new();
    for (name, effects) in direct_effects_map {
        transitive.insert(name.clone(), effects.clone());
    }

    // Fixed-point iteration
    loop {
        let mut changed = false;

        for (func_name, callees) in call_graph {
            let mut new_effects: HashSet<String> = HashSet::new();

            // Collect effects from all callees
            for callee in callees {
                if let Some(callee_effects) = transitive.get(callee) {
                    for effect in callee_effects {
                        new_effects.insert(effect.clone());
                    }
                }
            }

            // Add new effects to this function's transitive set
            let func_effects = transitive.entry(func_name.clone()).or_default();
            for effect in new_effects {
                if func_effects.insert(effect) {
                    changed = true;
                }
            }
        }

        if !changed {
            break;
        }
    }

    transitive
}

/// Find the callee chain that introduces a transitive effect.
///
/// Returns the name of the immediate callee through which the effect propagates.
fn find_effect_source<'a>(
    func_name: &str,
    effect: &str,
    call_graph: &'a HashMap<String, HashSet<String>>,
    direct_effects_map: &HashMap<String, HashSet<String>>,
    transitive_effects: &HashMap<String, HashSet<String>>,
) -> Option<&'a String> {
    if let Some(callees) = call_graph.get(func_name) {
        for callee in callees {
            // Check if this callee has the effect (directly or transitively)
            if let Some(callee_effects) = transitive_effects.get(callee.as_str())
                && callee_effects.contains(effect)
            {
                return Some(callee);
            }
            if let Some(callee_direct) = direct_effects_map.get(callee.as_str())
                && callee_direct.contains(effect)
            {
                return Some(callee);
            }
        }
    }
    None
}

/// Check transitive effects for a function and emit diagnostics.
#[allow(clippy::too_many_arguments)]
fn check_transitive_effects(
    func_node: Node,
    func_name: &str,
    mode: &EffectMode,
    source: &str,
    file: &str,
    transitive_effects: &HashMap<String, HashSet<String>>,
    call_graph: &HashMap<String, HashSet<String>>,
    direct_effects_map: &HashMap<String, HashSet<String>>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let declared: Vec<String> = match mode {
        EffectMode::Declared(effects) => effects.clone(),
        EffectMode::Pure => Vec::new(),
        EffectMode::Inferred => return, // No transitive checking for inferred functions
    };

    let direct = direct_effects_map
        .get(func_name)
        .cloned()
        .unwrap_or_default();

    if let Some(transitive) = transitive_effects.get(func_name) {
        for effect in transitive {
            // Skip ambient effects — they don't need declaration
            if is_ambient_effect(effect) {
                continue;
            }

            // Skip effects that are already caught by direct checking
            if direct.contains(effect) {
                continue;
            }

            // Check if this transitive effect is declared
            let has_effect = declared.iter().any(|e| e.eq_ignore_ascii_case(effect));

            if !has_effect {
                // Find which callee introduces this effect
                let via = find_effect_source(
                    func_name,
                    effect,
                    call_graph,
                    direct_effects_map,
                    transitive_effects,
                );
                let via_name = via.map(|s| s.as_str()).unwrap_or("unknown");

                let (patch, _) = build_effect_patch(func_node, source, effect);

                let diag_code = if matches!(mode, EffectMode::Pure) {
                    "CAP_003"
                } else {
                    "CAP_001"
                };

                let message = if matches!(mode, EffectMode::Pure) {
                    format!(
                        "Function '{}' is marked @pure but transitively requires {{{}}} via '{}'",
                        func_name, effect, via_name
                    )
                } else {
                    format!(
                        "Function '{}' transitively requires {{{}}} via '{}' but does not declare it",
                        func_name, effect, via_name
                    )
                };

                diagnostics.push(Diagnostic {
                    code: diag_code.to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file, &func_node),
                    message,
                    context: format!(
                        "Function '{}' declares effects {{{}}}, but transitively requires {{{}}} through call to '{}'.",
                        func_name,
                        declared.join(", "),
                        effect,
                        via_name
                    ),
                    suggestions: vec![
                        Suggestion {
                            strategy: "escalate_capability".to_string(),
                            description: format!("Add {} to the function signature", effect),
                            confidence: None,
                            patch: Some(patch),
                        },
                    ],
                });
            }
        }
    }
}

/// Check a single function declaration for direct effect violations.
fn check_function(
    node: Node,
    mode: &EffectMode,
    source: &str,
    file: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let func_name = find_function_name(node, source).unwrap_or("<unknown>");

    let declared_effects = match mode {
        EffectMode::Declared(effects) => effects.clone(),
        EffectMode::Pure => Vec::new(),
        EffectMode::Inferred => return, // No direct checking for inferred functions
    };

    if let Some(body) = get_function_body(node) {
        check_node_for_effects(
            body,
            source,
            file,
            func_name,
            &declared_effects,
            node,
            diagnostics,
        );
    }
}

/// Find the function name from a function_def node.
fn find_function_name<'a>(node: Node<'a>, source: &'a str) -> Option<&'a str> {
    // The grammar defines: fn field('name', $._name)(...)
    // child_by_field_name("name") returns the function name.
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
/// Looks for the `effects` field in `fn name() -> {Http, Db} ReturnType`
fn extract_declared_effects(node: Node, source: &str) -> Vec<String> {
    let mut effects = Vec::new();

    // Read the effects field directly from function_def
    if let Some(effect_set) = node.child_by_field_name("effects") {
        collect_effects_from_type(effect_set, source, &mut effects);
        return effects;
    }

    effects
}

/// Recursively collect effect names from a type expression.
fn collect_effects_from_type(node: Node, source: &str, effects: &mut Vec<String>) {
    if node.kind() == "effect_set" {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "type_identifier"
                && let Ok(name) = child.utf8_text(source.as_bytes())
            {
                effects.push(name.to_string());
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
    check_node_for_effects_inner(
        node,
        source,
        file,
        func_name,
        declared_effects,
        func_node,
        false,
        diagnostics,
    );
}

#[allow(clippy::too_many_arguments)]
fn check_node_for_effects_inner(
    node: Node,
    source: &str,
    file: &str,
    func_name: &str,
    declared_effects: &[String],
    func_node: Node,
    in_restrict: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Check for effectful identifiers (calls ending in !)
    if node.kind() == "effect_identifier"
        && let Ok(call_name) = node.utf8_text(source.as_bytes())
    {
        check_effectful_call(
            node,
            call_name,
            source,
            file,
            func_name,
            declared_effects,
            func_node,
            in_restrict,
            diagnostics,
        );
    }

    // Check for field expressions (Log.info!)
    if node.kind() == "field_expression" {
        // field_expression: expr . id
        let obj = node.named_child(0);
        let field = node.named_child(1);

        if let (Some(obj_node), Some(field_node)) = (obj, field)
            && field_node.kind() == "effect_identifier"
            && let Ok(effect_name) = obj_node.utf8_text(source.as_bytes())
            && let Ok(method_name) = field_node.utf8_text(source.as_bytes())
        {
            let full_name = format!("{}.{}", effect_name, method_name);
            check_effectful_call(
                node,
                &full_name,
                source,
                file,
                func_name,
                declared_effects,
                func_node,
                in_restrict,
                diagnostics,
            );
            // Don't recurse to avoid double reporting
            return;
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
                in_restrict,
                diagnostics,
            );
        }
        // Don't recurse into qualified identifiers to avoid checking the inner effectful_identifier separately
        return;
    }

    // with_expression and handle_expression suppress their named effects
    if node.kind() == "with_expression" || node.kind() == "handle_expression" {
        return;
    }

    // restrict_expression narrows the allowed effects for its body
    if node.kind() == "restrict_expression" {
        let allowed = extract_restrict_effects(node, source);
        if let Some(body) = node.child_by_field_name("body") {
            check_node_for_effects_inner(
                body,
                source,
                file,
                func_name,
                &allowed,
                func_node,
                true,
                diagnostics,
            );
        }
        return;
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        check_node_for_effects_inner(
            child,
            source,
            file,
            func_name,
            declared_effects,
            func_node,
            in_restrict,
            diagnostics,
        );
    }
}

/// Extract allowed effects from a restrict_expression node.
///
/// `restrict(DB, Http) { ... }` → `["DB", "Http"]`
/// `restrict { ... }` → `[]` (enforce total purity)
fn extract_restrict_effects(node: Node, source: &str) -> Vec<String> {
    let mut effects = Vec::new();
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "type_identifier" {
            if let Ok(name) = child.utf8_text(source.as_bytes()) {
                effects.push(name.to_string());
            }
        }
    }
    effects
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
#[allow(clippy::too_many_arguments)]
fn check_effectful_call(
    node: Node,
    call_name: &str,
    source: &str,
    file: &str,
    func_name: &str,
    declared_effects: &[String],
    func_node: Node,
    in_restrict: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Infer the required effect from the call
    let required_effect = infer_required_effect(call_name);

    // Skip ambient effects — they don't need declaration
    if is_ambient_effect(&required_effect) {
        return;
    }

    // Check if the required effect is in the declared set
    let has_effect = declared_effects
        .iter()
        .any(|e| e.eq_ignore_ascii_case(&required_effect));

    if !has_effect {
        let start = node.start_position();

        if in_restrict {
            // CAP_002: Effect not permitted inside restrict block
            let allowed_str = if declared_effects.is_empty() {
                "no effects (pure)".to_string()
            } else {
                format!("{{{}}}", declared_effects.join(", "))
            };
            diagnostics.push(Diagnostic {
                code: "CAP_002".to_string(),
                severity: Severity::Error,
                location: Location::from_node(file, &node),
                message: format!(
                    "Effect '{}' is not permitted inside this restrict block",
                    required_effect
                ),
                context: format!(
                    "This restrict block allows {}, but '{}' requires {{{}}}.",
                    allowed_str,
                    call_name,
                    required_effect
                ),
                suggestions: vec![
                    Suggestion {
                        strategy: "remove_call".to_string(),
                        description: "Remove the effectful call from the restrict block".to_string(),
                        confidence: None,
                        patch: Some(Patch {
                            start_line: start.row + 1,
                            original_text: Some(call_name.to_string()),
                            replacement_text: None,
                            operation: Some("delete".to_string()),
                        }),
                    },
                ],
            });
        } else {
            // CAP_001: Undeclared effect in function signature
            let (patch, _original_text) = build_effect_patch(func_node, source, &required_effect);

            diagnostics.push(Diagnostic {
                code: "CAP_001".to_string(),
                severity: Severity::Error,
                location: Location::from_node(file, &node),
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
                        confidence: None,
                        patch: Some(patch),
                    },
                    Suggestion {
                        strategy: "remove_call".to_string(),
                        description: "Delete the unauthorized call".to_string(),
                        confidence: None,
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
            "Scope" | "Cell" => "Async".to_string(),
            "DateTime" => "Time".to_string(),
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
        "scope" => return "Async".to_string(),
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
