//! CST-based closure cycle detection (W_CYCLE_001).
//!
//! Detects mutually recursive closures at the syntax level without requiring
//! a full IR lowering pass, so warnings appear during `blc check`.
//!
//! A W_CYCLE_001 warning fires when two `let` bindings in the same `block`
//! both bind lambda values and each lambda's body references the other's name
//! as a free variable (i.e. a mutual capture cycle).
//!
//! This is the only reference-cycle-forming pattern possible in today's Baseline
//! (no mutable references, no lazy thunks).

use std::collections::HashSet;

use tree_sitter::Node;

use crate::diagnostics::{Diagnostic, Location, Severity, Suggestion};

/// Walk the CST and emit W_CYCLE_001 warnings for any mutually capturing
/// closure pair in the same block.
pub fn check_closure_cycles(root: &Node, source: &str, file: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let top_level = collect_top_level_names(root, source);
    detect_cycles_in_node(root, source, file, &top_level, &mut diagnostics);
    diagnostics
}

// ---------------------------------------------------------------------------
// Core detection
// ---------------------------------------------------------------------------

fn detect_cycles_in_node(
    node: &Node,
    source: &str,
    file: &str,
    top_level: &HashSet<String>,
    out: &mut Vec<Diagnostic>,
) {
    // Only `block` nodes contain `let_binding` sequences (not source_file — those are fn defs)
    if node.kind() == "block" {
        check_block_for_cycles(node, source, file, top_level, out);
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        detect_cycles_in_node(&child, source, file, top_level, out);
    }
}

/// Scan a `block` node for mutual closure captures.
fn check_block_for_cycles(
    block: &Node,
    source: &str,
    file: &str,
    top_level: &HashSet<String>,
    out: &mut Vec<Diagnostic>,
) {
    // Collect (binding_name, name_node, lambda_node) for each let-bound lambda
    struct LambdaLet<'a> {
        name: String,
        name_node: Node<'a>,
        lambda: Node<'a>,
    }

    let mut lambda_lets: Vec<LambdaLet<'_>> = Vec::new();

    // Also track names bound by any let in this block (for top-level exclusion extension)
    let mut block_bound: HashSet<String> = top_level.clone();

    let mut cursor = block.walk();
    for child in block.children(&mut cursor) {
        if child.kind() != "let_binding" {
            continue;
        }
        // Grammar: let_binding = 'let' _pattern type_annotation? '=' _expression
        // First named child is the pattern; last named child is the expression
        let child_count = child.named_child_count();
        if child_count < 2 {
            continue;
        }
        let pattern = match child.named_child(0) {
            Some(n) => n,
            None => continue,
        };
        let value = match child.named_child(child_count - 1) {
            Some(n) => n,
            None => continue,
        };

        // Only handle simple identifier patterns for now
        if pattern.kind() != "identifier" {
            continue;
        }
        let name = match pattern.utf8_text(source.as_bytes()) {
            Ok(n) => n.to_string(),
            Err(_) => continue,
        };

        // Make this binding available to subsequent lambdas in the block
        block_bound.insert(name.clone());

        // Only emit if the value is a lambda
        if value.kind() == "lambda" {
            lambda_lets.push(LambdaLet { name, name_node: pattern, lambda: value });
        }
    }

    if lambda_lets.len() < 2 {
        return;
    }

    // Compute free variables for each lambda body, excluding its own params and top_level
    struct LambdaInfo<'a> {
        name: String,
        name_node: Node<'a>,
        free_vars: HashSet<String>,
    }

    let infos: Vec<LambdaInfo<'_>> = lambda_lets
        .iter()
        .map(|ll| {
            let params = collect_lambda_params(&ll.lambda, source);
            // Body: last named child of the lambda
            let child_count = ll.lambda.named_child_count();
            let free_vars = if child_count > 0 {
                if let Some(body) = ll.lambda.named_child(child_count - 1) {
                    collect_free_identifiers(&body, source, &params, top_level)
                } else {
                    HashSet::new()
                }
            } else {
                HashSet::new()
            };
            LambdaInfo {
                name: ll.name.clone(),
                name_node: ll.name_node,
                free_vars,
            }
        })
        .collect();

    // Check every pair for mutual capture
    for i in 0..infos.len() {
        for j in (i + 1)..infos.len() {
            let captures_b = infos[i].free_vars.contains(&infos[j].name);
            let captures_a = infos[j].free_vars.contains(&infos[i].name);
            if captures_a && captures_b {
                let diag = make_cycle_diagnostic(
                    &infos[i].name,
                    &infos[i].name_node,
                    &infos[j].name,
                    &infos[j].name_node,
                    file,
                );
                out.push(diag);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Free variable collection
// ---------------------------------------------------------------------------

/// Collect all identifiers reachable from `node` that are not in `bound` or `top_level`.
fn collect_free_identifiers(
    node: &Node,
    source: &str,
    bound: &HashSet<String>,
    top_level: &HashSet<String>,
) -> HashSet<String> {
    let mut result = HashSet::new();
    collect_identifiers_inner(node, source, bound, top_level, &mut result);
    result
}

fn collect_identifiers_inner(
    node: &Node,
    source: &str,
    bound: &HashSet<String>,
    top_level: &HashSet<String>,
    out: &mut HashSet<String>,
) {
    if node.kind() == "identifier" {
        if let Ok(name) = node.utf8_text(source.as_bytes()) {
            if !bound.contains(name) && !top_level.contains(name) {
                out.insert(name.to_string());
            }
        }
        return;
    }

    // Don't cross into nested lambdas — they bind their own params
    if node.kind() == "lambda" {
        let params = collect_lambda_params(node, source);
        let mut inner_bound = bound.clone();
        inner_bound.extend(params);
        let child_count = node.named_child_count();
        if let Some(body) = node.named_child(child_count.saturating_sub(1)) {
            collect_identifiers_inner(&body, source, &inner_bound, top_level, out);
        }
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_identifiers_inner(&child, source, bound, top_level, out);
    }
}

/// Collect the parameter names of a lambda (all named children except the body).
fn collect_lambda_params(lambda: &Node, source: &str) -> HashSet<String> {
    let mut params = HashSet::new();
    let child_count = lambda.named_child_count();
    // All named children except the last (body) are parameter patterns
    for i in 0..child_count.saturating_sub(1) {
        if let Some(child) = lambda.named_child(i) {
            collect_pattern_names_from_node(&child, source, &mut params);
        }
    }
    params
}

/// Walk a pattern node and collect bound names.
fn collect_pattern_names_from_node(node: &Node, source: &str, out: &mut HashSet<String>) {
    if node.kind() == "identifier" {
        if let Ok(name) = node.utf8_text(source.as_bytes()) {
            out.insert(name.to_string());
        }
        return;
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_pattern_names_from_node(&child, source, out);
    }
}

// ---------------------------------------------------------------------------
// Top-level name collection
// ---------------------------------------------------------------------------

/// Collect the names of all top-level `fn` definitions in the module.
/// These are never considered free variables (they are global, not captured).
fn collect_top_level_names(root: &Node, source: &str) -> HashSet<String> {
    let mut names = HashSet::new();
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if child.kind() == "function_def" {
            // Grammar: 'fn' name type_params? '(' ... ')' ...
            // field('name', $._name) is the second token; find first identifier child
            if let Some(name_node) = child
                .named_children(&mut child.walk())
                .find(|n| n.kind() == "identifier")
            {
                if let Ok(name) = name_node.utf8_text(source.as_bytes()) {
                    names.insert(name.to_string());
                }
            }
        }
    }
    names
}

// ---------------------------------------------------------------------------
// Diagnostic construction
// ---------------------------------------------------------------------------

fn make_cycle_diagnostic(
    name_a: &str,
    node_a: &Node,
    name_b: &str,
    node_b: &Node,
    file: &str,
) -> Diagnostic {
    let loc_a = Location::from_node(file, node_a);
    let loc_b = Location::from_node(file, node_b);
    let line_a = loc_a.line;
    let line_b = loc_b.line;
    Diagnostic {
        code: "W_CYCLE_001".to_string(),
        severity: Severity::Warning,
        location: loc_b,
        message: format!(
            "closures `{}` and `{}` capture each other and may form a reference cycle",
            name_a, name_b,
        ),
        context: format!(
            "`{}` (line {}) and `{}` (line {}) capture each other. \
             Perceus reference counting cannot reclaim mutually-referencing closures. \
             If both are scoped to a request handler or `scope!` block, \
             the arena frees them automatically — no action needed. \
             For long-lived closures, restructure to break the mutual capture.",
            name_a, line_a, name_b, line_b,
        ),
        suggestions: vec![
            Suggestion {
                strategy: "scope".to_string(),
                description: format!(
                    "wrap `{}` and `{}` in a `scope!` block — \
                     the arena frees both when the scope exits",
                    name_a, name_b,
                ),
                confidence: Some(0.7),
                patch: None,
            },
            Suggestion {
                strategy: "restructure".to_string(),
                description: "break the mutual capture by passing one closure \
                              as a parameter rather than closing over it"
                    .to_string(),
                confidence: Some(0.6),
                patch: None,
            },
        ],
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_and_check(source: &str) -> Vec<Diagnostic> {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .expect("Failed to load grammar");
        let tree = parser.parse(source, None).expect("Failed to parse");
        let root = tree.root_node();
        check_closure_cycles(&root, source, "test.bl")
    }

    #[test]
    fn no_warning_for_simple_functions() {
        // No mutual capture — no warning
        let diags = parse_and_check(
            "fn test() = {\n  let f = |x| x + 1\n  let g = |x| x * 2\n  f(1)\n}",
        );
        assert!(diags.is_empty(), "expected no warnings, got: {:?}", diags);
    }

    #[test]
    fn no_warning_for_one_sided_capture() {
        // g captures f, but f does not capture g — one-directional, no cycle
        let diags = parse_and_check(
            "fn test() = {\n  let f = |x| x + 1\n  let g = |x| f(x)\n  g(1)\n}",
        );
        assert!(diags.is_empty(), "expected no warnings, got: {:?}", diags);
    }

    #[test]
    fn warns_for_mutual_capture() {
        // f captures g AND g captures f → W_CYCLE_001
        let diags = parse_and_check(
            "fn test() = {\n  let f = |x| g(x)\n  let g = |x| f(x)\n  f(0)\n}",
        );
        assert_eq!(diags.len(), 1, "expected 1 warning, got: {:?}", diags);
        assert_eq!(diags[0].code, "W_CYCLE_001");
        assert!(diags[0].message.contains('f'));
        assert!(diags[0].message.contains('g'));
        assert_eq!(diags[0].severity, Severity::Warning);
    }

    #[test]
    fn no_warning_for_top_level_fn_calls() {
        // top_fn is a top-level fn def: not counted as a free variable
        let diags = parse_and_check(
            "fn top_fn(x: Int) -> Int = x + 1\nfn test() = {\n  let handler = |x| top_fn(x)\n  handler(1)\n}",
        );
        assert!(diags.is_empty(), "top-level fn calls should not trigger warning");
    }
}
