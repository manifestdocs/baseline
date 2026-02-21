//! Parsing module for Baseline source files.
//!
//! Uses tree-sitter-baseline to parse .bl files and convert
//! syntax errors into structured diagnostics.

use std::fs;
use std::path::Path;

use tree_sitter::Parser;
use tree_sitter_baseline::LANGUAGE;

use crate::diagnostics::{
    CheckResult, Diagnostic, Location, Severity, Suggestion, VerificationLevel,
};
use crate::resolver::ModuleLoader;

/// Parse a Baseline source file and return check results.
pub fn parse_file(path: &Path) -> Result<CheckResult, std::io::Error> {
    let source = fs::read_to_string(path)?;
    let file_name = path.display().to_string();

    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load Baseline grammar");

    let tree = parser.parse(&source, None).expect("Failed to parse");
    let root = tree.root_node();

    let mut diagnostics = Vec::new();

    // Collect syntax errors from tree-sitter
    collect_errors(root, &source, &file_name, &mut diagnostics);

    // Only run semantic analysis if there are no syntax errors
    if diagnostics.is_empty() {
        // Create a ModuleLoader for cross-module import resolution
        let base_dir = path.parent().map(|p| p.to_path_buf());
        let mut loader = match base_dir {
            Some(dir) => ModuleLoader::with_base_dir(dir),
            None => ModuleLoader::new(),
        };

        // Run type checking pass with import support
        let root_node = tree.root_node();
        let type_diagnostics = crate::analysis::check_types_with_loader(
            &root_node,
            &source,
            &file_name,
            Some(&mut loader),
        );
        diagnostics.extend(type_diagnostics);

        // Run effect checking pass
        let effect_diagnostics = crate::analysis::check_effects(&tree, &source, &file_name);
        diagnostics.extend(effect_diagnostics);

        // Run refinement checking pass
        let refinement_diagnostics = crate::analysis::check_refinements(&tree, &source, &file_name);
        diagnostics.extend(refinement_diagnostics);

        // Run closure cycle detection (W_CYCLE_001)
        let root_node = tree.root_node();
        let cycle_diagnostics = crate::analysis::check_closure_cycles(&root_node, &source, &file_name);
        diagnostics.extend(cycle_diagnostics);
    }

    let has_errors = diagnostics.iter().any(|d| d.severity == Severity::Error);
    let status = if has_errors {
        "failure".to_string()
    } else {
        "success".to_string()
    };

    // Default to refinements level (what we actually check)
    let mut result = CheckResult::new(VerificationLevel::Refinements);
    result.status = status;
    result.diagnostics = diagnostics;
    Ok(result)
}

/// Parse a source string and return check results (used by tests and API).
pub fn parse_source(source: &str, file_name: &str) -> CheckResult {
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load Baseline grammar");

    let tree = parser.parse(source, None).expect("Failed to parse");
    let root = tree.root_node();

    let mut diagnostics = Vec::new();

    collect_errors(root, source, file_name, &mut diagnostics);

    if diagnostics.is_empty() {
        // No base directory for string-based parsing — imports won't resolve,
        // but that's fine for unit tests that don't use imports.
        let root_node = tree.root_node();
        let type_diagnostics = crate::analysis::check_types(&root_node, source, file_name);
        diagnostics.extend(type_diagnostics);

        let effect_diagnostics = crate::analysis::check_effects(&tree, source, file_name);
        diagnostics.extend(effect_diagnostics);

        let refinement_diagnostics = crate::analysis::check_refinements(&tree, source, file_name);
        diagnostics.extend(refinement_diagnostics);

        // Run closure cycle detection (W_CYCLE_001)
        let root_node = tree.root_node();
        let cycle_diagnostics = crate::analysis::check_closure_cycles(&root_node, source, file_name);
        diagnostics.extend(cycle_diagnostics);
    }

    let has_errors = diagnostics.iter().any(|d| d.severity == Severity::Error);
    let status = if has_errors {
        "failure".to_string()
    } else {
        "success".to_string()
    };

    // Default to refinements level (what we actually check)
    let mut result = CheckResult::new(VerificationLevel::Refinements);
    result.status = status;
    result.diagnostics = diagnostics;
    result
}

/// Parse an integer literal that may use hex (0x), binary (0b), octal (0o),
/// or decimal notation, with optional underscore separators.
pub fn parse_int_literal(text: &str) -> Option<i64> {
    let s = text.replace('_', "");
    if s.starts_with("0x") || s.starts_with("0X") {
        i64::from_str_radix(&s[2..], 16).ok()
    } else if s.starts_with("0b") || s.starts_with("0B") {
        i64::from_str_radix(&s[2..], 2).ok()
    } else if s.starts_with("0o") || s.starts_with("0O") {
        i64::from_str_radix(&s[2..], 8).ok()
    } else {
        s.parse::<i64>().ok()
    }
}

/// Parse a source string with file path context for cross-file import resolution.
/// Like `parse_file()` but operates on in-memory source text (used by the LSP).
pub fn parse_source_with_path(source: &str, file_path: &Path) -> CheckResult {
    let file_name = file_path.display().to_string();

    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load Baseline grammar");

    let tree = parser.parse(source, None).expect("Failed to parse");
    let root = tree.root_node();

    let mut diagnostics = Vec::new();

    collect_errors(root, source, &file_name, &mut diagnostics);

    if diagnostics.is_empty() {
        let base_dir = file_path.parent().map(|p| p.to_path_buf());
        let mut loader = match base_dir {
            Some(dir) => ModuleLoader::with_base_dir(dir),
            None => ModuleLoader::new(),
        };

        let root_node = tree.root_node();
        let type_diagnostics = crate::analysis::check_types_with_loader(
            &root_node,
            source,
            &file_name,
            Some(&mut loader),
        );
        diagnostics.extend(type_diagnostics);

        let effect_diagnostics = crate::analysis::check_effects(&tree, source, &file_name);
        diagnostics.extend(effect_diagnostics);

        let refinement_diagnostics = crate::analysis::check_refinements(&tree, source, &file_name);
        diagnostics.extend(refinement_diagnostics);

        // Run closure cycle detection (W_CYCLE_001)
        let root_node = tree.root_node();
        let cycle_diagnostics = crate::analysis::check_closure_cycles(&root_node, source, &file_name);
        diagnostics.extend(cycle_diagnostics);
    }

    let has_errors = diagnostics.iter().any(|d| d.severity == Severity::Error);
    let status = if has_errors {
        "failure".to_string()
    } else {
        "success".to_string()
    };

    let mut result = CheckResult::new(VerificationLevel::Refinements);
    result.status = status;
    result.diagnostics = diagnostics;
    result
}

/// Recursively collect ERROR and MISSING nodes from the syntax tree.
pub(crate) fn collect_errors(
    node: tree_sitter::Node,
    source: &str,
    file: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if node.is_error() {
        let text = node.utf8_text(source.as_bytes()).unwrap_or("<unknown>");

        // Check if this error follows a match expression (common gotcha:
        // match arms are greedy and consume tokens that look like patterns).
        let after_match = has_preceding_match(&node);

        // Check for common gotcha: @test { ... } (braces not allowed)
        let after_test_annotation = (text.starts_with('{') || text.starts_with('}'))
            && has_preceding_test_annotation(&node);

        let (context, suggestions) = if after_test_annotation {
            (
                "`@test` sections do not use braces. Remove the `{` and `}` — just write \
                 `@test` followed by `test \"name\" = expr` lines."
                    .to_string(),
                vec![Suggestion {
                    strategy: "remove".to_string(),
                    description: "Remove `{` and `}` around the test block".to_string(),
                    confidence: None,
                    patch: None,
                }],
            )
        } else if after_match {
            (
                "Match arms are greedy — the parser consumed this as part of the match. \
                 Use `let _ = expr` for the next statement, or make the match the last expression in the block."
                    .to_string(),
                vec![Suggestion {
                    strategy: "rewrite".to_string(),
                    description:
                        "Wrap the following expression: `let _ = <expr>` (statements after match must start with `let`)"
                            .to_string(),
                    confidence: None,
                    patch: None,
                }],
            )
        } else {
            (
                "The parser encountered unexpected tokens.".to_string(),
                vec![],
            )
        };

        diagnostics.push(Diagnostic {
            code: "SYN_001".to_string(),
            severity: Severity::Error,
            location: Location::from_node(file, &node),
            message: format!(
                "Syntax error: unexpected `{}`",
                text.chars().take(20).collect::<String>()
            ),
            context,
            suggestions,
        });
    } else if node.is_missing() {
        diagnostics.push(Diagnostic {
            code: "SYN_002".to_string(),
            severity: Severity::Error,
            location: Location::from_node(file, &node),
            message: format!("Missing expected: {}", node.kind()),
            context: "A required syntax element is missing.".to_string(),
            suggestions: vec![Suggestion {
                strategy: "insert".to_string(),
                description: format!("Add missing {}", node.kind()),
                confidence: None,
                patch: None,
            }],
        });
    }

    // Recurse into children
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_errors(child, source, file, diagnostics);
    }
}

/// Check whether an ERROR node appears as a sibling after a `match_expression`.
/// This detects the common gotcha where match arms greedily consume tokens
/// that look like patterns (literals, identifiers, tuples).
fn has_preceding_match(node: &tree_sitter::Node) -> bool {
    if let Some(parent) = node.parent() {
        let mut cursor = parent.walk();
        let mut saw_match = false;
        for child in parent.children(&mut cursor) {
            if child.id() == node.id() {
                return saw_match;
            }
            // A match_expression or a let binding whose value is a match
            if child.kind() == "match_expression" {
                saw_match = true;
            } else if child.kind() == "let_declaration" {
                // Check if the let's value expression is a match
                let mut inner = child.walk();
                for grandchild in child.children(&mut inner) {
                    if grandchild.kind() == "match_expression" {
                        saw_match = true;
                    }
                }
            } else {
                // Any non-match statement resets — the error isn't related to the match
                saw_match = false;
            }
        }
    }
    false
}

/// Check whether an ERROR node containing `{` is inside or immediately after a `@test` section.
/// Tree-sitter parses `@test {` as: test_section → ERROR("{"), so the ERROR is a child of test_section.
/// The closing `}` is a sibling ERROR after test_section.
fn has_preceding_test_annotation(node: &tree_sitter::Node) -> bool {
    // Case 1: ERROR is a child of test_section (the opening `{`)
    if let Some(parent) = node.parent() {
        if parent.kind() == "test_section" {
            return true;
        }
    }
    // Case 2: ERROR is a sibling after test_section (the closing `}`)
    if let Some(prev) = node.prev_named_sibling() {
        if prev.kind() == "test_section" {
            return true;
        }
    }
    false
}
