//! Parsing module for Baseline source files.
//!
//! Uses tree-sitter-baseline to parse .bl files and convert
//! syntax errors into structured diagnostics.

use std::fs;
use std::path::Path;

use tree_sitter::Parser;
use tree_sitter_baseline::LANGUAGE;

use crate::diagnostics::{CheckResult, Diagnostic, Location, Suggestion};

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
        // Run type checking pass (P0)
        let root_node = tree.root_node();
        let type_diagnostics = crate::analysis::check_types(&root_node, &source, &file_name);
        diagnostics.extend(type_diagnostics);

        // Run effect checking pass
        let effect_diagnostics = crate::analysis::check_effects(&tree, &source, &file_name);
        diagnostics.extend(effect_diagnostics);
        
        // Run refinement checking pass
        let refinement_diagnostics = crate::analysis::check_refinements(&tree, &source, &file_name);
        diagnostics.extend(refinement_diagnostics);
    }

    let status = if diagnostics.is_empty() {
        "success".to_string()
    } else {
        "failure".to_string()
    };

    Ok(CheckResult { status, diagnostics })
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
        let root_node = tree.root_node();
        let type_diagnostics = crate::analysis::check_types(&root_node, source, file_name);
        diagnostics.extend(type_diagnostics);

        let effect_diagnostics = crate::analysis::check_effects(&tree, source, file_name);
        diagnostics.extend(effect_diagnostics);

        let refinement_diagnostics = crate::analysis::check_refinements(&tree, source, file_name);
        diagnostics.extend(refinement_diagnostics);
    }

    let status = if diagnostics.is_empty() {
        "success".to_string()
    } else {
        "failure".to_string()
    };

    CheckResult { status, diagnostics }
}

/// Recursively collect ERROR and MISSING nodes from the syntax tree.
fn collect_errors(
    node: tree_sitter::Node,
    source: &str,
    file: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if node.is_error() {
        let start = node.start_position();
        let text = node.utf8_text(source.as_bytes()).unwrap_or("<unknown>");

        diagnostics.push(Diagnostic {
            code: "SYN_001".to_string(),
            severity: "error".to_string(),
            location: Location {
                file: file.to_string(),
                line: start.row + 1,
                col: start.column + 1,
            },
            message: format!("Syntax error: unexpected `{}`", text.chars().take(20).collect::<String>()),
            context: "The parser encountered unexpected tokens.".to_string(),
            suggestions: vec![],
        });
    } else if node.is_missing() {
        let start = node.start_position();

        diagnostics.push(Diagnostic {
            code: "SYN_002".to_string(),
            severity: "error".to_string(),
            location: Location {
                file: file.to_string(),
                line: start.row + 1,
                col: start.column + 1,
            },
            message: format!("Missing expected: {}", node.kind()),
            context: "A required syntax element is missing.".to_string(),
            suggestions: vec![Suggestion {
                strategy: "insert".to_string(),
                description: format!("Add missing {}", node.kind()),
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
