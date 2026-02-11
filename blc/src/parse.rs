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

/// Recursively collect ERROR and MISSING nodes from the syntax tree.
fn collect_errors(
    node: tree_sitter::Node,
    source: &str,
    file: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if node.is_error() {
        let text = node.utf8_text(source.as_bytes()).unwrap_or("<unknown>");

        diagnostics.push(Diagnostic {
            code: "SYN_001".to_string(),
            severity: Severity::Error,
            location: Location::from_node(file, &node),
            message: format!(
                "Syntax error: unexpected `{}`",
                text.chars().take(20).collect::<String>()
            ),
            context: "The parser encountered unexpected tokens.".to_string(),
            suggestions: vec![],
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
