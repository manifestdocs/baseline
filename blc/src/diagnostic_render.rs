//! Source context rendering and fix suggestion helpers for diagnostics.
//!
//! BASEL-186: Renders diagnostics with source line and caret underlining.
//! BABEL-187: Levenshtein-based "did you mean?" suggestions.

use crate::diagnostics::{Diagnostic, Severity, Suggestion};
use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
// Source context rendering (BASEL-186)
// ---------------------------------------------------------------------------

/// Render a diagnostic with source context in Rust-compiler style.
///
/// Produces output like:
/// ```text
/// error[TYP_002]: Undefined variable `foo`
///   --> example.bl:5:10
///    |
///  5 |   let x = foo
///    |           ^^^
///   = Variable must be defined before use.
/// ```
pub fn render_diagnostic(diag: &Diagnostic, source: Option<&str>) -> String {
    let mut out = String::new();

    let severity_str = match diag.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "info",
    };
    out.push_str(&format!(
        "{}[{}]: {}\n",
        severity_str, diag.code, diag.message
    ));

    out.push_str(&format!(
        "  --> {}:{}:{}\n",
        diag.location.file, diag.location.line, diag.location.col
    ));

    if let Some(src) = source {
        if diag.location.line > 0 {
            let lines: Vec<&str> = src.lines().collect();
            let line_idx = diag.location.line - 1;

            if line_idx < lines.len() {
                let source_line = lines[line_idx];
                let line_num = diag.location.line;
                let gutter_width = format!("{}", line_num).len().max(2);

                out.push_str(&format!("{:>w$} |\n", "", w = gutter_width));
                out.push_str(&format!(
                    "{:>w$} | {}\n",
                    line_num,
                    source_line,
                    w = gutter_width
                ));

                let col = diag.location.col.saturating_sub(1);
                let span_len = compute_span_length(diag, line_idx);
                let underline = "^".repeat(span_len.max(1));

                let pad: String = source_line
                    .chars()
                    .take(col)
                    .map(|c| if c == '\t' { '\t' } else { ' ' })
                    .collect();
                out.push_str(&format!(
                    "{:>w$} | {}{}\n",
                    "",
                    pad,
                    underline,
                    w = gutter_width
                ));
            }
        }
    }

    if !diag.context.is_empty() {
        out.push_str(&format!("  = {}\n", diag.context));
    }

    for suggestion in &diag.suggestions {
        out.push_str(&format!("  help: {}\n", suggestion.description));
    }

    out
}

/// Compute the span length for the underline caret.
fn compute_span_length(diag: &Diagnostic, line_idx: usize) -> usize {
    if let (Some(end_line), Some(end_col)) = (diag.location.end_line, diag.location.end_col) {
        let end_line_idx = end_line.saturating_sub(1);
        if end_line_idx == line_idx && end_col > diag.location.col {
            return end_col - diag.location.col;
        }
    }
    1
}

/// Build a source context snippet for JSON output.
pub fn build_source_context(diag: &Diagnostic, source: &str) -> Option<SourceContext> {
    if diag.location.line == 0 {
        return None;
    }
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = diag.location.line - 1;
    if line_idx >= lines.len() {
        return None;
    }
    let source_line = lines[line_idx].to_string();
    let col = diag.location.col.saturating_sub(1);
    let span_len = compute_span_length(diag, line_idx);

    Some(SourceContext {
        line_number: diag.location.line,
        source_line,
        highlight_col: col,
        highlight_len: span_len.max(1),
    })
}

/// Source context for embedding in JSON diagnostics.
#[derive(Debug, Serialize, Deserialize)]
pub struct SourceContext {
    pub line_number: usize,
    pub source_line: String,
    pub highlight_col: usize,
    pub highlight_len: usize,
}

// ---------------------------------------------------------------------------
// Fix suggestions (BABEL-187)
// ---------------------------------------------------------------------------

/// Compute Levenshtein edit distance between two strings.
pub fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_len = a.len();
    let b_len = b.len();
    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }
    let mut prev: Vec<usize> = (0..=b_len).collect();
    let mut curr = vec![0; b_len + 1];
    for (i, ac) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, bc) in b.chars().enumerate() {
            let cost = if ac == bc { 0 } else { 1 };
            curr[j + 1] = (prev[j + 1] + 1)
                .min(curr[j] + 1)
                .min(prev[j] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[b_len]
}

/// Find the closest matching name from a list of candidates.
///
/// Returns `Some((name, distance))` if a close match is found.
pub fn find_similar_name<'a>(name: &str, candidates: &'a [String]) -> Option<(&'a str, usize)> {
    let max_dist = (name.len() / 2 + 1).min(3);
    let mut best: Option<(&str, usize)> = None;
    for candidate in candidates {
        let len_diff = if candidate.len() > name.len() {
            candidate.len() - name.len()
        } else {
            name.len() - candidate.len()
        };
        if len_diff > max_dist {
            continue;
        }
        let dist = levenshtein_distance(name, candidate);
        if dist > 0 && dist <= max_dist {
            if best.is_none() || dist < best.unwrap().1 {
                best = Some((candidate.as_str(), dist));
            }
        }
    }
    best
}

/// Build a "did you mean?" suggestion for an unknown identifier.
pub fn suggest_similar_identifier(name: &str, known_names: &[String]) -> Vec<Suggestion> {
    let mut suggestions = vec![];
    if let Some((similar, _)) = find_similar_name(name, known_names) {
        suggestions.push(Suggestion {
            strategy: "replace".to_string(),
            description: format!("Did you mean `{}`?", similar),
            confidence: Some(0.8),
            patch: None,
        });
    }
    suggestions
}

/// Build a type coercion suggestion for a type mismatch.
pub fn suggest_type_coercion(expected: &str, found: &str) -> Vec<Suggestion> {
    let mut suggestions = vec![];

    match (expected, found) {
        ("Int", "Float") => {
            suggestions.push(Suggestion {
                strategy: "convert".to_string(),
                description: "Use `Int.from_float(value)` to convert Float to Int".to_string(),
                confidence: Some(0.7),
                patch: None,
            });
        }
        ("Float", "Int") => {
            suggestions.push(Suggestion {
                strategy: "convert".to_string(),
                description: "Use `Float.from_int(value)` to convert Int to Float".to_string(),
                confidence: Some(0.7),
                patch: None,
            });
        }
        ("String", "Int") | ("String", "Float") | ("String", "Bool") => {
            suggestions.push(Suggestion {
                strategy: "convert".to_string(),
                description: format!(
                    "Use string interpolation `\"${{value}}\"` to convert {} to String",
                    found
                ),
                confidence: Some(0.8),
                patch: None,
            });
        }
        _ => {}
    }

    suggestions
}
