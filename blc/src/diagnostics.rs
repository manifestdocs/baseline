//! Diagnostic types for structured error reporting.
//!
//! This module implements the Agent Protocol JSON schema as defined in
//! the Technical Specification §4.2 and Language Specification v0.2 Appendix A.

use serde::{Deserialize, Serialize};

/// Diagnostic severity level.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Error,
    Warning,
    Info,
}

/// Verification level for compiler output.
///
/// Per spec §8.5, every compiler response must include the verification level
/// to enable LLM agents to reason about what has been verified.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum VerificationLevel {
    /// Type inference and exhaustiveness only (~ms)
    Types,
    /// Types + refinement constraints (~100ms)
    Refinements,
    /// Types + refinements + all specs + SMT (~seconds)
    Full,
    /// Types only, specs unchecked (escape hatch)
    Skip,
}

impl Default for VerificationLevel {
    fn default() -> Self {
        VerificationLevel::Refinements
    }
}

impl std::fmt::Display for VerificationLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VerificationLevel::Types => write!(f, "types"),
            VerificationLevel::Refinements => write!(f, "refinements"),
            VerificationLevel::Full => write!(f, "full"),
            VerificationLevel::Skip => write!(f, "skip"),
        }
    }
}

impl std::str::FromStr for VerificationLevel {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "types" => Ok(VerificationLevel::Types),
            "refinements" => Ok(VerificationLevel::Refinements),
            "full" => Ok(VerificationLevel::Full),
            "skip" => Ok(VerificationLevel::Skip),
            _ => Err(format!(
                "Unknown verification level: {}. Valid: types, refinements, full, skip",
                s
            )),
        }
    }
}

/// The result of checking a Baseline source file.
///
/// Per spec §8.5 and Appendix A, includes verification_level in every response.
#[derive(Debug, Serialize, Deserialize)]
pub struct CheckResult {
    pub status: String,
    /// The verification level that was run
    pub verification_level: VerificationLevel,
    /// What checks were actually performed
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub checked: Vec<String>,
    /// What checks were NOT performed (higher levels)
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub unchecked: Vec<String>,
    pub diagnostics: Vec<Diagnostic>,
}

impl CheckResult {
    /// Create a new CheckResult with the given level
    pub fn new(level: VerificationLevel) -> Self {
        let (checked, unchecked) = match level {
            VerificationLevel::Types => (
                vec!["types".to_string()],
                vec![
                    "refinements".to_string(),
                    "specs".to_string(),
                    "smt".to_string(),
                ],
            ),
            VerificationLevel::Refinements => (
                vec!["types".to_string(), "refinements".to_string()],
                vec!["specs".to_string(), "smt".to_string()],
            ),
            VerificationLevel::Full => (
                vec![
                    "types".to_string(),
                    "refinements".to_string(),
                    "specs".to_string(),
                    "smt".to_string(),
                ],
                vec![],
            ),
            VerificationLevel::Skip => (
                vec!["types".to_string()],
                vec![
                    "refinements".to_string(),
                    "specs".to_string(),
                    "smt".to_string(),
                ],
            ),
        };
        Self {
            status: "ok".to_string(),
            verification_level: level,
            checked,
            unchecked,
            diagnostics: Vec::new(),
        }
    }

    /// Set status to failure
    pub fn fail(mut self) -> Self {
        self.status = "failure".to_string();
        self
    }

    /// Add diagnostics
    pub fn with_diagnostics(mut self, diags: Vec<Diagnostic>) -> Self {
        self.diagnostics = diags;
        self
    }
}

/// A single diagnostic message.
#[derive(Debug, Serialize, Deserialize)]
pub struct Diagnostic {
    pub code: String,
    pub severity: Severity,
    pub location: Location,
    pub message: String,
    pub context: String,
    pub suggestions: Vec<Suggestion>,
}

/// Source location for a diagnostic.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Location {
    pub file: String,
    pub line: usize,
    pub col: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_line: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_col: Option<usize>,
}

impl Location {
    /// Build a Location from a tree-sitter node.
    pub fn from_node(file: &str, node: &tree_sitter::Node) -> Self {
        let start = node.start_position();
        let end = node.end_position();
        Location {
            file: file.to_string(),
            line: start.row + 1,
            col: start.column + 1,
            end_line: Some(end.row + 1),
            end_col: Some(end.column + 1),
        }
    }
}

/// A suggested fix for a diagnostic.
///
/// Per spec Appendix A, includes confidence score for LLM repair loops.
#[derive(Debug, Serialize, Deserialize)]
pub struct Suggestion {
    pub strategy: String,
    pub description: String,
    /// Confidence score (0.0 to 1.0) for ranking suggestions in LLM repair loops
    #[serde(skip_serializing_if = "Option::is_none")]
    pub confidence: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub patch: Option<Patch>,
}

/// A patch that can be applied to fix an issue.
#[derive(Debug, Serialize, Deserialize)]
pub struct Patch {
    pub start_line: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub original_text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub replacement_text: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operation: Option<String>,
}
