//! Diagnostic types for structured error reporting.
//!
//! This module implements the Agent Protocol JSON schema as defined in
//! the Technical Specification §4.2.

use serde::{Deserialize, Serialize};

/// The result of checking a Baseline source file.
#[derive(Debug, Serialize, Deserialize)]
pub struct CheckResult {
    pub status: String,
    pub diagnostics: Vec<Diagnostic>,
}

/// A single diagnostic message.
#[derive(Debug, Serialize, Deserialize)]
pub struct Diagnostic {
    pub code: String,
    pub severity: String,
    pub location: Location,
    pub message: String,
    pub context: String,
    pub suggestions: Vec<Suggestion>,
}

/// Source location for a diagnostic.
#[derive(Debug, Serialize, Deserialize)]
pub struct Location {
    pub file: String,
    pub line: usize,
    pub col: usize,
}

/// A suggested fix for a diagnostic.
#[derive(Debug, Serialize, Deserialize)]
pub struct Suggestion {
    pub strategy: String,
    pub description: String,
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
