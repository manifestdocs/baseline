use serde::Serialize;

use crate::diagnostics::Location;

// ---------------------------------------------------------------------------
// Shared result types used by vm::test_runner
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize)]
pub struct TestResult {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub function: Option<String>,
    pub status: TestStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    pub location: Location,
}

#[derive(Debug, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum TestStatus {
    Pass,
    Fail,
    #[serde(rename = "skipped")]
    Skip,
}

#[derive(Debug, Serialize)]
pub struct TestSuiteResult {
    pub status: String,
    pub tests: Vec<TestResult>,
    pub summary: TestSummary,
}

#[derive(Debug, Serialize)]
pub struct TestSummary {
    pub total: usize,
    pub passed: usize,
    pub failed: usize,
    #[serde(skip_serializing_if = "is_zero")]
    pub skipped: usize,
}

fn is_zero(n: &usize) -> bool {
    *n == 0
}
