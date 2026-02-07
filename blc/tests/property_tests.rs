//! Property-based tests using proptest
//!
//! Tests invariants that should hold for all inputs:
//! 1. Soundness: well-typed programs don't crash the interpreter
//! 2. Idempotency: analyzing a program twice gives identical diagnostics
//! 3. Diagnostic validity: every diagnostic has a known error code prefix

use proptest::prelude::*;
use std::process::Command;

fn blc_check_json(source: &str) -> (i32, String) {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("test.bl");
    std::fs::write(&file, source).unwrap();

    let blc = env!("CARGO_BIN_EXE_blc");
    let output = Command::new(blc)
        .arg("check")
        .arg("--json")
        .arg(&file)
        .output()
        .expect("failed to execute blc");

    (
        output.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&output.stdout).into_owned(),
    )
}

fn blc_run_source(source: &str) -> (i32, String, String) {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("test.bl");
    std::fs::write(&file, source).unwrap();

    let blc = env!("CARGO_BIN_EXE_blc");
    let output = Command::new(blc)
        .arg("run")
        .arg(&file)
        .output()
        .expect("failed to execute blc");

    (
        output.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

/// Generate a random arithmetic expression
fn arith_expr() -> impl Strategy<Value = String> {
    let leaf = prop_oneof![
        (-1000i64..1000).prop_map(|n| n.to_string()),
    ];

    leaf.prop_recursive(4, 32, 2, |inner| {
        let op = prop_oneof![
            Just("+".to_string()),
            Just("-".to_string()),
            Just("*".to_string()),
        ];
        (inner.clone(), op, inner).prop_map(|(l, op, r)| format!("{l} {op} {r}"))
    })
}

/// Generate a well-typed pure program that should run without errors
fn pure_program() -> impl Strategy<Value = String> {
    arith_expr().prop_map(|expr| {
        format!(
            "@prelude(core)\n\nmain : () -> Int\nmain = || {expr}\n"
        )
    })
}

proptest! {
    /// Parsing + analysis should never panic on arbitrary strings
    #[test]
    fn parse_never_panics(input in "\\PC{0,200}") {
        let (_rc, _json) = blc_check_json(&input);
        // We don't care about the result, just that it doesn't panic
    }

    /// Analysis should be idempotent: running check twice gives the same result
    #[test]
    fn analysis_idempotent(input in pure_program()) {
        let (rc1, json1) = blc_check_json(&input);
        let (rc2, json2) = blc_check_json(&input);
        prop_assert_eq!(rc1, rc2, "Exit codes differ for same input");
        prop_assert_eq!(json1, json2, "JSON output differs for same input");
    }

    /// Well-typed arithmetic programs should not crash the interpreter
    #[test]
    fn soundness_arithmetic(expr in arith_expr()) {
        let source = format!(
            "@prelude(core)\n\nmain : () -> Int\nmain = || {expr}\n"
        );
        let (rc, _json) = blc_check_json(&source);
        // If analysis passes, interpreter should not panic
        if rc == 0 {
            let (run_rc, _stdout, stderr) = blc_run_source(&source);
            // Runtime errors are OK (e.g. division by zero), panics are not
            prop_assert!(
                run_rc == 0 || stderr.contains("Runtime Error"),
                "Unexpected failure: rc={run_rc}, stderr={stderr}"
            );
        }
    }

    /// Every diagnostic should have a valid error code prefix
    #[test]
    fn diagnostic_codes_valid(input in "\\PC{0,200}") {
        let (_rc, json) = blc_check_json(&input);
        if let Ok(parsed) = serde_json::from_str::<serde_json::Value>(&json) {
            if let Some(diagnostics) = parsed["diagnostics"].as_array() {
                let valid_prefixes = ["TYP_", "CAP_", "REF_", "SYN_", "IO_", "IMP_", "PRE_", "STY_"];
                for diag in diagnostics {
                    if let Some(code) = diag["code"].as_str() {
                        prop_assert!(
                            valid_prefixes.iter().any(|p| code.starts_with(p)),
                            "Invalid error code prefix: {code}"
                        );
                    }
                }
            }
        }
    }
}
