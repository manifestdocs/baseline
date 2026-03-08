//! Conformance test runner
//!
//! Auto-discovers `.bl` files under `tests/conformance/` and runs them through
//! the appropriate blc subcommand based on file type:
//!
//! - Files with inline `test` statements → `blc test <file>`
//! - Files named `errors_*` → `blc check <file>`, verify expected error codes
//! - Files with `main!` → `blc check <file>` (check-pass)
//! - Helper files (e.g., util.bl) → skipped

use std::path::{Path, PathBuf};
use std::process::Command;

struct BlcOutput {
    exit_code: i32,
    signal_killed: bool,
    stdout: String,
    stderr: String,
}

fn blc_cmd(args: &[&str], file: &Path) -> BlcOutput {
    let blc = env!("CARGO_BIN_EXE_blc");
    let mut cmd = Command::new(blc);
    for arg in args {
        cmd.arg(arg);
    }
    cmd.arg(file);
    let output = cmd.output().expect("failed to execute blc");
    let signal_killed = output.status.code().is_none();
    BlcOutput {
        exit_code: output.status.code().unwrap_or(-1),
        signal_killed,
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

fn conformance_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/conformance")
}

fn discover_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if !dir.exists() {
        return files;
    }
    for entry in std::fs::read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_dir() {
            files.extend(discover_files(&path));
        } else if path.extension().is_some_and(|e| e == "bl") {
            files.push(path);
        }
    }
    files.sort();
    files
}

fn is_helper_file(path: &Path) -> bool {
    let name = path.file_name().unwrap().to_str().unwrap();
    if name == "util.bl" {
        return true;
    }
    // In the modules directory, files without fn main are support modules (not test targets)
    let parent = path
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
        .unwrap_or("");
    if parent == "09_modules" {
        let content = std::fs::read_to_string(path).unwrap_or_default();
        if !content.contains("fn main") {
            return true;
        }
    }
    false
}

fn is_negative_test(path: &Path) -> bool {
    let name = path.file_name().unwrap().to_str().unwrap();
    name.starts_with("errors_")
}

fn is_check_only(path: &Path) -> bool {
    // Files with `// check-only` marker are check-pass only (type checking, no execution)
    let content = std::fs::read_to_string(path).unwrap_or_default();
    if content.lines().any(|l| l.trim() == "// check-only") {
        return true;
    }
    // Directories that are check-pass only by default
    let parent = path
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
        .unwrap_or("");
    parent == "13_concurrency" || parent == "14_web"
}

/// Extract expected stdout lines from comments like:
/// // expect: some output
fn extract_expected_output(path: &Path) -> Vec<String> {
    let content = std::fs::read_to_string(path).unwrap();
    let mut out = Vec::new();
    for line in content.lines() {
        if let Some(rest) = line.trim().strip_prefix("// expect:") {
            out.push(rest.trim().to_string());
        }
    }
    out
}

/// Extract expected error codes from comments like:
/// // Expected errors: TYP_005, TYP_006
fn extract_expected_codes(path: &Path) -> Vec<String> {
    let content = std::fs::read_to_string(path).unwrap();
    for line in content.lines() {
        if line.contains("Expected errors:") {
            let after = line.split("Expected errors:").nth(1).unwrap().trim();
            return after
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();
        }
    }
    Vec::new()
}

/// Extract expected warning codes from comments like:
/// // Expected warnings: CAP_005
fn extract_expected_warnings(path: &Path) -> Vec<String> {
    let content = std::fs::read_to_string(path).unwrap();
    for line in content.lines() {
        if line.contains("Expected warnings:") {
            let after = line.split("Expected warnings:").nth(1).unwrap().trim();
            return after
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();
        }
    }
    Vec::new()
}

#[test]
fn conformance_positive_tests() {
    let dir = conformance_dir();
    let files = discover_files(&dir);
    let mut passed = 0;
    let mut failed = Vec::new();

    for file in &files {
        if is_helper_file(file) || is_negative_test(file) {
            continue;
        }

        let relative = file.strip_prefix(&dir).unwrap().display().to_string();
        let expected_output = extract_expected_output(file);

        if !expected_output.is_empty() {
            let out = blc_cmd(&["run"], file);
            if out.exit_code != 0 {
                failed.push(format!(
                    "{relative}: run failed\n  stdout: {}\n  stderr: {}",
                    out.stdout.trim(),
                    out.stderr.trim()
                ));
            } else {
                let actual: Vec<String> = out
                    .stdout
                    .lines()
                    .map(|l| l.trim_end().to_string())
                    .collect();
                if actual != expected_output {
                    failed.push(format!(
                        "{relative}: stdout mismatch\n  expected: {:?}\n  actual: {:?}",
                        expected_output, actual
                    ));
                } else {
                    passed += 1;
                }
            }
        } else if is_check_only(file) {
            let out = blc_cmd(&["check"], file);
            if out.exit_code != 0 {
                failed.push(format!(
                    "{relative}: check failed\n  stderr: {}",
                    out.stderr
                ));
            } else {
                passed += 1;
            }
        } else {
            // JIT backend (default for `blc test`)
            let out = blc_cmd(&["test"], file);
            if out.exit_code != 0 {
                if out.signal_killed {
                    eprintln!("  WARN: {relative} crashed (signal-killed)");
                }
                failed.push(format!(
                    "{relative}: test failed\n  stdout: {}\n  stderr: {}",
                    out.stdout.trim(),
                    out.stderr.trim()
                ));
            } else {
                passed += 1;
            }
        }
    }

    if !failed.is_empty() {
        panic!(
            "\n{} conformance test(s) failed (of {} total):\n\n{}\n",
            failed.len(),
            passed + failed.len(),
            failed.join("\n\n")
        );
    }

    assert!(passed > 0, "No conformance tests found!");

    // Regression gate: fail if tests silently disappear.
    // Update this number when you ADD new conformance tests.
    const MIN_POSITIVE: usize = 144;
    assert!(
        passed >= MIN_POSITIVE,
        "Conformance regression: expected at least {MIN_POSITIVE} positive tests, but only {passed} passed. \
         If tests were intentionally removed, update MIN_POSITIVE in conformance_tests.rs."
    );

    eprintln!("Conformance: {passed} positive tests passed (minimum: {MIN_POSITIVE})");
}

#[test]
fn conformance_negative_tests() {
    let dir = conformance_dir();
    let files = discover_files(&dir);
    let mut passed = 0;
    let mut failed = Vec::new();

    for file in &files {
        if !is_negative_test(file) {
            continue;
        }

        let relative = file.strip_prefix(&dir).unwrap().display().to_string();
        let expected_codes = extract_expected_codes(file);
        let expected_warnings = extract_expected_warnings(file);

        let out = blc_cmd(&["check"], file);

        // Warning-only files: check passes (exit 0) but warnings must appear in stderr
        if expected_codes.is_empty() && !expected_warnings.is_empty() {
            let mut ok = true;
            for code in &expected_warnings {
                if !out.stderr.contains(code.as_str()) {
                    failed.push(format!(
                        "{relative}: expected warning code {code} not found\n  stderr: {}",
                        out.stderr.trim()
                    ));
                    ok = false;
                }
            }
            if ok {
                passed += 1;
            }
            continue;
        }

        if out.exit_code == 0 {
            failed.push(format!("{relative}: expected check to fail, but it passed"));
            continue;
        }

        for code in &expected_codes {
            if !out.stderr.contains(code) {
                failed.push(format!(
                    "{relative}: expected error code {code} not found\n  stderr: {}",
                    out.stderr.trim()
                ));
            }
        }

        passed += 1;
    }

    if !failed.is_empty() {
        panic!(
            "\n{} negative test(s) failed (of {} total):\n\n{}\n",
            failed.len(),
            passed + failed.len(),
            failed.join("\n\n")
        );
    }

    assert!(passed > 0, "No negative tests found!");

    // Regression gate: fail if negative tests silently disappear.
    // Update this number when you ADD new negative test files.
    const MIN_NEGATIVE: usize = 27;
    assert!(
        passed >= MIN_NEGATIVE,
        "Negative test regression: expected at least {MIN_NEGATIVE} negative tests, but only {passed} passed. \
         If tests were intentionally removed, update MIN_NEGATIVE in conformance_tests.rs."
    );

    eprintln!("Negative tests: {passed} files passed (minimum: {MIN_NEGATIVE})");
}
