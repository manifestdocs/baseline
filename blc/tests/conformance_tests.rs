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
    // Files with main! or in effects/modules dirs are check-pass only
    let parent = path
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
        .unwrap_or("");
    parent == "08_effects"
        || parent == "09_modules"
        || parent == "13_concurrency"
        || parent == "14_web"
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

        if is_check_only(file) {
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
            // VM backend is required to pass
            let out = blc_cmd(&["test"], file);
            if out.exit_code != 0 {
                failed.push(format!(
                    "{relative} (VM): test failed\n  stdout: {}\n  stderr: {}",
                    out.stdout.trim(),
                    out.stderr.trim()
                ));
            } else {
                passed += 1;
            }

            // JIT backend: best-effort — files with unsupported constructs
            // (effect handlers, typed holes, etc.) will fail JIT compilation.
            // Crashes (segfault/abort) are logged but not fatal while JIT is
            // still gaining coverage.
            let jit_out = blc_cmd(&["test", "--jit"], file);
            if jit_out.exit_code == 0 {
                passed += 1;
            } else if jit_out.signal_killed {
                eprintln!("  WARN: {relative} (JIT) crashed (signal-killed)");
            }
            // else: JIT compilation failure (exit 1) — expected for unsupported constructs
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
    eprintln!("Conformance: {passed} files passed");
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

        let out = blc_cmd(&["check"], file);

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
    eprintln!("Negative tests: {passed} files passed");
}
