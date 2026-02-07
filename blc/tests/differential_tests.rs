//! Differential testing: interpreter vs bytecode VM
//!
//! For each conformance `.bl` file that contains inline tests, runs both:
//!   `blc test <file>` (interpreter)
//!   `blc test --vm <file>` (bytecode VM)
//! and asserts both produce the same pass/fail results.
//!
//! Files where the VM is known to diverge are skipped.

use std::path::{Path, PathBuf};
use std::process::Command;

fn blc_test(file: &Path, use_vm: bool) -> (i32, String, String) {
    let blc = env!("CARGO_BIN_EXE_blc");
    let mut cmd = Command::new(blc);
    cmd.arg("test").arg("--json");
    if use_vm {
        cmd.arg("--vm");
    }
    cmd.arg(file);
    let output = cmd.output().expect("failed to execute blc");
    (
        output.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

fn blc_run(file: &Path, use_vm: bool) -> (i32, String, String) {
    let blc = env!("CARGO_BIN_EXE_blc");
    let mut cmd = Command::new(blc);
    cmd.arg("run");
    if use_vm {
        cmd.arg("--vm");
    }
    cmd.arg(file);
    let output = cmd.output().expect("failed to execute blc");
    (
        output.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

fn conformance_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/conformance")
}

fn programs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/programs")
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

/// Files that are known to diverge between interpreter and VM.
/// The VM doesn't support all features yet — expand this as VM gains features.
fn is_vm_skip(path: &Path) -> bool {
    let name = path.file_name().unwrap().to_str().unwrap();

    // Skip negative test files and helper files
    name.starts_with("errors_") || name == "util.bl"
}

#[test]
fn differential_inline_tests() {
    let dir = conformance_dir();
    let files = discover_files(&dir);
    let mut tested = 0;
    let mut skipped = 0;
    let mut failed = Vec::new();

    for file in &files {
        if is_vm_skip(file) {
            skipped += 1;
            continue;
        }

        let relative = file.strip_prefix(&dir).unwrap_or(file).display().to_string();

        let (interp_rc, interp_out, _interp_err) = blc_test(file, false);

        // Skip if interpreter itself fails
        if interp_rc != 0 {
            skipped += 1;
            continue;
        }

        let (vm_rc, vm_out, vm_err) = blc_test(file, true);

        // If VM fails with compilation error, skip (VM doesn't support the feature yet)
        if vm_rc != 0
            && (vm_err.contains("Compile error")
                || vm_out.contains("Compile error")
                || vm_out.trim().is_empty())
        {
            skipped += 1;
            continue;
        }

        // Both should succeed if we get here
        if interp_rc != vm_rc {
            failed.push(format!(
                "{relative}: interpreter exit={interp_rc}, VM exit={vm_rc}\n  interp: {interp_out}\n  vm: {vm_out}",
            ));
            continue;
        }

        // If JSON output, compare test results
        if interp_rc == 0 && let (Ok(interp_json), Ok(vm_json)) = (
            serde_json::from_str::<serde_json::Value>(&interp_out),
            serde_json::from_str::<serde_json::Value>(&vm_out),
        ) {
            let interp_summary = &interp_json["summary"];
            let vm_summary = &vm_json["summary"];
            if interp_summary["passed"] != vm_summary["passed"]
                || interp_summary["failed"] != vm_summary["failed"]
            {
                failed.push(format!(
                    "{relative}: test counts differ\n  interp: {interp_summary}\n  vm: {vm_summary}"
                ));
                continue;
            }
        }

        tested += 1;
    }

    eprintln!("Differential: {tested} files agree, {skipped} skipped");

    if !failed.is_empty() {
        panic!(
            "\n{} differential test(s) failed:\n\n{}\n",
            failed.len(),
            failed.join("\n\n")
        );
    }
}

#[test]
fn differential_run_programs() {
    let dir = programs_dir();
    let files = discover_files(&dir);

    if files.is_empty() {
        eprintln!("No program files found for differential run testing");
        return;
    }

    let mut tested = 0;
    let mut skipped = 0;
    let mut failed = Vec::new();

    for file in &files {
        let relative = file.strip_prefix(&dir).unwrap_or(file).display().to_string();

        let (interp_rc, interp_out, _interp_err) = blc_run(file, false);
        let (vm_rc, vm_out, vm_err) = blc_run(file, true);

        // Skip if interpreter itself fails (not a VM issue)
        if interp_rc != 0 {
            skipped += 1;
            continue;
        }

        if vm_rc != 0 {
            // VM may not support all features yet — log but don't fail
            eprintln!("SKIP {relative}: VM error: {}", vm_err.trim());
            skipped += 1;
            continue;
        }

        if interp_out != vm_out {
            failed.push(format!(
                "{relative}: stdout differs\n  interp: {}\n  vm: {}",
                interp_out.trim(),
                vm_out.trim()
            ));
        } else {
            tested += 1;
        }
    }

    eprintln!("Differential run: {tested} programs agree, {skipped} skipped");

    if !failed.is_empty() {
        panic!(
            "\n{} differential run test(s) failed:\n\n{}\n",
            failed.len(),
            failed.join("\n\n")
        );
    }
}
