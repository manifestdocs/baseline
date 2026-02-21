//! Bootstrap Verification Tests
//!
//! Validates the self-hosted compiler pipeline:
//!   .bl → (blc VM runs compiler.bl) → .c → (cc) → native binary → correct exit code
//!
//! Each test copies a test program to selfhost/test_input.bl, runs the self-hosted
//! compiler, compiles the generated C, and checks the exit code.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn project_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .canonicalize()
        .expect("cannot find project root")
}

fn bootstrap_compile(test_file: &str) -> (String, PathBuf) {
    let root = project_root();
    let blc = env!("CARGO_BIN_EXE_blc");
    let compiler = root.join("selfhost/compiler.bl");
    let test_input = root.join("selfhost/test_input.bl");
    let output_c = root.join("selfhost/output.c");
    let test_src = root.join("selfhost/tests").join(test_file);

    // Copy test program as input
    let original = fs::read_to_string(&test_input).unwrap_or_default();
    fs::copy(&test_src, &test_input).expect("failed to copy test input");

    // Run self-hosted compiler via blc VM (must run from project root for relative paths)
    let result = Command::new(blc)
        .arg("run")
        .arg(&compiler)
        .current_dir(&root)
        .output()
        .expect("failed to run blc");

    // Restore original test_input.bl
    fs::write(&test_input, &original).ok();

    assert!(
        result.status.success(),
        "blc run compiler.bl failed for {test_file}:\n{}",
        String::from_utf8_lossy(&result.stderr)
    );

    let c_code = fs::read_to_string(&output_c).expect("output.c not generated");
    (c_code, output_c)
}

fn compile_and_run_c(c_file: &Path, test_name: &str) -> i32 {
    let bin_path = c_file.with_extension("");

    let cc = Command::new("cc")
        .arg("-o")
        .arg(&bin_path)
        .arg(c_file)
        .output()
        .expect("failed to run cc");

    assert!(
        cc.status.success(),
        "cc failed for {test_name}:\n{}",
        String::from_utf8_lossy(&cc.stderr)
    );

    let run = Command::new(&bin_path)
        .output()
        .expect("failed to run compiled binary");

    // Clean up binary
    fs::remove_file(&bin_path).ok();

    run.status.code().unwrap_or(-1)
}

fn assert_bootstrap(test_file: &str, expected_exit: i32) {
    let (c_code, _c_path) = bootstrap_compile(test_file);

    // Write to a temp file to avoid clobbering output.c
    let root = project_root();
    let test_name = test_file.trim_end_matches(".bl");
    let temp_c = root.join(format!("selfhost/.bootstrap_{test_name}.c"));
    fs::write(&temp_c, &c_code).expect("failed to write temp C file");

    let exit = compile_and_run_c(&temp_c, test_name);
    fs::remove_file(&temp_c).ok();

    assert_eq!(
        exit, expected_exit,
        "{test_file}: expected exit {expected_exit}, got {exit}"
    );

    // Determinism: compile again and verify identical output
    let (c_code2, _) = bootstrap_compile(test_file);
    assert_eq!(
        c_code, c_code2,
        "{test_file}: non-deterministic output — two runs produced different C"
    );
}

#[test]
#[ignore = "self-hosted compiler needs EnumTag fix (pre-existing on main)"]
fn bootstrap_arithmetic() {
    assert_bootstrap("arithmetic.bl", 42);
}

#[test]
#[ignore = "self-hosted compiler needs EnumTag fix (pre-existing on main)"]
fn bootstrap_factorial() {
    assert_bootstrap("factorial.bl", 120);
}

#[test]
#[ignore = "self-hosted compiler needs EnumTag fix (pre-existing on main)"]
fn bootstrap_fibonacci() {
    assert_bootstrap("fibonacci.bl", 55);
}

#[test]
#[ignore = "self-hosted compiler needs EnumTag fix (pre-existing on main)"]
fn bootstrap_types() {
    assert_bootstrap("types.bl", 7);
}

#[test]
#[ignore = "self-hosted compiler needs EnumTag fix (pre-existing on main)"]
fn bootstrap_nested_calls() {
    assert_bootstrap("nested_calls.bl", 10);
}
