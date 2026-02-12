//! Integration tests for AOT compilation.
//!
//! These tests compile Baseline source files to native executables and
//! verify their output matches expected values.

#![cfg(feature = "aot")]

use std::path::Path;
use std::process::Command;

/// Compile a .bl file to a native executable and run it, returning stdout.
fn compile_and_run(source_path: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let project_root = Path::new(manifest_dir).parent().unwrap();
    let source = project_root.join(source_path);

    // Build blc path from target directory
    let blc = env!("CARGO_BIN_EXE_blc");

    let tmp_dir = std::env::temp_dir();
    let stem = Path::new(source_path)
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap();
    let output = tmp_dir.join(format!("aot_test_{}", stem));

    // Compile
    let build_result = Command::new(blc)
        .arg("build")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()
        .expect("Failed to run blc build");

    if !build_result.status.success() {
        let stderr = String::from_utf8_lossy(&build_result.stderr);
        panic!(
            "blc build failed for {}:\n{}",
            source_path, stderr
        );
    }

    // Run the compiled executable
    let run_result = Command::new(&output)
        .output()
        .expect("Failed to run compiled executable");

    // Clean up
    let _ = std::fs::remove_file(&output);

    if !run_result.status.success() {
        let stderr = String::from_utf8_lossy(&run_result.stderr);
        panic!(
            "Executable failed for {}:\n{}",
            source_path, stderr
        );
    }

    String::from_utf8_lossy(&run_result.stdout).trim().to_string()
}

#[test]
fn aot_fib() {
    let output = compile_and_run("tests/aot/fib.bl");
    assert_eq!(output, "9227465");
}

#[test]
fn aot_tak() {
    let output = compile_and_run("tests/aot/tak.bl");
    assert_eq!(output, "7");
}

#[test]
fn aot_factorial() {
    let output = compile_and_run("tests/aot/factorial.bl");
    assert_eq!(output, "3628800");
}

// --- Phase 2: Heap type tests ---

#[test]
fn aot_string_hello() {
    let output = compile_and_run("tests/aot/string_hello.bl");
    assert_eq!(output, "hello world");
}

#[test]
fn aot_string_concat() {
    let output = compile_and_run("tests/aot/string_concat.bl");
    assert_eq!(output, "hello world");
}

#[test]
fn aot_enum_match() {
    let output = compile_and_run("tests/aot/enum_match.bl");
    assert_eq!(output, "positive");
}

#[test]
fn aot_tuple_access() {
    let output = compile_and_run("tests/aot/tuple_access.bl");
    assert_eq!(output, "42");
}

#[test]
fn aot_record_field() {
    let output = compile_and_run("tests/aot/record_field.bl");
    assert_eq!(output, "42");
}

#[test]
fn aot_closure_apply() {
    let output = compile_and_run("tests/aot/closure_apply.bl");
    assert_eq!(output, "42");
}

// --- Phase 3: Native function call tests ---

#[test]
fn aot_println() {
    let output = compile_and_run("tests/aot/println.bl");
    assert_eq!(output, "hello from aot");
}

#[test]
fn aot_string_ops() {
    let output = compile_and_run("tests/aot/string_ops.bl");
    assert_eq!(output, "11");
}

#[test]
fn aot_math_ops() {
    let output = compile_and_run("tests/aot/math_ops.bl");
    assert_eq!(output, "1096");
}

#[test]
fn aot_list_ops() {
    let output = compile_and_run("tests/aot/list_ops.bl");
    assert_eq!(output, "10");
}

#[test]
fn aot_option_unwrap() {
    let output = compile_and_run("tests/aot/option_unwrap.bl");
    assert_eq!(output, "54");
}

#[test]
fn aot_int_convert() {
    let output = compile_and_run("tests/aot/int_convert.bl");
    assert_eq!(output, "12345");
}

// --- Phase 4: Higher-order function tests ---

#[test]
fn aot_list_map() {
    let output = compile_and_run("tests/aot/list_map.bl");
    assert_eq!(output, "[2, 4, 6]");
}

#[test]
fn aot_list_filter() {
    let output = compile_and_run("tests/aot/list_filter.bl");
    assert_eq!(output, "[3, 4, 5]");
}

#[test]
fn aot_list_fold() {
    let output = compile_and_run("tests/aot/list_fold.bl");
    assert_eq!(output, "15");
}

#[test]
fn aot_list_find() {
    let output = compile_and_run("tests/aot/list_find.bl");
    assert_eq!(output, "Some(2)");
}

#[test]
fn aot_option_map() {
    let output = compile_and_run("tests/aot/option_map.bl");
    assert_eq!(output, "Some(42)");
}

#[test]
fn aot_result_map() {
    let output = compile_and_run("tests/aot/result_map.bl");
    assert_eq!(output, "Ok(42)");
}

// --- Phase 5: Fs, Map, Set native tests ---

#[test]
fn aot_fs_ops() {
    let output = compile_and_run("tests/aot/fs_ops.bl");
    assert_eq!(output, "hello from aot fs");
}

#[test]
fn aot_map_ops() {
    let output = compile_and_run("tests/aot/map_ops.bl");
    assert_eq!(output, "8");
}

#[test]
fn aot_set_ops() {
    let output = compile_and_run("tests/aot/set_ops.bl");
    assert_eq!(output, "6");
}
