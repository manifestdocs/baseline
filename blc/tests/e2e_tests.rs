use std::path::Path;
use std::process::Command;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

struct BlcOutput {
    exit_code: i32,
    stdout: String,
    stderr: String,
}

fn blc_run(file: &str) -> BlcOutput {
    let blc = env!("CARGO_BIN_EXE_blc");
    let examples = Path::new(env!("CARGO_MANIFEST_DIR")).join("../examples");
    let output = Command::new(blc)
        .arg("run")
        .arg(examples.join(file))
        .output()
        .expect("failed to execute blc");

    BlcOutput {
        exit_code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

fn blc_check(file: &str) -> BlcOutput {
    let blc = env!("CARGO_BIN_EXE_blc");
    let examples = Path::new(env!("CARGO_MANIFEST_DIR")).join("../examples");
    let output = Command::new(blc)
        .arg("check")
        .arg(examples.join(file))
        .output()
        .expect("failed to execute blc");

    BlcOutput {
        exit_code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

fn assert_run_ok(file: &str, expected_stdout: &str) {
    let out = blc_run(file);
    assert_eq!(
        out.exit_code, 0,
        "{file}: expected exit 0, got {}.\nstderr: {}",
        out.exit_code, out.stderr,
    );
    assert_eq!(
        out.stdout, expected_stdout,
        "{file}: stdout mismatch",
    );
}

fn assert_check_ok(file: &str) {
    let out = blc_check(file);
    assert_eq!(
        out.exit_code, 0,
        "{file}: expected check to pass.\nstderr: {}",
        out.stderr,
    );
}

fn assert_run_fails(file: &str, expected_fragments: &[&str]) {
    let out = blc_run(file);
    assert_eq!(
        out.exit_code, 1,
        "{file}: expected exit 1, got {}.\nstderr: {}",
        out.exit_code, out.stderr,
    );
    for frag in expected_fragments {
        assert!(
            out.stderr.contains(frag),
            "{file}: expected '{frag}' in stderr.\nstderr: {}",
            out.stderr,
        );
    }
}

fn assert_check_has_errors(file: &str, expected_codes: &[&str]) {
    let out = blc_check(file);
    assert_eq!(
        out.exit_code, 1,
        "{file}: expected check to fail.\nstdout: {}",
        out.stdout,
    );
    for code in expected_codes {
        assert!(
            out.stderr.contains(code),
            "{file}: expected error code {code} in stderr.\nstderr: {}",
            out.stderr,
        );
    }
}

// ===========================================================================
// Run tests — files with main/main! that should execute successfully
// ===========================================================================

#[test]
fn run_hello() {
    assert_run_ok("hello.bl", "Hello, World!1 + 2 = 3");
}

#[test]
fn run_grammar_test() {
    assert_run_ok("grammar_test.bl", "");
}

#[test]
fn run_list_test() {
    assert_run_ok(
        "list_test.bl",
        "length: 5\n\
         head: Some(5)\n\
         reverse: [2, 4, 1, 3, 5]\n\
         sort: [1, 2, 3, 4, 5]\n\
         map *2: [10, 6, 2, 8, 4]\n\
         filter >3: [5, 4]\n\
         fold sum: 15\n\
         find 4: Some(4)\n\
         find 9: None\n\
         concat: [1, 2, 3, 4]\n\
         tail: [2, 3]\n",
    );
}

#[test]
fn run_math_test() {
    assert_run_ok(
        "math_test.bl",
        "abs(-5) = 5\n\
         min(3, 7) = 3\n\
         max(3, 7) = 7\n\
         clamp(15, 0, 10) = 10\n\
         pow(2, 8) = 256\n",
    );
}

#[test]
fn run_option_test() {
    assert_run_ok(
        "option_test.bl",
        "unwrap Some(42): 42\n\
         unwrap_or None 99: 99\n\
         is_some Some(42): true\n\
         is_none None: true\n\
         map Some(42) *2: Some(84)\n\
         map None *2: None\n\
         match Some(10): 10\n",
    );
}

#[test]
fn run_result_test() {
    assert_run_ok(
        "result_test.bl",
        "unwrap Ok(42): 42\n\
         unwrap_or Err 0: 0\n\
         is_ok Ok(42): true\n\
         is_err Err: true\n\
         map Ok(42) *2: Ok(84)\n\
         map Err: Err(something went wrong)\n\
         10 / 3 = 3\n",
    );
}

#[test]
fn run_run_test() {
    assert_run_ok("run_test.bl", "60\n");
}

#[test]
fn run_string_test() {
    assert_run_ok(
        "string_test.bl",
        "length: 17\n\
         trim: Hello, World!\n\
         contains 'World': true\n\
         starts_with '  He': true\n\
         to_upper: HELLO\n\
         to_lower: hello\n\
         split: [alice, bob, charlie]\n\
         join: alice | bob | charlie\n\
         slice 0..5: hello\n",
    );
}

#[test]
fn run_struct_test() {
    assert_run_ok("struct_test.bl", "10\n");
}

// ===========================================================================
// Check-pass tests — files that should pass type/effect/refinement checking
// ===========================================================================

#[test]
fn check_sum_type_test() {
    assert_check_ok("sum_type_test.bl");
}

// Also verify that all runnable examples pass checking
#[test]
fn check_hello() {
    assert_check_ok("hello.bl");
}

#[test]
fn check_list_test() {
    assert_check_ok("list_test.bl");
}

#[test]
fn check_run_test() {
    assert_check_ok("run_test.bl");
}

// ===========================================================================
// Check-fail tests — files with intentional errors
// ===========================================================================

#[test]
fn check_effects_test() {
    assert_check_has_errors("effects_test.bl", &["CAP_001"]);
}

#[test]
fn check_refinement_test() {
    assert_check_has_errors("refinement_test.bl", &["REF_001"]);
}

#[test]
fn check_type_fail() {
    assert_check_has_errors(
        "type_fail.bl",
        &["TYP_005", "TYP_006", "TYP_008", "TYP_010"],
    );
}

#[test]
fn check_type_test() {
    assert_check_has_errors(
        "type_test.bl",
        &["TYP_001", "TYP_002", "TYP_003", "TYP_004", "TYP_005"],
    );
}

// ===========================================================================
// Runtime error tests — verify error format includes location and stack trace
// ===========================================================================

#[test]
fn run_runtime_error_has_location_and_stack() {
    assert_run_fails(
        "runtime_error_test.bl",
        &[
            "Division by zero",
            "runtime_error_test.bl:",
            "at divide",
            "at call_divide",
        ],
    );
}
