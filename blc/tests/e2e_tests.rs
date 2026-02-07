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

fn blc_run_interp(file: &str) -> BlcOutput {
    let blc = env!("CARGO_BIN_EXE_blc");
    let examples = Path::new(env!("CARGO_MANIFEST_DIR")).join("../examples");
    let output = Command::new(blc)
        .arg("run")
        .arg("--interp")
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

fn assert_run_fails_interp(file: &str, expected_fragments: &[&str]) {
    let out = blc_run_interp(file);
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

// Http example should pass checking (effects properly declared)
#[test]
fn check_http_test() {
    assert_check_ok("http_test.bl");
}

// Json example should pass checking (pure functions, no effects)
#[test]
fn check_json_test() {
    assert_check_ok("json_test.bl");
}

#[test]
fn run_json_test() {
    assert_run_ok(
        "json_test.bl",
        "name: Alice\n\
         age: 30\n\
         active: true\n\
         nums: [1, 2, 3]\n\
         null: Null\n\
         compact: {\"active\":true,\"age\":30,\"name\":\"Alice\"}\n\
         pretty:\n\
         [\n  1,\n  2,\n  3\n]\n\
         roundtrip: {\"x\":1,\"y\":2}\n",
    );
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
// Server framework tests
// ===========================================================================

#[test]
fn check_server_test() {
    assert_check_ok("server_test.bl");
}

#[test]
fn run_server_test() {
    assert_run_ok(
        "server_test.bl",
        "direct routes: 3\n\
         piped routes: 3\n\
         match: true\n\
         middleware: 1\n",
    );
}

// ===========================================================================
// Middleware tests
// ===========================================================================

#[test]
fn check_middleware_test() {
    assert_check_ok("middleware_test.bl");
}

#[test]
fn run_middleware_test() {
    assert_run_ok(
        "middleware_test.bl",
        "no-mw routes: 1\n\
         single-mw routes: 1\n\
         chained-mw routes: 1\n\
         direct-use routes: 1\n",
    );
}

// ===========================================================================
// Runtime error tests — verify error format includes location and stack trace
// ===========================================================================

#[test]
fn run_runtime_error_has_location_and_stack() {
    // Stack trace format differs between VM and interpreter; test interpreter behavior
    assert_run_fails_interp(
        "runtime_error_test.bl",
        &[
            "Division by zero",
            "runtime_error_test.bl:",
            "at divide",
            "at call_divide",
        ],
    );
}

// ===========================================================================
// Inline test execution — `blc test` subcommand
// ===========================================================================

fn blc_test(file: &str) -> BlcOutput {
    let blc = env!("CARGO_BIN_EXE_blc");
    let examples = Path::new(env!("CARGO_MANIFEST_DIR")).join("../examples");
    let output = Command::new(blc)
        .arg("test")
        .arg(examples.join(file))
        .output()
        .expect("failed to execute blc");

    BlcOutput {
        exit_code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

fn blc_test_json(file: &str) -> BlcOutput {
    let blc = env!("CARGO_BIN_EXE_blc");
    let examples = Path::new(env!("CARGO_MANIFEST_DIR")).join("../examples");
    let output = Command::new(blc)
        .arg("test")
        .arg("--json")
        .arg(examples.join(file))
        .output()
        .expect("failed to execute blc");

    BlcOutput {
        exit_code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

#[test]
fn test_inline_tests_pass() {
    let out = blc_test("inline_test.bl");
    assert_eq!(
        out.exit_code, 0,
        "inline_test.bl: expected exit 0.\nstdout: {}\nstderr: {}",
        out.stdout, out.stderr,
    );
    assert!(out.stdout.contains("5 tests: 5 passed, 0 failed"), "stdout: {}", out.stdout);
}

#[test]
fn test_inline_tests_json() {
    let out = blc_test_json("inline_test.bl");
    assert_eq!(out.exit_code, 0, "expected exit 0.\nstdout: {}", out.stdout);
    let json: serde_json::Value = serde_json::from_str(&out.stdout)
        .expect("JSON parse failed");
    assert_eq!(json["status"], "pass");
    assert_eq!(json["summary"]["total"], 5);
    assert_eq!(json["summary"]["passed"], 5);
    assert_eq!(json["summary"]["failed"], 0);
}

#[test]
fn test_inline_tests_failure() {
    let out = blc_test("inline_test_fail.bl");
    assert_eq!(
        out.exit_code, 1,
        "inline_test_fail.bl: expected exit 1.\nstdout: {}",
        out.stdout,
    );
    assert!(out.stdout.contains("FAIL"), "stdout: {}", out.stdout);
    assert!(out.stdout.contains("1 failed"), "stdout: {}", out.stdout);
}

// ---------------------------------------------------------------------------
// Cross-module imports
// ---------------------------------------------------------------------------

#[test]
fn run_import_qualified() {
    assert_run_ok("imports/main.bl", "Result: 10\n");
}

#[test]
fn run_import_selective() {
    assert_run_ok("imports/selective.bl", "Result: 10\n");
}

#[test]
fn run_import_wildcard() {
    assert_run_ok("imports/wildcard.bl", "Result: 10\n");
}

#[test]
fn check_import_qualified() {
    assert_check_ok("imports/main.bl");
}

#[test]
fn check_import_selective() {
    assert_check_ok("imports/selective.bl");
}

#[test]
fn run_import_missing_module() {
    // Import error message format differs between VM and interpreter
    assert_run_fails_interp("imports/import_missing.bl", &["Import Error"]);
}

#[test]
fn check_import_missing_module() {
    assert_check_has_errors("imports/import_missing.bl", &["IMP_001"]);
}

// Note: circular import detection requires recursive import resolution,
// which is deferred to a future version.

// ---------------------------------------------------------------------------
// VM cross-module imports — blc run --vm
// ---------------------------------------------------------------------------

fn blc_run_vm(file: &str) -> BlcOutput {
    let blc = env!("CARGO_BIN_EXE_blc");
    let examples = Path::new(env!("CARGO_MANIFEST_DIR")).join("../examples");
    let output = Command::new(blc)
        .arg("run")
        .arg("--vm")
        .arg(examples.join(file))
        .output()
        .expect("failed to execute blc");

    BlcOutput {
        exit_code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

fn blc_run_vm_conformance(file: &str) -> BlcOutput {
    let blc = env!("CARGO_BIN_EXE_blc");
    let conformance = Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/conformance");
    let output = Command::new(blc)
        .arg("run")
        .arg("--vm")
        .arg(conformance.join(file))
        .output()
        .expect("failed to execute blc");

    BlcOutput {
        exit_code: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
    }
}

fn assert_vm_run_ok(file: &str, expected_stdout: &str) {
    let out = blc_run_vm(file);
    assert_eq!(
        out.exit_code, 0,
        "VM {file}: expected exit 0, got {}.\nstderr: {}",
        out.exit_code, out.stderr,
    );
    assert_eq!(
        out.stdout, expected_stdout,
        "VM {file}: stdout mismatch",
    );
}

#[test]
fn vm_run_import_qualified() {
    assert_vm_run_ok("imports/main.bl", "Result: 10\n");
}

#[test]
fn vm_run_import_selective() {
    assert_vm_run_ok("imports/selective.bl", "Result: 10\n");
}

#[test]
fn vm_run_import_wildcard() {
    assert_vm_run_ok("imports/wildcard.bl", "Result: 10\n");
}

#[test]
fn vm_run_conformance_qualified_import() {
    let out = blc_run_vm_conformance("09_modules/qualified_import.bl");
    assert_eq!(out.exit_code, 0, "VM qualified_import: exit {}.\nstderr: {}", out.exit_code, out.stderr);
    assert_eq!(out.stdout, "10\n", "VM qualified_import: stdout mismatch");
}

#[test]
fn vm_run_conformance_selective_import() {
    let out = blc_run_vm_conformance("09_modules/selective_import.bl");
    assert_eq!(out.exit_code, 0, "VM selective_import: exit {}.\nstderr: {}", out.exit_code, out.stderr);
    assert_eq!(out.stdout, "11\n", "VM selective_import: stdout mismatch");
}

#[test]
fn vm_run_conformance_wildcard_import() {
    let out = blc_run_vm_conformance("09_modules/wildcard_import.bl");
    assert_eq!(out.exit_code, 0, "VM wildcard_import: exit {}.\nstderr: {}", out.exit_code, out.stderr);
    assert_eq!(out.stdout, "11\n", "VM wildcard_import: stdout mismatch");
}

#[test]
fn vm_run_import_matches_interpreter() {
    // Verify VM and interpreter produce identical output for all module test files
    let files = &[
        "imports/main.bl",
        "imports/selective.bl",
        "imports/wildcard.bl",
    ];
    for file in files {
        let interp = blc_run(file);
        let vm = blc_run_vm(file);
        assert_eq!(
            interp.exit_code, vm.exit_code,
            "{file}: exit code mismatch (interp={}, vm={})",
            interp.exit_code, vm.exit_code,
        );
        assert_eq!(
            interp.stdout, vm.stdout,
            "{file}: stdout mismatch\n  interp: {}\n  vm: {}",
            interp.stdout, vm.stdout,
        );
    }
}
