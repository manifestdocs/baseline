use std::path::Path;

use tree_sitter::Parser;
use tree_sitter_baseline::LANGUAGE;

use crate::diagnostics::Location;
use crate::test_runner::{TestResult, TestStatus, TestSuiteResult, TestSummary};

use super::lower::Lowerer;
use super::natives::NativeRegistry;

#[cfg(feature = "jit")]
pub fn run_test_file_jit(path: &Path) -> TestSuiteResult {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) => return empty_fail_result(),
    };
    let file_str = path.display().to_string();

    let mut parser = Parser::new();
    parser.set_language(&LANGUAGE.into()).unwrap();
    let tree = parser.parse(&source, None).unwrap();
    let root = tree.root_node();

    let check_result = crate::parse::parse_source(&source, &file_str);
    if check_result.diagnostics.iter().any(|d| d.severity == crate::diagnostics::Severity::Error) {
        return empty_fail_result_msg("Analysis failed");
    }

    let (type_map, dict_map) = {
        let (_, tm, dm) = crate::analysis::types::check_types_with_map(&root, &source, &file_str);
        (tm, dm)
    };

    let natives = NativeRegistry::new();
    let (stdlib_fns, stdlib_names) = super::stdlib::compile_stdlib(&natives);
    let stdlib_fn_count = stdlib_fns.len();

    let mut lowerer = Lowerer::new(&source, &natives, Some(type_map));
    lowerer.set_dict_map(dict_map);
    lowerer.add_functions(stdlib_names.iter().cloned());
    let ir_test_module = match lowerer.lower_module_with_tests(&root) {
        Ok(m) => m,
        Err(e) => return empty_fail_result_msg(&e.message),
    };

    // Collect test metadata before consuming the module
    let test_meta: Vec<(String, usize)> = ir_test_module
        .tests
        .iter()
        .filter(|t| !t.skip)
        .map(|t| (t.name.clone(), t.line))
        .collect();
    let test_count = test_meta.len();
    if test_count == 0 {
        return empty_pass_result();
    }

    let mut ir_test_module = ir_test_module;
    ir_test_module.functions.splice(0..0, stdlib_fns);
    ir_test_module.entry += stdlib_fn_count;
    let mut execute_module = ir_test_module.into_executable_module();
    crate::vm::optimize_ir::optimize(&mut execute_module);

    let program = match crate::vm::jit::compile_with_natives(&execute_module, true, Some(&natives)) {
        Ok(p) => p,
        Err(e) => return empty_fail_result_msg(&format!("JIT compile error: {}", e)),
    };

    // The entry function returns the count of failed tests (0 = all pass)
    let failed_count = match program.run_entry_nvalue() {
        Some(val) if val.is_int() => (val.as_int().max(0) as usize).min(test_count),
        _ => test_count,
    };
    let passed_count = test_count - failed_count;

    build_suite_result_counted(&test_meta, passed_count, failed_count)
}

#[cfg(not(feature = "jit"))]
pub fn run_test_file_jit(_path: &Path) -> TestSuiteResult {
    empty_fail_result_msg("JIT not enabled")
}

#[cfg(feature = "aot")]
pub fn run_test_file_aot(path: &Path) -> TestSuiteResult {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) => return empty_fail_result(),
    };
    let file_str = path.display().to_string();

    let mut parser = Parser::new();
    parser.set_language(&LANGUAGE.into()).unwrap();
    let tree = parser.parse(&source, None).unwrap();
    let root = tree.root_node();

    let check_result = crate::parse::parse_source(&source, &file_str);
    if check_result.diagnostics.iter().any(|d| d.severity == crate::diagnostics::Severity::Error) {
        return empty_fail_result_msg("Analysis failed");
    }

    let (type_map, dict_map) = {
        let (_, tm, dm) = crate::analysis::types::check_types_with_map(&root, &source, &file_str);
        (tm, dm)
    };

    let natives = NativeRegistry::new();
    let (stdlib_fns, stdlib_names) = super::stdlib::compile_stdlib(&natives);
    let stdlib_fn_count = stdlib_fns.len();

    let mut lowerer = Lowerer::new(&source, &natives, Some(type_map));
    lowerer.set_dict_map(dict_map);
    lowerer.add_functions(stdlib_names.iter().cloned());
    let ir_test_module = match lowerer.lower_module_with_tests(&root) {
        Ok(m) => m,
        Err(e) => return empty_fail_result_msg(&e.message),
    };

    // Collect test metadata before consuming the module
    let test_meta: Vec<(String, usize)> = ir_test_module
        .tests
        .iter()
        .filter(|t| !t.skip)
        .map(|t| (t.name.clone(), t.line))
        .collect();
    let test_count = test_meta.len();
    if test_count == 0 {
        return empty_pass_result();
    }

    let mut ir_test_module = ir_test_module;
    ir_test_module.functions.splice(0..0, stdlib_fns);
    ir_test_module.entry += stdlib_fn_count;
    let mut execute_module = ir_test_module.into_executable_module();
    crate::vm::optimize_ir::optimize(&mut execute_module);

    let obj_bytes = match crate::vm::jit::aot::compile_to_object(&execute_module, false) {
        Ok(b) => b,
        Err(e) => return empty_fail_result_msg(&format!("AOT compile error: {}", e)),
    };

    let tmp_dir = std::env::temp_dir();
    let exe_path = tmp_dir.join(format!("blc_test_aot_{}", std::process::id()));

    // Find libbaseline_rt
    let mut rt_lib = None;
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        let base = Path::new(&manifest_dir).join("..");
        for profile in &["debug", "release"] {
            let lib = base.join("target").join(profile).join("libbaseline_rt.a");
            if lib.exists() {
                rt_lib = Some(base.join("target").join(profile));
                break;
            }
        }
    }

    if let Err(e) = crate::vm::jit::aot::link_executable(&obj_bytes, &exe_path, rt_lib.as_deref(), false) {
        return empty_fail_result_msg(&format!("AOT link error: {}", e));
    }

    let passed = match std::process::Command::new(&exe_path).status() {
        Ok(status) => status.success(),
        Err(_) => false,
    };

    let _ = std::fs::remove_file(&exe_path);

    // AOT runner: exit code 0 = all pass, non-zero = some failed.
    // We don't know the exact failure count, so report all-or-nothing.
    let (passed_count, failed_count) = if passed {
        (test_count, 0)
    } else {
        (0, test_count)
    };
    build_suite_result_counted(&test_meta, passed_count, failed_count)
}

#[cfg(not(feature = "aot"))]
pub fn run_test_file_aot(_path: &Path) -> TestSuiteResult {
    empty_fail_result_msg("AOT not enabled")
}

fn empty_fail_result() -> TestSuiteResult {
    TestSuiteResult {
        status: "fail".to_string(),
        tests: vec![],
        summary: TestSummary { total: 0, passed: 0, failed: 0, skipped: 0 },
    }
}

fn empty_fail_result_msg(msg: &str) -> TestSuiteResult {
    TestSuiteResult {
        status: "fail".to_string(),
        tests: vec![TestResult {
            name: "setup".to_string(),
            status: TestStatus::Fail,
            message: Some(msg.to_string()),
            location: Location { file: String::new(), line: 0, col: 0, end_line: None, end_col: None },
        }],
        summary: TestSummary { total: 1, passed: 0, failed: 1, skipped: 0 },
    }
}

fn empty_pass_result() -> TestSuiteResult {
    TestSuiteResult {
        status: "pass".to_string(),
        tests: vec![],
        summary: TestSummary { total: 0, passed: 0, failed: 0, skipped: 0 },
    }
}

fn build_suite_result_counted(
    test_meta: &[(String, usize)],
    passed_count: usize,
    failed_count: usize,
) -> TestSuiteResult {
    let total = passed_count + failed_count;
    let mut tests = Vec::new();

    // We know the total passed/failed counts but not which specific tests failed.
    // Mark the first `passed_count` as pass and the rest as fail.
    for (i, (name, line)) in test_meta.iter().enumerate() {
        let is_pass = i < passed_count;
        tests.push(TestResult {
            name: name.clone(),
            status: if is_pass { TestStatus::Pass } else { TestStatus::Fail },
            message: if is_pass { None } else { Some("Assertion failed".to_string()) },
            location: Location {
                file: String::new(),
                line: *line,
                col: 0,
                end_line: None,
                end_col: None,
            },
        });
    }

    TestSuiteResult {
        status: if failed_count == 0 {
            "pass".to_string()
        } else {
            "fail".to_string()
        },
        tests,
        summary: TestSummary {
            total,
            passed: passed_count,
            failed: failed_count,
            skipped: 0,
        },
    }
}
