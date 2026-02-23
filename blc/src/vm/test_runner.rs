use std::path::Path;

use tree_sitter::Parser;
use tree_sitter_baseline::LANGUAGE;

use crate::diagnostics::Location;
use crate::test_runner::{TestResult, TestStatus, TestSuiteResult, TestSummary};

use super::chunk::CompileError;
use super::codegen::Codegen;
use super::exec::Vm;
use super::lower::Lowerer;
use super::natives::NativeRegistry;
use super::value::Value;

/// Run inline tests using the bytecode VM.
pub fn run_test_file(path: &Path) -> TestSuiteResult {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) => {
            return TestSuiteResult {
                status: "fail".to_string(),
                tests: vec![],
                summary: TestSummary {
                    total: 0,
                    passed: 0,
                    failed: 0,
                    skipped: 0,
                },
            };
        }
    };

    let file_str = path.display().to_string();

    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load Baseline grammar");
    let tree = parser.parse(&source, None).expect("Failed to parse");
    let root = tree.root_node();

    // Run analysis first — bail on errors
    let check_result = crate::parse::parse_source(&source, &file_str);
    let has_errors = check_result
        .diagnostics
        .iter()
        .any(|d| d.severity == crate::diagnostics::Severity::Error);
    if has_errors {
        return TestSuiteResult {
            status: "fail".to_string(),
            tests: vec![TestResult {
                name: "analysis".to_string(),
                status: TestStatus::Fail,
                message: Some(format!(
                    "File has {} analysis error(s)",
                    check_result
                        .diagnostics
                        .iter()
                        .filter(|d| d.severity == crate::diagnostics::Severity::Error)
                        .count()
                )),
                location: Location {
                    file: file_str.clone(),
                    line: 1,
                    col: 1,
                    end_line: None,
                    end_col: None,
                },
            }],
            summary: TestSummary {
                total: 1,
                passed: 0,
                failed: 1,
                skipped: 0,
            },
        };
    }

    // Get the TypeMap and DictMap from the type checker for trait dispatch
    let (type_map, dict_map) = {
        let (_, tm, dm) = crate::analysis::types::check_types_with_map(&root, &source, &file_str);
        (tm, dm)
    };

    // Compile with tests using the IR pipeline
    let natives = NativeRegistry::new();
    let mut lowerer = Lowerer::new(&source, &natives, Some(type_map));
    lowerer.set_dict_map(dict_map);
    let ir_test_module = match lowerer.lower_module_with_tests(&root) {
        Ok(m) => m,
        Err(e) => {
            let ce = CompileError {
                message: e.message,
                line: e.line,
                col: e.col,
            };
            return TestSuiteResult {
                status: "fail".to_string(),
                tests: vec![TestResult {
                    name: "compilation".to_string(),
                    status: TestStatus::Fail,
                    message: Some(format!("Compile error: {}", ce)),
                    location: Location {
                        file: file_str,
                        line: ce.line,
                        col: ce.col,
                        end_line: None,
                        end_col: None,
                    },
                }],
                summary: TestSummary {
                    total: 1,
                    passed: 0,
                    failed: 1,
                    skipped: 0,
                },
            };
        }
    };

    let codegen = Codegen::new(&natives);
    let test_program = match codegen.generate_test_program(&ir_test_module) {
        Ok(tp) => tp,
        Err(e) => {
            let ce = CompileError {
                message: e.message,
                line: e.line,
                col: e.col,
            };
            return TestSuiteResult {
                status: "fail".to_string(),
                tests: vec![TestResult {
                    name: "compilation".to_string(),
                    status: TestStatus::Fail,
                    message: Some(format!("Codegen error: {}", ce)),
                    location: Location {
                        file: file_str,
                        line: ce.line,
                        col: ce.col,
                        end_line: None,
                        end_col: None,
                    },
                }],
                summary: TestSummary {
                    total: 1,
                    passed: 0,
                    failed: 1,
                    skipped: 0,
                },
            };
        }
    };

    let mut test_program = test_program;
    test_program.program.optimize();

    if test_program.tests.is_empty() {
        return TestSuiteResult {
            status: "pass".to_string(),
            tests: vec![],
            summary: TestSummary {
                total: 0,
                passed: 0,
                failed: 0,
                skipped: 0,
            },
        };
    }

    // Run each test expression chunk in a fresh VM
    let mut results = Vec::new();
    let chunks = &test_program.program.chunks;

    for test in &test_program.tests {
        let location = Location {
            file: file_str.clone(),
            line: test.line,
            col: test.col,
            end_line: Some(test.end_line),
            end_col: Some(test.end_col),
        };

        if test.skip {
            results.push(TestResult {
                name: test.name.clone(),
                status: TestStatus::Skip,
                message: None,
                location,
            });
            continue;
        }

        let mut vm = Vm::new();
        match vm.execute_chunk_at(chunks, test.chunk_idx) {
            Ok(Value::Bool(true)) => {
                results.push(TestResult {
                    name: test.name.clone(),
                    status: TestStatus::Pass,
                    message: None,
                    location,
                });
            }
            Ok(Value::Bool(false)) => {
                results.push(TestResult {
                    name: test.name.clone(),
                    status: TestStatus::Fail,
                    message: Some("Expected true, got false".to_string()),
                    location,
                });
            }
            Ok(other) => {
                results.push(TestResult {
                    name: test.name.clone(),
                    status: TestStatus::Fail,
                    message: Some(format!("Expected Bool, got {}", other)),
                    location,
                });
            }
            Err(e) => {
                results.push(TestResult {
                    name: test.name.clone(),
                    status: TestStatus::Fail,
                    message: Some(format!("Runtime error: {}", e)),
                    location,
                });
            }
        }
    }

    let passed = results
        .iter()
        .filter(|r| r.status == TestStatus::Pass)
        .count();
    let failed = results
        .iter()
        .filter(|r| r.status == TestStatus::Fail)
        .count();
    let skipped = results
        .iter()
        .filter(|r| r.status == TestStatus::Skip)
        .count();
    let total = results.len();

    TestSuiteResult {
        status: if failed == 0 { "pass" } else { "fail" }.to_string(),
        tests: results,
        summary: TestSummary {
            total,
            passed,
            failed,
            skipped,
        },
    }
}

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
    let mut lowerer = Lowerer::new(&source, &natives, Some(type_map));
    lowerer.set_dict_map(dict_map);
    let ir_test_module = match lowerer.lower_module_with_tests(&root) {
        Ok(m) => m,
        Err(e) => return empty_fail_result_msg(&e.message),
    };

    let test_count = ir_test_module.tests.len();
    if test_count == 0 {
        return empty_pass_result();
    }

    let mut execute_module = ir_test_module.into_executable_module();
    crate::vm::optimize_ir::optimize(&mut execute_module);

    let program = match crate::vm::jit::compile_with_natives(&execute_module, false, Some(&natives)) {
        Ok(p) => p,
        Err(e) => return empty_fail_result_msg(&format!("JIT compile error: {}", e)),
    };

    let passed = match program.run_entry_nvalue() {
        Some(val) => val.is_int() && val.as_int() == 0,
        None => false,
    };

    build_suite_result("JIT Execution", test_count, passed)
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
    let mut lowerer = Lowerer::new(&source, &natives, Some(type_map));
    lowerer.set_dict_map(dict_map);
    let ir_test_module = match lowerer.lower_module_with_tests(&root) {
        Ok(m) => m,
        Err(e) => return empty_fail_result_msg(&e.message),
    };

    let test_count = ir_test_module.tests.len();
    if test_count == 0 {
        return empty_pass_result();
    }

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

    build_suite_result("AOT Execution", test_count, passed)
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

fn build_suite_result(name: &str, count: usize, passed: bool) -> TestSuiteResult {
    let mut tests = Vec::new();
    for i in 0..count {
        tests.push(TestResult {
            name: format!("{} test {}", name, i),
            status: if passed { TestStatus::Pass } else { TestStatus::Fail },
            message: if passed { None } else { Some("Native execution failed".to_string()) },
            location: Location { file: String::new(), line: 0, col: 0, end_line: None, end_col: None },
        });
    }
    
    TestSuiteResult {
        status: if passed { "pass".to_string() } else { "fail".to_string() },
        tests,
        summary: TestSummary {
            total: count,
            passed: if passed { count } else { 0 },
            failed: if passed { 0 } else { count },
            skipped: 0,
        },
    }
}
