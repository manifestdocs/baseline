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
