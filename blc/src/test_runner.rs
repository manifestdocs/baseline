use std::path::Path;

use serde::Serialize;
use tree_sitter::{Node, Parser};
use tree_sitter_baseline::LANGUAGE;

use crate::diagnostics::Location;
use crate::interpreter::{self, Context, RuntimeValue};
use crate::prelude;
use crate::resolver::{self, ImportKind, ModuleLoader};

// ---------------------------------------------------------------------------
// Result types
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize)]
pub struct TestResult {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub function: Option<String>,
    pub status: TestStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    pub location: Location,
}

#[derive(Debug, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum TestStatus {
    Pass,
    Fail,
    #[serde(rename = "skipped")]
    Skip,
}

#[derive(Debug, Serialize)]
pub struct TestSuiteResult {
    pub status: String,
    pub tests: Vec<TestResult>,
    pub summary: TestSummary,
}

#[derive(Debug, Serialize)]
pub struct TestSummary {
    pub total: usize,
    pub passed: usize,
    pub failed: usize,
    #[serde(skip_serializing_if = "is_zero")]
    pub skipped: usize,
}

fn is_zero(n: &usize) -> bool {
    *n == 0
}

// ---------------------------------------------------------------------------
// Test collection
// ---------------------------------------------------------------------------

struct CollectedTest<'a> {
    name: String,
    function: Option<String>,
    expr_node: Node<'a>,
    location: Location,
    skip: bool,
}

/// A BDD describe/context block with hooks and nested tests.
#[allow(dead_code)]
struct DescribeBlock<'a> {
    name: String,
    before_each: Vec<Node<'a>>,
    after_each: Vec<Node<'a>>,
    tests: Vec<CollectedTest<'a>>,
    children: Vec<DescribeBlock<'a>>,
}

fn collect_tests<'a>(root: &Node<'a>, source: &str, file: &str) -> Vec<CollectedTest<'a>> {
    let mut tests = Vec::new();
    let mut cursor = root.walk();

    // Check for focused tests (it.only) — if any exist, only run those
    let has_only = has_focused_tests(root, source);

    for child in root.children(&mut cursor) {
        // Unwrap spec_block to access the inner function_def
        let effective = if child.kind() == "spec_block" {
            let mut found = None;
            let mut sc = child.walk();
            for c in child.named_children(&mut sc) {
                if c.kind() == "function_def" {
                    found = Some(c);
                    break;
                }
            }
            found.unwrap_or(child)
        } else {
            child
        };
        match effective.kind() {
            "function_def" => {
                let func_name = effective
                    .child_by_field_name("name")
                    .map(|n| n.utf8_text(source.as_bytes()).unwrap_or("").to_string());

                let mut child_cursor = effective.walk();
                for fc in effective.children(&mut child_cursor) {
                    if fc.kind() == "where_block" {
                        let mut wb_cursor = fc.walk();
                        for test_node in fc.children(&mut wb_cursor) {
                            if test_node.kind() == "inline_test"
                                && let Some(ct) =
                                    parse_inline_test(&test_node, source, file, &func_name)
                            {
                                tests.push(ct);
                            }
                        }
                    }
                }
            }
            "inline_test" => {
                if let Some(ct) = parse_inline_test(&effective, source, file, &None) {
                    tests.push(ct);
                }
            }
            "describe_block" => {
                let block = collect_describe_block(&effective, source, file, "", has_only);
                flatten_describe_block(block, &mut tests);
            }
            _ => {}
        }
    }

    tests
}

fn has_focused_tests(node: &Node, source: &str) -> bool {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "it_block" {
            if let Some(mod_node) = child.child_by_field_name("modifier") {
                let modifier = mod_node.utf8_text(source.as_bytes()).unwrap_or("");
                if modifier == ".only" {
                    return true;
                }
            }
        }
        if child.kind() == "describe_block" && has_focused_tests(&child, source) {
            return true;
        }
    }
    false
}

fn collect_describe_block<'a>(
    node: &Node<'a>,
    source: &str,
    file: &str,
    prefix: &str,
    has_only: bool,
) -> DescribeBlock<'a> {
    let name_node = node.child_by_field_name("name");
    let raw_name = name_node
        .map(|n| {
            let text = n.utf8_text(source.as_bytes()).unwrap_or("");
            text.strip_prefix('"')
                .and_then(|s| s.strip_suffix('"'))
                .unwrap_or(text)
                .to_string()
        })
        .unwrap_or_default();

    let full_name = if prefix.is_empty() {
        raw_name.clone()
    } else {
        format!("{} > {}", prefix, raw_name)
    };

    let mut block = DescribeBlock {
        name: full_name.clone(),
        before_each: Vec::new(),
        after_each: Vec::new(),
        tests: Vec::new(),
        children: Vec::new(),
    };

    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        match child.kind() {
            "it_block" => {
                if let Some(ct) = parse_it_block(&child, source, file, &full_name, has_only) {
                    block.tests.push(ct);
                }
            }
            "inline_test" => {
                if let Some(mut ct) = parse_inline_test(&child, source, file, &None) {
                    ct.name = format!("{} > {}", full_name, ct.name);
                    block.tests.push(ct);
                }
            }
            "before_each_block" => {
                // Last named child is the expression
                let count = child.named_child_count();
                if count > 0 {
                    if let Some(expr) = child.named_child(count - 1) {
                        block.before_each.push(expr);
                    }
                }
            }
            "after_each_block" => {
                let count = child.named_child_count();
                if count > 0 {
                    if let Some(expr) = child.named_child(count - 1) {
                        block.after_each.push(expr);
                    }
                }
            }
            "describe_block" => {
                let nested = collect_describe_block(&child, source, file, &full_name, has_only);
                block.children.push(nested);
            }
            _ => {}
        }
    }

    block
}

fn parse_it_block<'a>(
    node: &Node<'a>,
    source: &str,
    file: &str,
    prefix: &str,
    has_only: bool,
) -> Option<CollectedTest<'a>> {
    let name_node = node.child_by_field_name("name")?;
    let raw_name = name_node.utf8_text(source.as_bytes()).ok()?;
    let name = raw_name
        .strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(raw_name)
        .to_string();

    let full_name = if prefix.is_empty() {
        name
    } else {
        format!("{} > {}", prefix, name)
    };

    let body_node = node.child_by_field_name("body")?;

    let modifier = node
        .child_by_field_name("modifier")
        .map(|n| n.utf8_text(source.as_bytes()).unwrap_or("").to_string());

    let skip = match modifier.as_deref() {
        Some(".skip") => true,
        Some(".only") => false,
        _ => has_only, // If any test has .only, skip non-only tests
    };

    let start = node.start_position();
    let end = node.end_position();
    let location = Location {
        file: file.to_string(),
        line: start.row + 1,
        col: start.column + 1,
        end_line: Some(end.row + 1),
        end_col: Some(end.column + 1),
    };

    Some(CollectedTest {
        name: full_name,
        function: None,
        expr_node: body_node,
        location,
        skip,
    })
}

fn flatten_describe_block<'a>(block: DescribeBlock<'a>, out: &mut Vec<CollectedTest<'a>>) {
    for test in block.tests {
        out.push(test);
    }
    for child in block.children {
        flatten_describe_block(child, out);
    }
}

fn parse_inline_test<'a>(
    node: &Node<'a>,
    source: &str,
    file: &str,
    function: &Option<String>,
) -> Option<CollectedTest<'a>> {
    // inline_test: "test" string_literal "=" _expression
    let count = node.named_child_count();
    if count < 2 {
        return None;
    }

    // First named child is the string_literal (test name)
    let name_node = node.named_child(0)?;
    let raw_name = name_node.utf8_text(source.as_bytes()).ok()?;
    // Strip surrounding quotes
    let name = raw_name
        .strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(raw_name)
        .to_string();

    // Last named child is the expression
    let expr_node = node.named_child(count - 1)?;

    let start = node.start_position();
    let end = node.end_position();
    let location = Location {
        file: file.to_string(),
        line: start.row + 1,
        col: start.column + 1,
        end_line: Some(end.row + 1),
        end_col: Some(end.column + 1),
    };

    Some(CollectedTest {
        name,
        function: function.clone(),
        expr_node,
        location,
        skip: false,
    })
}

// ---------------------------------------------------------------------------
// Test execution
// ---------------------------------------------------------------------------

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
                function: None,
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

    // Build interpreter context with all definitions
    let active_prelude = prelude::extract_prelude(&root, &source).unwrap_or(prelude::Prelude::Core);

    // Create ModuleLoader for imports — leak for lifetime compatibility
    let loader: &'static mut ModuleLoader = match path.parent() {
        Some(dir) => Box::leak(Box::new(ModuleLoader::with_base_dir(dir.to_path_buf()))),
        None => Box::leak(Box::new(ModuleLoader::new())),
    };

    let mut context = Context::with_prelude_and_file(active_prelude, file_str.clone());

    // Inject imports
    if let Err(msg) = inject_imports_for_test(&root, &source, path, loader, &mut context) {
        return TestSuiteResult {
            status: "fail".to_string(),
            tests: vec![TestResult {
                name: "imports".to_string(),
                function: None,
                status: TestStatus::Fail,
                message: Some(format!("Import error: {}", msg)),
                location: Location {
                    file: file_str,
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

    if let Err(e) = interpreter::eval(&root, &source, &mut context) {
        return TestSuiteResult {
            status: "fail".to_string(),
            tests: vec![TestResult {
                name: "evaluation".to_string(),
                function: None,
                status: TestStatus::Fail,
                message: Some(format!("Runtime error during setup: {}", e)),
                location: Location {
                    file: file_str,
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

    // Collect and run tests
    let collected = collect_tests(&root, &source, &file_str);
    let mut results = Vec::new();

    for test in &collected {
        let result = run_single_test(test, &source, &mut context);
        results.push(result);
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

fn inject_imports_for_test<'a>(
    root: &tree_sitter::Node<'a>,
    source: &str,
    file: &Path,
    loader: &'a mut ModuleLoader,
    context: &mut Context<'a>,
) -> Result<(), String> {
    let imports = ModuleLoader::parse_imports(root, source);
    let file_str = file.display().to_string();

    // Phase 1: resolve and load all modules (needs &mut loader)
    let mut resolved: Vec<(resolver::ResolvedImport, usize)> = Vec::new();
    for (import, import_node) in &imports {
        let file_path = loader
            .resolve_path(&import.module_name, import_node, &file_str)
            .map_err(|d| d.message.clone())?;
        let module_idx = loader
            .load_module(&file_path, import_node, &file_str)
            .map_err(|d| d.message.clone())?;
        resolved.push((import.clone(), module_idx));
    }

    // Phase 2: eval each module and inject into context (needs &loader)
    for (import, module_idx) in &resolved {
        let (mod_root, mod_source, mod_path) = loader
            .get_module(*module_idx)
            .ok_or_else(|| format!("Failed to get module {}", import.module_name))?;

        let mod_prelude =
            prelude::extract_prelude(&mod_root, mod_source).unwrap_or(prelude::Prelude::Core);

        let mod_file = mod_path.display().to_string();
        let mut mod_context = Context::with_prelude_and_file(mod_prelude, mod_file);

        interpreter::eval(&mod_root, mod_source, &mut mod_context)
            .map_err(|e| format!("Error evaluating module `{}`: {}", import.module_name, e))?;

        let exports = ModuleLoader::extract_exports(&mod_root, mod_source);
        let mut record_fields = std::collections::HashMap::new();

        for (func_name, _) in &exports.functions {
            if let Some(val) = mod_context.get(func_name) {
                record_fields.insert(func_name.clone(), val.clone());
            }
        }

        for type_name in &exports.types {
            if let Some(val) = mod_context.get(type_name) {
                record_fields.insert(type_name.clone(), val.clone());
            }
        }

        let short_name = import
            .module_name
            .split('.')
            .next_back()
            .unwrap_or(&import.module_name);

        context.set(
            short_name.to_string(),
            RuntimeValue::Record(record_fields.clone()),
        );

        match &import.kind {
            ImportKind::Selective(names) => {
                for name in names {
                    if let Some(val) = record_fields.get(name) {
                        context.set(name.clone(), val.clone());
                    }
                }
            }
            ImportKind::Wildcard => {
                for (name, val) in &record_fields {
                    context.set(name.clone(), val.clone());
                }
            }
            ImportKind::Qualified => {}
        }
    }

    Ok(())
}

fn run_single_test<'a>(
    test: &CollectedTest<'a>,
    source: &'a str,
    context: &mut Context<'a>,
) -> TestResult {
    if test.skip {
        return TestResult {
            name: test.name.clone(),
            function: test.function.clone(),
            status: TestStatus::Skip,
            message: None,
            location: test.location.clone(),
        };
    }

    context.enter_scope();
    let result = interpreter::eval(&test.expr_node, source, context);
    context.exit_scope();

    match result {
        Ok(RuntimeValue::Bool(true)) => TestResult {
            name: test.name.clone(),
            function: test.function.clone(),
            status: TestStatus::Pass,
            message: None,
            location: test.location.clone(),
        },
        Ok(RuntimeValue::Bool(false)) => TestResult {
            name: test.name.clone(),
            function: test.function.clone(),
            status: TestStatus::Fail,
            message: Some("Expected true, got false".to_string()),
            location: test.location.clone(),
        },
        Ok(other) => TestResult {
            name: test.name.clone(),
            function: test.function.clone(),
            status: TestStatus::Fail,
            message: Some(format!("Expected Bool, got {}", other)),
            location: test.location.clone(),
        },
        Err(e) => TestResult {
            name: test.name.clone(),
            function: test.function.clone(),
            status: TestStatus::Fail,
            message: Some(format!("Runtime error: {}", e)),
            location: test.location.clone(),
        },
    }
}
