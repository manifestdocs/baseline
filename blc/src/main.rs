use clap::{Parser, Subcommand};
use std::path::PathBuf;

use blc::diagnostics::{self, CheckResult};
use blc::interpreter;
use blc::parse;
use blc::prelude;
use blc::test_runner;

#[derive(Parser)]
#[command(name = "blc")]
#[command(about = "The Baseline language compiler")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Check a Baseline source file for errors
    Check {
        /// The file to check
        file: PathBuf,

        /// Output diagnostics as JSON (for LLM agents)
        #[arg(long)]
        json: bool,
    },

    /// Start the Language Server Protocol server
    Lsp,

    /// Execute a Baseline source file
    Run {
        /// The file to run
        file: PathBuf,
    },

    /// Run inline tests in a Baseline source file
    Test {
        /// The file to test
        file: PathBuf,

        /// Output results as JSON
        #[arg(long)]
        json: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Check { file, json } => {
            let result = check_file(&file);

            if json {
                println!("{}", serde_json::to_string_pretty(&result).unwrap());
            } else {
                print_human_readable(&result);
            }

            if result.status == "failure" {
                std::process::exit(1);
            }
        }
        Commands::Run { file } => {
             // 1. Parse
             use tree_sitter::Parser;
             use tree_sitter_baseline::LANGUAGE;

             let source = std::fs::read_to_string(&file).expect("Failed to read file");
             let mut parser = Parser::new();
             parser.set_language(&LANGUAGE.into()).expect("Failed to load language");
             let tree = parser.parse(&source, None).expect("Failed to parse");
             let root = tree.root_node();

             // 2. Extract prelude and create context
             let active_prelude = match prelude::extract_prelude(&root, &source) {
                 Ok(p) => p,
                 Err(msg) => {
                     eprintln!("Prelude Error: {}", msg);
                     std::process::exit(1);
                 }
             };
             let file_path = file.display().to_string();
             let mut context = interpreter::Context::with_prelude_and_file(active_prelude, file_path);

             // Evaluate top-level definitions (types/functions)
             if let Err(e) = interpreter::eval(&root, &source, &mut context) {
                 eprintln!("Runtime Error: {e}");
                 std::process::exit(1);
             }

             // 3. Find and run 'main' or 'main!'
             let main_val = context.get("main!").cloned()
                 .or_else(|| context.get("main").cloned());

             if let Some(main_val) = main_val {
                 let result = run_main(&main_val, &source, &mut context);
                 match result {
                     Ok(val) => {
                         // Unwrap EarlyReturn at the top level
                         let val = match val {
                             interpreter::RuntimeValue::EarlyReturn(inner) => *inner,
                             other => other,
                         };
                         if !matches!(val, interpreter::RuntimeValue::Unit) {
                             println!("{}", val);
                         }
                     }
                     Err(e) => {
                         eprintln!("Runtime Error: {e}");
                         std::process::exit(1);
                     }
                 }
             } else {
                 eprintln!("No 'main' or 'main!' function found");
                 std::process::exit(1);
             }
        }
        Commands::Test { file, json } => {
            let result = test_runner::run_test_file(&file);
            if json {
                println!("{}", serde_json::to_string_pretty(&result).unwrap());
            } else {
                print_test_results(&result);
            }
            if result.status == "fail" {
                std::process::exit(1);
            }
        }
        Commands::Lsp => {
            eprintln!("LSP server not yet implemented");
            std::process::exit(1);
        }
    }
}

/// Evaluate the main function body.
fn run_main<'a>(
    main_val: &interpreter::RuntimeValue<'a>,
    source: &str,
    context: &mut interpreter::Context<'a>,
) -> Result<interpreter::RuntimeValue<'a>, interpreter::RuntimeError> {
    match main_val {
        interpreter::RuntimeValue::Function(_, main_body) => {
            context.enter_scope();
            let result = interpreter::eval(main_body, source, context);
            context.exit_scope();
            result
        }
        interpreter::RuntimeValue::Closure(_, main_body, captured_env) => {
            context.enter_scope();
            for (k, v) in captured_env {
                context.set(k.clone(), v.clone());
            }
            let result = interpreter::eval(main_body, source, context);
            context.exit_scope();
            result
        }
        _ => Err(interpreter::RuntimeError::from("'main' is not a function".to_string())),
    }
}

fn check_file(path: &PathBuf) -> CheckResult {
    match parse::parse_file(path) {
        Ok(result) => result,
        Err(e) => CheckResult {
            status: "failure".to_string(),
            diagnostics: vec![diagnostics::Diagnostic {
                code: "IO_001".to_string(),
                severity: diagnostics::Severity::Error,
                location: diagnostics::Location {
                    file: path.display().to_string(),
                    line: 0,
                    col: 0,
                    end_line: None,
                    end_col: None,
                },
                message: format!("Failed to read file: {}", e),
                context: "Could not open or read the specified file.".to_string(),
                suggestions: vec![],
            }],
        },
    }
}

fn print_human_readable(result: &CheckResult) {
    if result.diagnostics.is_empty() {
        println!("✓ No errors found");
        return;
    }

    for diag in &result.diagnostics {
        let prefix = match diag.severity {
            diagnostics::Severity::Error => "error",
            diagnostics::Severity::Warning => "warning",
            diagnostics::Severity::Info => "info",
        };
        eprintln!(
            "{}:{}:{}: {}: {} [{}]",
            diag.location.file, diag.location.line, diag.location.col,
            prefix, diag.message, diag.code
        );
        if !diag.context.is_empty() {
            eprintln!("  {}", diag.context);
        }
        for suggestion in &diag.suggestions {
            eprintln!("  → {}: {}", suggestion.strategy, suggestion.description);
        }
    }

    let error_count = result.diagnostics.iter()
        .filter(|d| d.severity == diagnostics::Severity::Error)
        .count();
    let warning_count = result.diagnostics.iter()
        .filter(|d| d.severity == diagnostics::Severity::Warning)
        .count();

    match (error_count, warning_count) {
        (0, w) => eprintln!("\n{} warning(s)", w),
        (e, 0) => eprintln!("\n{} error(s)", e),
        (e, w) => eprintln!("\n{} error(s), {} warning(s)", e, w),
    }
}

fn print_test_results(result: &test_runner::TestSuiteResult) {
    for test in &result.tests {
        let context = match &test.function {
            Some(f) => format!("{}, line {}", f, test.location.line),
            None => format!("line {}", test.location.line),
        };
        match test.status {
            test_runner::TestStatus::Pass => {
                println!("PASS  {} ({})", test.name, context);
            }
            test_runner::TestStatus::Fail => {
                println!("FAIL  {} ({})", test.name, context);
                if let Some(msg) = &test.message {
                    println!("  {}", msg);
                }
            }
        }
    }

    println!(
        "\n{} tests: {} passed, {} failed",
        result.summary.total, result.summary.passed, result.summary.failed
    );
}
