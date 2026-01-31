use clap::{Parser, Subcommand};
use std::path::PathBuf;

use blc::diagnostics::{self, CheckResult};
use blc::interpreter;
use blc::parse;

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
             
             // 2. Interpreter
             let mut context = interpreter::Context::new();
             
             // Evaluate top-level definitions (types/functions)
             if let Err(e) = interpreter::eval(&root, &source, &mut context) {
                 eprintln!("Runtime Error: {}", e);
                 std::process::exit(1);
             }
             
             // 3. Find and run 'main'
             if let Some(main_val) = context.get("main").cloned() {
                 if let interpreter::RuntimeValue::Function(_, main_body) = main_val {
                      // Execute the body of main in a new scope (no arguments)
                      context.enter_scope();
                      match interpreter::eval(&main_body, &source, &mut context) {
                          Ok(val) => println!("{}", val),
                          Err(e) => {
                              eprintln!("Runtime Error in main: {}", e);
                              std::process::exit(1);
                          }
                      }
                      context.exit_scope();
                 } else {
                     eprintln!("'main' is not a function");
                     std::process::exit(1);
                 }
             } else {
                 eprintln!("No 'main' function found");
                 std::process::exit(1);
             }
        }
        Commands::Lsp => {
            eprintln!("LSP server not yet implemented");
            std::process::exit(1);
        }
    }
}

fn check_file(path: &PathBuf) -> CheckResult {
    match parse::parse_file(path) {
        Ok(result) => result,
        Err(e) => CheckResult {
            status: "failure".to_string(),
            diagnostics: vec![diagnostics::Diagnostic {
                code: "IO_001".to_string(),
                severity: "error".to_string(),
                location: diagnostics::Location {
                    file: path.display().to_string(),
                    line: 0,
                    col: 0,
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
        eprintln!(
            "{}:{}:{}: {} [{}]",
            diag.location.file, diag.location.line, diag.location.col, diag.message, diag.code
        );
        if !diag.context.is_empty() {
            eprintln!("  {}", diag.context);
        }
        for suggestion in &diag.suggestions {
            eprintln!("  → {}: {}", suggestion.strategy, suggestion.description);
        }
    }
    eprintln!("\n{} error(s) found", result.diagnostics.len());
}
