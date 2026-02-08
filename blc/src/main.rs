use clap::{Parser, Subcommand};
use std::path::{Path, PathBuf};

use blc::diagnostics::{self, CheckResult};
use blc::interpreter;
use blc::parse;
use blc::prelude;
use blc::resolver::{self, ImportKind, ModuleLoader};
use blc::test_runner;
use blc::vm;

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

        /// Verification level: types, refinements, or full (includes SMT spec checking)
        #[arg(long, default_value = "refinements")]
        level: String,
    },

    /// Start the Language Server Protocol server
    Lsp,

    /// Execute a Baseline source file
    Run {
        /// The file to run
        file: PathBuf,

        /// Use the legacy tree-walk interpreter (debug only, deprecated)
        #[arg(long)]
        interp: bool,

        /// Use the bytecode VM (default, kept for backward compatibility)
        #[arg(long, hide = true)]
        vm: bool,

        /// Use the Cranelift JIT compiler (requires --features jit)
        #[arg(long)]
        jit: bool,

        /// Use the LLVM JIT backend (requires --features llvm)
        #[arg(long)]
        llvm: bool,

        /// Print heap allocation statistics after execution
        #[arg(long)]
        mem_stats: bool,
    },

    /// Run inline tests in a Baseline source file
    Test {
        /// The file to test
        file: PathBuf,

        /// Output results as JSON
        #[arg(long)]
        json: bool,

        /// Use the legacy tree-walk interpreter (debug only, deprecated)
        #[arg(long)]
        interp: bool,

        /// Use the bytecode VM (default, kept for backward compatibility)
        #[arg(long, hide = true)]
        vm: bool,
    },

    /// Format a Baseline source file
    #[command(name = "fmt")]
    Format {
        /// The file to format
        file: PathBuf,

        /// Check formatting without modifying the file (exit 1 if changes needed)
        #[arg(long)]
        check: bool,
    },

    /// Start the Constrained Generation Protocol (CGP) server
    Cgp {
        /// Port to listen on
        #[arg(long, default_value = "8765")]
        port: u16,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Check { file, json, level } => {
            // Parse verification level from command line
            let ver_level: diagnostics::VerificationLevel = level.parse().unwrap_or_else(|e| {
                eprintln!("{}", e);
                std::process::exit(1);
            });

            let mut result = check_file(&file);
            // Update the verification level in the result
            result.verification_level = ver_level.clone();
            result.checked = match ver_level {
                diagnostics::VerificationLevel::Types => vec!["types".to_string()],
                diagnostics::VerificationLevel::Refinements => {
                    vec!["types".to_string(), "refinements".to_string()]
                }
                diagnostics::VerificationLevel::Full => vec![
                    "types".to_string(),
                    "refinements".to_string(),
                    "specs".to_string(),
                    "smt".to_string(),
                ],
                diagnostics::VerificationLevel::Skip => vec!["types".to_string()],
            };
            result.unchecked = match ver_level {
                diagnostics::VerificationLevel::Types => vec![
                    "refinements".to_string(),
                    "specs".to_string(),
                    "smt".to_string(),
                ],
                diagnostics::VerificationLevel::Refinements => {
                    vec!["specs".to_string(), "smt".to_string()]
                }
                diagnostics::VerificationLevel::Full => vec![],
                diagnostics::VerificationLevel::Skip => vec![
                    "refinements".to_string(),
                    "specs".to_string(),
                    "smt".to_string(),
                ],
            };

            // Run SMT verification at --level=full
            if ver_level == diagnostics::VerificationLevel::Full {
                let source = std::fs::read_to_string(&file).unwrap_or_default();
                let mut parser = tree_sitter::Parser::new();
                parser
                    .set_language(&tree_sitter_baseline::LANGUAGE.into())
                    .expect("Failed to load grammar");
                if let Some(tree) = parser.parse(&source, None) {
                    let file_name = file.display().to_string();
                    let smt_diags = blc::analysis::check_specs(&tree, &source, &file_name, 5000);
                    let has_smt_errors = smt_diags
                        .iter()
                        .any(|d| d.severity == diagnostics::Severity::Error);
                    result.diagnostics.extend(smt_diags);
                    if has_smt_errors && result.status != "failure" {
                        result.status = "failure".to_string();
                    }
                }
            }

            if json {
                println!("{}", serde_json::to_string_pretty(&result).unwrap());
            } else {
                print_human_readable(&result);
            }

            if result.status == "failure" {
                std::process::exit(1);
            }
        }
        Commands::Run {
            file,
            interp,
            jit,
            llvm,
            mem_stats,
            ..
        } => {
            if interp {
                eprintln!("Warning: --interp is deprecated. The bytecode VM is the default and recommended backend.");
                run_file(&file);
            } else if jit {
                run_file_jit(&file);
            } else if llvm {
                run_file_llvm(&file);
            } else {
                run_file_vm(&file);
            }
            if mem_stats {
                let stats = vm::nvalue::alloc_stats();
                eprintln!("[mem] {}", stats);
            }
        }
        Commands::Test {
            file, json, interp, ..
        } => {
            let result = if interp {
                test_runner::run_test_file(&file)
            } else {
                vm::test_runner::run_test_file(&file)
            };
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
            let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime");
            rt.block_on(blc::lsp::run_server());
        }
        Commands::Format { file, check } => {
            let source = std::fs::read_to_string(&file).unwrap_or_else(|e| {
                eprintln!("Failed to read {}: {}", file.display(), e);
                std::process::exit(1);
            });

            let formatted = blc::format::format_source(&source).unwrap_or_else(|e| {
                eprintln!("Format error: {}", e);
                std::process::exit(1);
            });

            if check {
                if source != formatted {
                    eprintln!("{} needs formatting", file.display());
                    std::process::exit(1);
                }
            } else {
                std::fs::write(&file, &formatted).unwrap_or_else(|e| {
                    eprintln!("Failed to write {}: {}", file.display(), e);
                    std::process::exit(1);
                });
            }
        }
        Commands::Cgp { port } => {
            blc::cgp::run_server(port);
        }
    }
}

fn run_file_vm(file: &PathBuf) {
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    let source = std::fs::read_to_string(file).expect("Failed to read file");
    let file_str = file.display().to_string();
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load language");
    let tree = parser.parse(&source, None).expect("Failed to parse");
    let root = tree.root_node();

    // Run the type checker to build a TypeMap for opcode specialization.
    // If analysis produces errors, degrade gracefully (compile without TypeMap).
    let (type_diags, type_map) = blc::analysis::check_types_with_map(&root, &source, &file_str);
    let has_type_errors = type_diags
        .iter()
        .any(|d| d.severity == diagnostics::Severity::Error);
    let type_map = if has_type_errors {
        None
    } else {
        Some(type_map)
    };

    let mut vm_instance = vm::vm::Vm::new();

    let imports = resolver::ModuleLoader::parse_imports(&root, &source);
    let program = if imports.is_empty() {
        // Use the new IR pipeline: CST → lower → codegen → Program
        let mut lowerer = vm::lower::Lowerer::new(&source, vm_instance.natives(), type_map);
        let ir_module = lowerer
            .lower_module(&root)
            .map_err(|e| vm::compiler::CompileError {
                message: e.message,
                line: e.line,
                col: e.col,
            });
        ir_module.and_then(|mut module| {
            vm::optimize_ir::optimize(&mut module);
            let codegen = vm::codegen::Codegen::new(vm_instance.natives());
            codegen
                .generate_program(&module)
                .map_err(|e| vm::compiler::CompileError {
                    message: e.message,
                    line: e.line,
                    col: e.col,
                })
        })
    } else {
        vm::module_compiler::compile_with_imports(
            &source,
            &root,
            file.as_path(),
            vm_instance.natives(),
        )
    };

    let mut program = match program {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Compile Error: {}", e);
            std::process::exit(1);
        }
    };
    program.optimize();
    match vm_instance.execute_program(&program) {
        Ok(val) => {
            if !matches!(val, vm::value::Value::Unit) {
                println!("{}", val);
            }
        }
        Err(e) => {
            eprintln!(
                "Runtime Error: {}:{}:{}: {}",
                file.display(),
                e.line,
                e.col,
                e.message
            );
            std::process::exit(1);
        }
    }
}

#[cfg(feature = "jit")]
fn run_file_jit(file: &PathBuf) {
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    let source = std::fs::read_to_string(file).expect("Failed to read file");
    let file_str = file.display().to_string();
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load language");
    let tree = parser.parse(&source, None).expect("Failed to parse");
    let root = tree.root_node();

    let (type_diags, type_map) = blc::analysis::check_types_with_map(&root, &source, &file_str);
    let has_type_errors = type_diags
        .iter()
        .any(|d| d.severity == diagnostics::Severity::Error);
    let type_map = if has_type_errors {
        None
    } else {
        Some(type_map)
    };

    let vm_instance = vm::vm::Vm::new();
    let mut lowerer = vm::lower::Lowerer::new(&source, vm_instance.natives(), type_map);
    let ir_module = match lowerer.lower_module(&root) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Compile Error: {}", e.message);
            std::process::exit(1);
        }
    };

    let mut optimized = ir_module;
    vm::optimize_ir::optimize(&mut optimized);

    let jit_program =
        match vm::jit::compile_with_natives(&optimized, false, Some(vm_instance.natives())) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("JIT Error: {}", e);
                std::process::exit(1);
            }
        };

    match jit_program.run_entry_nvalue() {
        Some(val) => {
            // Check for runtime errors stored in the thread-local error slot
            if let Some(err) = vm::jit::jit_take_error() {
                eprintln!("Runtime Error: {}", err);
                std::process::exit(1);
            }
            if !val.is_unit() {
                println!("{}", val);
            }
        }
        None => {
            eprintln!("JIT Error: entry function was not compiled");
            std::process::exit(1);
        }
    }
}

#[cfg(not(feature = "jit"))]
fn run_file_jit(_file: &PathBuf) {
    eprintln!("Error: JIT support requires building with --features jit");
    eprintln!("  cargo build --features jit --release");
    std::process::exit(1);
}

#[cfg(feature = "llvm")]
fn run_file_llvm(file: &PathBuf) {
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    let source = std::fs::read_to_string(file).expect("Failed to read file");
    let file_str = file.display().to_string();
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load language");
    let tree = parser.parse(&source, None).expect("Failed to parse");
    let root = tree.root_node();

    let (type_diags, type_map) = blc::analysis::check_types_with_map(&root, &source, &file_str);
    let has_type_errors = type_diags
        .iter()
        .any(|d| d.severity == diagnostics::Severity::Error);
    let type_map = if has_type_errors {
        None
    } else {
        Some(type_map)
    };

    let vm_instance = vm::vm::Vm::new();
    let mut lowerer = vm::lower::Lowerer::new(&source, vm_instance.natives(), type_map);
    let ir_module = match lowerer.lower_module(&root) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Compile Error: {}", e.message);
            std::process::exit(1);
        }
    };

    let mut optimized = ir_module;
    vm::optimize_ir::optimize(&mut optimized);

    let trace = std::env::var("BLC_TRACE").is_ok();
    let program = match vm::llvm_backend::compile(&optimized, trace) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("LLVM Error: {}", e);
            std::process::exit(1);
        }
    };

    let result = program.run_entry();
    if result != 0 {
        println!("{}", result);
    }
}

#[cfg(not(feature = "llvm"))]
fn run_file_llvm(_file: &PathBuf) {
    eprintln!("Error: LLVM support requires building with --features llvm");
    eprintln!(
        "  LLVM_SYS_181_PREFIX=/opt/homebrew/opt/llvm@18 cargo build --features llvm --release"
    );
    std::process::exit(1);
}

fn run_file(file: &PathBuf) {
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    let source = std::fs::read_to_string(file).expect("Failed to read file");
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load language");
    let tree = parser.parse(&source, None).expect("Failed to parse");
    let root = tree.root_node();

    let active_prelude = match prelude::extract_prelude(&root, &source) {
        Ok(p) => p,
        Err(msg) => {
            eprintln!("Prelude Error: {}", msg);
            std::process::exit(1);
        }
    };
    let file_path = file.display().to_string();

    // Leak the loader so imported tree-sitter Nodes get 'static lifetime
    let loader: &'static mut ModuleLoader = match file.parent() {
        Some(dir) => Box::leak(Box::new(ModuleLoader::with_base_dir(dir.to_path_buf()))),
        None => Box::leak(Box::new(ModuleLoader::new())),
    };

    let mut context = interpreter::Context::with_prelude_and_file(active_prelude, file_path);

    if let Err(msg) = inject_imports(&root, &source, file, loader, &mut context) {
        eprintln!("Import Error: {}", msg);
        std::process::exit(1);
    }

    if let Err(e) = interpreter::eval(&root, &source, &mut context) {
        eprintln!("Runtime Error: {e}");
        std::process::exit(1);
    }

    let main_val = context
        .get("main!")
        .cloned()
        .or_else(|| context.get("main").cloned());

    if let Some(main_val) = main_val {
        let result = run_main(&main_val, &mut context);
        match result {
            Ok(val) => {
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

/// Pre-inject imported modules into the interpreter context.
fn inject_imports<'a>(
    root: &tree_sitter::Node<'a>,
    source: &str,
    file: &Path,
    loader: &'a mut ModuleLoader,
    context: &mut interpreter::Context<'a>,
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

    // Phase 2: eval each module and inject into context
    for (import, module_idx) in &resolved {
        let (mod_root, mod_source, mod_path) = loader
            .get_module(*module_idx)
            .ok_or_else(|| format!("Failed to get module {}", import.module_name))?;

        let mod_prelude =
            prelude::extract_prelude(&mod_root, mod_source).unwrap_or(prelude::Prelude::Core);

        let mod_file = mod_path.display().to_string();
        let mut mod_context = interpreter::Context::with_prelude_and_file(mod_prelude, mod_file);

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
            .rsplit('.')
            .next()
            .unwrap_or(&import.module_name);

        context.set(
            short_name.to_string(),
            interpreter::RuntimeValue::Record(record_fields.clone()),
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

/// Evaluate the main function body.
fn run_main<'a>(
    main_val: &interpreter::RuntimeValue<'a>,
    context: &mut interpreter::Context<'a>,
) -> Result<interpreter::RuntimeValue<'a>, interpreter::RuntimeError> {
    match main_val {
        interpreter::RuntimeValue::Function(_, main_body, fn_source) => {
            context.enter_scope();
            let result = interpreter::eval(main_body, fn_source, context);
            context.exit_scope();
            result
        }
        interpreter::RuntimeValue::Closure(_, main_body, fn_source, captured_env) => {
            context.enter_scope();
            for (k, v) in captured_env {
                context.set(k.clone(), v.clone());
            }
            let result = interpreter::eval(main_body, fn_source, context);
            context.exit_scope();
            result
        }
        _ => Err(interpreter::RuntimeError::from(
            "'main' is not a function".to_string(),
        )),
    }
}

fn check_file(path: &Path) -> CheckResult {
    match parse::parse_file(path) {
        Ok(result) => result,
        Err(e) => {
            let mut result = CheckResult::new(diagnostics::VerificationLevel::Types);
            result.status = "failure".to_string();
            result.diagnostics.push(diagnostics::Diagnostic {
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
            });
            result
        }
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
            diag.location.file,
            diag.location.line,
            diag.location.col,
            prefix,
            diag.message,
            diag.code
        );
        if !diag.context.is_empty() {
            eprintln!("  {}", diag.context);
        }
        for suggestion in &diag.suggestions {
            eprintln!("  → {}: {}", suggestion.strategy, suggestion.description);
        }
    }

    let error_count = result
        .diagnostics
        .iter()
        .filter(|d| d.severity == diagnostics::Severity::Error)
        .count();
    let warning_count = result
        .diagnostics
        .iter()
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
            test_runner::TestStatus::Skip => {
                println!("SKIP  {} ({})", test.name, context);
            }
        }
    }

    if result.summary.skipped > 0 {
        println!(
            "\n{} tests: {} passed, {} failed, {} skipped",
            result.summary.total,
            result.summary.passed,
            result.summary.failed,
            result.summary.skipped
        );
    } else {
        println!(
            "\n{} tests: {} passed, {} failed",
            result.summary.total, result.summary.passed, result.summary.failed
        );
    }
}
