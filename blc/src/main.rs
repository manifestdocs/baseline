use clap::{Parser, Subcommand};
use std::path::{Path, PathBuf};

use blc::diagnostics::{self, CheckResult};
use blc::parse;
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

        /// Use the Cranelift JIT compiler (requires --features jit)
        #[arg(long)]
        jit: bool,

        /// Print heap allocation statistics after execution
        #[arg(long)]
        mem_stats: bool,

        /// Arguments to pass to the Baseline program (after --)
        #[arg(last = true)]
        program_args: Vec<String>,
    },

    /// Run inline tests in a Baseline source file
    Test {
        /// The file to test
        file: PathBuf,

        /// Output results as JSON
        #[arg(long)]
        json: bool,
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

    /// Compile a Baseline source file to a standalone native executable
    Build {
        /// The file to compile
        file: PathBuf,

        /// Output binary path (default: input filename stem)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Print compilation trace
        #[arg(long)]
        trace: bool,
    },

    /// Start the Constrained Generation Protocol (CGP) server
    Cgp {
        /// Port to listen on
        #[arg(long, default_value = "8765")]
        port: u16,
    },

    /// Initialize a new Baseline project
    Init {
        /// Project name (defaults to directory name)
        name: Option<String>,
    },

    /// Generate standard library documentation
    Docs {
        /// Output as JSON (default: markdown)
        #[arg(long)]
        json: bool,
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

            // Read source for source context in diagnostics
            let source_text = std::fs::read_to_string(&file).unwrap_or_default();

            if json {
                print_json_with_context(&result, &source_text);
            } else {
                print_human_readable(&result, &source_text);
            }

            if result.status == "failure" {
                std::process::exit(1);
            }
        }
        Commands::Run {
            file,
            jit,
            mem_stats,
            program_args,
        } => {
            vm::natives::set_program_args(program_args);
            if jit {
                run_file_jit(&file);
            } else {
                run_file_vm(&file);
            }
            if mem_stats {
                let stats = vm::nvalue::alloc_stats();
                eprintln!("[mem] {}", stats);
            }
        }
        Commands::Test { file, json, .. } => {
            let result = vm::test_runner::run_test_file(&file);
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
        Commands::Build {
            file,
            output,
            trace,
        } => {
            build_file_aot(&file, output.as_deref(), trace);
        }
        Commands::Cgp { port } => {
            blc::cgp::run_server(port);
        }
        Commands::Init { name } => {
            init_project(name);
        }
        Commands::Docs { json } => {
            let docs = blc::docs::generate_docs();
            if json {
                println!("{}", serde_json::to_string_pretty(&docs).unwrap());
            } else {
                print!("{}", blc::docs::render_markdown(&docs));
            }
        }
    }
}

fn init_project(name: Option<String>) {
    let cwd = std::env::current_dir().unwrap_or_else(|e| {
        eprintln!("Cannot determine current directory: {}", e);
        std::process::exit(1);
    });

    if cwd.join("baseline.toml").exists() {
        eprintln!("Error: baseline.toml already exists in this directory");
        std::process::exit(1);
    }

    let project_name = name.unwrap_or_else(|| {
        cwd.file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("my-app")
            .to_string()
    });

    std::fs::write(
        cwd.join("baseline.toml"),
        blc::manifest::create_manifest(&project_name),
    )
    .unwrap_or_else(|e| {
        eprintln!("Failed to write baseline.toml: {}", e);
        std::process::exit(1);
    });

    let src_dir = cwd.join("src");
    if !src_dir.exists() {
        std::fs::create_dir_all(&src_dir).unwrap_or_else(|e| {
            eprintln!("Failed to create src/ directory: {}", e);
            std::process::exit(1);
        });
    }

    let main_bl = src_dir.join("main.bl");
    if !main_bl.exists() {
        std::fs::write(&main_bl, blc::manifest::hello_world_source()).unwrap_or_else(|e| {
            eprintln!("Failed to write src/main.bl: {}", e);
            std::process::exit(1);
        });
    }

    println!("Created new Baseline project '{}'", project_name);
    println!("  baseline.toml");
    println!("  src/main.bl");
    println!();
    println!("Run with: blc run src/main.bl");
}

fn run_file_vm(file: &PathBuf) {
    use blc::resolver;
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

    let imports = resolver::ModuleLoader::parse_imports(&root, &source);

    // Run the type checker to build a TypeMap for opcode specialization.
    // For files with imports, use the loader-aware variant for transitive import support.
    let (type_diags, type_map) = if imports.is_empty() {
        blc::analysis::check_types_with_map(&root, &source, &file_str)
    } else {
        let base_dir = file.parent().expect("Cannot determine base directory");
        let mut loader = resolver::ModuleLoader::with_base_dir(base_dir.to_path_buf());
        blc::analysis::check_types_with_loader_and_map(
            &root,
            &source,
            &file_str,
            Some(&mut loader),
        )
    };
    let has_type_errors = type_diags
        .iter()
        .any(|d| d.severity == diagnostics::Severity::Error);
    let type_map = if has_type_errors {
        None
    } else {
        Some(type_map)
    };

    let mut vm_instance = vm::exec::Vm::new();

    let program = if imports.is_empty() {
        // Use the new IR pipeline: CST → lower → codegen → Program
        let mut lowerer = vm::lower::Lowerer::new(&source, vm_instance.natives(), type_map);
        let ir_module = lowerer.lower_module(&root);
        ir_module.and_then(|mut module| {
            vm::optimize_ir::optimize_for_bytecode(&mut module);
            let codegen = vm::codegen::Codegen::new(vm_instance.natives());
            codegen.generate_program(&module)
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

    let vm_instance = vm::exec::Vm::new();
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

/// Find the directory containing libbaseline_rt.a for AOT linking.
///
/// Search order:
/// 1. BASELINE_RT_LIB env var (explicit override)
/// 2. Cargo target directory relative to the blc binary
#[cfg(feature = "aot")]
fn find_baseline_rt_lib() -> Option<PathBuf> {
    // 1. Explicit env var
    if let Ok(dir) = std::env::var("BASELINE_RT_LIB") {
        let path = PathBuf::from(&dir);
        if path.is_dir() {
            return Some(path);
        }
    }

    // 2. Cargo target dir: check relative to CARGO_MANIFEST_DIR (build-time)
    //    or relative to the current executable
    if let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") {
        // During cargo run/test: CARGO_MANIFEST_DIR/target/{debug,release}
        // Pick the newest library if both exist to avoid stale-library bugs.
        let base = PathBuf::from(&manifest_dir).join("..");
        let mut best: Option<(PathBuf, std::time::SystemTime)> = None;
        for profile in &["debug", "release"] {
            let candidate = base.join("target").join(profile);
            let lib_path = candidate.join("libbaseline_rt.a");
            if let Ok(meta) = std::fs::metadata(&lib_path) {
                let mtime = meta.modified().unwrap_or(std::time::UNIX_EPOCH);
                if best.as_ref().map_or(true, |(_, t)| mtime > *t) {
                    best = Some((candidate, mtime));
                }
            }
        }
        if let Some((dir, _)) = best {
            return dir.canonicalize().ok();
        }
    }

    // 3. Relative to the blc binary itself
    if let Ok(exe) = std::env::current_exe() {
        if let Some(dir) = exe.parent() {
            if dir.join("libbaseline_rt.a").exists() {
                return Some(dir.to_path_buf());
            }
        }
    }

    None
}

#[cfg(feature = "aot")]
fn build_file_aot(file: &PathBuf, output: Option<&Path>, trace: bool) {
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    let source = std::fs::read_to_string(file).unwrap_or_else(|e| {
        eprintln!("Failed to read {}: {}", file.display(), e);
        std::process::exit(1);
    });
    let file_str = file.display().to_string();
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load language");
    let tree = parser.parse(&source, None).expect("Failed to parse");
    let root = tree.root_node();

    // Type-check (exit on errors)
    let (type_diags, type_map) = blc::analysis::check_types_with_map(&root, &source, &file_str);
    let has_type_errors = type_diags
        .iter()
        .any(|d| d.severity == diagnostics::Severity::Error);
    if has_type_errors {
        for d in &type_diags {
            if d.severity == diagnostics::Severity::Error {
                eprintln!(
                    "{}:{}:{}: error: {} [{}]",
                    d.location.file, d.location.line, d.location.col, d.message, d.code
                );
            }
        }
        std::process::exit(1);
    }

    // Lower to IR + optimize
    let vm_instance = vm::exec::Vm::new();
    let mut lowerer = vm::lower::Lowerer::new(&source, vm_instance.natives(), Some(type_map));
    let ir_module = match lowerer.lower_module(&root) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Compile Error: {}", e.message);
            std::process::exit(1);
        }
    };

    let mut optimized = ir_module;
    vm::optimize_ir::optimize(&mut optimized);

    // AOT compile
    let obj_bytes = match vm::jit::aot::compile_to_object(&optimized, trace) {
        Ok(bytes) => bytes,
        Err(e) => {
            eprintln!("AOT Error: {}", e);
            std::process::exit(1);
        }
    };

    // Determine output path
    let output_path = match output {
        Some(p) => p.to_path_buf(),
        None => {
            let stem = file.file_stem().unwrap_or_default();
            PathBuf::from(stem)
        }
    };

    // Find libbaseline_rt for linking
    let rt_lib_dir = find_baseline_rt_lib();
    if rt_lib_dir.is_none() && trace {
        eprintln!("AOT: warning: libbaseline_rt not found, linking without runtime");
    }

    // Link
    if let Err(e) =
        vm::jit::aot::link_executable(&obj_bytes, &output_path, rt_lib_dir.as_deref(), trace)
    {
        eprintln!("Link Error: {}", e);
        std::process::exit(1);
    }

    eprintln!("Built: {}", output_path.display());
}

#[cfg(not(feature = "aot"))]
fn build_file_aot(_file: &PathBuf, _output: Option<&Path>, _trace: bool) {
    eprintln!("Error: AOT compilation requires building with --features aot");
    eprintln!("  cargo build --features aot --release");
    std::process::exit(1);
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

fn print_human_readable(result: &CheckResult, source: &str) {
    if result.diagnostics.is_empty() {
        println!("✓ No errors found");
        return;
    }

    for diag in &result.diagnostics {
        let rendered = blc::diagnostic_render::render_diagnostic(diag, Some(source));
        eprint!("{}", rendered);
        eprintln!();
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
        (0, w) => eprintln!("{} warning(s)", w),
        (e, 0) => eprintln!("{} error(s)", e),
        (e, w) => eprintln!("{} error(s), {} warning(s)", e, w),
    }
}

/// Print JSON output with optional source_context fields embedded.
fn print_json_with_context(result: &CheckResult, source: &str) {
    let mut json_val = serde_json::to_value(result).unwrap();

    if let Some(diags) = json_val.get_mut("diagnostics").and_then(|v| v.as_array_mut()) {
        for (i, diag_val) in diags.iter_mut().enumerate() {
            if let Some(diag) = result.diagnostics.get(i)
                && let Some(ctx) = blc::diagnostic_render::build_source_context(diag, source)
                && let Some(obj) = diag_val.as_object_mut()
            {
                obj.insert(
                    "source_context".to_string(),
                    serde_json::to_value(&ctx).unwrap(),
                );
            }
        }
    }

    println!("{}", serde_json::to_string_pretty(&json_val).unwrap());
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
