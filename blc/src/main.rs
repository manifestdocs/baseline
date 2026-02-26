use clap::{Parser, Subcommand};
use std::path::{Path, PathBuf};

use blc::diagnostics::{self, CheckResult};
use blc::parse;
use blc::resolver;
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

        /// Print heap allocation statistics after execution
        #[arg(long)]
        mem_stats: bool,

        /// Restrict filesystem access to the given directory
        #[arg(long)]
        fs_sandbox: Option<PathBuf>,

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

        /// Run tests using the AOT compiler
        #[arg(long)]
        aot: bool,
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

    /// Start the MCP (Model Context Protocol) server over stdio
    #[cfg(feature = "mcp")]
    Mcp,

    /// Generate standard library documentation
    Docs {
        /// Output as JSON (default: markdown)
        #[arg(long)]
        json: bool,

        /// Search query (e.g. "List.map" or "filter")
        #[arg(long)]
        search: Option<String>,

        /// Look up a specific function by qualified name (e.g. List.map)
        query: Option<String>,
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
            mem_stats,
            fs_sandbox,
            program_args,
        } => {
            vm::natives::set_program_args(program_args);
            vm::natives::set_fs_sandbox(fs_sandbox);
            run_file_jit(&file);
            if mem_stats {
                let stats = vm::nvalue::alloc_stats();
                eprintln!("[mem] {}", stats);
            }
        }
        Commands::Test { file, json, aot } => {
            let result = if aot {
                #[cfg(feature = "aot")]
                {
                    vm::test_runner::run_test_file_aot(&file)
                }
                #[cfg(not(feature = "aot"))]
                {
                    eprintln!("Error: AOT testing requires building with --features aot");
                    std::process::exit(1);
                }
            } else {
                vm::test_runner::run_test_file_jit(&file)
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

        #[cfg(feature = "mcp")]
        Commands::Mcp => {
            let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime");
            rt.block_on(blc::mcp::run_server());
        }
        Commands::Docs {
            json,
            search,
            query,
        } => {
            let docs = blc::docs::generate_docs();

            // Apply search/query filter if provided
            let q = query.or(search);
            let docs = match &q {
                Some(query) => blc::docs::filter_docs(&docs, query),
                None => docs,
            };

            if q.is_some() && docs.modules.is_empty() {
                eprintln!("No matching functions found.");
                std::process::exit(1);
            }

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

// ---------------------------------------------------------------------------
// Shared compilation pipeline
// ---------------------------------------------------------------------------

/// Result of the shared front-end pipeline: optimized IR ready for JIT or AOT.
struct CompileResult {
    module: vm::ir::IrModule,
    natives: vm::natives::NativeRegistry,
}

/// Shared front-end: parse → type-check → analysis → import resolution →
/// lower → stdlib prepend → optimize.
///
/// Used by both `run_file_jit` and `build_file_aot` so that every code path
/// gets the same validation and import handling.
fn compile_to_ir(file: &PathBuf) -> CompileResult {
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

    // Parse imports from the main file
    let imports = resolver::ModuleLoader::parse_imports(&root, &source);

    // Create a single ModuleLoader to reuse across type checking and lowering
    let base_dir = file.parent().map(|p| p.to_path_buf());
    let mut loader = match &base_dir {
        Some(dir) => resolver::ModuleLoader::with_base_dir(dir.clone()),
        None => resolver::ModuleLoader::new(),
    };

    // Type checking: always use loader-aware variant (handles the no-imports case fine)
    let (type_diags, type_map, dict_map) = {
        let (diags, tm, _defs, dm) = blc::analysis::check_types_with_loader_map_and_defs(
            &root,
            &source,
            &file_str,
            Some(&mut loader),
        );
        (diags, tm, dm)
    };
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

    // Run effect and refinement checking
    let effect_diags = blc::analysis::check_effects(&tree, &source, &file_str);
    let refinement_diags = blc::analysis::check_refinements(&tree, &source, &file_str);
    for d in effect_diags.iter().chain(refinement_diags.iter()) {
        eprintln!(
            "{}:{}:{}: {}: {} [{}]",
            d.location.file,
            d.location.line,
            d.location.col,
            match d.severity {
                diagnostics::Severity::Error => "error",
                diagnostics::Severity::Warning => "warning",
                diagnostics::Severity::Info => "info",
            },
            d.message,
            d.code
        );
    }
    let has_analysis_errors = effect_diags
        .iter()
        .chain(refinement_diags.iter())
        .any(|d| d.severity == diagnostics::Severity::Error);
    if has_analysis_errors {
        std::process::exit(1);
    }

    let natives = vm::natives::NativeRegistry::new();
    let (stdlib_fns, stdlib_names) = vm::stdlib::compile_stdlib(&natives);
    let stdlib_fn_count = stdlib_fns.len();

    // Resolve and lower imported modules into IR functions
    let mut imported_fns: Vec<vm::ir::IrFunction> = Vec::new();
    let mut imported_names: Vec<String> = Vec::new();

    if !imports.is_empty() {
        // Reuse the same loader that type checking already populated — modules
        // are cached so they won't be re-parsed (fixes issue #6).

        enum StackStep {
            Process(resolver::ResolvedImport, String),
            Pop(std::path::PathBuf),
        }

        let mut to_process: Vec<StackStep> = imports
            .into_iter()
            .map(|(import, _)| StackStep::Process(import, file_str.clone()))
            .collect();

        let mut loaded_modules_meta = Vec::new();
        let mut visited = std::collections::HashSet::new();

        while let Some(step) = to_process.pop() {
            match step {
                StackStep::Pop(path) => {
                    loader.pop_resolution(&path);
                }
                StackStep::Process(import, from_file) => {
                    let mod_path = match loader.resolve_path(&import.module_name, &root, &from_file)
                    {
                        Ok(p) => p,
                        Err(d) => {
                            eprintln!("Compile Error: {}", d.message);
                            std::process::exit(1);
                        }
                    };

                    // If we already fully processed this module's transitive closure,
                    // just get its idx and continue.
                    if !visited.insert(mod_path.clone()) {
                        let idx = match loader.load_module(&mod_path, &root, &from_file) {
                            Ok(i) => i,
                            Err(d) => {
                                eprintln!("Compile Error: {}", d.message);
                                std::process::exit(1);
                            }
                        };
                        loaded_modules_meta.push((import, idx));
                        continue;
                    }

                    let idx = match loader.load_module(&mod_path, &root, &from_file) {
                        Ok(i) => i,
                        Err(d) => {
                            eprintln!("Compile Error: {}", d.message);
                            std::process::exit(1);
                        }
                    };

                    loaded_modules_meta.push((import, idx));

                    // Push the pop action to be executed AFTER nested imports are processed
                    to_process.push(StackStep::Pop(mod_path.clone()));

                    let nested_imports = {
                        let (mod_root, mod_source, _) = loader.get_module(idx).unwrap();
                        resolver::ModuleLoader::parse_imports(&mod_root, mod_source)
                    };

                    for (nested_import, _) in nested_imports {
                        to_process.push(StackStep::Process(
                            nested_import,
                            mod_path.display().to_string(),
                        ));
                    }
                }
            }
        }

        // Deduplicate the modules to lower so we don't compile them twice,
        // while preserving order.
        let mut lowered_modules = std::collections::HashSet::new();

        // Pre-collect ALL function names from all loaded modules to inject into the lowerer
        // so that functions can call each other directly.
        let mut global_functions = std::collections::HashSet::new();
        // and collect module exports
        let mut mod_exports: std::collections::HashMap<usize, std::collections::HashSet<String>> =
            std::collections::HashMap::new();

        for i in 0..loaded_modules_meta.len() {
            let (_, idx) = loaded_modules_meta[i];
            if !mod_exports.contains_key(&idx) {
                let (mod_root, mod_source, _) = loader.get_module(idx).unwrap();
                let uses_exports = resolver::module_uses_exports(&mod_root, mod_source);

                // Hack: We need all functions to collect their names
                let mut dummy_lowerer = vm::lower::Lowerer::new(mod_source, &natives, None);
                let mod_fns = dummy_lowerer
                    .lower_module_functions(&mod_root)
                    .unwrap_or_default();

                let exported = if uses_exports {
                    resolver::exported_function_names(&mod_root, mod_source)
                } else {
                    mod_fns
                        .iter()
                        .map(|f| f.name.as_str().to_string())
                        .collect()
                };
                mod_exports.insert(idx, exported);

                let short_name = loaded_modules_meta[i]
                    .0
                    .module_name
                    .split('.')
                    .next_back()
                    .unwrap_or("?");
                for f in mod_fns {
                    global_functions.insert(f.name.clone());
                    global_functions.insert(format!("{}.{}", short_name, f.name));
                }
            }
        }

        // Now lower each module once, injecting all known functions
        let mut lowered_fns: std::collections::HashMap<usize, Vec<vm::ir::IrFunction>> =
            std::collections::HashMap::new();

        for i in (0..loaded_modules_meta.len()).rev() {
            let (import, idx) = &loaded_modules_meta[i];

            if !lowered_fns.contains_key(idx) {
                let (mod_root, mod_source, _) = loader.get_module(*idx).unwrap();
                let mut mod_lowerer = vm::lower::Lowerer::new(mod_source, &natives, None);
                mod_lowerer.add_functions(global_functions.iter().cloned());
                let mod_fns = match mod_lowerer.lower_module_functions(&mod_root) {
                    Ok(fns) => fns,
                    Err(e) => {
                        eprintln!("Import Compile Error: {}", e.message);
                        std::process::exit(1);
                    }
                };
                lowered_fns.insert(*idx, mod_fns);
            }

            let mod_fns = lowered_fns.get(idx).unwrap();
            let exported = mod_exports.get(idx).unwrap();

            let short_name = import
                .module_name
                .split('.')
                .next_back()
                .unwrap_or(&import.module_name);

            // Register names and functions for THIS specific import statement
            let mut qualified_clones: Vec<vm::ir::IrFunction> = Vec::new();
            for f in mod_fns {
                if !exported.contains(f.name.as_str()) {
                    continue;
                }
                let qualified = format!("{}.{}", short_name, f.name);

                if !imported_names.contains(&qualified) {
                    // Avoid dups
                    imported_names.push(qualified.clone());
                    let mut alias = f.clone();
                    alias.name = qualified;
                    qualified_clones.push(alias);
                }

                match &import.kind {
                    resolver::ImportKind::Selective(names) => {
                        if names.contains(&f.name) && !imported_names.contains(&f.name) {
                            imported_names.push(f.name.clone());
                        }
                    }
                    resolver::ImportKind::Wildcard => {
                        if !imported_names.contains(&f.name) {
                            imported_names.push(f.name.clone());
                        }
                    }
                    resolver::ImportKind::Qualified => {}
                }
            }

            if lowered_modules.insert(*idx) {
                imported_fns.extend(mod_fns.clone());
            }
            imported_fns.extend(qualified_clones);
        }
    }

    let mut lowerer = vm::lower::Lowerer::new(&source, &natives, Some(type_map));
    lowerer.set_dict_map(dict_map);
    lowerer.add_functions(stdlib_names.iter().cloned());
    lowerer.add_functions(imported_names.iter().cloned());
    let ir_module = match lowerer.lower_module(&root) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Compile Error: {}", e.message);
            std::process::exit(1);
        }
    };

    let mut optimized = ir_module;
    // Prepend stdlib functions, then imported functions; adjust entry index
    let import_fn_count = imported_fns.len();
    optimized.functions.splice(0..0, stdlib_fns);
    optimized
        .functions
        .splice(stdlib_fn_count..stdlib_fn_count, imported_fns);
    optimized.entry += stdlib_fn_count + import_fn_count;
    vm::optimize_ir::optimize(&mut optimized);

    CompileResult {
        module: optimized,
        natives,
    }
}

// ---------------------------------------------------------------------------
// JIT execution
// ---------------------------------------------------------------------------

#[cfg(feature = "jit")]
fn run_file_jit(file: &PathBuf) {
    let result = compile_to_ir(file);

    let jit_program =
        match vm::jit::compile_with_natives(&result.module, false, Some(&result.natives)) {
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
                eprintln!("{}: Runtime Error: {}", file.display(), err);
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

// ---------------------------------------------------------------------------
// AOT compilation
// ---------------------------------------------------------------------------

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
    let result = compile_to_ir(file);

    // AOT compile
    let obj_bytes = match vm::jit::aot::compile_to_object(&result.module, trace) {
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

    if let Some(diags) = json_val
        .get_mut("diagnostics")
        .and_then(|v| v.as_array_mut())
    {
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
        let context = format!("line {}", test.location.line);
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
