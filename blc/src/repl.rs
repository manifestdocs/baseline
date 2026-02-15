use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use tree_sitter::Parser;
use tree_sitter_baseline::LANGUAGE;

use crate::analysis;
use crate::diagnostics::{Diagnostic, Severity};
use crate::vm;

const REPL_FILE: &str = "<repl>";

// ---------------------------------------------------------------------------
// Context
// ---------------------------------------------------------------------------

struct ReplContext {
    /// Top-level declarations: fn, type, effect, import
    top_level_decls: Vec<String>,
    /// let bindings (placed inside main body)
    let_bindings: Vec<String>,
}

impl ReplContext {
    fn new() -> Self {
        ReplContext {
            top_level_decls: Vec::new(),
            let_bindings: Vec::new(),
        }
    }

    fn clear(&mut self) {
        self.top_level_decls.clear();
        self.let_bindings.clear();
    }

    /// Build synthetic source to evaluate an expression.
    fn build_expr_source(&self, expr: &str) -> String {
        let mut src = String::from("@prelude(script)\n\n");
        for decl in &self.top_level_decls {
            src.push_str(decl);
            src.push('\n');
        }
        src.push_str("\nfn main!() -> Unit = {\n");
        for binding in &self.let_bindings {
            src.push_str("  ");
            src.push_str(binding);
            src.push('\n');
        }
        src.push_str("  ");
        src.push_str(expr);
        src.push_str("\n}\n");
        src
    }

    /// Build synthetic source for adding a let binding (returns Unit).
    fn build_let_source(&self, new_binding: &str) -> String {
        let mut src = String::from("@prelude(script)\n\n");
        for decl in &self.top_level_decls {
            src.push_str(decl);
            src.push('\n');
        }
        src.push_str("\nfn main!() -> Unit = {\n");
        for binding in &self.let_bindings {
            src.push_str("  ");
            src.push_str(binding);
            src.push('\n');
        }
        src.push_str("  ");
        src.push_str(new_binding);
        src.push_str("\n  ()\n}\n");
        src
    }

    /// Build synthetic source to validate a top-level declaration.
    fn build_decl_source(&self, new_decl: &str) -> String {
        let mut src = String::from("@prelude(script)\n\n");
        for decl in &self.top_level_decls {
            src.push_str(decl);
            src.push('\n');
        }
        src.push_str(new_decl);
        src.push_str("\n\nfn main!() -> Unit = ()\n");
        src
    }
}

// ---------------------------------------------------------------------------
// Input classification
// ---------------------------------------------------------------------------

enum InputKind {
    Expression(String),
    LetBinding(String),
    TopLevelDecl(String),
    Command(String),
    Empty,
}

fn classify_input(input: &str) -> InputKind {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return InputKind::Empty;
    }
    if trimmed.starts_with(':') {
        return InputKind::Command(trimmed.to_string());
    }
    if trimmed.starts_with("let ") {
        return InputKind::LetBinding(trimmed.to_string());
    }
    if trimmed.starts_with("fn ")
        || trimmed.starts_with("type ")
        || trimmed.starts_with("effect ")
        || trimmed.starts_with("import ")
    {
        return InputKind::TopLevelDecl(trimmed.to_string());
    }
    InputKind::Expression(trimmed.to_string())
}

/// Check if multi-line continuation is needed.
fn needs_continuation(input: &str) -> bool {
    let trimmed = input.trim();

    // Function signature without body
    if trimmed.starts_with("fn ") && !trimmed.contains('=') {
        return true;
    }

    // Trailing operators
    if trimmed.ends_with('=')
        || trimmed.ends_with('|')
        || trimmed.ends_with("|>")
        || trimmed.ends_with("then")
    {
        return true;
    }

    // Brace imbalance
    let open = trimmed.chars().filter(|&c| c == '{').count();
    let close = trimmed.chars().filter(|&c| c == '}').count();
    if open > close {
        return true;
    }

    false
}

// ---------------------------------------------------------------------------
// Diagnostic filtering
// ---------------------------------------------------------------------------

/// Filter out diagnostics that are artifacts of the synthetic REPL wrapper.
fn filter_repl_diagnostics(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| {
            // Suppress effect capability errors on main! (our wrapper has no effects)
            if d.code == "CAP_001" && d.message.contains("'main!'") {
                return false;
            }
            // Suppress style warnings in REPL
            if d.code.starts_with("STY_") {
                return false;
            }
            true
        })
        .collect()
}

fn has_real_errors(diags: &[&Diagnostic]) -> bool {
    diags.iter().any(|d| d.severity == Severity::Error)
}

// ---------------------------------------------------------------------------
// Pipeline helpers
// ---------------------------------------------------------------------------

fn compile_and_run(source: &str) -> Result<vm::value::Value, String> {
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load grammar");
    let tree = parser.parse(source, None).ok_or("Parse failed")?;
    let root = tree.root_node();

    let (_type_diags, type_map, dict_map) =
        analysis::check_types_with_map(&root, source, REPL_FILE);

    let mut vm_instance = vm::exec::Vm::new();
    let mut lowerer =
        vm::lower::Lowerer::new(source, vm_instance.natives(), Some(type_map));
    lowerer.set_dict_map(dict_map);
    let ir_module = lowerer
        .lower_module(&root)
        .map_err(|e| format!("Compile error: {}", e.message))?;
    let mut module = ir_module;
    vm::optimize_ir::optimize_for_bytecode(&mut module);
    let codegen = vm::codegen::Codegen::new(vm_instance.natives());
    let mut program = codegen
        .generate_program(&module)
        .map_err(|e| format!("Codegen error: {}", e.message))?;
    program.optimize();
    vm_instance
        .execute_program(&program)
        .map_err(|e| format!("Runtime error: {}", e.message))
}

fn type_check_source(source: &str) -> Vec<Diagnostic> {
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load grammar");
    let tree = match parser.parse(source, None) {
        Some(t) => t,
        None => return vec![],
    };
    let root = tree.root_node();
    let (diags, _type_map, _dict_map) = analysis::check_types_with_map(&root, source, REPL_FILE);
    diags
}

/// For :type — extract the expression's type from the TypeMap.
fn infer_expr_type(source: &str, expr_byte_offset: usize) -> Option<String> {
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load grammar");
    let tree = parser.parse(source, None)?;
    let root = tree.root_node();
    let (_diags, type_map, _dict_map) = analysis::check_types_with_map(&root, source, REPL_FILE);

    // Look for the TypeMap entry at or near the expression offset.
    // The expression's top-level node should be in the map.
    // Search for the closest entry at or after the offset.
    let mut best: Option<(usize, &analysis::Type)> = None;
    for (byte, ty) in &type_map {
        if *byte >= expr_byte_offset {
            match best {
                None => best = Some((*byte, ty)),
                Some((prev, _)) if *byte < prev => best = Some((*byte, ty)),
                _ => {}
            }
        }
    }
    best.map(|(_, ty)| ty.to_string())
}

// ---------------------------------------------------------------------------
// REPL commands
// ---------------------------------------------------------------------------

fn handle_command(cmd: &str, ctx: &mut ReplContext) {
    let parts: Vec<&str> = cmd.splitn(2, ' ').collect();
    let command = parts[0];
    let arg = parts.get(1).map(|s| s.trim()).unwrap_or("");

    match command {
        ":help" | ":h" => {
            println!("Commands:");
            println!("  :help    :h   Show this help");
            println!("  :quit    :q   Exit REPL");
            println!("  :clear   :c   Clear accumulated context");
            println!("  :type    :t   Show type of expression");
            println!("  :context       Show accumulated declarations/bindings");
            println!("  :load    :l   Load declarations from a .bl file");
        }
        ":quit" | ":q" => std::process::exit(0),
        ":clear" | ":c" => {
            ctx.clear();
            println!("Context cleared.");
        }
        ":context" => {
            if ctx.top_level_decls.is_empty() && ctx.let_bindings.is_empty() {
                println!("(empty)");
            } else {
                for decl in &ctx.top_level_decls {
                    println!("{}", decl);
                }
                for binding in &ctx.let_bindings {
                    println!("{}", binding);
                }
            }
        }
        ":type" | ":t" => {
            if arg.is_empty() {
                println!("Usage: :type <expression>");
                return;
            }
            let source = ctx.build_expr_source(arg);
            let expr_offset = source.rfind(arg).unwrap_or(0);
            match infer_expr_type(&source, expr_offset) {
                Some(ty) => println!("{}", ty),
                None => println!("Could not infer type"),
            }
        }
        ":load" | ":l" => {
            if arg.is_empty() {
                println!("Usage: :load <file.bl>");
                return;
            }
            load_file(arg, ctx);
        }
        _ => {
            println!("Unknown command: {}. Type :help for available commands.", command);
        }
    }
}

fn load_file(path: &str, ctx: &mut ReplContext) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            println!("Error reading {}: {}", path, e);
            return;
        }
    };

    // Extract top-level declarations (skip main, prelude directives)
    let mut count = 0;
    let mut current_decl = String::new();
    let mut in_block = false;
    let mut brace_depth: i32 = 0;

    for line in source.lines() {
        let trimmed = line.trim();

        // Skip prelude directives and empty lines at top level
        if !in_block && current_decl.is_empty() {
            if trimmed.is_empty()
                || trimmed.starts_with("//")
                || trimmed.starts_with("@prelude")
            {
                continue;
            }
        }

        // Skip main function
        if !in_block && current_decl.is_empty() && trimmed.starts_with("fn main") {
            // Skip until we pass the main function body
            if trimmed.contains('{') {
                brace_depth = trimmed.chars().filter(|&c| c == '{').count() as i32
                    - trimmed.chars().filter(|&c| c == '}').count() as i32;
                if brace_depth > 0 {
                    in_block = true;
                }
            }
            continue;
        }

        if in_block {
            brace_depth += trimmed.chars().filter(|&c| c == '{').count() as i32;
            brace_depth -= trimmed.chars().filter(|&c| c == '}').count() as i32;
            if brace_depth <= 0 {
                in_block = false;
                brace_depth = 0;
            }
            continue;
        }

        // Accumulate declaration lines
        if current_decl.is_empty() {
            if trimmed.starts_with("fn ")
                || trimmed.starts_with("type ")
                || trimmed.starts_with("effect ")
                || trimmed.starts_with("import ")
            {
                current_decl.push_str(line);
                // Check if declaration is complete
                let open = current_decl.chars().filter(|&c| c == '{').count();
                let close = current_decl.chars().filter(|&c| c == '}').count();
                if open <= close && current_decl.contains('=') {
                    ctx.top_level_decls.push(current_decl.clone());
                    current_decl.clear();
                    count += 1;
                } else if trimmed.starts_with("import ") || trimmed.starts_with("type ") {
                    // imports and type defs may be single-line without =
                    if !needs_continuation(trimmed) || trimmed.starts_with("import ") {
                        ctx.top_level_decls.push(current_decl.clone());
                        current_decl.clear();
                        count += 1;
                    }
                }
            }
        } else {
            // Continuation of multi-line declaration
            current_decl.push('\n');
            current_decl.push_str(line);
            let open = current_decl.chars().filter(|&c| c == '{').count();
            let close = current_decl.chars().filter(|&c| c == '}').count();
            if open <= close {
                ctx.top_level_decls.push(current_decl.clone());
                current_decl.clear();
                count += 1;
            }
        }
    }

    // Don't forget incomplete declarations
    if !current_decl.is_empty() {
        ctx.top_level_decls.push(current_decl);
        count += 1;
    }

    println!("Loaded {} declaration(s) from {}", count, path);
}

// ---------------------------------------------------------------------------
// Main REPL loop
// ---------------------------------------------------------------------------

pub fn run() {
    println!("Baseline REPL v{}", env!("CARGO_PKG_VERSION"));
    println!("Type :help for commands, :quit to exit\n");

    let mut rl = DefaultEditor::new().expect("Failed to initialize line editor");
    let mut ctx = ReplContext::new();

    loop {
        let input = match read_input(&mut rl) {
            Some(input) => input,
            None => break, // Ctrl-D
        };

        match classify_input(&input) {
            InputKind::Empty => continue,
            InputKind::Command(cmd) => handle_command(&cmd, &mut ctx),
            InputKind::Expression(expr) => eval_expression(&expr, &mut ctx),
            InputKind::LetBinding(binding) => eval_let_binding(&binding, &mut ctx),
            InputKind::TopLevelDecl(decl) => eval_top_level_decl(&decl, &mut ctx),
        }
    }

    println!();
}

fn read_input(rl: &mut DefaultEditor) -> Option<String> {
    let first_line = match rl.readline(">> ") {
        Ok(line) => line,
        Err(ReadlineError::Interrupted) => return Some(String::new()),
        Err(ReadlineError::Eof) => return None,
        Err(e) => {
            eprintln!("Error: {}", e);
            return Some(String::new());
        }
    };

    let mut input = first_line;

    // Multi-line continuation
    while needs_continuation(&input) {
        match rl.readline(".. ") {
            Ok(line) => {
                if line.trim().is_empty() {
                    break; // Empty line finalizes
                }
                input.push('\n');
                input.push_str(&line);
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl-C cancels multi-line input
                return Some(String::new());
            }
            Err(ReadlineError::Eof) => return None,
            Err(e) => {
                eprintln!("Error: {}", e);
                return Some(String::new());
            }
        }
    }

    if !input.trim().is_empty() {
        let _ = rl.add_history_entry(&input);
    }

    Some(input)
}

fn eval_expression(expr: &str, ctx: &mut ReplContext) {
    let source = ctx.build_expr_source(expr);

    // Type check first
    let diags = type_check_source(&source);
    let filtered = filter_repl_diagnostics(&diags);
    if has_real_errors(&filtered) {
        for d in &filtered {
            if d.severity == Severity::Error {
                eprintln!("Error: {} [{}]", d.message, d.code);
            }
        }
        return;
    }

    match compile_and_run(&source) {
        Ok(val) => {
            if !matches!(val, vm::value::Value::Unit) {
                println!("{}", val);
            }
        }
        Err(e) => eprintln!("{}", e),
    }
}

fn eval_let_binding(binding: &str, ctx: &mut ReplContext) {
    let source = ctx.build_let_source(binding);

    let diags = type_check_source(&source);
    let filtered = filter_repl_diagnostics(&diags);
    if has_real_errors(&filtered) {
        for d in &filtered {
            if d.severity == Severity::Error {
                eprintln!("Error: {} [{}]", d.message, d.code);
            }
        }
        return;
    }

    // Validate by compiling (catches lowering errors)
    match compile_and_run(&source) {
        Ok(_) => {
            ctx.let_bindings.push(binding.to_string());
        }
        Err(e) => eprintln!("{}", e),
    }
}

fn eval_top_level_decl(decl: &str, ctx: &mut ReplContext) {
    let source = ctx.build_decl_source(decl);

    let diags = type_check_source(&source);
    let filtered = filter_repl_diagnostics(&diags);
    if has_real_errors(&filtered) {
        for d in &filtered {
            if d.severity == Severity::Error {
                eprintln!("Error: {} [{}]", d.message, d.code);
            }
        }
        return;
    }

    // Validate by compiling
    match compile_and_run(&source) {
        Ok(_) => {
            ctx.top_level_decls.push(decl.to_string());
        }
        Err(e) => eprintln!("{}", e),
    }
}
