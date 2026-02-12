//! Fuzz target for VM execution.
//!
//! This fuzzer tests that the VM never panics or crashes on arbitrary
//! well-formed programs, even if they produce runtime errors.
//!
//! The fuzzer:
//! 1. Parses the input as Baseline source
//! 2. Compiles to bytecode (if parsing succeeds)
//! 3. Executes with resource limits (if compilation succeeds)
//! 4. Verifies no panic occurs (runtime errors are OK)

#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Only test valid UTF-8
    let source = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Skip empty or very long inputs
    if source.is_empty() || source.len() > 10_000 {
        return;
    }

    // Parse the source - must not panic
    let result = blc::parse::parse_source(source, "<fuzz>");

    // If there are parse errors, we're done (that's fine)
    if result.status == "failure" {
        return;
    }

    // Try to compile and execute with the VM
    // Use a limited VM to prevent infinite loops
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    let mut parser = Parser::new();
    if parser.set_language(&LANGUAGE.into()).is_err() {
        return;
    }

    let tree = match parser.parse(source, None) {
        Some(t) => t,
        None => return,
    };

    let root = tree.root_node();

    // Create a VM with strict instruction limit for fuzzing
    let mut vm = blc::vm::exec::Vm::with_instruction_limit(100_000);

    // Try to compile the source to IR
    let (_type_diags, type_map) = blc::analysis::check_types_with_map(&root, source, "<fuzz>");
    let type_map = Some(type_map);

    let mut lowerer = blc::vm::lower::Lowerer::new(source, vm.natives(), type_map);
    let ir_module = match lowerer.lower_module(&root) {
        Ok(m) => m,
        Err(_) => return, // Compilation errors are fine
    };

    let mut optimized = ir_module;
    blc::vm::optimize_ir::optimize(&mut optimized);

    let codegen = blc::vm::codegen::Codegen::new(vm.natives());
    let mut program = match codegen.generate_program(&optimized) {
        Ok(p) => p,
        Err(_) => return, // Code generation errors are fine
    };
    program.optimize();

    // Execute - runtime errors are OK, panics are not
    let _ = vm.execute_program(&program);
    // If we get here without panic, the test passes
});
