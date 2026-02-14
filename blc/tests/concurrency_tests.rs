//! Integration tests for structured concurrency (fiber runtime).
//!
//! These tests compile Baseline source code with scope!/Scope.spawn!/Cell.await!
//! and run it inside a tokio runtime to verify end-to-end fiber execution.
//!
//! Note: Effect annotations like `-> {Async} Int` are check-time-only and not
//! supported by the VM lowerer, so test functions omit them.

use std::sync::Arc;

use blc::vm;

/// Compile Baseline source to a Program using the IR pipeline.
/// Uses prelude(none) since we only need the async primitives as native functions.
fn compile_source(source: &str) -> vm::chunk::Program {
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load language");
    let tree = parser.parse(source, None).expect("Failed to parse");
    let root = tree.root_node();

    let vm_instance = vm::exec::Vm::new();
    let mut lowerer = vm::lower::Lowerer::new(source, vm_instance.natives(), None);
    let mut module = lowerer.lower_module(&root).expect("Failed to lower");
    vm::optimize_ir::optimize_for_bytecode(&mut module);
    let codegen = vm::codegen::Codegen::new(vm_instance.natives());
    let mut program = codegen
        .generate_program(&module)
        .expect("Failed to generate program");
    program.optimize();
    program
}

#[test]
fn scope_returns_body_value() {
    let source = r#"
fn main() -> Int =
  scope!(|s| 42)
"#;
    let program = Arc::new(compile_source(source));
    let rt = tokio::runtime::Runtime::new().unwrap();
    let result = rt.block_on(async {
        tokio::task::spawn_blocking(move || {
            let mut vm = vm::exec::Vm::new();
            vm.execute_program_arc(program)
        })
        .await
        .unwrap()
    });
    match result {
        Ok(val) => assert_eq!(format!("{}", val), "42"),
        Err(e) => panic!("Runtime error: {}", e),
    }
}

#[test]
fn spawn_and_await_returns_value() {
    let source = r#"
fn main() -> Int =
  scope!(|s| {
    let cell = Scope.spawn!(s, || 42)
    Cell.await!(cell)
  })
"#;
    let program = Arc::new(compile_source(source));
    let rt = tokio::runtime::Runtime::new().unwrap();
    let result = rt.block_on(async {
        tokio::task::spawn_blocking(move || {
            let mut vm = vm::exec::Vm::new();
            vm.execute_program_arc(program)
        })
        .await
        .unwrap()
    });
    match result {
        Ok(val) => assert_eq!(format!("{}", val), "42"),
        Err(e) => panic!("Runtime error: {}", e),
    }
}

#[test]
fn multiple_spawns_all_complete() {
    let source = r#"
fn main() -> Int =
  scope!(|s| {
    let a = Scope.spawn!(s, || 10)
    let b = Scope.spawn!(s, || 20)
    let c = Scope.spawn!(s, || 12)
    Cell.await!(a) + Cell.await!(b) + Cell.await!(c)
  })
"#;
    let program = Arc::new(compile_source(source));
    let rt = tokio::runtime::Runtime::new().unwrap();
    let result = rt.block_on(async {
        tokio::task::spawn_blocking(move || {
            let mut vm = vm::exec::Vm::new();
            vm.execute_program_arc(program)
        })
        .await
        .unwrap()
    });
    match result {
        Ok(val) => assert_eq!(format!("{}", val), "42"),
        Err(e) => panic!("Runtime error: {}", e),
    }
}

#[test]
fn cell_cancel_aborts_fiber() {
    let source = r#"
fn main() -> Int =
  scope!(|s| {
    let cell = Scope.spawn!(s, || 99)
    Cell.cancel!(cell)
    42
  })
"#;
    let program = Arc::new(compile_source(source));
    let rt = tokio::runtime::Runtime::new().unwrap();
    let result = rt.block_on(async {
        tokio::task::spawn_blocking(move || {
            let mut vm = vm::exec::Vm::new();
            vm.execute_program_arc(program)
        })
        .await
        .unwrap()
    });
    match result {
        Ok(val) => assert_eq!(format!("{}", val), "42"),
        Err(e) => panic!("Runtime error: {}", e),
    }
}
