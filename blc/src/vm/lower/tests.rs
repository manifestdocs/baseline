#[cfg(test)]
use super::*;

use crate::vm::ir::*;
use crate::vm::natives::NativeRegistry;

fn parse(source: &str) -> tree_sitter::Tree {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_baseline::LANGUAGE.into())
        .expect("Error loading grammar");
    parser.parse(source, None).expect("Parse failed")
}

fn lower_expr(source: &str) -> Expr {
    let wrapped = format!("fn x() -> Unknown = {}", source);
    let tree = parse(&wrapped);
    let root = tree.root_node();
    let func_def = root.named_child(0).unwrap();
    let body = func_def.child_by_field_name("body").unwrap();
    let natives = NativeRegistry::new();
    let mut lowerer = Lowerer::new(&wrapped, &natives, None);
    lowerer.lower_expression(&body).unwrap()
}

#[test]
fn lower_int_literal() {
    match lower_expr("42") {
        Expr::Int(42) => {}
        other => panic!("Expected Int(42), got {:?}", other),
    }
}

#[test]
fn lower_float_literal() {
    match lower_expr("1.5") {
        Expr::Float(f) => assert!((f - 1.5).abs() < 1e-10),
        other => panic!("Expected Float(1.5), got {:?}", other),
    }
}

#[test]
fn lower_bool_literal() {
    match lower_expr("true") {
        Expr::Bool(true) => {}
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn lower_string_literal_plain() {
    match lower_expr("\"hello\"") {
        Expr::String(s) => assert_eq!(s, "hello"),
        other => panic!("Expected String, got {:?}", other),
    }
}

#[test]
fn lower_addition_const_fold() {
    match lower_expr("1 + 2") {
        Expr::Int(3) => {}
        other => panic!("Expected Int(3) from const fold, got {:?}", other),
    }
}

#[test]
fn lower_pipe_desugars() {
    // x |> f desugars to a call
    let source = "fn double(x: Int) -> Int = x * 2\nfn main() -> Int = 5 |> double";
    let tree = parse(source);
    let root = tree.root_node();
    let natives = NativeRegistry::new();
    let mut lowerer = Lowerer::new(source, &natives, None);
    let module = lowerer.lower_module(&root).unwrap();
    // main function body should be a call, not a pipe
    match &module.functions[module.entry].body {
        Expr::CallDirect { name, args, .. } => {
            assert_eq!(name, "double");
            assert_eq!(args.len(), 1);
        }
        other => panic!("Expected CallDirect, got {:?}", other),
    }
}

#[test]
fn lower_some_constructor() {
    match lower_expr("Some(42)") {
        Expr::MakeEnum { tag, payload, .. } => {
            assert_eq!(tag, "Some");
            match *payload {
                Expr::Int(42) => {}
                other => panic!("Expected Int(42) payload, got {:?}", other),
            }
        }
        other => panic!("Expected MakeEnum, got {:?}", other),
    }
}

#[test]
fn lower_none_constructor() {
    match lower_expr("None") {
        Expr::MakeEnum { tag, payload, .. } => {
            assert_eq!(tag, "None");
            match *payload {
                Expr::Unit => {}
                other => panic!("Expected Unit payload, got {:?}", other),
            }
        }
        other => panic!("Expected MakeEnum, got {:?}", other),
    }
}

#[test]
fn lower_module_basic() {
    let source = "fn inc(x: Int) -> Int = x + 1\nfn main() -> Int = inc(5)";
    let tree = parse(source);
    let root = tree.root_node();
    let natives = NativeRegistry::new();
    let mut lowerer = Lowerer::new(source, &natives, None);
    let module = lowerer.lower_module(&root).unwrap();
    assert_eq!(module.functions.len(), 2);
    assert_eq!(module.functions[module.entry].name, "main");
}
