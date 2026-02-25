//! Closure, lambda lifting, and CallIndirect tests.

use super::tests::*;
use super::*;
use crate::analysis::types::Type;
use crate::vm::ir::{
    BinOp, Expr, IrFunction, IrModule, MatchArm, Pattern, Span, TagRegistry,
};
use crate::vm::nvalue::{HeapObject, NValue};


// -- Closure / Lambda lifting / CallIndirect tests --

/// Compile an IrModule through optimize (incl. lambda lifting) + JIT, run entry.
fn optimize_compile_and_run(module: &mut IrModule) -> NValue {
    crate::vm::optimize_ir::optimize(module);
    let program = compile(module, false).expect("JIT compilation failed");
    program
        .run_entry_nvalue()
        .expect("Entry function not compiled")
}

#[test]
fn jit_lambda_no_captures() {
    // let f = |x| x + 1; f(42) → 43
    let mut module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("f".into())),
                        value: Box::new(Expr::Lambda {
                            params: vec!["x".into()],
                            body: Box::new(make_binop(
                                BinOp::Add,
                                Expr::Var("x".into(), Some(Type::Int)),
                                make_int(1),
                            )),
                            ty: None,
                        }),
                        ty: None,
                    },
                    Expr::CallIndirect {
                        callee: Box::new(Expr::Var("f".into(), None)),
                        args: vec![make_int(42)],
                        ty: Some(Type::Int),
                    },
                ],
                None,
            ),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let result = optimize_compile_and_run(&mut module);
    assert!(result.is_int(), "Expected Int, got: {}", result);
    assert_eq!(result.as_int(), 43);
}

#[test]
fn jit_lambda_with_capture() {
    // let y = 10; let f = |x| x + y; f(5) → 15
    let mut module = IrModule {
        functions: vec![
            IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Block(
                    vec![
                        Expr::Let {
                            pattern: Box::new(Pattern::Var("y".into())),
                            value: Box::new(Expr::CallDirect {
                                name: "get_ten".into(),
                                args: vec![],
                                ty: Some(Type::Int),
                            }),
                            ty: None,
                        },
                        Expr::Let {
                            pattern: Box::new(Pattern::Var("f".into())),
                            value: Box::new(Expr::Lambda {
                                params: vec!["x".into()],
                                body: Box::new(make_binop(
                                    BinOp::Add,
                                    Expr::Var("x".into(), Some(Type::Int)),
                                    Expr::Var("y".into(), Some(Type::Int)),
                                )),
                                ty: None,
                            }),
                            ty: None,
                        },
                        Expr::CallIndirect {
                            callee: Box::new(Expr::Var("f".into(), None)),
                            args: vec![make_int(5)],
                            ty: Some(Type::Int),
                        },
                    ],
                    None,
                ),
                ty: Some(Type::Int),
                span: dummy_span(),
            },
            // Helper function to prevent constant propagation of y
            IrFunction {
                name: "get_ten".into(),
                params: vec![],
                body: make_int(10),
                ty: Some(Type::Int),
                span: dummy_span(),
            },
        ],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let result = optimize_compile_and_run(&mut module);
    assert!(result.is_int(), "Expected Int, got: {}", result);
    assert_eq!(result.as_int(), 15);
}

#[test]
fn jit_nested_lambdas() {
    // let add = |x| |y| x + y; add(3)(4) → 7
    let mut module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("add".into())),
                        value: Box::new(Expr::Lambda {
                            params: vec!["x".into()],
                            body: Box::new(Expr::Lambda {
                                params: vec!["y".into()],
                                body: Box::new(make_binop(
                                    BinOp::Add,
                                    Expr::Var("x".into(), Some(Type::Int)),
                                    Expr::Var("y".into(), Some(Type::Int)),
                                )),
                                ty: None,
                            }),
                            ty: None,
                        }),
                        ty: None,
                    },
                    // add(3)
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("add3".into())),
                        value: Box::new(Expr::CallIndirect {
                            callee: Box::new(Expr::Var("add".into(), None)),
                            args: vec![make_int(3)],
                            ty: None,
                        }),
                        ty: None,
                    },
                    // add3(4)
                    Expr::CallIndirect {
                        callee: Box::new(Expr::Var("add3".into(), None)),
                        args: vec![make_int(4)],
                        ty: Some(Type::Int),
                    },
                ],
                None,
            ),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let result = optimize_compile_and_run(&mut module);
    assert!(result.is_int(), "Expected Int, got: {}", result);
    assert_eq!(result.as_int(), 7);
}

#[test]
fn jit_call_direct_function_as_value() {
    // fn double(x) = x + x; let f = double; f(21) → 42
    let mut module = IrModule {
        functions: vec![
            IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::Block(
                    vec![
                        Expr::Let {
                            pattern: Box::new(Pattern::Var("f".into())),
                            value: Box::new(Expr::Var("double".into(), None)),
                            ty: None,
                        },
                        Expr::CallIndirect {
                            callee: Box::new(Expr::Var("f".into(), None)),
                            args: vec![make_int(21)],
                            ty: Some(Type::Int),
                        },
                    ],
                    None,
                ),
                ty: Some(Type::Int),
                span: dummy_span(),
            },
            IrFunction {
                name: "double".into(),
                params: vec!["x".into()],
                body: make_binop(BinOp::Add, make_var("x"), make_var("x")),
                ty: Some(Type::Int),
                span: dummy_span(),
            },
        ],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let result = optimize_compile_and_run(&mut module);
    assert!(result.is_int(), "Expected Int, got: {}", result);
    assert_eq!(result.as_int(), 42);
}

#[test]
fn jit_list_concat() {
    // [1, 2] ++ [3, 4] → [1, 2, 3, 4]
    let mut module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::BinOp {
                op: BinOp::ListConcat,
                lhs: Box::new(Expr::MakeList(vec![make_int(1), make_int(2)], None)),
                rhs: Box::new(Expr::MakeList(vec![make_int(3), make_int(4)], None)),
                ty: None,
            },
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let result = optimize_compile_and_run(&mut module);
    let items = result.as_list().expect("Expected List");
    assert_eq!(items.len(), 4);
    assert_eq!(items[0].as_int(), 1);
    assert_eq!(items[1].as_int(), 2);
    assert_eq!(items[2].as_int(), 3);
    assert_eq!(items[3].as_int(), 4);
}
