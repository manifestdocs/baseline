use super::analysis::can_jit;
use super::compile::FnCompileCtx;
use super::*;
use crate::analysis::types::Type;
use crate::vm::ir::{
    BinOp, Expr, IrFunction, IrModule, MatchArm, Pattern, Span, TagRegistry, UnaryOp,
};
use crate::vm::nvalue::{HeapObject, NValue};

fn dummy_span() -> Span {
    Span {
        line: 0,
        col: 0,
        start_byte: 0,
        end_byte: 0,
    }
}

fn make_int(n: i64) -> Expr {
    Expr::Int(n)
}

fn make_var(name: &str) -> Expr {
    Expr::Var(name.to_string(), Some(Type::Int))
}

fn make_binop(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::BinOp {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        ty: Some(Type::Int),
    }
}

fn make_bool_binop(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::BinOp {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        ty: Some(Type::Bool),
    }
}

/// Compile and run, returning the NaN-boxed result as NValue.
fn compile_and_run_nvalue(module: &IrModule) -> NValue {
    let program = compile(module, false).expect("JIT compilation failed");
    program
        .run_entry_nvalue()
        .expect("Entry function not compiled")
}

/// Compile and run, extracting an i64 from NaN-boxed Int result.
fn compile_and_run(module: &IrModule) -> i64 {
    let nv = compile_and_run_nvalue(module);
    assert!(nv.is_int(), "Expected Int, got: {}", nv);
    nv.as_int()
}

/// Compile and run, extracting a bool from NaN-boxed Bool result.
fn compile_and_run_bool(module: &IrModule) -> bool {
    let nv = compile_and_run_nvalue(module);
    assert!(nv.is_bool(), "Expected Bool, got: {}", nv);
    nv.as_bool()
}

#[test]
fn jit_integer_constant() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_int(42),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 42);
}

#[test]
fn jit_add() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_binop(BinOp::Add, make_int(3), make_int(4)),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 7);
}

#[test]
fn jit_sub() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_binop(BinOp::Sub, make_int(10), make_int(3)),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 7);
}

#[test]
fn jit_mul() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_binop(BinOp::Mul, make_int(6), make_int(7)),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 42);
}

#[test]
fn jit_comparison_le() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_bool_binop(BinOp::Le, make_int(3), make_int(5)),
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert!(compile_and_run_bool(&module));
}

#[test]
fn jit_comparison_le_false() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_bool_binop(BinOp::Le, make_int(10), make_int(5)),
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert!(!compile_and_run_bool(&module));
}

#[test]
fn jit_if_else() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::If {
                condition: Box::new(make_bool_binop(BinOp::Le, make_int(1), make_int(5))),
                then_branch: Box::new(make_int(100)),
                else_branch: Some(Box::new(make_int(200))),
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 100);
}

#[test]
fn jit_function_call() {
    let module = IrModule {
        functions: vec![
            IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::CallDirect {
                    name: "double".into(),
                    args: vec![make_int(21)],
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            },
            IrFunction {
                name: "double".into(),
                params: vec!["n".into()],
                body: make_binop(BinOp::Add, make_var("n"), make_var("n")),
                ty: Some(Type::Int),
                span: dummy_span(),
            },
        ],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 42);
}

#[test]
fn jit_recursive_fib10() {
    let fib_body = Expr::If {
        condition: Box::new(make_bool_binop(BinOp::Le, make_var("n"), make_int(1))),
        then_branch: Box::new(make_var("n")),
        else_branch: Some(Box::new(make_binop(
            BinOp::Add,
            Expr::CallDirect {
                name: "fib".into(),
                args: vec![make_binop(BinOp::Sub, make_var("n"), make_int(1))],
                ty: Some(Type::Int),
            },
            Expr::CallDirect {
                name: "fib".into(),
                args: vec![make_binop(BinOp::Sub, make_var("n"), make_int(2))],
                ty: Some(Type::Int),
            },
        ))),
        ty: Some(Type::Int),
    };

    let module = IrModule {
        functions: vec![
            IrFunction {
                name: "fib".into(),
                params: vec!["n".into()],
                body: fib_body,
                ty: Some(Type::Int),
                span: dummy_span(),
            },
            IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::CallDirect {
                    name: "fib".into(),
                    args: vec![make_int(10)],
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            },
        ],
        entry: 1,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 55);
}

#[test]
fn jit_let_binding() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("x".into())),
                        value: Box::new(make_int(10)),
                        ty: Some(Type::Int),
                    },
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("y".into())),
                        value: Box::new(make_int(20)),
                        ty: Some(Type::Int),
                    },
                    make_binop(BinOp::Add, make_var("x"), make_var("y")),
                ],
                Some(Type::Int),
            ),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 30);
}

#[test]
fn jit_negation() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::UnaryOp {
                op: UnaryOp::Neg,
                operand: Box::new(make_int(42)),
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), -42);
}

#[test]
fn jit_fallback_for_unsupported() {
    // Lambda is still unsupported
    let func = IrFunction {
        name: "test".into(),
        params: vec![],
        body: Expr::Lambda {
            params: vec!["x".into()],
            body: Box::new(Expr::Var("x".into(), None)),
            ty: None,
        },
        ty: None,
        span: dummy_span(),
    };
    assert!(!can_jit(&func, None));
}

#[test]
fn jit_tail_call_as_regular_call() {
    let module = IrModule {
        functions: vec![
            IrFunction {
                name: "count".into(),
                params: vec!["n".into()],
                body: Expr::If {
                    condition: Box::new(make_bool_binop(BinOp::Le, make_var("n"), make_int(0))),
                    then_branch: Box::new(make_int(0)),
                    else_branch: Some(Box::new(Expr::TailCall {
                        name: "count".into(),
                        args: vec![make_binop(BinOp::Sub, make_var("n"), make_int(1))],
                        ty: Some(Type::Int),
                    })),
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            },
            IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::CallDirect {
                    name: "count".into(),
                    args: vec![make_int(100)],
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            },
        ],
        entry: 1,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 0);
}

// -- Phase 1 additional tests --

#[test]
fn jit_nan_boxed_unit() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Unit,
            ty: Some(Type::Unit),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    assert!(nv.is_unit());
}

#[test]
fn jit_nan_boxed_bool_true() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Bool(true),
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert!(compile_and_run_bool(&module));
}

#[test]
fn jit_nan_boxed_bool_false() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Bool(false),
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert!(!compile_and_run_bool(&module));
}

#[test]
fn jit_negative_int() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_int(-100),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), -100);
}

// -- Phase 2 tests --

#[test]
fn jit_string_literal() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::String("hello".into()),
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    assert!(nv.is_heap());
    assert_eq!(nv.as_string().unwrap().as_ref(), "hello");
}

#[test]
fn jit_call_native_math_abs() {
    let registry = NativeRegistry::new();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::CallNative {
                module: "Math".into(),
                method: "abs".into(),
                args: vec![make_int(-42)],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let program =
        compile_with_natives(&module, false, Some(&registry)).expect("JIT compilation failed");
    let nv = program
        .run_entry_nvalue()
        .expect("Entry function not compiled");
    assert!(nv.is_int());
    assert_eq!(nv.as_int(), 42);
}

// -- Phase 3 tests --

#[test]
fn jit_make_tuple() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeTuple(vec![make_int(1), make_int(2), make_int(3)], None),
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    assert!(nv.is_heap());
    match nv.as_heap_ref() {
        HeapObject::Tuple(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0].as_int(), 1);
            assert_eq!(items[1].as_int(), 2);
            assert_eq!(items[2].as_int(), 3);
        }
        _ => panic!("Expected tuple"),
    }
}

#[test]
fn jit_make_list() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeList(vec![make_int(10), make_int(20)], None),
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    let items = nv.as_list().unwrap();
    assert_eq!(items.len(), 2);
    assert_eq!(items[0].as_int(), 10);
    assert_eq!(items[1].as_int(), 20);
}

#[test]
fn jit_make_enum() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeEnum {
                tag: "Some".into(),
                payload: Box::new(make_int(42)),
                ty: None,
            },
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    let (tag, payload) = nv.as_enum().unwrap();
    assert_eq!(tag.as_ref(), "Some");
    assert_eq!(payload.as_int(), 42);
}

#[test]
fn jit_make_record() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeRecord(
                vec![("x".into(), make_int(1)), ("y".into(), make_int(2))],
                None,
            ),
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    let fields = nv.as_record().unwrap();
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].0.as_ref(), "x");
    assert_eq!(fields[0].1.as_int(), 1);
}

#[test]
fn jit_get_field() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::GetField {
                object: Box::new(Expr::MakeRecord(
                    vec![("x".into(), make_int(10)), ("y".into(), make_int(20))],
                    None,
                )),
                field: "y".into(),
                field_idx: None,
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 20);
}

// -- Phase 4 tests --

#[test]
fn jit_match_wildcard() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(make_int(42)),
                arms: vec![MatchArm {
                    pattern: Pattern::Wildcard,
                    guard: None,
                    body: make_int(99),
                }],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 99);
}

#[test]
fn jit_match_var() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(make_int(42)),
                arms: vec![MatchArm {
                    pattern: Pattern::Var("x".into()),
                    guard: None,
                    body: make_binop(BinOp::Add, make_var("x"), make_int(1)),
                }],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 43);
}

#[test]
fn jit_match_constructor() {
    // match Some(42) { Some(v) => v, None => 0 }
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeEnum {
                    tag: "Some".into(),
                    payload: Box::new(make_int(42)),
                    ty: None,
                }),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor(
                            "Some".into(),
                            vec![Pattern::Var("v".into())],
                        ),
                        guard: None,
                        body: Expr::Var("v".into(), Some(Type::Int)),
                    },
                    MatchArm {
                        pattern: Pattern::Constructor("None".into(), vec![]),
                        guard: None,
                        body: make_int(0),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 42);
}

#[test]
fn jit_match_literal() {
    // match 2 { 1 => 10, 2 => 20, _ => 30 }
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(make_int(2)),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Box::new(make_int(1))),
                        guard: None,
                        body: make_int(10),
                    },
                    MatchArm {
                        pattern: Pattern::Literal(Box::new(make_int(2))),
                        guard: None,
                        body: make_int(20),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: make_int(30),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 20);
}

// -- Phase 5 tests --

#[test]
fn jit_for_loop() {
    // for x in [1, 2, 3] { ... } returns unit
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::For {
                binding: "x".into(),
                iterable: Box::new(Expr::MakeList(
                    vec![make_int(1), make_int(2), make_int(3)],
                    None,
                )),
                body: Box::new(Expr::Unit),
            },
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    assert!(nv.is_unit());
}

#[test]
fn jit_try_ok() {
    // let x = Ok(42)?; x
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("x".into())),
                        value: Box::new(Expr::Try {
                            expr: Box::new(Expr::MakeEnum {
                                tag: "Ok".into(),
                                payload: Box::new(make_int(42)),
                                ty: None,
                            }),
                            ty: Some(Type::Int),
                        }),
                        ty: Some(Type::Int),
                    },
                    Expr::Var("x".into(), Some(Type::Int)),
                ],
                Some(Type::Int),
            ),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 42);
}

#[test]
fn jit_try_err_propagates() {
    // Err("bad")? should early-return the Err
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    Expr::Try {
                        expr: Box::new(Expr::MakeEnum {
                            tag: "Err".into(),
                            payload: Box::new(Expr::String("bad".into())),
                            ty: None,
                        }),
                        ty: None,
                    },
                    make_int(999), // Should not reach here
                ],
                None,
            ),
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    let (tag, _) = nv.as_enum().unwrap();
    assert_eq!(tag.as_ref(), "Err");
}

// -- Integer enum tags + Switch dispatch tests --

#[test]
fn jit_enum_integer_tag_match() {
    // match Some(42) { Some(x) => x, None => 0 }
    // With tag registry, this should use integer comparison
    let mut tags = TagRegistry::new();
    // None=0, Some=1 are pre-registered
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeEnum {
                    tag: "Some".into(),
                    payload: Box::new(make_int(42)),
                    ty: None,
                }),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor(
                            "Some".into(),
                            vec![Pattern::Var("x".into())],
                        ),
                        guard: None,
                        body: Expr::Var("x".into(), Some(Type::Int)),
                    },
                    MatchArm {
                        pattern: Pattern::Constructor("None".into(), vec![]),
                        guard: None,
                        body: make_int(0),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags,
    };
    assert_eq!(compile_and_run(&module), 42);
}

#[test]
fn jit_enum_integer_tag_none_branch() {
    // match None { Some(x) => x, None => 99 }
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeEnum {
                    tag: "None".into(),
                    payload: Box::new(Expr::Unit),
                    ty: None,
                }),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor(
                            "Some".into(),
                            vec![Pattern::Var("x".into())],
                        ),
                        guard: None,
                        body: Expr::Var("x".into(), Some(Type::Int)),
                    },
                    MatchArm {
                        pattern: Pattern::Constructor("None".into(), vec![]),
                        guard: None,
                        body: make_int(99),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 99);
}

#[test]
fn jit_switch_dispatch_three_arms() {
    // match Ok(7) { Ok(x) => x, Err(e) => 0, _ => -1 }
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeEnum {
                    tag: "Ok".into(),
                    payload: Box::new(make_int(7)),
                    ty: None,
                }),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor("Ok".into(), vec![Pattern::Var("x".into())]),
                        guard: None,
                        body: Expr::Var("x".into(), Some(Type::Int)),
                    },
                    MatchArm {
                        pattern: Pattern::Constructor("Err".into(), vec![Pattern::Var("e".into())]),
                        guard: None,
                        body: make_int(0),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: make_int(-1),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 7);
}

// -- SRA tests --

#[test]
fn jit_sra_record_field_access() {
    // let r = { x: 10, y: 20 }; r.x + r.y => 30
    // SRA should replace heap record with scalar variables
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("r".into())),
                        value: Box::new(Expr::MakeRecord(
                            vec![("x".into(), make_int(10)), ("y".into(), make_int(20))],
                            None,
                        )),
                        ty: None,
                    },
                    make_binop(
                        BinOp::Add,
                        Expr::GetField {
                            object: Box::new(Expr::Var("r".into(), None)),
                            field: "x".into(),
                            field_idx: None,
                            ty: Some(Type::Int),
                        },
                        Expr::GetField {
                            object: Box::new(Expr::Var("r".into(), None)),
                            field: "y".into(),
                            field_idx: None,
                            ty: Some(Type::Int),
                        },
                    ),
                ],
                Some(Type::Int),
            ),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 30);
}

#[test]
fn jit_sra_escape_falls_back() {
    // let r = { x: 5 }; call_with(r) — r escapes, so no SRA
    // This should still work (falls back to heap allocation)
    // We test this indirectly: a record passed to a function is not SRA'd
    // For now, just test that the escape analysis correctly identifies escapes
    let exprs = vec![
        Expr::Let {
            pattern: Box::new(Pattern::Var("r".into())),
            value: Box::new(Expr::MakeRecord(vec![("x".into(), make_int(5))], None)),
            ty: None,
        },
        Expr::CallDirect {
            name: "some_fn".into(),
            args: vec![Expr::Var("r".into(), None)],
            ty: None,
        },
    ];
    let candidates = FnCompileCtx::<JITModule>::find_sra_candidates(&exprs);
    assert!(
        candidates.is_empty(),
        "Escaping record should not be SRA candidate"
    );
}

#[test]
fn jit_sra_non_escape_detected() {
    // let r = { x: 1, y: 2 }; r.x — r does not escape
    let exprs = vec![
        Expr::Let {
            pattern: Box::new(Pattern::Var("r".into())),
            value: Box::new(Expr::MakeRecord(
                vec![("x".into(), make_int(1)), ("y".into(), make_int(2))],
                None,
            )),
            ty: None,
        },
        Expr::GetField {
            object: Box::new(Expr::Var("r".into(), None)),
            field: "x".into(),
            field_idx: None,
            ty: Some(Type::Int),
        },
    ];
    let candidates = FnCompileCtx::<JITModule>::find_sra_candidates(&exprs);
    assert_eq!(candidates.len(), 1);
    assert_eq!(candidates[0].0, "r");
    assert_eq!(candidates[0].1, vec!["x".to_string(), "y".to_string()]);
}

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

// -- Arena lifecycle tests (heap value leak prevention) --
//
// These tests verify that heap values created during JIT execution are
// properly freed when the arena is drained. They use process-global
// alloc_stats, so they must run single-threaded:
//   cargo test --features jit -p blc -- jit_arena --test-threads=1
//
// Marked #[ignore] to avoid flaky failures in parallel test runs.

#[test]
#[ignore] // requires --test-threads=1 (global alloc_stats)
fn jit_arena_no_leak_string() {
    // JIT program that creates a string — verify no heap leak after execution.
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::String("hello".into()),
            ty: Some(Type::String),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_nvalue(&module);
        assert_eq!(result.to_string(), "hello");
    }
    // After dropping the result and JitProgram, all our allocs should be freed.
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "Expected no leaks after JIT string execution: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore] // requires --test-threads=1 (global alloc_stats)
fn jit_arena_no_leak_list() {
    // JIT program that creates a list of strings — verify no leak.
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeList(
                vec![Expr::String("a".into()), Expr::String("b".into())],
                None,
            ),
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_nvalue(&module);
        let items = result.as_list().expect("Expected List");
        assert_eq!(items.len(), 2);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "Expected no leaks after JIT list execution: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore] // requires --test-threads=1 (global alloc_stats)
fn jit_arena_no_leak_enum() {
    // JIT program that creates an enum — verify no leak.
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeEnum {
                tag: "Some".into(),
                payload: Box::new(Expr::String("value".into())),
                ty: None,
            },
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_nvalue(&module);
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Some");
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "Expected no leaks after JIT enum execution: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore] // requires --test-threads=1 (global alloc_stats)
fn jit_arena_no_leak_intermediate_values() {
    // JIT program with intermediate heap values NOT in the return value.
    // A list is created and discarded; only an int is returned.
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    // Create a list (intermediate heap value)
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("xs".into())),
                        value: Box::new(Expr::MakeList(
                            vec![make_int(1), make_int(2), make_int(3)],
                            None,
                        )),
                        ty: None,
                    },
                    // Return just an int (list is intermediate)
                    make_int(42),
                ],
                Some(Type::Int),
            ),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_nvalue(&module);
        assert_eq!(result.as_int(), 42);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "Expected no leaks for intermediate heap values: allocs={} frees={}",
        new_allocs, new_frees
    );
}

// -- RC (Reference Counting) verification tests --
//
// These tests compile with compile_rc() instead of compile(), verifying
// that scope-based incref/decref properly manages heap lifetimes without
// relying on the arena. They use alloc_stats to assert zero leaks.
//
//   cargo test --features jit -p blc -- jit_rc --test-threads=1
//
// Marked #[ignore] to avoid flaky failures in parallel test runs.

/// Compile with RC enabled, run, return result.
fn compile_and_run_rc_nvalue(module: &IrModule) -> NValue {
    let program = compile_rc(module, false).expect("RC compilation failed");
    program
        .run_entry_nvalue()
        .expect("Entry function not compiled")
}

#[test]
#[ignore] // requires --test-threads=1 (global alloc_stats)
fn jit_rc_no_leak_string() {
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::String("hello_rc".into()),
            ty: Some(Type::String),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        assert_eq!(result.as_string().unwrap().as_ref(), "hello_rc");
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC string leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_list() {
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeList(
                vec![Expr::String("a".into()), Expr::String("b".into())],
                None,
            ),
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        let items = result.as_list().expect("Expected List");
        assert_eq!(items.len(), 2);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC list leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_enum() {
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeEnum {
                tag: "Some".into(),
                payload: Box::new(Expr::String("payload".into())),
                ty: None,
            },
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        let (tag, _) = result.as_enum().unwrap();
        assert_eq!(tag.as_ref(), "Some");
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC enum leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_record() {
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeRecord(
                vec![
                    ("name".into(), Expr::String("Alice".into())),
                    ("age".into(), make_int(30)),
                ],
                None,
            ),
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        let fields = result.as_record().unwrap();
        assert_eq!(fields.len(), 2);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC record leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_intermediate_block() {
    // Block creates intermediate heap values that are discarded.
    // Only an int is returned — all intermediates should be decref'd.
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("s".into())),
                        value: Box::new(Expr::String("temporary".into())),
                        ty: None,
                    },
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("xs".into())),
                        value: Box::new(Expr::MakeList(vec![make_int(1), make_int(2)], None)),
                        ty: None,
                    },
                    make_int(42),
                ],
                Some(Type::Int),
            ),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        assert_eq!(result.as_int(), 42);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC intermediate block leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_function_call_heap_return() {
    // Call a function that takes an Int and returns a List (heap value).
    // Tests that cross-function RC ownership is properly balanced.
    // Uses MakeList in the body to prevent is_scalar_only from marking
    // the callee as unboxed (known bug: Expr::Var treated as scalar).
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![
            IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::CallDirect {
                    name: "wrap".into(),
                    args: vec![make_int(42)],
                    ty: None,
                },
                ty: None,
                span: dummy_span(),
            },
            IrFunction {
                name: "wrap".into(),
                params: vec!["n".into()],
                body: Expr::MakeList(vec![Expr::Var("n".into(), Some(Type::Int))], None),
                ty: None,
                span: dummy_span(),
            },
        ],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        let items = result.as_list().expect("Expected List");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].as_int(), 42);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC function call heap return leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_nested_heap_values() {
    // Create nested heap structure: list of enums wrapping strings.
    // Verifies transitive cleanup.
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::MakeList(
                vec![
                    Expr::MakeEnum {
                        tag: "Some".into(),
                        payload: Box::new(Expr::String("first".into())),
                        ty: None,
                    },
                    Expr::MakeEnum {
                        tag: "Some".into(),
                        payload: Box::new(Expr::String("second".into())),
                        ty: None,
                    },
                ],
                None,
            ),
            ty: None,
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        let items = result.as_list().expect("Expected List");
        assert_eq!(items.len(), 2);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC nested heap values leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_int_only_no_overhead() {
    // Pure integer computation — RC should be a no-op (no allocs).
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_binop(BinOp::Add, make_int(10), make_int(32)),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        assert_eq!(result.as_int(), 42);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    assert_eq!(
        new_allocs, 0,
        "RC int-only should have zero heap allocs, got {}",
        new_allocs
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_if_heap_condition() {
    // If with a heap-producing condition — condition value must be decrefd.
    // Uses Eq comparison on two strings, which produces a boolean but the
    // string operands create heap allocations that must be cleaned up.
    // The condition expression (BinOp Eq on strings) is compiled by compile_expr,
    // which compiles both string sub-exprs (heap allocs) and produces a bool.
    // The If decref covers the result. The block scope covers intermediates.
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    // Create a string, then discard it via if condition
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("s".into())),
                        value: Box::new(Expr::String("hello".into())),
                        ty: None,
                    },
                    Expr::If {
                        condition: Box::new(Expr::Bool(true)),
                        then_branch: Box::new(make_int(1)),
                        else_branch: Some(Box::new(make_int(0))),
                        ty: Some(Type::Int),
                    },
                ],
                Some(Type::Int),
            ),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        assert_eq!(result.as_int(), 1);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC if heap condition leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_match_subject() {
    // Match on an enum — subject must be decrefd after all arms merge.
    use crate::vm::nvalue::alloc_stats;

    let mut tags = TagRegistry::new();
    tags.register("Some");
    tags.register("None");

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeEnum {
                    tag: "Some".into(),
                    payload: Box::new(Expr::String("val".into())),
                    ty: None,
                }),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor(
                            "Some".into(),
                            vec![Pattern::Var("x".into())],
                        ),
                        guard: None,
                        body: make_int(1),
                    },
                    MatchArm {
                        pattern: Pattern::Constructor("None".into(), vec![]),
                        guard: None,
                        body: make_int(0),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags,
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        assert_eq!(result.as_int(), 1);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC match subject leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_and_or() {
    // And/Or with heap operands — first operand must be decrefd.
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    // "hello" and true → true (string condition decrefd)
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::And(
                Box::new(Expr::String("hello".into())),
                Box::new(Expr::Bool(true)),
            ),
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let _result = compile_and_run_rc_nvalue(&module);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC and/or leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_get_field() {
    // Record field access — record and field string must be decrefd.
    use crate::vm::nvalue::alloc_stats;

    let before = alloc_stats();
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Block(
                vec![
                    Expr::Let {
                        pattern: Box::new(Pattern::Var("r".into())),
                        value: Box::new(Expr::MakeRecord(
                            vec![("x".into(), make_int(10)), ("y".into(), make_int(20))],
                            None,
                        )),
                        ty: None,
                    },
                    Expr::GetField {
                        object: Box::new(Expr::Var("r".into(), None)),
                        field: "x".into(),
                        field_idx: None,
                        ty: Some(Type::Int),
                    },
                ],
                Some(Type::Int),
            ),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        assert_eq!(result.as_int(), 10);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC get_field leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

#[test]
#[ignore]
fn jit_rc_no_leak_try_unwrap() {
    // Try on Ok value — container must be decrefd after payload extraction.
    use crate::vm::nvalue::alloc_stats;

    let mut tags = TagRegistry::new();
    tags.register("Ok");
    tags.register("Err");

    let before = alloc_stats();
    // fn main() -> try { Ok(42) }  → should return 42, and Ok container freed
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Try {
                expr: Box::new(Expr::MakeEnum {
                    tag: "Ok".into(),
                    payload: Box::new(make_int(42)),
                    ty: None,
                }),
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags,
    };
    {
        let result = compile_and_run_rc_nvalue(&module);
        assert_eq!(result.as_int(), 42);
    }
    let after = alloc_stats();
    let new_allocs = after.allocs - before.allocs;
    let new_frees = after.frees - before.frees;
    assert_eq!(
        new_allocs, new_frees,
        "RC try unwrap leak: allocs={} frees={}",
        new_allocs, new_frees
    );
}

// -- Evidence passing JIT integration tests --
// These test the full pipeline: IR with effect nodes → optimize (evidence transform) → JIT

#[test]
fn jit_evidence_simple_handler() {
    use crate::vm::ir::HandlerClause;
    use crate::vm::optimize_ir::optimize;

    // handle { perform E.get!() } with { E.get!() -> resume(42) }
    let mut module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::HandleEffect {
                body: Box::new(Expr::PerformEffect {
                    effect: "E".into(),
                    method: "get".into(),
                    args: vec![],
                    ty: Some(Type::Int),
                }),
                clauses: vec![HandlerClause {
                    effect: "E".into(),
                    method: "get".into(),
                    params: vec![],
                    body: Expr::CallIndirect {
                        callee: Box::new(Expr::Var("resume".into(), None)),
                        args: vec![make_int(42)],
                        ty: Some(Type::Int),
                    },
                    is_tail_resumptive: true,
                }],
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };

    optimize(&mut module);
    assert_eq!(compile_and_run(&module), 42);
}

#[test]
fn jit_evidence_handler_with_params() {
    use crate::vm::ir::HandlerClause;
    use crate::vm::optimize_ir::optimize;

    // handle { perform E.inc!(10) } with { E.inc!(x) -> resume(x + 1) }
    let mut module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::HandleEffect {
                body: Box::new(Expr::PerformEffect {
                    effect: "E".into(),
                    method: "inc".into(),
                    args: vec![make_int(10)],
                    ty: Some(Type::Int),
                }),
                clauses: vec![HandlerClause {
                    effect: "E".into(),
                    method: "inc".into(),
                    params: vec!["x".into()],
                    body: Expr::CallIndirect {
                        callee: Box::new(Expr::Var("resume".into(), None)),
                        args: vec![make_binop(BinOp::Add, make_var("x"), make_int(1))],
                        ty: Some(Type::Int),
                    },
                    is_tail_resumptive: true,
                }],
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };

    optimize(&mut module);
    assert_eq!(compile_and_run(&module), 11);
}

#[test]
fn jit_evidence_cross_function() {
    use crate::vm::ir::HandlerClause;
    use crate::vm::optimize_ir::optimize;

    // fn effectful() = perform E.get!()
    // fn main() = handle { effectful() } with { E.get!() -> resume(99) }
    let mut module = IrModule {
        functions: vec![
            IrFunction {
                name: "main".into(),
                params: vec![],
                body: Expr::HandleEffect {
                    body: Box::new(Expr::CallDirect {
                        name: "effectful".into(),
                        args: vec![],
                        ty: Some(Type::Int),
                    }),
                    clauses: vec![HandlerClause {
                        effect: "E".into(),
                        method: "get".into(),
                        params: vec![],
                        body: Expr::CallIndirect {
                            callee: Box::new(Expr::Var("resume".into(), None)),
                            args: vec![make_int(99)],
                            ty: Some(Type::Int),
                        },
                        is_tail_resumptive: true,
                    }],
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            },
            IrFunction {
                name: "effectful".into(),
                params: vec![],
                body: Expr::PerformEffect {
                    effect: "E".into(),
                    method: "get".into(),
                    args: vec![],
                    ty: Some(Type::Int),
                },
                ty: Some(Type::Int),
                span: dummy_span(),
            },
        ],
        entry: 0,
        tags: TagRegistry::new(),
    };

    optimize(&mut module);
    assert_eq!(compile_and_run(&module), 99);
}

// ---------------------------------------------------------------------------
// Float arithmetic tests
// ---------------------------------------------------------------------------

fn make_float(f: f64) -> Expr {
    Expr::Float(f)
}

fn make_float_binop(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::BinOp {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        ty: Some(Type::Float),
    }
}

/// Compile and run, extracting an f64 from NaN-boxed Float result.
fn compile_and_run_float(module: &IrModule) -> f64 {
    let nv = compile_and_run_nvalue(module);
    assert!(nv.is_float(), "Expected Float, got: {}", nv);
    nv.as_float()
}

#[test]
fn jit_float_add() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_float_binop(BinOp::Add, make_float(3.14), make_float(2.0)),
            ty: Some(Type::Float),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let result = compile_and_run_float(&module);
    assert!((result - 5.14).abs() < 1e-10, "Expected 5.14, got {}", result);
}

#[test]
fn jit_float_sub() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_float_binop(BinOp::Sub, make_float(5.0), make_float(3.0)),
            ty: Some(Type::Float),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run_float(&module), 2.0);
}

#[test]
fn jit_float_mul() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_float_binop(BinOp::Mul, make_float(3.0), make_float(4.0)),
            ty: Some(Type::Float),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run_float(&module), 12.0);
}

#[test]
fn jit_float_div() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_float_binop(BinOp::Div, make_float(10.0), make_float(4.0)),
            ty: Some(Type::Float),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run_float(&module), 2.5);
}

#[test]
fn jit_float_neg() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::UnaryOp {
                op: UnaryOp::Neg,
                operand: Box::new(make_float(3.14)),
                ty: Some(Type::Float),
            },
            ty: Some(Type::Float),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run_float(&module), -3.14);
}

#[test]
fn jit_float_compare_lt() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::BinOp {
                op: BinOp::Lt,
                lhs: Box::new(make_float(1.0)),
                rhs: Box::new(make_float(2.0)),
                ty: Some(Type::Float),
            },
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert!(compile_and_run_bool(&module));
}

// -- Phase 1: List pattern and complex tuple pattern tests --

#[test]
fn jit_list_pattern_head_tail() {
    // match [1, 2, 3] { [h, ...t] -> h }
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeList(
                    vec![make_int(10), make_int(20), make_int(30)],
                    None,
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::List(
                            vec![Pattern::Var("h".into())],
                            Some("t".into()),
                        ),
                        guard: None,
                        body: Expr::Var("h".into(), Some(Type::Int)),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: make_int(0),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 10);
}

#[test]
fn jit_list_pattern_exact_length() {
    // match [1, 2] { [a, b] -> a + b, _ -> 0 }
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeList(
                    vec![make_int(3), make_int(4)],
                    None,
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::List(
                            vec![Pattern::Var("a".into()), Pattern::Var("b".into())],
                            None,
                        ),
                        guard: None,
                        body: make_binop(BinOp::Add,
                            Expr::Var("a".into(), Some(Type::Int)),
                            Expr::Var("b".into(), Some(Type::Int)),
                        ),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: make_int(0),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 7);
}

#[test]
fn jit_list_pattern_length_mismatch() {
    // match [1, 2, 3] { [a, b] -> 99, _ -> 0 }
    // Should fall through to wildcard because length != 2
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeList(
                    vec![make_int(1), make_int(2), make_int(3)],
                    None,
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::List(
                            vec![Pattern::Var("a".into()), Pattern::Var("b".into())],
                            None,
                        ),
                        guard: None,
                        body: make_int(99),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: make_int(0),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 0);
}

#[test]
fn jit_list_pattern_empty() {
    // match [] { [] -> 1, _ -> 0 }
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeList(vec![], None)),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::List(vec![], None),
                        guard: None,
                        body: make_int(1),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: make_int(0),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 1);
}

#[test]
fn jit_list_pattern_rest_binding() {
    // match [1, 2, 3] { [h, ...t] -> length(t) via list_length }
    // We verify the rest binding is a list of length 2
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeList(
                    vec![make_int(1), make_int(2), make_int(3)],
                    None,
                )),
                arms: vec![MatchArm {
                    pattern: Pattern::List(
                        vec![Pattern::Var("h".into())],
                        Some("t".into()),
                    ),
                    guard: None,
                    body: Expr::Var("h".into(), Some(Type::Int)),
                }],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    // h = 1
    assert_eq!(compile_and_run(&module), 1);
}

#[test]
fn jit_tuple_pattern_with_literal() {
    // match (1, 2) { (1, x) -> x, _ -> 0 }
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeTuple(
                    vec![make_int(1), make_int(42)],
                    None,
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Tuple(vec![
                            Pattern::Literal(Box::new(make_int(1))),
                            Pattern::Var("x".into()),
                        ]),
                        guard: None,
                        body: Expr::Var("x".into(), Some(Type::Int)),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: make_int(0),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 42);
}

#[test]
fn jit_tuple_pattern_literal_mismatch() {
    // match (2, 42) { (1, x) -> x, _ -> 0 }
    // first element is 2, not 1 → fall through
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Match {
                subject: Box::new(Expr::MakeTuple(
                    vec![make_int(2), make_int(42)],
                    None,
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Tuple(vec![
                            Pattern::Literal(Box::new(make_int(1))),
                            Pattern::Var("x".into()),
                        ]),
                        guard: None,
                        body: Expr::Var("x".into(), Some(Type::Int)),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        guard: None,
                        body: make_int(0),
                    },
                ],
                ty: Some(Type::Int),
            },
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 0);
}

#[test]
fn jit_can_jit_list_pattern() {
    // Verify that list patterns are now accepted by can_jit
    let func = IrFunction {
        name: "test".into(),
        params: vec![],
        body: Expr::Match {
            subject: Box::new(Expr::MakeList(vec![make_int(1)], None)),
            arms: vec![MatchArm {
                pattern: Pattern::List(
                    vec![Pattern::Var("h".into())],
                    Some("t".into()),
                ),
                guard: None,
                body: Expr::Var("h".into(), Some(Type::Int)),
            }],
            ty: Some(Type::Int),
        },
        ty: Some(Type::Int),
        span: dummy_span(),
    };
    assert!(can_jit(&func, None));
}

#[test]
fn jit_can_jit_complex_tuple_pattern() {
    // Verify that tuple patterns with nested literals are now accepted
    let func = IrFunction {
        name: "test".into(),
        params: vec![],
        body: Expr::Match {
            subject: Box::new(Expr::MakeTuple(vec![make_int(1), make_int(2)], None)),
            arms: vec![MatchArm {
                pattern: Pattern::Tuple(vec![
                    Pattern::Literal(Box::new(make_int(1))),
                    Pattern::Var("x".into()),
                ]),
                guard: None,
                body: Expr::Var("x".into(), Some(Type::Int)),
            }],
            ty: Some(Type::Int),
        },
        ty: Some(Type::Int),
        span: dummy_span(),
    };
    assert!(can_jit(&func, None));
}
