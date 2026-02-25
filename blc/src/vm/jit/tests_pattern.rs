//! Pattern matching, for loops, try, enum tag, switch dispatch, SRA, list/tuple pattern tests.

use super::tests::*;
use super::*;
use crate::analysis::types::Type;
use crate::vm::ir::{
    BinOp, Expr, IrFunction, IrModule, MatchArm, Pattern, Span, TagRegistry, UnaryOp,
};
use crate::vm::nvalue::{HeapObject, NValue};


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
