use super::analysis::can_jit;
use super::compile::FnCompileCtx;
use super::*;
use crate::analysis::types::Type;
use crate::vm::ir::{
    BinOp, Expr, IrFunction, IrModule, MatchArm, Pattern, Span, TagRegistry, UnaryOp,
};
use crate::vm::nvalue::{HeapObject, NValue};

pub(super) fn dummy_span() -> Span {
    Span {
        line: 0,
        col: 0,
        start_byte: 0,
        end_byte: 0,
    }
}

pub(super) fn make_int(n: i64) -> Expr {
    Expr::Int(n)
}

pub(super) fn make_var(name: &str) -> Expr {
    Expr::Var(name.to_string(), Some(Type::Int))
}

pub(super) fn make_binop(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::BinOp {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        ty: Some(Type::Int),
    }
}

pub(super) fn make_bool_binop(op: BinOp, lhs: Expr, rhs: Expr) -> Expr {
    Expr::BinOp {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        ty: Some(Type::Bool),
    }
}

/// Compile and run, returning the NaN-boxed result as NValue.
pub(super) fn compile_and_run_nvalue(module: &IrModule) -> NValue {
    let program = compile(module, false).expect("JIT compilation failed");
    program
        .run_entry_nvalue()
        .expect("Entry function not compiled")
}

/// Compile and run, extracting an i64 from NaN-boxed Int result.
pub(super) fn compile_and_run(module: &IrModule) -> i64 {
    let nv = compile_and_run_nvalue(module);
    assert!(nv.is_int(), "Expected Int, got: {}", nv);
    nv.as_int()
}

/// Compile and run, extracting a bool from NaN-boxed Bool result.
pub(super) fn compile_and_run_bool(module: &IrModule) -> bool {
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

// -- UpdateRecord, Concat, and NaN-boxing boundary tests --

#[test]
fn jit_update_record() {
    // { ..{x: 1, y: 2}, x: 10 } → {x: 10, y: 2}
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::GetField {
                object: Box::new(Expr::UpdateRecord {
                    base: Box::new(Expr::MakeRecord(
                        vec![("x".into(), make_int(1)), ("y".into(), make_int(2))],
                        None,
                    )),
                    updates: vec![("x".into(), make_int(10))],
                    ty: None,
                }),
                field: "x".into(),
                field_idx: None,
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
fn jit_update_record_preserves_other_fields() {
    // { ..{x: 1, y: 2}, x: 10 }.y should still be 2
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::GetField {
                object: Box::new(Expr::UpdateRecord {
                    base: Box::new(Expr::MakeRecord(
                        vec![("x".into(), make_int(1)), ("y".into(), make_int(2))],
                        None,
                    )),
                    updates: vec![("x".into(), make_int(10))],
                    ty: None,
                }),
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
    assert_eq!(compile_and_run(&module), 2);
}

#[test]
fn jit_string_concat() {
    // "hello" ++ " " ++ "world" → "hello world"
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Concat(vec![
                Expr::String("hello".into()),
                Expr::String(" ".into()),
                Expr::String("world".into()),
            ]),
            ty: Some(Type::String),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    assert_eq!(nv.as_string().unwrap().as_ref(), "hello world");
}

#[test]
fn jit_string_concat_empty() {
    // "" ++ "" → ""
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Concat(vec![Expr::String("".into()), Expr::String("".into())]),
            ty: Some(Type::String),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    assert_eq!(nv.as_string().unwrap().as_ref(), "");
}

#[test]
fn jit_nan_boxing_max_inline_int() {
    // 2^47 - 1 = 140_737_488_355_327 is the max 48-bit signed value
    let max_inline = 140_737_488_355_327i64;
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_int(max_inline),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), max_inline);
}

#[test]
fn jit_nan_boxing_min_inline_int() {
    // -2^47 = -140_737_488_355_328
    let min_inline = -140_737_488_355_328i64;
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_int(min_inline),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), min_inline);
}

#[test]
fn jit_nan_boxing_zero() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: make_int(0),
            ty: Some(Type::Int),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert_eq!(compile_and_run(&module), 0);
}

#[test]
fn jit_float_literal() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Float(3.14),
            ty: Some(Type::Float),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    let nv = compile_and_run_nvalue(&module);
    assert!(nv.is_float());
    assert!((nv.as_float() - 3.14).abs() < 1e-10);
}
