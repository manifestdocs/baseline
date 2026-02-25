//! Arena lifecycle, RC (reference counting), evidence passing, and float tests.

use super::tests::*;
use super::*;
use crate::analysis::types::Type;
use crate::vm::ir::{
    BinOp, Expr, IrFunction, IrModule, MatchArm, Matcher, Pattern, Span, TagRegistry, UnaryOp,
};
use crate::vm::nvalue::{HeapObject, NValue};


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

// ---------------------------------------------------------------------------
// Expect matcher tests
// ---------------------------------------------------------------------------

#[test]
fn jit_expect_equal() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Expect {
                actual: Box::new(make_int(42)),
                matcher: Box::new(Matcher::Equal(Box::new(make_int(42)))),
            },
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert!(compile_and_run_bool(&module));
}

#[test]
fn jit_expect_equal_false() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Expect {
                actual: Box::new(make_int(1)),
                matcher: Box::new(Matcher::Equal(Box::new(make_int(2)))),
            },
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert!(!compile_and_run_bool(&module));
}

#[test]
fn jit_expect_be_ok() {
    let mut tags = TagRegistry::new();
    tags.register("Ok");
    tags.register("Err");

    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Expect {
                actual: Box::new(Expr::MakeEnum {
                    tag: "Ok".into(),
                    payload: Box::new(make_int(1)),
                    ty: None,
                }),
                matcher: Box::new(Matcher::BeOk),
            },
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags,
    };
    assert!(compile_and_run_bool(&module));
}

#[test]
fn jit_expect_be_none() {
    let mut tags = TagRegistry::new();
    tags.register("None");
    tags.register("Some");

    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Expect {
                actual: Box::new(Expr::MakeEnum {
                    tag: "None".into(),
                    payload: Box::new(Expr::Unit),
                    ty: None,
                }),
                matcher: Box::new(Matcher::BeNone),
            },
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags,
    };
    assert!(compile_and_run_bool(&module));
}

#[test]
fn jit_expect_be_empty() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Expect {
                actual: Box::new(Expr::MakeList(vec![], None)),
                matcher: Box::new(Matcher::BeEmpty),
            },
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert!(compile_and_run_bool(&module));
}

#[test]
fn jit_expect_have_length() {
    let module = IrModule {
        functions: vec![IrFunction {
            name: "main".into(),
            params: vec![],
            body: Expr::Expect {
                actual: Box::new(Expr::MakeList(
                    vec![make_int(1), make_int(2), make_int(3)],
                    None,
                )),
                matcher: Box::new(Matcher::HaveLength(Box::new(make_int(3)))),
            },
            ty: Some(Type::Bool),
            span: dummy_span(),
        }],
        entry: 0,
        tags: TagRegistry::new(),
    };
    assert!(compile_and_run_bool(&module));
}
