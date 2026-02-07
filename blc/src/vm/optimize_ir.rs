use std::collections::HashMap;

use super::ir::{BinOp, Expr, IrModule, MatchArm, Pattern, UnaryOp};

/// Run all IR optimization passes on the module.
pub fn optimize(module: &mut IrModule) {
    for func in &mut module.functions {
        func.body = propagate_and_fold(func.body.clone());
        func.body = eliminate_dead_lets(func.body.clone());
    }
}

// ---------------------------------------------------------------------------
// Pass 1+2: Constant propagation + constant folding (combined single walk)
// ---------------------------------------------------------------------------

/// Propagate known literal bindings through expressions, folding constant
/// operations as we go.
fn propagate_and_fold(expr: Expr) -> Expr {
    let mut env: Vec<HashMap<String, Expr>> = vec![HashMap::new()];
    propagate_expr(expr, &mut env)
}

fn propagate_expr(expr: Expr, env: &mut Vec<HashMap<String, Expr>>) -> Expr {
    match expr {
        Expr::Var(ref name, _) => {
            // Look up in scopes (innermost first)
            for scope in env.iter().rev() {
                if let Some(lit) = scope.get(name) {
                    return lit.clone();
                }
            }
            expr
        }

        Expr::Let { pattern, value, ty } => {
            let value = propagate_expr(*value, env);
            // Track simple variable bindings to literals
            if let Pattern::Var(ref name) = *pattern
                && is_literal(&value)
                && let Some(scope) = env.last_mut()
            {
                scope.insert(name.clone(), value.clone());
            }
            Expr::Let {
                pattern,
                value: Box::new(value),
                ty,
            }
        }

        Expr::Block(exprs, ty) => {
            env.push(HashMap::new());
            let exprs = exprs.into_iter().map(|e| propagate_expr(e, env)).collect();
            env.pop();
            Expr::Block(exprs, ty)
        }

        Expr::BinOp { op, lhs, rhs, ty } => {
            let lhs = propagate_expr(*lhs, env);
            let rhs = propagate_expr(*rhs, env);
            // Try constant folding
            if let Some(folded) = try_fold_binop(op, &lhs, &rhs) {
                return folded;
            }
            Expr::BinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                ty,
            }
        }

        Expr::UnaryOp { op, operand, ty } => {
            let operand = propagate_expr(*operand, env);
            if let Some(folded) = try_fold_unaryop(op, &operand) {
                return folded;
            }
            Expr::UnaryOp {
                op,
                operand: Box::new(operand),
                ty,
            }
        }

        Expr::If {
            condition,
            then_branch,
            else_branch,
            ty,
        } => Expr::If {
            condition: Box::new(propagate_expr(*condition, env)),
            then_branch: Box::new(propagate_expr(*then_branch, env)),
            else_branch: else_branch.map(|e| Box::new(propagate_expr(*e, env))),
            ty,
        },

        Expr::Match { subject, arms, ty } => Expr::Match {
            subject: Box::new(propagate_expr(*subject, env)),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern,
                    body: propagate_expr(arm.body, env),
                })
                .collect(),
            ty,
        },

        Expr::CallDirect { name, args, ty } => Expr::CallDirect {
            name,
            args: args.into_iter().map(|a| propagate_expr(a, env)).collect(),
            ty,
        },

        Expr::CallNative {
            module,
            method,
            args,
            ty,
        } => Expr::CallNative {
            module,
            method,
            args: args.into_iter().map(|a| propagate_expr(a, env)).collect(),
            ty,
        },

        Expr::CallIndirect { callee, args, ty } => Expr::CallIndirect {
            callee: Box::new(propagate_expr(*callee, env)),
            args: args.into_iter().map(|a| propagate_expr(a, env)).collect(),
            ty,
        },

        Expr::TailCall { name, args, ty } => Expr::TailCall {
            name,
            args: args.into_iter().map(|a| propagate_expr(a, env)).collect(),
            ty,
        },

        Expr::MakeEnum { tag, payload, ty } => Expr::MakeEnum {
            tag,
            payload: Box::new(propagate_expr(*payload, env)),
            ty,
        },

        Expr::MakeStruct { name, fields, ty } => Expr::MakeStruct {
            name,
            fields: fields
                .into_iter()
                .map(|(k, v)| (k, propagate_expr(v, env)))
                .collect(),
            ty,
        },

        Expr::MakeList(items, ty) => Expr::MakeList(
            items.into_iter().map(|e| propagate_expr(e, env)).collect(),
            ty,
        ),

        Expr::MakeRecord(fields, ty) => Expr::MakeRecord(
            fields
                .into_iter()
                .map(|(k, v)| (k, propagate_expr(v, env)))
                .collect(),
            ty,
        ),

        Expr::MakeTuple(items, ty) => Expr::MakeTuple(
            items.into_iter().map(|e| propagate_expr(e, env)).collect(),
            ty,
        ),

        Expr::MakeRange(start, end) => Expr::MakeRange(
            Box::new(propagate_expr(*start, env)),
            Box::new(propagate_expr(*end, env)),
        ),

        Expr::UpdateRecord { base, updates, ty } => Expr::UpdateRecord {
            base: Box::new(propagate_expr(*base, env)),
            updates: updates
                .into_iter()
                .map(|(k, v)| (k, propagate_expr(v, env)))
                .collect(),
            ty,
        },

        Expr::GetField { object, field, ty } => Expr::GetField {
            object: Box::new(propagate_expr(*object, env)),
            field,
            ty,
        },

        Expr::And(lhs, rhs) => Expr::And(
            Box::new(propagate_expr(*lhs, env)),
            Box::new(propagate_expr(*rhs, env)),
        ),

        Expr::Or(lhs, rhs) => Expr::Or(
            Box::new(propagate_expr(*lhs, env)),
            Box::new(propagate_expr(*rhs, env)),
        ),

        Expr::For {
            binding,
            iterable,
            body,
        } => {
            // The binding shadows, so push a new scope for the body
            env.push(HashMap::new());
            let result = Expr::For {
                binding,
                iterable: Box::new(propagate_expr(*iterable, env)),
                body: Box::new(propagate_expr(*body, env)),
            };
            env.pop();
            result
        }

        Expr::Lambda { params, body, ty } => {
            // Lambda params shadow outer bindings
            env.push(HashMap::new());
            let result = Expr::Lambda {
                params,
                body: Box::new(propagate_expr(*body, env)),
                ty,
            };
            env.pop();
            result
        }

        Expr::Try { expr, ty } => Expr::Try {
            expr: Box::new(propagate_expr(*expr, env)),
            ty,
        },

        Expr::Concat(parts) => {
            Expr::Concat(parts.into_iter().map(|e| propagate_expr(e, env)).collect())
        }

        // Literals pass through unchanged
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::Unit => expr,
    }
}

fn is_literal(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::String(_) | Expr::Unit
    )
}

// ---------------------------------------------------------------------------
// Constant folding helpers
// ---------------------------------------------------------------------------

fn try_fold_binop(op: BinOp, lhs: &Expr, rhs: &Expr) -> Option<Expr> {
    match (lhs, rhs) {
        (Expr::Int(a), Expr::Int(b)) => fold_int_binop(op, *a, *b),
        (Expr::Float(a), Expr::Float(b)) => fold_float_binop(op, *a, *b),
        (Expr::Bool(a), Expr::Bool(b)) => fold_bool_binop(op, *a, *b),
        (Expr::String(a), Expr::String(b)) => fold_string_binop(op, a, b),
        _ => None,
    }
}

fn fold_int_binop(op: BinOp, a: i64, b: i64) -> Option<Expr> {
    match op {
        BinOp::Add => Some(Expr::Int(a.wrapping_add(b))),
        BinOp::Sub => Some(Expr::Int(a.wrapping_sub(b))),
        BinOp::Mul => Some(Expr::Int(a.wrapping_mul(b))),
        BinOp::Div => {
            if b == 0 {
                None
            } else {
                Some(Expr::Int(a / b))
            }
        }
        BinOp::Mod => {
            if b == 0 {
                None
            } else {
                Some(Expr::Int(a % b))
            }
        }
        BinOp::Eq => Some(Expr::Bool(a == b)),
        BinOp::Ne => Some(Expr::Bool(a != b)),
        BinOp::Lt => Some(Expr::Bool(a < b)),
        BinOp::Gt => Some(Expr::Bool(a > b)),
        BinOp::Le => Some(Expr::Bool(a <= b)),
        BinOp::Ge => Some(Expr::Bool(a >= b)),
    }
}

fn fold_float_binop(op: BinOp, a: f64, b: f64) -> Option<Expr> {
    match op {
        BinOp::Add => Some(Expr::Float(a + b)),
        BinOp::Sub => Some(Expr::Float(a - b)),
        BinOp::Mul => Some(Expr::Float(a * b)),
        BinOp::Div => Some(Expr::Float(a / b)),
        BinOp::Mod => Some(Expr::Float(a % b)),
        BinOp::Eq => Some(Expr::Bool(a == b)),
        BinOp::Ne => Some(Expr::Bool(a != b)),
        BinOp::Lt => Some(Expr::Bool(a < b)),
        BinOp::Gt => Some(Expr::Bool(a > b)),
        BinOp::Le => Some(Expr::Bool(a <= b)),
        BinOp::Ge => Some(Expr::Bool(a >= b)),
    }
}

fn fold_bool_binop(op: BinOp, a: bool, b: bool) -> Option<Expr> {
    match op {
        BinOp::Eq => Some(Expr::Bool(a == b)),
        BinOp::Ne => Some(Expr::Bool(a != b)),
        _ => None,
    }
}

fn fold_string_binop(op: BinOp, a: &str, b: &str) -> Option<Expr> {
    match op {
        BinOp::Eq => Some(Expr::Bool(a == b)),
        BinOp::Ne => Some(Expr::Bool(a != b)),
        BinOp::Lt => Some(Expr::Bool(a < b)),
        BinOp::Gt => Some(Expr::Bool(a > b)),
        BinOp::Le => Some(Expr::Bool(a <= b)),
        BinOp::Ge => Some(Expr::Bool(a >= b)),
        _ => None,
    }
}

fn try_fold_unaryop(op: UnaryOp, operand: &Expr) -> Option<Expr> {
    match (op, operand) {
        (UnaryOp::Neg, Expr::Int(n)) => Some(Expr::Int(-n)),
        (UnaryOp::Neg, Expr::Float(f)) => Some(Expr::Float(-f)),
        (UnaryOp::Not, Expr::Bool(b)) => Some(Expr::Bool(!b)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Pass 3: Dead let elimination
// ---------------------------------------------------------------------------

/// Remove `let` bindings whose variables are never referenced.
fn eliminate_dead_lets(expr: Expr) -> Expr {
    let mut counts: HashMap<String, usize> = HashMap::new();
    count_var_uses(&expr, &mut counts);
    remove_dead_lets(expr, &counts)
}

fn count_var_uses(expr: &Expr, counts: &mut HashMap<String, usize>) {
    match expr {
        Expr::Var(name, _) => {
            *counts.entry(name.clone()).or_insert(0) += 1;
        }
        Expr::Let { value, .. } => {
            count_var_uses(value, counts);
        }
        Expr::Block(exprs, _) => {
            for e in exprs {
                count_var_uses(e, counts);
            }
        }
        Expr::BinOp { lhs, rhs, .. } => {
            count_var_uses(lhs, counts);
            count_var_uses(rhs, counts);
        }
        Expr::UnaryOp { operand, .. } => {
            count_var_uses(operand, counts);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            count_var_uses(condition, counts);
            count_var_uses(then_branch, counts);
            if let Some(e) = else_branch {
                count_var_uses(e, counts);
            }
        }
        Expr::Match { subject, arms, .. } => {
            count_var_uses(subject, counts);
            for arm in arms {
                count_var_uses(&arm.body, counts);
            }
        }
        Expr::CallDirect { args, .. }
        | Expr::CallNative { args, .. }
        | Expr::TailCall { args, .. } => {
            for a in args {
                count_var_uses(a, counts);
            }
        }
        Expr::CallIndirect { callee, args, .. } => {
            count_var_uses(callee, counts);
            for a in args {
                count_var_uses(a, counts);
            }
        }
        Expr::MakeEnum { payload, .. } => {
            count_var_uses(payload, counts);
        }
        Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => {
            for (_, v) in fields {
                count_var_uses(v, counts);
            }
        }
        Expr::MakeList(items, _) | Expr::MakeTuple(items, _) => {
            for item in items {
                count_var_uses(item, counts);
            }
        }
        Expr::MakeRange(start, end) => {
            count_var_uses(start, counts);
            count_var_uses(end, counts);
        }
        Expr::UpdateRecord { base, updates, .. } => {
            count_var_uses(base, counts);
            for (_, v) in updates {
                count_var_uses(v, counts);
            }
        }
        Expr::GetField { object, .. } => {
            count_var_uses(object, counts);
        }
        Expr::And(lhs, rhs) | Expr::Or(lhs, rhs) => {
            count_var_uses(lhs, counts);
            count_var_uses(rhs, counts);
        }
        Expr::For { iterable, body, .. } => {
            count_var_uses(iterable, counts);
            count_var_uses(body, counts);
        }
        Expr::Lambda { body, .. } => {
            count_var_uses(body, counts);
        }
        Expr::Try { expr, .. } => {
            count_var_uses(expr, counts);
        }
        Expr::Concat(parts) => {
            for p in parts {
                count_var_uses(p, counts);
            }
        }
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::Unit => {}
    }
}

fn remove_dead_lets(expr: Expr, counts: &HashMap<String, usize>) -> Expr {
    match expr {
        Expr::Let { pattern, value, ty } => {
            let value = remove_dead_lets(*value, counts);
            // Only eliminate if: simple var binding, literal value, zero references
            if let Pattern::Var(ref name) = *pattern
                && is_literal(&value)
                && counts.get(name).copied().unwrap_or(0) == 0
            {
                return Expr::Unit;
            }
            Expr::Let {
                pattern,
                value: Box::new(value),
                ty,
            }
        }
        Expr::Block(exprs, ty) => {
            let exprs: Vec<Expr> = exprs
                .into_iter()
                .map(|e| remove_dead_lets(e, counts))
                .collect();
            Expr::Block(exprs, ty)
        }
        Expr::BinOp { op, lhs, rhs, ty } => Expr::BinOp {
            op,
            lhs: Box::new(remove_dead_lets(*lhs, counts)),
            rhs: Box::new(remove_dead_lets(*rhs, counts)),
            ty,
        },
        Expr::UnaryOp { op, operand, ty } => Expr::UnaryOp {
            op,
            operand: Box::new(remove_dead_lets(*operand, counts)),
            ty,
        },
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ty,
        } => Expr::If {
            condition: Box::new(remove_dead_lets(*condition, counts)),
            then_branch: Box::new(remove_dead_lets(*then_branch, counts)),
            else_branch: else_branch.map(|e| Box::new(remove_dead_lets(*e, counts))),
            ty,
        },
        Expr::Match { subject, arms, ty } => Expr::Match {
            subject: Box::new(remove_dead_lets(*subject, counts)),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern,
                    body: remove_dead_lets(arm.body, counts),
                })
                .collect(),
            ty,
        },
        Expr::CallDirect { name, args, ty } => Expr::CallDirect {
            name,
            args: args
                .into_iter()
                .map(|a| remove_dead_lets(a, counts))
                .collect(),
            ty,
        },
        Expr::CallNative {
            module,
            method,
            args,
            ty,
        } => Expr::CallNative {
            module,
            method,
            args: args
                .into_iter()
                .map(|a| remove_dead_lets(a, counts))
                .collect(),
            ty,
        },
        Expr::CallIndirect { callee, args, ty } => Expr::CallIndirect {
            callee: Box::new(remove_dead_lets(*callee, counts)),
            args: args
                .into_iter()
                .map(|a| remove_dead_lets(a, counts))
                .collect(),
            ty,
        },
        Expr::TailCall { name, args, ty } => Expr::TailCall {
            name,
            args: args
                .into_iter()
                .map(|a| remove_dead_lets(a, counts))
                .collect(),
            ty,
        },
        // Everything else: return as-is (literals, Var, etc.)
        other => other,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::ir::{IrFunction, Span};

    fn dummy_span() -> Span {
        Span {
            line: 1,
            col: 0,
            start_byte: 0,
            end_byte: 0,
        }
    }

    fn make_module(body: Expr) -> IrModule {
        IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body,
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
        }
    }

    #[test]
    fn constant_propagation_simple() {
        // let x = 10; x + 5 → let x = 10; 15
        let body = Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("x".into())),
                    value: Box::new(Expr::Int(10)),
                    ty: None,
                },
                Expr::BinOp {
                    op: BinOp::Add,
                    lhs: Box::new(Expr::Var("x".into(), None)),
                    rhs: Box::new(Expr::Int(5)),
                    ty: None,
                },
            ],
            None,
        );

        let mut module = make_module(body);
        optimize(&mut module);

        // The second expression should now be Int(15)
        if let Expr::Block(exprs, _) = &module.functions[0].body {
            assert!(matches!(exprs[1], Expr::Int(15)));
        } else {
            panic!("Expected Block");
        }
    }

    #[test]
    fn constant_folding_arithmetic() {
        // 3 + 4 → 7
        let body = Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(Expr::Int(3)),
            rhs: Box::new(Expr::Int(4)),
            ty: None,
        };

        let mut module = make_module(body);
        optimize(&mut module);
        assert!(matches!(module.functions[0].body, Expr::Int(7)));
    }

    #[test]
    fn constant_folding_comparison() {
        // 5 < 3 → false
        let body = Expr::BinOp {
            op: BinOp::Lt,
            lhs: Box::new(Expr::Int(5)),
            rhs: Box::new(Expr::Int(3)),
            ty: None,
        };

        let mut module = make_module(body);
        optimize(&mut module);
        assert!(matches!(module.functions[0].body, Expr::Bool(false)));
    }

    #[test]
    fn constant_folding_negation() {
        // -42 → Int(-42)
        let body = Expr::UnaryOp {
            op: UnaryOp::Neg,
            operand: Box::new(Expr::Int(42)),
            ty: None,
        };

        let mut module = make_module(body);
        optimize(&mut module);
        assert!(matches!(module.functions[0].body, Expr::Int(-42)));
    }

    #[test]
    fn dead_let_elimination() {
        // let x = 10; 42 → Unit; 42  (x is never used)
        let body = Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("x".into())),
                    value: Box::new(Expr::Int(10)),
                    ty: None,
                },
                Expr::Int(42),
            ],
            None,
        );

        let mut module = make_module(body);
        optimize(&mut module);

        if let Expr::Block(exprs, _) = &module.functions[0].body {
            // Dead let should be replaced with Unit
            assert!(matches!(exprs[0], Expr::Unit));
            assert!(matches!(exprs[1], Expr::Int(42)));
        } else {
            panic!("Expected Block");
        }
    }

    #[test]
    fn propagation_does_not_cross_scopes() {
        // let x = 10; { let x = 20; x } + x
        // Inner x should resolve to 20, outer x should resolve to 10
        let body = Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("x".into())),
                    value: Box::new(Expr::Int(10)),
                    ty: None,
                },
                Expr::BinOp {
                    op: BinOp::Add,
                    lhs: Box::new(Expr::Block(
                        vec![
                            Expr::Let {
                                pattern: Box::new(Pattern::Var("x".into())),
                                value: Box::new(Expr::Int(20)),
                                ty: None,
                            },
                            Expr::Var("x".into(), None),
                        ],
                        None,
                    )),
                    rhs: Box::new(Expr::Var("x".into(), None)),
                    ty: None,
                },
            ],
            None,
        );

        let mut module = make_module(body);
        optimize(&mut module);

        // The rhs Var("x") should be propagated to Int(10) (outer scope)
        // The inner block's Var("x") should be propagated to Int(20) (inner scope)
        if let Expr::Block(exprs, _) = &module.functions[0].body {
            if let Expr::BinOp { rhs, lhs, .. } = &exprs[1] {
                // rhs should be outer x = 10
                assert!(matches!(**rhs, Expr::Int(10)));
                // lhs is a Block; its last expr should be inner x = 20
                if let Expr::Block(inner, _) = &**lhs {
                    assert!(matches!(inner[1], Expr::Int(20)));
                } else {
                    panic!("Expected inner Block");
                }
            } else {
                panic!("Expected BinOp");
            }
        } else {
            panic!("Expected Block");
        }
    }

    #[test]
    fn no_propagation_of_non_literals() {
        // let x = some_call(); x + 1  — should NOT propagate
        let body = Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("x".into())),
                    value: Box::new(Expr::CallDirect {
                        name: "some_call".into(),
                        args: vec![],
                        ty: None,
                    }),
                    ty: None,
                },
                Expr::BinOp {
                    op: BinOp::Add,
                    lhs: Box::new(Expr::Var("x".into(), None)),
                    rhs: Box::new(Expr::Int(1)),
                    ty: None,
                },
            ],
            None,
        );

        let mut module = make_module(body);
        optimize(&mut module);

        // x + 1 should remain (not folded)
        if let Expr::Block(exprs, _) = &module.functions[0].body {
            assert!(matches!(exprs[1], Expr::BinOp { .. }));
        } else {
            panic!("Expected Block");
        }
    }

    #[test]
    fn division_by_zero_not_folded() {
        // 10 / 0 should NOT be folded (leave for runtime error)
        let body = Expr::BinOp {
            op: BinOp::Div,
            lhs: Box::new(Expr::Int(10)),
            rhs: Box::new(Expr::Int(0)),
            ty: None,
        };

        let mut module = make_module(body);
        optimize(&mut module);
        assert!(matches!(module.functions[0].body, Expr::BinOp { .. }));
    }

    #[test]
    fn chained_propagation() {
        // let x = 5; let y = x; y + 1 → 6
        let body = Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("x".into())),
                    value: Box::new(Expr::Int(5)),
                    ty: None,
                },
                Expr::Let {
                    pattern: Box::new(Pattern::Var("y".into())),
                    value: Box::new(Expr::Var("x".into(), None)),
                    ty: None,
                },
                Expr::BinOp {
                    op: BinOp::Add,
                    lhs: Box::new(Expr::Var("y".into(), None)),
                    rhs: Box::new(Expr::Int(1)),
                    ty: None,
                },
            ],
            None,
        );

        let mut module = make_module(body);
        optimize(&mut module);

        if let Expr::Block(exprs, _) = &module.functions[0].body {
            assert!(matches!(exprs[2], Expr::Int(6)));
        } else {
            panic!("Expected Block");
        }
    }

    #[test]
    fn float_constant_folding() {
        // 1.5 + 2.5 → 4.0
        let body = Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(Expr::Float(1.5)),
            rhs: Box::new(Expr::Float(2.5)),
            ty: None,
        };

        let mut module = make_module(body);
        optimize(&mut module);
        if let Expr::Float(f) = module.functions[0].body {
            assert!((f - 4.0).abs() < f64::EPSILON);
        } else {
            panic!("Expected Float");
        }
    }

    #[test]
    fn not_folding() {
        // not true → false
        let body = Expr::UnaryOp {
            op: UnaryOp::Not,
            operand: Box::new(Expr::Bool(true)),
            ty: None,
        };

        let mut module = make_module(body);
        optimize(&mut module);
        assert!(matches!(module.functions[0].body, Expr::Bool(false)));
    }
}
