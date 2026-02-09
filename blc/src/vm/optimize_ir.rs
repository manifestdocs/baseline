use std::collections::HashMap;

use super::ir::{BinOp, Expr, HandlerClause, IrModule, MatchArm, Pattern, UnaryOp};

/// Run all IR optimization passes on the module.
pub fn optimize(module: &mut IrModule) {
    // Pass 1-2: Constant propagation + folding
    for func in &mut module.functions {
        func.body = propagate_and_fold(func.body.clone());
    }
    // Pass 3: Function inlining (with interleaved propagation)
    inline_functions(module);
    // Cleanup: dead let elimination + block simplification
    for func in &mut module.functions {
        func.body = eliminate_dead_lets(func.body.clone());
        func.body = simplify_blocks(func.body.clone());
    }
    // Final: propagation + folding again (to fold across simplified blocks)
    for func in &mut module.functions {
        func.body = propagate_and_fold(func.body.clone());
        func.body = eliminate_dead_lets(func.body.clone());
        func.body = simplify_blocks(func.body.clone());
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

        Expr::WithHandlers { handlers, body } => Expr::WithHandlers {
            handlers: handlers.into_iter().map(|(name, methods)| {
                (name, methods.into_iter().map(|(mk, h)| (mk, propagate_expr(h, env))).collect())
            }).collect(),
            body: Box::new(propagate_expr(*body, env)),
        },

        Expr::HandleEffect { body, clauses } => Expr::HandleEffect {
            body: Box::new(propagate_expr(*body, env)),
            clauses: clauses.into_iter().map(|c| HandlerClause {
                body: propagate_expr(c.body, env),
                ..c
            }).collect(),
        },

        Expr::PerformEffect { effect, method, args, ty } => Expr::PerformEffect {
            effect,
            method,
            args: args.into_iter().map(|a| propagate_expr(a, env)).collect(),
            ty,
        },

        // Literals pass through unchanged
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole => expr,
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
        BinOp::ListConcat => None,
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
        BinOp::ListConcat => None,
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
// Pass 3: Function inlining
// ---------------------------------------------------------------------------

/// Maximum node count for a function body to be considered for inlining.
const INLINE_THRESHOLD: usize = 30;

/// Maximum number of inlining iterations to handle chains (f calls g calls h).
const INLINE_MAX_ROUNDS: usize = 3;

/// Count the number of AST nodes in an expression (for inlining budget).
fn expr_node_count(expr: &Expr) -> usize {
    match expr {
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole
        | Expr::Var(_, _) => 1,
        Expr::BinOp { lhs, rhs, .. } => 1 + expr_node_count(lhs) + expr_node_count(rhs),
        Expr::UnaryOp { operand, .. } => 1 + expr_node_count(operand),
        Expr::And(l, r) | Expr::Or(l, r) => 1 + expr_node_count(l) + expr_node_count(r),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            1 + expr_node_count(condition)
                + expr_node_count(then_branch)
                + else_branch.as_ref().map_or(0, |e| expr_node_count(e))
        }
        Expr::CallDirect { args, .. }
        | Expr::CallNative { args, .. }
        | Expr::TailCall { args, .. } => {
            1 + args.iter().map(expr_node_count).sum::<usize>()
        }
        Expr::CallIndirect { callee, args, .. } => {
            1 + expr_node_count(callee) + args.iter().map(expr_node_count).sum::<usize>()
        }
        Expr::Let { value, .. } => 1 + expr_node_count(value),
        Expr::Block(exprs, _) => exprs.iter().map(expr_node_count).sum::<usize>(),
        Expr::Match { subject, arms, .. } => {
            1 + expr_node_count(subject)
                + arms.iter().map(|a| expr_node_count(&a.body)).sum::<usize>()
        }
        Expr::Lambda { body, .. } => 1 + expr_node_count(body),
        Expr::MakeEnum { payload, .. } => 1 + expr_node_count(payload),
        Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => {
            1 + fields.iter().map(|(_, v)| expr_node_count(v)).sum::<usize>()
        }
        Expr::MakeList(items, _) | Expr::MakeTuple(items, _) => {
            items.iter().map(expr_node_count).sum::<usize>()
        }
        Expr::MakeRange(s, e) => 1 + expr_node_count(s) + expr_node_count(e),
        Expr::UpdateRecord { base, updates, .. } => {
            1 + expr_node_count(base)
                + updates.iter().map(|(_, v)| expr_node_count(v)).sum::<usize>()
        }
        Expr::GetField { object, .. } => 1 + expr_node_count(object),
        Expr::For { iterable, body, .. } => 1 + expr_node_count(iterable) + expr_node_count(body),
        Expr::Try { expr, .. } => 1 + expr_node_count(expr),
        Expr::Concat(parts) => parts.iter().map(expr_node_count).sum::<usize>(),
        Expr::WithHandlers { handlers, body, .. } => {
            1 + handlers.iter().map(|(_, methods)| methods.iter().map(|(_, h)| expr_node_count(h)).sum::<usize>()).sum::<usize>()
                + expr_node_count(body)
        }
        Expr::HandleEffect { body, clauses, .. } => {
            1 + expr_node_count(body)
                + clauses.iter().map(|c| expr_node_count(&c.body)).sum::<usize>()
        }
        Expr::PerformEffect { args, .. } => {
            1 + args.iter().map(expr_node_count).sum::<usize>()
        }
    }
}

/// Check if an expression contains a call (direct or tail) to the named function.
fn references_function(expr: &Expr, name: &str) -> bool {
    match expr {
        Expr::CallDirect {
            name: n, args, ..
        }
        | Expr::TailCall {
            name: n, args, ..
        } => n == name || args.iter().any(|a| references_function(a, name)),
        Expr::BinOp { lhs, rhs, .. } => {
            references_function(lhs, name) || references_function(rhs, name)
        }
        Expr::UnaryOp { operand, .. } => references_function(operand, name),
        Expr::And(l, r) | Expr::Or(l, r) => {
            references_function(l, name) || references_function(r, name)
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            references_function(condition, name)
                || references_function(then_branch, name)
                || else_branch
                    .as_ref()
                    .is_some_and(|e| references_function(e, name))
        }
        Expr::Let { value, .. } => references_function(value, name),
        Expr::Block(exprs, _) => exprs.iter().any(|e| references_function(e, name)),
        Expr::Match { subject, arms, .. } => {
            references_function(subject, name)
                || arms.iter().any(|a| references_function(&a.body, name))
        }
        Expr::CallNative { args, .. } => args.iter().any(|a| references_function(a, name)),
        Expr::CallIndirect { callee, args, .. } => {
            references_function(callee, name)
                || args.iter().any(|a| references_function(a, name))
        }
        Expr::Lambda { body, .. } => references_function(body, name),
        Expr::MakeEnum { payload, .. } => references_function(payload, name),
        Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => {
            fields.iter().any(|(_, v)| references_function(v, name))
        }
        Expr::MakeList(items, _) | Expr::MakeTuple(items, _) => {
            items.iter().any(|e| references_function(e, name))
        }
        Expr::MakeRange(s, e) => references_function(s, name) || references_function(e, name),
        Expr::UpdateRecord { base, updates, .. } => {
            references_function(base, name)
                || updates.iter().any(|(_, v)| references_function(v, name))
        }
        Expr::GetField { object, .. } => references_function(object, name),
        Expr::For { iterable, body, .. } => {
            references_function(iterable, name) || references_function(body, name)
        }
        Expr::Try { expr, .. } => references_function(expr, name),
        Expr::Concat(parts) => parts.iter().any(|e| references_function(e, name)),
        Expr::WithHandlers { handlers, body, .. } => {
            handlers.iter().any(|(_, methods)| methods.iter().any(|(_, h)| references_function(h, name)))
                || references_function(body, name)
        }
        Expr::HandleEffect { body, clauses, .. } => {
            references_function(body, name)
                || clauses.iter().any(|c| references_function(&c.body, name))
        }
        Expr::PerformEffect { args, .. } => {
            args.iter().any(|a| references_function(a, name))
        }
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole
        | Expr::Var(_, _) => false,
    }
}

/// Rename all occurrences of variables in `rename_map` within an expression.
fn rename_vars(expr: Expr, rename_map: &HashMap<String, String>) -> Expr {
    match expr {
        Expr::Var(name, ty) => {
            if let Some(new_name) = rename_map.get(&name) {
                Expr::Var(new_name.clone(), ty)
            } else {
                Expr::Var(name, ty)
            }
        }
        Expr::BinOp { op, lhs, rhs, ty } => Expr::BinOp {
            op,
            lhs: Box::new(rename_vars(*lhs, rename_map)),
            rhs: Box::new(rename_vars(*rhs, rename_map)),
            ty,
        },
        Expr::UnaryOp { op, operand, ty } => Expr::UnaryOp {
            op,
            operand: Box::new(rename_vars(*operand, rename_map)),
            ty,
        },
        Expr::And(l, r) => Expr::And(
            Box::new(rename_vars(*l, rename_map)),
            Box::new(rename_vars(*r, rename_map)),
        ),
        Expr::Or(l, r) => Expr::Or(
            Box::new(rename_vars(*l, rename_map)),
            Box::new(rename_vars(*r, rename_map)),
        ),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ty,
        } => Expr::If {
            condition: Box::new(rename_vars(*condition, rename_map)),
            then_branch: Box::new(rename_vars(*then_branch, rename_map)),
            else_branch: else_branch.map(|e| Box::new(rename_vars(*e, rename_map))),
            ty,
        },
        Expr::CallDirect { name, args, ty } => Expr::CallDirect {
            name,
            args: args
                .into_iter()
                .map(|a| rename_vars(a, rename_map))
                .collect(),
            ty,
        },
        Expr::TailCall { name, args, ty } => Expr::TailCall {
            name,
            args: args
                .into_iter()
                .map(|a| rename_vars(a, rename_map))
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
                .map(|a| rename_vars(a, rename_map))
                .collect(),
            ty,
        },
        Expr::CallIndirect { callee, args, ty } => Expr::CallIndirect {
            callee: Box::new(rename_vars(*callee, rename_map)),
            args: args
                .into_iter()
                .map(|a| rename_vars(a, rename_map))
                .collect(),
            ty,
        },
        Expr::Let { pattern, value, ty } => {
            let new_pattern = rename_pattern(*pattern, rename_map);
            Expr::Let {
                pattern: Box::new(new_pattern),
                value: Box::new(rename_vars(*value, rename_map)),
                ty,
            }
        }
        Expr::Block(exprs, ty) => Expr::Block(
            exprs
                .into_iter()
                .map(|e| rename_vars(e, rename_map))
                .collect(),
            ty,
        ),
        Expr::Match { subject, arms, ty } => Expr::Match {
            subject: Box::new(rename_vars(*subject, rename_map)),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern,
                    body: rename_vars(arm.body, rename_map),
                })
                .collect(),
            ty,
        },
        Expr::Lambda { params, body, ty } => {
            // Lambda params shadow — don't rename inside if shadowed
            let mut inner_map = rename_map.clone();
            for p in &params {
                inner_map.remove(p);
            }
            Expr::Lambda {
                params,
                body: Box::new(rename_vars(*body, &inner_map)),
                ty,
            }
        }
        Expr::MakeEnum { tag, payload, ty } => Expr::MakeEnum {
            tag,
            payload: Box::new(rename_vars(*payload, rename_map)),
            ty,
        },
        Expr::MakeStruct { name, fields, ty } => Expr::MakeStruct {
            name,
            fields: fields
                .into_iter()
                .map(|(k, v)| (k, rename_vars(v, rename_map)))
                .collect(),
            ty,
        },
        Expr::MakeList(items, ty) => Expr::MakeList(
            items
                .into_iter()
                .map(|e| rename_vars(e, rename_map))
                .collect(),
            ty,
        ),
        Expr::MakeRecord(fields, ty) => Expr::MakeRecord(
            fields
                .into_iter()
                .map(|(k, v)| (k, rename_vars(v, rename_map)))
                .collect(),
            ty,
        ),
        Expr::MakeTuple(items, ty) => Expr::MakeTuple(
            items
                .into_iter()
                .map(|e| rename_vars(e, rename_map))
                .collect(),
            ty,
        ),
        Expr::MakeRange(s, e) => Expr::MakeRange(
            Box::new(rename_vars(*s, rename_map)),
            Box::new(rename_vars(*e, rename_map)),
        ),
        Expr::UpdateRecord { base, updates, ty } => Expr::UpdateRecord {
            base: Box::new(rename_vars(*base, rename_map)),
            updates: updates
                .into_iter()
                .map(|(k, v)| (k, rename_vars(v, rename_map)))
                .collect(),
            ty,
        },
        Expr::GetField { object, field, ty } => Expr::GetField {
            object: Box::new(rename_vars(*object, rename_map)),
            field,
            ty,
        },
        Expr::For {
            binding,
            iterable,
            body,
        } => {
            let mut inner_map = rename_map.clone();
            inner_map.remove(&binding);
            Expr::For {
                binding,
                iterable: Box::new(rename_vars(*iterable, rename_map)),
                body: Box::new(rename_vars(*body, &inner_map)),
            }
        }
        Expr::Try { expr, ty } => Expr::Try {
            expr: Box::new(rename_vars(*expr, rename_map)),
            ty,
        },
        Expr::Concat(parts) => Expr::Concat(
            parts
                .into_iter()
                .map(|e| rename_vars(e, rename_map))
                .collect(),
        ),
        Expr::WithHandlers { handlers, body } => Expr::WithHandlers {
            handlers: handlers.into_iter().map(|(name, methods)| {
                (name, methods.into_iter().map(|(mk, h)| (mk, rename_vars(h, rename_map))).collect())
            }).collect(),
            body: Box::new(rename_vars(*body, rename_map)),
        },
        Expr::HandleEffect { body, clauses } => Expr::HandleEffect {
            body: Box::new(rename_vars(*body, rename_map)),
            clauses: clauses.into_iter().map(|c| HandlerClause {
                body: rename_vars(c.body, rename_map),
                ..c
            }).collect(),
        },
        Expr::PerformEffect { effect, method, args, ty } => Expr::PerformEffect {
            effect,
            method,
            args: args.into_iter().map(|a| rename_vars(a, rename_map)).collect(),
            ty,
        },
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole => expr,
    }
}

fn rename_pattern(pattern: Pattern, rename_map: &HashMap<String, String>) -> Pattern {
    match pattern {
        Pattern::Var(name) => {
            if let Some(new_name) = rename_map.get(&name) {
                Pattern::Var(new_name.clone())
            } else {
                Pattern::Var(name)
            }
        }
        other => other,
    }
}

/// Inline eligible function calls within the module.
/// Runs multiple rounds to handle chains (f→g→h).
fn inline_functions(module: &mut IrModule) {
    // Build a map of inlineable functions: name → (params, body)
    // A function is inlineable if:
    //   1. Body size <= INLINE_THRESHOLD
    //   2. Not self-recursive (no CallDirect/TailCall to self)
    let inlineable: HashMap<String, (Vec<String>, Expr)> = module
        .functions
        .iter()
        .filter(|f| {
            let size = expr_node_count(&f.body);
            size <= INLINE_THRESHOLD && !references_function(&f.body, &f.name)
        })
        .map(|f| (f.name.clone(), (f.params.clone(), f.body.clone())))
        .collect();

    if inlineable.is_empty() {
        return;
    }

    let mut counter: usize = 0;

    for round in 0..INLINE_MAX_ROUNDS {
        let mut changed = false;
        for func in &mut module.functions {
            let new_body = inline_expr(func.body.clone(), &inlineable, &mut counter);
            if !exprs_equal(&func.body, &new_body) {
                func.body = new_body;
                changed = true;
            }
        }
        if !changed {
            break;
        }
        // After inlining, re-run propagation + folding to simplify
        if round < INLINE_MAX_ROUNDS - 1 {
            for func in &mut module.functions {
                func.body = propagate_and_fold(func.body.clone());
            }
        }
    }
}

/// Inline eligible calls within an expression.
fn inline_expr(
    expr: Expr,
    inlineable: &HashMap<String, (Vec<String>, Expr)>,
    counter: &mut usize,
) -> Expr {
    match expr {
        Expr::CallDirect { name, args, ty } => {
            let args: Vec<Expr> = args
                .into_iter()
                .map(|a| inline_expr(a, inlineable, counter))
                .collect();

            if let Some((params, body)) = inlineable.get(&name)
                && params.len() == args.len()
            {
                // Build let bindings with fresh names
                let mut stmts = Vec::new();
                let mut rename_map = HashMap::new();
                for (param, arg) in params.iter().zip(args) {
                    let fresh = format!("__inl_{}_{}", param, *counter);
                    *counter += 1;
                    rename_map.insert(param.clone(), fresh.clone());
                    stmts.push(Expr::Let {
                        pattern: Box::new(Pattern::Var(fresh)),
                        value: Box::new(arg),
                        ty: None,
                    });
                }
                let inlined_body = rename_vars(body.clone(), &rename_map);
                stmts.push(inlined_body);
                return Expr::Block(stmts, ty);
            }
            Expr::CallDirect { name, args, ty }
        }
        // Recurse into subexpressions
        Expr::BinOp { op, lhs, rhs, ty } => Expr::BinOp {
            op,
            lhs: Box::new(inline_expr(*lhs, inlineable, counter)),
            rhs: Box::new(inline_expr(*rhs, inlineable, counter)),
            ty,
        },
        Expr::UnaryOp { op, operand, ty } => Expr::UnaryOp {
            op,
            operand: Box::new(inline_expr(*operand, inlineable, counter)),
            ty,
        },
        Expr::And(l, r) => Expr::And(
            Box::new(inline_expr(*l, inlineable, counter)),
            Box::new(inline_expr(*r, inlineable, counter)),
        ),
        Expr::Or(l, r) => Expr::Or(
            Box::new(inline_expr(*l, inlineable, counter)),
            Box::new(inline_expr(*r, inlineable, counter)),
        ),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ty,
        } => Expr::If {
            condition: Box::new(inline_expr(*condition, inlineable, counter)),
            then_branch: Box::new(inline_expr(*then_branch, inlineable, counter)),
            else_branch: else_branch.map(|e| Box::new(inline_expr(*e, inlineable, counter))),
            ty,
        },
        Expr::Let { pattern, value, ty } => Expr::Let {
            pattern,
            value: Box::new(inline_expr(*value, inlineable, counter)),
            ty,
        },
        Expr::Block(exprs, ty) => Expr::Block(
            exprs
                .into_iter()
                .map(|e| inline_expr(e, inlineable, counter))
                .collect(),
            ty,
        ),
        Expr::Match { subject, arms, ty } => Expr::Match {
            subject: Box::new(inline_expr(*subject, inlineable, counter)),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern,
                    body: inline_expr(arm.body, inlineable, counter),
                })
                .collect(),
            ty,
        },
        Expr::TailCall { name, args, ty } => Expr::TailCall {
            name,
            args: args
                .into_iter()
                .map(|a| inline_expr(a, inlineable, counter))
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
                .map(|a| inline_expr(a, inlineable, counter))
                .collect(),
            ty,
        },
        Expr::CallIndirect { callee, args, ty } => Expr::CallIndirect {
            callee: Box::new(inline_expr(*callee, inlineable, counter)),
            args: args
                .into_iter()
                .map(|a| inline_expr(a, inlineable, counter))
                .collect(),
            ty,
        },
        Expr::Lambda { params, body, ty } => Expr::Lambda {
            params,
            body: Box::new(inline_expr(*body, inlineable, counter)),
            ty,
        },
        Expr::MakeEnum { tag, payload, ty } => Expr::MakeEnum {
            tag,
            payload: Box::new(inline_expr(*payload, inlineable, counter)),
            ty,
        },
        Expr::MakeStruct { name, fields, ty } => Expr::MakeStruct {
            name,
            fields: fields
                .into_iter()
                .map(|(k, v)| (k, inline_expr(v, inlineable, counter)))
                .collect(),
            ty,
        },
        Expr::MakeList(items, ty) => Expr::MakeList(
            items
                .into_iter()
                .map(|e| inline_expr(e, inlineable, counter))
                .collect(),
            ty,
        ),
        Expr::MakeRecord(fields, ty) => Expr::MakeRecord(
            fields
                .into_iter()
                .map(|(k, v)| (k, inline_expr(v, inlineable, counter)))
                .collect(),
            ty,
        ),
        Expr::MakeTuple(items, ty) => Expr::MakeTuple(
            items
                .into_iter()
                .map(|e| inline_expr(e, inlineable, counter))
                .collect(),
            ty,
        ),
        Expr::MakeRange(s, e) => Expr::MakeRange(
            Box::new(inline_expr(*s, inlineable, counter)),
            Box::new(inline_expr(*e, inlineable, counter)),
        ),
        Expr::UpdateRecord { base, updates, ty } => Expr::UpdateRecord {
            base: Box::new(inline_expr(*base, inlineable, counter)),
            updates: updates
                .into_iter()
                .map(|(k, v)| (k, inline_expr(v, inlineable, counter)))
                .collect(),
            ty,
        },
        Expr::GetField { object, field, ty } => Expr::GetField {
            object: Box::new(inline_expr(*object, inlineable, counter)),
            field,
            ty,
        },
        Expr::For {
            binding,
            iterable,
            body,
        } => Expr::For {
            binding,
            iterable: Box::new(inline_expr(*iterable, inlineable, counter)),
            body: Box::new(inline_expr(*body, inlineable, counter)),
        },
        Expr::Try { expr, ty } => Expr::Try {
            expr: Box::new(inline_expr(*expr, inlineable, counter)),
            ty,
        },
        Expr::Concat(parts) => Expr::Concat(
            parts
                .into_iter()
                .map(|e| inline_expr(e, inlineable, counter))
                .collect(),
        ),
        Expr::WithHandlers { handlers, body } => Expr::WithHandlers {
            handlers: handlers.into_iter().map(|(name, methods)| {
                (name, methods.into_iter().map(|(mk, h)| (mk, inline_expr(h, inlineable, counter))).collect())
            }).collect(),
            body: Box::new(inline_expr(*body, inlineable, counter)),
        },
        Expr::HandleEffect { body, clauses } => Expr::HandleEffect {
            body: Box::new(inline_expr(*body, inlineable, counter)),
            clauses: clauses.into_iter().map(|c| HandlerClause {
                body: inline_expr(c.body, inlineable, counter),
                ..c
            }).collect(),
        },
        Expr::PerformEffect { effect, method, args, ty } => Expr::PerformEffect {
            effect,
            method,
            args: args.into_iter().map(|a| inline_expr(a, inlineable, counter)).collect(),
            ty,
        },
        // Literals pass through
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole
        | Expr::Var(_, _) => expr,
    }
}

/// Shallow structural equality check (for detecting if inlining changed anything).
fn exprs_equal(a: &Expr, b: &Expr) -> bool {
    // Use debug representation for a quick check
    format!("{:?}", a) == format!("{:?}", b)
}

// ---------------------------------------------------------------------------
// Pass 4: Block simplification
// ---------------------------------------------------------------------------

/// Simplify blocks: remove non-final Unit expressions, flatten nested blocks,
/// unwrap single-element blocks.
fn simplify_blocks(expr: Expr) -> Expr {
    match expr {
        Expr::Block(exprs, ty) => {
            // Recursively simplify children first
            let mut simplified: Vec<Expr> = Vec::new();
            for e in exprs {
                let e = simplify_blocks(e);
                // Flatten nested blocks
                if let Expr::Block(inner, _) = e {
                    simplified.extend(inner);
                } else {
                    simplified.push(e);
                }
            }
            // Remove non-final Unit expressions
            if simplified.len() > 1 {
                let last = simplified.pop().unwrap();
                simplified.retain(|e| !matches!(e, Expr::Unit));
                simplified.push(last);
            }
            // Unwrap single-element blocks
            if simplified.len() == 1 {
                return simplified.into_iter().next().unwrap();
            }
            Expr::Block(simplified, ty)
        }
        // Recurse into all other expression types
        Expr::BinOp { op, lhs, rhs, ty } => Expr::BinOp {
            op,
            lhs: Box::new(simplify_blocks(*lhs)),
            rhs: Box::new(simplify_blocks(*rhs)),
            ty,
        },
        Expr::UnaryOp { op, operand, ty } => Expr::UnaryOp {
            op,
            operand: Box::new(simplify_blocks(*operand)),
            ty,
        },
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ty,
        } => Expr::If {
            condition: Box::new(simplify_blocks(*condition)),
            then_branch: Box::new(simplify_blocks(*then_branch)),
            else_branch: else_branch.map(|e| Box::new(simplify_blocks(*e))),
            ty,
        },
        Expr::Let { pattern, value, ty } => Expr::Let {
            pattern,
            value: Box::new(simplify_blocks(*value)),
            ty,
        },
        Expr::CallDirect { name, args, ty } => Expr::CallDirect {
            name,
            args: args.into_iter().map(simplify_blocks).collect(),
            ty,
        },
        Expr::TailCall { name, args, ty } => Expr::TailCall {
            name,
            args: args.into_iter().map(simplify_blocks).collect(),
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
            args: args.into_iter().map(simplify_blocks).collect(),
            ty,
        },
        Expr::CallIndirect { callee, args, ty } => Expr::CallIndirect {
            callee: Box::new(simplify_blocks(*callee)),
            args: args.into_iter().map(simplify_blocks).collect(),
            ty,
        },
        Expr::And(l, r) => Expr::And(
            Box::new(simplify_blocks(*l)),
            Box::new(simplify_blocks(*r)),
        ),
        Expr::Or(l, r) => Expr::Or(
            Box::new(simplify_blocks(*l)),
            Box::new(simplify_blocks(*r)),
        ),
        Expr::Match { subject, arms, ty } => Expr::Match {
            subject: Box::new(simplify_blocks(*subject)),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern,
                    body: simplify_blocks(arm.body),
                })
                .collect(),
            ty,
        },
        Expr::Lambda { params, body, ty } => Expr::Lambda {
            params,
            body: Box::new(simplify_blocks(*body)),
            ty,
        },
        Expr::MakeEnum { tag, payload, ty } => Expr::MakeEnum {
            tag,
            payload: Box::new(simplify_blocks(*payload)),
            ty,
        },
        Expr::MakeStruct { name, fields, ty } => Expr::MakeStruct {
            name,
            fields: fields
                .into_iter()
                .map(|(k, v)| (k, simplify_blocks(v)))
                .collect(),
            ty,
        },
        Expr::MakeList(items, ty) => {
            Expr::MakeList(items.into_iter().map(simplify_blocks).collect(), ty)
        }
        Expr::MakeRecord(fields, ty) => Expr::MakeRecord(
            fields
                .into_iter()
                .map(|(k, v)| (k, simplify_blocks(v)))
                .collect(),
            ty,
        ),
        Expr::MakeTuple(items, ty) => {
            Expr::MakeTuple(items.into_iter().map(simplify_blocks).collect(), ty)
        }
        Expr::MakeRange(s, e) => Expr::MakeRange(
            Box::new(simplify_blocks(*s)),
            Box::new(simplify_blocks(*e)),
        ),
        Expr::UpdateRecord { base, updates, ty } => Expr::UpdateRecord {
            base: Box::new(simplify_blocks(*base)),
            updates: updates
                .into_iter()
                .map(|(k, v)| (k, simplify_blocks(v)))
                .collect(),
            ty,
        },
        Expr::GetField { object, field, ty } => Expr::GetField {
            object: Box::new(simplify_blocks(*object)),
            field,
            ty,
        },
        Expr::For {
            binding,
            iterable,
            body,
        } => Expr::For {
            binding,
            iterable: Box::new(simplify_blocks(*iterable)),
            body: Box::new(simplify_blocks(*body)),
        },
        Expr::Try { expr, ty } => Expr::Try {
            expr: Box::new(simplify_blocks(*expr)),
            ty,
        },
        Expr::Concat(parts) => {
            Expr::Concat(parts.into_iter().map(simplify_blocks).collect())
        }
        Expr::WithHandlers { handlers, body } => Expr::WithHandlers {
            handlers: handlers.into_iter().map(|(name, methods)| {
                (name, methods.into_iter().map(|(mk, h)| (mk, simplify_blocks(h))).collect())
            }).collect(),
            body: Box::new(simplify_blocks(*body)),
        },
        Expr::HandleEffect { body, clauses } => Expr::HandleEffect {
            body: Box::new(simplify_blocks(*body)),
            clauses: clauses.into_iter().map(|c| HandlerClause {
                body: simplify_blocks(c.body),
                ..c
            }).collect(),
        },
        Expr::PerformEffect { effect, method, args, ty } => Expr::PerformEffect {
            effect,
            method,
            args: args.into_iter().map(simplify_blocks).collect(),
            ty,
        },
        // Leaves pass through
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole
        | Expr::Var(_, _) => expr,
    }
}

// ---------------------------------------------------------------------------
// Pass 5: Dead let elimination
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
        Expr::WithHandlers { handlers, body, .. } => {
            for (_, methods) in handlers {
                for (_, h) in methods {
                    count_var_uses(h, counts);
                }
            }
            count_var_uses(body, counts);
        }
        Expr::HandleEffect { body, clauses, .. } => {
            count_var_uses(body, counts);
            for c in clauses {
                count_var_uses(&c.body, counts);
            }
        }
        Expr::PerformEffect { args, .. } => {
            for a in args {
                count_var_uses(a, counts);
            }
        }
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole => {}
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
        // let x = 10; x + 5 → 15 (after propagation + dead let + simplification)
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

        assert!(
            matches!(module.functions[0].body, Expr::Int(15)),
            "Expected Int(15), got: {:?}",
            module.functions[0].body
        );
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
        // let x = 10; 42 → 42 (x is never used, dead let removed, block simplified)
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

        assert!(
            matches!(module.functions[0].body, Expr::Int(42)),
            "Expected Int(42), got: {:?}",
            module.functions[0].body
        );
    }

    #[test]
    fn propagation_does_not_cross_scopes() {
        // let x = 10; { let x = 20; x } + x
        // Inner x=20, outer x=10 → 20 + 10 → 30
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

        // After propagation + folding + dead let + block simplification: Int(30)
        assert!(
            matches!(module.functions[0].body, Expr::Int(30)),
            "Expected Int(30), got: {:?}",
            module.functions[0].body
        );
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

        assert!(
            matches!(module.functions[0].body, Expr::Int(6)),
            "Expected Int(6), got: {:?}",
            module.functions[0].body
        );
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

    // -- Inlining tests --

    fn make_two_func_module(
        caller_body: Expr,
        callee_name: &str,
        callee_params: Vec<&str>,
        callee_body: Expr,
    ) -> IrModule {
        IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: caller_body,
                    ty: None,
                    span: dummy_span(),
                },
                IrFunction {
                    name: callee_name.into(),
                    params: callee_params.into_iter().map(String::from).collect(),
                    body: callee_body,
                    ty: None,
                    span: dummy_span(),
                },
            ],
            entry: 0,
        }
    }

    #[test]
    fn inline_simple_wrapper() {
        // callee: fn double(x) = x + x
        // caller: main() = double(5)
        // After inlining: main() = Block([let __x = 5, __x + __x]) → folded to 10
        let caller = Expr::CallDirect {
            name: "double".into(),
            args: vec![Expr::Int(5)],
            ty: None,
        };
        let callee_body = Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(Expr::Var("x".into(), None)),
            rhs: Box::new(Expr::Var("x".into(), None)),
            ty: None,
        };

        let mut module = make_two_func_module(caller, "double", vec!["x"], callee_body);
        optimize(&mut module);

        // After inlining + constant propagation, result should be Int(10)
        let body = &module.functions[0].body;
        assert!(
            matches!(body, Expr::Int(10)),
            "Expected Int(10) after inlining, got: {:?}",
            body
        );
    }

    #[test]
    fn inline_multi_param_function() {
        // callee: fn add(a, b) = a + b
        // caller: main() = add(3, 4)
        let caller = Expr::CallDirect {
            name: "add".into(),
            args: vec![Expr::Int(3), Expr::Int(4)],
            ty: None,
        };
        let callee_body = Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(Expr::Var("a".into(), None)),
            rhs: Box::new(Expr::Var("b".into(), None)),
            ty: None,
        };

        let mut module = make_two_func_module(caller, "add", vec!["a", "b"], callee_body);
        optimize(&mut module);

        assert!(
            matches!(module.functions[0].body, Expr::Int(7)),
            "Expected Int(7), got: {:?}",
            module.functions[0].body
        );
    }

    #[test]
    fn no_inline_recursive_function() {
        // callee: fn count(n) = if n <= 0 then 0 else count(n - 1)
        // Should NOT be inlined because it's recursive
        let caller = Expr::CallDirect {
            name: "count".into(),
            args: vec![Expr::Int(10)],
            ty: None,
        };
        let callee_body = Expr::If {
            condition: Box::new(Expr::BinOp {
                op: BinOp::Le,
                lhs: Box::new(Expr::Var("n".into(), None)),
                rhs: Box::new(Expr::Int(0)),
                ty: None,
            }),
            then_branch: Box::new(Expr::Int(0)),
            else_branch: Some(Box::new(Expr::CallDirect {
                name: "count".into(),
                args: vec![Expr::BinOp {
                    op: BinOp::Sub,
                    lhs: Box::new(Expr::Var("n".into(), None)),
                    rhs: Box::new(Expr::Int(1)),
                    ty: None,
                }],
                ty: None,
            })),
            ty: None,
        };

        let mut module = make_two_func_module(caller, "count", vec!["n"], callee_body);
        optimize(&mut module);

        // Should still be a CallDirect (not inlined)
        assert!(
            matches!(module.functions[0].body, Expr::CallDirect { .. }),
            "Recursive function should not be inlined, got: {:?}",
            module.functions[0].body
        );
    }

    #[test]
    fn no_inline_tail_recursive_function() {
        // callee: fn loop(n) = TailCall loop(n - 1)
        // Should NOT be inlined
        let caller = Expr::CallDirect {
            name: "loop_fn".into(),
            args: vec![Expr::Int(10)],
            ty: None,
        };
        let callee_body = Expr::TailCall {
            name: "loop_fn".into(),
            args: vec![Expr::BinOp {
                op: BinOp::Sub,
                lhs: Box::new(Expr::Var("n".into(), None)),
                rhs: Box::new(Expr::Int(1)),
                ty: None,
            }],
            ty: None,
        };

        let mut module = make_two_func_module(caller, "loop_fn", vec!["n"], callee_body);
        optimize(&mut module);

        assert!(
            matches!(module.functions[0].body, Expr::CallDirect { .. }),
            "Tail-recursive function should not be inlined"
        );
    }

    #[test]
    fn inline_preserves_variable_scoping() {
        // callee: fn inc(x) = x + 1
        // caller: main() = let x = 100; inc(x)
        // After inlining, the inlined 'x' should not shadow the outer 'x'
        let caller = Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("x".into())),
                    value: Box::new(Expr::Int(100)),
                    ty: None,
                },
                Expr::CallDirect {
                    name: "inc".into(),
                    args: vec![Expr::Var("x".into(), None)],
                    ty: None,
                },
            ],
            None,
        );
        let callee_body = Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(Expr::Var("x".into(), None)),
            rhs: Box::new(Expr::Int(1)),
            ty: None,
        };

        let mut module = make_two_func_module(caller, "inc", vec!["x"], callee_body);
        optimize(&mut module);

        // After inlining + propagation: should be Int(101)
        let body = &module.functions[0].body;
        match body {
            Expr::Block(exprs, _) => {
                let last = exprs.last().unwrap();
                assert!(
                    matches!(last, Expr::Int(101)),
                    "Expected Int(101), got: {:?}",
                    last
                );
            }
            Expr::Int(101) => {} // Also acceptable if fully simplified
            other => panic!("Expected Block or Int(101), got: {:?}", other),
        }
    }

    #[test]
    fn inline_chain() {
        // f(x) = x + 1, g(x) = f(x) + f(x)
        // main() = g(5) → should inline g, then inline f inside g → 12
        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "g".into(),
                        args: vec![Expr::Int(5)],
                        ty: None,
                    },
                    ty: None,
                    span: dummy_span(),
                },
                IrFunction {
                    name: "f".into(),
                    params: vec!["x".into()],
                    body: Expr::BinOp {
                        op: BinOp::Add,
                        lhs: Box::new(Expr::Var("x".into(), None)),
                        rhs: Box::new(Expr::Int(1)),
                        ty: None,
                    },
                    ty: None,
                    span: dummy_span(),
                },
                IrFunction {
                    name: "g".into(),
                    params: vec!["x".into()],
                    body: Expr::BinOp {
                        op: BinOp::Add,
                        lhs: Box::new(Expr::CallDirect {
                            name: "f".into(),
                            args: vec![Expr::Var("x".into(), None)],
                            ty: None,
                        }),
                        rhs: Box::new(Expr::CallDirect {
                            name: "f".into(),
                            args: vec![Expr::Var("x".into(), None)],
                            ty: None,
                        }),
                        ty: None,
                    },
                    ty: None,
                    span: dummy_span(),
                },
            ],
            entry: 0,
        };

        let mut module = module;
        optimize(&mut module);

        // g(5) → f(5) + f(5) → (5+1) + (5+1) → 12
        assert!(
            matches!(module.functions[0].body, Expr::Int(12)),
            "Expected Int(12) after chain inlining, got: {:?}",
            module.functions[0].body
        );
    }

    #[test]
    fn expr_size_counts_correctly() {
        assert_eq!(expr_node_count(&Expr::Int(1)), 1);
        assert_eq!(
            expr_node_count(&Expr::BinOp {
                op: BinOp::Add,
                lhs: Box::new(Expr::Int(1)),
                rhs: Box::new(Expr::Int(2)),
                ty: None,
            }),
            3
        );
    }

    #[test]
    fn is_recursive_detects_self_call() {
        let body = Expr::CallDirect {
            name: "f".into(),
            args: vec![],
            ty: None,
        };
        assert!(references_function(&body, "f"));
        assert!(!references_function(&body, "g"));
    }
}
