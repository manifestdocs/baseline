//! Static analysis for JIT compilation decisions.
//!
//! Determines which functions can be JIT-compiled, which can use unboxed
//! codegen, which are used as indirect call targets, etc.

use std::collections::HashMap;

use crate::analysis::types::Type;
use super::super::ir::{Expr, IrFunction, IrModule, MatchArm, Pattern};
use super::super::natives::NativeRegistry;

/// Check if a function can be JIT-compiled.
pub(super) fn can_jit(func: &IrFunction, natives: Option<&NativeRegistry>) -> bool {
    expr_can_jit(&func.body, natives)
}

/// Check if an expression contains a self-tail-call to the given function name.
pub(super) fn has_self_tail_call(expr: &Expr, name: &str) -> bool {
    match expr {
        Expr::TailCall {
            name: call_name, ..
        } => call_name == name,
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            has_self_tail_call(condition, name)
                || has_self_tail_call(then_branch, name)
                || else_branch
                    .as_ref()
                    .is_some_and(|e| has_self_tail_call(e, name))
        }
        Expr::Block(exprs, _) => exprs.iter().any(|e| has_self_tail_call(e, name)),
        Expr::Let { value, .. } => has_self_tail_call(value, name),
        Expr::BinOp { lhs, rhs, .. } => {
            has_self_tail_call(lhs, name) || has_self_tail_call(rhs, name)
        }
        Expr::UnaryOp { operand, .. } => has_self_tail_call(operand, name),
        Expr::And(a, b) | Expr::Or(a, b) => {
            has_self_tail_call(a, name) || has_self_tail_call(b, name)
        }
        Expr::CallDirect { args, .. } => args.iter().any(|a| has_self_tail_call(a, name)),
        Expr::Match { subject, arms, .. } => {
            has_self_tail_call(subject, name)
                || arms.iter().any(|a| has_self_tail_call(&a.body, name))
        }
        _ => false,
    }
}

pub(super) fn expr_can_jit(expr: &Expr, natives: Option<&NativeRegistry>) -> bool {
    match expr {
        Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::Unit | Expr::String(_) => true,
        Expr::Hole => false, // Typed holes should not be JIT-compiled
        Expr::Var(_, _) => true,
        Expr::BinOp { lhs, rhs, .. } => expr_can_jit(lhs, natives) && expr_can_jit(rhs, natives),
        Expr::UnaryOp { operand, .. } => expr_can_jit(operand, natives),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            expr_can_jit(condition, natives)
                && expr_can_jit(then_branch, natives)
                && else_branch
                    .as_ref()
                    .is_none_or(|e| expr_can_jit(e, natives))
        }
        Expr::CallDirect { args, .. } => args.iter().all(|a| expr_can_jit(a, natives)),
        Expr::TailCall { args, .. } => args.iter().all(|a| expr_can_jit(a, natives)),
        Expr::Let { value, .. } => expr_can_jit(value, natives),
        Expr::Block(exprs, _) => exprs.iter().all(|e| expr_can_jit(e, natives)),
        Expr::And(a, b) | Expr::Or(a, b) => expr_can_jit(a, natives) && expr_can_jit(b, natives),
        Expr::Concat(parts) => parts.iter().all(|e| expr_can_jit(e, natives)),
        Expr::CallNative {
            args,
            module,
            method,
            ..
        } => {
            let qualified = format!("{}.{}", module, method);

            // AOT path: check if we have a known symbol or inline HOF for this native
            if natives.is_none() {
                #[cfg(feature = "aot")]
                {
                    let is_hof = matches!(
                        qualified.as_str(),
                        "List.map"
                            | "List.filter"
                            | "List.fold"
                            | "List.find"
                            | "Option.map"
                            | "Result.map"
                    );
                    if !is_hof && super::aot::aot_native_symbol(&qualified).is_none() {
                        return false;
                    }
                    return args.iter().all(|a| expr_can_jit(a, natives));
                }
                #[cfg(not(feature = "aot"))]
                return false;
            }

            // JIT path: resolve through registry
            let reg = natives.unwrap();
            // Inline HOFs are always allowed
            let is_hof = matches!(
                qualified.as_str(),
                "List.map"
                    | "List.filter"
                    | "List.fold"
                    | "List.find"
                    | "Option.map"
                    | "Result.map"
            );
            if is_hof {
                return args.iter().all(|a| expr_can_jit(a, natives));
            }
            if reg.lookup(&qualified).is_none() {
                return false;
            }
            // Non-inline HOFs still need VM — can't JIT them
            if let Some(id) = reg.lookup(&qualified)
                && reg.is_hof(id)
            {
                return false;
            }
            args.iter().all(|a| expr_can_jit(a, natives))
        }
        Expr::MakeEnum { payload, .. } => expr_can_jit(payload, natives),
        Expr::MakeStruct { fields, .. } => fields.iter().all(|(_, e)| expr_can_jit(e, natives)),
        Expr::MakeList(items, _) => items.iter().all(|e| expr_can_jit(e, natives)),
        Expr::MakeRecord(fields, _) => fields.iter().all(|(_, e)| expr_can_jit(e, natives)),
        Expr::MakeTuple(items, _) => items.iter().all(|e| expr_can_jit(e, natives)),
        Expr::MakeRange(a, b) => expr_can_jit(a, natives) && expr_can_jit(b, natives),
        Expr::UpdateRecord { base, updates, .. } => {
            expr_can_jit(base, natives) && updates.iter().all(|(_, e)| expr_can_jit(e, natives))
        }
        Expr::GetField { object, .. } => expr_can_jit(object, natives),
        Expr::Match { subject, arms, .. } => {
            expr_can_jit(subject, natives) && arms.iter().all(|a| arm_can_jit(a, natives))
        }
        Expr::For { iterable, body, .. } => {
            expr_can_jit(iterable, natives) && expr_can_jit(body, natives)
        }
        Expr::Try { expr, .. } => expr_can_jit(expr, natives),
        // MakeClosure/GetClosureVar are produced by lambda lifting and are JIT-compilable
        Expr::MakeClosure { captures, .. } => captures.iter().all(|c| expr_can_jit(c, natives)),
        Expr::GetClosureVar(_) => true,
        // Lambda should be eliminated by lifting; CallIndirect is now compilable
        Expr::Lambda { .. } => false,
        Expr::CallIndirect { callee, args, .. }
        | Expr::TailCallIndirect { callee, args, .. } => {
            expr_can_jit(callee, natives) && args.iter().all(|a| expr_can_jit(a, natives))
        }
        // WithHandlers, effects, and Expect not yet JIT-supported
        Expr::WithHandlers { .. }
        | Expr::HandleEffect { .. }
        | Expr::PerformEffect { .. }
        | Expr::Expect { .. } => false,
    }
}

fn arm_can_jit(arm: &MatchArm, natives: Option<&NativeRegistry>) -> bool {
    pattern_can_jit(&arm.pattern) && expr_can_jit(&arm.body, natives)
}

/// Check if a match pattern can be correctly compiled by the JIT.
/// Tuple patterns with nested literals/constructors are not yet supported
/// (they would unconditionally match, producing wrong results).
fn pattern_can_jit(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Wildcard | Pattern::Var(_) | Pattern::Literal(_) => true,
        Pattern::Constructor(_, sub_patterns) => sub_patterns.iter().all(pattern_can_jit),
        Pattern::Tuple(sub_patterns) => {
            // Only allow simple bindings (Var/Wildcard) in tuple patterns.
            // Nested literals or constructors inside tuples are not yet implemented.
            sub_patterns
                .iter()
                .all(|p| matches!(p, Pattern::Var(_) | Pattern::Wildcard))
        }
        Pattern::Record(fields) => {
            fields.iter().all(|(_, sub)| pattern_can_jit(sub))
        }
    }
}

// ---------------------------------------------------------------------------
// Scalar-only analysis for unboxed fast path
// ---------------------------------------------------------------------------

/// Check if a function body only uses scalar types (Int, Bool, Unit, Float).
/// Functions marked scalar-only can use unboxed codegen internally.
pub(super) fn is_scalar_only(func: &IrFunction) -> bool {
    expr_is_scalar(&func.body)
}

fn type_is_scalar(ty: Option<&Type>) -> bool {
    match ty {
        None => true, // No type info — optimistic (other exprs will catch heap usage)
        Some(Type::Int | Type::Float | Type::Bool | Type::Unit) => true,
        Some(_) => false, // String, List, Record, Enum, etc. → heap
    }
}

fn expr_is_scalar(expr: &Expr) -> bool {
    match expr {
        Expr::Int(_) | Expr::Bool(_) | Expr::Unit | Expr::Float(_) => true,
        Expr::Hole => false,
        Expr::Var(_, ty) => type_is_scalar(ty.as_ref()),
        Expr::BinOp { lhs, rhs, .. } => expr_is_scalar(lhs) && expr_is_scalar(rhs),
        Expr::UnaryOp { operand, .. } => expr_is_scalar(operand),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            expr_is_scalar(condition)
                && expr_is_scalar(then_branch)
                && else_branch.as_ref().is_none_or(|e| expr_is_scalar(e))
        }
        Expr::Let { value, .. } => expr_is_scalar(value),
        Expr::Block(exprs, _) => exprs.iter().all(expr_is_scalar),
        Expr::CallDirect { args, .. } | Expr::TailCall { args, .. } => {
            args.iter().all(expr_is_scalar)
        }
        Expr::And(a, b) | Expr::Or(a, b) => expr_is_scalar(a) && expr_is_scalar(b),
        // Everything else (strings, lists, records, match, enums, etc.) → not scalar
        _ => false,
    }
}

/// Collect all function names called (directly or via tail call) from an expression.
fn collect_called_functions(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::CallDirect { name, args, .. } => {
            out.push(name.clone());
            for a in args {
                collect_called_functions(a, out);
            }
        }
        Expr::TailCall { name, args, .. } => {
            out.push(name.clone());
            for a in args {
                collect_called_functions(a, out);
            }
        }
        Expr::BinOp { lhs, rhs, .. } => {
            collect_called_functions(lhs, out);
            collect_called_functions(rhs, out);
        }
        Expr::UnaryOp { operand, .. } => collect_called_functions(operand, out),
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_called_functions(condition, out);
            collect_called_functions(then_branch, out);
            if let Some(e) = else_branch {
                collect_called_functions(e, out);
            }
        }
        Expr::Let { value, .. } => collect_called_functions(value, out),
        Expr::Block(exprs, _) => {
            for e in exprs {
                collect_called_functions(e, out);
            }
        }
        Expr::And(a, b) | Expr::Or(a, b) => {
            collect_called_functions(a, out);
            collect_called_functions(b, out);
        }
        _ => {}
    }
}

/// Compute which functions can use unboxed codegen. A function is unboxed if:
///
/// 1. Its body is scalar-only (no strings, lists, records, etc.)
/// 2. Its return type is Int (the hot path for benchmarks)
/// 3. All functions it calls are also unboxed
///
/// Uses fixed-point iteration to handle mutual recursion.
/// Collect function names that are referenced as values (not just called directly).
/// These functions may be called via CallIndirect and must use boxed ABI.
fn collect_indirect_targets(module: &IrModule) -> std::collections::HashSet<String> {
    let func_name_set: std::collections::HashSet<&str> =
        module.functions.iter().map(|f| f.name.as_str()).collect();
    let mut targets = std::collections::HashSet::new();
    for func in &module.functions {
        collect_indirect_targets_expr(&func.body, &func_name_set, &mut targets);
    }
    targets
}

fn collect_indirect_targets_expr(
    expr: &Expr,
    func_names: &std::collections::HashSet<&str>,
    targets: &mut std::collections::HashSet<String>,
) {
    match expr {
        // For CallDirect/TailCall, the callee name is NOT an indirect target.
        // Only recurse into args.
        Expr::CallDirect { args, .. } | Expr::TailCall { args, .. } => {
            for a in args {
                collect_indirect_targets_expr(a, func_names, targets);
            }
        }
        // A bare Var referencing a function name (outside CallDirect position)
        // means the function is used as a value.
        Expr::Var(name, _) if func_names.contains(name.as_str()) => {
            targets.insert(name.clone());
        }
        // For CallIndirect/TailCallIndirect, callee IS used as a value — recurse into it.
        Expr::CallIndirect { callee, args, .. }
        | Expr::TailCallIndirect { callee, args, .. } => {
            collect_indirect_targets_expr(callee, func_names, targets);
            for a in args {
                collect_indirect_targets_expr(a, func_names, targets);
            }
        }
        // For everything else, recurse into all sub-expressions.
        Expr::BinOp { lhs, rhs, .. } | Expr::And(lhs, rhs) | Expr::Or(lhs, rhs)
        | Expr::MakeRange(lhs, rhs) => {
            collect_indirect_targets_expr(lhs, func_names, targets);
            collect_indirect_targets_expr(rhs, func_names, targets);
        }
        Expr::UnaryOp { operand, .. } | Expr::GetField { object: operand, .. }
        | Expr::Try { expr: operand, .. } | Expr::Let { value: operand, .. } => {
            collect_indirect_targets_expr(operand, func_names, targets);
        }
        Expr::If { condition, then_branch, else_branch, .. } => {
            collect_indirect_targets_expr(condition, func_names, targets);
            collect_indirect_targets_expr(then_branch, func_names, targets);
            if let Some(e) = else_branch {
                collect_indirect_targets_expr(e, func_names, targets);
            }
        }
        Expr::Match { subject, arms, .. } => {
            collect_indirect_targets_expr(subject, func_names, targets);
            for arm in arms {
                collect_indirect_targets_expr(&arm.body, func_names, targets);
            }
        }
        Expr::Block(exprs, _) | Expr::MakeList(exprs, _) | Expr::MakeTuple(exprs, _) => {
            for e in exprs {
                collect_indirect_targets_expr(e, func_names, targets);
            }
        }
        Expr::Concat(parts) => {
            for p in parts {
                collect_indirect_targets_expr(p, func_names, targets);
            }
        }
        Expr::MakeClosure { captures, .. } => {
            for c in captures {
                collect_indirect_targets_expr(c, func_names, targets);
            }
        }
        Expr::CallNative { args, .. } => {
            for a in args {
                collect_indirect_targets_expr(a, func_names, targets);
            }
        }
        Expr::MakeEnum { payload, .. } => {
            collect_indirect_targets_expr(payload, func_names, targets);
        }
        Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => {
            for (_, v) in fields {
                collect_indirect_targets_expr(v, func_names, targets);
            }
        }
        Expr::UpdateRecord { base, updates, .. } => {
            collect_indirect_targets_expr(base, func_names, targets);
            for (_, v) in updates {
                collect_indirect_targets_expr(v, func_names, targets);
            }
        }
        Expr::For { iterable, body, .. } => {
            collect_indirect_targets_expr(iterable, func_names, targets);
            collect_indirect_targets_expr(body, func_names, targets);
        }
        Expr::Lambda { body, .. } => {
            collect_indirect_targets_expr(body, func_names, targets);
        }
        // Leaf nodes — no children
        Expr::Int(_) | Expr::Float(_) | Expr::String(_) | Expr::Bool(_)
        | Expr::Unit | Expr::Hole | Expr::Var(_, _) | Expr::GetClosureVar(_) => {}
        // Effect handler nodes
        Expr::WithHandlers { handlers, body, .. } => {
            for (_, methods) in handlers {
                for (_, h) in methods {
                    collect_indirect_targets_expr(h, func_names, targets);
                }
            }
            collect_indirect_targets_expr(body, func_names, targets);
        }
        Expr::HandleEffect { body, clauses, .. } => {
            collect_indirect_targets_expr(body, func_names, targets);
            for clause in clauses {
                collect_indirect_targets_expr(&clause.body, func_names, targets);
            }
        }
        Expr::PerformEffect { args, .. } => {
            for a in args {
                collect_indirect_targets_expr(a, func_names, targets);
            }
        }
        Expr::Expect { actual, .. } => {
            collect_indirect_targets_expr(actual, func_names, targets);
        }
    }
}

pub(super) fn compute_unboxed_flags(module: &IrModule) -> Vec<bool> {
    let func_names: HashMap<String, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, f)| (f.name.clone(), idx))
        .collect();

    // Functions that can be called indirectly must stay boxed
    let indirect_targets = collect_indirect_targets(module);

    // Start optimistic: all scalar-only non-entry functions are candidates.
    // The entry function stays boxed to preserve correct NaN-boxing for the caller.
    // Lifted lambdas (__lambda_*) stay boxed (they may receive closure as first arg).
    // Functions used as values (indirect targets) stay boxed.
    let mut unboxed: Vec<bool> = module
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| {
            i != module.entry
                && is_scalar_only(f)
                && !f.name.starts_with("__lambda_")
                && !indirect_targets.contains(&f.name)
        })
        .collect();

    // Fixed-point: remove functions whose callees aren't all unboxed
    loop {
        let mut changed = false;
        for (i, func) in module.functions.iter().enumerate() {
            if !unboxed[i] {
                continue;
            }
            let mut callees = Vec::new();
            collect_called_functions(&func.body, &mut callees);
            for callee_name in &callees {
                if let Some(&callee_idx) = func_names.get(callee_name)
                    && callee_idx != i
                    && !unboxed[callee_idx]
                {
                    // Self-recursion is fine, but calling a boxed function isn't
                    unboxed[i] = false;
                    changed = true;
                    break;
                }
            }
        }
        if !changed {
            break;
        }
    }
    unboxed
}

/// Check if an expression only references the given parameter names
/// (no local variables or complex sub-expressions).
pub(super) fn expr_only_refs_params(expr: &Expr, params: &[String]) -> bool {
    match expr {
        Expr::Var(name, _) => params.iter().any(|p| p == name),
        Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::Unit | Expr::Hole => true,
        Expr::BinOp { lhs, rhs, .. } => {
            expr_only_refs_params(lhs, params) && expr_only_refs_params(rhs, params)
        }
        Expr::UnaryOp { operand, .. } => expr_only_refs_params(operand, params),
        _ => false,
    }
}
