//! Static analysis for JIT compilation decisions.
//!
//! Determines which functions can be JIT-compiled, which can use unboxed
//! codegen, which are used as indirect call targets, etc.

use std::collections::HashMap;

use super::super::ir::{Expr, IrFunction, IrModule, MatchArm, Matcher, Pattern};
use super::super::natives::NativeRegistry;
use crate::analysis::types::Type;

/// Higher-order functions that are inlined as Cranelift loops by the JIT.
/// Referenced in both AOT and JIT paths of `expr_can_jit`.
const INLINE_HOFS: &[&str] = &[
    "List.map",
    "List.filter",
    "List.fold",
    "List.find",
    "Map.map",
    "Map.filter",
    "Map.fold",
    "Option.map",
    "Result.map",
    "Result.map_err",
];

/// Check if a function can be JIT-compiled.
pub(super) fn can_jit(func: &IrFunction, natives: Option<&NativeRegistry>) -> bool {
    expr_can_jit(&func.body, natives)
}

/// Check if a function can be JIT-compiled, returning the unsupported
/// expression kind on failure.
pub(super) fn can_jit_reason(
    func: &IrFunction,
    natives: Option<&NativeRegistry>,
) -> Result<(), &'static str> {
    if expr_can_jit(&func.body, natives) {
        Ok(())
    } else {
        Err(find_unsupported_expr(&func.body, natives))
    }
}

/// Find the deepest expression kind that prevents JIT compilation.
/// Recurses into children so that e.g. a Block containing an unsupported
/// Lambda reports "Lambda" rather than "Block".
fn find_unsupported_expr(expr: &Expr, natives: Option<&NativeRegistry>) -> &'static str {
    if expr_can_jit(expr, natives) {
        return "unknown";
    }
    // Try to find a more specific child that fails
    let children: Vec<&Expr> = match expr {
        Expr::BinOp { lhs, rhs, .. } | Expr::And(lhs, rhs) | Expr::Or(lhs, rhs) => {
            vec![lhs, rhs]
        }
        Expr::UnaryOp { operand, .. } => vec![operand],
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            let mut c = vec![condition.as_ref(), then_branch.as_ref()];
            if let Some(e) = else_branch {
                c.push(e);
            }
            c
        }
        Expr::Block(exprs, _) | Expr::Concat(exprs) | Expr::MakeList(exprs, _) | Expr::MakeTuple(exprs, _) => {
            exprs.iter().collect()
        }
        Expr::Let { value, .. } | Expr::Assign { value, .. } | Expr::FieldAssign { value, .. } => {
            vec![value]
        }
        Expr::CallDirect { args, .. }
        | Expr::TailCall { args, .. }
        | Expr::CallNative { args, .. }
        | Expr::PerformEffect { args, .. } => args.iter().collect(),
        Expr::CallIndirect { callee, args, .. }
        | Expr::TailCallIndirect { callee, args, .. } => {
            let mut c: Vec<&Expr> = vec![callee];
            c.extend(args.iter());
            c
        }
        Expr::MakeEnum { payload, .. } => vec![payload],
        Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => {
            fields.iter().map(|(_, v)| v).collect()
        }
        Expr::MakeRange(a, b) => vec![a, b],
        Expr::UpdateRecord { base, updates, .. } => {
            let mut c: Vec<&Expr> = vec![base];
            c.extend(updates.iter().map(|(_, v)| v));
            c
        }
        Expr::GetField { object, .. } => vec![object],
        Expr::Match { subject, arms, .. } => {
            let mut c: Vec<&Expr> = vec![subject];
            c.extend(arms.iter().map(|a| &a.body));
            c
        }
        Expr::For { iterable, body, .. } => vec![iterable, body],
        Expr::Try { expr, .. } => vec![expr],
        Expr::MakeClosure { captures, .. } => captures.iter().collect(),
        Expr::Lambda { body, .. } => vec![body],
        Expr::Drop { body, .. } => vec![body],
        Expr::Reuse { alloc, .. } => vec![alloc],
        Expr::Expect { actual, .. } => vec![actual],
        _ => vec![],
    };
    for child in children {
        if !expr_can_jit(child, natives) {
            return find_unsupported_expr(child, natives);
        }
    }
    // No child is unsupported — this node itself is the culprit
    expr_kind_name(expr)
}

fn expr_kind_name(expr: &Expr) -> &'static str {
    match expr {
        Expr::Int(_) => "Int",
        Expr::Float(_) => "Float",
        Expr::Bool(_) => "Bool",
        Expr::Unit => "Unit",
        Expr::String(_) => "String",
        Expr::Hole => "Hole",
        Expr::Var(_, _) => "Var",
        Expr::BinOp { .. } => "BinOp",
        Expr::UnaryOp { .. } => "UnaryOp",
        Expr::If { .. } => "If",
        Expr::CallDirect { .. } => "CallDirect",
        Expr::TailCall { .. } => "TailCall",
        Expr::Let { .. } => "Let",
        Expr::Block(_, _) => "Block",
        Expr::And(_, _) => "And",
        Expr::Or(_, _) => "Or",
        Expr::Concat(_) => "Concat",
        Expr::CallNative { .. } => "CallNative",
        Expr::MakeEnum { .. } => "MakeEnum",
        Expr::MakeStruct { .. } => "MakeStruct",
        Expr::MakeList(_, _) => "MakeList",
        Expr::MakeRecord(_, _) => "MakeRecord",
        Expr::MakeTuple(_, _) => "MakeTuple",
        Expr::MakeRange(_, _) => "MakeRange",
        Expr::UpdateRecord { .. } => "UpdateRecord",
        Expr::GetField { .. } => "GetField",
        Expr::Match { .. } => "Match",
        Expr::For { .. } => "For",
        Expr::Try { .. } => "Try",
        Expr::MakeClosure { .. } => "MakeClosure",
        Expr::GetClosureVar(_) => "GetClosureVar",
        Expr::Lambda { .. } => "Lambda",
        Expr::CallIndirect { .. } => "CallIndirect",
        Expr::TailCallIndirect { .. } => "TailCallIndirect",
        Expr::Expect { .. } => "Expect",
        Expr::Drop { .. } => "Drop",
        Expr::Reuse { .. } => "Reuse",
        Expr::WithHandlers { .. } => "WithHandlers",
        Expr::HandleEffect { .. } => "HandleEffect",
        Expr::PerformEffect { .. } => "PerformEffect",
        Expr::Assign { .. } => "Assign",
        Expr::FieldAssign { .. } => "FieldAssign",
    }
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
        Expr::Hole => true, // Compiles to a trap — panics at runtime if reached
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
            let qualified = if module.is_empty() {
                method.to_string()
            } else {
                format!("{}.{}", module, method)
            };

            // AOT path: check if we have a known symbol or inline HOF for this native
            if natives.is_none() {
                #[cfg(feature = "aot")]
                {
                    let is_hof = INLINE_HOFS.contains(&qualified.as_str());
                    if !is_hof && super::aot::aot_native_symbol(&qualified).is_none() {
                        return false;
                    }
                    return args.iter().all(|a| expr_can_jit(a, natives));
                }
                #[cfg(not(feature = "aot"))]
                return false;
            }

            // JIT path: resolve through registry
            let Some(reg) = natives else {
                return false;
            };
            // Inline HOFs are always allowed (compiled as pure IR in stdlib)
            let is_inline_hof = INLINE_HOFS.contains(&qualified.as_str());
            if is_inline_hof {
                return args.iter().all(|a| expr_can_jit(a, natives));
            }
            if reg.lookup(&qualified).is_none() {
                return false;
            }
            // Non-inline HOFs that need VM re-entrancy can't be JIT-compiled
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
        Expr::CallIndirect { callee, args, .. } | Expr::TailCallIndirect { callee, args, .. } => {
            expr_can_jit(callee, natives) && args.iter().all(|a| expr_can_jit(a, natives))
        }
        // Expect: recurse into the actual expression (matcher is compiled inline)
        Expr::Expect { actual, matcher } => {
            expr_can_jit(actual, natives) && matcher_can_jit(matcher, natives)
        }
        // Perceus reuse nodes — fully JIT-compilable
        Expr::Drop { body, .. } => expr_can_jit(body, natives),
        Expr::Reuse { alloc, .. } => expr_can_jit(alloc, natives),
        // Effect handler nodes should not reach can_jit — both tail-resumptive
        // (evidence transform) and non-tail-resumptive (fiber transform) are
        // eliminated before this check. This false is a safety net only.
        Expr::WithHandlers { .. } | Expr::HandleEffect { .. } | Expr::PerformEffect { .. } => false,
        Expr::Assign { value, .. } | Expr::FieldAssign { value, .. } => {
            expr_can_jit(value, natives)
        }
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
        Pattern::Tuple(sub_patterns) => sub_patterns.iter().all(pattern_can_jit),
        Pattern::Record(fields) => fields.iter().all(|(_, sub)| pattern_can_jit(sub)),
        Pattern::List(elems, _rest) => elems.iter().all(pattern_can_jit),
    }
}

/// Check if a Matcher variant can be compiled by the JIT.
fn matcher_can_jit(matcher: &Matcher, natives: Option<&NativeRegistry>) -> bool {
    match matcher {
        Matcher::Equal(expected) => expr_can_jit(expected, natives),
        Matcher::BeOk | Matcher::BeSome | Matcher::BeNone | Matcher::BeEmpty => true,
        Matcher::HaveLength(expected) => expr_can_jit(expected, natives),
        Matcher::Contain(expected) => expr_can_jit(expected, natives),
        Matcher::StartWith(expected) => expr_can_jit(expected, natives),
        Matcher::Satisfy(pred) => expr_can_jit(pred, natives),
        Matcher::Be(_pattern) => true, // compiled as match expression
    }
}

// ---------------------------------------------------------------------------
// Scalar-only analysis for unboxed fast path
// ---------------------------------------------------------------------------

/// Check if a function body only uses scalar types (Int, Bool, Unit).
/// Functions marked scalar-only can use unboxed codegen internally.
pub(super) fn is_scalar_only(func: &IrFunction) -> bool {
    expr_is_scalar(&func.body)
}

fn type_is_scalar(ty: Option<&Type>) -> bool {
    match ty {
        None => false, // No type info — pessimistic to avoid misclassifying heap pointers as scalars
        Some(Type::Int | Type::Bool | Type::Unit) => true,
        Some(_) => false, // String, List, Record, Enum, Float, etc. → not scalar
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
/// 2. Its return type is a supported scalar boundary type (currently Int/Bool/Unit)
/// 3. All functions it calls are also unboxed
///
/// Uses fixed-point iteration to handle mutual recursion.
/// Collect function names that are referenced as values (not just called directly).
/// These functions may be called via CallIndirect and must use boxed ABI.
pub(super) fn collect_indirect_targets(module: &IrModule) -> std::collections::HashSet<String> {
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
        Expr::CallIndirect { callee, args, .. } | Expr::TailCallIndirect { callee, args, .. } => {
            collect_indirect_targets_expr(callee, func_names, targets);
            for a in args {
                collect_indirect_targets_expr(a, func_names, targets);
            }
        }
        // For everything else, recurse into all sub-expressions.
        Expr::BinOp { lhs, rhs, .. }
        | Expr::And(lhs, rhs)
        | Expr::Or(lhs, rhs)
        | Expr::MakeRange(lhs, rhs) => {
            collect_indirect_targets_expr(lhs, func_names, targets);
            collect_indirect_targets_expr(rhs, func_names, targets);
        }
        Expr::UnaryOp { operand, .. }
        | Expr::GetField {
            object: operand, ..
        }
        | Expr::Try { expr: operand, .. }
        | Expr::Let { value: operand, .. } => {
            collect_indirect_targets_expr(operand, func_names, targets);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
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
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Unit
        | Expr::Hole
        | Expr::Var(_, _)
        | Expr::GetClosureVar(_) => {}
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
        Expr::Drop { body, .. } => {
            collect_indirect_targets_expr(body, func_names, targets);
        }
        Expr::Reuse { alloc, .. } => {
            collect_indirect_targets_expr(alloc, func_names, targets);
        }
        Expr::Assign { value, .. } | Expr::FieldAssign { value, .. } => {
            collect_indirect_targets_expr(value, func_names, targets);
        }
    }
}

pub(super) fn compute_unboxed_flags(
    module: &IrModule,
    indirect_targets: &std::collections::HashSet<String>,
) -> Vec<bool> {
    let func_names: HashMap<String, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, f)| (f.name.clone(), idx))
        .collect();

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
                // Float operations still use boxed lowering; keep Float returns boxed.
                && !matches!(f.ty, Some(Type::Float))
                && !f.name.starts_with("__lambda_")
                && !indirect_targets.contains(&f.name)
        })
        .collect();

    // Pre-compute call graph: for each function, collect the indices of callees.
    // This avoids re-walking the AST on every fixed-point iteration.
    let call_graph: Vec<Vec<usize>> = module
        .functions
        .iter()
        .map(|func| {
            let mut names = Vec::new();
            collect_called_functions(&func.body, &mut names);
            names
                .iter()
                .filter_map(|n| func_names.get(n).copied())
                .collect()
        })
        .collect();

    // Fixed-point: remove functions whose callees aren't all unboxed
    loop {
        let mut changed = false;
        for (i, callees) in call_graph.iter().enumerate() {
            if !unboxed[i] {
                continue;
            }
            for &callee_idx in callees {
                if callee_idx != i && !unboxed[callee_idx] {
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

/// Extract sorted field names from a record type if all fields are scalar (Int, Float, Bool).
/// Returns None for non-record types or records with non-scalar fields.
fn record_scalar_fields(ty: &Type) -> Option<Vec<String>> {
    let fields: &HashMap<String, Type> = match ty {
        Type::Record(fields, _) => fields,
        Type::Struct(_, fields) => fields,
        _ => return None,
    };
    let mut names: Vec<String> = Vec::new();
    for (name, field_ty) in fields {
        match field_ty {
            Type::Int | Type::Float | Type::Bool | Type::Unit => {
                names.push(name.clone());
            }
            _ => return None,
        }
    }
    names.sort();
    Some(names)
}

/// Compute multi-return info for each function.
/// Returns a Vec parallel to module.functions where Some(field_names) means the function
/// can use multi-value returns (one return register per field), and None means standard
/// single-return ABI.
///
/// A function qualifies for multi-return when:
/// 1. Its return type is a record with all-scalar fields
/// 2. It's not the entry function
/// 3. It's not used as an indirect call target (closure, passed as value)
/// 4. It's not a lambda
pub(super) fn compute_multireturn_info(
    module: &IrModule,
    indirect_targets: &std::collections::HashSet<String>,
) -> Vec<Option<Vec<String>>> {
    module
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| {
            if i == module.entry {
                return None;
            }
            if f.name.starts_with("__lambda_") {
                return None;
            }
            if indirect_targets.contains(&f.name) {
                return None;
            }
            // Check return type
            let ret_ty = match &f.ty {
                Some(Type::Function(_, ret)) => ret.as_ref(),
                _ => return None,
            };
            record_scalar_fields(ret_ty)
        })
        .collect()
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
