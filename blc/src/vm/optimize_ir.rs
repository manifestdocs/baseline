use std::collections::{HashMap, HashSet};

use super::ir::{
    BinOp, Expr, HandlerClause, IrFunction, IrModule, MatchArm, Matcher, Pattern, Span,
    TagRegistry, UnaryOp,
};

/// Run all IR optimization passes on the module.
/// Run all IR optimization passes on the module.
/// When `lift_lambdas_pass` is true, performs lambda lifting (required for JIT).
/// The bytecode codegen handles lambdas natively, so lambda lifting should be
/// skipped when targeting bytecode.
pub fn optimize(module: &mut IrModule) {
    optimize_inner(module, true);
}

/// Optimize for bytecode codegen: skip lambda lifting (bytecode handles
/// `Expr::Lambda` directly via its own closure compilation).
pub fn optimize_for_bytecode(module: &mut IrModule) {
    optimize_inner(module, false);
}

/// Detect mutually-capturing closure pairs in a module.
///
/// Scans each function body for `Block` sequences where two or more
/// `let name = |..| ...` bindings form a mutual capture cycle:
///   `a`'s body references `b` AND `b`'s body references `a`.
///
/// Returns `(name_a, name_b)` pairs (lexicographically ordered, deduplicated).
/// These are the only reference cycles possible in today's Baseline language
/// (no mutable refs, no lazy thunks).
pub fn detect_module_closure_cycles(module: &IrModule) -> Vec<(String, String)> {
    let top_level: HashSet<String> = module.functions.iter().map(|f| f.name.clone()).collect();
    let mut all_cycles: Vec<(String, String)> = Vec::new();
    for func in &module.functions {
        detect_closure_cycles_in_expr(&func.body, &top_level, &mut all_cycles);
    }
    // Deduplicate (a,b) vs (b,a): canonical form is alpha-sorted
    all_cycles.sort();
    all_cycles.dedup();
    all_cycles
}

/// Recursively scan an expression for mutual closure captures.
fn detect_closure_cycles_in_expr(
    expr: &Expr,
    top_level: &HashSet<String>,
    out: &mut Vec<(String, String)>,
) {
    match expr {
        Expr::Block(exprs, _) => {
            detect_cycles_in_block(exprs, top_level, out);
            // Also recurse into non-Lambda children
            for e in exprs {
                detect_closure_cycles_in_expr(e, top_level, out);
            }
        }
        Expr::Lambda { body, .. } => {
            detect_closure_cycles_in_expr(body, top_level, out);
        }
        other => {
            visit_expr_children(other, &mut |child| {
                detect_closure_cycles_in_expr(child, top_level, out);
            });
        }
    }
}

/// Scan a flat list of Block expressions for mutually-capturing Lambda lets.
fn detect_cycles_in_block(
    exprs: &[Expr],
    top_level: &HashSet<String>,
    out: &mut Vec<(String, String)>,
) {
    // Collect (name, free_vars_of_lambda_body) for each let-bound Lambda
    let mut lambda_lets: Vec<(String, HashSet<String>)> = Vec::new();

    for e in exprs {
        if let Expr::Let { pattern, value, .. } = e
            && let Pattern::Var(name) = pattern.as_ref()
            && let Expr::Lambda { params, body, .. } = value.as_ref()
        {
            let params_set: HashSet<String> = params.iter().cloned().collect();
            let fvs = free_vars(body, &params_set, top_level);
            lambda_lets.push((name.clone(), fvs));
        }
    }

    // Check every pair for mutual capture
    for i in 0..lambda_lets.len() {
        for j in (i + 1)..lambda_lets.len() {
            let (ref name_a, ref fvs_a) = lambda_lets[i];
            let (ref name_b, ref fvs_b) = lambda_lets[j];
            if fvs_a.contains(name_b) && fvs_b.contains(name_a) {
                // Canonical: alphabetical order
                let pair = if name_a <= name_b {
                    (name_a.clone(), name_b.clone())
                } else {
                    (name_b.clone(), name_a.clone())
                };
                out.push(pair);
            }
        }
    }
}

fn optimize_inner(module: &mut IrModule, lift: bool) {
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
    if lift {
        // Lambda lifting: transform Lambda nodes into MakeClosure + lifted functions
        lift_lambdas(module);
    }
    // Build tag registry: assign integer IDs to all enum tags
    build_tag_registry(module);
}

// ---------------------------------------------------------------------------
// Lambda Lifting
// ---------------------------------------------------------------------------

/// Compute the free variables of an expression.
/// `bound` tracks variables bound in the current scope.
/// `top_level` contains function names that don't need capturing.
fn free_vars(expr: &Expr, bound: &HashSet<String>, top_level: &HashSet<String>) -> HashSet<String> {
    let mut result = HashSet::new();
    free_vars_inner(expr, bound, top_level, &mut result);
    result
}

fn free_vars_inner(
    expr: &Expr,
    bound: &HashSet<String>,
    top_level: &HashSet<String>,
    out: &mut HashSet<String>,
) {
    match expr {
        Expr::Var(name, _) => {
            if !bound.contains(name) && !top_level.contains(name) {
                out.insert(name.clone());
            }
        }
        Expr::GetClosureVar(_) => {} // Already resolved — not free
        Expr::MakeClosure { captures, .. } => {
            for c in captures {
                free_vars_inner(c, bound, top_level, out);
            }
        }
        Expr::Lambda { params, body, .. } => {
            let mut inner_bound = bound.clone();
            for p in params {
                inner_bound.insert(p.clone());
            }
            free_vars_inner(body, &inner_bound, top_level, out);
        }
        Expr::Let {
            pattern: _, value, ..
        } => {
            free_vars_inner(value, bound, top_level, out);
            // The pattern binds names for subsequent expressions in the same block,
            // but Let is a single node here — the binding is visible in the block
            // that contains this Let, handled by Block below.
        }
        Expr::Block(exprs, _) => {
            // Process sequentially: each Let adds to bound for subsequent exprs
            let mut block_bound = bound.clone();
            for e in exprs {
                free_vars_inner(e, &block_bound, top_level, out);
                if let Expr::Let { pattern, .. } = e {
                    collect_pattern_names(pattern, &mut block_bound);
                }
            }
        }
        Expr::For {
            binding,
            iterable,
            body,
        } => {
            free_vars_inner(iterable, bound, top_level, out);
            let mut inner_bound = bound.clone();
            inner_bound.insert(binding.clone());
            free_vars_inner(body, &inner_bound, top_level, out);
        }
        Expr::Match { subject, arms, .. } => {
            free_vars_inner(subject, bound, top_level, out);
            for arm in arms {
                let mut arm_bound = bound.clone();
                collect_pattern_names(&arm.pattern, &mut arm_bound);
                if let Some(guard) = &arm.guard {
                    free_vars_inner(guard, &arm_bound, top_level, out);
                }
                free_vars_inner(&arm.body, &arm_bound, top_level, out);
            }
        }
        // All other nodes: recurse into children
        _ => {
            visit_expr_children(expr, &mut |child| {
                free_vars_inner(child, bound, top_level, out);
            });
        }
    }
}

/// Collect variable names introduced by a pattern.
fn collect_pattern_names(pattern: &Pattern, names: &mut HashSet<String>) {
    match pattern {
        Pattern::Var(name) => {
            names.insert(name.clone());
        }
        Pattern::Constructor(_, sub) | Pattern::Tuple(sub) => {
            for p in sub {
                collect_pattern_names(p, names);
            }
        }
        Pattern::Record(fields) => {
            for (_, sub_pat) in fields {
                collect_pattern_names(sub_pat, names);
            }
        }
        Pattern::List(sub_pats, rest) => {
            for p in sub_pats {
                collect_pattern_names(p, names);
            }
            if let Some(rest_name) = rest {
                names.insert(rest_name.clone());
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) => {}
    }
}

/// Replace occurrences of free variable names with GetClosureVar(index).
fn replace_free_vars(expr: Expr, fv_map: &HashMap<String, usize>) -> Expr {
    transform_expr(expr, &mut |e| {
        if let Expr::Var(ref name, _) = e
            && let Some(&idx) = fv_map.get(name)
        {
            return Expr::GetClosureVar(idx);
        }
        e
    })
}

/// Lift all Lambda expressions into top-level functions with explicit captures.
fn lift_lambdas(module: &mut IrModule) {
    let top_level: HashSet<String> = module.functions.iter().map(|f| f.name.clone()).collect();
    let mut counter: usize = 0;
    let mut new_functions: Vec<IrFunction> = Vec::new();

    // Process each existing function (iterate by index since we append new ones)
    let original_count = module.functions.len();
    for fi in 0..original_count {
        let body = module.functions[fi].body.clone();
        let lifted = lift_lambdas_in_expr(
            body,
            &top_level,
            &mut counter,
            &mut new_functions,
            original_count,
        );
        module.functions[fi].body = lifted;
    }

    module.functions.extend(new_functions);
}

/// Recursively lift lambdas in an expression (bottom-up).
fn lift_lambdas_in_expr(
    expr: Expr,
    top_level: &HashSet<String>,
    counter: &mut usize,
    new_functions: &mut Vec<IrFunction>,
    base_idx: usize,
) -> Expr {
    transform_expr(expr, &mut |e| {
        if let Expr::Lambda { params, body, ty } = e {
            // Compute free variables of the lambda body
            let params_set: HashSet<String> = params.iter().cloned().collect();
            let fvs = free_vars(&body, &params_set, top_level);
            let mut fv_list: Vec<String> = fvs.into_iter().collect();
            fv_list.sort(); // Deterministic ordering

            // Build mapping from free var name to closure index
            let fv_map: HashMap<String, usize> = fv_list
                .iter()
                .enumerate()
                .map(|(i, name)| (name.clone(), i))
                .collect();

            // Replace free vars in body with GetClosureVar(i)
            let transformed_body = if fv_map.is_empty() {
                *body
            } else {
                replace_free_vars(*body, &fv_map)
            };

            // Create the lifted function
            let func_name = format!("__lambda_{}", *counter);
            *counter += 1;
            let func_idx = base_idx + new_functions.len();

            let mut lifted_params = Vec::with_capacity(1 + params.len());
            if !fv_list.is_empty() {
                lifted_params.push("__closure".to_string());
            }
            lifted_params.extend(params);

            new_functions.push(IrFunction {
                name: func_name,
                params: lifted_params,
                body: transformed_body,
                ty: ty.clone(),
                span: Span {
                    line: 0,
                    col: 0,
                    start_byte: 0,
                    end_byte: 0,
                },
            });

            if fv_list.is_empty() {
                // Zero captures: just a function reference, no heap allocation
                Expr::MakeClosure {
                    func_idx,
                    captures: vec![],
                }
            } else {
                Expr::MakeClosure {
                    func_idx,
                    captures: fv_list
                        .into_iter()
                        .map(|name| Expr::Var(name, None))
                        .collect(),
                }
            }
        } else {
            e
        }
    })
}

// ---------------------------------------------------------------------------
// Tag Registry Builder
// ---------------------------------------------------------------------------

/// Walk all functions in the module, collecting enum tags from `MakeEnum` and
/// `Pattern::Constructor` nodes, and register them in the module's TagRegistry.
fn build_tag_registry(module: &mut IrModule) {
    for func in &module.functions {
        collect_tags_from_expr(&func.body, &mut module.tags);
    }
}

/// Collect enum tag strings from an expression tree.
fn collect_tags_from_expr(expr: &Expr, tags: &mut TagRegistry) {
    visit_expr(expr, &mut |e| match e {
        Expr::MakeEnum { tag, .. } => {
            tags.register(tag);
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                collect_tags_from_pattern(&arm.pattern, tags);
            }
        }
        _ => {}
    });
}

/// Collect enum tag strings from a pattern.
fn collect_tags_from_pattern(pattern: &Pattern, tags: &mut TagRegistry) {
    if let Pattern::Constructor(tag, sub_patterns) = pattern {
        tags.register(tag);
        for sub in sub_patterns {
            collect_tags_from_pattern(sub, tags);
        }
    }
}

// ---------------------------------------------------------------------------
// Generic tree-walk utilities
// ---------------------------------------------------------------------------

/// Recursively transform an expression tree bottom-up.
///
/// Walks all children first (applying `f` recursively), then applies `f`
/// to the reconstructed node. This handles the exhaustive `Expr` match
/// once, so callers only need to handle the variants they care about.
fn transform_expr(expr: Expr, f: &mut impl FnMut(Expr) -> Expr) -> Expr {
    let walked = walk_expr_children(expr, |child| transform_expr(child, f));
    f(walked)
}

/// Walk all immediate children of an expression, applying `f` to each child.
/// The node itself is NOT transformed — only its children are.
///
/// This is the single exhaustive match that replaces all the duplicated
/// boilerplate across the optimizer passes.
fn walk_expr_children(expr: Expr, mut f: impl FnMut(Expr) -> Expr) -> Expr {
    match expr {
        // Literals — no children
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Unit
        | Expr::Hole
        | Expr::Var(_, _) => expr,

        // Binary operations
        Expr::BinOp { op, lhs, rhs, ty } => Expr::BinOp {
            op,
            lhs: Box::new(f(*lhs)),
            rhs: Box::new(f(*rhs)),
            ty,
        },
        Expr::UnaryOp { op, operand, ty } => Expr::UnaryOp {
            op,
            operand: Box::new(f(*operand)),
            ty,
        },
        Expr::And(l, r) => Expr::And(Box::new(f(*l)), Box::new(f(*r))),
        Expr::Or(l, r) => Expr::Or(Box::new(f(*l)), Box::new(f(*r))),

        // Control flow
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ty,
        } => Expr::If {
            condition: Box::new(f(*condition)),
            then_branch: Box::new(f(*then_branch)),
            else_branch: else_branch.map(|e| Box::new(f(*e))),
            ty,
        },
        Expr::Match { subject, arms, ty } => Expr::Match {
            subject: Box::new(f(*subject)),
            arms: arms
                .into_iter()
                .map(|arm| MatchArm {
                    pattern: arm.pattern,
                    guard: arm.guard.map(&mut f),
                    body: f(arm.body),
                })
                .collect(),
            ty,
        },
        Expr::For {
            binding,
            iterable,
            body,
        } => Expr::For {
            binding,
            iterable: Box::new(f(*iterable)),
            body: Box::new(f(*body)),
        },

        // Bindings
        Expr::Let { pattern, value, ty } => Expr::Let {
            pattern,
            value: Box::new(f(*value)),
            ty,
        },
        Expr::Block(exprs, ty) => Expr::Block(exprs.into_iter().map(&mut f).collect(), ty),

        // Calls
        Expr::CallDirect { name, args, ty } => Expr::CallDirect {
            name,
            args: args.into_iter().map(&mut f).collect(),
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
            args: args.into_iter().map(&mut f).collect(),
            ty,
        },
        Expr::CallIndirect { callee, args, ty } => Expr::CallIndirect {
            callee: Box::new(f(*callee)),
            args: args.into_iter().map(&mut f).collect(),
            ty,
        },
        Expr::TailCallIndirect { callee, args, ty } => Expr::TailCallIndirect {
            callee: Box::new(f(*callee)),
            args: args.into_iter().map(&mut f).collect(),
            ty,
        },
        Expr::TailCall { name, args, ty } => Expr::TailCall {
            name,
            args: args.into_iter().map(&mut f).collect(),
            ty,
        },

        // Constructors
        Expr::MakeEnum { tag, payload, ty } => Expr::MakeEnum {
            tag,
            payload: Box::new(f(*payload)),
            ty,
        },
        Expr::MakeStruct { name, fields, ty } => Expr::MakeStruct {
            name,
            fields: fields.into_iter().map(|(k, v)| (k, f(v))).collect(),
            ty,
        },

        // Data construction
        Expr::MakeList(items, ty) => Expr::MakeList(items.into_iter().map(&mut f).collect(), ty),
        Expr::MakeRecord(fields, ty) => {
            Expr::MakeRecord(fields.into_iter().map(|(k, v)| (k, f(v))).collect(), ty)
        }
        Expr::MakeTuple(items, ty) => Expr::MakeTuple(items.into_iter().map(&mut f).collect(), ty),
        Expr::MakeRange(s, e) => Expr::MakeRange(Box::new(f(*s)), Box::new(f(*e))),
        Expr::UpdateRecord { base, updates, ty } => Expr::UpdateRecord {
            base: Box::new(f(*base)),
            updates: updates.into_iter().map(|(k, v)| (k, f(v))).collect(),
            ty,
        },

        // Field access
        Expr::GetField {
            object,
            field,
            field_idx,
            ty,
        } => Expr::GetField {
            object: Box::new(f(*object)),
            field,
            field_idx,
            ty,
        },

        // Functions
        Expr::Lambda { params, body, ty } => Expr::Lambda {
            params,
            body: Box::new(f(*body)),
            ty,
        },
        Expr::MakeClosure { func_idx, captures } => Expr::MakeClosure {
            func_idx,
            captures: captures.into_iter().map(&mut f).collect(),
        },
        Expr::GetClosureVar(_) => expr,

        // Error handling
        Expr::Try { expr, ty } => Expr::Try {
            expr: Box::new(f(*expr)),
            ty,
        },

        // String interpolation
        Expr::Concat(parts) => Expr::Concat(parts.into_iter().map(&mut f).collect()),

        // Effect handlers
        Expr::WithHandlers { handlers, body } => Expr::WithHandlers {
            handlers: handlers
                .into_iter()
                .map(|(name, methods)| {
                    (
                        name,
                        methods.into_iter().map(|(mk, h)| (mk, f(h))).collect(),
                    )
                })
                .collect(),
            body: Box::new(f(*body)),
        },
        Expr::HandleEffect { body, clauses } => Expr::HandleEffect {
            body: Box::new(f(*body)),
            clauses: clauses
                .into_iter()
                .map(|c| HandlerClause {
                    body: f(c.body),
                    ..c
                })
                .collect(),
        },
        Expr::PerformEffect {
            effect,
            method,
            args,
            ty,
        } => Expr::PerformEffect {
            effect,
            method,
            args: args.into_iter().map(&mut f).collect(),
            ty,
        },

        // Test expressions
        Expr::Expect { actual, matcher } => {
            let actual = Box::new(f(*actual));
            let matcher = match *matcher {
                Matcher::Equal(e) => Matcher::Equal(Box::new(f(*e))),
                Matcher::HaveLength(e) => Matcher::HaveLength(Box::new(f(*e))),
                Matcher::Contain(e) => Matcher::Contain(Box::new(f(*e))),
                Matcher::StartWith(e) => Matcher::StartWith(Box::new(f(*e))),
                Matcher::Satisfy(e) => Matcher::Satisfy(Box::new(f(*e))),
                other => other,
            };
            Expr::Expect {
                actual,
                matcher: Box::new(matcher),
            }
        }
    }
}

/// Visit all nodes in an expression tree (read-only, depth-first).
fn visit_expr(expr: &Expr, f: &mut impl FnMut(&Expr)) {
    f(expr);
    visit_expr_children(expr, f);
}

/// Visit all immediate children of an expression (read-only).
fn visit_expr_children(expr: &Expr, f: &mut impl FnMut(&Expr)) {
    match expr {
        // Literals — no children
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Unit
        | Expr::Hole
        | Expr::Var(_, _) => {}

        // Binary operations
        Expr::BinOp { lhs, rhs, .. } => {
            visit_expr(lhs, f);
            visit_expr(rhs, f);
        }
        Expr::UnaryOp { operand, .. } => visit_expr(operand, f),
        Expr::And(l, r) | Expr::Or(l, r) => {
            visit_expr(l, f);
            visit_expr(r, f);
        }

        // Control flow
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            visit_expr(condition, f);
            visit_expr(then_branch, f);
            if let Some(e) = else_branch {
                visit_expr(e, f);
            }
        }
        Expr::Match { subject, arms, .. } => {
            visit_expr(subject, f);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    visit_expr(guard, f);
                }
                visit_expr(&arm.body, f);
            }
        }
        Expr::For { iterable, body, .. } => {
            visit_expr(iterable, f);
            visit_expr(body, f);
        }

        // Bindings
        Expr::Let { value, .. } => visit_expr(value, f),
        Expr::Block(exprs, _) => {
            for e in exprs {
                visit_expr(e, f);
            }
        }

        // Calls
        Expr::CallDirect { args, .. }
        | Expr::CallNative { args, .. }
        | Expr::TailCall { args, .. } => {
            for a in args {
                visit_expr(a, f);
            }
        }
        Expr::CallIndirect { callee, args, .. } | Expr::TailCallIndirect { callee, args, .. } => {
            visit_expr(callee, f);
            for a in args {
                visit_expr(a, f);
            }
        }

        // Constructors
        Expr::MakeEnum { payload, .. } => visit_expr(payload, f),
        Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => {
            for (_, v) in fields {
                visit_expr(v, f);
            }
        }

        // Data construction
        Expr::MakeList(items, _) | Expr::MakeTuple(items, _) => {
            for item in items {
                visit_expr(item, f);
            }
        }
        Expr::MakeRange(s, e) => {
            visit_expr(s, f);
            visit_expr(e, f);
        }
        Expr::UpdateRecord { base, updates, .. } => {
            visit_expr(base, f);
            for (_, v) in updates {
                visit_expr(v, f);
            }
        }

        // Field access
        Expr::GetField { object, .. } => visit_expr(object, f),

        // Functions
        Expr::Lambda { body, .. } => visit_expr(body, f),
        Expr::MakeClosure { captures, .. } => {
            for c in captures {
                visit_expr(c, f);
            }
        }
        Expr::GetClosureVar(_) => {}

        // Error handling
        Expr::Try { expr, .. } => visit_expr(expr, f),

        // String interpolation
        Expr::Concat(parts) => {
            for p in parts {
                visit_expr(p, f);
            }
        }

        // Effect handlers
        Expr::WithHandlers { handlers, body, .. } => {
            for (_, methods) in handlers {
                for (_, h) in methods {
                    visit_expr(h, f);
                }
            }
            visit_expr(body, f);
        }
        Expr::HandleEffect { body, clauses, .. } => {
            visit_expr(body, f);
            for c in clauses {
                visit_expr(&c.body, f);
            }
        }
        Expr::PerformEffect { args, .. } => {
            for a in args {
                visit_expr(a, f);
            }
        }

        // Test expressions
        Expr::Expect { actual, matcher } => {
            visit_expr(actual, f);
            match matcher.as_ref() {
                Matcher::Equal(e)
                | Matcher::HaveLength(e)
                | Matcher::Contain(e)
                | Matcher::StartWith(e)
                | Matcher::Satisfy(e) => visit_expr(e, f),
                _ => {}
            }
        }
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

/// Propagate constants through an expression tree, folding as we go.
///
/// This pass needs scope-aware tracking (Block/Lambda/For push new scopes),
/// so it does a manual walk rather than using `transform_expr`.
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

        // All other nodes: recurse into children with propagate_expr
        other => walk_expr_children(other, |child| propagate_expr(child, env)),
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
    let mut count = 0;
    visit_expr(expr, &mut |_| count += 1);
    count
}

/// Check if an expression contains a call (direct or tail) to the named function.
fn references_function(expr: &Expr, name: &str) -> bool {
    let mut found = false;
    visit_expr(expr, &mut |e| {
        if found {
            return;
        }
        match e {
            Expr::CallDirect { name: n, .. } | Expr::TailCall { name: n, .. } => {
                if n == name {
                    found = true;
                }
            }
            _ => {}
        }
    });
    found
}

/// Rename all occurrences of variables in `rename_map` within an expression.
fn rename_vars(expr: Expr, rename_map: &HashMap<String, String>) -> Expr {
    transform_expr(expr, &mut |e| {
        match e {
            Expr::Var(name, ty) => {
                if let Some(new_name) = rename_map.get(&name) {
                    Expr::Var(new_name.clone(), ty)
                } else {
                    Expr::Var(name, ty)
                }
            }
            Expr::Let { pattern, value, ty } => {
                let new_pattern = rename_pattern(*pattern, rename_map);
                Expr::Let {
                    pattern: Box::new(new_pattern),
                    value,
                    ty,
                }
            }
            // Lambda/For shadow — need to filter rename_map
            // Note: transform_expr already walked children with the full
            // rename_map, which is correct because children were walked
            // bottom-up before we get here. The shadowing only affects
            // the body, which transform_expr already recursed into.
            // Since transform_expr is bottom-up, the Var nodes inside
            // were already renamed. No extra handling needed — the
            // transform_expr walk handles this correctly because
            // variable renaming is context-free (rename if in map).
            other => other,
        }
    })
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
    // Use transform_expr for the recursive walk, intercept at CallDirect
    transform_expr(expr, &mut |e| {
        if let Expr::CallDirect { name, args, ty } = &e
            && let Some((params, body)) = inlineable.get(name)
            && params.len() == args.len()
        {
            // Build let bindings with fresh names
            let mut stmts = Vec::new();
            let mut rename_map = HashMap::new();
            for (param, arg) in params.iter().zip(args.iter()) {
                let fresh = format!("__inl_{}_{}", param, *counter);
                *counter += 1;
                rename_map.insert(param.clone(), fresh.clone());
                stmts.push(Expr::Let {
                    pattern: Box::new(Pattern::Var(fresh)),
                    value: Box::new(arg.clone()),
                    ty: None,
                });
            }
            let inlined_body = rename_vars(body.clone(), &rename_map);
            stmts.push(inlined_body);
            return Expr::Block(stmts, ty.clone());
        }
        e
    })
}

/// Structural equality check (for detecting if inlining changed anything).
fn exprs_equal(a: &Expr, b: &Expr) -> bool {
    a == b
}

// ---------------------------------------------------------------------------
// Pass 4: Block simplification
// ---------------------------------------------------------------------------

/// Simplify blocks: remove non-final Unit expressions, flatten nested blocks,
/// unwrap single-element blocks.
fn simplify_blocks(expr: Expr) -> Expr {
    transform_expr(expr, &mut |e| {
        if let Expr::Block(exprs, ty) = e {
            // Flatten nested blocks
            let mut simplified: Vec<Expr> = Vec::new();
            for e in exprs {
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
        } else {
            e
        }
    })
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

/// Count variable references in an expression tree.
fn count_var_uses(expr: &Expr, counts: &mut HashMap<String, usize>) {
    visit_expr(expr, &mut |e| {
        if let Expr::Var(name, _) = e {
            *counts.entry(name.clone()).or_insert(0) += 1;
        }
    });
}

/// Remove dead let bindings (literal-valued, zero references).
fn remove_dead_lets(expr: Expr, counts: &HashMap<String, usize>) -> Expr {
    transform_expr(expr, &mut |e| {
        if let Expr::Let {
            ref pattern,
            ref value,
            ..
        } = e
            && let Pattern::Var(ref name) = **pattern
            && is_literal(value)
            && counts.get(name).copied().unwrap_or(0) == 0
        {
            return Expr::Unit;
        }
        e
    })
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
            tags: TagRegistry::new(),
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
            tags: TagRegistry::new(),
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
            tags: TagRegistry::new(),
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

    #[test]
    fn build_tag_registry_collects_tags() {
        // MakeEnum + match with constructor patterns
        let body = Expr::Block(
            vec![
                Expr::MakeEnum {
                    tag: "MyTag".into(),
                    payload: Box::new(Expr::Int(1)),
                    ty: None,
                },
                Expr::Match {
                    subject: Box::new(Expr::Var("x".into(), None)),
                    arms: vec![
                        MatchArm {
                            pattern: Pattern::Constructor(
                                "Some".into(),
                                vec![Pattern::Var("v".into())],
                            ),
                            guard: None,
                            body: Expr::Int(1),
                        },
                        MatchArm {
                            pattern: Pattern::Constructor("None".into(), vec![]),
                            guard: None,
                            body: Expr::Int(0),
                        },
                    ],
                    ty: None,
                },
            ],
            None,
        );

        let mut module = make_module(body);
        optimize(&mut module);

        // Well-known tags + MyTag should all be registered
        assert!(module.tags.get_id("None").is_some());
        assert!(module.tags.get_id("Some").is_some());
        assert!(module.tags.get_id("MyTag").is_some());
    }

    // -- Lambda lifting tests --

    #[test]
    fn free_vars_simple() {
        use std::collections::HashSet;
        // |x| x + y → free vars = {y}
        let expr = Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(Expr::Var("x".into(), None)),
            rhs: Box::new(Expr::Var("y".into(), None)),
            ty: None,
        };
        let bound: HashSet<String> = ["x".into()].into();
        let top: HashSet<String> = HashSet::new();
        let fvs = super::free_vars(&expr, &bound, &top);
        assert_eq!(fvs, ["y".into()].into());
    }

    #[test]
    fn free_vars_excludes_top_level() {
        use std::collections::HashSet;
        // |x| foo(x) → foo is top-level, not free
        let expr = Expr::CallDirect {
            name: "foo".into(),
            args: vec![Expr::Var("x".into(), None)],
            ty: None,
        };
        let bound: HashSet<String> = ["x".into()].into();
        let top: HashSet<String> = ["foo".into()].into();
        let fvs = super::free_vars(&expr, &bound, &top);
        assert!(fvs.is_empty());
    }

    #[test]
    fn free_vars_nested_let() {
        use std::collections::HashSet;
        // { let a = y; a + z } → free = {y, z}
        let expr = Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("a".into())),
                    value: Box::new(Expr::Var("y".into(), None)),
                    ty: None,
                },
                Expr::BinOp {
                    op: BinOp::Add,
                    lhs: Box::new(Expr::Var("a".into(), None)),
                    rhs: Box::new(Expr::Var("z".into(), None)),
                    ty: None,
                },
            ],
            None,
        );
        let bound: HashSet<String> = HashSet::new();
        let top: HashSet<String> = HashSet::new();
        let fvs = super::free_vars(&expr, &bound, &top);
        assert!(fvs.contains("y"));
        assert!(fvs.contains("z"));
        assert!(!fvs.contains("a")); // bound by let
    }

    #[test]
    fn lift_lambda_no_captures() {
        // main() = let f = |x| x + 1; f
        let body = Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("f".into())),
                    value: Box::new(Expr::Lambda {
                        params: vec!["x".into()],
                        body: Box::new(Expr::BinOp {
                            op: BinOp::Add,
                            lhs: Box::new(Expr::Var("x".into(), None)),
                            rhs: Box::new(Expr::Int(1)),
                            ty: None,
                        }),
                        ty: None,
                    }),
                    ty: None,
                },
                Expr::Var("f".into(), None),
            ],
            None,
        );

        let mut module = make_module(body);
        optimize(&mut module);

        // Should have lifted the lambda into a new function
        assert!(
            module.functions.len() > 1,
            "Expected lifted function, got {} functions",
            module.functions.len()
        );
        // The lifted function should have name starting with __lambda_
        let lifted = module.functions.last().unwrap();
        assert!(lifted.name.starts_with("__lambda_"), "Got: {}", lifted.name);
        // No __closure param (zero captures)
        assert!(
            !lifted.params.contains(&"__closure".to_string()),
            "Zero-capture lambda should not have __closure param"
        );
    }

    #[test]
    fn lift_lambda_with_captures() {
        // main(y) = let f = |x| x + y; f
        // y is a function param, not a literal, so it won't be propagated away
        let body = Expr::Block(
            vec![
                Expr::Let {
                    pattern: Box::new(Pattern::Var("f".into())),
                    value: Box::new(Expr::Lambda {
                        params: vec!["x".into()],
                        body: Box::new(Expr::BinOp {
                            op: BinOp::Add,
                            lhs: Box::new(Expr::Var("x".into(), None)),
                            rhs: Box::new(Expr::Var("y".into(), None)),
                            ty: None,
                        }),
                        ty: None,
                    }),
                    ty: None,
                },
                Expr::Var("f".into(), None),
            ],
            None,
        );

        let mut module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec!["y".into()],
                body,
                ty: None,
                span: dummy_span(),
            }],
            entry: 0,
            tags: TagRegistry::new(),
        };
        optimize(&mut module);

        // Should have lifted function
        assert!(
            module.functions.len() > 1,
            "Expected lifted function, got {} functions",
            module.functions.len()
        );
        let lifted = module.functions.last().unwrap();
        assert!(lifted.name.starts_with("__lambda_"), "Got: {}", lifted.name);
        // Should have __closure as first param
        assert_eq!(lifted.params[0], "__closure");
        // Body should contain GetClosureVar(0) for 'y'
        let mut found_closure_var = false;
        super::visit_expr(&lifted.body, &mut |e| {
            if matches!(e, Expr::GetClosureVar(0)) {
                found_closure_var = true;
            }
        });
        assert!(
            found_closure_var,
            "Expected GetClosureVar(0) in lifted body"
        );
    }
}
