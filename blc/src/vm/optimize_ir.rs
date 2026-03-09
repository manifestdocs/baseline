use std::collections::{HashMap, HashSet};

use super::ir::{
    BinOp, Expr, HandlerClause, IrFunction, IrModule, MatchArm, Matcher, Pattern, Span,
    TagRegistry, UnaryOp,
};

/// Run all IR optimization passes on the module, including lambda lifting
/// (required for JIT codegen).
pub fn optimize(module: &mut IrModule) {
    optimize_inner(module, true);
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
    // Tuple-let fusion (after simplify_blocks flattens inlined bodies)
    for func in &mut module.functions {
        func.body = fuse_tuple_lets(func.body.clone());
        func.body = simplify_blocks(func.body.clone());
    }
    // Final: propagation + folding again (to fold across simplified blocks)
    for func in &mut module.functions {
        func.body = propagate_and_fold(func.body.clone());
        func.body = eliminate_dead_lets(func.body.clone());
        func.body = simplify_blocks(func.body.clone());
    }
    if lift {
        // Evidence passing: transform tail-resumptive effect handlers into
        // MakeRecord/GetField/CallIndirect so the JIT can compile them.
        evidence_transform(module);
        // Lambda lifting: transform Lambda nodes into MakeClosure + lifted functions
        lift_lambdas(module);
        // Tail recursion modulo constructor (strict first pass).
        apply_trmc_transform(module);
        // Perceus reuse analysis: insert Drop/Reuse for match-destructure-reconstruct
        insert_drop_reuse(module);
    }
    // Build tag registry: assign integer IDs to all enum tags
    build_tag_registry(module);

    // IR dump: print optimized IR when BASELINE_DUMP_IR=1
    if std::env::var("BASELINE_DUMP_IR").is_ok() {
        eprintln!("=== Optimized IR ===");
        for func in &module.functions {
            eprintln!("--- fn {} ({}) ---", func.name, func.params.join(", "));
            eprintln!("{:#?}", func.body);
            eprintln!();
        }
        eprintln!("=== End IR ===");
    }
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
                param_types: vec![],
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
// Evidence Passing Transform (effects → records + indirect calls)
// ---------------------------------------------------------------------------

/// Context carried through the evidence transform.
struct EvidenceCtx {
    /// The evidence variable currently in scope (e.g. "__ev_0").
    current_ev: Option<String>,
    /// Module-global counter for unique evidence variable names.
    ev_counter: usize,
    /// Set of function names that need an __ev parameter prepended.
    needs_evidence: HashSet<String>,
}

impl EvidenceCtx {
    fn fresh_ev(&mut self) -> String {
        let name = format!("__ev_{}", self.ev_counter);
        self.ev_counter += 1;
        name
    }
}

/// Determine which functions need an `__ev` evidence parameter.
///
/// 1. Functions whose body contains `PerformEffect` are directly effectful.
/// 2. Functions that call (via CallDirect/TailCall) a directly effectful
///    function are transitively effectful.
/// 3. Fixed-point iteration propagates until stable.
///
/// Functions containing `HandleEffect` where ALL clauses are tail-resumptive
/// provide their own evidence — their body may need evidence internally, but
/// the handler itself creates it. These are handled during the transform.
fn compute_needs_evidence(module: &IrModule) -> HashSet<String> {
    let func_names: HashMap<String, usize> = module
        .functions
        .iter()
        .enumerate()
        .map(|(i, f)| (f.name.clone(), i))
        .collect();

    // Step 1: Mark functions that directly contain PerformEffect.
    let mut needs: Vec<bool> = module
        .functions
        .iter()
        .map(|f| expr_has_perform(&f.body))
        .collect();

    // Step 2: Build call graph (indices of callees for each function).
    let call_graph: Vec<Vec<usize>> = module
        .functions
        .iter()
        .map(|f| {
            let mut callees = Vec::new();
            collect_call_targets(&f.body, &mut callees);
            callees
                .iter()
                .filter_map(|n| func_names.get(n).copied())
                .collect()
        })
        .collect();

    // Step 3: Fixed-point — propagate evidence needs through call graph.
    loop {
        let mut changed = false;
        for (i, callees) in call_graph.iter().enumerate() {
            if needs[i] {
                continue;
            }
            for &callee_idx in callees {
                if needs[callee_idx] {
                    needs[i] = true;
                    changed = true;
                    break;
                }
            }
        }
        if !changed {
            break;
        }
    }

    needs
        .into_iter()
        .enumerate()
        .filter(|(_, b)| *b)
        .map(|(i, _)| module.functions[i].name.clone())
        .collect()
}

/// Check if an expression contains any *unhandled* PerformEffect node.
///
/// PerformEffect nodes inside a tail-resumptive HandleEffect that handles
/// them are considered handled and don't count. Only PerformEffect nodes
/// that escape their handler (or have no handler) make the function effectful.
fn expr_has_perform(expr: &Expr) -> bool {
    let mut handled: HashSet<String> = HashSet::new();
    expr_has_unhandled_perform(expr, &mut handled)
}

fn expr_has_unhandled_perform(expr: &Expr, handled: &mut HashSet<String>) -> bool {
    match expr {
        Expr::PerformEffect {
            effect,
            method,
            args,
            ..
        } => {
            let key = format!("{}.{}", effect, method);
            if handled.contains(&key) {
                // This effect is handled by an enclosing HandleEffect.
                // Still check args for unhandled performs.
                args.iter().any(|a| expr_has_unhandled_perform(a, handled))
            } else {
                true
            }
        }
        Expr::HandleEffect { body, clauses } => {
            // Both tail-resumptive and non-tail-resumptive handlers handle
            // their effects (tail-resumptive via evidence passing, non-tail
            // via fiber transform). Add handled effects to scope.
            let mut added = Vec::new();
            for c in clauses {
                let key = format!("{}.{}", c.effect, c.method);
                if handled.insert(key.clone()) {
                    added.push(key);
                }
            }
            let result = expr_has_unhandled_perform(body, handled);
            // Restore handled set.
            for key in added {
                handled.remove(&key);
            }
            result
        }
        Expr::WithHandlers { handlers, body } => {
            // WithHandlers provides evidence for its effects.
            // For now, just check the body — WithHandlers doesn't suppress
            // PerformEffect detection because it's a different mechanism.
            let _ = handlers;
            expr_has_unhandled_perform(body, handled)
        }
        _ => {
            let mut found = false;
            visit_immediate_children(expr, &mut |child| {
                if !found && expr_has_unhandled_perform(child, handled) {
                    found = true;
                }
            });
            found
        }
    }
}

/// Collect function names targeted by CallDirect and TailCall.
fn collect_call_targets(expr: &Expr, out: &mut Vec<String>) {
    visit_expr(expr, &mut |e| match e {
        Expr::CallDirect { name, .. } | Expr::TailCall { name, .. } => {
            out.push(name.clone());
        }
        _ => {}
    });
}

/// Run the evidence-passing transform on the module.
///
/// Converts tail-resumptive HandleEffect, WithHandlers, and PerformEffect
/// nodes into ordinary IR constructs (MakeRecord, GetField, CallIndirect,
/// Lambda). After this pass, all effect nodes in JIT-eligible functions are
/// eliminated. Non-tail-resumptive handlers are left untouched (can_jit
/// rejects them → VM fallback).
fn evidence_transform(module: &mut IrModule) {
    // Check if there are any effect nodes at all.
    let has_any_effects = module
        .functions
        .iter()
        .any(|f| expr_has_effect_node(&f.body));
    if !has_any_effects {
        return;
    }

    let needs = compute_needs_evidence(module);

    let mut ctx = EvidenceCtx {
        current_ev: None,
        ev_counter: 0,
        needs_evidence: needs,
    };

    // Prepend __ev parameter to effectful function signatures.
    for func in &mut module.functions {
        if ctx.needs_evidence.contains(&func.name) {
            func.params.insert(0, "__ev".to_string());
        }
    }

    // Transform each function body.
    for i in 0..module.functions.len() {
        let has_ev = ctx.needs_evidence.contains(&module.functions[i].name);
        ctx.current_ev = if has_ev {
            Some("__ev".to_string())
        } else {
            None
        };
        let body = module.functions[i].body.clone();
        module.functions[i].body = transform_evidence_expr(body, &mut ctx);
    }
}

/// Check if an expression contains any effect-related node.
fn expr_has_effect_node(expr: &Expr) -> bool {
    let mut found = false;
    visit_expr(expr, &mut |e| {
        if found {
            return;
        }
        if matches!(
            e,
            Expr::HandleEffect { .. } | Expr::PerformEffect { .. } | Expr::WithHandlers { .. }
        ) {
            found = true;
        }
    });
    found
}

/// Recursively transform an expression, eliminating effect nodes.
fn transform_evidence_expr(expr: Expr, ctx: &mut EvidenceCtx) -> Expr {
    match expr {
        // -----------------------------------------------------------------
        // HandleEffect: all-tail-resumptive → build evidence record
        // -----------------------------------------------------------------
        Expr::HandleEffect { body, clauses } => {
            let all_tail_resumptive = clauses.iter().all(|c| c.is_tail_resumptive);
            if !all_tail_resumptive {
                // Non-tail-resumptive handler: transform to fiber-based dispatch.
                // 1. Body → Lambda with PerformEffect replaced by __fiber.perform
                // 2. Handler clauses → Lambdas with resume() replaced by __handler.resume
                // 3. Wrap in __fiber.run_handler(body_lambda, keys_list, handlers_list)
                return transform_fiber_handler(*body, clauses, ctx);
            }

            let ev_name = ctx.fresh_ev();
            let mut block = Vec::new();

            // Build handler lambdas and evidence record fields.
            let mut fields: Vec<(String, Expr)> = Vec::new();
            for clause in &clauses {
                let field_key = format!("{}.{}", clause.effect, clause.method);
                // Strip the resume(expr) wrapper to get just the inner expr.
                let stripped_body = strip_tail_resume_owned(&clause.body);
                let lambda = Expr::Lambda {
                    params: clause
                        .params
                        .iter()
                        .filter(|p| *p != "resume")
                        .cloned()
                        .collect(),
                    body: Box::new(stripped_body),
                    ty: None,
                };
                fields.push((field_key, lambda));
            }

            // Build evidence record, merging with parent evidence if present.
            let record_expr = if let Some(ref parent_ev) = ctx.current_ev {
                // Nested handler: merge with parent evidence.
                Expr::UpdateRecord {
                    base: Box::new(Expr::Var(parent_ev.clone(), None)),
                    updates: fields,
                    ty: None,
                }
            } else {
                Expr::MakeRecord(fields, None)
            };

            block.push(Expr::Let {
                pattern: Box::new(Pattern::Var(ev_name.clone())),
                value: Box::new(record_expr),
                ty: None,
            });

            // Transform body with this new evidence in scope.
            let prev_ev = ctx.current_ev.replace(ev_name.clone());
            let transformed_body = transform_evidence_expr(*body, ctx);
            ctx.current_ev = prev_ev;

            block.push(transformed_body);
            Expr::Block(block, None)
        }

        // -----------------------------------------------------------------
        // WithHandlers: build evidence record from handler expressions
        // -----------------------------------------------------------------
        Expr::WithHandlers { handlers, body } => {
            let ev_name = ctx.fresh_ev();
            let mut block = Vec::new();

            // Build evidence fields from handler expressions.
            let mut all_fields: Vec<(String, Expr)> = Vec::new();
            for (effect_name, methods) in handlers {
                if methods.len() == 1 && methods[0].0 == "__record__" {
                    // `with { Effect: handler_expr }` — handler_expr is a record.
                    // We need to wrap each method access. For now store the whole
                    // record keyed by effect name, and GetField will use
                    // "Effect.method" keys after we flatten.
                    // Since WithHandlers stores an entire handler record, store it
                    // and let PerformEffect do a two-level lookup or store flat.
                    // Simpler: store as flat fields using a temp.
                    let handler_tmp = format!("__ev_wh_{}", effect_name);
                    let handler_expr =
                        transform_evidence_expr(methods.into_iter().next().unwrap().1, ctx);
                    block.push(Expr::Let {
                        pattern: Box::new(Pattern::Var(handler_tmp.clone())),
                        value: Box::new(handler_expr),
                        ty: None,
                    });
                    // The handler is a record with method keys. Store it under
                    // effect name for lookup in PerformEffect.
                    all_fields.push((effect_name, Expr::Var(handler_tmp, None)));
                } else {
                    // Individual method entries
                    for (method_key, handler_fn) in methods {
                        let field_key = format!("{}.{}", effect_name, method_key);
                        let handler = transform_evidence_expr(handler_fn, ctx);
                        all_fields.push((field_key, handler));
                    }
                }
            }

            let record_expr = if let Some(ref parent_ev) = ctx.current_ev {
                Expr::UpdateRecord {
                    base: Box::new(Expr::Var(parent_ev.clone(), None)),
                    updates: all_fields,
                    ty: None,
                }
            } else {
                Expr::MakeRecord(all_fields, None)
            };

            block.push(Expr::Let {
                pattern: Box::new(Pattern::Var(ev_name.clone())),
                value: Box::new(record_expr),
                ty: None,
            });

            let prev_ev = ctx.current_ev.replace(ev_name.clone());
            let transformed_body = transform_evidence_expr(*body, ctx);
            ctx.current_ev = prev_ev;

            block.push(transformed_body);
            Expr::Block(block, None)
        }

        // -----------------------------------------------------------------
        // PerformEffect: look up handler in evidence record and call it
        // -----------------------------------------------------------------
        Expr::PerformEffect {
            effect,
            method,
            args,
            ty,
        } => {
            let field_key = format!("{}.{}", effect, method);
            let ev_var = ctx
                .current_ev
                .as_ref()
                .expect("PerformEffect without evidence in scope")
                .clone();

            let args = args
                .into_iter()
                .map(|a| transform_evidence_expr(a, ctx))
                .collect();

            let tmp = format!("__ev_fn_{}_{}", effect, method);
            Expr::Block(
                vec![
                    Expr::Let {
                        pattern: Box::new(Pattern::Var(tmp.clone())),
                        value: Box::new(Expr::GetField {
                            object: Box::new(Expr::Var(ev_var, None)),
                            field: field_key,
                            field_idx: None,
                            ty: None,
                        }),
                        ty: None,
                    },
                    Expr::CallIndirect {
                        callee: Box::new(Expr::Var(tmp, None)),
                        args,
                        ty,
                    },
                ],
                None,
            )
        }

        // -----------------------------------------------------------------
        // CallDirect / TailCall to effectful function: prepend evidence arg
        // -----------------------------------------------------------------
        Expr::CallDirect { name, args, ty } => {
            let args: Vec<Expr> = args
                .into_iter()
                .map(|a| transform_evidence_expr(a, ctx))
                .collect();
            if ctx.needs_evidence.contains(&name) {
                let ev_var = ctx
                    .current_ev
                    .as_ref()
                    .expect("CallDirect to effectful function without evidence")
                    .clone();
                let mut new_args = vec![Expr::Var(ev_var, None)];
                new_args.extend(args);
                Expr::CallDirect {
                    name,
                    args: new_args,
                    ty,
                }
            } else {
                Expr::CallDirect { name, args, ty }
            }
        }

        Expr::TailCall { name, args, ty } => {
            let args: Vec<Expr> = args
                .into_iter()
                .map(|a| transform_evidence_expr(a, ctx))
                .collect();
            if ctx.needs_evidence.contains(&name) {
                let ev_var = ctx
                    .current_ev
                    .as_ref()
                    .expect("TailCall to effectful function without evidence")
                    .clone();
                let mut new_args = vec![Expr::Var(ev_var, None)];
                new_args.extend(args);
                Expr::TailCall {
                    name,
                    args: new_args,
                    ty,
                }
            } else {
                Expr::TailCall { name, args, ty }
            }
        }

        // -----------------------------------------------------------------
        // All other nodes: recurse into children
        // -----------------------------------------------------------------
        other => walk_expr_children(other, |child| transform_evidence_expr(child, ctx)),
    }
}

/// Strip `resume(expr)` wrapper from a tail-resumptive handler body,
/// returning an owned clone of the inner expression.
fn strip_tail_resume_owned(expr: &Expr) -> Expr {
    match expr {
        Expr::CallIndirect { callee, args, .. } | Expr::TailCallIndirect { callee, args, .. }
            if matches!(callee.as_ref(), Expr::Var(name, _) if name == "resume")
                && args.len() == 1 =>
        {
            args[0].clone()
        }
        Expr::Block(stmts, _) if stmts.len() == 1 => strip_tail_resume_owned(&stmts[0]),
        _ => expr.clone(),
    }
}

// ---------------------------------------------------------------------------
// Fiber-based handler transform (non-tail-resumptive)
// ---------------------------------------------------------------------------

/// Transform a non-tail-resumptive HandleEffect into a fiber-based dispatch.
///
/// ```text
/// handle { body } with { E.m!(msg, resume) -> { let r = resume(()); work; r } }
/// ```
/// becomes:
/// ```text
/// __fiber.run_handler(
///     Lambda { body_with_performs_replaced },
///     ["E.m", ...],
///     [Lambda { handler_clause_with_resume_replaced }, ...]
/// )
/// ```
fn transform_fiber_handler(body: Expr, clauses: Vec<HandlerClause>, ctx: &mut EvidenceCtx) -> Expr {
    // Collect effect keys handled by these clauses, so we know which
    // PerformEffect nodes in the body to replace.
    let handled_keys: HashSet<String> = clauses
        .iter()
        .map(|c| format!("{}.{}", c.effect, c.method))
        .collect();

    // 1. Transform body: replace PerformEffect with CallNative("__fiber.perform")
    let transformed_body = replace_perform_in_body(body, &handled_keys, ctx);
    let body_lambda = Expr::Lambda {
        params: vec![],
        body: Box::new(transformed_body),
        ty: None,
    };

    // 2. Build handler keys and handler lambdas
    let mut key_exprs = Vec::new();
    let mut handler_exprs = Vec::new();

    for clause in clauses {
        let field_key = format!("{}.{}", clause.effect, clause.method);
        key_exprs.push(Expr::String(field_key));

        // Transform handler clause body: replace resume(val) with
        // CallNative("__handler.resume", [val])
        let transformed_clause = replace_resume_in_clause(clause.body, ctx);
        // Handler lambda params = clause.params (the effect operation args,
        // NOT including resume — resume is replaced with __handler.resume)
        let handler_lambda = Expr::Lambda {
            params: clause
                .params
                .into_iter()
                .filter(|p| p != "resume")
                .collect(),
            body: Box::new(transformed_clause),
            ty: None,
        };
        handler_exprs.push(handler_lambda);
    }

    // 3. Build the CallNative: __fiber.run_handler(body, keys_list, handlers_list)
    Expr::CallNative {
        module: "__fiber".to_string(),
        method: "run_handler".to_string(),
        args: vec![
            body_lambda,
            Expr::MakeList(key_exprs, None),
            Expr::MakeList(handler_exprs, None),
        ],
        ty: None,
    }
}

/// Replace PerformEffect nodes in a handler body with CallNative("__fiber.perform").
/// Only replaces performs for effects in `handled_keys` (this handler's clauses).
/// Nested HandleEffect/WithHandlers that handle the same effects shadow them.
fn replace_perform_in_body(
    expr: Expr,
    handled_keys: &HashSet<String>,
    ctx: &mut EvidenceCtx,
) -> Expr {
    match expr {
        Expr::PerformEffect {
            effect,
            method,
            args,
            ty,
        } => {
            let key = format!("{}.{}", effect, method);
            if handled_keys.contains(&key) {
                // This perform is handled by our fiber handler → replace with fiber yield
                let transformed_args: Vec<Expr> = args
                    .into_iter()
                    .map(|a| replace_perform_in_body(a, handled_keys, ctx))
                    .collect();
                let mut native_args = vec![Expr::String(key)];
                native_args.extend(transformed_args);
                Expr::CallNative {
                    module: "__fiber".to_string(),
                    method: "perform".to_string(),
                    args: native_args,
                    ty,
                }
            } else if ctx.current_ev.is_some() {
                // Not handled by us, but evidence is in scope — use evidence transform
                let args = args
                    .into_iter()
                    .map(|a| replace_perform_in_body(a, handled_keys, ctx))
                    .collect();
                transform_evidence_expr(
                    Expr::PerformEffect {
                        effect,
                        method,
                        args,
                        ty,
                    },
                    ctx,
                )
            } else {
                // No evidence in scope — leave as PerformEffect for an outer
                // fiber handler to pick up during its own transform pass.
                let args = args
                    .into_iter()
                    .map(|a| replace_perform_in_body(a, handled_keys, ctx))
                    .collect();
                Expr::PerformEffect {
                    effect,
                    method,
                    args,
                    ty,
                }
            }
        }
        // Nested HandleEffect shadows some keys — don't replace those
        Expr::HandleEffect { body, clauses } => {
            let nested_all_tail = clauses.iter().all(|c| c.is_tail_resumptive);
            if nested_all_tail {
                // Nested tail-resumptive: use normal evidence transform
                transform_evidence_expr(Expr::HandleEffect { body, clauses }, ctx)
            } else {
                // Nested non-tail-resumptive: recurse with fiber transform
                // The nested handler will get its own fiber.
                let nested_handled: HashSet<String> = clauses
                    .iter()
                    .map(|c| format!("{}.{}", c.effect, c.method))
                    .collect();
                let mut remaining_keys = handled_keys.clone();
                for k in &nested_handled {
                    remaining_keys.remove(k);
                }
                let body = replace_perform_in_body(*body, &remaining_keys, ctx);
                let clauses = clauses
                    .into_iter()
                    .map(|c| HandlerClause {
                        body: replace_perform_in_body(c.body, &remaining_keys, ctx),
                        ..c
                    })
                    .collect();
                transform_fiber_handler(body, clauses, ctx)
            }
        }
        // Recurse into children
        other => walk_expr_children(other, |child| {
            replace_perform_in_body(child, handled_keys, ctx)
        }),
    }
}

/// Replace `resume(val)` calls in a handler clause body with
/// `CallNative("__handler.resume", [val])`.
fn replace_resume_in_clause(expr: Expr, ctx: &mut EvidenceCtx) -> Expr {
    match expr {
        // resume(val) → __handler.resume(val)
        Expr::CallIndirect { callee, args, ty } if matches!(callee.as_ref(), Expr::Var(name, _) if name == "resume") =>
        {
            let transformed_args: Vec<Expr> = args
                .into_iter()
                .map(|a| replace_resume_in_clause(a, ctx))
                .collect();
            Expr::CallNative {
                module: "__handler".to_string(),
                method: "resume".to_string(),
                args: transformed_args,
                ty,
            }
        }
        // TailCallIndirect to resume → also replace
        Expr::TailCallIndirect { callee, args, ty } if matches!(callee.as_ref(), Expr::Var(name, _) if name == "resume") =>
        {
            let transformed_args: Vec<Expr> = args
                .into_iter()
                .map(|a| replace_resume_in_clause(a, ctx))
                .collect();
            Expr::CallNative {
                module: "__handler".to_string(),
                method: "resume".to_string(),
                args: transformed_args,
                ty,
            }
        }
        // Recurse into children
        other => walk_expr_children(other, |child| replace_resume_in_clause(child, ctx)),
    }
}

// ---------------------------------------------------------------------------
// Perceus Reuse Analysis: Drop/Reuse insertion
// ---------------------------------------------------------------------------

/// Insert Drop/Reuse nodes for match-destructure-reconstruct patterns.
///
/// Scans each function for `Match { subject: Var(name), arms }` where `name`
/// has exactly 1 use (this match). For each arm that reconstructs a heap
/// value (MakeEnum/MakeStruct/MakeRecord/MakeTuple), wraps the arm body in
/// `Drop { name, token, body: Reuse { token, alloc } }` to enable in-place
/// allocation reuse.
fn insert_drop_reuse(module: &mut IrModule) {
    let mut token_counter: usize = 0;
    for func in &mut module.functions {
        let mut counts: HashMap<String, usize> = HashMap::new();
        count_var_uses(&func.body, &mut counts);
        func.body = insert_reuse_in_expr(func.body.clone(), &counts, &mut token_counter);
    }
}

// ---------------------------------------------------------------------------
// Tail recursion modulo constructor (strict pass)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct TrmcCandidate {
    subject_name: String,
    recursive_arm_index: usize,
    constructor_tag: String,
    non_recursive_fields: Vec<Expr>,
    recursive_args: Vec<Expr>,
    recursive_prefix: Vec<Expr>,
}

#[derive(Debug, Clone)]
struct TrmcRewrite {
    wrapper_body: Expr,
    loop_fn: IrFunction,
    build_fn: IrFunction,
}

fn apply_trmc_transform(module: &mut IrModule) {
    let mut used_fn_names: HashSet<String> =
        module.functions.iter().map(|f| f.name.clone()).collect();
    let mut generated: Vec<IrFunction> = Vec::new();

    for func in &mut module.functions {
        let Some(rewrite) = try_trmc_rewrite(func, &used_fn_names) else {
            continue;
        };

        used_fn_names.insert(rewrite.loop_fn.name.clone());
        used_fn_names.insert(rewrite.build_fn.name.clone());
        func.body = rewrite.wrapper_body;
        generated.push(rewrite.loop_fn);
        generated.push(rewrite.build_fn);
    }

    module.functions.extend(generated);
}

fn try_trmc_rewrite(func: &IrFunction, used_fn_names: &HashSet<String>) -> Option<TrmcRewrite> {
    let candidate = detect_trmc_candidate(func)?;
    if candidate.recursive_args.len() != func.params.len() {
        return None;
    }

    let loop_name = format!("{}__trmc_loop", func.name);
    let build_name = format!("{}__trmc_build", func.name);
    if used_fn_names.contains(&loop_name) || used_fn_names.contains(&build_name) {
        return None;
    }

    let suffix = sanitize_symbol_name(&func.name);
    let frame_nil_tag = format!("__trmc_nil_{}", suffix);
    let frame_cons_tag = format!("__trmc_cons_{}", suffix);

    let frames_param = "__trmc_frames".to_string();
    let acc_param = "__trmc_acc".to_string();
    let token_name = "__trmc_token".to_string();

    let wrapper_body = make_trmc_wrapper_body(func, &loop_name, &frame_nil_tag);
    let loop_fn = make_trmc_loop_function(
        func,
        &candidate,
        &loop_name,
        &build_name,
        &frame_cons_tag,
        &frames_param,
        &token_name,
    )?;
    let build_fn = make_trmc_build_function(
        func,
        &candidate,
        &build_name,
        &frame_nil_tag,
        &frame_cons_tag,
        &frames_param,
        &acc_param,
    );

    Some(TrmcRewrite {
        wrapper_body,
        loop_fn,
        build_fn,
    })
}

fn sanitize_symbol_name(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() { "_".to_string() } else { out }
}

fn make_trmc_wrapper_body(func: &IrFunction, loop_name: &str, frame_nil_tag: &str) -> Expr {
    let mut args = Vec::with_capacity(func.params.len() + 1);
    args.push(Expr::MakeEnum {
        tag: frame_nil_tag.to_string(),
        payload: Box::new(Expr::Unit),
        ty: None,
    });
    args.extend(param_vars(func));
    Expr::CallDirect {
        name: loop_name.to_string(),
        args,
        ty: func.ty.clone(),
    }
}

fn make_trmc_loop_function(
    func: &IrFunction,
    candidate: &TrmcCandidate,
    loop_name: &str,
    build_name: &str,
    frame_cons_tag: &str,
    frames_param: &str,
    token_name: &str,
) -> Option<IrFunction> {
    let Expr::Match { subject, arms, ty } = &func.body else {
        return None;
    };

    let mut rewritten_arms = Vec::with_capacity(arms.len());
    for (idx, arm) in arms.iter().cloned().enumerate() {
        if idx == candidate.recursive_arm_index {
            let recursive_body = make_trmc_recursive_arm_body(
                &candidate.subject_name,
                loop_name,
                frame_cons_tag,
                frames_param,
                token_name,
                &candidate.non_recursive_fields,
                &candidate.recursive_args,
                &candidate.recursive_prefix,
                &func.ty,
            );
            rewritten_arms.push(MatchArm {
                pattern: arm.pattern,
                guard: arm.guard,
                body: recursive_body,
            });
        } else {
            rewritten_arms.push(MatchArm {
                pattern: arm.pattern,
                guard: arm.guard,
                body: Expr::TailCall {
                    name: build_name.to_string(),
                    args: vec![Expr::Var(frames_param.to_string(), None), arm.body],
                    ty: func.ty.clone(),
                },
            });
        }
    }

    let mut params = Vec::with_capacity(func.params.len() + 1);
    params.push(frames_param.to_string());
    params.extend(func.params.iter().cloned());

    let mut param_types = Vec::with_capacity(func.param_types.len() + 1);
    param_types.push(None);
    param_types.extend(func.param_types.iter().cloned());

    Some(IrFunction {
        name: loop_name.to_string(),
        params,
        body: Expr::Match {
            subject: subject.clone(),
            arms: rewritten_arms,
            ty: ty.clone(),
        },
        ty: func.ty.clone(),
        param_types,
        span: func.span.clone(),
    })
}

fn make_trmc_recursive_arm_body(
    subject_name: &str,
    loop_name: &str,
    frame_cons_tag: &str,
    frames_param: &str,
    token_name: &str,
    non_recursive_fields: &[Expr],
    recursive_args: &[Expr],
    recursive_prefix: &[Expr],
    ret_ty: &Option<crate::analysis::types::Type>,
) -> Expr {
    let mut stmts = recursive_prefix.to_vec();

    let mut frame_field_vars = Vec::with_capacity(non_recursive_fields.len());
    for (idx, field_expr) in non_recursive_fields.iter().enumerate() {
        let tmp = format!("__trmc_field_{}", idx);
        frame_field_vars.push(tmp.clone());
        stmts.push(Expr::Let {
            pattern: Box::new(Pattern::Var(tmp.clone())),
            value: Box::new(field_expr.clone()),
            ty: None,
        });
    }

    let mut recursive_arg_vars = Vec::with_capacity(recursive_args.len());
    for (idx, arg_expr) in recursive_args.iter().enumerate() {
        let tmp = format!("__trmc_arg_{}", idx);
        recursive_arg_vars.push(tmp.clone());
        stmts.push(Expr::Let {
            pattern: Box::new(Pattern::Var(tmp.clone())),
            value: Box::new(arg_expr.clone()),
            ty: None,
        });
    }

    let mut frame_items = Vec::with_capacity(frame_field_vars.len() + 2);
    frame_items.extend(
        frame_field_vars
            .iter()
            .map(|name| Expr::Var(name.clone(), None)),
    );
    frame_items.push(Expr::Var(token_name.to_string(), None));
    frame_items.push(Expr::Var(frames_param.to_string(), None));

    let frame_push = Expr::MakeEnum {
        tag: frame_cons_tag.to_string(),
        payload: Box::new(make_payload_expr(frame_items)),
        ty: None,
    };

    let mut tail_args = Vec::with_capacity(recursive_arg_vars.len() + 1);
    tail_args.push(frame_push);
    tail_args.extend(
        recursive_arg_vars
            .iter()
            .map(|name| Expr::Var(name.clone(), None)),
    );
    let tail = Expr::TailCall {
        name: loop_name.to_string(),
        args: tail_args,
        ty: ret_ty.clone(),
    };

    stmts.push(Expr::Drop {
        name: subject_name.to_string(),
        token: Some(token_name.to_string()),
        body: Box::new(tail),
    });
    Expr::Block(stmts, None)
}

fn make_trmc_build_function(
    func: &IrFunction,
    candidate: &TrmcCandidate,
    build_name: &str,
    frame_nil_tag: &str,
    frame_cons_tag: &str,
    frames_param: &str,
    acc_param: &str,
) -> IrFunction {
    let rest_name = "__trmc_rest".to_string();
    let token_name = "__trmc_token".to_string();
    let field_names: Vec<String> = (0..candidate.non_recursive_fields.len())
        .map(|idx| format!("__trmc_field_{}", idx))
        .collect();

    let mut cons_patterns = Vec::with_capacity(field_names.len() + 2);
    cons_patterns.extend(field_names.iter().cloned().map(Pattern::Var));
    cons_patterns.push(Pattern::Var(token_name.clone()));
    cons_patterns.push(Pattern::Var(rest_name.clone()));

    let mut rebuilt_fields: Vec<Expr> = field_names
        .iter()
        .cloned()
        .map(|name| Expr::Var(name, None))
        .collect();
    rebuilt_fields.push(Expr::Var(acc_param.to_string(), func.ty.clone()));

    let rebuilt_alloc = Expr::MakeEnum {
        tag: candidate.constructor_tag.clone(),
        payload: Box::new(make_payload_expr(rebuilt_fields)),
        ty: None,
    };

    let reuse_acc = Expr::Reuse {
        token: token_name,
        alloc: Box::new(rebuilt_alloc),
    };

    let body = Expr::Match {
        subject: Box::new(Expr::Var(frames_param.to_string(), None)),
        arms: vec![
            MatchArm {
                pattern: Pattern::Constructor(frame_nil_tag.to_string(), vec![]),
                guard: None,
                body: Expr::Var(acc_param.to_string(), func.ty.clone()),
            },
            MatchArm {
                pattern: Pattern::Constructor(frame_cons_tag.to_string(), cons_patterns),
                guard: None,
                body: Expr::TailCall {
                    name: build_name.to_string(),
                    args: vec![Expr::Var(rest_name, None), reuse_acc],
                    ty: func.ty.clone(),
                },
            },
        ],
        ty: func.ty.clone(),
    };

    IrFunction {
        name: build_name.to_string(),
        params: vec![frames_param.to_string(), acc_param.to_string()],
        body,
        ty: func.ty.clone(),
        param_types: vec![None, func.ty.clone()],
        span: func.span.clone(),
    }
}

fn make_payload_expr(mut fields: Vec<Expr>) -> Expr {
    match fields.len() {
        0 => Expr::Unit,
        1 => fields.pop().unwrap(),
        _ => Expr::MakeTuple(fields, None),
    }
}

fn param_vars(func: &IrFunction) -> Vec<Expr> {
    func.params
        .iter()
        .enumerate()
        .map(|(idx, name)| {
            let ty = func.param_types.get(idx).cloned().unwrap_or(None);
            Expr::Var(name.clone(), ty)
        })
        .collect()
}

fn detect_trmc_candidate(func: &IrFunction) -> Option<TrmcCandidate> {
    let Expr::Match { subject, arms, .. } = &func.body else {
        return None;
    };
    let Expr::Var(subject_name, _) = subject.as_ref() else {
        return None;
    };

    let mut counts: HashMap<String, usize> = HashMap::new();
    count_var_uses(&func.body, &mut counts);
    if counts.get(subject_name).copied().unwrap_or(0) != 1 {
        return None;
    }

    let mut recursive_arm_index = None;
    let mut recursive_arm = None;

    for (idx, arm) in arms.iter().enumerate() {
        if !references_function(&arm.body, &func.name) {
            continue;
        }

        let Pattern::Constructor(pattern_tag, pattern_fields) = &arm.pattern else {
            return None;
        };
        let info = analyze_trmc_recursive_arm(&arm.body, &func.name)?;
        if info.constructor_tag != *pattern_tag {
            return None;
        }
        if pattern_fields.len() != info.non_recursive_fields.len() + 1 {
            return None;
        }

        if recursive_arm_index.is_some() {
            // Strict first pass: only one recursive constructor arm.
            return None;
        }
        recursive_arm_index = Some(idx);
        recursive_arm = Some(info);
    }

    let recursive_arm_index = recursive_arm_index?;
    let recursive_arm = recursive_arm?;

    for (idx, arm) in arms.iter().enumerate() {
        if idx != recursive_arm_index && references_function(&arm.body, &func.name) {
            return None;
        }
    }

    Some(TrmcCandidate {
        subject_name: subject_name.clone(),
        recursive_arm_index,
        constructor_tag: recursive_arm.constructor_tag,
        non_recursive_fields: recursive_arm.non_recursive_fields,
        recursive_args: recursive_arm.recursive_args,
        recursive_prefix: recursive_arm.prefix_exprs,
    })
}

#[derive(Debug, Clone)]
struct TrmcRecursiveArmInfo {
    constructor_tag: String,
    non_recursive_fields: Vec<Expr>,
    recursive_args: Vec<Expr>,
    prefix_exprs: Vec<Expr>,
}

fn analyze_trmc_recursive_arm(body: &Expr, func_name: &str) -> Option<TrmcRecursiveArmInfo> {
    let (prefix_exprs, terminal) = split_terminal_expr(body.clone());
    if prefix_exprs
        .iter()
        .any(|expr| references_function(expr, func_name))
    {
        return None;
    }

    let Expr::MakeEnum { tag, payload, .. } = terminal else {
        return None;
    };
    let fields = enum_payload_fields(*payload);
    if fields.is_empty() {
        return None;
    }

    let recursive_field = fields.last().cloned().unwrap();
    let non_recursive_fields = fields[..fields.len() - 1].to_vec();
    if non_recursive_fields
        .iter()
        .any(|expr| references_function(expr, func_name))
    {
        return None;
    }

    let recursive_args = match recursive_field {
        Expr::CallDirect { name, args, .. } if name == func_name => args,
        Expr::TailCall { name, args, .. } if name == func_name => args,
        _ => return None,
    };
    if recursive_args
        .iter()
        .any(|expr| references_function(expr, func_name))
    {
        return None;
    }

    Some(TrmcRecursiveArmInfo {
        constructor_tag: tag,
        non_recursive_fields,
        recursive_args,
        prefix_exprs,
    })
}

fn split_terminal_expr(body: Expr) -> (Vec<Expr>, Expr) {
    match body {
        Expr::Block(mut exprs, _) if !exprs.is_empty() => {
            let terminal = exprs.pop().unwrap();
            (exprs, terminal)
        }
        other => (Vec::new(), other),
    }
}

fn enum_payload_fields(payload: Expr) -> Vec<Expr> {
    match payload {
        Expr::MakeTuple(items, _) => items,
        Expr::Unit => Vec::new(),
        other => vec![other],
    }
}

/// Recursively walk an expression, inserting Drop/Reuse where appropriate.
fn insert_reuse_in_expr(expr: Expr, counts: &HashMap<String, usize>, counter: &mut usize) -> Expr {
    match expr {
        Expr::Match { subject, arms, ty } => {
            // Check if subject is a single-use variable
            let reuse_name = if let Expr::Var(ref name, _) = *subject {
                if counts.get(name).copied().unwrap_or(0) == 1 {
                    Some(name.clone())
                } else {
                    None
                }
            } else {
                None
            };

            // Recurse into arms first
            let arms: Vec<MatchArm> = arms
                .into_iter()
                .map(|arm| MatchArm {
                    body: insert_reuse_in_expr(arm.body, counts, counter),
                    guard: arm.guard.map(|g| insert_reuse_in_expr(g, counts, counter)),
                    pattern: arm.pattern,
                })
                .collect();

            if let Some(ref name) = reuse_name {
                // Try to wrap each arm with Drop/Reuse if it contains a constructor
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        if is_cow_arm(&arm) {
                            return arm; // Skip arms already handled by CoW
                        }
                        // Skip nullary constructor patterns — the JIT bakes these as
                        // compile-time constants (emit_heap_nvalue), so jit_drop_reuse
                        // would mutate a shared constant, corrupting all future uses.
                        if matches!(&arm.pattern, Pattern::Constructor(_, fields) if fields.is_empty()) {
                            return arm;
                        }
                        if let Some(rewritten) =
                            try_wrap_with_reuse(&arm.body, &arm.pattern, name, counter)
                        {
                            MatchArm {
                                body: rewritten,
                                ..arm
                            }
                        } else {
                            arm
                        }
                    })
                    .collect();
                Expr::Match { subject, arms, ty }
            } else {
                Expr::Match { subject, arms, ty }
            }
        }
        // Recurse into all other expressions
        other => walk_expr_children(other, |child| insert_reuse_in_expr(child, counts, counter)),
    }
}

/// Check if a match arm exhibits the CoW single-field-update pattern
/// (same enum tag with only one field changed). Skip reuse for these.
fn is_cow_arm(arm: &MatchArm) -> bool {
    let Pattern::Constructor(tag, payload_patterns) = &arm.pattern else {
        return false;
    };
    if payload_patterns.is_empty() {
        return false;
    }
    let bindings: Vec<String> = payload_patterns
        .iter()
        .map(|p| match p {
            Pattern::Var(name) => name.clone(),
            _ => String::new(),
        })
        .collect();
    is_single_field_enum_update_expr(&arm.body, tag, &bindings)
}

fn detect_single_field_update(fields: &[Expr], bindings: &[String]) -> bool {
    if fields.len() != bindings.len() {
        return false;
    }
    let mut changed = 0usize;
    for (field, binding) in fields.iter().zip(bindings.iter()) {
        if matches!(field, Expr::Var(name, _) if name == binding) {
            continue;
        }
        changed += 1;
        if changed > 1 {
            return false;
        }
    }
    changed == 1
}

fn is_single_field_enum_update_expr(expr: &Expr, tag: &str, bindings: &[String]) -> bool {
    match expr {
        Expr::MakeEnum {
            tag: body_tag,
            payload,
            ..
        } if body_tag == tag => {
            if let Expr::MakeTuple(fields, _) = payload.as_ref() {
                detect_single_field_update(fields, bindings)
            } else {
                false
            }
        }
        Expr::Block(exprs, _) if !exprs.is_empty() => {
            is_single_field_enum_update_expr(exprs.last().unwrap(), tag, bindings)
        }
        Expr::If {
            then_branch,
            else_branch: Some(else_branch),
            ..
        } => {
            is_single_field_enum_update_expr(then_branch, tag, bindings)
                && is_single_field_enum_update_expr(else_branch, tag, bindings)
        }
        _ => false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ReuseShape {
    Enum { tag: String, arity: usize },
    Tuple { len: usize },
    Record { fields: Vec<String> },
    Struct { name: String, fields: Vec<String> },
}

fn sorted_expr_field_names(fields: &[(String, Expr)]) -> Vec<String> {
    let mut names: Vec<String> = fields.iter().map(|(name, _)| name.clone()).collect();
    names.sort();
    names
}

fn sorted_pattern_field_names(fields: &[(String, Pattern)]) -> Vec<String> {
    let mut names: Vec<String> = fields.iter().map(|(name, _)| name.clone()).collect();
    names.sort();
    names
}

fn enum_payload_arity(payload: &Expr) -> usize {
    match payload {
        Expr::Unit => 0,
        Expr::MakeTuple(items, _) => items.len(),
        _ => 1,
    }
}

fn pattern_reuse_shape(pattern: &Pattern) -> Option<ReuseShape> {
    match pattern {
        // Nullary constructors are compile-time constants in the JIT and are not
        // safe to target for in-place mutation through jit_drop_reuse.
        Pattern::Constructor(_, fields) if fields.is_empty() => None,
        Pattern::Constructor(tag, fields) => Some(ReuseShape::Enum {
            tag: tag.clone(),
            arity: fields.len(),
        }),
        Pattern::Tuple(fields) => Some(ReuseShape::Tuple { len: fields.len() }),
        Pattern::Record(fields) => Some(ReuseShape::Record {
            fields: sorted_pattern_field_names(fields),
        }),
        Pattern::Wildcard | Pattern::Var(_) | Pattern::Literal(_) | Pattern::List(_, _) => None,
    }
}

/// Determine the constructor shape produced by an expression's result position.
///
/// For conditionals, both branches must produce the same shape to be eligible.
fn expr_reuse_shape(expr: &Expr) -> Option<ReuseShape> {
    match expr {
        Expr::MakeEnum { tag, payload, .. } => Some(ReuseShape::Enum {
            tag: tag.clone(),
            arity: enum_payload_arity(payload),
        }),
        Expr::MakeStruct { name, fields, .. } => Some(ReuseShape::Struct {
            name: name.clone(),
            fields: sorted_expr_field_names(fields),
        }),
        Expr::MakeRecord(fields, _) => Some(ReuseShape::Record {
            fields: sorted_expr_field_names(fields),
        }),
        Expr::MakeTuple(items, _) => Some(ReuseShape::Tuple { len: items.len() }),
        Expr::Block(exprs, _) if !exprs.is_empty() => expr_reuse_shape(exprs.last().unwrap()),
        Expr::If {
            then_branch,
            else_branch: Some(else_branch),
            ..
        } => {
            let then_shape = expr_reuse_shape(then_branch)?;
            let else_shape = expr_reuse_shape(else_branch)?;
            if then_shape == else_shape {
                Some(then_shape)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Reuse is only valid when the destructured pattern shape matches the
/// reconstructed allocation shape in that arm.
fn is_reuse_shape_compatible(pattern: &Pattern, body: &Expr) -> bool {
    let Some(pattern_shape) = pattern_reuse_shape(pattern) else {
        return false;
    };
    let Some(body_shape) = expr_reuse_shape(body) else {
        return false;
    };

    match (pattern_shape, body_shape) {
        (
            ReuseShape::Enum {
                tag: pattern_tag,
                arity: pattern_arity,
            },
            ReuseShape::Enum {
                tag: body_tag,
                arity: body_arity,
            },
        ) => pattern_tag == body_tag && pattern_arity == body_arity,
        (ReuseShape::Tuple { len: p }, ReuseShape::Tuple { len: b }) => p == b,
        (ReuseShape::Record { fields: p }, ReuseShape::Record { fields: b }) => p == b,
        // Record patterns can match named structs by field shape.
        (ReuseShape::Record { fields: p }, ReuseShape::Struct { fields: b, .. }) => p == b,
        _ => false,
    }
}

/// Try to find a constructor in the arm body and wrap it with Drop/Reuse.
/// Returns Some(wrapped_expr) if successful.
fn try_wrap_with_reuse(
    body: &Expr,
    pattern: &Pattern,
    drop_name: &str,
    counter: &mut usize,
) -> Option<Expr> {
    if !is_reuse_shape_compatible(pattern, body) {
        return None;
    }

    let token = format!("_reuse_{}", *counter);
    *counter += 1;

    // Wrap the entire body: Drop { name, token, body: <body with Reuse around constructor> }
    let rewritten_body = wrap_constructor_with_reuse(body.clone(), &token);
    Some(Expr::Drop {
        name: drop_name.to_string(),
        token: Some(token),
        body: Box::new(rewritten_body),
    })
}

/// Wrap the outermost constructor in an expression with Reuse.
/// Descends through Block, If/else, and Let to find tail constructors.
fn wrap_constructor_with_reuse(expr: Expr, token: &str) -> Expr {
    match expr {
        Expr::MakeEnum { .. }
        | Expr::MakeStruct { .. }
        | Expr::MakeRecord(_, _)
        | Expr::MakeTuple(_, _) => Expr::Reuse {
            token: token.to_string(),
            alloc: Box::new(expr),
        },
        Expr::Block(mut exprs, ty) if !exprs.is_empty() => {
            let last = exprs.pop().unwrap();
            let wrapped = wrap_constructor_with_reuse(last, token);
            exprs.push(wrapped);
            Expr::Block(exprs, ty)
        }
        Expr::If {
            condition,
            then_branch,
            else_branch: Some(else_branch),
            ty,
        } => Expr::If {
            condition,
            then_branch: Box::new(wrap_constructor_with_reuse(*then_branch, token)),
            else_branch: Some(Box::new(wrap_constructor_with_reuse(*else_branch, token))),
            ty,
        },
        other => other,
    }
}

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
        Expr::Assign { name, value } => Expr::Assign {
            name,
            value: Box::new(f(*value)),
        },
        Expr::FieldAssign {
            object,
            field,
            value,
        } => Expr::FieldAssign {
            object,
            field,
            value: Box::new(f(*value)),
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

        // Perceus reuse analysis
        Expr::Drop { name, token, body } => Expr::Drop {
            name,
            token,
            body: Box::new(f(*body)),
        },
        Expr::Reuse { token, alloc } => Expr::Reuse {
            token,
            alloc: Box::new(f(*alloc)),
        },
    }
}

/// Visit all nodes in an expression tree (read-only, depth-first).
fn visit_expr(expr: &Expr, f: &mut impl FnMut(&Expr)) {
    f(expr);
    visit_expr_children(expr, f);
}

/// Visit all immediate children of an expression (read-only).
fn visit_immediate_children(expr: &Expr, f: &mut impl FnMut(&Expr)) {
    match expr {
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Unit
        | Expr::Hole
        | Expr::Var(_, _) => {}
        Expr::BinOp { lhs, rhs, .. } => {
            f(lhs);
            f(rhs);
        }
        Expr::UnaryOp { operand, .. } => f(operand),
        Expr::And(l, r) | Expr::Or(l, r) => {
            f(l);
            f(r);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            f(condition);
            f(then_branch);
            if let Some(e) = else_branch {
                f(e);
            }
        }
        Expr::Match { subject, arms, .. } => {
            f(subject);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    f(guard);
                }
                f(&arm.body);
            }
        }
        Expr::For { iterable, body, .. } => {
            f(iterable);
            f(body);
        }
        Expr::Let { value, .. } | Expr::Assign { value, .. } | Expr::FieldAssign { value, .. } => {
            f(value)
        }
        Expr::Block(exprs, _) => {
            for e in exprs {
                f(e);
            }
        }
        Expr::CallDirect { args, .. }
        | Expr::CallNative { args, .. }
        | Expr::TailCall { args, .. } => {
            for a in args {
                f(a);
            }
        }
        Expr::CallIndirect { callee, args, .. } | Expr::TailCallIndirect { callee, args, .. } => {
            f(callee);
            for a in args {
                f(a);
            }
        }
        Expr::MakeEnum { payload, .. } => f(payload),
        Expr::MakeStruct { fields, .. } | Expr::MakeRecord(fields, _) => {
            for (_, v) in fields {
                f(v);
            }
        }
        Expr::MakeList(items, _) | Expr::MakeTuple(items, _) => {
            for item in items {
                f(item);
            }
        }
        Expr::MakeRange(s, e) => {
            f(s);
            f(e);
        }
        Expr::UpdateRecord { base, updates, .. } => {
            f(base);
            for (_, v) in updates {
                f(v);
            }
        }
        Expr::GetField { object, .. } => f(object),
        Expr::Lambda { body, .. } => f(body),
        Expr::MakeClosure { captures, .. } => {
            for c in captures {
                f(c);
            }
        }
        Expr::GetClosureVar(_) => {}
        Expr::Try { expr, .. } => f(expr),
        Expr::Concat(parts) => {
            for p in parts {
                f(p);
            }
        }
        Expr::WithHandlers { handlers, body, .. } => {
            for (_, methods) in handlers {
                for (_, h) in methods {
                    f(h);
                }
            }
            f(body);
        }
        Expr::HandleEffect { body, clauses, .. } => {
            f(body);
            for c in clauses {
                f(&c.body);
            }
        }
        Expr::PerformEffect { args, .. } => {
            for a in args {
                f(a);
            }
        }
        Expr::Expect { actual, matcher } => {
            f(actual);
            match matcher.as_ref() {
                crate::vm::ir::Matcher::Equal(e)
                | crate::vm::ir::Matcher::HaveLength(e)
                | crate::vm::ir::Matcher::Contain(e)
                | crate::vm::ir::Matcher::StartWith(e)
                | crate::vm::ir::Matcher::Satisfy(e) => f(e),
                _ => {}
            }
        }
        Expr::Drop { body, .. } => f(body),
        Expr::Reuse { alloc, .. } => f(alloc),
    }
}

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
        Expr::Let { value, .. } | Expr::Assign { value, .. } | Expr::FieldAssign { value, .. } => {
            visit_expr(value, f)
        }
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

        // Perceus reuse analysis
        Expr::Drop { body, .. } => visit_expr(body, f),
        Expr::Reuse { alloc, .. } => visit_expr(alloc, f),
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
            // (but NOT for mutable variables — those are tracked in Block)
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
            // Collect variables that are assigned anywhere in this block
            // (including nested For/If/Match bodies) — these must not be
            // constant-propagated since their values change at runtime.
            let mut assigned_vars = HashSet::new();
            for e in &exprs {
                collect_assigned_vars(e, &mut assigned_vars);
            }
            env.push(HashMap::new());
            let exprs: Vec<Expr> = exprs
                .into_iter()
                .map(|e| {
                    let result = propagate_expr(e, env);
                    // Remove any assigned vars that snuck into the env from Let
                    if let Some(scope) = env.last_mut() {
                        for name in &assigned_vars {
                            scope.remove(name);
                        }
                    }
                    result
                })
                .collect();
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
            // Invalidate any variables assigned inside the loop body — their
            // values change across iterations so constants from before the loop
            // must not be propagated into the body.
            let mut assigned = HashSet::new();
            collect_assigned_vars(&body, &mut assigned);
            for name in &assigned {
                for scope in env.iter_mut().rev() {
                    if scope.remove(name).is_some() {
                        break;
                    }
                }
            }
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

        Expr::Assign { name, value } => {
            let value = propagate_expr(*value, env);
            // Invalidate propagated constant for this variable
            for scope in env.iter_mut().rev() {
                if scope.remove(&name).is_some() {
                    break;
                }
            }
            Expr::Assign {
                name,
                value: Box::new(value),
            }
        }

        Expr::FieldAssign {
            object,
            field,
            value,
        } => {
            let value = propagate_expr(*value, env);
            // Invalidate propagated constant for the object variable
            for scope in env.iter_mut().rev() {
                if scope.remove(&object).is_some() {
                    break;
                }
            }
            Expr::FieldAssign {
                object,
                field,
                value: Box::new(value),
            }
        }

        // All other nodes: recurse into children with propagate_expr
        other => walk_expr_children(other, |child| propagate_expr(child, env)),
    }
}

/// Recursively collect all variable names that are targets of `Assign` in an expression tree.
fn collect_assigned_vars(expr: &Expr, out: &mut HashSet<String>) {
    match expr {
        Expr::Assign { name, value } => {
            out.insert(name.clone());
            collect_assigned_vars(value, out);
        }
        Expr::FieldAssign { object, value, .. } => {
            out.insert(object.clone());
            collect_assigned_vars(value, out);
        }
        Expr::Block(exprs, _) => {
            for e in exprs {
                collect_assigned_vars(e, out);
            }
        }
        Expr::For { iterable, body, .. } => {
            collect_assigned_vars(iterable, out);
            collect_assigned_vars(body, out);
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_assigned_vars(condition, out);
            collect_assigned_vars(then_branch, out);
            if let Some(e) = else_branch {
                collect_assigned_vars(e, out);
            }
        }
        Expr::Let { value, .. } => collect_assigned_vars(value, out),
        Expr::Match { subject, arms, .. } => {
            collect_assigned_vars(subject, out);
            for arm in arms {
                collect_assigned_vars(&arm.body, out);
            }
        }
        _ => {}
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
        BinOp::Add => a.checked_add(b).map(Expr::Int),
        BinOp::Sub => a.checked_sub(b).map(Expr::Int),
        BinOp::Mul => a.checked_mul(b).map(Expr::Int),
        BinOp::Div => {
            if b == 0 {
                None
            } else {
                a.checked_div(b).map(Expr::Int)
            }
        }
        BinOp::Mod => {
            if b == 0 {
                None
            } else {
                a.checked_rem(b).map(Expr::Int)
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

/// Higher inline threshold for lightweight functions (only arithmetic + record ops,
/// no closures, effects, or list construction).
const INLINE_THRESHOLD_LARGE: usize = 900;

/// Maximum number of inlining iterations to handle chains (f calls g calls h).
const INLINE_MAX_ROUNDS: usize = 4;

/// Count the number of AST nodes in an expression (for inlining budget).
fn expr_node_count(expr: &Expr) -> usize {
    let mut count = 0;
    visit_expr(expr, &mut |_| count += 1);
    count
}

/// Returns true if a function body contains only lightweight operations
/// (arithmetic, records, fields, control flow, direct calls). No closures,
/// effects, lists, or dynamic dispatch.
fn is_lightweight(expr: &Expr) -> bool {
    let mut lightweight = true;
    visit_expr(expr, &mut |e| {
        if !lightweight {
            return;
        }
        match e {
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::String(_)
            | Expr::Bool(_)
            | Expr::Unit
            | Expr::Var(_, _)
            | Expr::BinOp { .. }
            | Expr::UnaryOp { .. }
            | Expr::And(_, _)
            | Expr::Or(_, _)
            | Expr::GetField { .. }
            | Expr::MakeRecord(_, _)
            | Expr::MakeStruct { .. }
            | Expr::UpdateRecord { .. }
            | Expr::MakeTuple(_, _)
            | Expr::MakeRange(_, _)
            | Expr::Let { .. }
            | Expr::Block(_, _)
            | Expr::If { .. }
            | Expr::Match { .. }
            | Expr::CallDirect { .. }
            | Expr::TailCall { .. }
            | Expr::CallNative { .. } => {}
            _ => {
                lightweight = false;
            }
        }
    });
    lightweight
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
        Pattern::Tuple(pats) => Pattern::Tuple(
            pats.into_iter()
                .map(|p| rename_pattern(p, rename_map))
                .collect(),
        ),
        Pattern::Constructor(tag, pats) => Pattern::Constructor(
            tag,
            pats.into_iter()
                .map(|p| rename_pattern(p, rename_map))
                .collect(),
        ),
        Pattern::Record(fields) => Pattern::Record(
            fields
                .into_iter()
                .map(|(k, p)| (k, rename_pattern(p, rename_map)))
                .collect(),
        ),
        Pattern::List(pats, rest) => {
            let rest = rest.map(|r| rename_map.get(&r).cloned().unwrap_or(r));
            Pattern::List(
                pats.into_iter()
                    .map(|p| rename_pattern(p, rename_map))
                    .collect(),
                rest,
            )
        }
        other => other,
    }
}

/// Check if an expression contains constructs that return early from the current function.
/// Functions containing these cannot be safely inlined without transforming the early returns.
fn has_early_return(expr: &Expr) -> bool {
    let mut found = false;
    visit_expr(expr, &mut |e| {
        if found {
            return;
        }
        match e {
            Expr::Try { .. } | Expr::TailCall { .. } | Expr::TailCallIndirect { .. } => {
                found = true;
            }
            _ => {}
        }
    });
    found
}

/// Inline eligible function calls within the module.
/// Runs multiple rounds to handle chains (f→g→h).
fn inline_functions(module: &mut IrModule) {
    // Build a map of inlineable functions: name → (params, body)
    // A function is inlineable if:
    //   1. Body size <= INLINE_THRESHOLD
    //   2. Not self-recursive (no CallDirect/TailCall to self)
    //   3. Doesn't have early returns (like Expr::Try) which break inline semantics.
    let inlineable: HashMap<String, (Vec<String>, Expr)> = module
        .functions
        .iter()
        .filter(|f| {
            let size = expr_node_count(&f.body);
            let threshold = if is_lightweight(&f.body) {
                INLINE_THRESHOLD_LARGE
            } else {
                INLINE_THRESHOLD
            };
            size <= threshold
                && !references_function(&f.body, &f.name)
                && !has_early_return(&f.body)
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

/// Fuse `let (a, b, ...) = (x, y, ...)` into individual let bindings.
/// Also handles `let (a, b) = { ..stmts; (x, y) }` by extracting stmts
/// and binding each element directly, eliminating the intermediate tuple.
fn fuse_tuple_lets(expr: Expr) -> Expr {
    transform_expr(expr, &mut |e| {
        if let Expr::Let {
            ref pattern,
            ref value,
            ..
        } = e
        {
            if let Pattern::Tuple(ref pats) = **pattern {
                if let Expr::MakeTuple(ref elems, _) = **value {
                    if pats.len() == elems.len() {
                        let bindings: Vec<Expr> = pats
                            .iter()
                            .zip(elems.iter())
                            .map(|(p, v)| Expr::Let {
                                pattern: Box::new(p.clone()),
                                value: Box::new(v.clone()),
                                ty: None,
                            })
                            .collect();
                        return if bindings.len() == 1 {
                            bindings.into_iter().next().unwrap()
                        } else {
                            Expr::Block(bindings, None)
                        };
                    }
                }
                if let Expr::Block(ref exprs, _) = **value {
                    if let Some(Expr::MakeTuple(elems, _)) = exprs.last() {
                        if pats.len() == elems.len() {
                            let mut stmts: Vec<Expr> = exprs[..exprs.len() - 1].to_vec();
                            stmts.extend(pats.iter().zip(elems.iter()).map(|(p, v)| Expr::Let {
                                pattern: Box::new(p.clone()),
                                value: Box::new(v.clone()),
                                ty: None,
                            }));
                            return Expr::Block(stmts, None);
                        }
                    }
                }
            }
        }
        e
    })
}

/// Simplify blocks: remove non-final Unit expressions, flatten nested blocks,
/// unwrap single-element blocks.
fn simplify_blocks(expr: Expr) -> Expr {
    transform_expr(expr, &mut |e| {
        if let Expr::Block(exprs, ty) = e {
            // Flatten nested blocks and let-float Block values
            let mut simplified: Vec<Expr> = Vec::new();
            for e in exprs {
                match e {
                    Expr::Block(inner, _) => {
                        simplified.extend(inner);
                    }
                    // Let-floating: `let x = Block([s1, s2, final])` →
                    // `s1; s2; let x = final`
                    // This exposes UpdateRecord/MakeRecord values to SRA
                    // after function inlining creates Block-valued Let bindings.
                    Expr::Let { pattern, value, ty: let_ty }
                        if matches!(*value, Expr::Block(_, _)) =>
                    {
                        let Expr::Block(mut stmts, _) = *value else {
                            unreachable!()
                        };
                        if stmts.is_empty() {
                            simplified.push(Expr::Let {
                                pattern,
                                value: Box::new(Expr::Unit),
                                ty: let_ty,
                            });
                        } else {
                            let final_expr = stmts.pop().unwrap();
                            simplified.extend(stmts);
                            simplified.push(Expr::Let {
                                pattern,
                                value: Box::new(final_expr),
                                ty: let_ty,
                            });
                        }
                    }
                    other => {
                        simplified.push(other);
                    }
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
/// Also counts Assign targets — a variable that is reassigned must not be eliminated.
fn count_var_uses(expr: &Expr, counts: &mut HashMap<String, usize>) {
    visit_expr(expr, &mut |e| match e {
        Expr::Var(name, _) => {
            *counts.entry(name.clone()).or_insert(0) += 1;
        }
        Expr::Assign { name, .. } => {
            *counts.entry(name.clone()).or_insert(0) += 1;
        }
        Expr::FieldAssign { object, .. } => {
            *counts.entry(object.clone()).or_insert(0) += 1;
        }
        _ => {}
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
                param_types: vec![],
                span: dummy_span(),
            }],
            entry: 0,
            tags: TagRegistry::new(),
        }
    }

    fn make_module_with_params(params: &[&str], body: Expr) -> IrModule {
        IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: params.iter().map(|p| (*p).to_string()).collect(),
                body,
                ty: None,
                param_types: vec![],
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
                    param_types: vec![],
                    span: dummy_span(),
                },
                IrFunction {
                    name: callee_name.into(),
                    params: callee_params.into_iter().map(String::from).collect(),
                    body: callee_body,
                    ty: None,
                    param_types: vec![],
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
                    param_types: vec![],
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
                    param_types: vec![],
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
                    param_types: vec![],
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

    #[test]
    fn drop_reuse_skips_mismatched_enum_tag() {
        let body = Expr::Match {
            subject: Box::new(Expr::Var("x".into(), None)),
            arms: vec![MatchArm {
                pattern: Pattern::Constructor("Some".into(), vec![Pattern::Var("v".into())]),
                guard: None,
                body: Expr::MakeEnum {
                    tag: "Other".into(),
                    payload: Box::new(Expr::Var("v".into(), None)),
                    ty: None,
                },
            }],
            ty: None,
        };

        let mut module = make_module_with_params(&["x"], body);
        optimize(&mut module);

        let Expr::Match { arms, .. } = &module.functions[0].body else {
            panic!("Expected Match, got: {:?}", module.functions[0].body);
        };
        assert!(
            matches!(arms[0].body, Expr::MakeEnum { .. }),
            "Expected arm body to remain MakeEnum (no Drop/Reuse), got: {:?}",
            arms[0].body
        );
    }

    #[test]
    fn drop_reuse_skips_mismatched_enum_arity() {
        let body = Expr::Match {
            subject: Box::new(Expr::Var("x".into(), None)),
            arms: vec![MatchArm {
                pattern: Pattern::Constructor("Some".into(), vec![Pattern::Var("v".into())]),
                guard: None,
                body: Expr::MakeEnum {
                    tag: "Some".into(),
                    payload: Box::new(Expr::MakeTuple(
                        vec![Expr::Var("v".into(), None), Expr::Int(1)],
                        None,
                    )),
                    ty: None,
                },
            }],
            ty: None,
        };

        let mut module = make_module_with_params(&["x"], body);
        optimize(&mut module);

        let Expr::Match { arms, .. } = &module.functions[0].body else {
            panic!("Expected Match, got: {:?}", module.functions[0].body);
        };
        assert!(
            matches!(arms[0].body, Expr::MakeEnum { .. }),
            "Expected arity-mismatched arm to skip Drop/Reuse, got: {:?}",
            arms[0].body
        );
    }

    #[test]
    fn drop_reuse_inserts_for_matching_tuple_shape() {
        let body = Expr::Match {
            subject: Box::new(Expr::Var("t".into(), None)),
            arms: vec![MatchArm {
                pattern: Pattern::Tuple(vec![Pattern::Var("a".into()), Pattern::Var("b".into())]),
                guard: None,
                body: Expr::MakeTuple(
                    vec![Expr::Var("a".into(), None), Expr::Var("b".into(), None)],
                    None,
                ),
            }],
            ty: None,
        };

        let mut module = make_module_with_params(&["t"], body);
        optimize(&mut module);

        let Expr::Match { arms, .. } = &module.functions[0].body else {
            panic!("Expected Match, got: {:?}", module.functions[0].body);
        };
        let Expr::Drop {
            token: Some(_),
            body,
            ..
        } = &arms[0].body
        else {
            panic!(
                "Expected matching tuple arm to get Drop/Reuse, got: {:?}",
                arms[0].body
            );
        };
        assert!(
            matches!(body.as_ref(), Expr::Reuse { alloc, .. } if matches!(alloc.as_ref(), Expr::MakeTuple(items, _) if items.len() == 2)),
            "Expected Drop body to contain Reuse(MakeTuple(len=2)), got: {:?}",
            body
        );
    }

    #[test]
    fn drop_reuse_skips_if_branch_shape_mismatch() {
        let body = Expr::Match {
            subject: Box::new(Expr::Var("t".into(), None)),
            arms: vec![MatchArm {
                pattern: Pattern::Tuple(vec![Pattern::Var("a".into()), Pattern::Var("b".into())]),
                guard: None,
                body: Expr::If {
                    condition: Box::new(Expr::Var("cond".into(), None)),
                    then_branch: Box::new(Expr::MakeTuple(
                        vec![Expr::Var("a".into(), None), Expr::Var("b".into(), None)],
                        None,
                    )),
                    else_branch: Some(Box::new(Expr::MakeTuple(
                        vec![
                            Expr::Var("a".into(), None),
                            Expr::Var("b".into(), None),
                            Expr::Int(0),
                        ],
                        None,
                    ))),
                    ty: None,
                },
            }],
            ty: None,
        };

        let mut module = make_module_with_params(&["t", "cond"], body);
        optimize(&mut module);

        let Expr::Match { arms, .. } = &module.functions[0].body else {
            panic!("Expected Match, got: {:?}", module.functions[0].body);
        };
        assert!(
            !matches!(arms[0].body, Expr::Drop { .. }),
            "Expected branch-shape mismatch to skip Drop/Reuse, got: {:?}",
            arms[0].body
        );
    }

    #[test]
    fn drop_reuse_skips_single_field_enum_cow_arm() {
        let body = Expr::Match {
            subject: Box::new(Expr::Var("node".into(), None)),
            arms: vec![MatchArm {
                pattern: Pattern::Constructor(
                    "Node".into(),
                    vec![
                        Pattern::Var("a".into()),
                        Pattern::Var("b".into()),
                        Pattern::Var("c".into()),
                    ],
                ),
                guard: None,
                body: Expr::MakeEnum {
                    tag: "Node".into(),
                    payload: Box::new(Expr::MakeTuple(
                        vec![
                            Expr::Var("a".into(), None),
                            Expr::Int(1),
                            Expr::Var("c".into(), None),
                        ],
                        None,
                    )),
                    ty: None,
                },
            }],
            ty: None,
        };

        let mut module = make_module_with_params(&["node"], body);
        optimize(&mut module);

        let Expr::Match { arms, .. } = &module.functions[0].body else {
            panic!("Expected Match, got: {:?}", module.functions[0].body);
        };
        assert!(
            !matches!(arms[0].body, Expr::Drop { .. }),
            "Expected single-field CoW arm to skip Drop/Reuse, got: {:?}",
            arms[0].body
        );
    }

    #[test]
    fn drop_reuse_inserts_for_multi_field_enum_update() {
        let body = Expr::Match {
            subject: Box::new(Expr::Var("node".into(), None)),
            arms: vec![MatchArm {
                pattern: Pattern::Constructor(
                    "Node".into(),
                    vec![
                        Pattern::Var("a".into()),
                        Pattern::Var("b".into()),
                        Pattern::Var("c".into()),
                    ],
                ),
                guard: None,
                body: Expr::MakeEnum {
                    tag: "Node".into(),
                    payload: Box::new(Expr::MakeTuple(
                        vec![Expr::Int(0), Expr::Int(1), Expr::Var("c".into(), None)],
                        None,
                    )),
                    ty: None,
                },
            }],
            ty: None,
        };

        let mut module = make_module_with_params(&["node"], body);
        optimize(&mut module);

        let Expr::Match { arms, .. } = &module.functions[0].body else {
            panic!("Expected Match, got: {:?}", module.functions[0].body);
        };
        assert!(
            matches!(arms[0].body, Expr::Drop { .. }),
            "Expected multi-field enum update to use Drop/Reuse, got: {:?}",
            arms[0].body
        );
    }

    #[test]
    fn drop_reuse_inserts_for_mixed_if_enum_update() {
        let body = Expr::Match {
            subject: Box::new(Expr::Var("node".into(), None)),
            arms: vec![MatchArm {
                pattern: Pattern::Constructor(
                    "Node".into(),
                    vec![
                        Pattern::Var("a".into()),
                        Pattern::Var("b".into()),
                        Pattern::Var("c".into()),
                    ],
                ),
                guard: None,
                body: Expr::If {
                    condition: Box::new(Expr::Var("cond".into(), None)),
                    then_branch: Box::new(Expr::MakeEnum {
                        tag: "Node".into(),
                        payload: Box::new(Expr::MakeTuple(
                            vec![
                                Expr::Var("a".into(), None),
                                Expr::Int(1),
                                Expr::Var("c".into(), None),
                            ],
                            None,
                        )),
                        ty: None,
                    }),
                    else_branch: Some(Box::new(Expr::MakeEnum {
                        tag: "Node".into(),
                        payload: Box::new(Expr::MakeTuple(
                            vec![Expr::Int(0), Expr::Int(1), Expr::Var("c".into(), None)],
                            None,
                        )),
                        ty: None,
                    })),
                    ty: None,
                },
            }],
            ty: None,
        };

        let mut module = make_module_with_params(&["node", "cond"], body);
        optimize(&mut module);

        let Expr::Match { arms, .. } = &module.functions[0].body else {
            panic!("Expected Match, got: {:?}", module.functions[0].body);
        };
        assert!(
            matches!(arms[0].body, Expr::Drop { .. }),
            "Expected mixed if-branch enum update to keep Drop/Reuse path, got: {:?}",
            arms[0].body
        );
    }

    #[test]
    fn trmc_rewrites_single_recursive_constructor_arm() {
        let body = Expr::Match {
            subject: Box::new(Expr::Var("xs".into(), None)),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Constructor(
                        "Cons".into(),
                        vec![Pattern::Var("h".into()), Pattern::Var("t".into())],
                    ),
                    guard: None,
                    body: Expr::MakeEnum {
                        tag: "Cons".into(),
                        payload: Box::new(Expr::MakeTuple(
                            vec![
                                Expr::Var("h".into(), None),
                                Expr::CallDirect {
                                    name: "main".into(),
                                    args: vec![Expr::Var("t".into(), None)],
                                    ty: None,
                                },
                            ],
                            None,
                        )),
                        ty: None,
                    },
                },
                MatchArm {
                    pattern: Pattern::Constructor("Nil".into(), vec![]),
                    guard: None,
                    body: Expr::MakeEnum {
                        tag: "Nil".into(),
                        payload: Box::new(Expr::Unit),
                        ty: None,
                    },
                },
            ],
            ty: None,
        };

        let mut module = make_module_with_params(&["xs"], body);
        optimize(&mut module);

        assert!(
            module.functions.iter().any(|f| f.name == "main__trmc_loop"),
            "Expected generated loop helper, functions: {:?}",
            module
                .functions
                .iter()
                .map(|f| f.name.clone())
                .collect::<Vec<_>>()
        );
        assert!(
            module
                .functions
                .iter()
                .any(|f| f.name == "main__trmc_build"),
            "Expected generated build helper"
        );
        assert!(
            matches!(&module.functions[0].body, Expr::CallDirect { name, .. } if name == "main__trmc_loop"),
            "Expected wrapper body to call main__trmc_loop, got: {:?}",
            module.functions[0].body
        );

        let loop_fn = module
            .functions
            .iter()
            .find(|f| f.name == "main__trmc_loop")
            .unwrap();
        let Expr::Match { arms, .. } = &loop_fn.body else {
            panic!(
                "Expected TRMC loop body to be a match, got: {:?}",
                loop_fn.body
            );
        };
        let cons_arm = arms
            .iter()
            .find(|arm| matches!(&arm.pattern, Pattern::Constructor(tag, _) if tag == "Cons"))
            .unwrap();
        let mut has_drop_tail_loop = false;
        super::visit_expr(&cons_arm.body, &mut |e| {
            if let Expr::Drop { body, .. } = e
                && matches!(body.as_ref(), Expr::TailCall { name, .. } if name == "main__trmc_loop")
            {
                has_drop_tail_loop = true;
            }
        });
        assert!(
            has_drop_tail_loop,
            "Expected recursive loop arm to contain Drop -> TailCall(main__trmc_loop), got: {:?}",
            cons_arm.body
        );

        let build_fn = module
            .functions
            .iter()
            .find(|f| f.name == "main__trmc_build")
            .unwrap();
        let Expr::Match {
            arms: build_arms, ..
        } = &build_fn.body
        else {
            panic!(
                "Expected TRMC build body to be a match, got: {:?}",
                build_fn.body
            );
        };
        let frame_arm = build_arms
            .iter()
            .find(|arm| matches!(&arm.pattern, Pattern::Constructor(tag, _) if tag.starts_with("__trmc_cons_")))
            .unwrap();
        let mut has_build_tail = false;
        let mut has_cons_reuse = false;
        super::visit_expr(&frame_arm.body, &mut |e| {
            if matches!(e, Expr::TailCall { name, .. } if name == "main__trmc_build") {
                has_build_tail = true;
            }
            if matches!(e, Expr::Reuse { alloc, .. } if matches!(alloc.as_ref(), Expr::MakeEnum { tag, .. } if tag == "Cons"))
            {
                has_cons_reuse = true;
            }
        });
        assert!(has_build_tail, "Expected build helper to self-tail-call");
        assert!(
            has_cons_reuse,
            "Expected build helper to reconstruct Cons via Reuse"
        );
    }

    #[test]
    fn trmc_skips_when_recursive_call_not_last_field() {
        let body = Expr::Match {
            subject: Box::new(Expr::Var("xs".into(), None)),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Constructor(
                        "Cons".into(),
                        vec![Pattern::Var("h".into()), Pattern::Var("t".into())],
                    ),
                    guard: None,
                    body: Expr::MakeEnum {
                        tag: "Cons".into(),
                        payload: Box::new(Expr::MakeTuple(
                            vec![
                                Expr::CallDirect {
                                    name: "main".into(),
                                    args: vec![Expr::Var("t".into(), None)],
                                    ty: None,
                                },
                                Expr::Var("h".into(), None),
                            ],
                            None,
                        )),
                        ty: None,
                    },
                },
                MatchArm {
                    pattern: Pattern::Constructor("Nil".into(), vec![]),
                    guard: None,
                    body: Expr::MakeEnum {
                        tag: "Nil".into(),
                        payload: Box::new(Expr::Unit),
                        ty: None,
                    },
                },
            ],
            ty: None,
        };

        let mut module = make_module_with_params(&["xs"], body);
        optimize(&mut module);

        assert!(
            !module.functions.iter().any(|f| f.name == "main__trmc_loop"),
            "TRMC should skip non-tail-modulo-constructor shape"
        );
        assert!(
            matches!(&module.functions[0].body, Expr::Match { .. }),
            "Original function should stay unchanged when TRMC is ineligible"
        );
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
                param_types: vec![],
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

    // -- Evidence passing transform tests --

    #[test]
    fn compute_needs_evidence_direct() {
        // A function that directly performs an effect needs evidence.
        let module = IrModule {
            functions: vec![IrFunction {
                name: "effectful".into(),
                params: vec![],
                body: Expr::PerformEffect {
                    effect: "E".into(),
                    method: "get".into(),
                    args: vec![],
                    ty: None,
                },
                ty: None,
                param_types: vec![],
                span: dummy_span(),
            }],
            entry: 0,
            tags: TagRegistry::new(),
        };
        let needs = super::compute_needs_evidence(&module);
        assert!(needs.contains("effectful"));
    }

    #[test]
    fn compute_needs_evidence_transitive() {
        // main calls effectful, so main also needs evidence.
        let module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::CallDirect {
                        name: "effectful".into(),
                        args: vec![],
                        ty: None,
                    },
                    ty: None,
                    param_types: vec![],
                    span: dummy_span(),
                },
                IrFunction {
                    name: "effectful".into(),
                    params: vec![],
                    body: Expr::PerformEffect {
                        effect: "E".into(),
                        method: "get".into(),
                        args: vec![],
                        ty: None,
                    },
                    ty: None,
                    param_types: vec![],
                    span: dummy_span(),
                },
            ],
            entry: 0,
            tags: TagRegistry::new(),
        };
        let needs = super::compute_needs_evidence(&module);
        assert!(needs.contains("main"));
        assert!(needs.contains("effectful"));
    }

    #[test]
    fn compute_needs_evidence_pure_function() {
        // A pure function should not need evidence.
        let module = IrModule {
            functions: vec![IrFunction {
                name: "pure".into(),
                params: vec!["x".into()],
                body: Expr::BinOp {
                    op: BinOp::Add,
                    lhs: Box::new(Expr::Var("x".into(), None)),
                    rhs: Box::new(Expr::Int(1)),
                    ty: None,
                },
                ty: None,
                param_types: vec![],
                span: dummy_span(),
            }],
            entry: 0,
            tags: TagRegistry::new(),
        };
        let needs = super::compute_needs_evidence(&module);
        assert!(needs.is_empty());
    }

    #[test]
    fn evidence_transform_tail_resumptive_handler() {
        // handle { perform E.get!() } with { E.get!() -> resume(42) }
        // After transform, no effect nodes should remain.
        use crate::vm::ir::HandlerClause;
        let handler_body = Expr::HandleEffect {
            body: Box::new(Expr::PerformEffect {
                effect: "E".into(),
                method: "get".into(),
                args: vec![],
                ty: None,
            }),
            clauses: vec![HandlerClause {
                effect: "E".into(),
                method: "get".into(),
                params: vec![],
                body: Expr::CallIndirect {
                    callee: Box::new(Expr::Var("resume".into(), None)),
                    args: vec![Expr::Int(42)],
                    ty: None,
                },
                is_tail_resumptive: true,
            }],
        };

        let mut module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: handler_body,
                ty: None,
                param_types: vec![],
                span: dummy_span(),
            }],
            entry: 0,
            tags: TagRegistry::new(),
        };

        super::evidence_transform(&mut module);

        // After transform, should have no HandleEffect or PerformEffect nodes.
        let mut has_effect_nodes = false;
        super::visit_expr(&module.functions[0].body, &mut |e| {
            if matches!(e, Expr::HandleEffect { .. } | Expr::PerformEffect { .. }) {
                has_effect_nodes = true;
            }
        });
        assert!(
            !has_effect_nodes,
            "Expected no effect nodes after transform, got: {:?}",
            module.functions[0].body
        );

        // Should contain MakeRecord (evidence) and CallIndirect (dispatch)
        let mut has_record = false;
        let mut has_indirect = false;
        super::visit_expr(&module.functions[0].body, &mut |e| {
            if matches!(e, Expr::MakeRecord(..)) {
                has_record = true;
            }
            if matches!(e, Expr::CallIndirect { .. }) {
                has_indirect = true;
            }
        });
        assert!(has_record, "Expected MakeRecord in transformed body");
        assert!(has_indirect, "Expected CallIndirect in transformed body");
    }

    #[test]
    fn evidence_transform_non_tail_resumptive_fiber_transform() {
        // Non-tail-resumptive handler should be transformed to fiber dispatch.
        use crate::vm::ir::HandlerClause;
        let handler_body = Expr::HandleEffect {
            body: Box::new(Expr::PerformEffect {
                effect: "E".into(),
                method: "get".into(),
                args: vec![],
                ty: None,
            }),
            clauses: vec![HandlerClause {
                effect: "E".into(),
                method: "get".into(),
                params: vec!["k".into()],
                body: Expr::BinOp {
                    op: BinOp::Add,
                    lhs: Box::new(Expr::CallIndirect {
                        callee: Box::new(Expr::Var("resume".into(), None)),
                        args: vec![Expr::Int(1)],
                        ty: None,
                    }),
                    rhs: Box::new(Expr::Int(2)),
                    ty: None,
                },
                is_tail_resumptive: false,
            }],
        };

        let mut module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body: handler_body,
                ty: None,
                param_types: vec![],
                span: dummy_span(),
            }],
            entry: 0,
            tags: TagRegistry::new(),
        };

        super::evidence_transform(&mut module);

        // HandleEffect should be eliminated — replaced with CallNative to __fiber.run_handler.
        let mut has_handle = false;
        let mut has_fiber_call = false;
        super::visit_expr(&module.functions[0].body, &mut |e| {
            if matches!(e, Expr::HandleEffect { .. }) {
                has_handle = true;
            }
            if let Expr::CallNative {
                module: m, method, ..
            } = e
            {
                if m == "__fiber" && method == "run_handler" {
                    has_fiber_call = true;
                }
            }
        });
        assert!(
            !has_handle,
            "HandleEffect should be eliminated by fiber transform"
        );
        assert!(has_fiber_call, "Should have __fiber.run_handler CallNative");
    }

    #[test]
    fn evidence_transform_threads_through_call_direct() {
        // main calls effectful_fn, both should get __ev param.
        use crate::vm::ir::HandlerClause;
        let mut module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::HandleEffect {
                        body: Box::new(Expr::CallDirect {
                            name: "effectful_fn".into(),
                            args: vec![Expr::Int(10)],
                            ty: None,
                        }),
                        clauses: vec![HandlerClause {
                            effect: "E".into(),
                            method: "inc".into(),
                            params: vec!["x".into()],
                            body: Expr::CallIndirect {
                                callee: Box::new(Expr::Var("resume".into(), None)),
                                args: vec![Expr::BinOp {
                                    op: BinOp::Add,
                                    lhs: Box::new(Expr::Var("x".into(), None)),
                                    rhs: Box::new(Expr::Int(1)),
                                    ty: None,
                                }],
                                ty: None,
                            },
                            is_tail_resumptive: true,
                        }],
                    },
                    ty: None,
                    param_types: vec![],
                    span: dummy_span(),
                },
                IrFunction {
                    name: "effectful_fn".into(),
                    params: vec!["n".into()],
                    body: Expr::PerformEffect {
                        effect: "E".into(),
                        method: "inc".into(),
                        args: vec![Expr::Var("n".into(), None)],
                        ty: None,
                    },
                    ty: None,
                    param_types: vec![],
                    span: dummy_span(),
                },
            ],
            entry: 0,
            tags: TagRegistry::new(),
        };

        super::evidence_transform(&mut module);

        // effectful_fn should now have __ev as first param
        assert_eq!(
            module.functions[1].params[0], "__ev",
            "effectful_fn should have __ev as first param"
        );
        assert_eq!(
            module.functions[1].params[1], "n",
            "effectful_fn should still have n as second param"
        );

        // No PerformEffect should remain in effectful_fn
        let mut has_perform = false;
        super::visit_expr(&module.functions[1].body, &mut |e| {
            if matches!(e, Expr::PerformEffect { .. }) {
                has_perform = true;
            }
        });
        assert!(
            !has_perform,
            "PerformEffect should be eliminated in effectful_fn"
        );
    }

    #[test]
    fn evidence_transform_nested_handlers() {
        // Nested handlers should merge evidence via UpdateRecord.
        use crate::vm::ir::HandlerClause;
        let body = Expr::HandleEffect {
            body: Box::new(Expr::HandleEffect {
                body: Box::new(Expr::PerformEffect {
                    effect: "B".into(),
                    method: "get".into(),
                    args: vec![],
                    ty: None,
                }),
                clauses: vec![HandlerClause {
                    effect: "B".into(),
                    method: "get".into(),
                    params: vec![],
                    body: Expr::CallIndirect {
                        callee: Box::new(Expr::Var("resume".into(), None)),
                        args: vec![Expr::Int(2)],
                        ty: None,
                    },
                    is_tail_resumptive: true,
                }],
            }),
            clauses: vec![HandlerClause {
                effect: "A".into(),
                method: "get".into(),
                params: vec![],
                body: Expr::CallIndirect {
                    callee: Box::new(Expr::Var("resume".into(), None)),
                    args: vec![Expr::Int(1)],
                    ty: None,
                },
                is_tail_resumptive: true,
            }],
        };

        let mut module = IrModule {
            functions: vec![IrFunction {
                name: "main".into(),
                params: vec![],
                body,
                ty: None,
                param_types: vec![],
                span: dummy_span(),
            }],
            entry: 0,
            tags: TagRegistry::new(),
        };

        super::evidence_transform(&mut module);

        // Should have no effect nodes remaining.
        let mut has_effect = false;
        super::visit_expr(&module.functions[0].body, &mut |e| {
            if matches!(e, Expr::HandleEffect { .. } | Expr::PerformEffect { .. }) {
                has_effect = true;
            }
        });
        assert!(
            !has_effect,
            "Nested handler should eliminate all effect nodes"
        );

        // Should have an UpdateRecord (inner handler merges with outer).
        let mut has_update = false;
        super::visit_expr(&module.functions[0].body, &mut |e| {
            if matches!(e, Expr::UpdateRecord { .. }) {
                has_update = true;
            }
        });
        assert!(
            has_update,
            "Nested handler should use UpdateRecord to merge evidence"
        );
    }

    #[test]
    fn evidence_transform_tail_call_threading() {
        // TailCall to effectful function should also get evidence prepended.
        use crate::vm::ir::HandlerClause;
        let mut module = IrModule {
            functions: vec![
                IrFunction {
                    name: "main".into(),
                    params: vec![],
                    body: Expr::HandleEffect {
                        body: Box::new(Expr::CallDirect {
                            name: "loop_fn".into(),
                            args: vec![Expr::Int(5)],
                            ty: None,
                        }),
                        clauses: vec![HandlerClause {
                            effect: "E".into(),
                            method: "tick".into(),
                            params: vec![],
                            body: Expr::CallIndirect {
                                callee: Box::new(Expr::Var("resume".into(), None)),
                                args: vec![Expr::Unit],
                                ty: None,
                            },
                            is_tail_resumptive: true,
                        }],
                    },
                    ty: None,
                    param_types: vec![],
                    span: dummy_span(),
                },
                IrFunction {
                    name: "loop_fn".into(),
                    params: vec!["n".into()],
                    body: Expr::If {
                        condition: Box::new(Expr::BinOp {
                            op: BinOp::Le,
                            lhs: Box::new(Expr::Var("n".into(), None)),
                            rhs: Box::new(Expr::Int(0)),
                            ty: None,
                        }),
                        then_branch: Box::new(Expr::Int(0)),
                        else_branch: Some(Box::new(Expr::Block(
                            vec![
                                Expr::PerformEffect {
                                    effect: "E".into(),
                                    method: "tick".into(),
                                    args: vec![],
                                    ty: None,
                                },
                                Expr::TailCall {
                                    name: "loop_fn".into(),
                                    args: vec![Expr::BinOp {
                                        op: BinOp::Sub,
                                        lhs: Box::new(Expr::Var("n".into(), None)),
                                        rhs: Box::new(Expr::Int(1)),
                                        ty: None,
                                    }],
                                    ty: None,
                                },
                            ],
                            None,
                        ))),
                        ty: None,
                    },
                    ty: None,
                    param_types: vec![],
                    span: dummy_span(),
                },
            ],
            entry: 0,
            tags: TagRegistry::new(),
        };

        super::evidence_transform(&mut module);

        // loop_fn should have __ev as first param
        assert_eq!(module.functions[1].params[0], "__ev");

        // TailCall in loop_fn body should have evidence prepended
        let mut has_tail_call_with_ev = false;
        super::visit_expr(&module.functions[1].body, &mut |e| {
            if let Expr::TailCall { name, args, .. } = e {
                if name == "loop_fn" && args.len() == 2 {
                    // First arg should be __ev variable
                    if matches!(&args[0], Expr::Var(n, _) if n == "__ev") {
                        has_tail_call_with_ev = true;
                    }
                }
            }
        });
        assert!(
            has_tail_call_with_ev,
            "TailCall to effectful loop_fn should have __ev prepended"
        );
    }

    #[test]
    fn strip_tail_resume_owned_works() {
        // resume(42) → 42
        let expr = Expr::CallIndirect {
            callee: Box::new(Expr::Var("resume".into(), None)),
            args: vec![Expr::Int(42)],
            ty: None,
        };
        let stripped = super::strip_tail_resume_owned(&expr);
        assert_eq!(stripped, Expr::Int(42));

        // Block([resume(42)]) → 42
        let block = Expr::Block(vec![expr], None);
        let stripped = super::strip_tail_resume_owned(&block);
        assert_eq!(stripped, Expr::Int(42));

        // Non resume call → returned as-is
        let not_resume = Expr::CallIndirect {
            callee: Box::new(Expr::Var("foo".into(), None)),
            args: vec![Expr::Int(42)],
            ty: None,
        };
        let stripped = super::strip_tail_resume_owned(&not_resume);
        assert_eq!(stripped, not_resume);
    }
}
