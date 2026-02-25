//! Stdlib HOFs compiled from Baseline source (.bl files).
//!
//! Embeds List/Option/Result HOF implementations as .bl source,
//! parses and lowers them to IR functions with qualified names
//! (e.g., `map` → `List.map`). These replace the native HOF
//! placeholders, enabling CallDirect instead of CallNative.

use super::ir::{Expr, IrFunction};
use super::lower::Lowerer;
use super::natives::NativeRegistry;

const LIST_SOURCE: &str = include_str!("../../../stdlib/List.bl");
const OPTION_SOURCE: &str = include_str!("../../../stdlib/Option.bl");
const RESULT_SOURCE: &str = include_str!("../../../stdlib/Result.bl");

/// Compile all stdlib HOFs and return (functions, qualified_names).
///
/// The returned functions have qualified names (e.g., `List.map`)
/// and internal references (TailCall, CallDirect) are patched accordingly.
pub fn compile_stdlib(natives: &NativeRegistry) -> (Vec<IrFunction>, Vec<String>) {
    let mut all_fns = Vec::new();
    let mut all_names = Vec::new();

    for (prefix, source) in [
        ("List", LIST_SOURCE),
        ("Option", OPTION_SOURCE),
        ("Result", RESULT_SOURCE),
    ] {
        let fns = compile_module(prefix, source, natives);
        for f in fns {
            all_names.push(f.name.clone());
            all_fns.push(f);
        }
    }

    (all_fns, all_names)
}

/// Parse and lower a single .bl module, qualifying names with the given prefix.
fn compile_module(prefix: &str, source: &str, natives: &NativeRegistry) -> Vec<IrFunction> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_baseline::LANGUAGE.into())
        .expect("Failed to load Baseline grammar");
    let tree = parser.parse(source, None).expect("Failed to parse stdlib");
    let root = tree.root_node();

    let mut lowerer = Lowerer::new(source, natives, None);
    let functions = lowerer
        .lower_module_functions(&root)
        .expect("Failed to lower stdlib");

    // Qualify names and patch self-references
    functions
        .into_iter()
        .map(|mut f| {
            let old_name = f.name.clone();
            let qualified = format!("{}.{}", prefix, old_name);
            f.name = qualified.clone();
            // Patch TailCall and CallDirect references from unqualified to qualified
            patch_references(&mut f.body, prefix, &old_name, &qualified);
            f
        })
        .collect()
}

/// Recursively patch TailCall and CallDirect names from unqualified to qualified form.
fn patch_references(expr: &mut Expr, prefix: &str, old_name: &str, qualified_name: &str) {
    match expr {
        Expr::TailCall { name, args, .. } => {
            if name == old_name {
                *name = qualified_name.to_string();
            }
            for a in args.iter_mut() {
                patch_references(a, prefix, old_name, qualified_name);
            }
        }
        Expr::CallDirect { name, args, .. } => {
            // Patch calls to sibling functions in the same module
            let qualified_target = format!("{}.{}", prefix, name);
            // Check if this is a call to a function in the same stdlib module
            if is_stdlib_function(prefix, name) {
                *name = qualified_target;
            }
            for a in args.iter_mut() {
                patch_references(a, prefix, old_name, qualified_name);
            }
        }
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            patch_references(condition, prefix, old_name, qualified_name);
            patch_references(then_branch, prefix, old_name, qualified_name);
            if let Some(e) = else_branch.as_mut() {
                patch_references(e, prefix, old_name, qualified_name);
            }
        }
        Expr::Block(exprs, _) => {
            for e in exprs.iter_mut() {
                patch_references(e, prefix, old_name, qualified_name);
            }
        }
        Expr::Let { value, .. } => {
            patch_references(value, prefix, old_name, qualified_name);
        }
        Expr::BinOp { lhs, rhs, .. } => {
            patch_references(lhs, prefix, old_name, qualified_name);
            patch_references(rhs, prefix, old_name, qualified_name);
        }
        Expr::UnaryOp { operand, .. } => {
            patch_references(operand, prefix, old_name, qualified_name);
        }
        Expr::And(a, b) | Expr::Or(a, b) => {
            patch_references(a, prefix, old_name, qualified_name);
            patch_references(b, prefix, old_name, qualified_name);
        }
        Expr::Match { subject, arms, .. } => {
            patch_references(subject, prefix, old_name, qualified_name);
            for arm in arms.iter_mut() {
                patch_references(&mut arm.body, prefix, old_name, qualified_name);
            }
        }
        Expr::MakeEnum { payload, .. } => {
            patch_references(payload, prefix, old_name, qualified_name);
        }
        Expr::MakeList(items, _) => {
            for item in items.iter_mut() {
                patch_references(item, prefix, old_name, qualified_name);
            }
        }
        Expr::Concat(parts) => {
            for p in parts.iter_mut() {
                patch_references(p, prefix, old_name, qualified_name);
            }
        }
        Expr::CallIndirect { callee, args, .. } => {
            patch_references(callee, prefix, old_name, qualified_name);
            for a in args.iter_mut() {
                patch_references(a, prefix, old_name, qualified_name);
            }
        }
        Expr::TailCallIndirect { callee, args, .. } => {
            patch_references(callee, prefix, old_name, qualified_name);
            for a in args.iter_mut() {
                patch_references(a, prefix, old_name, qualified_name);
            }
        }
        Expr::CallNative { args, .. } => {
            for a in args.iter_mut() {
                patch_references(a, prefix, old_name, qualified_name);
            }
        }
        Expr::MakeRecord(fields, _) | Expr::MakeStruct { fields, .. } => {
            for (_, v) in fields.iter_mut() {
                patch_references(v, prefix, old_name, qualified_name);
            }
        }
        Expr::MakeTuple(items, _) => {
            for item in items.iter_mut() {
                patch_references(item, prefix, old_name, qualified_name);
            }
        }
        Expr::GetField { object, .. } => {
            patch_references(object, prefix, old_name, qualified_name);
        }
        Expr::UpdateRecord { base, updates, .. } => {
            patch_references(base, prefix, old_name, qualified_name);
            for (_, v) in updates.iter_mut() {
                patch_references(v, prefix, old_name, qualified_name);
            }
        }
        Expr::For { iterable, body, .. } => {
            patch_references(iterable, prefix, old_name, qualified_name);
            patch_references(body, prefix, old_name, qualified_name);
        }
        Expr::MakeClosure { captures, .. } => {
            for c in captures.iter_mut() {
                patch_references(c, prefix, old_name, qualified_name);
            }
        }
        Expr::MakeRange(a, b) => {
            patch_references(a, prefix, old_name, qualified_name);
            patch_references(b, prefix, old_name, qualified_name);
        }
        Expr::Try { expr: e, .. } => {
            patch_references(e, prefix, old_name, qualified_name);
        }
        // Leaf nodes — nothing to patch
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::String(_)
        | Expr::Bool(_)
        | Expr::Unit
        | Expr::Hole
        | Expr::Var(_, _)
        | Expr::GetClosureVar(_) => {}
        // Effect-related nodes — unlikely in stdlib but handle for completeness
        Expr::Lambda { body, .. } => {
            patch_references(body, prefix, old_name, qualified_name);
        }
        Expr::WithHandlers { body, .. } => {
            patch_references(body, prefix, old_name, qualified_name);
        }
        Expr::HandleEffect { body, clauses, .. } => {
            patch_references(body, prefix, old_name, qualified_name);
            for clause in clauses.iter_mut() {
                patch_references(&mut clause.body, prefix, old_name, qualified_name);
            }
        }
        Expr::PerformEffect { args, .. } => {
            for a in args.iter_mut() {
                patch_references(a, prefix, old_name, qualified_name);
            }
        }
        Expr::Expect { actual, .. } => {
            patch_references(actual, prefix, old_name, qualified_name);
        }
    }
}

/// Check if a function name belongs to a stdlib module.
fn is_stdlib_function(prefix: &str, name: &str) -> bool {
    match prefix {
        "List" => matches!(name, "map" | "filter" | "fold" | "find"),
        "Option" => matches!(name, "map" | "flat_map"),
        "Result" => matches!(name, "map" | "map_err" | "and_then"),
        _ => false,
    }
}
