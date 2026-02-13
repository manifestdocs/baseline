use super::check_node::check_node;
use super::symbol_table::SymbolTable;
use super::type_def::Type;
use crate::diagnostics::{Diagnostic, Location, Severity};
use tree_sitter::Node;

/// Check a lambda with expected parameter types inferred from the call site.
/// Returns the lambda's type with concrete param types instead of Unknown.
pub(super) fn check_lambda_with_expected(
    node: &Node,
    expected_params: &[Type],
    _expected_ret: &Type,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) -> Type {
    let count = node.named_child_count();
    let arg_count = if count > 0 { count - 1 } else { 0 };

    symbols.enter_scope();

    let mut param_types = Vec::new();
    for i in 0..arg_count {
        let arg = node.named_child(i).unwrap();
        if arg.kind() == "identifier" {
            let name = arg.utf8_text(source.as_bytes()).unwrap().to_string();
            let ty = if i < expected_params.len() {
                expected_params[i].clone()
            } else {
                Type::Unknown
            };
            symbols.insert(name, ty.clone());
            param_types.push(ty);
        }
    }

    let body_type = if count > 0 {
        let body = node.named_child(count - 1).unwrap();
        check_node(&body, source, file, symbols, diagnostics)
    } else {
        Type::Unit
    };

    // Escape analysis: if any param is Scoped, the body must not return a Scoped type
    let has_scoped_param = expected_params.iter().any(|p| matches!(p, Type::Scoped(_)));
    if has_scoped_param && contains_scoped(&body_type) {
        diagnostics.push(Diagnostic {
            code: "RES_001".to_string(),
            severity: Severity::Error,
            location: Location::from_node(file, node),
            message: "scoped resource handle cannot escape closure".to_string(),
            context: format!(
                "closure returns {}, which contains a scoped resource handle",
                body_type
            ),
            suggestions: vec![],
        });
    }

    symbols.exit_scope();

    Type::Function(param_types, Box::new(body_type))
}

/// Check if a type contains `Scoped` anywhere in its structure.
fn contains_scoped(ty: &Type) -> bool {
    match ty {
        Type::Scoped(_) => true,
        Type::List(inner) | Type::Set(inner) => contains_scoped(inner),
        Type::Map(k, v) => contains_scoped(k) || contains_scoped(v),
        Type::Tuple(elems) => elems.iter().any(contains_scoped),
        Type::Function(params, ret) => params.iter().any(contains_scoped) || contains_scoped(ret),
        Type::Enum(_, variants) => variants
            .iter()
            .any(|(_, payloads)| payloads.iter().any(contains_scoped)),
        Type::Record(fields, _) => fields.values().any(contains_scoped),
        Type::Struct(_, fields) => fields.values().any(contains_scoped),
        Type::Refined(base, _) => contains_scoped(base),
        _ => false,
    }
}

pub(super) fn bind_pattern(node: &Node, ty: Type, source: &str, symbols: &mut SymbolTable) {
    match node.kind() {
        "identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
            symbols.insert(name, ty);
        }
        "tuple_pattern" => {
            let count = node.named_child_count();
            if let Type::Tuple(ref elem_types) = ty {
                for i in 0..count {
                    let sub_pat = node.named_child(i).unwrap();
                    let elem_ty = if i < elem_types.len() {
                        elem_types[i].clone()
                    } else {
                        Type::Unknown
                    };
                    bind_pattern(&sub_pat, elem_ty, source, symbols);
                }
            } else {
                for i in 0..count {
                    let sub_pat = node.named_child(i).unwrap();
                    bind_pattern(&sub_pat, Type::Unknown, source, symbols);
                }
            }
        }
        _ => {}
    }
}
