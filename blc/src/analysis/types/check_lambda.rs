use super::check_node::check_node;
use super::symbol_table::SymbolTable;
use super::type_def::Type;
use crate::diagnostics::Diagnostic;
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

    symbols.exit_scope();

    Type::Function(param_types, Box::new(body_type))
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
