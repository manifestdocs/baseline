use super::symbol_table::SymbolTable;
use super::type_def::Type;
use std::collections::HashSet;
use tree_sitter::Node;

/// Check if two types are compatible, treating Unknown as a wildcard.
/// Same-named Enums check payload types recursively for generic enforcement.
pub(super) fn types_compatible(a: &Type, b: &Type) -> bool {
    // Unwrap refined types to their base for compatibility checks
    let a = match a {
        Type::Refined(base, _) => base.as_ref(),
        other => other,
    };
    let b = match b {
        Type::Refined(base, _) => base.as_ref(),
        other => other,
    };
    if a == b {
        return true;
    }
    if *a == Type::Unknown || *b == Type::Unknown {
        return true;
    }
    // Unwrap Scoped<T> to T for compatibility (scoped handles can be consumed as T)
    let a = match a {
        Type::Scoped(inner) => inner.as_ref(),
        other => other,
    };
    let b = match b {
        Type::Scoped(inner) => inner.as_ref(),
        other => other,
    };
    if a == b {
        return true;
    }
    if *a == Type::Unknown || *b == Type::Unknown {
        return true;
    }
    match (a, b) {
        (Type::Var(_), _) | (_, Type::Var(_)) => true,
        // TypeParam matches itself (same name) or Unknown/Var
        (Type::TypeParam(na), Type::TypeParam(nb)) => na == nb,
        (Type::TypeParam(_), _) | (_, Type::TypeParam(_)) => false,
        (Type::Enum(na, va), Type::Enum(nb, vb)) => {
            if na != nb {
                return false;
            }
            // Same-named enums: check payload types are compatible
            for ((_, pa), (_, pb)) in va.iter().zip(vb.iter()) {
                for (ta, tb) in pa.iter().zip(pb.iter()) {
                    if !types_compatible(ta, tb) {
                        return false;
                    }
                }
            }
            true
        }
        (Type::List(ia), Type::List(ib)) => types_compatible(ia, ib),
        (Type::Map(ka, va), Type::Map(kb, vb)) => {
            types_compatible(ka, kb) && types_compatible(va, vb)
        }
        (Type::Set(ia), Type::Set(ib)) => types_compatible(ia, ib),
        (Type::Weak(ia), Type::Weak(ib)) => types_compatible(ia, ib),
        // Row polymorphism: open record { a: T, ..r } is compatible with any record
        // that has at least those fields with compatible types
        (Type::Record(fields_a, Some(_)), Type::Record(fields_b, _))
        | (Type::Record(fields_b, _), Type::Record(fields_a, Some(_))) => {
            // The open record's fields must all exist in the other record with compatible types
            fields_a.iter().all(|(k, ta)| {
                fields_b.get(k).is_some_and(|tb| types_compatible(ta, tb))
            })
        }
        (Type::Record(fa, None), Type::Record(fb, None)) => {
            // Closed records: exact field match
            fa.len() == fb.len()
                && fa.iter().all(|(k, ta)| {
                    fb.get(k).is_some_and(|tb| types_compatible(ta, tb))
                })
        }
        // Open record with Struct (structs have fixed fields)
        (Type::Record(fields, Some(_)), Type::Struct(_, sfields))
        | (Type::Struct(_, sfields), Type::Record(fields, Some(_))) => {
            fields.iter().all(|(k, ta)| {
                sfields.get(k).is_some_and(|tb| types_compatible(ta, tb))
            })
        }
        _ => false,
    }
}

/// Extract explicit type parameter names from a function_def's type_params node.
/// Returns an empty vec if no type_params present.
pub(super) fn extract_explicit_type_params(func_node: &Node, source: &str) -> Vec<String> {
    let mut cursor = func_node.walk();
    for child in func_node.children(&mut cursor) {
        if child.kind() == "type_params" {
            let mut params = Vec::new();
            let mut inner = child.walk();
            for tp in child.named_children(&mut inner) {
                if tp.kind() == "type_identifier" {
                    params.push(tp.utf8_text(source.as_bytes()).unwrap().to_string());
                }
            }
            return params;
        }
    }
    Vec::new()
}

/// Detect implicit type parameters: scan a function's param and return types
/// for single-letter uppercase type identifiers that aren't known types.
pub(super) fn detect_implicit_type_params(
    func_node: &Node,
    source: &str,
    symbols: &SymbolTable,
) -> Vec<String> {
    let mut type_params = Vec::new();
    let mut seen = HashSet::new();

    fn scan_for_type_params(
        node: &Node,
        source: &str,
        symbols: &SymbolTable,
        type_params: &mut Vec<String>,
        seen: &mut HashSet<String>,
    ) {
        if node.kind() == "type_identifier" {
            let name = node.utf8_text(source.as_bytes()).unwrap();
            // Implicit type params: single uppercase letter not matching a known type
            if name.len() == 1
                && name.chars().next().is_some_and(|c| c.is_ascii_uppercase())
                && !is_builtin_type_name(name)
                && symbols.lookup_type(name).is_none()
                && !seen.contains(name)
            {
                seen.insert(name.to_string());
                type_params.push(name.to_string());
            }
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            scan_for_type_params(&child, source, symbols, type_params, seen);
        }
    }

    if let Some(params) = func_node.child_by_field_name("params") {
        scan_for_type_params(&params, source, symbols, &mut type_params, &mut seen);
    }
    if let Some(ret) = func_node.child_by_field_name("return_type") {
        scan_for_type_params(&ret, source, symbols, &mut type_params, &mut seen);
    }
    type_params
}

pub(super) fn is_builtin_type_name(name: &str) -> bool {
    matches!(
        name,
        "Int" | "Float" | "String" | "Bool" | "Unit" | "List" | "Option" | "Result" | "Map"
            | "Set" | "Weak"
    )
}
