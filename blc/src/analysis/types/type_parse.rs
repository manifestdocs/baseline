use super::symbol_table::SymbolTable;
use super::type_def::Type;
use std::collections::{HashMap, HashSet};
use tree_sitter::Node;

/// Infer the expected type for a typed hole from its parent context.
pub(super) fn infer_expected_type(node: &Node, source: &str, symbols: &SymbolTable) -> Type {
    if let Some(parent) = node.parent() {
        match parent.kind() {
            "let_binding" => {
                // let x: T = ?? -> look for type annotation
                if let Some(ann_node) = parent.child_by_field_name("type") {
                    if let Some(type_node) = ann_node.named_child(0) {
                        return parse_type(&type_node, source, symbols);
                    }
                }
            }
            "function_def" => {
                // fn foo() -> T = ?? -> look for return type
                if let Some(ret_node) = parent.child_by_field_name("return_type") {
                    return parse_type(&ret_node, source, symbols);
                }
            }
            _ => {}
        }
    }
    Type::Unknown
}

/// Extract the expression node from a call argument (handles named_argument).
pub(super) fn call_arg_expr<'a>(arg: &Node<'a>) -> Node<'a> {
    if arg.kind() == "named_argument" {
        // named_argument: name ':' expression
        // The expression is the last named child
        let count = arg.named_child_count();
        arg.named_child(count - 1).unwrap_or(*arg)
    } else {
        *arg
    }
}

pub(super) fn parse_type(node: &Node, source: &str, symbols: &SymbolTable) -> Type {
    parse_type_ext(node, source, symbols, &HashSet::new())
}

/// Parse a type expression, treating names in `type_params` as TypeParam.
pub(super) fn parse_type_ext(
    node: &Node,
    source: &str,
    symbols: &SymbolTable,
    type_params: &HashSet<String>,
) -> Type {
    match node.kind() {
        "type_identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap();
            match name {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "String" => Type::String,
                "Bool" => Type::Bool,
                "Unit" => Type::Unit,
                _ => {
                    if type_params.contains(name) {
                        Type::TypeParam(name.to_string())
                    } else if let Some(ty) = symbols.lookup_type(name) {
                        ty.clone()
                    } else {
                        Type::Unknown
                    }
                }
            }
        }
        "type_signature" => {
            // A -> B
            // If A is a Tuple, we decompose it into args.
            let left = node.child(0).unwrap();
            let right = node.child(node.child_count() - 1).unwrap();
            let right_type = parse_type_ext(&right, source, symbols, type_params);

            if left.kind() == "tuple_type" {
                // Decompose tuple into args
                let mut args = Vec::new();
                let mut cursor = left.walk();

                // tuple_type children: (, type, ,, type, ...)
                for child in left.children(&mut cursor) {
                    let k = child.kind();
                    if k != "(" && k != "," && k != ")" {
                        args.push(parse_type_ext(&child, source, symbols, type_params));
                    }
                }
                Type::Function(args, Box::new(right_type))
            } else {
                let left_type = parse_type_ext(&left, source, symbols, type_params);
                Type::Function(vec![left_type], Box::new(right_type))
            }
        }
        "function_type" => {
            // (T, U) -> V in other contexts
            let mut args = Vec::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "->" {
                    break;
                }
                if child.kind() != "(" && child.kind() != "," && child.kind() != ")" {
                    args.push(parse_type_ext(&child, source, symbols, type_params));
                }
            }
            let ret_node = node.child(node.child_count() - 1).unwrap();
            let ret = parse_type_ext(&ret_node, source, symbols, type_params);
            Type::Function(args, Box::new(ret))
        }
        "tuple_type" => {
            let count = node.named_child_count();
            if count == 0 {
                Type::Unit
            } else if count == 1 {
                // (T) is just parenthesized, not a 1-tuple
                parse_type_ext(&node.named_child(0).unwrap(), source, symbols, type_params)
            } else {
                let elems: Vec<Type> = (0..count)
                    .map(|i| {
                        parse_type_ext(
                            &node.named_child(i).unwrap(),
                            source,
                            symbols,
                            type_params,
                        )
                    })
                    .collect();
                Type::Tuple(elems)
            }
        }
        "record_type" => {
            let mut fields = HashMap::new();
            let mut row_var = None;
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "record_field_def" {
                    let name_node = child.child(0).unwrap();
                    let type_node = child.child(2).unwrap();

                    let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();
                    let ty = parse_type_ext(&type_node, source, symbols, type_params);
                    fields.insert(name, ty);
                } else if child.kind() == "row_variable" {
                    // ..r — row variable for open record types
                    // Use the InferCtx's next_var counter convention
                    // For now, generate a unique var ID from the row variable's byte position
                    row_var = Some(child.start_byte() as u32);
                }
            }
            Type::Record(fields, row_var)
        }

        "generic_type" => {
            // Name<T, ...> — named_child(0) is name, named_child(1..) are type args
            let name_node = node.named_child(0).unwrap();
            let name = name_node.utf8_text(source.as_bytes()).unwrap();

            match name {
                "List" => {
                    let arg = node.named_child(1).unwrap();
                    let inner = parse_type_ext(&arg, source, symbols, type_params);
                    Type::List(Box::new(inner))
                }
                "Map" => {
                    let key = node.named_child(1).unwrap();
                    let val = node.named_child(2).unwrap();
                    Type::Map(
                        Box::new(parse_type_ext(&key, source, symbols, type_params)),
                        Box::new(parse_type_ext(&val, source, symbols, type_params)),
                    )
                }
                "Set" => {
                    let arg = node.named_child(1).unwrap();
                    Type::Set(Box::new(parse_type_ext(&arg, source, symbols, type_params)))
                }
                "Weak" => {
                    let arg = node.named_child(1).unwrap();
                    Type::Weak(Box::new(parse_type_ext(&arg, source, symbols, type_params)))
                }
                "Option" => {
                    let arg = node.named_child(1).unwrap();
                    let inner = parse_type_ext(&arg, source, symbols, type_params);
                    Type::Enum(
                        "Option".to_string(),
                        vec![
                            ("Some".to_string(), vec![inner]),
                            ("None".to_string(), vec![]),
                        ],
                    )
                }
                "Result" => {
                    let ok_node = node.named_child(1).unwrap();
                    let ok_type = parse_type_ext(&ok_node, source, symbols, type_params);
                    let err_type = node
                        .named_child(2)
                        .map(|n| parse_type_ext(&n, source, symbols, type_params))
                        .unwrap_or(Type::Unknown);
                    Type::Enum(
                        "Result".to_string(),
                        vec![
                            ("Ok".to_string(), vec![ok_type]),
                            ("Err".to_string(), vec![err_type]),
                        ],
                    )
                }
                _ => Type::Unknown,
            }
        }
        "option_type" => {
            // T? desugars to Option<T>
            let inner_node = node.named_child(0).unwrap();
            let inner = parse_type_ext(&inner_node, source, symbols, type_params);
            Type::Enum(
                "Option".to_string(),
                vec![
                    ("Some".to_string(), vec![inner]),
                    ("None".to_string(), vec![]),
                ],
            )
        }
        _ => Type::Unknown,
    }
}
