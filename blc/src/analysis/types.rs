use std::collections::HashMap;
use tree_sitter::Node;
use crate::diagnostics::{Diagnostic, Location};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Unit,
    Function(Vec<Type>, Box<Type>),
    List(Box<Type>),
    Struct(String, HashMap<String, Type>), // Name, Fields
    Record(HashMap<String, Type>),         // Anonymous record
    Module(String),                        // Module/Effect namespace
    Unknown,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::String => write!(f, "String"),
            Type::Bool => write!(f, "Bool"),
            Type::Unit => write!(f, "Unit"),
            Type::Function(args, ret) => {
                let args_str = args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "({}) -> {}", args_str, ret)
            }
            Type::List(inner) => write!(f, "List<{}>", inner),
            Type::Struct(name, _) => write!(f, "{}", name),
            Type::Record(fields) => {
                let mut sorted_fields: Vec<_> = fields.iter().collect();
                sorted_fields.sort_by_key(|(k, _)| *k);
                let fields_str = sorted_fields.iter().map(|(k, v)| format!("{}: {}", k, v)).collect::<Vec<_>>().join(", ");
                write!(f, "{{ {} }}", fields_str)
            }
            Type::Module(name) => write!(f, "Module({})", name),
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Type>>,
    types: HashMap<String, Type>, // Registry for named types (structs)
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            types: HashMap::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    fn insert_type(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }
}

pub fn check_types(root: &Node, source: &str, file: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let mut symbols = SymbolTable::new();

    check_node(root, source, file, &mut symbols, &mut diagnostics);
    diagnostics
}

fn check_node(
    node: &Node,
    source: &str,
    file: &str,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) -> Type {
    let k = node.kind();

    match k {
        "source_file" | "module_decl" => {
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                check_node(&child, source, file, symbols, diagnostics);
            }
            Type::Unit
        }
        "prelude_decl" => {
            Type::Unit
        }
        "effect_def" => {
            // effect_def: seq(..., 'effect', $.type_identifier, ...)
            // Manual search since field access creates panic
            let mut name = String::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                 if child.kind() == "type_identifier" {
                     name = child.utf8_text(source.as_bytes()).unwrap().to_string();
                     break;
                 }
            }
            if !name.is_empty() {
                symbols.insert_type(name.clone(), Type::Module(name));
            }
            Type::Unit
        }
        "type_def" => {
            // type Point = { x: Int, y: Int }
            let name_node = node.child_by_field_name("name")
                .unwrap_or_else(|| node.child(1).unwrap());
            let name = name_node.utf8_text(source.as_bytes()).unwrap_or("Unknown").to_string();

            // Def node is after '=' — use field if available, else positional
            let def_node_candidate = node.child_by_field_name("def")
                .unwrap_or_else(|| node.child(3).unwrap());
            let ty = parse_type(&def_node_candidate, source, symbols); 
            
            // If it's a record, wrap it in a Struct with the name
            let defined_type = match ty {
                Type::Record(fields) => Type::Struct(name.clone(), fields),
                _ => ty,
            };

            symbols.insert_type(name, defined_type);
            Type::Unit
        }
        "function_def" => {
            let name_node = node.child_by_field_name("name").unwrap();
            let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();

            // Check signature
            let sig_node = node.child_by_field_name("signature").unwrap();
            let function_type = parse_type(&sig_node, source, symbols);
            
            symbols.insert(name.clone(), function_type.clone());



            if let Type::Function(arg_types, ret_type) = &function_type {
                symbols.enter_scope();

                let body_node = node.child_by_field_name("body").unwrap();
                
                // Special handling if body is a lambda to bind args
                if body_node.kind() == "lambda" {
                     let child_count = body_node.named_child_count();
                     // Last identified named child is body, others are args
                     let arg_count = if child_count > 0 { child_count - 1 } else { 0 };
                     
                     if arg_count != arg_types.len() {
                          diagnostics.push(Diagnostic {
                             code: "TYP_005".to_string(),
                             severity: "error".to_string(),
                             location: Location {
                                 file: file.to_string(),
                                 line: body_node.start_position().row + 1,
                                 col: body_node.start_position().column + 1,
                             },
                             message: format!("Function `{}` expects {} arguments, lambda has {}", name, arg_types.len(), arg_count),
                             context: "Function implementation must match signature.".to_string(),
                             suggestions: vec![],
                         });
                     }

                     for i in 0..std::cmp::min(arg_count, arg_types.len()) {
                         let arg_node = body_node.named_child(i).unwrap();
                         if arg_node.kind() == "identifier" {
                             let arg_name = arg_node.utf8_text(source.as_bytes()).unwrap().to_string();
                             symbols.insert(arg_name, arg_types[i].clone());
                         }
                     }
                     
                     if child_count > 0 {
                         let lambda_body = body_node.named_child(child_count - 1).unwrap();
                         let body_type = check_node(&lambda_body, source, file, symbols, diagnostics);
                         
                         if body_type != **ret_type && body_type != Type::Unknown && **ret_type != Type::Unit {
                              diagnostics.push(Diagnostic {
                                 code: "TYP_006".to_string(),
                                 severity: "error".to_string(),
                                 location: Location {
                                     file: file.to_string(),
                                     line: lambda_body.start_position().row + 1,
                                     col: lambda_body.start_position().column + 1,
                                 },
                                 message: format!("Function `{}` declares return type {}, body returns {}", name, ret_type, body_type),
                                 context: "Function body return type must match signature.".to_string(),
                                 suggestions: vec![],
                             });
                         }
                     }
                } else {
                     let body_type = check_node(&body_node, source, file, symbols, diagnostics);
                     if body_type != **ret_type && body_type != Type::Unknown && **ret_type != Type::Unit {
                          diagnostics.push(Diagnostic {
                                 code: "TYP_006".to_string(),
                                 severity: "error".to_string(),
                                 location: Location {
                                     file: file.to_string(),
                                     line: body_node.start_position().row + 1,
                                     col: body_node.start_position().column + 1,
                                 },
                                 message: format!("Function `{}` declares return type {}, body returns {}", name, ret_type, body_type),
                                 context: "Function body return type must match signature.".to_string(),
                                 suggestions: vec![],
                             });
                     }
                }

                symbols.exit_scope();
            }

            Type::Unit 
        }
        "let_binding" => {
            if let Some(pattern) = node.named_child(0) {
                 if let Some(expr) = node.named_child(1) {
                     let expr_type = check_node(&expr, source, file, symbols, diagnostics);
                     bind_pattern(&pattern, expr_type, source, symbols);
                 }
            }
            Type::Unit
        }
        "call_expression" => {
            let func_node = node.named_child(0).unwrap();
            let func_type = check_node(&func_node, source, file, symbols, diagnostics);
            
            if let Type::Function(arg_types, ret_type) = func_type {
                let total_children = node.named_child_count();
                // 0 is func, 1..N are args
                let params_provided = if total_children > 1 { total_children - 1 } else { 0 };

                if params_provided != arg_types.len() {
                    diagnostics.push(Diagnostic {
                        code: "TYP_007".to_string(),
                        severity: "error".to_string(),
                        location: Location {
                             file: file.to_string(),
                             line: node.start_position().row + 1,
                             col: node.start_position().column + 1,
                        },
                        message: format!("Function call expects {} arguments, found {}", arg_types.len(), params_provided),
                        context: "Argument count mismatch.".to_string(),
                        suggestions: vec![],
                    });
                }
                
                for i in 0..std::cmp::min(params_provided, arg_types.len()) {
                    let arg_expr = node.named_child(i + 1).unwrap();
                    let arg_type = check_node(&arg_expr, source, file, symbols, diagnostics);
                    
                    if arg_type != arg_types[i] && arg_type != Type::Unknown {
                         diagnostics.push(Diagnostic {
                            code: "TYP_008".to_string(),
                            severity: "error".to_string(),
                            location: Location {
                                 file: file.to_string(),
                                 line: arg_expr.start_position().row + 1,
                                 col: arg_expr.start_position().column + 1,
                            },
                            message: format!("Argument {} mismatch: expected {}, found {}", i+1, arg_types[i], arg_type),
                            context: "Argument type must match function signature.".to_string(),
                            suggestions: vec![],
                        });
                    }
                }
                *ret_type
            } else if func_type == Type::Unknown {
                Type::Unknown
            } else {
                 diagnostics.push(Diagnostic {
                    code: "TYP_009".to_string(),
                    severity: "error".to_string(),
                    location: Location {
                         file: file.to_string(),
                         line: func_node.start_position().row + 1,
                         col: func_node.start_position().column + 1,
                    },
                    message: format!("Called expression is not a function, it is {}", func_type),
                    context: "Only functions can be called.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "struct_expression" => {
            let type_name_node = node.named_child(0).unwrap();
            let type_name = type_name_node.utf8_text(source.as_bytes()).unwrap();
            
            if let Some(Type::Struct(name, fields)) = symbols.lookup_type(type_name).cloned() {
                 let mut initialized_fields = std::collections::HashSet::new();
                 
                 let count = node.named_child_count();
                 for i in 1..count {
                     let field_init = node.named_child(i).unwrap();
                     let fname_node = field_init.child(0).unwrap(); 
                     let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                     
                     let fexpr_node = field_init.child(2).unwrap();
                     let ftype = check_node(&fexpr_node, source, file, symbols, diagnostics);
                     
                     if let Some(expected_type) = fields.get(&fname) {
                         if ftype != *expected_type && ftype != Type::Unknown {
                              diagnostics.push(Diagnostic {
                                code: "TYP_010".to_string(),
                                severity: "error".to_string(),
                                location: Location {
                                    file: file.to_string(),
                                    line: fexpr_node.start_position().row + 1,
                                    col: fexpr_node.start_position().column + 1,
                                },
                                message: format!("Field `{}` expects {}, found {}", fname, expected_type, ftype),
                                context: "Struct field type mismatch.".to_string(),
                                suggestions: vec![],
                            });
                         }
                         initialized_fields.insert(fname);
                     } else {
                          diagnostics.push(Diagnostic {
                            code: "TYP_011".to_string(),
                            severity: "error".to_string(),
                            location: Location {
                                file: file.to_string(),
                                line: fname_node.start_position().row + 1,
                                col: fname_node.start_position().column + 1,
                            },
                            message: format!("Struct `{}` has no field `{}`", name, fname),
                            context: "Field not defined in struct.".to_string(),
                            suggestions: vec![],
                        });
                     }
                 }
                 
                 for required_field in fields.keys() {
                     if !initialized_fields.contains(required_field) {
                          diagnostics.push(Diagnostic {
                            code: "TYP_012".to_string(),
                            severity: "error".to_string(),
                            location: Location {
                                file: file.to_string(),
                                line: node.start_position().row + 1,
                                col: node.start_position().column + 1,
                            },
                            message: format!("Missing field `{}`", required_field),
                            context: "All struct fields must be initialized.".to_string(),
                            suggestions: vec![],
                        });
                     }
                 }
                 
                 Type::Struct(name, fields)
            } else {
                 diagnostics.push(Diagnostic {
                    code: "TYP_013".to_string(),
                    severity: "error".to_string(),
                    location: Location {
                        file: file.to_string(),
                        line: type_name_node.start_position().row + 1,
                        col: type_name_node.start_position().column + 1,
                    },
                    message: format!("Unknown type `{}`", type_name),
                    context: "Type must be defined before use.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "field_expression" => {
            let obj_node = node.named_child(0).unwrap();
            let field_node = node.named_child(1).unwrap();
            let field_name = field_node.utf8_text(source.as_bytes()).unwrap();
            
            let obj_type = check_node(&obj_node, source, file, symbols, diagnostics);
            
            match obj_type {
                Type::Struct(name, fields) => {
                    if let Some(ty) = fields.get(field_name) {
                        ty.clone()
                    } else {
                        diagnostics.push(Diagnostic {
                            code: "TYP_014".to_string(),
                            severity: "error".to_string(),
                            location: Location {
                                file: file.to_string(),
                                line: field_node.start_position().row + 1,
                                col: field_node.start_position().column + 1,
                            },
                            message: format!("Struct `{}` has no field `{}`", name, field_name),
                            context: "Field access error.".to_string(),
                            suggestions: vec![],
                        });
                        Type::Unknown
                    }
                }
                Type::Module(_) => {
                    // Allow field access on modules (for effects)
                    // We return Unknown (or a generic Function) to pass type checking
                    // The actual Effect checking happens in effects.rs
                    Type::Unknown 
                }
                Type::Unknown => Type::Unknown,
                _ => {
                    diagnostics.push(Diagnostic {
                        code: "TYP_015".to_string(),
                        severity: "error".to_string(),
                        location: Location {
                            file: file.to_string(),
                            line: obj_node.start_position().row + 1,
                            col: obj_node.start_position().column + 1,
                        },
                        message: format!("Type {} excludes field access", obj_type),
                        context: "Only Structs, Records, and Modules support field access.".to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown
                }
            }
        }
        "block" => {
            symbols.enter_scope();
            let mut last_type = Type::Unit;
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "let_binding" {
                    check_node(&child, source, file, symbols, diagnostics);
                    last_type = Type::Unit;
                } else if child.kind().ends_with("_expression") || child.kind() == "identifier" || child.kind().ends_with("_literal") || child.kind() == "literal" {
                    last_type = check_node(&child, source, file, symbols, diagnostics);
                }
            }
            symbols.exit_scope();
            last_type
        }
        "identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap();
            if let Some(ty) = symbols.lookup(name) {
                ty.clone()
            } else {
                 diagnostics.push(Diagnostic {
                    code: "TYP_002".to_string(),
                    severity: "error".to_string(),
                    location: Location {
                        file: file.to_string(),
                        line: node.start_position().row + 1,
                        col: node.start_position().column + 1,
                    },
                    message: format!("Undefined variable `{}`", name),
                    context: "Variable must be defined before use.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "effect_identifier" | "effectful_identifier" => {
            // TODO: Lookup in a real Effect Registry for return types.
            // For now, assume most effects return Unit or Unknown to allow flow.
            Type::Unknown 
        }
        "literal" | "parenthesized_expression" => {
             if let Some(child) = node.named_child(0) {
                 check_node(&child, source, file, symbols, diagnostics)
             } else {
                 Type::Unit
             }
        }
        "integer_literal" => Type::Int,
        "float_literal" => Type::Float,
        "string_literal" => Type::String,
        "boolean_literal" => Type::Bool,
        "binary_expression" => {
            let left_type = check_node(&node.named_child(0).unwrap(), source, file, symbols, diagnostics);
            let right_type = check_node(&node.named_child(1).unwrap(), source, file, symbols, diagnostics);
            let op_str = node.child(1).unwrap().utf8_text(source.as_bytes()).unwrap();
            
            match op_str {
                "+" | "-" | "*" | "/" | "%" => {
                    if left_type == Type::Int && right_type == Type::Int {
                        Type::Int
                    } else if left_type == Type::Float && right_type == Type::Float {
                        Type::Float
                    } else {
                        if left_type != Type::Unknown && right_type != Type::Unknown {
                            diagnostics.push(Diagnostic {
                                code: "TYP_001".to_string(),
                                severity: "error".to_string(),
                                location: Location {
                                    file: file.to_string(),
                                    line: node.start_position().row + 1,
                                    col: node.start_position().column + 1,
                                },
                                message: format!("Binary operator `{}` requires matching Int or Float operands, found {} and {}", op_str, left_type, right_type),
                                context: "Arithmetic operations require matching numeric types.".to_string(),
                                suggestions: vec![],
                            });
                        }
                        Type::Unknown
                    }
                }
                "==" | "!=" | "<" | ">" | "<=" | ">=" => Type::Bool,
                _ => Type::Unknown
            }
        }
        "if_expression" => {
             let cond = node.named_child(0).unwrap();
             let cond_type = check_node(&cond, source, file, symbols, diagnostics);
             
             if cond_type != Type::Bool && cond_type != Type::Unknown {
                   diagnostics.push(Diagnostic {
                    code: "TYP_003".to_string(),
                    severity: "error".to_string(),
                    location: Location {
                        file: file.to_string(),
                        line: cond.start_position().row + 1,
                        col: cond.start_position().column + 1,
                    },
                    message: format!("If condition must be Boolean, found {}", cond_type),
                    context: "Control flow conditions must evaluate to true or false.".to_string(),
                    suggestions: vec![],
                });
             }

             let then_branch = node.named_child(1).unwrap();
             let then_type = check_node(&then_branch, source, file, symbols, diagnostics);
             
             if let Some(else_branch) = node.named_child(2) {
                 let else_type = check_node(&else_branch, source, file, symbols, diagnostics);
                 
                 if then_type != else_type && then_type != Type::Unknown && else_type != Type::Unknown {
                      diagnostics.push(Diagnostic {
                        code: "TYP_004".to_string(),
                        severity: "error".to_string(),
                        location: Location {
                            file: file.to_string(),
                            line: else_branch.start_position().row + 1,
                            col: else_branch.start_position().column + 1,
                        },
                        message: format!("If branches match mismatch: then is {}, else is {}", then_type, else_type),
                        context: "Both branches of an if expression must return the same type.".to_string(),
                        suggestions: vec![],
                    });
                    Type::Unknown 
                 } else {
                     then_type
                 }
             } else {
                 Type::Unit 
             }

        }
        "list_expression" => {
            // [1, 2, 3]
            let mut element_type = Type::Unknown;
            let mut first = true;
            
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.kind() == "[" || child.kind() == "]" || child.kind() == "," {
                    continue;
                }
                
                let ty = check_node(&child, source, file, symbols, diagnostics);
                
                if first {
                    element_type = ty;
                    first = false;
                } else if ty != element_type && ty != Type::Unknown && element_type != Type::Unknown {
                     diagnostics.push(Diagnostic {
                        code: "TYP_016".to_string(),
                        severity: "error".to_string(),
                        location: Location {
                            file: file.to_string(),
                            line: child.start_position().row + 1,
                            col: child.start_position().column + 1,
                        },
                        message: format!("List element type mismatch: expected {}, found {}", element_type, ty),
                        context: "All elements in a list must have the same type.".to_string(),
                        suggestions: vec![],
                    });
                }
            }
            
            Type::List(Box::new(element_type))
        }
        "lambda" => {
             // |x| body
             let mut arg_types = Vec::new();
             
             // Last named child is body, others are args
             let count = node.named_child_count();
             symbols.enter_scope();
             
             for i in 0..count-1 {
                 let arg = node.named_child(i).unwrap();
                 if arg.kind() == "identifier" {
                      let name = arg.utf8_text(source.as_bytes()).unwrap().to_string();
                      symbols.insert(name, Type::Unknown);
                      arg_types.push(Type::Unknown);
                 }
             }
             
             let body = node.named_child(count-1).unwrap();
             let body_type = check_node(&body, source, file, symbols, diagnostics);
             
             symbols.exit_scope();
             
             Type::Function(arg_types, Box::new(body_type))
        }
        "match_expression" => {
             let expr_node = node.named_child(0).unwrap();
             let expr_type = check_node(&expr_node, source, file, symbols, diagnostics);
             
             let mut ret_type = Type::Unknown;
             let mut first = true;
             
             let count = node.named_child_count();
             for i in 1..count {
                  let arm = node.named_child(i).unwrap();
                  let pat = arm.child(0).unwrap();
                  let body = arm.child(2).unwrap();
                  
                  symbols.enter_scope();
                  check_pattern(&pat, &expr_type, source, symbols, diagnostics);
                  
                  let body_type = check_node(&body, source, file, symbols, diagnostics);
                  symbols.exit_scope();
                  
                  if first {
                      ret_type = body_type;
                      first = false;
                  } else if body_type != ret_type && body_type != Type::Unknown && ret_type != Type::Unknown {
                       diagnostics.push(Diagnostic {
                            code: "TYP_017".to_string(),
                            severity: "error".to_string(),
                            location: Location {
                                file: file.to_string(),
                                line: body.start_position().row + 1,
                                col: body.start_position().column + 1,
                            },
                            message: format!("Match arm mismatch: expected {}, found {}", ret_type, body_type),
                            context: "All match arms must return same type.".to_string(),
                            suggestions: vec![],
                        });
                  }
             }
             ret_type
        }
        "pipe_expression" => {
            let left_node = node.named_child(0).unwrap();
            let right_node = node.named_child(1).unwrap();
            
            let left_type = check_node(&left_node, source, file, symbols, diagnostics);
            let right_type = check_node(&right_node, source, file, symbols, diagnostics);
            
            if let Type::Function(args, ret) = right_type {
                 if args.len() != 1 {
                       diagnostics.push(Diagnostic {
                            code: "TYP_018".to_string(),
                            severity: "error".to_string(),
                            location: Location {
                                file: file.to_string(),
                                line: right_node.start_position().row + 1,
                                col: right_node.start_position().column + 1,
                            },
                            message: format!("Pipe function expects 1 argument, found {}", args.len()),
                            context: "Pipe operator expects a unary function.".to_string(),
                            suggestions: vec![],
                        });
                 } else if args[0] != left_type && left_type != Type::Unknown && args[0] != Type::Unknown {
                       diagnostics.push(Diagnostic {
                            code: "TYP_019".to_string(),
                            severity: "error".to_string(),
                            location: Location {
                                file: file.to_string(),
                                line: left_node.start_position().row + 1,
                                col: left_node.start_position().column + 1,
                            },
                            message: format!("Pipe argument mismatch: expected {}, found {}", args[0], left_type),
                            context: "Argument type must match function signature.".to_string(),
                            suggestions: vec![],
                        });
                 }
                 *ret
            } else if right_type == Type::Unknown {
                Type::Unknown 
            } else {
                 diagnostics.push(Diagnostic {
                    code: "TYP_020".to_string(),
                    severity: "error".to_string(),
                    location: Location {
                        file: file.to_string(),
                        line: right_node.start_position().row + 1,
                        col: right_node.start_position().column + 1,
                    },
                    message: format!("Pipe target is not a function, it is {}", right_type),
                    context: "Left side must be piped into a function.".to_string(),
                    suggestions: vec![],
                });
                Type::Unknown
            }
        }
        "type_identifier" => {
             // In expression context, this is a module/effect reference
             let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
             if let Some(ty) = symbols.lookup_type(&name) {
                 ty.clone()
             } else {
                 // Assume it might be a module not yet defined or external
                 Type::Module(name) 
             }
        }
        _ => {
            Type::Unit // simplified
        }
    }
}

fn parse_type(node: &Node, source: &str, symbols: &SymbolTable) -> Type {
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
                    if let Some(ty) = symbols.lookup_type(name) {
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
             let right_type = parse_type(&right, source, symbols);
             
             if left.kind() == "tuple_type" {
                 // Decompose tuple into args
                 let mut args = Vec::new();
                 let mut cursor = left.walk();
                 
                 // tuple_type children: (, type, ,, type, ...)
                 for child in left.children(&mut cursor) {
                     let k = child.kind();
                     if k != "(" && k != "," && k != ")" {
                         args.push(parse_type(&child, source, symbols));
                     }
                 }
                 Type::Function(args, Box::new(right_type))
             } else {
                  let left_type = parse_type(&left, source, symbols);
                  Type::Function(vec![left_type], Box::new(right_type))
             }
        }
        "function_type" => {
            // (T, U) -> V in other contexts
            let mut args = Vec::new();
            let mut cursor = node.walk();
             for child in node.children(&mut cursor) {
                 if child.kind() == "->" { break; }
                 if child.kind() != "(" && child.kind() != "," && child.kind() != ")" {
                     args.push(parse_type(&child, source, symbols));
                 }
             }
             let ret_node = node.child(node.child_count() - 1).unwrap();
             let ret = parse_type(&ret_node, source, symbols);
             Type::Function(args, Box::new(ret))
        }
        "tuple_type" => {
             if node.named_child_count() == 0 {
                 Type::Unit
             } else {
                 Type::Unknown
             }
        }
        "record_type" => {
            let mut fields = HashMap::new();
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                 if child.kind() == "record_field_def" {
                     let name_node = child.child(0).unwrap();
                     let type_node = child.child(2).unwrap();
                     
                     let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();
                     let ty = parse_type(&type_node, source, symbols);
                     fields.insert(name, ty);
                 }
            }
            Type::Record(fields)
        }

        "generic_type" => {
            // List<T>
            let name_node = node.child(0).unwrap();
            let name = name_node.utf8_text(source.as_bytes()).unwrap();
            
            if name == "List" {
                 // Format: identifier < type >
                 // Child 0: id, 1: <, 2: type, 3: >
                 // Actually commaSep1 might mean children are interspersed with comma
                 // Simple generic_type: name < type >
                 
                 let arg_node = node.child(2).unwrap();
                 let inner_type = parse_type(&arg_node, source, symbols);
                 Type::List(Box::new(inner_type))
            } else {
                Type::Unknown 
            }
        }
        _ => Type::Unknown
    }
}

fn bind_pattern(node: &Node, ty: Type, source: &str, symbols: &mut SymbolTable) {
    if node.kind() == "identifier" {
         let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
         symbols.insert(name, ty);
    }
}

fn check_pattern(node: &Node, expected_type: &Type, source: &str, symbols: &mut SymbolTable, diagnostics: &mut Vec<Diagnostic>) {
    match node.kind() {
        "wildcard_pattern" => {},
        "identifier" => {
             let name = node.utf8_text(source.as_bytes()).unwrap().to_string();
             symbols.insert(name, expected_type.clone());
        },
        "literal" => {
             if let Some(child) = node.named_child(0) {
                 check_pattern(&child, expected_type, source, symbols, diagnostics);
             }
        },
        "integer_literal" => {
             // Basic check
        },
        _ => {}
    }
}
