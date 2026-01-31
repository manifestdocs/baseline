use std::collections::HashMap;
use tree_sitter::Node;

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue<'a> {
    Int(i64),
    String(String),
    Bool(bool),
    Unit,
    Function(Vec<String>, Node<'a>),
    Struct(String, HashMap<String, RuntimeValue<'a>>),
    Record(HashMap<String, RuntimeValue<'a>>),
    List(Vec<RuntimeValue<'a>>),
}

impl<'a> std::fmt::Display for RuntimeValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Int(i) => write!(f, "{}", i),
            RuntimeValue::String(s) => write!(f, "{}", s), // TODO: quote strings?
            RuntimeValue::Bool(b) => write!(f, "{}", b),
            RuntimeValue::Unit => write!(f, "()"),
            RuntimeValue::Function(args, _) => write!(f, "|{}| ...", args.join(", ")),
            RuntimeValue::Struct(name, _fields) => {
                 write!(f, "{} {{ ... }}", name)
            }
            RuntimeValue::Record(_) => write!(f, "{{ ... }}"),
            RuntimeValue::List(vals) => {
                 let s = vals.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ");
                 write!(f, "[{}]", s)
            }
        }
    }
}

pub struct Context<'a> {
    scopes: Vec<HashMap<String, RuntimeValue<'a>>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn set(&mut self, name: String, val: RuntimeValue<'a>) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, val);
        }
    }

    pub fn get(&self, name: &str) -> Option<&RuntimeValue<'a>> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }
}

pub fn eval<'a>(node: &Node<'a>, source: &str, context: &mut Context<'a>) -> Result<RuntimeValue<'a>, String> {
    match node.kind() {
        "source_file" | "module_decl" => {
            let mut last_val = RuntimeValue::Unit;
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                last_val = eval(&child, source, context)?;
            }
            Ok(last_val)
        }
        "function_def" => {
            // main = |args| { ... }
            // Bind name to the function value (which is likely a lambda)
            let name_node = node.child_by_field_name("name").unwrap();
            let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();
            
            let body_node = node.child_by_field_name("body").unwrap();
            let body_val = eval(&body_node, source, context)?;
            
            context.set(name, body_val);
            Ok(RuntimeValue::Unit)
        }
        "lambda" => {
            let mut args = Vec::new();
            let count = node.named_child_count();
            // Last one is body
            for i in 0..count-1 {
                let child = node.named_child(i).unwrap();
                if child.kind() == "identifier" {
                    args.push(child.utf8_text(source.as_bytes()).unwrap().to_string());
                }
            }
            
            let body = node.named_child(count-1).unwrap();
            Ok(RuntimeValue::Function(args, body))
        }
        "call_expression" => {
             // func(arg1, arg2)
             let func_node = node.named_child(0).unwrap();
             let func_val = eval(&func_node, source, context)?;
             
             match func_val {
                 RuntimeValue::Function(param_names, body_node) => {
                     // Eval args
                     let mut arg_vals = Vec::new();
                     let total_children = node.named_child_count();
                     for i in 1..total_children {
                         let arg_node = node.named_child(i).unwrap();
                         arg_vals.push(eval(&arg_node, source, context)?);
                     }
                     
                     if arg_vals.len() != param_names.len() {
                         return Err(format!("Arg count mismatch: expected {}, got {}", param_names.len(), arg_vals.len()));
                     }
                     
                     // New Scope
                     context.enter_scope();
                     for (name, val) in param_names.iter().zip(arg_vals.into_iter()) {
                         context.set(name.clone(), val);
                     }
                     
                     let result = eval(&body_node, source, context);
                     context.exit_scope();
                     result
                 }
                 _ => Err(format!("Not a function: {}", func_node.utf8_text(source.as_bytes()).unwrap()))
             }
        }
        "block" => {
            context.enter_scope();
            let mut last_val = RuntimeValue::Unit;
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                 if child.kind().ends_with("_expression") || child.kind().ends_with("_literal") || child.kind() == "identifier" || child.kind() == "let_binding" || child.kind() == "literal" {
                     last_val = eval(&child, source, context)?;
                 }
            }
            context.exit_scope();
            Ok(last_val)
        }
        "let_binding" => {
            let pattern = node.named_child(0).unwrap();
            let expr = node.named_child(1).unwrap();
            let val = eval(&expr, source, context)?;
            
            if pattern.kind() == "identifier" {
                let name = pattern.utf8_text(source.as_bytes()).unwrap().to_string();
                context.set(name, val);
            }
            Ok(RuntimeValue::Unit)
        }
        "identifier" => {
            let name = node.utf8_text(source.as_bytes()).unwrap();
            context.get(name).cloned().ok_or_else(|| format!("Undefined variable: {}", name))
        }
        "integer_literal" => {
            let text = node.utf8_text(source.as_bytes()).unwrap();
            let val = text.parse::<i64>().map_err(|_| "Invalid integer".to_string())?;
            Ok(RuntimeValue::Int(val))
        }
        "string_literal" => {
            let text = node.utf8_text(source.as_bytes()).unwrap();
            // Remove quotes
            let s = text.trim_matches('"').to_string(); // Simplified unescaping
            Ok(RuntimeValue::String(s))
        }
        "boolean_literal" => {
             let text = node.utf8_text(source.as_bytes()).unwrap();
             Ok(RuntimeValue::Bool(text == "true"))
        }
        "binary_expression" => {
             let left = eval(&node.named_child(0).unwrap(), source, context)?;
             let right = eval(&node.named_child(1).unwrap(), source, context)?;
             let op = node.child(1).unwrap().utf8_text(source.as_bytes()).unwrap();
             
             match (left, right) {
                 (RuntimeValue::Int(l), RuntimeValue::Int(r)) => match op {
                     "+" => Ok(RuntimeValue::Int(l + r)),
                     "-" => Ok(RuntimeValue::Int(l - r)),
                     "*" => Ok(RuntimeValue::Int(l * r)),
                     "/" => {
                         if r == 0 {
                             Err("Division by zero".to_string())
                         } else {
                             Ok(RuntimeValue::Int(l / r))
                         }
                     }
                     "%" => {
                         if r == 0 {
                             Err("Modulo by zero".to_string())
                         } else {
                             Ok(RuntimeValue::Int(l % r))
                         }
                     }
                     "==" => Ok(RuntimeValue::Bool(l == r)),
                     "!=" => Ok(RuntimeValue::Bool(l != r)),
                     "<" => Ok(RuntimeValue::Bool(l < r)),
                     ">" => Ok(RuntimeValue::Bool(l > r)),
                     _ => Err(format!("Unknown int operator {}", op))
                 },
                 (l, r) => Err(format!("Invalid operands for {}: {} and {}", op, l, r))
             }
        }
        "if_expression" => {
            let cond = eval(&node.named_child(0).unwrap(), source, context)?;
            if let RuntimeValue::Bool(b) = cond {
                if b {
                     eval(&node.named_child(1).unwrap(), source, context)
                } else if let Some(else_branch) = node.named_child(2) {
                     eval(&else_branch, source, context)
                } else {
                     Ok(RuntimeValue::Unit)
                }
            } else {
                Err("Condition must be boolean".to_string())
            }
        }
        "parenthesized_expression" => {
             eval(&node.named_child(0).unwrap(), source, context)
        }
        "literal" => {
             if let Some(child) = node.named_child(0) {
                 eval(&child, source, context)
             } else {
                 Ok(RuntimeValue::Unit)
             }
        }
        "list_expression" => {
             // [expr, expr]
             let mut vals = Vec::new();
             let mut cursor = node.walk();
             for child in node.children(&mut cursor) {
                 if child.kind() == "[" || child.kind() == "]" || child.kind() == "," {
                     continue;
                 }
                 vals.push(eval(&child, source, context)?);
             }
             Ok(RuntimeValue::List(vals))
        }
        "struct_expression" => {
            let type_name_node = node.named_child(0).unwrap();
            let type_name = type_name_node.utf8_text(source.as_bytes()).unwrap().to_string();
            
            let mut fields = HashMap::new();
            let count = node.named_child_count();
            for i in 1..count {
                 let field_init = node.named_child(i).unwrap();
                 let fname_node = field_init.child(0).unwrap();
                 let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                 let val_node = field_init.child(2).unwrap();
                 let val = eval(&val_node, source, context)?;
                 fields.insert(fname, val);
            }
            Ok(RuntimeValue::Struct(type_name, fields))
        }
        "record_expression" => {
            let mut fields = HashMap::new();
            let count = node.named_child_count();
            for i in 0..count {
                 let field_init = node.named_child(i).unwrap();
                 let fname_node = field_init.child(0).unwrap();
                 let fname = fname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                 let val_node = field_init.child(2).unwrap();
                 let val = eval(&val_node, source, context)?;
                 fields.insert(fname, val);
            }
            Ok(RuntimeValue::Record(fields))
        }
        "field_expression" => {
             let obj_node = node.named_child(0).unwrap();
             let field_node = node.named_child(1).unwrap();
             let field_name = field_node.utf8_text(source.as_bytes()).unwrap();

             let obj_val = eval(&obj_node, source, context)?;
             match obj_val {
                 RuntimeValue::Struct(_, fields) | RuntimeValue::Record(fields) => {
                      fields.get(field_name).cloned().ok_or_else(|| format!("Field not found: {}", field_name))
                 }
                 _ => Err(format!("Cannot access field {} on non-struct", field_name))
             }
        }
        "pipe_expression" => {
             let left_node = node.named_child(0).unwrap();
             let right_node = node.named_child(1).unwrap();
             
             let left_val = eval(&left_node, source, context)?;
             let right_val = eval(&right_node, source, context)?;
             
             if let RuntimeValue::Function(params, body_node) = right_val {
                 if params.len() != 1 {
                     return Err(format!("Pipe expects a function with 1 argument, found {}", params.len()));
                 }
                 
                 context.enter_scope();
                 context.set(params[0].clone(), left_val);
                 let result = eval(&body_node, source, context);
                 context.exit_scope();
                 result
             } else {
                 Err("Pipe target must be a function".to_string())
             }
        }
        "match_expression" => {
             // match expr { pat -> body, ... }
             let expr_node = node.named_child(0).unwrap();
             let val = eval(&expr_node, source, context)?;
             
             let count = node.named_child_count();
             for i in 1..count {
                 let arm = node.named_child(i).unwrap();
                 // match_arm: pattern -> expression
                 let pat = arm.child(0).unwrap();
                 let body = arm.child(2).unwrap();
                 
                 if let Some(bindings) = match_pattern(&pat, &val, source) {
                     context.enter_scope();
                     for (k, v) in bindings {
                         context.set(k, v);
                     }
                     let result = eval(&body, source, context);
                     context.exit_scope();
                     return result;
                 }
             }
             Err("No match found".to_string())
        }
        _ => {
            // Ignore others or return Unit
            Ok(RuntimeValue::Unit)
        }
    }
}

fn match_pattern<'a>(
    pattern: &Node<'a>,
    value: &RuntimeValue<'a>,
    source: &str,
) -> Option<HashMap<String, RuntimeValue<'a>>> {
    match pattern.kind() {
        "wildcard_pattern" => Some(HashMap::new()),
        "identifier" => {
            let name = pattern.utf8_text(source.as_bytes()).unwrap().to_string();
            let mut map = HashMap::new();
            map.insert(name, value.clone());
            Some(map)
        }
        "literal" => {
             if let Some(child) = pattern.named_child(0) {
                 match_pattern(&child, value, source)
             } else {
                 None
             }
        }
        "integer_literal" => {
            let text = pattern.utf8_text(source.as_bytes()).unwrap();
            let p_val = text.parse::<i64>().ok()?;
            if let RuntimeValue::Int(v) = value {
                if *v == p_val { Some(HashMap::new()) } else { None }
            } else { None }
        }
        "boolean_literal" => {
            let text = pattern.utf8_text(source.as_bytes()).unwrap();
             if let RuntimeValue::Bool(v) = value {
                if *v == (text == "true") { Some(HashMap::new()) } else { None }
            } else { None }
        }
        _ => None 
    }
}
