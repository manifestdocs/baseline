use std::collections::HashMap;
use tree_sitter::Node;

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue<'a> {
    Int(i64),
    String(String),
    Bool(bool),
    Unit,
    Function(Vec<String>, Node<'a>),
    /// Closure: params, body, captured environment
    Closure(Vec<String>, Node<'a>, HashMap<String, RuntimeValue<'a>>),
    Struct(String, HashMap<String, RuntimeValue<'a>>),
    Record(HashMap<String, RuntimeValue<'a>>),
    List(Vec<RuntimeValue<'a>>),
    Enum(String, Vec<RuntimeValue<'a>>), // VariantName, PayloadValues
}

impl<'a> std::fmt::Display for RuntimeValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Int(i) => write!(f, "{}", i),
            RuntimeValue::String(s) => write!(f, "{}", s), // TODO: quote strings?
            RuntimeValue::Bool(b) => write!(f, "{}", b),
            RuntimeValue::Unit => write!(f, "()"),
            RuntimeValue::Function(args, _) => write!(f, "|{}| ...", args.join(", ")),
            RuntimeValue::Closure(args, _, _) => write!(f, "|{}| <closure>", args.join(", ")),
            RuntimeValue::Struct(name, _fields) => {
                 write!(f, "{} {{ ... }}", name)
            }
            RuntimeValue::Record(_) => write!(f, "{{ ... }}"),
            RuntimeValue::List(vals) => {
                 let s = vals.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ");
                 write!(f, "[{}]", s)
            }
            RuntimeValue::Enum(name, payload) => {
                if payload.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let s = payload.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ");
                    write!(f, "{}({})", name, s)
                }
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

    /// Snapshot the entire environment (all scopes) into a flat HashMap.
    /// Used for closure capture.
    pub fn snapshot(&self) -> HashMap<String, RuntimeValue<'a>> {
        let mut env = HashMap::new();
        // Iterate from outermost to innermost so inner scopes shadow outer
        for scope in &self.scopes {
            env.extend(scope.iter().map(|(k, v)| (k.clone(), v.clone())));
        }
        env
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
            let name_node = node.child_by_field_name("name").unwrap();
            let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();

            let body_node = node.child_by_field_name("body").unwrap();

            if body_node.kind() == "lambda" {
                // Lambda evaluates to Function(args, body) — store it
                let body_val = eval(&body_node, source, context)?;
                context.set(name, body_val);
            } else {
                // Non-lambda body (block, expression, constructor, etc.)
                // Store as zero-arg function for deferred evaluation
                context.set(name, RuntimeValue::Function(Vec::new(), body_node));
            }
            Ok(RuntimeValue::Unit)
        }
        "type_def" => {
            // Register enum constructors as values in context
            if let Some(def_node) = node.child_by_field_name("def") {
                if def_node.kind() == "variant_list" {
                    let mut cursor = def_node.walk();
                    for child in def_node.children(&mut cursor) {
                        if child.kind() == "variant" {
                            if let Some(vname_node) = child.child_by_field_name("name") {
                                let vname = vname_node.utf8_text(source.as_bytes()).unwrap().to_string();
                                // Register nullary constructors as enum values
                                // Constructors with payload will be created via call_expression
                                context.set(vname.clone(), RuntimeValue::Enum(vname, Vec::new()));
                            }
                        }
                    }
                }
            }
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
            let captured = context.snapshot();
            Ok(RuntimeValue::Closure(args, body, captured))
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
                 RuntimeValue::Closure(param_names, body_node, captured_env) => {
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

                     // New scope with captured environment
                     context.enter_scope();
                     for (k, v) in &captured_env {
                         context.set(k.clone(), v.clone());
                     }
                     for (name, val) in param_names.iter().zip(arg_vals.into_iter()) {
                         context.set(name.clone(), val);
                     }

                     let result = eval(&body_node, source, context);
                     context.exit_scope();
                     result
                }
                RuntimeValue::Enum(ctor_name, _) => {
                     // Constructor call: Some(42) -> Enum("Some", [Int(42)])
                     let mut arg_vals = Vec::new();
                     let total_children = node.named_child_count();
                     for i in 1..total_children {
                         let arg_node = node.named_child(i).unwrap();
                         arg_vals.push(eval(&arg_node, source, context)?);
                     }
                     Ok(RuntimeValue::Enum(ctor_name, arg_vals))
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
        "type_identifier" => {
            // Nullary constructor or type reference in expression context
            let name = node.utf8_text(source.as_bytes()).unwrap();
            if let Some(val) = context.get(name) {
                Ok(val.clone())
            } else {
                // Treat as nullary enum constructor
                Ok(RuntimeValue::Enum(name.to_string(), Vec::new()))
            }
        }
        "integer_literal" => {
            let text = node.utf8_text(source.as_bytes()).unwrap();
            let val = text.parse::<i64>().map_err(|_| "Invalid integer".to_string())?;
            Ok(RuntimeValue::Int(val))
        }
        "string_literal" => {
            // Range-based approach: extract raw text between named children
            // (interpolation and escape_sequence are named; raw text is absorbed by tree-sitter)
            let mut result = String::new();
            let bytes = source.as_bytes();

            // Start after the opening quote
            let str_start = node.start_byte() + 1; // skip "
            let str_end = node.end_byte() - 1;     // skip "

            let named_count = node.named_child_count();
            if named_count == 0 {
                // Simple string with no interpolation or escapes
                let raw = &source[str_start..str_end];
                result.push_str(raw);
            } else {
                let mut cursor = str_start;
                for i in 0..named_count {
                    let child = node.named_child(i).unwrap();
                    let child_start = child.start_byte();
                    let child_end = child.end_byte();

                    // Raw text before this named child
                    if cursor < child_start {
                        result.push_str(&source[cursor..child_start]);
                    }

                    match child.kind() {
                        "interpolation" => {
                            let expr = child.named_child(0).unwrap();
                            let val = eval(&expr, source, context)?;
                            result.push_str(&val.to_string());
                        }
                        "escape_sequence" => {
                            let esc = child.utf8_text(bytes).unwrap();
                            match esc {
                                "\\n" => result.push('\n'),
                                "\\t" => result.push('\t'),
                                "\\r" => result.push('\r'),
                                "\\\\" => result.push('\\'),
                                "\\\"" => result.push('"'),
                                "\\0" => result.push('\0'),
                                other => result.push_str(other),
                            }
                        }
                        _ => {}
                    }
                    cursor = child_end;
                }
                // Raw text after the last named child
                if cursor < str_end {
                    result.push_str(&source[cursor..str_end]);
                }
            }
            Ok(RuntimeValue::String(result))
        }
        "boolean_literal" => {
             let text = node.utf8_text(source.as_bytes()).unwrap();
             Ok(RuntimeValue::Bool(text == "true"))
        }
        "binary_expression" => {
             let op = node.child(1).unwrap().utf8_text(source.as_bytes()).unwrap();

             // Short-circuit logical operators: evaluate left first,
             // only evaluate right if needed
             match op {
                 "&&" => {
                     let left = eval(&node.named_child(0).unwrap(), source, context)?;
                     match left {
                         RuntimeValue::Bool(false) => Ok(RuntimeValue::Bool(false)),
                         RuntimeValue::Bool(true) => {
                             let right = eval(&node.named_child(1).unwrap(), source, context)?;
                             match right {
                                 RuntimeValue::Bool(b) => Ok(RuntimeValue::Bool(b)),
                                 _ => Err("Right operand of && must be Bool".to_string()),
                             }
                         }
                         _ => Err("Left operand of && must be Bool".to_string()),
                     }
                 }
                 "||" => {
                     let left = eval(&node.named_child(0).unwrap(), source, context)?;
                     match left {
                         RuntimeValue::Bool(true) => Ok(RuntimeValue::Bool(true)),
                         RuntimeValue::Bool(false) => {
                             let right = eval(&node.named_child(1).unwrap(), source, context)?;
                             match right {
                                 RuntimeValue::Bool(b) => Ok(RuntimeValue::Bool(b)),
                                 _ => Err("Right operand of || must be Bool".to_string()),
                             }
                         }
                         _ => Err("Left operand of || must be Bool".to_string()),
                     }
                 }
                 _ => {
                     let left = eval(&node.named_child(0).unwrap(), source, context)?;
                     let right = eval(&node.named_child(1).unwrap(), source, context)?;
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
                             "<=" => Ok(RuntimeValue::Bool(l <= r)),
                             ">=" => Ok(RuntimeValue::Bool(l >= r)),
                             _ => Err(format!("Unknown int operator {}", op))
                         },
                         (l, r) => Err(format!("Invalid operands for {}: {} and {}", op, l, r))
                     }
                 }
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
             
             match right_val {
                 RuntimeValue::Function(params, body_node) => {
                     if params.len() != 1 {
                         return Err(format!("Pipe expects a function with 1 argument, found {}", params.len()));
                     }
                     context.enter_scope();
                     context.set(params[0].clone(), left_val);
                     let result = eval(&body_node, source, context);
                     context.exit_scope();
                     result
                 }
                 RuntimeValue::Closure(params, body_node, captured_env) => {
                     if params.len() != 1 {
                         return Err(format!("Pipe expects a function with 1 argument, found {}", params.len()));
                     }
                     context.enter_scope();
                     for (k, v) in &captured_env {
                         context.set(k.clone(), v.clone());
                     }
                     context.set(params[0].clone(), left_val);
                     let result = eval(&body_node, source, context);
                     context.exit_scope();
                     result
                 }
                 _ => Err("Pipe target must be a function".to_string()),
             }
        }
        "for_expression" => {
            // for <pattern> in <collection> do <body>
            let pat_node = node.named_child(0).unwrap();
            let collection_node = node.named_child(1).unwrap();
            let body_node = node.named_child(2).unwrap();

            let collection = eval(&collection_node, source, context)?;
            match collection {
                RuntimeValue::List(items) => {
                    let mut last_val = RuntimeValue::Unit;
                    for item in &items {
                        if let Some(bindings) = match_pattern(&pat_node, item, source) {
                            context.enter_scope();
                            for (k, v) in bindings {
                                context.set(k, v);
                            }
                            last_val = eval(&body_node, source, context)?;
                            context.exit_scope();
                        }
                    }
                    Ok(last_val)
                }
                _ => Err("for..in requires a List".to_string()),
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
        "type_identifier" => {
            // Nullary constructor pattern: Red, None, Active
            let ctor_name = pattern.utf8_text(source.as_bytes()).unwrap();
            if let RuntimeValue::Enum(vname, payload) = value {
                if vname == ctor_name && payload.is_empty() {
                    Some(HashMap::new())
                } else {
                    None
                }
            } else {
                None
            }
        }
        "constructor_pattern" => {
            // Constructor pattern: Some(v), Pending(msg)
            let ctor_name_node = pattern.child(0).unwrap();
            let ctor_name = ctor_name_node.utf8_text(source.as_bytes()).unwrap();

            if let RuntimeValue::Enum(vname, payload) = value {
                if vname != ctor_name {
                    return None;
                }

                // Collect sub-patterns (skip type_identifier, parens, commas)
                let mut sub_patterns = Vec::new();
                let child_count = pattern.child_count();
                for i in 0..child_count {
                    let child = pattern.child(i).unwrap();
                    let k = child.kind();
                    if k != "type_identifier" && k != "(" && k != ")" && k != "," {
                        sub_patterns.push(child);
                    }
                }

                if sub_patterns.len() != payload.len() {
                    return None;
                }

                let mut bindings = HashMap::new();
                for (sub_pat, sub_val) in sub_patterns.iter().zip(payload.iter()) {
                    let sub_bindings = match_pattern(sub_pat, sub_val, source)?;
                    bindings.extend(sub_bindings);
                }
                Some(bindings)
            } else {
                None
            }
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

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    /// Parse and evaluate Baseline source, returning main's evaluated body.
    /// Source should define main as: `main : () -> T\nmain = || expr`
    fn eval_source(source: &str) -> Result<RuntimeValue<'static>, String> {
        let mut parser = Parser::new();
        parser
            .set_language(&LANGUAGE.into())
            .expect("Failed to load language");
        let tree = parser.parse(source, None).expect("Failed to parse");
        let tree = Box::leak(Box::new(tree));
        let root = tree.root_node();
        let mut context = Context::new();
        eval(&root, source, &mut context)?;

        let main_val = context
            .get("main")
            .cloned()
            .ok_or_else(|| "No 'main' binding found".to_string())?;

        // main is a Function or Closure — evaluate its body to get the actual value
        match main_val {
            RuntimeValue::Function(_, body_node) => {
                context.enter_scope();
                let result = eval(&body_node, source, &mut context);
                context.exit_scope();
                result
            }
            RuntimeValue::Closure(_, body_node, captured_env) => {
                context.enter_scope();
                for (k, v) in &captured_env {
                    context.set(k.clone(), v.clone());
                }
                let result = eval(&body_node, source, &mut context);
                context.exit_scope();
                result
            }
            other => Ok(other),
        }
    }

    // -- Comparison operators --

    #[test]
    fn test_less_than_or_equal_true() {
        let result = eval_source("main : () -> Bool\nmain = || 3 <= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_less_than_or_equal_equal() {
        let result = eval_source("main : () -> Bool\nmain = || 5 <= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_less_than_or_equal_false() {
        let result = eval_source("main : () -> Bool\nmain = || 7 <= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    #[test]
    fn test_greater_than_or_equal_true() {
        let result = eval_source("main : () -> Bool\nmain = || 5 >= 3");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_greater_than_or_equal_equal() {
        let result = eval_source("main : () -> Bool\nmain = || 5 >= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_greater_than_or_equal_false() {
        let result = eval_source("main : () -> Bool\nmain = || 3 >= 5");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    // -- Logical operators --

    #[test]
    fn test_and_true_true() {
        let result = eval_source("main : () -> Bool\nmain = || true && true");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_and_true_false() {
        let result = eval_source("main : () -> Bool\nmain = || true && false");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    #[test]
    fn test_and_false_short_circuits() {
        // false && <anything> should return false without evaluating right side
        let result = eval_source("main : () -> Bool\nmain = || false && true");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    #[test]
    fn test_or_false_false() {
        let result = eval_source("main : () -> Bool\nmain = || false || false");
        assert_eq!(result, Ok(RuntimeValue::Bool(false)));
    }

    #[test]
    fn test_or_false_true() {
        let result = eval_source("main : () -> Bool\nmain = || false || true");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    #[test]
    fn test_or_true_short_circuits() {
        // true || <anything> should return true without evaluating right side
        let result = eval_source("main : () -> Bool\nmain = || true || false");
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }

    // -- Combined expressions --

    #[test]
    fn test_comparison_in_if() {
        let source = "main : () -> Int\nmain = || if 3 <= 5 then 42 else 0";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Int(42)));
    }

    #[test]
    fn test_logical_and_with_comparisons() {
        let source = "main : () -> Bool\nmain = || 3 <= 5 && 10 >= 8";
        let result = eval_source(source);
        assert_eq!(result, Ok(RuntimeValue::Bool(true)));
    }
}
