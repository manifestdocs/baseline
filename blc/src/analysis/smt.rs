use crate::diagnostics::{Diagnostic, Location, Severity};
use std::collections::HashMap;
use std::io::Write;
use std::process::Command;
use tree_sitter::Node;

/// A parsed specification for a function.
#[allow(dead_code)]
struct FuncSpec {
    name: String,
    params: Vec<(String, String)>, // (name, type)
    requires: Vec<String>,         // precondition SMT expressions
    ensures: Vec<String>,          // postcondition SMT expressions (may reference "result")
    return_type: Option<String>,
    is_pure: bool,
    is_total: bool,
    assumes: Vec<String>,
    location: Location,
}

/// Check all spec blocks in a source file using Z3.
pub fn check_specs(
    tree: &tree_sitter::Tree,
    source: &str,
    file: &str,
    timeout_ms: u32,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let root = tree.root_node();
    let mut cursor = root.walk();

    for child in root.named_children(&mut cursor) {
        if child.kind() == "spec_block" {
            check_spec_block(&child, source, file, timeout_ms, &mut diagnostics);
        }
    }

    diagnostics
}

fn check_spec_block(
    node: &Node,
    source: &str,
    file: &str,
    timeout_ms: u32,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let spec = match parse_spec_block(node, source, file) {
        Some(s) => s,
        None => return,
    };

    // Find the function body in the definition child
    let def_node = find_definition(node);
    let def_node = match def_node {
        Some(n) => n,
        None => return,
    };

    if spec.ensures.is_empty() {
        return; // No postconditions to verify
    }

    // Build SMT-LIB2 script
    let smt_script = build_smt_script(&spec, &def_node, source);

    // Run Z3
    match run_z3(&smt_script, timeout_ms) {
        Z3Result::Unsat => {
            // Verification succeeded — the negation of the spec is unsatisfiable
        }
        Z3Result::Sat(model) => {
            // Counter-example found — spec violation
            let counter = format_counter_example(&model, &spec);
            diagnostics.push(Diagnostic {
                code: "SPEC_001".to_string(),
                severity: Severity::Error,
                location: spec.location.clone(),
                message: format!(
                    "Specification violation in `{}`: postcondition may not hold",
                    spec.name
                ),
                context: if counter.is_empty() {
                    "Z3 found a counter-example.".to_string()
                } else {
                    format!("Counter-example: {}", counter)
                },
                suggestions: vec![],
            });
        }
        Z3Result::Unknown => {
            diagnostics.push(Diagnostic {
                code: "SPEC_003".to_string(),
                severity: Severity::Warning,
                location: spec.location.clone(),
                message: format!(
                    "Specification for `{}` could not be verified (solver timeout or undecidable)",
                    spec.name
                ),
                context: format!("Try simplifying the specification or increasing the timeout (current: {}ms).", timeout_ms),
                suggestions: vec![],
            });
        }
        Z3Result::Error(msg) => {
            diagnostics.push(Diagnostic {
                code: "SPEC_003".to_string(),
                severity: Severity::Warning,
                location: spec.location.clone(),
                message: format!(
                    "SMT verification error for `{}`: {}",
                    spec.name, msg
                ),
                context: "The SMT solver encountered an error.".to_string(),
                suggestions: vec![],
            });
        }
    }
}

fn find_definition<'a>(spec_block: &Node<'a>) -> Option<Node<'a>> {
    let count = spec_block.named_child_count();
    for i in 0..count {
        let child = spec_block.named_child(i).unwrap();
        if child.kind() == "function_def" {
            return Some(child);
        }
    }
    None
}

fn parse_spec_block(node: &Node, source: &str, file: &str) -> Option<FuncSpec> {
    let mut name = String::new();
    let mut params = Vec::new();
    let mut requires = Vec::new();
    let mut ensures = Vec::new();
    let mut assumes = Vec::new();
    let mut return_type = None;
    let mut is_pure = false;
    let mut is_total = false;

    let count = node.named_child_count();
    for i in 0..count {
        let raw_child = node.named_child(i).unwrap();
        // Unwrap spec_attribute wrapper to get the inner clause
        let child = if raw_child.kind() == "spec_attribute" {
            raw_child.named_child(0).unwrap_or(raw_child)
        } else {
            raw_child
        };
        match child.kind() {
            "spec_decl" => {
                if let Some(name_node) = child.child_by_field_name("name") {
                    name = text(&name_node, source).to_string();
                }
            }
            "given_clause" => {
                // Extract params from given clause
                let mut cursor = child.walk();
                for gc in child.named_children(&mut cursor) {
                    if gc.kind() == "param" {
                        // param has 'pattern' and 'type' fields
                        let pname = if let Some(pat) = gc.child_by_field_name("pattern") {
                            text(&pat, source)
                        } else {
                            // Fallback if grammar mismatch or error
                            "_"
                        };
                        let ptype = text(&gc.child_by_field_name("type").unwrap(), source);
                        params.push((pname.to_string(), ptype.to_string()));
                    }
                }
                // Extract where condition if present
                if let Some(cond) = child.child_by_field_name("condition") {
                    requires.push(expr_to_smt(&cond, source));
                }
            }
            "requires_clause" => {
                if let Some(cond) = child.child_by_field_name("condition") {
                    requires.push(expr_to_smt(&cond, source));
                }
            }
            "ensures_clause" => {
                if let Some(cond) = child.child_by_field_name("condition") {
                    ensures.push(expr_to_smt(&cond, source));
                }
            }
            "assume_clause" => {
                if let Some(cond) = child.child_by_field_name("condition") {
                    assumes.push(expr_to_smt(&cond, source));
                }
            }
            "returns_clause" => {
                if let Some(t) = child.child_by_field_name("type") {
                    return_type = Some(text(&t, source).to_string());
                }
            }
            "pure_attribute" => {
                is_pure = true;
            }
            "total_attribute" => {
                is_total = true;
            }
            "function_def" => {
                // If name not set from @spec, get it from function
                if name.is_empty()
                    && let Some(fn_name) = child.child_by_field_name("name")
                {
                    name = text(&fn_name, source).to_string();
                }
                // Extract params from function signature if not from @given
                if params.is_empty()
                    && let Some(param_list) = child.child_by_field_name("params")
                {
                    let mut cursor = param_list.walk();
                    let mut idx = 0;
                    for p in param_list.named_children(&mut cursor) {
                        if p.kind() == "param" {
                            let pname = if let Some(pat) = p.child_by_field_name("pattern") {
                                if pat.kind() == "identifier" {
                                    text(&pat, source).to_string()
                                } else {
                                    // Complex pattern: use synthetic name (user likely won't reference this in spec directly)
                                    // or validation will fail if they try.
                                    format!("param_{}", idx)
                                }
                            } else {
                                format!("param_{}", idx)
                            };
                            
                            let ptype = if let Some(t) = p.child_by_field_name("type") {
                                text(&t, source).to_string()
                            } else {
                                "Unknown".to_string()
                            };
                            
                            params.push((pname, ptype));
                            idx += 1;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    if name.is_empty() {
        return None;
    }

    let location = Location::from_node(file, node);

    Some(FuncSpec {
        name,
        params,
        requires,
        ensures,
        return_type,
        is_pure,
        is_total,
        assumes,
        location,
    })
}

fn text<'a>(node: &Node<'a>, source: &'a str) -> &'a str {
    node.utf8_text(source.as_bytes()).unwrap_or("")
}

/// Build an SMT-LIB2 script that checks if the spec can be violated.
/// Strategy: declare params, assert preconditions + assumptions,
/// define result from the function body, then assert NOT(postconditions).
/// If UNSAT, the spec holds. If SAT, we have a counter-example.
fn build_smt_script(spec: &FuncSpec, func_def: &Node, source: &str) -> String {
    let mut smt = String::new();
    smt.push_str("(set-logic ALL)\n");

    // Declare parameters
    for (name, typ) in &spec.params {
        let smt_type = type_to_smt(typ);
        smt.push_str(&format!("(declare-const {} {})\n", name, smt_type));
    }

    // Declare result variable
    let result_type = spec
        .return_type
        .as_deref()
        .or_else(|| {
            func_def
                .child_by_field_name("return_type")
                .map(|n| text(&n, source))
        })
        .unwrap_or("Int");
    smt.push_str(&format!(
        "(declare-const result {})\n",
        type_to_smt(result_type)
    ));

    // Assert preconditions
    for req in &spec.requires {
        smt.push_str(&format!("(assert {})\n", req));
    }

    // Assert assumptions
    for asn in &spec.assumes {
        smt.push_str(&format!("(assert {})\n", asn));
    }

    // Define result from the function body
    if let Some(body) = func_def.child_by_field_name("body") {
        let body_smt = expr_to_smt(&body, source);
        smt.push_str(&format!("(assert (= result {}))\n", body_smt));
    }

    // Assert negation of postconditions (if unsat, spec holds)
    if !spec.ensures.is_empty() {
        if spec.ensures.len() == 1 {
            smt.push_str(&format!("(assert (not {}))\n", spec.ensures[0]));
        } else {
            let conj = spec
                .ensures
                .iter()
                .map(|e| e.as_str())
                .collect::<Vec<_>>()
                .join(" ");
            smt.push_str(&format!("(assert (not (and {})))\n", conj));
        }
    }

    smt.push_str("(check-sat)\n");
    smt.push_str("(get-model)\n");

    smt
}

/// Translate a Baseline expression CST node to an SMT-LIB2 S-expression.
fn expr_to_smt(node: &Node, source: &str) -> String {
    match node.kind() {
        "literal" => {
            // Wrapper node — delegate to inner child
            if let Some(inner) = node.named_child(0) {
                expr_to_smt(&inner, source)
            } else {
                "0".to_string()
            }
        }
        "identifier" => {
            let name = text(node, source);
            // Map 'self' to a standard name
            if name == "self" {
                "self_val".to_string()
            } else {
                name.to_string()
            }
        }
        "integer_literal" => {
            let val = text(node, source).replace('_', "");
            // Handle hex/bin/oct
            if val.starts_with("0x") || val.starts_with("0X") {
                format!(
                    "{}",
                    i64::from_str_radix(&val[2..], 16).unwrap_or(0)
                )
            } else if val.starts_with("0b") || val.starts_with("0B") {
                format!("{}", i64::from_str_radix(&val[2..], 2).unwrap_or(0))
            } else if val.starts_with("0o") || val.starts_with("0O") {
                format!("{}", i64::from_str_radix(&val[2..], 8).unwrap_or(0))
            } else {
                val
            }
        }
        "float_literal" => text(node, source).to_string(),
        "boolean_literal" => {
            if text(node, source) == "true" {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        "binary_expression" => {
            let lhs = node.child(0).unwrap();
            let op = node.child(1).unwrap();
            let rhs = node.child(2).unwrap();
            let op_text = text(&op, source);
            let smt_op = match op_text {
                "+" => "+",
                "-" => "-",
                "*" => "*",
                "/" => "div",
                "%" => "mod",
                "==" => "=",
                "!=" => "distinct",
                "<" => "<",
                ">" => ">",
                "<=" => "<=",
                ">=" => ">=",
                "&&" => "and",
                "||" => "or",
                _ => return format!("(error \"unsupported operator: {}\")", op_text),
            };
            format!(
                "({} {} {})",
                smt_op,
                expr_to_smt(&lhs, source),
                expr_to_smt(&rhs, source)
            )
        }
        "unary_expression" => {
            let op = node.child(0).unwrap();
            let operand = node.child(1).unwrap();
            let op_text = text(&op, source);
            match op_text {
                "not" => format!("(not {})", expr_to_smt(&operand, source)),
                "-" => format!("(- {})", expr_to_smt(&operand, source)),
                _ => format!("(error \"unsupported unary: {}\")", op_text),
            }
        }
        "parenthesized_expression" => {
            if let Some(inner) = node.named_child(0) {
                expr_to_smt(&inner, source)
            } else {
                "0".to_string()
            }
        }
        "if_expression" => {
            // if cond then a else b → (ite cond a b)
            let mut children = Vec::new();
            let count = node.child_count();
            for i in 0..count {
                let child = node.child(i).unwrap();
                if child.is_named() {
                    children.push(child);
                }
            }
            if children.len() >= 3 {
                format!(
                    "(ite {} {} {})",
                    expr_to_smt(&children[0], source),
                    expr_to_smt(&children[1], source),
                    expr_to_smt(&children[2], source)
                )
            } else if children.len() == 2 {
                format!(
                    "(ite {} {} 0)",
                    expr_to_smt(&children[0], source),
                    expr_to_smt(&children[1], source)
                )
            } else {
                "0".to_string()
            }
        }
        "call_expression" => {
            // f(x, y) → (f x y) — uninterpreted function
            let callee = node.child(0).unwrap();
            let fname = text(&callee, source);
            let mut args = Vec::new();
            let count = node.named_child_count();
            for i in 1..count {
                let arg = node.named_child(i).unwrap();
                args.push(expr_to_smt(&arg, source));
            }
            if args.is_empty() {
                fname.to_string()
            } else {
                format!("({} {})", fname, args.join(" "))
            }
        }
        "field_expression" => {
            // a.b — treat as uninterpreted
            text(node, source).replace('.', "_").to_string()
        }
        "type_identifier" => {
            // Could be a nullary constructor — treat as constant
            text(node, source).to_string()
        }
        _ => {
            // Fallback: use raw text as uninterpreted constant
            let raw = text(node, source);
            if raw.is_empty() {
                "0".to_string()
            } else {
                format!("(error \"unsupported node: {}\")", node.kind())
            }
        }
    }
}

/// Convert a Baseline type name to an SMT-LIB2 sort.
fn type_to_smt(typ: &str) -> &str {
    match typ {
        "Int" => "Int",
        "Float" => "Real",
        "Boolean" | "Bool" => "Bool",
        "String" => "String",
        _ => "Int", // Default to Int for unknown types
    }
}

enum Z3Result {
    Unsat,
    Sat(String),
    Unknown,
    Error(String),
}

fn run_z3(script: &str, timeout_ms: u32) -> Z3Result {
    let mut child = match Command::new("z3")
        .args(["-in", &format!("-T:{}", timeout_ms / 1000 + 1)])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => return Z3Result::Error(format!("Failed to run z3: {}", e)),
    };

    if let Some(ref mut stdin) = child.stdin {
        let _ = stdin.write_all(script.as_bytes());
    }

    let output = match child.wait_with_output() {
        Ok(o) => o,
        Err(e) => return Z3Result::Error(format!("Z3 execution failed: {}", e)),
    };

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    let first_line = stdout.lines().next().unwrap_or("").trim();

    match first_line {
        "unsat" => Z3Result::Unsat,
        "sat" => Z3Result::Sat(stdout.to_string()),
        "unknown" => Z3Result::Unknown,
        _ => {
            if !stderr.is_empty() {
                Z3Result::Error(stderr.to_string())
            } else if stdout.contains("error") {
                Z3Result::Error(stdout.to_string())
            } else {
                Z3Result::Unknown
            }
        }
    }
}

/// Extract counter-example values from Z3 model output.
/// Z3 multiline model format:
///   (define-fun d () Int
///     1)
///   (define-fun n () Int
///     (- 5))
fn format_counter_example(model: &str, spec: &FuncSpec) -> String {
    let mut values = HashMap::new();
    let lines: Vec<&str> = model.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();
        if line.starts_with("(define-fun ") {
            // Extract variable name
            let after = line.trim_start_matches("(define-fun ");
            let var_name = after.split_whitespace().next().unwrap_or("");

            // Skip functions with arguments (like div0, mod0)
            if after.contains("((") {
                i += 1;
                // Skip until closing paren
                while i < lines.len() && !lines[i].trim().ends_with(')') {
                    i += 1;
                }
                i += 1;
                continue;
            }

            // Check if value is on same line (single-line format)
            // Pattern: (define-fun name () Type value)
            let paren_depth: i32 = line.chars().map(|c| match c {
                '(' => 1,
                ')' => -1,
                _ => 0,
            }).sum();

            if paren_depth == 0 {
                // Single-line — extract value after the type
                if let Some(pos) = after.rfind(") ") {
                    let val = after[pos + 2..].trim_end_matches(')').trim();
                    values.insert(var_name.to_string(), normalize_smt_value(val));
                }
            } else {
                // Multi-line — value is on the next line(s)
                i += 1;
                if i < lines.len() {
                    let val_line = lines[i].trim().trim_end_matches(')');
                    values.insert(var_name.to_string(), normalize_smt_value(val_line));
                }
            }
        }
        i += 1;
    }

    if values.is_empty() {
        return String::new();
    }

    let mut parts = Vec::new();
    for (name, _typ) in &spec.params {
        if let Some(val) = values.get(name) {
            parts.push(format!("{} = {}", name, val));
        }
    }
    if let Some(val) = values.get("result") {
        parts.push(format!("result = {}", val));
    }

    parts.join(", ")
}

fn normalize_smt_value(val: &str) -> String {
    let val = val.trim();
    if let Some(rest) = val.strip_prefix("(- ") {
        format!("-{}", rest.trim_end_matches(')').trim())
    } else {
        val.to_string()
    }
}
