use tree_sitter::{Node, Tree};
use crate::diagnostics::{Diagnostic, Location, Suggestion};
use std::collections::HashMap;

/// Represents a numeric interval (inclusive).
#[derive(Debug, Clone, Copy)]
pub struct Interval {
    pub min: i64,
    pub max: i64,
}

impl Interval {
    pub fn new(min: i64, max: i64) -> Self {
        Self { min, max }
    }

    pub fn full() -> Self {
        Self { min: i64::MIN, max: i64::MAX }
    }

    pub fn contains(&self, val: i64) -> bool {
        val >= self.min && val <= self.max
    }
}

pub fn check_refinements(tree: &Tree, source: &str, file: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let root = tree.root_node();
    
    // 1. Collect refined types
    let mut refined_types = HashMap::new();
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if child.kind() == "type_declaration" {
            if let Some((name, interval)) = parse_refined_type(child, source) {
                refined_types.insert(name, interval);
            }
        }
    }

    // 2. Check usages
    check_node(root, source, file, &refined_types, &mut diagnostics);

    diagnostics
}

fn check_node(
    node: Node, 
    source: &str, 
    file: &str, 
    refined_types: &HashMap<String, Interval>, 
    diagnostics: &mut Vec<Diagnostic>
) {
    // Check let bindings: let x : Port = 70000
    if node.kind() == "let_expression" {
        check_let_binding(node, source, file, refined_types, diagnostics);
    }

    // Recurse
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        check_node(child, source, file, refined_types, diagnostics);
    }
}

fn parse_refined_type(node: Node, source: &str) -> Option<(String, Interval)> {
    let mut name = None;
    let mut refinement = None;
    
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "upper_identifier" {
            name = child.utf8_text(source.as_bytes()).ok().map(|s| s.to_string());
        }
        if child.kind() == "where_clause" {
            refinement = Some(child);
        }
    }

    let name = name?;
    let refinement = refinement?;
    
    let mut interval = Interval::full();
    
    let mut cursor = refinement.walk();
    for child in refinement.children(&mut cursor) {
        // The where_clause contains "where" keyword and the expression.
        // The expression is aliased as "refinement".
        if child.kind() == "refinement" || child.kind() == "binary_expression" {
             update_interval_from_expr(child, source, &mut interval);
        }
    }

    // Only return if we actually constrained something
    if interval.min != i64::MIN || interval.max != i64::MAX {
        Some((name, interval))
    } else {
        None
    }
}



fn update_interval_from_expr(expr: Node, source: &str, interval: &mut Interval) {
    let mut op_node = None;
    let mut left_node = None;
    let mut right_node = None;
    
    // binary_expression children are: left, operator, right (usually)
    // But since no fields, we rely on order or scanning.
    // children: expr operator expr
    
    let mut cursor = expr.walk();
    let mut children = Vec::new();
    for child in expr.children(&mut cursor) {
        children.push(child);
    }
    
    if children.len() == 3 {
        left_node = Some(children[0]);
        op_node = Some(children[1]); // The operator is usually an anonymous token, but tree-sitter might expose it
        right_node = Some(children[2]);
    } else if children.len() == 1 {
        // Handle wrapper nodes (e.g. from aliases) by recursing
        update_interval_from_expr(children[0], source, interval);
        return;
    }

    if let (Some(left), Some(op), Some(right)) = (left_node, op_node, right_node) {
        let op_str = op.utf8_text(source.as_bytes()).unwrap_or("");
        
        // Handle logical AND: expr && expr
        if op_str == "&&" {
            update_interval_from_expr(left, source, interval);
            update_interval_from_expr(right, source, interval);
            return;
        }

        let left_text = left.utf8_text(source.as_bytes()).unwrap_or("");
        let right_text = right.utf8_text(source.as_bytes()).unwrap_or("");
        
        // Assume 'self' is always on the left for now (simplification)
        if left_text == "self" {
            if let Ok(val) = right_text.parse::<i64>() {
                match op_str {
                    ">" => interval.min = std::cmp::max(interval.min, val + 1),
                    ">=" => interval.min = std::cmp::max(interval.min, val),
                    "<" => interval.max = std::cmp::min(interval.max, val - 1),
                    "<=" => interval.max = std::cmp::min(interval.max, val),
                    "==" => {
                        interval.min = val;
                        interval.max = val;
                    }
                    _ => {}
                }
            }
        }
    }
}

fn check_let_binding(
    node: Node, 
    source: &str, 
    file: &str, 
    refined_types: &HashMap<String, Interval>, 
    diagnostics: &mut Vec<Diagnostic>
) {
    // let pattern: type = value
    // find value node (expression after =)
    
    let mut type_name = None;
    let mut value_node = None;
    let mut found_equals = false;
    
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "type_annotation" {
             let mut inner = child.walk();
             for grand in child.children(&mut inner) {
                 if grand.kind() == "type_identifier" {
                     type_name = grand.utf8_text(source.as_bytes()).ok().map(|s| s.to_string());
                     break;
                 }
             }
        }
        
        if child.kind() == "=" {
            found_equals = true;
            continue;
        }
        
        if found_equals {
            // First node after equals is likely the value
            // Check if it's an expression
            // Simplified: take first non-comment node properly
            value_node = Some(child);
            break; // assume single value
        }
    }
    
    if let (Some(t_name), Some(val)) = (type_name, value_node) {
        if let Some(interval) = refined_types.get(&t_name) {

             if val.kind() == "integer_literal" {
                 if let Ok(int_val) = val.utf8_text(source.as_bytes()).unwrap_or("0").parse::<i64>() {
                     // Check if compatible
                     if !interval.contains(int_val) {
                         let start = val.start_position();
                         diagnostics.push(Diagnostic {
                             code: "REF_001".to_string(),
                             severity: "error".to_string(),
                             location: Location {
                                 file: file.to_string(),
                                 line: start.row + 1,
                                 col: start.column + 1,
                             },
                             message: format!(
                                 "Refinement Violation: Value {} is out of bounds for type {}", 
                                 int_val, t_name
                             ),
                             context: format!(
                                 "Type {} requires value in range [{}, {}]", 
                                 t_name, 
                                 if interval.min == i64::MIN { "-inf".to_string() } else { interval.min.to_string() },
                                 if interval.max == i64::MAX { "inf".to_string() } else { interval.max.to_string() }
                             ),
                             suggestions: vec![],
                         });
                     }
                 }
             }
        }
    }
}
