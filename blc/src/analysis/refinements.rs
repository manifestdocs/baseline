use crate::diagnostics::{Diagnostic, Location, Severity};
use std::collections::HashMap;
use tree_sitter::{Node, Tree};

/// Represents a numeric interval (inclusive).
#[derive(Debug, Clone, Copy)]
pub struct Interval {
    pub min: i64,
    pub max: i64,
}

impl Interval {
    pub fn full() -> Self {
        Self {
            min: i64::MIN,
            max: i64::MAX,
        }
    }

    pub fn contains(&self, val: i64) -> bool {
        val >= self.min && val <= self.max
    }
}

struct ValueTable {
    scopes: Vec<HashMap<String, i64>>,
}

impl ValueTable {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, name: String, val: i64) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, val);
        }
    }

    fn lookup(&self, name: &str) -> Option<i64> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(*val);
            }
        }
        None
    }
}

pub fn check_refinements(tree: &Tree, source: &str, file: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    let root = tree.root_node();

    // 1. Collect refined types
    let mut refined_types = HashMap::new();
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if child.kind() == "type_def"
            && let Some((name, interval)) = parse_refined_type(child, source)
        {
            refined_types.insert(name, interval);
        }
    }

    // 2. Check usages
    let mut values = ValueTable::new();
    check_node(
        root,
        source,
        file,
        &refined_types,
        &mut values,
        &mut diagnostics,
    );

    diagnostics
}

fn check_node(
    node: Node,
    source: &str,
    file: &str,
    refined_types: &HashMap<String, Interval>,
    values: &mut ValueTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let k = node.kind();

    // Scoping
    if k == "function_def" || k == "block" {
        values.enter_scope();
    }

    // Check let bindings: let x : Port = 70000
    if k == "let_binding" {
        check_let_binding(node, source, file, refined_types, values, diagnostics);
    }

    // Recurse
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        check_node(child, source, file, refined_types, values, diagnostics);
    }

    // Exit Scope
    if k == "function_def" || k == "block" {
        values.exit_scope();
    }
}

fn parse_refined_type(node: Node, source: &str) -> Option<(String, Interval)> {
    let mut name = None;
    let mut refinement = None;

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "type_identifier" && name.is_none() {
            name = child
                .utf8_text(source.as_bytes())
                .ok()
                .map(|s| s.to_string());
        }
        if child.kind() == "refinement_clause" {
            refinement = Some(child);
        }
    }

    let name = name?;
    let refinement = refinement?;

    let mut interval = Interval::full();

    let mut cursor = refinement.walk();
    for child in refinement.children(&mut cursor) {
        if child.kind() == "predicate" || child.kind() == "binary_expression" {
            update_interval_from_expr(child, source, &mut interval);
        }
    }

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

    let mut cursor = expr.walk();
    let mut children = Vec::new();
    for child in expr.children(&mut cursor) {
        children.push(child);
    }

    if children.len() == 3 {
        left_node = Some(children[0]);
        op_node = Some(children[1]);
        right_node = Some(children[2]);
    } else if children.len() == 1 {
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

        if left_text == "self"
            && let Ok(val) = right_text.parse::<i64>()
        {
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

fn check_let_binding(
    node: Node,
    source: &str,
    file: &str,
    refined_types: &HashMap<String, Interval>,
    values: &mut ValueTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let _pattern_node = node.named_child(0); // pattern
    let binding_name = if let Some(p) = _pattern_node {
        if p.kind() == "identifier" {
            p.utf8_text(source.as_bytes()).ok().map(|s| s.to_string())
        } else {
            None
        }
    } else {
        None
    };

    let type_annotation_node = node.child_by_field_name("type");
    let mut value_node = None;
    let mut type_name = None;

    if let Some(type_ann) = type_annotation_node {
        let mut inner = type_ann.walk();
        for child in type_ann.children(&mut inner) {
            if child.kind() == "type_identifier" {
                type_name = child
                    .utf8_text(source.as_bytes())
                    .ok()
                    .map(|s| s.to_string());
                break;
            }
        }
    }

    if let Some(last_child) = node.named_child(node.named_child_count() - 1) {
        // Only exclude type_annotation. Do NOT exclude identifier.
        if last_child.kind() != "type_annotation" {
            value_node = Some(last_child);
        }
    }

    // 1. Resolve Value from expression
    let mut resolved_value: Option<i64> = None;

    if let Some(mut val) = value_node {
        // Peel wrappers
        while val.kind() == "literal" || val.kind() == "parenthesized_expression" {
            if let Some(child) = val.named_child(0) {
                val = child;
            } else {
                break;
            }
        }

        if val.kind() == "integer_literal" {
            resolved_value = val
                .utf8_text(source.as_bytes())
                .unwrap_or("0")
                .parse::<i64>()
                .ok();
        } else if val.kind() == "identifier" {
            // Lookup in ValueTable
            if let Ok(var_name) = val.utf8_text(source.as_bytes()) {
                resolved_value = values.lookup(var_name);
            }
        }
    }

    // 2. Register value if let binding has a name
    if let (Some(name), Some(val)) = (binding_name, resolved_value) {
        values.insert(name, val);
    }

    // 3. Verify refinement
    if let (Some(t_name), Some(int_val)) = (type_name, resolved_value)
        && let Some(interval) = refined_types.get(&t_name)
        && !interval.contains(int_val)
    {
        let vn = value_node.unwrap();
        let start = vn.start_position();
        let end = vn.end_position();
        diagnostics.push(Diagnostic {
            code: "REF_001".to_string(),
            severity: Severity::Error,
            location: Location {
                file: file.to_string(),
                line: start.row + 1,
                col: start.column + 1,
                end_line: Some(end.row + 1),
                end_col: Some(end.column + 1),
            },
            message: format!(
                "Refinement Violation: Value {} is out of bounds for type {}",
                int_val, t_name
            ),
            context: format!(
                "Type {} requires value in range [{}, {}]",
                t_name,
                if interval.min == i64::MIN {
                    "-inf".to_string()
                } else {
                    interval.min.to_string()
                },
                if interval.max == i64::MAX {
                    "inf".to_string()
                } else {
                    interval.max.to_string()
                }
            ),
            suggestions: vec![],
        });
    }
}
