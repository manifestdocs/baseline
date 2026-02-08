use crate::diagnostics::{Diagnostic, Location, Severity};
use regex::Regex;
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

/// String-level constraint for refined string types.
#[derive(Debug, Clone)]
pub enum StringConstraint {
    /// String.matches(self, pattern)
    Matches(String),
    /// String.length bounds: min <= length <= max
    Length {
        min: Option<usize>,
        max: Option<usize>,
    },
    /// String.starts_with(self, prefix)
    StartsWith(String),
    /// String.ends_with(self, suffix)
    EndsWith(String),
    /// String.contains(self, substr)
    Contains(String),
    /// Intersection of multiple constraints (from `&&`)
    All(Vec<StringConstraint>),
}

impl StringConstraint {
    /// Check whether a string literal satisfies this constraint.
    /// Returns `Ok(())` on success, or `Err(reason)` on failure.
    pub fn check(&self, value: &str) -> Result<(), String> {
        match self {
            StringConstraint::Matches(pattern) => match Regex::new(pattern) {
                Ok(re) => {
                    if re.is_match(value) {
                        Ok(())
                    } else {
                        Err(format!("does not match pattern /{}/", pattern))
                    }
                }
                Err(e) => Err(format!("invalid regex pattern: {}", e)),
            },
            StringConstraint::Length { min, max } => {
                let len = value.len();
                if let Some(min_val) = min {
                    if len < *min_val {
                        return Err(format!(
                            "length {} is less than minimum {}",
                            len, min_val
                        ));
                    }
                }
                if let Some(max_val) = max {
                    if len > *max_val {
                        return Err(format!(
                            "length {} exceeds maximum {}",
                            len, max_val
                        ));
                    }
                }
                Ok(())
            }
            StringConstraint::StartsWith(prefix) => {
                if value.starts_with(prefix.as_str()) {
                    Ok(())
                } else {
                    Err(format!("does not start with \"{}\"", prefix))
                }
            }
            StringConstraint::EndsWith(suffix) => {
                if value.ends_with(suffix.as_str()) {
                    Ok(())
                } else {
                    Err(format!("does not end with \"{}\"", suffix))
                }
            }
            StringConstraint::Contains(substr) => {
                if value.contains(substr.as_str()) {
                    Ok(())
                } else {
                    Err(format!("does not contain \"{}\"", substr))
                }
            }
            StringConstraint::All(constraints) => {
                for c in constraints {
                    c.check(value)?;
                }
                Ok(())
            }
        }
    }

    /// Human-readable description of this constraint.
    pub fn describe(&self) -> String {
        match self {
            StringConstraint::Matches(p) => format!("matches /{}/", p),
            StringConstraint::Length { min, max } => match (min, max) {
                (Some(lo), Some(hi)) => format!("length in [{}, {}]", lo, hi),
                (Some(lo), None) => format!("length >= {}", lo),
                (None, Some(hi)) => format!("length <= {}", hi),
                (None, None) => "any length".to_string(),
            },
            StringConstraint::StartsWith(s) => format!("starts with \"{}\"", s),
            StringConstraint::EndsWith(s) => format!("ends with \"{}\"", s),
            StringConstraint::Contains(s) => format!("contains \"{}\"", s),
            StringConstraint::All(cs) => cs
                .iter()
                .map(|c| c.describe())
                .collect::<Vec<_>>()
                .join(" && "),
        }
    }
}

/// A refinement constraint — either numeric or string.
#[derive(Debug, Clone)]
pub enum Constraint {
    IntInterval(Interval),
    StringConstraint(StringConstraint),
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
    let mut refined_types: HashMap<String, Constraint> = HashMap::new();
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if child.kind() == "type_def" {
            if let Some((name, constraint)) = parse_refined_type(child, source) {
                refined_types.insert(name, constraint);
            }
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
    refined_types: &HashMap<String, Constraint>,
    values: &mut ValueTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let k = node.kind();

    // Scoping
    if k == "function_def" || k == "block" {
        values.enter_scope();
    }

    // Check let bindings
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

/// Parse a type definition and extract its refinement constraint (if any).
///
/// Supports both integer and string base types:
/// - `type Port = Int where self >= 1 && self <= 65535`
/// - `type Email = String where String.matches(self, "pattern")`
fn parse_refined_type(node: Node, source: &str) -> Option<(String, Constraint)> {
    let mut name = None;
    let mut refinement = None;
    let mut base_type = None;

    // Use field accessors when available (more reliable than positional iteration)
    if let Some(name_node) = node.child_by_field_name("name") {
        name = name_node
            .utf8_text(source.as_bytes())
            .ok()
            .map(|s| s.to_string());
    }
    if let Some(def_node) = node.child_by_field_name("def") {
        base_type = def_node
            .utf8_text(source.as_bytes())
            .ok()
            .map(|s| s.to_string());
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "refinement_clause" {
            refinement = Some(child);
        }
    }

    let name = name?;
    let refinement = refinement?;

    // Determine if this is a string or integer refinement
    let is_string = base_type.as_deref() == Some("String");

    if is_string {
        let constraint = parse_string_constraint(refinement, source)?;
        Some((name, Constraint::StringConstraint(constraint)))
    } else {
        // Default: integer interval
        let mut interval = Interval::full();
        let mut cursor = refinement.walk();
        for child in refinement.children(&mut cursor) {
            if child.kind() == "predicate" || child.kind() == "binary_expression" {
                update_interval_from_expr(child, source, &mut interval);
            }
        }

        if interval.min != i64::MIN || interval.max != i64::MAX {
            Some((name, Constraint::IntInterval(interval)))
        } else {
            None
        }
    }
}

/// Parse string constraints from a refinement clause AST node.
///
/// The AST structure is:
///   refinement_clause → predicate → call_expression | binary_expression
///   call_expression → field_expression("String.matches") + args
///   binary_expression (&&) → left_expr && right_expr
fn parse_string_constraint(node: Node, source: &str) -> Option<StringConstraint> {
    // Walk into refinement_clause → predicate → actual expression
    let expr = find_constraint_expr(node)?;
    parse_constraint_node(expr, source)
}

/// Find the actual constraint expression inside a refinement_clause.
fn find_constraint_expr(node: Node) -> Option<Node> {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "predicate" {
            // predicate wraps the actual expression
            return child.named_child(0).or(Some(child));
        }
        if child.kind() == "call_expression" || child.kind() == "binary_expression" {
            return Some(child);
        }
    }
    // fallback: return first named child
    node.named_child(0)
}

/// Parse a constraint from an AST node (call_expression or binary_expression).
fn parse_constraint_node(node: Node, source: &str) -> Option<StringConstraint> {
    match node.kind() {
        "binary_expression" => {
            // Could be && conjunction or comparison (length >= n)
            let children: Vec<_> = {
                let mut cursor = node.walk();
                node.children(&mut cursor).collect()
            };
            if children.len() >= 3 {
                let op = children[1].utf8_text(source.as_bytes()).unwrap_or("");
                if op == "&&" {
                    let mut parts = Vec::new();
                    parts.extend(collect_constraint_parts(children[0], source));
                    parts.extend(collect_constraint_parts(children[2], source));
                    return if parts.len() > 1 {
                        Some(StringConstraint::All(parts))
                    } else {
                        parts.into_iter().next()
                    };
                }
                // Comparison: String.length(self) >= n
                return parse_length_comparison(node, source);
            }
            None
        }
        "call_expression" => parse_call_constraint(node, source),
        _ => {
            // Try recursing into children
            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if let Some(c) = parse_constraint_node(child, source) {
                    return Some(c);
                }
            }
            None
        }
    }
}

/// Recursively collect leaf constraints from && conjunctions.
fn collect_constraint_parts(node: Node, source: &str) -> Vec<StringConstraint> {
    if node.kind() == "binary_expression" {
        let children: Vec<_> = {
            let mut cursor = node.walk();
            node.children(&mut cursor).collect()
        };
        if children.len() >= 3 {
            let op = children[1].utf8_text(source.as_bytes()).unwrap_or("");
            if op == "&&" {
                let mut parts = Vec::new();
                parts.extend(collect_constraint_parts(children[0], source));
                parts.extend(collect_constraint_parts(children[2], source));
                return parts;
            }
        }
    }
    // Leaf: try to parse this node directly
    if let Some(c) = parse_constraint_node(node, source) {
        vec![c]
    } else {
        vec![]
    }
}

/// Parse a call_expression like `String.matches(self, "pattern")`.
fn parse_call_constraint(node: Node, source: &str) -> Option<StringConstraint> {
    // Extract the method name from the field_expression child
    let field_expr = node.child_by_field_name("function")
        .or_else(|| {
            // Fallback: first child that is a field_expression
            let mut cursor = node.walk();
            node.children(&mut cursor)
                .find(|c| c.kind() == "field_expression")
        })?;

    let method_name = extract_method_name(field_expr, source)?;

    // Extract string argument (the second arg after "self")
    let string_arg = extract_call_string_arg(node, source);

    match method_name.as_str() {
        "matches" => {
            let arg = string_arg?;
            Some(StringConstraint::Matches(arg))
        }
        "starts_with" => {
            let arg = string_arg?;
            Some(StringConstraint::StartsWith(arg))
        }
        "ends_with" => {
            let arg = string_arg?;
            Some(StringConstraint::EndsWith(arg))
        }
        "contains" => {
            let arg = string_arg?;
            Some(StringConstraint::Contains(arg))
        }
        "length" => {
            // length() is used in comparisons, not directly as a constraint
            // This case is handled by parse_length_comparison at the parent level
            None
        }
        _ => None,
    }
}

/// Extract the method name from a field_expression like `String.matches`.
fn extract_method_name(node: Node, source: &str) -> Option<String> {
    // field_expression has children: object (type_identifier "String") + field (identifier "matches")
    let mut cursor = node.walk();
    let children: Vec<_> = node.children(&mut cursor).collect();

    // The method name is typically the last identifier child
    for child in children.iter().rev() {
        if child.kind() == "identifier" {
            return child.utf8_text(source.as_bytes()).ok().map(|s| s.to_string());
        }
    }
    None
}

/// Extract a string literal argument from a call_expression's arguments.
/// Looks for the string_literal node among the call's children (skipping "self").
fn extract_call_string_arg(node: Node, source: &str) -> Option<String> {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.kind() == "literal" || child.kind() == "string_literal" {
            return extract_string_value(child, source);
        }
    }
    None
}

/// Extract the text content from a string_literal node.
/// Handles tree-sitter's fragmented string representation:
///   string_literal → string_start + string_content* + escape_sequence* + string_end
fn extract_string_value(node: Node, source: &str) -> Option<String> {
    let mut target = node;
    // Unwrap literal wrapper if present
    if target.kind() == "literal" {
        target = target.named_child(0)?;
    }
    if target.kind() != "string_literal" {
        return None;
    }

    // Concatenate string_content and escape_sequence children
    let mut result = std::string::String::new();
    let mut cursor = target.walk();
    for child in target.children(&mut cursor) {
        match child.kind() {
            "string_content" => {
                if let Ok(text) = child.utf8_text(source.as_bytes()) {
                    result.push_str(text);
                }
            }
            "escape_sequence" => {
                if let Ok(text) = child.utf8_text(source.as_bytes()) {
                    // Convert escape sequences to their actual characters
                    match text {
                        "\\n" => result.push('\n'),
                        "\\t" => result.push('\t'),
                        "\\r" => result.push('\r'),
                        "\\\\" => result.push('\\'),
                        "\\\"" => result.push('"'),
                        "\\." => {
                            // Regex escape: keep as literal \. in the pattern
                            result.push_str(text);
                        }
                        _ => result.push_str(text),
                    }
                }
            }
            _ => {} // skip string_start, string_end
        }
    }

    if result.is_empty() {
        None
    } else {
        Some(result)
    }
}

/// Parse a comparison like `String.length(self) >= 1` from a binary_expression.
fn parse_length_comparison(node: Node, source: &str) -> Option<StringConstraint> {
    let children: Vec<_> = {
        let mut cursor = node.walk();
        node.children(&mut cursor).collect()
    };

    if children.len() < 3 {
        return None;
    }

    let left_text = children[0].utf8_text(source.as_bytes()).unwrap_or("");
    let op = children[1].utf8_text(source.as_bytes()).unwrap_or("");
    let right_text = children[2].utf8_text(source.as_bytes()).unwrap_or("");

    // Check if left side is String.length(self)
    let is_length_left = left_text.contains("length");
    let is_length_right = right_text.contains("length");

    if is_length_left {
        let num: usize = right_text.trim().parse().ok()?;
        match op {
            ">=" => Some(StringConstraint::Length { min: Some(num), max: None }),
            ">" => Some(StringConstraint::Length { min: Some(num + 1), max: None }),
            "<=" => Some(StringConstraint::Length { min: None, max: Some(num) }),
            "<" => Some(StringConstraint::Length { min: None, max: Some(num.saturating_sub(1)) }),
            _ => None,
        }
    } else if is_length_right {
        let num: usize = left_text.trim().parse().ok()?;
        // Reversed: n >= String.length(self) means length <= n
        match op {
            ">=" => Some(StringConstraint::Length { min: None, max: Some(num) }),
            ">" => Some(StringConstraint::Length { min: None, max: Some(num.saturating_sub(1)) }),
            "<=" => Some(StringConstraint::Length { min: Some(num), max: None }),
            "<" => Some(StringConstraint::Length { min: Some(num + 1), max: None }),
            _ => None,
        }
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
    refined_types: &HashMap<String, Constraint>,
    values: &mut ValueTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let _pattern_node = node.named_child(0);
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
        if last_child.kind() != "type_annotation" {
            value_node = Some(last_child);
        }
    }

    let constraint = type_name.as_ref().and_then(|t| refined_types.get(t));

    match constraint {
        Some(Constraint::IntInterval(interval)) => {
            check_int_refinement(
                node,
                source,
                file,
                &binding_name,
                value_node,
                type_name.as_deref().unwrap_or(""),
                interval,
                values,
                diagnostics,
            );
        }
        Some(Constraint::StringConstraint(sc)) => {
            check_string_refinement(
                node,
                source,
                file,
                value_node,
                type_name.as_deref().unwrap_or(""),
                sc,
                diagnostics,
            );
        }
        None => {
            // No refinement — still track integer values for later use
            if let Some(mut val) = value_node {
                while val.kind() == "literal"
                    || val.kind() == "parenthesized_expression"
                {
                    if let Some(child) = val.named_child(0) {
                        val = child;
                    } else {
                        break;
                    }
                }

                if val.kind() == "integer_literal" {
                    if let Some(int_val) = crate::parse::parse_int_literal(
                        val.utf8_text(source.as_bytes()).unwrap_or("0"),
                    ) {
                        if let Some(name) = binding_name {
                            values.insert(name, int_val);
                        }
                    }
                }
            }
        }
    }
}

/// Check an integer value against an integer interval refinement.
#[allow(clippy::too_many_arguments)]
fn check_int_refinement(
    _node: Node,
    source: &str,
    file: &str,
    binding_name: &Option<String>,
    value_node: Option<Node>,
    type_name: &str,
    interval: &Interval,
    values: &mut ValueTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut resolved_value: Option<i64> = None;

    if let Some(mut val) = value_node {
        while val.kind() == "literal" || val.kind() == "parenthesized_expression" {
            if let Some(child) = val.named_child(0) {
                val = child;
            } else {
                break;
            }
        }

        if val.kind() == "integer_literal" {
            resolved_value = crate::parse::parse_int_literal(
                val.utf8_text(source.as_bytes()).unwrap_or("0"),
            );
        } else if val.kind() == "identifier" {
            if let Ok(var_name) = val.utf8_text(source.as_bytes()) {
                resolved_value = values.lookup(var_name);
            }
        }
    }

    if let (Some(name), Some(val)) = (binding_name.clone(), resolved_value) {
        values.insert(name, val);
    }

    if let Some(int_val) = resolved_value {
        if !interval.contains(int_val) {
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
                    int_val, type_name
                ),
                context: format!(
                    "Type {} requires value in range [{}, {}]",
                    type_name,
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
}

/// Check a string literal against a string constraint refinement.
#[allow(clippy::too_many_arguments)]
fn check_string_refinement(
    _node: Node,
    source: &str,
    file: &str,
    value_node: Option<Node>,
    type_name: &str,
    constraint: &StringConstraint,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(mut val) = value_node {
        while val.kind() == "literal" || val.kind() == "parenthesized_expression" {
            if let Some(child) = val.named_child(0) {
                val = child;
            } else {
                break;
            }
        }

        if val.kind() == "string_literal" || val.kind() == "string_content" {
            let raw_text = val.utf8_text(source.as_bytes()).unwrap_or("");
            // Strip surrounding quotes if present
            let string_val = if raw_text.starts_with('"') && raw_text.ends_with('"') {
                &raw_text[1..raw_text.len() - 1]
            } else {
                raw_text
            };

            if let Err(reason) = constraint.check(string_val) {
                let start = val.start_position();
                let end = val.end_position();
                diagnostics.push(Diagnostic {
                    code: "REF_002".to_string(),
                    severity: Severity::Error,
                    location: Location {
                        file: file.to_string(),
                        line: start.row + 1,
                        col: start.column + 1,
                        end_line: Some(end.row + 1),
                        end_col: Some(end.column + 1),
                    },
                    message: format!(
                        "Refinement Violation: String {} for type {}",
                        reason, type_name
                    ),
                    context: format!(
                        "Type {} requires: {}",
                        type_name,
                        constraint.describe()
                    ),
                    suggestions: vec![],
                });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_constraint_matches() {
        let c = StringConstraint::Matches("^[a-z]+$".to_string());
        assert!(c.check("hello").is_ok());
        assert!(c.check("Hello").is_err());
        assert!(c.check("").is_err());
    }

    #[test]
    fn test_string_constraint_length() {
        let c = StringConstraint::Length {
            min: Some(1),
            max: Some(5),
        };
        assert!(c.check("hi").is_ok());
        assert!(c.check("hello").is_ok());
        assert!(c.check("").is_err());
        assert!(c.check("toolong").is_err());
    }

    #[test]
    fn test_string_constraint_starts_with() {
        let c = StringConstraint::StartsWith("https://".to_string());
        assert!(c.check("https://example.com").is_ok());
        assert!(c.check("http://example.com").is_err());
    }

    #[test]
    fn test_string_constraint_contains() {
        let c = StringConstraint::Contains("@".to_string());
        assert!(c.check("user@example.com").is_ok());
        assert!(c.check("no-at-sign").is_err());
    }

    #[test]
    fn test_string_constraint_all() {
        let c = StringConstraint::All(vec![
            StringConstraint::Length {
                min: Some(5),
                max: None,
            },
            StringConstraint::Contains("@".to_string()),
        ]);
        assert!(c.check("user@example.com").is_ok());
        assert!(c.check("u@e").is_err()); // too short
        assert!(c.check("no-at-sign-here").is_err()); // no @
    }

    #[test]
    fn test_end_to_end_matches_violation() {
        let source = r#"
type Email = String where String.matches(self, "^[^@]+@[^@]+\.[^@]+$")
fn check() -> () = {
  let bad : Email = "not-an-email"
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        let diags = check_refinements(&tree, source, "test.bl");
        assert!(
            diags.iter().any(|d| d.code == "REF_002"),
            "Expected REF_002 for invalid email, got: {:?}",
            diags
        );
    }

    #[test]
    fn test_end_to_end_matches_valid() {
        let source = r#"
type Email = String where String.matches(self, "^[^@]+@[^@]+\.[^@]+$")
fn check() -> () = {
  let good : Email = "user@example.com"
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        let diags = check_refinements(&tree, source, "test.bl");
        assert!(
            !diags.iter().any(|d| d.code == "REF_002"),
            "Expected no REF_002 for valid email, got: {:?}",
            diags
        );
    }

    #[test]
    fn test_end_to_end_length_violation() {
        let source = r#"
type ShortName = String where String.length(self) >= 1 && String.length(self) <= 5
fn check() -> () = {
  let bad : ShortName = "toolong"
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        let diags = check_refinements(&tree, source, "test.bl");
        assert!(
            diags.iter().any(|d| d.code == "REF_002"),
            "Expected REF_002 for too-long name, got: {:?}",
            diags
        );
    }

    #[test]
    fn test_end_to_end_starts_with() {
        let source = r#"
type HttpsUrl = String where String.starts_with(self, "https://")
fn check() -> () = {
  let bad : HttpsUrl = "http://insecure.com"
}
"#;
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        let diags = check_refinements(&tree, source, "test.bl");
        assert!(
            diags.iter().any(|d| d.code == "REF_002"),
            "Expected REF_002 for http URL, got: {:?}",
            diags
        );
    }

    #[test]
    fn test_interval_contains() {
        let i = Interval { min: 1, max: 100 };
        assert!(i.contains(1));
        assert!(i.contains(50));
        assert!(i.contains(100));
        assert!(!i.contains(0));
        assert!(!i.contains(101));
    }
}
