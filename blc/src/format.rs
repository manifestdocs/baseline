//! CST-based code formatter for Baseline.
//!
//! Walks tree-sitter nodes and emits canonically formatted source.
//! Falls back to raw source text for unhandled node kinds (safety net).
//!
//! Rules:
//! - 2-space indentation
//! - Blank line between top-level definitions
//! - Canonical spacing around operators
//! - Preserve comments in place
//! - No trailing whitespace
//! - Single trailing newline

use tree_sitter::{Node, Parser};
use tree_sitter_baseline::LANGUAGE;

/// Format Baseline source code, returning the formatted string.
pub fn format_source(source: &str) -> Result<String, String> {
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .map_err(|e| format!("Failed to load grammar: {}", e))?;

    let tree = parser
        .parse(source, None)
        .ok_or_else(|| "Failed to parse source".to_string())?;

    let root = tree.root_node();
    let mut fmt = Formatter::new(source);
    fmt.format_node(&root);
    Ok(fmt.finish())
}

struct Formatter<'a> {
    source: &'a str,
    output: String,
    indent: usize,
}

impl<'a> Formatter<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            output: String::with_capacity(source.len()),
            indent: 0,
        }
    }

    fn finish(mut self) -> String {
        // Ensure single trailing newline
        let trimmed = self.output.trim_end().to_string();
        self.output = trimmed;
        self.output.push('\n');
        self.output
    }

    fn text(&self, node: &Node) -> &'a str {
        node.utf8_text(self.source.as_bytes()).unwrap_or("")
    }

    fn push(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn push_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("  ");
        }
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn blank_line(&mut self) {
        // Ensure exactly one blank line (two newlines)
        let trimmed = self.output.trim_end_matches(' ');
        let trailing_newlines = trimmed.len() - trimmed.trim_end_matches('\n').len();
        match trailing_newlines {
            0 => {
                self.newline();
                self.newline();
            }
            1 => {
                self.newline();
            }
            _ => {} // already have blank line
        }
    }

    fn format_node(&mut self, node: &Node) {
        match node.kind() {
            "source_file" => self.format_source_file(node),
            "prelude_decl" => self.format_prelude(node),
            "module_decl" => self.format_raw(node),
            "import_decl" => self.format_raw(node),
            "function_def" => self.format_function_def(node),
            "type_def" => self.format_type_def(node),
            "effect_def" => self.format_raw(node),
            "inline_test" => self.format_inline_test(node),
            _ => self.format_raw(node),
        }
    }

    /// Format the root source_file node.
    ///
    /// Strategy: preserve blank lines from the original source between
    /// top-level items. If adjacent items had a blank line in the source,
    /// keep it. If not, don't add one. The only rule we enforce: at least
    /// one blank line between top-level definitions (function_def, type_def,
    /// effect_def, inline_test).
    fn format_source_file(&mut self, node: &Node) {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();

        let mut prev_end_row: Option<usize> = None;
        let mut prev_was_def = false;

        for child in &children {
            let kind = child.kind();
            let start_row = child.start_position().row;

            let is_comment = kind == "line_comment" || kind == "block_comment";
            let is_def = matches!(
                kind,
                "function_def" | "type_def" | "effect_def" | "inline_test"
            );

            if let Some(per) = prev_end_row {
                let gap = start_row.saturating_sub(per);
                if gap >= 2 {
                    // Original source had a blank line — preserve it
                    self.blank_line();
                } else if prev_was_def && is_def {
                    // Force blank line between adjacent definitions
                    self.blank_line();
                }
            }

            if is_comment {
                self.push_indent();
                self.push(self.text(child));
                self.newline();
            } else {
                self.format_node(child);
                if !self.output.ends_with('\n') {
                    self.newline();
                }
            }

            // Track prev_was_def: set true for defs, keep true through
            // comment runs (doc comments), reset for other nodes
            if is_def {
                prev_was_def = true;
            } else if !is_comment {
                prev_was_def = false;
            }
            prev_end_row = Some(child.end_position().row);
        }
    }

    fn format_prelude(&mut self, node: &Node) {
        self.push(self.text(node));
    }

    fn format_function_def(&mut self, node: &Node) {
        // Use field accessors: name, signature, body
        let name_node = node.child_by_field_name("name");
        let sig_node = node.child_by_field_name("signature");
        let body_node = node.child_by_field_name("body");

        let name = name_node.map(|n| self.text(&n)).unwrap_or("?");

        // Check for 'export' keyword
        let mut cursor = node.walk();
        let has_export = node
            .children(&mut cursor)
            .any(|c| self.text(&c) == "export");

        // Signature line: [export] name : signature
        self.push_indent();
        if has_export {
            self.push("export ");
        }
        self.push(name);
        if let Some(sig) = sig_node {
            self.push(" : ");
            self.format_type_signature(&sig);
        }
        self.newline();

        // Implementation line: name = body
        self.push_indent();
        self.push(name);
        if let Some(body) = body_node {
            self.push(" = ");
            self.format_expression(&body);
        }

        // Optional where block
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "where_block" {
                self.newline();
                self.format_where_block(&child);
            }
        }
    }

    fn format_type_signature(&mut self, node: &Node) {
        // type_signature: _type_expr -> optional(effect_set) _type_expr
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();

        let mut first = true;
        for child in &children {
            let txt = self.text(child);
            if txt == "->" {
                self.push(" -> ");
            } else if child.kind() == "effect_set" {
                self.format_effect_set(child);
                self.push(" ");
            } else {
                if !first && !self.output.ends_with(' ') && !self.output.ends_with('>') {
                    self.push(", ");
                }
                self.format_type_expr(child);
            }
            first = false;
        }
    }

    fn format_effect_set(&mut self, node: &Node) {
        self.push("{");
        let mut cursor = node.walk();
        let mut first = true;
        for child in node.children(&mut cursor) {
            let txt = self.text(&child);
            if txt == "{" || txt == "}" {
                continue;
            }
            if txt == "," {
                continue;
            }
            if child.kind() == "type_identifier" {
                if !first {
                    self.push(", ");
                }
                self.push(txt);
                first = false;
            }
        }
        self.push("}");
    }

    fn format_type_expr(&mut self, node: &Node) {
        // Fall back to raw text for type expressions
        self.push(self.text(node));
    }

    fn format_type_def(&mut self, node: &Node) {
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();
        let mut i = 0;

        self.push_indent();

        // optional 'export'
        if i < children.len() && self.text(&children[i]) == "export" {
            self.push("export ");
            i += 1;
        }

        // 'type'
        if i < children.len() && self.text(&children[i]) == "type" {
            self.push("type ");
            i += 1;
        }

        // name
        if i < children.len() {
            self.push(self.text(&children[i]));
            i += 1;
        }

        // optional type_params
        if i < children.len() && children[i].kind() == "type_params" {
            self.push(self.text(&children[i]));
            i += 1;
        }

        // '='
        if i < children.len() && self.text(&children[i]) == "=" {
            self.push(" =");
            i += 1;
        }

        // def (variant_list or type expression)
        if i < children.len() {
            let def = &children[i];
            if def.kind() == "variant_list" {
                self.format_variant_list(def);
            } else if def.kind() == "record_type" {
                self.push(" ");
                self.format_record_type(def);
            } else {
                self.push(" ");
                self.push(self.text(def));
            }
            i += 1;
        }

        // optional refinement_clause
        if i < children.len() && children[i].kind() == "refinement_clause" {
            self.push(" ");
            self.push(self.text(&children[i]));
        }
    }

    fn format_variant_list(&mut self, node: &Node) {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "variant" {
                self.newline();
                self.indent += 1;
                self.push_indent();
                self.push("| ");
                self.push(self.text(&child).trim_start_matches("| ").trim());
                self.indent -= 1;
            }
        }
    }

    fn format_record_type(&mut self, node: &Node) {
        let txt = self.text(node);
        // Multi-line if it contains multiple fields
        if txt.contains(',') {
            self.push("{");
            self.newline();
            self.indent += 1;
            let mut cursor = node.walk();
            let mut first = true;
            for child in node.children(&mut cursor) {
                if child.kind() == "record_field_def" {
                    if !first {
                        self.push(",");
                        self.newline();
                    }
                    self.push_indent();
                    self.push(self.text(&child).trim());
                    first = false;
                }
            }
            self.push(",");
            self.newline();
            self.indent -= 1;
            self.push_indent();
            self.push("}");
        } else {
            self.push(txt);
        }
    }

    fn format_inline_test(&mut self, node: &Node) {
        self.push_indent();
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();

        // test "name" = expression
        let mut i = 0;
        while i < children.len() {
            let txt = self.text(&children[i]);
            if txt == "test" {
                self.push("test ");
                i += 1;
                continue;
            }
            if children[i].kind() == "string_literal" {
                self.push(txt);
                i += 1;
                continue;
            }
            if txt == "=" {
                self.push(" = ");
                i += 1;
                if i < children.len() {
                    self.format_expression(&children[i]);
                    i += 1;
                }
                continue;
            }
            i += 1;
        }
    }

    fn format_where_block(&mut self, node: &Node) {
        self.push_indent();
        self.push("where");
        self.newline();
        self.indent += 1;
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "inline_test" {
                self.format_inline_test(&child);
                self.newline();
            }
        }
        self.indent -= 1;
    }

    fn format_expression(&mut self, node: &Node) {
        match node.kind() {
            "block" | "block_expression" => self.format_block(node),
            "binary_expression" => self.format_binary(node),
            "pipe_expression" => self.format_pipe(node),
            "if_expression" => self.format_if(node),
            "match_expression" => self.format_match(node),
            "for_expression" => self.format_for(node),
            "with_expression" => self.format_with(node),
            "lambda" | "lambda_expression" => self.format_lambda(node),
            "call_expression" => self.format_call(node),
            "let_binding" => self.format_let(node),
            _ => self.push(self.text(node)),
        }
    }

    fn format_block(&mut self, node: &Node) {
        self.push("{");
        self.newline();
        self.indent += 1;

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            let kind = child.kind();
            if kind == "{" || kind == "}" || kind == ";" {
                continue;
            }
            if kind == "line_comment" || kind == "block_comment" {
                self.push_indent();
                self.push(self.text(&child));
                self.newline();
                continue;
            }
            self.push_indent();
            self.format_expression(&child);
            self.newline();
        }

        self.indent -= 1;
        self.push_indent();
        self.push("}");
    }

    fn format_binary(&mut self, node: &Node) {
        let count = node.child_count();
        if count >= 3 {
            // left op right
            let left = node.child(0).unwrap();
            let op = node.child(1).unwrap();
            let right = node.child(2).unwrap();

            self.format_expression(&left);
            self.push(" ");
            self.push(self.text(&op));
            self.push(" ");
            self.format_expression(&right);
        } else {
            self.push(self.text(node));
        }
    }

    fn format_pipe(&mut self, node: &Node) {
        if node.named_child_count() >= 2 {
            let left = node.named_child(0).unwrap();
            let right = node.named_child(1).unwrap();
            self.format_expression(&left);
            self.push(" |> ");
            self.format_expression(&right);
        } else {
            self.push(self.text(node));
        }
    }

    fn format_if(&mut self, node: &Node) {
        // if cond then expr [else expr]
        self.push(self.text(node));
    }

    fn format_match(&mut self, node: &Node) {
        self.push(self.text(node));
    }

    fn format_for(&mut self, node: &Node) {
        self.push(self.text(node));
    }

    fn format_with(&mut self, node: &Node) {
        // with effect { body }
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();
        let mut i = 0;

        while i < children.len() {
            let txt = self.text(&children[i]);
            if txt == "with" {
                self.push("with ");
                i += 1;
                continue;
            }
            if children[i].kind() == "field_expression" {
                self.push(txt);
                self.push(" ");
                i += 1;
                continue;
            }
            if children[i].kind() == "block" {
                self.format_block(&children[i]);
                i += 1;
                continue;
            }
            i += 1;
        }
    }

    fn format_lambda(&mut self, node: &Node) {
        // Lambda: named children are params (all but last) + body (last)
        let count = node.named_child_count();
        if count == 0 {
            self.push(self.text(node));
            return;
        }

        self.push("|");
        // All named children except the last are parameters
        for i in 0..count - 1 {
            if i > 0 {
                self.push(", ");
            }
            let param = node.named_child(i).unwrap();
            self.push(self.text(&param));
        }
        self.push("| ");

        // Last named child is the body
        let body = node.named_child(count - 1).unwrap();
        self.format_expression(&body);
    }

    fn format_call(&mut self, node: &Node) {
        // func(arg1, arg2)
        let count = node.named_child_count();
        if count == 0 {
            self.push(self.text(node));
            return;
        }

        let func = node.named_child(0).unwrap();
        self.format_expression(&func);
        self.push("(");
        for i in 1..count {
            if i > 1 {
                self.push(", ");
            }
            let arg = node.named_child(i).unwrap();
            self.format_expression(&arg);
        }
        self.push(")");
    }

    fn format_let(&mut self, node: &Node) {
        // let pattern [: type] = expr
        let mut cursor = node.walk();
        let children: Vec<Node> = node.children(&mut cursor).collect();

        self.push("let ");
        let mut i = 0;
        while i < children.len() {
            let txt = self.text(&children[i]);
            if txt == "let" {
                i += 1;
                continue;
            }
            if txt == "=" {
                self.push("= ");
                i += 1;
                if i < children.len() {
                    self.format_expression(&children[i]);
                }
                break;
            }
            if children[i].kind() == "type_annotation" {
                self.push(self.text(&children[i]));
                self.push(" ");
                i += 1;
                continue;
            }
            // Pattern
            self.push(txt);
            self.push(" ");
            i += 1;
        }
    }

    /// Emit raw source text for a node (fallback).
    fn format_raw(&mut self, node: &Node) {
        self.push_indent();
        self.push(self.text(node));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_idempotency() {
        let source = r#"@prelude(script)

greet : String -> String
greet = |name| "Hello, ${name}!"
"#;
        let once = format_source(source).unwrap();
        let twice = format_source(&once).unwrap();
        assert_eq!(once, twice, "format should be idempotent");
    }

    #[test]
    fn test_trailing_newline() {
        let source = "@prelude(script)";
        let result = format_source(source).unwrap();
        assert!(result.ends_with('\n'), "should end with newline");
        assert!(
            !result.ends_with("\n\n"),
            "should not end with multiple newlines"
        );
    }

    #[test]
    fn test_blank_line_between_defs() {
        let source =
            "greet : String -> String\ngreet = |name| name\nadd : Int -> Int\nadd = |n| n + 1\n";
        let result = format_source(source).unwrap();
        assert!(
            result.contains("\n\n"),
            "should have blank line between function defs: {:?}",
            result
        );
    }

    #[test]
    fn test_preserves_comments() {
        let source = "// A comment\n@prelude(script)\n";
        let result = format_source(source).unwrap();
        assert!(result.contains("// A comment"), "should preserve comments");
    }

    #[test]
    fn test_format_binary_spacing() {
        let source = r#"add : Int -> Int
add = |n| n + 1
"#;
        let result = format_source(source).unwrap();
        assert!(
            result.contains("n + 1"),
            "should have spaces around operator"
        );
    }

    #[test]
    fn test_format_type_def_enum() {
        let source = "type Color = | Red | Green | Blue\n";
        let result = format_source(source).unwrap();
        assert!(result.contains("type Color ="), "should format type def");
        assert!(result.contains("| Red"), "should have variant");
    }

    #[test]
    fn test_no_trailing_whitespace() {
        let source = "@prelude(script)\n\ngreet : String -> String\ngreet = |name| name\n";
        let result = format_source(source).unwrap();
        for (i, line) in result.lines().enumerate() {
            assert!(
                !line.ends_with(' '),
                "line {} has trailing whitespace: {:?}",
                i + 1,
                line
            );
        }
    }
}
