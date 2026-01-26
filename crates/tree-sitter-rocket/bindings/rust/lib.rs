//! Tree-sitter grammar for the Rocket programming language.
//!
//! This crate provides Rust bindings for the tree-sitter-rocket grammar,
//! enabling parsing of Rocket source code into a concrete syntax tree.

use tree_sitter_language::LanguageFn;

extern "C" {
    fn tree_sitter_rocket() -> *const ();
}

/// Returns the tree-sitter Language for Rocket.
///
/// # Example
///
/// ```
/// let language = tree_sitter_rocket::LANGUAGE;
/// let mut parser = tree_sitter::Parser::new();
/// parser.set_language(&language.into()).expect("Error loading Rocket grammar");
/// ```
pub const LANGUAGE: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_rocket) };

/// Returns the syntax highlighting query for Rocket.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");

/// Returns the locals query for scope tracking.
pub const LOCALS_QUERY: &str = include_str!("../../queries/locals.scm");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_can_load_grammar() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&LANGUAGE.into())
            .expect("Error loading Rocket grammar");
    }
}
