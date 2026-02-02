//! Prelude loading — maps `@prelude(name)` directives to available stdlib modules.

use tree_sitter::Node;

/// Which prelude variant is active for a file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prelude {
    /// No `@prelude` directive — no stdlib modules in scope.
    None,
    /// `@prelude(core)` — Core types + String, List, Math.
    Core,
    /// `@prelude(script)` — Core + Console, Log, Time, Random, Env, Fs.
    Script,
}

/// Stdlib module names gated by prelude.
impl Prelude {
    /// Native modules (NativeRegistry): pure functions on data types.
    pub fn native_modules(&self) -> &[&str] {
        match self {
            Prelude::None => &[],
            Prelude::Core | Prelude::Script => &["Option", "Result", "String", "List"],
        }
    }

    /// Builtin modules (BuiltinRegistry): effect functions + Math.
    pub fn builtin_modules(&self) -> &[&str] {
        match self {
            Prelude::None => &[],
            Prelude::Core => &["Math"],
            Prelude::Script => &["Math", "Console", "Log", "Time", "Random", "Env", "Fs"],
        }
    }

    /// Module names available in the type checker's symbol table.
    pub fn type_modules(&self) -> &[&str] {
        match self {
            Prelude::None => &[],
            Prelude::Core => &["Option", "Result", "String", "List", "Math"],
            Prelude::Script => &[
                "Option", "Result", "String", "List", "Math",
                "Console", "Log", "Time", "Random", "Env", "Fs",
            ],
        }
    }
}

/// Walk the root AST node and extract the `@prelude(name)` variant.
///
/// Returns `Prelude::None` if no `@prelude` directive is found.
/// Returns an `Err` with the unknown name if the identifier is unrecognised.
pub fn extract_prelude<'a>(root: &Node<'a>, source: &str) -> Result<Prelude, String> {
    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        if child.kind() == "prelude_decl" {
            // The identifier is the first (and only) named child.
            if let Some(id_node) = child.named_child(0) {
                let name = id_node.utf8_text(source.as_bytes()).unwrap_or("");
                return match name {
                    "core" => Ok(Prelude::Core),
                    "script" => Ok(Prelude::Script),
                    other => Err(format!("Unknown prelude variant: `{}`", other)),
                };
            }
        }
    }
    Ok(Prelude::None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    fn parse(source: &str) -> Prelude {
        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let tree = parser.parse(source, None).unwrap();
        extract_prelude(&tree.root_node(), source).unwrap()
    }

    #[test]
    fn no_prelude() {
        assert_eq!(parse("main : () -> Int\nmain = || 1"), Prelude::None);
    }

    #[test]
    fn core_prelude() {
        assert_eq!(parse("@prelude(core)\nmain : () -> Int\nmain = || 1"), Prelude::Core);
    }

    #[test]
    fn script_prelude() {
        assert_eq!(parse("@prelude(script)\nmain : () -> Int\nmain = || 1"), Prelude::Script);
    }

    #[test]
    fn unknown_prelude_is_error() {
        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let source = "@prelude(unknown)\nmain : () -> Int\nmain = || 1";
        let tree = parser.parse(source, None).unwrap();
        assert!(extract_prelude(&tree.root_node(), source).is_err());
    }
}
