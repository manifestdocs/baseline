//! Prelude loading — maps `@prelude(name)` directives to available stdlib modules.

use tree_sitter::Node;

/// Which prelude variant is active for a file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prelude {
    /// No `@prelude` directive — no stdlib modules in scope.
    None,
    /// `@prelude(minimal)` — Only Option/Result core types.
    Minimal,
    /// `@prelude(pure)` — Option, Result, String, List, Math — no effects.
    Pure,
    /// `@prelude(core)` — Core types + String, List, Math.
    Core,
    /// `@prelude(script)` — Core + Console, Log, Time, Random, Env, Fs.
    Script,
    /// `@prelude(server)` — Script + Router, Server, Db, Metrics.
    Server,
}

/// Stdlib module names gated by prelude.
impl Prelude {
    /// Native modules (NativeRegistry): functions operating on RuntimeValues directly.
    pub fn native_modules(&self) -> &[&str] {
        match self {
            Prelude::None => &[],
            Prelude::Minimal => &["Option", "Result"],
            Prelude::Pure | Prelude::Core => &["Option", "Result", "String", "List", "Json"],
            Prelude::Script => &[
                "Option", "Result", "String", "List", "Json", "Http", "Response", "Request",
            ],
            Prelude::Server => &[
                "Option", "Result", "String", "List", "Json", "Http", "Response", "Request",
                "Router",
            ],
        }
    }

    /// Builtin modules (BuiltinRegistry): string-based effect functions + Math.
    pub fn builtin_modules(&self) -> &[&str] {
        match self {
            Prelude::None | Prelude::Minimal => &[],
            Prelude::Pure | Prelude::Core => &["Math"],
            Prelude::Script => &["Math", "Console", "Log", "Time", "Random", "Env", "Fs"],
            Prelude::Server => &[
                "Math", "Console", "Log", "Time", "Env", "Server", "Db", "Metrics",
            ],
        }
    }

    /// Module names available in the type checker's symbol table.
    pub fn type_modules(&self) -> &[&str] {
        match self {
            Prelude::None => &[],
            Prelude::Minimal => &["Option", "Result"],
            Prelude::Pure | Prelude::Core => {
                &["Option", "Result", "String", "List", "Json", "Math"]
            }
            Prelude::Script => &[
                "Option", "Result", "String", "List", "Json", "Math", "Console", "Log", "Time",
                "Random", "Env", "Fs", "Http", "Response", "Request",
            ],
            Prelude::Server => &[
                "Option", "Result", "String", "List", "Json", "Math", "Console", "Log", "Time",
                "Env", "Http", "Response", "Request", "Router", "Server", "Db", "Metrics",
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
                    "minimal" => Ok(Prelude::Minimal),
                    "pure" => Ok(Prelude::Pure),
                    "core" => Ok(Prelude::Core),
                    "script" => Ok(Prelude::Script),
                    "server" => Ok(Prelude::Server),
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
    fn minimal_prelude() {
        assert_eq!(
            parse("@prelude(minimal)\nmain : () -> Int\nmain = || 1"),
            Prelude::Minimal
        );
    }

    #[test]
    fn pure_prelude() {
        assert_eq!(
            parse("@prelude(pure)\nmain : () -> Int\nmain = || 1"),
            Prelude::Pure
        );
    }

    #[test]
    fn core_prelude() {
        assert_eq!(
            parse("@prelude(core)\nmain : () -> Int\nmain = || 1"),
            Prelude::Core
        );
    }

    #[test]
    fn script_prelude() {
        assert_eq!(
            parse("@prelude(script)\nmain : () -> Int\nmain = || 1"),
            Prelude::Script
        );
    }

    #[test]
    fn server_prelude() {
        assert_eq!(
            parse("@prelude(server)\nmain : () -> Int\nmain = || 1"),
            Prelude::Server
        );
    }

    #[test]
    fn server_has_router_and_server_modules() {
        let p = Prelude::Server;
        assert!(p.native_modules().contains(&"Router"));
        assert!(p.native_modules().contains(&"Http"));
        assert!(p.builtin_modules().contains(&"Server"));
        assert!(p.builtin_modules().contains(&"Db"));
        assert!(p.builtin_modules().contains(&"Metrics"));
        assert!(p.type_modules().contains(&"Router"));
        assert!(p.type_modules().contains(&"Server"));
    }

    #[test]
    fn minimal_has_only_option_result() {
        let p = Prelude::Minimal;
        assert_eq!(p.native_modules(), &["Option", "Result"]);
        assert!(p.builtin_modules().is_empty());
        assert_eq!(p.type_modules(), &["Option", "Result"]);
    }

    #[test]
    fn pure_has_no_effects() {
        let p = Prelude::Pure;
        assert_eq!(
            p.native_modules(),
            &["Option", "Result", "String", "List", "Json"]
        );
        assert_eq!(p.builtin_modules(), &["Math"]);
        assert_eq!(
            p.type_modules(),
            &["Option", "Result", "String", "List", "Json", "Math"]
        );
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
