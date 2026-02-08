use std::collections::HashMap;
use std::path::{Path, PathBuf};

use tree_sitter::{Parser, Tree};
use tree_sitter_baseline::LANGUAGE;

use crate::diagnostics::{Diagnostic, Location, Severity};

/// Describes what an imported module exports: function names with their type signatures.
#[derive(Debug, Clone)]
pub struct ModuleExports {
    /// Function name -> (param_names, signature_text)
    pub functions: Vec<(String, Option<String>)>,
    /// Type names (structs/enums)
    pub types: Vec<String>,
}

/// The kind of import the user wrote.
#[derive(Debug, Clone)]
pub enum ImportKind {
    /// `import Foo` — qualified access only (Foo.bar)
    Qualified,
    /// `import Foo.{bar, baz}` — selective: inject named symbols directly
    Selective(Vec<String>),
    /// `import Foo.*` — wildcard: inject all exports directly
    Wildcard,
}

/// A resolved import: module name, path, and import kind.
#[derive(Debug, Clone)]
pub struct ResolvedImport {
    pub module_name: String,
    pub file_path: PathBuf,
    pub kind: ImportKind,
}

/// Owns all imported source strings and parse trees, keeping Node lifetimes valid.
pub struct ModuleLoader {
    /// Owns (canonical_path, source, tree) — keeps Node references alive
    modules: Vec<(PathBuf, String, Tree)>,
    /// Cycle detection: files currently being resolved
    resolution_stack: Vec<PathBuf>,
    /// Cache: canonical path -> index in modules vec
    loaded: HashMap<PathBuf, usize>,
    /// Base directory for resolving imports
    base_dir: Option<PathBuf>,
}

impl Default for ModuleLoader {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
            resolution_stack: Vec::new(),
            loaded: HashMap::new(),
            base_dir: None,
        }
    }

    pub fn with_base_dir(base_dir: PathBuf) -> Self {
        Self {
            modules: Vec::new(),
            resolution_stack: Vec::new(),
            loaded: HashMap::new(),
            base_dir: Some(base_dir),
        }
    }

    /// Get the base directory for imports.
    pub fn base_dir(&self) -> Option<&Path> {
        self.base_dir.as_deref()
    }

    /// Parse import declarations from a source file's root node.
    pub fn parse_imports<'a>(
        root: &tree_sitter::Node<'a>,
        source: &str,
    ) -> Vec<(ResolvedImport, tree_sitter::Node<'a>)> {
        let mut imports = Vec::new();
        let mut cursor = root.walk();

        for child in root.children(&mut cursor) {
            if child.kind() != "import_decl" {
                continue;
            }

            // Find the import_path node (field "path", or first named child)
            let path_node = child.child_by_field_name("path").or_else(|| {
                // Fallback: find import_path or module_path child
                let mut c = child.walk();
                child
                    .children(&mut c)
                    .find(|n| n.kind() == "import_path" || n.kind() == "module_path")
            });

            let path_node = match path_node {
                Some(n) => n,
                None => continue,
            };

            // Collect type_identifiers (module path) and identifiers (selective imports)
            let mut path_parts = Vec::new();
            let mut selected_names = Vec::new();
            let mut has_wildcard = false;

            let mut ip_cursor = path_node.walk();
            for ip_child in path_node.children(&mut ip_cursor) {
                match ip_child.kind() {
                    "type_identifier" => {
                        let part = ip_child.utf8_text(source.as_bytes()).unwrap_or("");
                        path_parts.push(part.to_string());
                    }
                    "identifier" => {
                        let name = ip_child.utf8_text(source.as_bytes()).unwrap_or("");
                        selected_names.push(name.to_string());
                    }
                    _ => {
                        // Check for anonymous '*' token
                        if !ip_child.is_named() {
                            let text = ip_child.utf8_text(source.as_bytes()).unwrap_or("");
                            if text == "*" {
                                has_wildcard = true;
                            }
                        }
                    }
                }
            }

            if path_parts.is_empty() {
                continue;
            }

            let module_name = path_parts.join(".");

            let kind = if has_wildcard {
                ImportKind::Wildcard
            } else if !selected_names.is_empty() {
                ImportKind::Selective(selected_names)
            } else {
                ImportKind::Qualified
            };

            imports.push((
                ResolvedImport {
                    module_name,
                    file_path: PathBuf::new(),
                    kind,
                },
                child,
            ));
        }

        imports
    }

    /// Resolve a module name to a file path.
    #[allow(clippy::result_large_err)]
    pub fn resolve_path(
        &self,
        module_name: &str,
        import_node: &tree_sitter::Node,
        file: &str,
    ) -> Result<PathBuf, Diagnostic> {
        let base = match &self.base_dir {
            Some(dir) => dir.clone(),
            None => {
                return Err(Diagnostic {
                    code: "IMP_001".to_string(),
                    severity: Severity::Error,
                    location: node_location(file, import_node),
                    message: format!("Cannot resolve import `{}`: no base directory", module_name),
                    context: "Imports require a file-based context.".to_string(),
                    suggestions: vec![],
                });
            }
        };

        // Convert module name to path components: "Api.Users" -> ["Api", "Users"]
        let parts: Vec<&str> = module_name.split('.').collect();

        // Build candidate paths
        let mut candidates = Vec::new();

        if parts.len() == 1 {
            let name = parts[0];
            // {dir}/Name.bl, {dir}/name.bl, {dir}/lib/Name.bl, {dir}/lib/name.bl
            candidates.push(base.join(format!("{}.bl", name)));
            candidates.push(base.join(format!("{}.bl", name.to_lowercase())));
            candidates.push(base.join("lib").join(format!("{}.bl", name)));
            candidates.push(base.join("lib").join(format!("{}.bl", name.to_lowercase())));
        } else {
            // Dotted path: Api.Users -> Api/Users.bl, api/users.bl, lib/Api/Users.bl, lib/api/users.bl
            let last = parts.last().unwrap();
            let dir_parts = &parts[..parts.len() - 1];

            let exact_dir: PathBuf = dir_parts.iter().collect();
            let lower_dir: PathBuf = dir_parts.iter().map(|p| p.to_lowercase()).collect();

            candidates.push(base.join(&exact_dir).join(format!("{}.bl", last)));
            candidates.push(
                base.join(&lower_dir)
                    .join(format!("{}.bl", last.to_lowercase())),
            );
            candidates.push(
                base.join("lib")
                    .join(&exact_dir)
                    .join(format!("{}.bl", last)),
            );
            candidates.push(
                base.join("lib")
                    .join(&lower_dir)
                    .join(format!("{}.bl", last.to_lowercase())),
            );
        }

        for candidate in &candidates {
            if candidate.exists() {
                return Ok(candidate.clone());
            }
        }

        let tried = candidates
            .iter()
            .map(|p| p.display().to_string())
            .collect::<Vec<_>>()
            .join(", ");

        Err(Diagnostic {
            code: "IMP_001".to_string(),
            severity: Severity::Error,
            location: node_location(file, import_node),
            message: format!("Module `{}` not found", module_name),
            context: format!("Searched: {}", tried),
            suggestions: vec![],
        })
    }

    /// Load and parse a module file. Returns index into internal storage.
    /// Performs cycle detection and caching.
    #[allow(clippy::result_large_err)]
    pub fn load_module(
        &mut self,
        path: &Path,
        import_node: &tree_sitter::Node,
        file: &str,
    ) -> Result<usize, Diagnostic> {
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());

        // Check cache
        if let Some(&idx) = self.loaded.get(&canonical) {
            return Ok(idx);
        }

        // Check for circular imports
        if self.resolution_stack.contains(&canonical) {
            let cycle = self
                .resolution_stack
                .iter()
                .map(|p| p.display().to_string())
                .collect::<Vec<_>>()
                .join(" -> ");
            return Err(Diagnostic {
                code: "IMP_002".to_string(),
                severity: Severity::Error,
                location: node_location(file, import_node),
                message: format!(
                    "Circular import detected: {} -> {}",
                    cycle,
                    canonical.display()
                ),
                context: "Modules cannot import each other in a cycle.".to_string(),
                suggestions: vec![],
            });
        }

        // Read source
        let source = std::fs::read_to_string(path).map_err(|e| Diagnostic {
            code: "IMP_001".to_string(),
            severity: Severity::Error,
            location: node_location(file, import_node),
            message: format!("Failed to read module `{}`: {}", path.display(), e),
            context: "Check that the file exists and is readable.".to_string(),
            suggestions: vec![],
        })?;

        // Parse
        let mut parser = Parser::new();
        parser
            .set_language(&LANGUAGE.into())
            .expect("Failed to load Baseline grammar");

        let tree = parser.parse(&source, None).ok_or_else(|| Diagnostic {
            code: "IMP_003".to_string(),
            severity: Severity::Error,
            location: node_location(file, import_node),
            message: format!("Failed to parse module `{}`", path.display()),
            context: "The imported module has syntax errors.".to_string(),
            suggestions: vec![],
        })?;

        // Check for syntax errors in the imported module
        if has_syntax_errors(&tree.root_node()) {
            return Err(Diagnostic {
                code: "IMP_003".to_string(),
                severity: Severity::Error,
                location: node_location(file, import_node),
                message: format!("Imported module `{}` has syntax errors", path.display()),
                context: "Fix the errors in the imported module first.".to_string(),
                suggestions: vec![],
            });
        }

        // Push to resolution stack for cycle detection during recursive loads
        self.resolution_stack.push(canonical.clone());

        let idx = self.modules.len();
        self.modules.push((canonical.clone(), source, tree));
        self.loaded.insert(canonical.clone(), idx);

        // Pop from resolution stack
        self.resolution_stack.retain(|p| p != &canonical);

        Ok(idx)
    }

    /// Get the root node and source for a loaded module.
    pub fn get_module(&self, idx: usize) -> Option<(tree_sitter::Node<'_>, &str, &Path)> {
        self.modules
            .get(idx)
            .map(|(path, source, tree)| (tree.root_node(), source.as_str(), path.as_path()))
    }

    /// Extract exported symbols from a loaded module's root node.
    pub fn extract_exports(root: &tree_sitter::Node, source: &str) -> ModuleExports {
        let mut functions = Vec::new();
        let mut types = Vec::new();
        let mut cursor = root.walk();

        for child in root.children(&mut cursor) {
            match child.kind() {
                "function_def" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        let name = name_node
                            .utf8_text(source.as_bytes())
                            .unwrap_or("")
                            .to_string();
                        // Build signature string from params and return type
                        let sig = {
                            let mut parts = Vec::new();
                            if let Some(params) = child.child_by_field_name("params") {
                                parts.push(
                                    params
                                        .utf8_text(source.as_bytes())
                                        .unwrap_or("")
                                        .to_string(),
                                );
                            }
                            if let Some(ret) = child.child_by_field_name("return_type") {
                                parts.push(
                                    ret.utf8_text(source.as_bytes()).unwrap_or("").to_string(),
                                );
                            }
                            if parts.is_empty() {
                                None
                            } else {
                                Some(parts.join(" -> "))
                            }
                        };
                        functions.push((name, sig));
                    }
                }
                "type_def" => {
                    if let Some(name_node) = child.child_by_field_name("name") {
                        let name = name_node
                            .utf8_text(source.as_bytes())
                            .unwrap_or("")
                            .to_string();
                        types.push(name);
                    }
                }
                _ => {}
            }
        }

        ModuleExports { functions, types }
    }

    /// Push a path to the resolution stack (for external callers managing recursion).
    pub fn push_resolution(&mut self, path: &Path) {
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        self.resolution_stack.push(canonical);
    }

    /// Pop a path from the resolution stack.
    pub fn pop_resolution(&mut self, path: &Path) {
        let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        self.resolution_stack.retain(|p| p != &canonical);
    }
}

fn node_location(file: &str, node: &tree_sitter::Node) -> Location {
    let start = node.start_position();
    let end = node.end_position();
    Location {
        file: file.to_string(),
        line: start.row + 1,
        col: start.column + 1,
        end_line: Some(end.row + 1),
        end_col: Some(end.column + 1),
    }
}

fn has_syntax_errors(node: &tree_sitter::Node) -> bool {
    if node.is_error() || node.is_missing() {
        return true;
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if has_syntax_errors(&child) {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_resolve_path_exact_case() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("Math.bl"), "").unwrap();

        let loader = ModuleLoader::with_base_dir(dir.path().to_path_buf());

        // Create a dummy node for error reporting
        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let tree = parser.parse("import Math", None).unwrap();
        let root = tree.root_node();
        let import_node = root.named_child(0).unwrap();

        let result = loader.resolve_path("Math", &import_node, "test.bl");
        assert!(result.is_ok());
        assert_eq!(result.unwrap().file_name().unwrap(), "Math.bl");
    }

    #[test]
    fn test_resolve_path_lowercase() {
        let dir = tempfile::tempdir().unwrap();
        fs::write(dir.path().join("math.bl"), "").unwrap();

        let loader = ModuleLoader::with_base_dir(dir.path().to_path_buf());

        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let tree = parser.parse("import Math", None).unwrap();
        let root = tree.root_node();
        let import_node = root.named_child(0).unwrap();

        let result = loader.resolve_path("Math", &import_node, "test.bl");
        assert!(result.is_ok());
        // On case-insensitive filesystems (macOS), Math.bl matches math.bl
        let resolved = result.unwrap();
        assert!(resolved.exists());
    }

    #[test]
    fn test_resolve_path_not_found() {
        let dir = tempfile::tempdir().unwrap();

        let loader = ModuleLoader::with_base_dir(dir.path().to_path_buf());

        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let tree = parser.parse("import Nonexistent", None).unwrap();
        let root = tree.root_node();
        let import_node = root.named_child(0).unwrap();

        let result = loader.resolve_path("Nonexistent", &import_node, "test.bl");
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, "IMP_001");
    }

    #[test]
    fn test_resolve_dotted_path() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("api")).unwrap();
        fs::write(dir.path().join("api").join("users.bl"), "").unwrap();

        let loader = ModuleLoader::with_base_dir(dir.path().to_path_buf());

        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let tree = parser.parse("import Api.Users", None).unwrap();
        let root = tree.root_node();
        let import_node = root.named_child(0).unwrap();

        let result = loader.resolve_path("Api.Users", &import_node, "test.bl");
        assert!(result.is_ok());
    }

    #[test]
    fn test_circular_import_detection() {
        let dir = tempfile::tempdir().unwrap();
        let file_a = dir.path().join("a.bl");
        fs::write(&file_a, "import B").unwrap();

        let mut loader = ModuleLoader::with_base_dir(dir.path().to_path_buf());
        // Simulate that a.bl is being resolved
        loader.push_resolution(&file_a);

        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let tree = parser.parse("import A", None).unwrap();
        let root = tree.root_node();
        let import_node = root.named_child(0).unwrap();

        let result = loader.load_module(&file_a, &import_node, "b.bl");
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().code, "IMP_002");
    }

    #[test]
    fn test_parse_qualified_import() {
        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let tree = parser.parse("import Math", None).unwrap();
        let root = tree.root_node();

        let imports = ModuleLoader::parse_imports(&root, "import Math");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].0.module_name, "Math");
        assert!(matches!(imports[0].0.kind, ImportKind::Qualified));
    }

    #[test]
    fn test_parse_selective_import() {
        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let source = "import Math.{add, multiply}";
        let tree = parser.parse(source, None).unwrap();
        let root = tree.root_node();

        let imports = ModuleLoader::parse_imports(&root, source);
        assert_eq!(
            imports.len(),
            1,
            "Expected 1 import, got {}: tree={}",
            imports.len(),
            root.to_sexp()
        );
        assert_eq!(imports[0].0.module_name, "Math");
        match &imports[0].0.kind {
            ImportKind::Selective(names) => {
                assert_eq!(names, &["add", "multiply"]);
            }
            other => panic!(
                "Expected Selective, got {:?}; tree={}",
                other,
                root.to_sexp()
            ),
        }
    }

    #[test]
    fn test_parse_wildcard_import() {
        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let source = "import Math.*";
        let tree = parser.parse(source, None).unwrap();
        let root = tree.root_node();

        let imports = ModuleLoader::parse_imports(&root, source);
        assert_eq!(imports.len(), 1, "Tree: {}", root.to_sexp());
        assert_eq!(imports[0].0.module_name, "Math");
        assert!(matches!(imports[0].0.kind, ImportKind::Wildcard));
    }

    #[test]
    fn test_extract_exports() {
        let mut parser = Parser::new();
        parser.set_language(&LANGUAGE.into()).unwrap();
        let source = r#"
fn add(a: Int, b: Int) -> Int = a + b

fn multiply(a: Int, b: Int) -> Int = a * b
"#;
        let tree = parser.parse(source, None).unwrap();
        let root = tree.root_node();

        let exports = ModuleLoader::extract_exports(&root, source);
        let func_names: Vec<&str> = exports.functions.iter().map(|(n, _)| n.as_str()).collect();
        assert!(func_names.contains(&"add"));
        assert!(func_names.contains(&"multiply"));
    }
}
