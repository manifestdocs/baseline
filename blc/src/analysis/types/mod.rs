mod builtins;
mod check_lambda;
mod check_node;
mod check_pattern;
mod symbol_table;
mod type_compat;
mod type_def;
mod type_parse;

pub use type_def::{Type, TypeMap};
pub use symbol_table::SymbolTable;

use check_node::check_node;
use type_compat::{detect_implicit_type_params, extract_explicit_type_params};
use type_parse::parse_type_ext;

use super::infer::UserGenericSchema;
use crate::diagnostics::{Diagnostic, Location, Severity};
use crate::prelude::{self, Prelude};
use crate::resolver::{ImportKind, ModuleLoader};
use std::collections::HashSet;
use tree_sitter::Node;

pub fn check_types(root: &Node, source: &str, file: &str) -> Vec<Diagnostic> {
    check_types_with_loader(root, source, file, None)
}

pub fn check_types_with_loader(
    root: &Node,
    source: &str,
    file: &str,
    loader: Option<&mut ModuleLoader>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Extract prelude from the AST to gate module availability.
    let prelude = match prelude::extract_prelude(root, source) {
        Ok(p) => p,
        Err(msg) => {
            diagnostics.push(Diagnostic {
                code: "PRE_001".to_string(),
                severity: Severity::Error,
                location: Location {
                    file: file.to_string(),
                    line: 1,
                    col: 1,
                    end_line: None,
                    end_col: None,
                },
                message: msg,
                context: "Valid prelude variants are: core, script.".to_string(),
                suggestions: vec![],
            });
            return diagnostics;
        }
    };

    let mut symbols = SymbolTable::with_prelude(prelude);

    // Process imports before collecting signatures
    if let Some(loader) = loader {
        process_imports(root, source, file, loader, &mut symbols, &mut diagnostics);
    }

    collect_signatures(root, source, &mut symbols);
    check_node(root, source, file, &mut symbols, &mut diagnostics);
    diagnostics
}

/// Run the type checker and return both diagnostics and a TypeMap.
/// The TypeMap maps CST node start_byte to resolved Type, enabling
/// the VM compiler to emit specialized opcodes.
pub fn check_types_with_map(root: &Node, source: &str, file: &str) -> (Vec<Diagnostic>, TypeMap) {
    let mut diagnostics = Vec::new();

    let prelude = match prelude::extract_prelude(root, source) {
        Ok(p) => p,
        Err(msg) => {
            diagnostics.push(Diagnostic {
                code: "PRE_001".to_string(),
                severity: Severity::Error,
                location: Location {
                    file: file.to_string(),
                    line: 1,
                    col: 1,
                    end_line: None,
                    end_col: None,
                },
                message: msg,
                context: "Valid prelude variants are: core, script.".to_string(),
                suggestions: vec![],
            });
            return (diagnostics, TypeMap::new());
        }
    };

    let mut symbols = SymbolTable::with_prelude(prelude);
    collect_signatures(root, source, &mut symbols);
    check_node(root, source, file, &mut symbols, &mut diagnostics);
    let type_map = symbols.type_map;
    (diagnostics, type_map)
}

/// Run the type checker with import support and return both diagnostics and a TypeMap.
/// Combines `check_types_with_loader` (import handling) with `check_types_with_map` (TypeMap).
pub fn check_types_with_loader_and_map(
    root: &Node,
    source: &str,
    file: &str,
    loader: Option<&mut ModuleLoader>,
) -> (Vec<Diagnostic>, TypeMap) {
    let mut diagnostics = Vec::new();

    let prelude = match prelude::extract_prelude(root, source) {
        Ok(p) => p,
        Err(msg) => {
            diagnostics.push(Diagnostic {
                code: "PRE_001".to_string(),
                severity: Severity::Error,
                location: Location {
                    file: file.to_string(),
                    line: 1,
                    col: 1,
                    end_line: None,
                    end_col: None,
                },
                message: msg,
                context: "Valid prelude variants are: core, script.".to_string(),
                suggestions: vec![],
            });
            return (diagnostics, TypeMap::new());
        }
    };

    let mut symbols = SymbolTable::with_prelude(prelude);

    if let Some(loader) = loader {
        process_imports(root, source, file, loader, &mut symbols, &mut diagnostics);
    }

    collect_signatures(root, source, &mut symbols);
    check_node(root, source, file, &mut symbols, &mut diagnostics);
    let type_map = symbols.type_map;
    (diagnostics, type_map)
}

/// CGP type-check result: diagnostics, type map, visible bindings, and module methods.
pub struct CgpTypeInfo {
    pub diagnostics: Vec<Diagnostic>,
    pub type_map: TypeMap,
    pub bindings: Vec<(String, Type)>,
    pub module_methods: Vec<(String, Type)>,
}

/// Run the type checker and return everything the CGP needs:
/// diagnostics, type map, visible bindings, and module methods.
/// Designed to work on partial/incomplete code (will produce Unknown for error nodes).
pub fn check_types_for_cgp(root: &Node, source: &str, file: &str) -> CgpTypeInfo {
    let mut diagnostics = Vec::new();

    let prelude = match prelude::extract_prelude(root, source) {
        Ok(p) => p,
        Err(_) => Prelude::Core, // default to Core for partial code
    };

    let mut symbols = SymbolTable::with_prelude(prelude);
    collect_signatures(root, source, &mut symbols);
    check_node(root, source, file, &mut symbols, &mut diagnostics);

    let bindings = symbols.visible_bindings();
    let module_methods: Vec<(String, Type)> = symbols
        .module_methods
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    let type_map = symbols.type_map;

    CgpTypeInfo {
        diagnostics,
        type_map,
        bindings,
        module_methods,
    }
}

/// Process import declarations: resolve each import, type-check the imported module,
/// and register its exports into the current symbol table.
fn process_imports(
    root: &Node,
    source: &str,
    file: &str,
    loader: &mut ModuleLoader,
    symbols: &mut SymbolTable,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let imports = ModuleLoader::parse_imports(root, source);

    for (import, import_node) in imports {
        // Resolve module path to a file
        let file_path = match loader.resolve_path(&import.module_name, &import_node, file) {
            Ok(p) => p,
            Err(diag) => {
                diagnostics.push(diag);
                continue;
            }
        };

        // Load and parse the module
        let module_idx = match loader.load_module(&file_path, &import_node, file) {
            Ok(idx) => idx,
            Err(diag) => {
                diagnostics.push(diag);
                continue;
            }
        };

        // Get the module file path (owned) so we can release the borrow
        let mod_file = match loader.get_module(module_idx) {
            Some((_, _, path)) => path.display().to_string(),
            None => continue,
        };

        // Type-check the imported module with recursive import support.
        // Clone source/path to release the borrow on loader, so we can pass &mut loader.
        {
            let (mod_root, mod_source, _) = loader.get_module(module_idx).unwrap();
            let mod_source_owned = mod_source.to_string();
            let mod_file_owned = mod_file.clone();
            let _ = mod_root;
            // Re-parse to get an owned tree (required because mod_root borrows loader)
            let mut parser = tree_sitter::Parser::new();
            parser
                .set_language(&tree_sitter_baseline::LANGUAGE.into())
                .expect("Failed to load Baseline grammar");
            let tree = parser.parse(&mod_source_owned, None).unwrap();
            let mod_root = tree.root_node();
            let mod_diagnostics = check_types_with_loader(
                &mod_root,
                &mod_source_owned,
                &mod_file_owned,
                Some(loader),
            );
            // Pop the resolution stack now that this module's transitive imports are resolved
            loader.pop_resolution(&file_path);
            let has_errors = mod_diagnostics
                .iter()
                .any(|d| d.severity == Severity::Error);
            if has_errors {
                diagnostics.push(Diagnostic {
                    code: "IMP_003".to_string(),
                    severity: Severity::Error,
                    location: Location::from_node(file,&import_node),
                    message: format!(
                        "Imported module `{}` has {} type error(s)",
                        import.module_name,
                        mod_diagnostics
                            .iter()
                            .filter(|d| d.severity == Severity::Error)
                            .count()
                    ),
                    context: "Fix the errors in the imported module first.".to_string(),
                    suggestions: vec![],
                });
                continue;
            }
        }

        // Re-borrow to extract exports and build module's symbol table
        let (mod_root, mod_source, _) = loader.get_module(module_idx).unwrap();

        let exports = ModuleLoader::extract_exports(&mod_root, mod_source);

        // Get the short module name (last segment for dotted paths)
        let short_name = import
            .module_name
            .split('.')
            .next_back()
            .unwrap_or(&import.module_name);

        // Build a temporary SymbolTable for the imported module to parse types
        let mod_prelude = prelude::extract_prelude(&mod_root, mod_source).unwrap_or(Prelude::Core);
        let mut mod_symbols = SymbolTable::with_prelude(mod_prelude);
        collect_signatures(&mod_root, mod_source, &mut mod_symbols);

        // Register the module namespace
        symbols.insert(short_name.to_string(), Type::Module(short_name.to_string()));
        symbols.user_modules.insert(short_name.to_string());

        // Register each exported function as a module method
        for (func_name, _sig_text) in &exports.functions {
            let qualified = format!("{}.{}", short_name, func_name);
            // Look up the function type from the module's symbol table
            let func_type = mod_symbols
                .lookup(func_name)
                .cloned()
                .unwrap_or(Type::Unknown);
            symbols.module_methods.insert(qualified, func_type.clone());

            // For selective/wildcard imports, also put the symbol directly in scope
            match &import.kind {
                ImportKind::Selective(names) => {
                    if names.contains(func_name) {
                        symbols.insert(func_name.clone(), func_type);
                    }
                }
                ImportKind::Wildcard => {
                    symbols.insert(func_name.clone(), func_type);
                }
                ImportKind::Qualified => {}
            }
        }

        // Register exported types
        for type_name in &exports.types {
            let type_def = mod_symbols.lookup_type(type_name).cloned();
            if let Some(ty) = type_def {
                // For selective/wildcard, register type directly
                match &import.kind {
                    ImportKind::Selective(names) => {
                        if names.contains(type_name) {
                            symbols.insert_type(type_name.clone(), ty);
                        }
                    }
                    ImportKind::Wildcard => {
                        symbols.insert_type(type_name.clone(), ty);
                    }
                    ImportKind::Qualified => {}
                }
            }
        }

        // Validate selective imports: check that requested symbols exist
        if let ImportKind::Selective(names) = &import.kind {
            let exported_names: Vec<&str> = exports
                .functions
                .iter()
                .map(|(n, _)| n.as_str())
                .chain(exports.types.iter().map(|n| n.as_str()))
                .collect();
            for name in names {
                if !exported_names.contains(&name.as_str()) {
                    diagnostics.push(Diagnostic {
                        code: "IMP_004".to_string(),
                        severity: Severity::Error,
                        location: Location::from_node(file,&import_node),
                        message: format!(
                            "Symbol `{}` not found in module `{}`",
                            name, import.module_name
                        ),
                        context: format!("Available exports: {}", exported_names.join(", ")),
                        suggestions: vec![],
                    });
                }
            }
        }
    }
}

/// Pre-scan top-level function signatures so forward references resolve.
/// Also registers UserGenericSchema for functions with type parameters.
fn collect_signatures(node: &Node, source: &str, symbols: &mut SymbolTable) {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        match child.kind() {
            "function_def" => {
                if let Some(name_node) = child.child_by_field_name("name") {
                    let name = name_node.utf8_text(source.as_bytes()).unwrap().to_string();

                    // Detect type parameters: explicit <T, U> or implicit single-letter
                    let mut type_param_names = extract_explicit_type_params(&child, source);
                    if type_param_names.is_empty() {
                        type_param_names = detect_implicit_type_params(&child, source, symbols);
                    }

                    let type_param_set: HashSet<String> =
                        type_param_names.iter().cloned().collect();
                    let ty =
                        parse_function_type_with_params(&child, source, symbols, &type_param_set);
                    symbols.insert(name.clone(), ty.clone());

                    // Register user generic schema if the function has type parameters
                    if !type_param_names.is_empty() {
                        symbols.user_generic_schemas.insert(
                            name,
                            UserGenericSchema {
                                type_param_names,
                                fn_type: ty,
                            },
                        );
                    }
                }
            }
            "module_decl" => collect_signatures(&child, source, symbols),
            _ => {}
        }
    }
}

/// Build a Type::Function from a function_def, treating names in `type_params` as TypeParam.
fn parse_function_type_with_params(
    func_node: &Node,
    source: &str,
    symbols: &SymbolTable,
    type_params: &HashSet<String>,
) -> Type {
    let mut arg_types = Vec::new();
    if let Some(params) = func_node.child_by_field_name("params") {
        let mut cursor = params.walk();
        for param in params.named_children(&mut cursor) {
            if param.kind() == "param"
                && let Some(type_node) = param.child_by_field_name("type")
            {
                arg_types.push(parse_type_ext(&type_node, source, symbols, type_params));
            }
        }
    }
    let ret_type = if let Some(ret_node) = func_node.child_by_field_name("return_type") {
        parse_type_ext(&ret_node, source, symbols, type_params)
    } else {
        Type::Unit
    };
    Type::Function(arg_types, Box::new(ret_type))
}
