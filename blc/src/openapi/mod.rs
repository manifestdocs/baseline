mod extract;
pub mod model;
mod schema;
mod serialize;

use std::collections::HashMap;
use std::path::Path;

use tree_sitter::{Parser, Tree};

use crate::analysis::refinements::{self, Constraint};
use crate::analysis::Type;
use crate::resolver::ModuleLoader;

pub use extract::extract_api_model;
pub use model::ApiModel;
pub use serialize::to_openapi_json;

/// Extract an ApiModel from a Baseline source file.
/// Runs parse + type check + refinement collection, then extracts routes and schemas.
/// Follows imports to find routes in sub-modules (e.g., Router.group with imported handlers).
pub fn extract_from_file(path: &Path) -> Result<ApiModel, String> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;

    let file_name = path.display().to_string();

    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_baseline::LANGUAGE.into())
        .map_err(|e| format!("Failed to load grammar: {}", e))?;

    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| "Failed to parse source".to_string())?;

    // Run type checking to get type_map and type_defs
    let base_dir = path.parent();
    let mut loader = match base_dir {
        Some(dir) => ModuleLoader::with_base_dir(dir.to_path_buf()),
        None => ModuleLoader::new(),
    };

    // Register dependency paths from baseline.toml
    if let Some(dir) = base_dir {
        if let Some((m, manifest_dir)) = crate::manifest::load_manifest(dir) {
            for (name, dep) in &m.dependencies {
                let dep_dir = match dep {
                    crate::manifest::Dependency::Url { hash, .. } => {
                        manifest_dir.join(".baseline").join("deps").join(hash)
                    }
                    crate::manifest::Dependency::Path { path } => manifest_dir.join(path),
                };
                loader.add_dep_path(name.clone(), dep_dir);
            }
        }
    }

    let root = tree.root_node();
    let (_diags, type_map, type_defs, _dict_map) =
        crate::analysis::check_types_with_loader_map_and_defs(
            &root,
            &source,
            &file_name,
            Some(&mut loader),
        );

    // Collect refinement constraints
    let refined_types = refinements::collect_refined_types(&tree, &source);

    let mut all_type_defs = type_defs;
    let mut all_refined_types = refined_types;

    // Collect types and routes from imported modules
    let mut imported_sources: Vec<(String, String)> = Vec::new(); // (source, file_name)
    collect_imported_data(
        &tree,
        &source,
        base_dir,
        &mut all_type_defs,
        &mut all_refined_types,
        &mut imported_sources,
    );

    // Extract from main module
    let mut model = extract_api_model(
        &tree,
        &source,
        &type_map,
        &all_type_defs,
        &all_refined_types,
    );

    // Extract group mappings from main module (Router.group calls with function refs)
    let group_mappings = extract::extract_group_mappings(tree.root_node(), &source);

    // For each imported module, parse and extract additional routes
    for (mod_source, _mod_file) in &imported_sources {
        let mut mod_parser = Parser::new();
        if mod_parser
            .set_language(&tree_sitter_baseline::LANGUAGE.into())
            .is_err()
        {
            continue;
        }
        let mod_tree = match mod_parser.parse(mod_source.as_str(), None) {
            Some(t) => t,
            None => continue,
        };

        // Find functions in this module that are referenced in group mappings
        let mod_functions = extract::collect_module_functions(mod_tree.root_node(), mod_source);

        for (func_name, prefix) in &group_mappings {
            if let Some(func_body) = mod_functions.get(func_name.as_str()) {
                // Extract routes from this function's body with the group prefix
                let sub_routes =
                    extract::extract_routes_from_node(*func_body, mod_source, prefix);

                // For each sub-route, analyze handler from the module functions
                for raw in sub_routes {
                    let handler_info = mod_functions
                        .get(raw.handler_name.as_str())
                        .map(|body_node| extract::analyze_handler_node(*body_node, mod_source))
                        .unwrap_or_default();

                    let mut responses = handler_info.responses;
                    if responses.is_empty() {
                        responses.push(model::ApiResponse {
                            status: 200,
                            description: "OK".to_string(),
                            schema: None,
                        });
                    }
                    responses.extend(handler_info.error_responses);

                    if handler_info.request_body.is_some()
                        && !responses.iter().any(|r| r.status == 422)
                    {
                        responses.push(model::ApiResponse {
                            status: 422,
                            description: "Unprocessable Entity".to_string(),
                            schema: None,
                        });
                    }

                    let mut parameters = extract::extract_path_params_from(&raw.path);
                    parameters.extend(handler_info.query_params);

                    let summary = extract::extract_handler_comment(
                        mod_tree.root_node(),
                        mod_source,
                        &raw.handler_name,
                    );

                    model.routes.push(model::ApiRoute {
                        method: raw.method,
                        path: raw.path,
                        operation_id: raw.handler_name,
                        summary,
                        request_body: handler_info.request_body,
                        parameters,
                        responses,
                    });
                }
            }
        }
    }

    Ok(model)
}

/// Collect type definitions, refinements, and source code from imported modules.
fn collect_imported_data(
    tree: &Tree,
    source: &str,
    base_dir: Option<&Path>,
    type_defs: &mut HashMap<String, Type>,
    refined_types: &mut HashMap<String, Constraint>,
    imported_sources: &mut Vec<(String, String)>,
) {
    let root = tree.root_node();
    let mut cursor = root.walk();

    for child in root.children(&mut cursor) {
        if child.kind() == "import_decl" {
            if let Some(module_name) = extract_import_module_name(child, source) {
                if let Some(dir) = base_dir {
                    let module_path = dir.join(format!("{}.bl", module_name));
                    if module_path.exists() {
                        if let Ok(module_source) = std::fs::read_to_string(&module_path) {
                            let mut parser = Parser::new();
                            if parser
                                .set_language(&tree_sitter_baseline::LANGUAGE.into())
                                .is_ok()
                            {
                                if let Some(module_tree) = parser.parse(&module_source, None) {
                                    let module_root = module_tree.root_node();
                                    let file_name = module_path.display().to_string();

                                    let (_d, _tm, module_type_defs, _dm) =
                                        crate::analysis::check_types_with_loader_map_and_defs(
                                            &module_root,
                                            &module_source,
                                            &file_name,
                                            None,
                                        );

                                    let module_refined =
                                        refinements::collect_refined_types(&module_tree, &module_source);

                                    for (k, v) in module_type_defs {
                                        type_defs.entry(k).or_insert(v);
                                    }
                                    for (k, v) in module_refined {
                                        refined_types.entry(k).or_insert(v);
                                    }

                                    imported_sources.push((module_source, file_name));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn extract_import_module_name(node: tree_sitter::Node, source: &str) -> Option<String> {
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        let kind = child.kind();
        // Direct module name: import ModuleName
        if kind == "type_identifier" || kind == "identifier" {
            return child.utf8_text(source.as_bytes()).ok().map(String::from);
        }
        // Selective import: import Handlers.{health, todo_routes}
        // Various CST representations for the module name
        if kind == "import_path"
            || kind == "scoped_identifier"
            || kind == "field_expression"
            || kind == "scoped_import"
        {
            // Walk recursively to find the first type_identifier or identifier
            return find_first_identifier(child, source);
        }
    }
    None
}

fn find_first_identifier(node: tree_sitter::Node, source: &str) -> Option<String> {
    if node.kind() == "type_identifier" || node.kind() == "identifier" {
        return node.utf8_text(source.as_bytes()).ok().map(String::from);
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(name) = find_first_identifier(child, source) {
            return Some(name);
        }
    }
    None
}
