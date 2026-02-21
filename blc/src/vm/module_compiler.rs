use std::collections::HashMap;
use std::path::Path;

use crate::resolver::{
    ImportKind, ModuleLoader, ResolvedImport, exported_function_names, module_uses_exports,
};

use super::chunk::CompileError;
use super::chunk::{Chunk, Program};
use super::codegen::Codegen;
use super::lower::Lowerer;
use super::natives::NativeRegistry;

/// Cache entry for a compiled module: its offset in merged_chunks and name->local_index map.
struct CompiledModule {
    /// Offset of this module's first chunk in the merged chunk list
    offset: usize,
    local_map: HashMap<String, usize>,
}

/// Compile a program that has imports by recursively resolving, compiling,
/// and merging imported modules before compiling the main file.
///
/// Supports transitive imports: if module A imports B and B imports C,
/// all three are compiled and linked. Diamond dependencies (A imports B+C,
/// both import D) compile D only once.
pub fn compile_with_imports(
    main_source: &str,
    main_root: &tree_sitter::Node,
    file_path: &Path,
    natives: &NativeRegistry,
) -> Result<Program, CompileError> {
    let base_dir = file_path.parent().ok_or_else(|| CompileError {
        message: "Cannot determine base directory for imports".into(),
        line: 1,
        col: 0,
    })?;

    let mut loader = ModuleLoader::with_base_dir(base_dir.to_path_buf());

    // Cache: canonical path -> CompiledModule (avoids recompiling diamonds)
    let mut compiled_cache: HashMap<String, CompiledModule> = HashMap::new();

    // Merged chunks from all transitive imports
    let mut merged_chunks: Vec<Chunk> = Vec::new();
    let mut merged_functions: HashMap<String, usize> = HashMap::new();

    // Process the main file's direct imports (which recursively process theirs)
    let imports = ModuleLoader::parse_imports(main_root, main_source);
    let file_str = file_path.display().to_string();

    for (import, import_node) in &imports {
        let path = loader
            .resolve_path(&import.module_name, import_node, &file_str)
            .map_err(|d| CompileError {
                message: d.message,
                line: import_node.start_position().row + 1,
                col: import_node.start_position().column,
            })?;

        compile_module_recursive(
            &path,
            import_node,
            &file_str,
            natives,
            &mut loader,
            &mut compiled_cache,
            &mut merged_chunks,
            &mut merged_functions,
        )?;

        // Now register this module's exports according to the import kind
        let canonical = path.canonicalize().unwrap_or_else(|_| path.clone());
        let canonical_str = canonical.display().to_string();
        if let Some(compiled) = compiled_cache.get(&canonical_str) {
            register_exports(
                import,
                &compiled.local_map,
                compiled.offset,
                &mut merged_functions,
            );
        }
    }

    // Compile the main program with pre-compiled imports
    let mut lowerer = Lowerer::new(main_source, natives, None);
    lowerer.add_functions(merged_functions.keys().cloned());
    let ir_module = lowerer.lower_module(main_root)?;
    let mut codegen = Codegen::new(natives);
    codegen.set_pre_compiled(merged_chunks, merged_functions);
    codegen.generate_program(&ir_module)
}

/// Recursively compile a module and all its transitive imports.
/// Adds the module's chunks to `merged_chunks` and caches in `compiled_cache`.
#[allow(clippy::too_many_arguments)]
fn compile_module_recursive(
    module_path: &Path,
    import_node: &tree_sitter::Node,
    importer_file: &str,
    natives: &NativeRegistry,
    loader: &mut ModuleLoader,
    compiled_cache: &mut HashMap<String, CompiledModule>,
    merged_chunks: &mut Vec<Chunk>,
    merged_functions: &mut HashMap<String, usize>,
) -> Result<(), CompileError> {
    let canonical = module_path
        .canonicalize()
        .unwrap_or_else(|_| module_path.to_path_buf());
    let canonical_str = canonical.display().to_string();

    // Already compiled — skip
    if compiled_cache.contains_key(&canonical_str) {
        return Ok(());
    }

    // Load and parse the module (cycle detection via resolution_stack)
    let module_idx = loader
        .load_module(module_path, import_node, importer_file)
        .map_err(|d| CompileError {
            message: d.message,
            line: import_node.start_position().row + 1,
            col: import_node.start_position().column,
        })?;

    // Get the module's source and parse tree, then find its own imports.
    // We need to clone source/path to release the borrow on loader for recursion.
    let (mod_source_owned, mod_file_str) = {
        let (_, mod_source, mod_path) =
            loader.get_module(module_idx).ok_or_else(|| CompileError {
                message: format!("Failed to get module {}", module_path.display()),
                line: 1,
                col: 0,
            })?;
        (mod_source.to_string(), mod_path.display().to_string())
    };

    // Re-parse to get an owned tree (the loader's tree borrows the loader)
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_baseline::LANGUAGE.into())
        .expect("Failed to load Baseline grammar");
    let tree = parser.parse(&mod_source_owned, None).unwrap();
    let mod_root = tree.root_node();

    // Recursively compile this module's own imports first
    let sub_imports = ModuleLoader::parse_imports(&mod_root, &mod_source_owned);
    for (sub_import, sub_import_node) in &sub_imports {
        let sub_path = loader
            .resolve_path(&sub_import.module_name, sub_import_node, &mod_file_str)
            .map_err(|d| CompileError {
                message: d.message,
                line: sub_import_node.start_position().row + 1,
                col: sub_import_node.start_position().column,
            })?;

        compile_module_recursive(
            &sub_path,
            sub_import_node,
            &mod_file_str,
            natives,
            loader,
            compiled_cache,
            merged_chunks,
            merged_functions,
        )?;

        // Register the sub-module's exports for this module
        let sub_canonical = sub_path.canonicalize().unwrap_or_else(|_| sub_path.clone());
        let sub_canonical_str = sub_canonical.display().to_string();
        if let Some(compiled) = compiled_cache.get(&sub_canonical_str) {
            register_exports(
                sub_import,
                &compiled.local_map,
                compiled.offset,
                merged_functions,
            );
        }
    }

    // Pop the resolution stack now that all transitive imports are resolved
    loader.pop_resolution(module_path);

    // Now compile this module itself. It needs to know about its imported functions
    // so the lowerer can resolve calls to them.
    let mut lowerer = Lowerer::new(&mod_source_owned, natives, None);

    // Tell the lowerer about functions available from this module's imports
    let sub_function_names: Vec<String> = sub_imports
        .iter()
        .flat_map(|(_import, _node)| {
            // Collect qualified function names this module can call
            let short_name = _import
                .module_name
                .split('.')
                .next_back()
                .unwrap_or(&_import.module_name);
            merged_functions
                .keys()
                .filter(move |k| k.starts_with(&format!("{}.", short_name)))
                .cloned()
                .collect::<Vec<_>>()
        })
        .collect();
    lowerer.add_functions(sub_function_names);

    let ir_functions = lowerer.lower_module_functions(&mod_root)?;

    // Seed the codegen with already-compiled chunks so this module's functions
    // can reference imported functions (e.g., MathHelpers.square from MathOps).
    let pre_chunk_count = merged_chunks.len();
    let mut codegen = Codegen::new(natives);
    codegen.set_pre_compiled(merged_chunks.clone(), merged_functions.clone());
    let (all_chunks, all_functions) = codegen.generate_module(&ir_functions)?;

    // Extract only this module's NEW chunks (after the pre-compiled ones)
    let mod_chunks: Vec<Chunk> = all_chunks[pre_chunk_count..].to_vec();

    // Build the local_map: function_name -> local_index (relative to this module's offset)
    let mut local_map = HashMap::new();
    for (name, &global_idx) in &all_functions {
        if global_idx >= pre_chunk_count {
            local_map.insert(name.clone(), global_idx - pre_chunk_count);
        }
    }

    // Filter local_map by export visibility: if module uses `export`, only keep exported functions
    let uses_exports = module_uses_exports(&mod_root, &mod_source_owned);
    if uses_exports {
        let exported = exported_function_names(&mod_root, &mod_source_owned);
        local_map.retain(|name, _| exported.contains(name.as_str()));
    }

    let offset = merged_chunks.len();

    // Cache the compiled module with its offset
    compiled_cache.insert(canonical_str, CompiledModule { offset, local_map });

    merged_chunks.extend(mod_chunks);

    Ok(())
}

/// Register a module's exports into the merged function map according to import kind.
fn register_exports(
    import: &ResolvedImport,
    local_map: &HashMap<String, usize>,
    offset: usize,
    merged_functions: &mut HashMap<String, usize>,
) {
    let short_name = import
        .module_name
        .split('.')
        .next_back()
        .unwrap_or(&import.module_name);

    match &import.kind {
        ImportKind::Qualified => {
            for (name, &local_idx) in local_map {
                let qualified = format!("{}.{}", short_name, name);
                merged_functions.insert(qualified, offset + local_idx);
            }
        }
        ImportKind::Selective(names) => {
            for (name, &local_idx) in local_map {
                let qualified = format!("{}.{}", short_name, name);
                merged_functions.insert(qualified, offset + local_idx);
            }
            for sel_name in names {
                if let Some(&local_idx) = local_map.get(sel_name) {
                    merged_functions.insert(sel_name.clone(), offset + local_idx);
                }
            }
        }
        ImportKind::Wildcard => {
            for (name, &local_idx) in local_map {
                let qualified = format!("{}.{}", short_name, name);
                merged_functions.insert(qualified, offset + local_idx);
            }
            for (name, &local_idx) in local_map {
                merged_functions.insert(name.clone(), offset + local_idx);
            }
        }
    }
}
