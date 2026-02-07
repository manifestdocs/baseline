use std::collections::HashMap;
use std::path::Path;

use crate::resolver::{ImportKind, ModuleLoader};

use super::chunk::{Chunk, Program};
use super::compiler::{CompileError, Compiler};
use super::natives::NativeRegistry;

/// Compile a program that has imports by resolving, compiling, and merging
/// imported modules before compiling the main file.
pub fn compile_with_imports(
    main_source: &str,
    main_root: &tree_sitter::Node,
    file_path: &Path,
    natives: &NativeRegistry,
) -> Result<Program, CompileError> {
    let imports = ModuleLoader::parse_imports(main_root, main_source);
    let file_str = file_path.display().to_string();

    let base_dir = file_path.parent().ok_or_else(|| CompileError {
        message: "Cannot determine base directory for imports".into(),
        line: 1,
        col: 0,
    })?;

    let mut loader = ModuleLoader::with_base_dir(base_dir.to_path_buf());

    // Phase 1: resolve and load all modules
    let mut resolved = Vec::new();
    for (import, import_node) in &imports {
        let path = loader
            .resolve_path(&import.module_name, import_node, &file_str)
            .map_err(|d| CompileError {
                message: d.message,
                line: import_node.start_position().row + 1,
                col: import_node.start_position().column,
            })?;
        let module_idx = loader
            .load_module(&path, import_node, &file_str)
            .map_err(|d| CompileError {
                message: d.message,
                line: import_node.start_position().row + 1,
                col: import_node.start_position().column,
            })?;
        resolved.push((import.clone(), module_idx));
    }

    // Phase 2: compile each module and merge chunks
    let mut merged_chunks: Vec<Chunk> = Vec::new();
    let mut merged_functions: HashMap<String, usize> = HashMap::new();

    for (import, module_idx) in &resolved {
        let (mod_root, mod_source, _mod_path) = loader
            .get_module(*module_idx)
            .ok_or_else(|| CompileError {
                message: format!("Failed to get module {}", import.module_name),
                line: 1,
                col: 0,
            })?;

        let compiler = Compiler::new(mod_source, natives);
        let (mut mod_chunks, local_map) = compiler.compile_module(&mod_root)?;

        let offset = merged_chunks.len();

        // Rewrite internal references in each chunk
        for chunk in &mut mod_chunks {
            chunk.offset_chunk_refs(offset);
        }

        // Extract the short module name (last component of dotted path)
        let short_name = import
            .module_name
            .split('.')
            .last()
            .unwrap_or(&import.module_name);

        // Build merged function map based on import kind
        match &import.kind {
            ImportKind::Qualified => {
                for (name, &local_idx) in &local_map {
                    let qualified = format!("{}.{}", short_name, name);
                    merged_functions.insert(qualified, offset + local_idx);
                }
            }
            ImportKind::Selective(names) => {
                // Also add qualified names for field_expression resolution
                for (name, &local_idx) in &local_map {
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
                // Also add qualified names for field_expression resolution
                for (name, &local_idx) in &local_map {
                    let qualified = format!("{}.{}", short_name, name);
                    merged_functions.insert(qualified, offset + local_idx);
                }
                for (name, &local_idx) in &local_map {
                    merged_functions.insert(name.clone(), offset + local_idx);
                }
            }
        }

        merged_chunks.extend(mod_chunks);
    }

    // Phase 3: compile the main program with pre-compiled imports
    let mut compiler = Compiler::new(main_source, natives);
    compiler.set_pre_compiled(merged_chunks, merged_functions);
    compiler.compile_program(main_root)
}
