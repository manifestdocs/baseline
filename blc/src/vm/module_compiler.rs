use std::collections::HashMap;
use std::path::Path;

use crate::resolver::{ImportKind, ModuleLoader};

use super::chunk::{Chunk, Program};
use super::codegen::Codegen;
use super::compiler::CompileError;
use super::lower::Lowerer;
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

        // Use IR pipeline for module compilation
        let mut lowerer = Lowerer::new(mod_source, natives, None);
        let ir_functions = lowerer.lower_module_functions(&mod_root).map_err(|e| CompileError {
            message: e.message,
            line: e.line,
            col: e.col,
        })?;
        let codegen = Codegen::new(natives);
        let (mut mod_chunks, local_map) = codegen.generate_module(&ir_functions).map_err(|e| CompileError {
            message: e.message,
            line: e.line,
            col: e.col,
        })?;

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

    // Phase 3: compile the main program with pre-compiled imports using IR pipeline
    let mut lowerer = Lowerer::new(main_source, natives, None);
    lowerer.add_functions(merged_functions.keys().cloned());
    let ir_module = lowerer.lower_module(main_root).map_err(|e| CompileError {
        message: e.message,
        line: e.line,
        col: e.col,
    })?;
    let mut codegen = Codegen::new(natives);
    codegen.set_pre_compiled(merged_chunks, merged_functions);
    codegen.generate_program(&ir_module).map_err(|e| CompileError {
        message: e.message,
        line: e.line,
        col: e.col,
    })
}
