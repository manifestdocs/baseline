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
            .next_back()
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;
    use tree_sitter::Parser;
    use tree_sitter_baseline::LANGUAGE;

    use super::super::vm::Vm;

    fn parse(source: &str) -> (tree_sitter::Tree, String) {
        let mut parser = Parser::new();
        parser
            .set_language(&LANGUAGE.into())
            .expect("Failed to load grammar");
        let tree = parser.parse(source, None).expect("Failed to parse");
        (tree, source.to_string())
    }

    fn setup_module_dir(files: &[(&str, &str)]) -> TempDir {
        let dir = TempDir::new().unwrap();
        for (name, content) in files {
            fs::write(dir.path().join(name), content).unwrap();
        }
        dir
    }

    #[test]
    fn qualified_import_compiles_and_runs() {
        let dir = setup_module_dir(&[
            ("util.bl", "@prelude(core)\nadd : (Int, Int) -> Int\nadd = |a, b| a + b\n"),
            ("main.bl", "@prelude(core)\nimport Util\nmain : () -> Int\nmain = { Util.add(3, 7) }\n"),
        ]);

        let main_path = dir.path().join("main.bl");
        let source = fs::read_to_string(&main_path).unwrap();
        let (tree, _) = parse(&source);
        let natives = NativeRegistry::new();

        let program = compile_with_imports(&source, &tree.root_node(), &main_path, &natives)
            .expect("compile_with_imports failed");

        let mut vm = Vm::new();
        let result = vm.execute_program(&program).expect("VM execution failed");
        assert_eq!(result, super::super::value::Value::Int(10));
    }

    #[test]
    fn selective_import_compiles_and_runs() {
        let dir = setup_module_dir(&[
            ("util.bl", "@prelude(core)\nadd : (Int, Int) -> Int\nadd = |a, b| a + b\ndouble : Int -> Int\ndouble = |x| x * 2\n"),
            ("main.bl", "@prelude(core)\nimport Util.{add, double}\nmain : () -> Int\nmain = { add(double(5), 1) }\n"),
        ]);

        let main_path = dir.path().join("main.bl");
        let source = fs::read_to_string(&main_path).unwrap();
        let (tree, _) = parse(&source);
        let natives = NativeRegistry::new();

        let program = compile_with_imports(&source, &tree.root_node(), &main_path, &natives)
            .expect("compile_with_imports failed");

        let mut vm = Vm::new();
        let result = vm.execute_program(&program).expect("VM execution failed");
        assert_eq!(result, super::super::value::Value::Int(11));
    }

    #[test]
    fn wildcard_import_compiles_and_runs() {
        let dir = setup_module_dir(&[
            ("util.bl", "@prelude(core)\nadd : (Int, Int) -> Int\nadd = |a, b| a + b\ndouble : Int -> Int\ndouble = |x| x * 2\n"),
            ("main.bl", "@prelude(core)\nimport Util.*\nmain : () -> Int\nmain = { add(double(5), 1) }\n"),
        ]);

        let main_path = dir.path().join("main.bl");
        let source = fs::read_to_string(&main_path).unwrap();
        let (tree, _) = parse(&source);
        let natives = NativeRegistry::new();

        let program = compile_with_imports(&source, &tree.root_node(), &main_path, &natives)
            .expect("compile_with_imports failed");

        let mut vm = Vm::new();
        let result = vm.execute_program(&program).expect("VM execution failed");
        assert_eq!(result, super::super::value::Value::Int(11));
    }

    #[test]
    fn qualified_import_also_works_via_field_expression() {
        let dir = setup_module_dir(&[
            ("util.bl", "@prelude(core)\ndouble : Int -> Int\ndouble = |x| x * 2\n"),
            ("main.bl", "@prelude(core)\nimport Util\nmain : () -> Int\nmain = { Util.double(21) }\n"),
        ]);

        let main_path = dir.path().join("main.bl");
        let source = fs::read_to_string(&main_path).unwrap();
        let (tree, _) = parse(&source);
        let natives = NativeRegistry::new();

        let program = compile_with_imports(&source, &tree.root_node(), &main_path, &natives)
            .expect("compile_with_imports failed");

        let mut vm = Vm::new();
        let result = vm.execute_program(&program).expect("VM execution failed");
        assert_eq!(result, super::super::value::Value::Int(42));
    }

    #[test]
    fn missing_module_returns_error() {
        let dir = setup_module_dir(&[
            ("main.bl", "@prelude(core)\nimport Missing\nmain : () -> Int\nmain = { Missing.foo(1) }\n"),
        ]);

        let main_path = dir.path().join("main.bl");
        let source = fs::read_to_string(&main_path).unwrap();
        let (tree, _) = parse(&source);
        let natives = NativeRegistry::new();

        let err = compile_with_imports(&source, &tree.root_node(), &main_path, &natives)
            .expect_err("Should fail for missing module");
        assert!(
            err.message.contains("Missing") || err.message.contains("missing") || err.message.contains("not found"),
            "Error should mention the missing module, got: {}",
            err.message
        );
    }

    #[test]
    fn multiple_modules_imported() {
        let dir = setup_module_dir(&[
            ("math.bl", "@prelude(core)\nadd : (Int, Int) -> Int\nadd = |a, b| a + b\n"),
            ("strings.bl", "@prelude(core)\nlen : String -> Int\nlen = |s| 5\n"),
            ("main.bl", "@prelude(core)\nimport Math\nimport Strings\nmain : () -> Int\nmain = { Math.add(Strings.len(\"hello\"), 1) }\n"),
        ]);

        let main_path = dir.path().join("main.bl");
        let source = fs::read_to_string(&main_path).unwrap();
        let (tree, _) = parse(&source);
        let natives = NativeRegistry::new();

        let program = compile_with_imports(&source, &tree.root_node(), &main_path, &natives)
            .expect("compile_with_imports failed");

        let mut vm = Vm::new();
        let result = vm.execute_program(&program).expect("VM execution failed");
        assert_eq!(result, super::super::value::Value::Int(6));
    }

    #[test]
    fn chunk_offsets_correct_with_multiple_functions() {
        let dir = setup_module_dir(&[
            ("util.bl", "@prelude(core)\nadd : (Int, Int) -> Int\nadd = |a, b| a + b\nsub : (Int, Int) -> Int\nsub = |a, b| a - b\n"),
            ("main.bl", "@prelude(core)\nimport Util\nmain : () -> Int\nmain = { Util.sub(Util.add(10, 5), 3) }\n"),
        ]);

        let main_path = dir.path().join("main.bl");
        let source = fs::read_to_string(&main_path).unwrap();
        let (tree, _) = parse(&source);
        let natives = NativeRegistry::new();

        let program = compile_with_imports(&source, &tree.root_node(), &main_path, &natives)
            .expect("compile_with_imports failed");

        // Module has 2 functions, main has 1 → at least 3 chunks
        assert!(program.chunks.len() >= 3, "Expected >= 3 chunks, got {}", program.chunks.len());

        let mut vm = Vm::new();
        let result = vm.execute_program(&program).expect("VM execution failed");
        assert_eq!(result, super::super::value::Value::Int(12));
    }
}
