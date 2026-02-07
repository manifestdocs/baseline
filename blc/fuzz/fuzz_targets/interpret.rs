#![no_main]
use libfuzzer_sys::fuzz_target;
use tree_sitter::Parser;
use tree_sitter_baseline::LANGUAGE;

fuzz_target!(|data: &[u8]| {
    // Only test valid UTF-8
    let source = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    // First, check if analysis passes without errors
    let result = blc::parse::parse_source(source, "<fuzz>");
    if result.status != "success" {
        return;
    }

    // If analysis passes, the interpreter must not panic
    let mut parser = Parser::new();
    parser
        .set_language(&LANGUAGE.into())
        .expect("Failed to load language");
    let tree = match parser.parse(source, None) {
        Some(t) => t,
        None => return,
    };
    let root = tree.root_node();

    let prelude = match blc::prelude::extract_prelude(&root, source) {
        Ok(p) => p,
        Err(_) => return,
    };

    let mut context = blc::interpreter::Context::with_prelude(prelude);
    // Interpreter errors are fine, panics are not
    let _ = blc::interpreter::eval(&root, source, &mut context);
});
