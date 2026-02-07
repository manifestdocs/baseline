#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Only test valid UTF-8
    let source = match std::str::from_utf8(data) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Parse + analyze — must never panic (errors/diagnostics are fine)
    let _ = blc::parse::parse_source(source, "<fuzz>");
});
