use blc::parse::parse_source;

/// Helper: check source has no diagnostics
fn check_ok(source: &str) {
    let result = parse_source(source, "<test>");
    assert!(
        result.diagnostics.is_empty(),
        "Expected no diagnostics, got: {:?}",
        result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// Helper: check source has a specific diagnostic code
fn check_has_error(source: &str, code: &str) {
    let result = parse_source(source, "<test>");
    assert!(
        result.diagnostics.iter().any(|d| d.code == code),
        "Expected diagnostic {}, got: {:?}",
        code,
        result.diagnostics.iter().map(|d| format!("{}: {}", d.code, d.message)).collect::<Vec<_>>()
    );
}

/// Helper: count diagnostics with given code prefix
fn count_errors(source: &str, prefix: &str) -> usize {
    let result = parse_source(source, "<test>");
    result.diagnostics.iter().filter(|d| d.code.starts_with(prefix)).count()
}

// ============================================================
// Type Checker Tests
// ============================================================

#[test]
fn type_check_simple_function() {
    check_ok("add : (Int, Int) -> Int\nadd = |a, b| a + b");
}

#[test]
fn type_check_string_function() {
    check_ok("greet : String -> String\ngreet = |name| name");
}

#[test]
fn type_check_bool_function() {
    check_ok("is_pos : Int -> Bool\nis_pos = |n| n > 0");
}

#[test]
fn type_check_float_literal() {
    check_ok("pi : () -> Float\npi = 3.14");
}

#[test]
fn type_check_type_alias() {
    check_ok("type UserId = Int");
}

#[test]
fn type_check_struct_definition() {
    check_ok("type User = { name: String, age: Int }");
}

#[test]
fn type_check_struct_construction() {
    check_ok(
        "type User = { name: String, age: Int }\n\
         make_user : () -> User\n\
         make_user = User { name: \"Alice\", age: 30 }"
    );
}

#[test]
fn type_check_struct_field_access() {
    check_ok(
        "type User = { name: String, age: Int }\n\
         get_name : User -> String\n\
         get_name = |u| u.name"
    );
}

#[test]
fn type_check_struct_missing_field() {
    check_has_error(
        "type User = { name: String, age: Int }\n\
         make : () -> User\n\
         make = User { name: \"Alice\" }",
        "TYP_012"
    );
}

#[test]
fn type_check_struct_wrong_field_type() {
    check_has_error(
        "type User = { name: String, age: Int }\n\
         make : () -> User\n\
         make = User { name: \"Alice\", age: \"thirty\" }",
        "TYP_010"
    );
}

#[test]
fn type_check_if_expression_branch_mismatch() {
    check_has_error(
        "foo : Bool -> Int\n\
         foo = |x| if x then 1 else \"two\"",
        "TYP_004"
    );
}

#[test]
fn type_check_if_condition_not_bool() {
    check_has_error(
        "foo : Int -> Int\n\
         foo = |x| if x then 1 else 2",
        "TYP_003"
    );
}

#[test]
fn type_check_undefined_variable() {
    check_has_error(
        "foo : () -> Int\n\
         foo = unknown_var",
        "TYP_002"
    );
}

#[test]
fn type_check_arg_count_mismatch() {
    check_has_error(
        "add : (Int, Int) -> Int\n\
         add = |a, b| a + b\n\
         main : () -> Int\n\
         main = add(1)",
        "TYP_007"
    );
}

#[test]
fn type_check_binary_op_type_mismatch() {
    check_has_error(
        "foo : () -> Int\n\
         foo = 1 + \"two\"",
        "TYP_001"
    );
}

#[test]
fn type_check_list_homogeneity() {
    check_has_error(
        "foo : () -> List<Int>\n\
         foo = [1, \"two\", 3]",
        "TYP_016"
    );
}

#[test]
fn type_check_list_ok() {
    check_ok(
        "foo : () -> List<Int>\n\
         foo = [1, 2, 3]"
    );
}

// ============================================================
// Builtin Type Signature Tests
// ============================================================

#[test]
fn type_check_println_wrong_arg_type() {
    // Console.println! expects String, passing Int should error
    check_has_error(
        "@prelude(script)\n\
         main! : () -> {Console} ()\n\
         main! = Console.println!(42)",
        "TYP_008"
    );
}

#[test]
fn type_check_println_correct_arg() {
    // Console.println! with String should be fine
    assert_eq!(
        count_errors(
            "@prelude(script)\n\
             main! : () -> {Console} ()\n\
             main! = Console.println!(\"hello\")",
            "TYP"
        ),
        0
    );
}

#[test]
fn type_check_println_wrong_arg_count() {
    // Console.println! expects 1 arg, passing 2 should error
    check_has_error(
        "@prelude(script)\n\
         main! : () -> {Console} ()\n\
         main! = Console.println!(\"a\", \"b\")",
        "TYP_007"
    );
}

#[test]
fn type_check_read_line_returns_string() {
    // Console.read_line! returns String, assigning to Int-expecting context should error
    check_has_error(
        "@prelude(script)\n\
         main! : () -> {Console} Int\n\
         main! = Console.read_line!()",
        "TYP_006"
    );
}

#[test]
fn type_check_time_sleep_expects_int() {
    // Time.sleep! expects Int, passing String should error
    check_has_error(
        "@prelude(script)\n\
         main! : () -> {Time} ()\n\
         main! = Time.sleep!(\"100\")",
        "TYP_008"
    );
}

#[test]
fn type_check_fs_exists_returns_bool() {
    // Fs.exists! returns Bool
    assert_eq!(
        count_errors(
            "@prelude(script)\n\
             main! : () -> {Fs} Bool\n\
             main! = Fs.exists!(\"file.txt\")",
            "TYP"
        ),
        0
    );
}

#[test]
fn type_check_string_length_correct() {
    // String.length takes String, returns Int
    assert_eq!(
        count_errors(
            "@prelude(core)\n\
             len : String -> Int\n\
             len = |s| String.length(s)",
            "TYP"
        ),
        0
    );
}

#[test]
fn type_check_string_length_wrong_arg() {
    // String.length expects String, passing Int should error
    check_has_error(
        "@prelude(core)\n\
         len : () -> Int\n\
         len = String.length(42)",
        "TYP_008"
    );
}

// ============================================================
// Sum Type Tests
// ============================================================

#[test]
fn type_check_sum_type_definition() {
    check_ok("type Color = | Red | Green | Blue");
}

#[test]
fn type_check_sum_type_with_payload() {
    check_ok("type Option = | Some(Int) | None");
}

#[test]
fn type_check_sum_type_constructor_expression() {
    check_ok(
        "type Option = | Some(Int) | None\n\
         wrap : Int -> Option\n\
         wrap = |x| Some(x)"
    );
}

#[test]
fn type_check_nullary_constructor_expression() {
    check_ok(
        "type Color = | Red | Green | Blue\n\
         get : () -> Color\n\
         get = Red"
    );
}

#[test]
fn type_check_sum_type_without_leading_pipe() {
    check_ok("type Status = Active | Inactive | Pending");
}

#[test]
fn type_check_match_on_sum_type() {
    check_ok(
        "type Color = | Red | Green | Blue\n\
         describe : Color -> String\n\
         describe = |c| {\n\
           match c\n\
             Red -> \"red\"\n\
             Green -> \"green\"\n\
             Blue -> \"blue\"\n\
         }"
    );
}

#[test]
fn type_check_constructor_pattern_match() {
    check_ok(
        "type Option = | Some(Int) | None\n\
         unwrap : (Option, Int) -> Int\n\
         unwrap = |opt, default| {\n\
           match opt\n\
             Some(v) -> v\n\
             None -> default\n\
         }"
    );
}

// ============================================================
// Effect Checker Tests
// ============================================================

#[test]
fn effect_check_no_effect_needed() {
    // Pure function should have zero effect diagnostics
    assert_eq!(count_errors("add : (Int, Int) -> Int\nadd = |a, b| a + b", "CAP"), 0);
}

#[test]
fn effect_check_missing_capability() {
    check_has_error(
        "fetch! : () -> String\n\
         fetch! = Http.get!(\"http://example.com\")",
        "CAP_001"
    );
}

#[test]
fn effect_check_declared_capability() {
    // Function declares {Console} and uses Console.print! — should be ok
    assert_eq!(
        count_errors(
            "main! : () -> {Console} ()\n\
             main! = Console.print!(\"hello\")",
            "CAP"
        ),
        0
    );
}

// ============================================================
// Refinement Checker Tests
// ============================================================

#[test]
fn refinement_check_valid_port() {
    assert_eq!(
        count_errors(
            "type Port = Int where self > 0 && self < 65536\n\
             test_port : () -> ()\n\
             test_port = {\n\
               let p : Port = 8080\n\
             }",
            "REF"
        ),
        0
    );
}

#[test]
fn refinement_check_invalid_port_too_high() {
    check_has_error(
        "type Port = Int where self > 0 && self < 65536\n\
         test_port : () -> ()\n\
         test_port = {\n\
           let p : Port = 70000\n\
         }",
        "REF_001"
    );
}

#[test]
fn refinement_check_invalid_port_zero() {
    check_has_error(
        "type Port = Int where self > 0 && self < 65536\n\
         test_port : () -> ()\n\
         test_port = {\n\
           let p : Port = 0\n\
         }",
        "REF_001"
    );
}

#[test]
fn refinement_check_probability_range() {
    check_has_error(
        "type Prob = Int where self >= 0 && self <= 100\n\
         check_prob : () -> ()\n\
         check_prob = {\n\
           let p : Prob = 101\n\
         }",
        "REF_001"
    );
}

// ============================================================
// Syntax / Parse Tests
// ============================================================

#[test]
fn parse_empty_file() {
    check_ok("");
}

#[test]
fn parse_comments_only() {
    check_ok("// This is a comment\n/* block comment */");
}

#[test]
fn parse_pipe_expression() {
    check_ok(
        "double : Int -> Int\n\
         double = |x| x + x\n\
         main : () -> Int\n\
         main = 5 |> double"
    );
}

#[test]
fn parse_match_expression() {
    check_ok(
        "foo : Int -> String\n\
         foo = |x| {\n\
           match x\n\
             1 -> \"one\"\n\
             _ -> \"other\"\n\
         }"
    );
}

#[test]
fn parse_for_expression() {
    check_ok(
        "foo : () -> ()\n\
         foo = for x in [1, 2, 3] do x"
    );
}

#[test]
fn parse_string_interpolation() {
    check_ok(
        "greet : String -> String\n\
         greet = |name| \"Hello, ${name}!\""
    );
}

#[test]
fn parse_range_expression() {
    // Range expression parses correctly
    let result = parse_source(
        "r : () -> ()\n\
         r = 1..10",
        "<test>"
    );
    let syntax_errors = result.diagnostics.iter().filter(|d| d.code.starts_with("SYN")).count();
    assert_eq!(syntax_errors, 0);
}

#[test]
fn parse_effect_def() {
    check_ok(
        "effect Logger {\n\
           log! : String -> ()\n\
         }"
    );
}

#[test]
fn parse_module_decl() {
    check_ok("@module MyModule");
}

#[test]
fn parse_prelude_decl() {
    check_ok("@prelude(core)");
}

#[test]
fn parse_semicolons_in_block() {
    check_ok(
        "foo : () -> Int\n\
         foo = {\n\
           let x = 1;\n\
           let y = 2;\n\
           x + y\n\
         }"
    );
}

#[test]
fn parse_trailing_commas() {
    check_ok(
        "type User = { name: String, age: Int, }\n\
         make : () -> User\n\
         make = User { name: \"Alice\", age: 30, }"
    );
}

// ============================================================
// Integration: Full Programs
// ============================================================

#[test]
fn integration_hello_world() {
    let source = std::fs::read_to_string("../examples/hello.bl").expect("Can't read hello.bl");
    let result = parse_source(&source, "hello.bl");
    let syntax_errors: Vec<_> = result.diagnostics.iter().filter(|d| d.code.starts_with("SYN")).collect();
    assert!(syntax_errors.is_empty(), "hello.bl has syntax errors: {:?}", syntax_errors);
}

#[test]
fn integration_sum_type_test() {
    let source = std::fs::read_to_string("../examples/sum_type_test.bl").expect("Can't read");
    let result = parse_source(&source, "sum_type_test.bl");
    let syntax_errors: Vec<_> = result.diagnostics.iter().filter(|d| d.code.starts_with("SYN")).collect();
    assert!(syntax_errors.is_empty(), "sum_type_test.bl has syntax errors: {:?}", syntax_errors);
}

#[test]
fn integration_struct_test() {
    let source = std::fs::read_to_string("../examples/struct_test.bl").expect("Can't read");
    let result = parse_source(&source, "struct_test.bl");
    assert_eq!(result.status, "success", "struct_test.bl should pass: {:?}",
        result.diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>());
}

#[test]
fn integration_refinement_test() {
    let source = std::fs::read_to_string("../examples/refinement_test.bl").expect("Can't read");
    let result = parse_source(&source, "refinement_test.bl");
    let syntax_errors: Vec<_> = result.diagnostics.iter().filter(|d| d.code.starts_with("SYN")).collect();
    assert!(syntax_errors.is_empty(), "refinement_test.bl has syntax errors: {:?}", syntax_errors);
    // Should have refinement violations
    let ref_errors = result.diagnostics.iter().filter(|d| d.code.starts_with("REF")).count();
    assert!(ref_errors > 0, "Expected refinement errors for out-of-range values");
}
