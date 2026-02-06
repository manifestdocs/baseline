use blc::diagnostics::Severity;
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
// Tuple Type Checking Tests
// ============================================================

#[test]
fn type_check_tuple_return_type() {
    // Function returning (Int, String) with correct body
    check_ok(
        "pair : () -> (Int, String)\n\
         pair = (1, \"hello\")"
    );
}

#[test]
fn type_check_tuple_return_mismatch() {
    // Function returning (Int, String) but body has (Int, Int)
    check_has_error(
        "pair : () -> (Int, String)\n\
         pair = (1, 2)",
        "TYP_006"
    );
}

#[test]
fn type_check_tuple_destructuring() {
    // Tuple destructuring binds correct types
    check_ok(
        "sum : () -> Int\n\
         sum = {\n\
           let (x, y) = (10, 20)\n\
           x + y\n\
         }"
    );
}

#[test]
fn type_check_tuple_in_match() {
    // Match on a tuple expression with pattern binding
    check_ok(
        "first : () -> Int\n\
         first = {\n\
           let t = (10, \"hello\")\n\
           match t\n\
             (a, _) -> a\n\
         }"
    );
}

// ============================================================
// Float Type Checking Tests
// ============================================================

#[test]
fn type_check_float_annotation_mismatch() {
    // let x: Int = 3.14 should report type mismatch
    check_has_error(
        "foo : () -> ()\n\
         foo = {\n\
           let x : Int = 3.14\n\
         }",
        "TYP_021"
    );
}

#[test]
fn type_check_float_annotation_ok() {
    // let x: Float = 3.14 should pass
    check_ok(
        "foo : () -> ()\n\
         foo = {\n\
           let x : Float = 3.14\n\
         }"
    );
}

#[test]
fn type_check_mixed_int_float_arithmetic() {
    // 1 + 3.14 should promote to Float
    check_ok(
        "foo : () -> Float\n\
         foo = 1 + 3.14"
    );
}

#[test]
fn type_check_float_arithmetic_pure() {
    // Float + Float = Float
    check_ok(
        "foo : () -> Float\n\
         foo = 1.5 + 2.5"
    );
}

#[test]
fn type_check_let_annotation_string() {
    // let s: String = 42 should error
    check_has_error(
        "foo : () -> ()\n\
         foo = {\n\
           let s : String = 42\n\
         }",
        "TYP_021"
    );
}

// ============================================================
// Lambda Argument Inference Tests
// ============================================================

#[test]
fn type_check_lambda_inferred_from_higher_order() {
    // apply expects ((Int) -> Int, Int) -> Int; lambda arg x inferred as Int
    check_ok(
        "apply : ((Int) -> Int, Int) -> Int\n\
         apply = |f, x| f(x)\n\
         main : () -> Int\n\
         main = apply(|x| x + 1, 5)"
    );
}

#[test]
fn type_check_lambda_inferred_body_mismatch() {
    // apply expects (Int) -> String callback, but lambda body returns Int (x + 1)
    check_has_error(
        "apply : ((Int) -> String, Int) -> String\n\
         apply = |f, x| f(x)\n\
         main : () -> String\n\
         main = apply(|x| x + 1, 5)",
        "TYP_008"
    );
}

#[test]
fn type_check_lambda_standalone_unknown_args() {
    // Standalone lambda without context — args remain Unknown, no error
    check_ok(
        "foo : () -> ()\n\
         foo = {\n\
           let f = |x| x\n\
         }"
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

// ============================================================
// Option & Result Type Awareness
// ============================================================

#[test]
fn type_check_option_int_annotation() {
    // Option<Int> in a function return type — Some(1) should be compatible
    let source = r#"
@prelude(core)
wrap : (Int) -> Option<Int>
wrap = |x| Some(x)
"#;
    check_ok(source);
}

#[test]
fn type_check_option_type_mismatch() {
    // Returning a Result where Option<Int> is expected should error
    let source = r#"
@prelude(core)
wrap : (Int) -> Option<Int>
wrap = |x| Ok(x)
"#;
    check_has_error(source, "TYP_006");
}

#[test]
fn type_check_result_annotation() {
    // Result<Int, String> in a function return type — Ok(1) should be compatible
    let source = r#"
@prelude(core)
safe_div : (Int, Int) -> Result<Int, String>
safe_div = |a, b| Ok(a)
"#;
    check_ok(source);
}

#[test]
fn type_check_option_sugar_question_mark() {
    // Int? desugars to Option<Int>
    let source = r#"
@prelude(core)
find : (Int) -> Int?
find = |x| Some(x)
"#;
    check_ok(source);
}

#[test]
fn type_check_option_sugar_mismatch() {
    // Returning plain Int where Int? (Option<Int>) is expected should error
    let source = r#"
@prelude(core)
find : (Int) -> Int?
find = |x| x
"#;
    check_has_error(source, "TYP_006");
}

#[test]
fn type_check_let_option_annotation() {
    // let x : Option<Int> = Some(1) — should pass
    let source = r#"
@prelude(core)
main : () -> Int
main = || {
    let x : Option<Int> = Some(1);
    1
}
"#;
    check_ok(source);
}

#[test]
fn type_check_let_result_annotation() {
    // let x : Result<Int, String> = Ok(1) — should pass
    let source = r#"
@prelude(core)
main : () -> Int
main = || {
    let x : Result<Int, String> = Ok(1);
    1
}
"#;
    check_ok(source);
}

#[test]
fn type_check_option_result_cross_mismatch() {
    // let x : Option<Int> = Ok(1) — should error, Result != Option
    let source = r#"
@prelude(core)
main : () -> Int
main = || {
    let x : Option<Int> = Ok(1);
    1
}
"#;
    check_has_error(source, "TYP_021");
}

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

// ============================================================
// Exhaustive Match Checking (TYP_022)
// ============================================================

#[test]
fn exhaustive_enum_all_variants() {
    let source = r#"
type Color = | Red | Green | Blue
describe : Color -> String
describe = |c| {
  match c
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
}
"#;
    check_ok(source);
}

#[test]
fn exhaustive_enum_missing_variant() {
    let source = r#"
type Color = | Red | Green | Blue
describe : Color -> String
describe = |c| {
  match c
    Red -> "red"
    Green -> "green"
}
"#;
    check_has_error(source, "TYP_022");
}

#[test]
fn exhaustive_enum_wildcard() {
    let source = r#"
type Color = | Red | Green | Blue
describe : Color -> String
describe = |c| {
  match c
    Red -> "red"
    _ -> "other"
}
"#;
    check_ok(source);
}

#[test]
fn exhaustive_enum_variable_binding() {
    let source = r#"
type Color = | Red | Green | Blue
describe : Color -> String
describe = |c| {
  match c
    Red -> "red"
    other -> "other"
}
"#;
    check_ok(source);
}

#[test]
fn exhaustive_option_complete() {
    let source = r#"
type Option = | Some(Int) | None
unwrap_or : (Option, Int) -> Int
unwrap_or = |opt, default| {
  match opt
    Some(v) -> v
    None -> default
}
"#;
    check_ok(source);
}

#[test]
fn exhaustive_option_missing_none() {
    let source = r#"
type Option = | Some(Int) | None
unwrap : Option -> Int
unwrap = |opt| {
  match opt
    Some(v) -> v
}
"#;
    check_has_error(source, "TYP_022");
}

#[test]
fn exhaustive_result_complete() {
    let source = r#"
type Result = | Ok(Int) | Err(String)
get_val : Result -> Int
get_val = |r| {
  match r
    Ok(v) -> v
    Err(e) -> 0
}
"#;
    check_ok(source);
}

#[test]
fn exhaustive_bool_both() {
    let source = r#"
to_str : Bool -> String
to_str = |b| {
  match b
    true -> "yes"
    false -> "no"
}
"#;
    check_ok(source);
}

#[test]
fn exhaustive_bool_missing_false() {
    let source = r#"
to_str : Bool -> String
to_str = |b| {
  match b
    true -> "yes"
}
"#;
    check_has_error(source, "TYP_022");
}

#[test]
fn exhaustive_int_with_wildcard() {
    let source = r#"
describe : Int -> String
describe = |n| {
  match n
    1 -> "one"
    _ -> "other"
}
"#;
    check_ok(source);
}

#[test]
fn exhaustive_int_without_wildcard() {
    let source = r#"
describe : Int -> String
describe = |n| {
  match n
    1 -> "one"
    2 -> "two"
}
"#;
    check_has_error(source, "TYP_022");
}

#[test]
fn exhaustive_user_enum_missing() {
    let source = r#"
type Status = | Active | Pending | Closed
label : Status -> String
label = |s| {
  match s
    Active -> "active"
    Pending -> "pending"
}
"#;
    check_has_error(source, "TYP_022");
}

// ============================================================
// Forward Function Reference Tests
// ============================================================

#[test]
fn forward_call() {
    check_ok(r#"
main : () -> Int
main = || helper(1)

helper : Int -> Int
helper = |x| x + 1
"#);
}

#[test]
fn forward_mutual_recursion() {
    check_ok(r#"
is_even : Int -> Bool
is_even = |n| {
  match n
    0 -> true
    _ -> is_odd(n - 1)
}

is_odd : Int -> Bool
is_odd = |n| {
  match n
    0 -> false
    _ -> is_even(n - 1)
}
"#);
}

#[test]
fn forward_call_type_check() {
    check_ok(r#"
run : () -> String
run = || format(42)

format : Int -> String
format = |n| "hello"
"#);
}

#[test]
fn truly_undefined_still_errors() {
    check_has_error(r#"
main : () -> Int
main = || nonexistent(1)
"#, "TYP_002");
}

#[test]
fn forward_arg_type_mismatch() {
    check_has_error(r#"
main : () -> Int
main = || helper("wrong")

helper : Int -> Int
helper = |x| x + 1
"#, "TYP_008");
}

#[test]
fn local_let_stays_sequential() {
    check_has_error(r#"
main : () -> Int
main = || {
  let result = future_val
  let future_val = 42
  result
}
"#, "TYP_002");
}

// ============================================================
// For Expression Type Checking (TYP_023)
// ============================================================

#[test]
fn for_over_list_ok() {
    check_ok(r#"
foo : () -> ()
foo = for x in [1, 2, 3] do x + 1
"#);
}

#[test]
fn for_over_non_list_errors() {
    check_has_error(r#"
foo : () -> ()
foo = for x in 42 do x
"#, "TYP_023");
}

// ============================================================
// Range Expression Type Checking (TYP_024)
// ============================================================

#[test]
fn range_int_ok() {
    check_ok(r#"
foo : () -> ()
foo = {
  let r = 1..10
}
"#);
}

#[test]
fn range_non_int_errors() {
    check_has_error(r#"
foo : () -> ()
foo = {
  let r = "a".."z"
}
"#, "TYP_024");
}

// ============================================================
// Unary Expression Type Checking (TYP_025)
// ============================================================

#[test]
fn unary_not_bool_ok() {
    check_ok(r#"
foo : () -> Bool
foo = not true
"#);
}

#[test]
fn unary_negate_int_ok() {
    check_ok(r#"
foo : () -> Int
foo = -5
"#);
}

#[test]
fn unary_not_int_errors() {
    check_has_error(r#"
foo : () -> Bool
foo = not 42
"#, "TYP_025");
}

#[test]
fn unary_negate_string_errors() {
    check_has_error(r#"
foo : () -> ()
foo = {
  let n = -"hello"
}
"#, "TYP_025");
}

// ============================================================
// String Interpolation Type Checking
// ============================================================

#[test]
fn string_interpolation_checks_expr() {
    check_has_error(r#"
foo : () -> String
foo = "hello ${undefined_var}"
"#, "TYP_002");
}

#[test]
fn string_interpolation_ok() {
    check_ok(r#"
foo : () -> String
foo = {
  let x = 1
  "val: ${x}"
}
"#);
}

// ============================================================
// Warning Behavior Tests
// ============================================================

#[test]
fn warning_does_not_cause_failure() {
    // f(g(x)) produces STY_001 warning but no errors — status should be success
    let source = r#"
g : Int -> Int
g = |x| x + 1

f : Int -> Int
f = |x| x * 2

main : () -> Int
main = f(g(1))
"#;
    let result = parse_source(source, "<test>");
    assert_eq!(result.status, "success", "Warnings should not cause failure");
    assert!(
        result.diagnostics.iter().any(|d| d.code == "STY_001"),
        "Expected STY_001 warning"
    );
}

#[test]
fn nested_call_pipe_suggestion() {
    // f(g(x)) should emit STY_001 warning
    let source = r#"
g : Int -> Int
g = |x| x + 1

f : Int -> Int
f = |x| x * 2

main : () -> Int
main = f(g(1))
"#;
    let result = parse_source(source, "<test>");
    let sty_warnings: Vec<_> = result.diagnostics.iter()
        .filter(|d| d.code == "STY_001")
        .collect();
    assert!(!sty_warnings.is_empty(), "Expected STY_001 for nested call");
    assert_eq!(sty_warnings[0].severity, Severity::Warning);
}

#[test]
fn pipe_style_no_warning() {
    // x |> g |> f should NOT emit STY_001
    let source = r#"
g : Int -> Int
g = |x| x + 1

f : Int -> Int
f = |x| x * 2

main : () -> Int
main = 1 |> g |> f
"#;
    let result = parse_source(source, "<test>");
    assert!(
        !result.diagnostics.iter().any(|d| d.code == "STY_001"),
        "Pipe style should not trigger STY_001"
    );
}

#[test]
fn error_and_warning_both_reported() {
    // Source with both a type error and a nested call warning
    let source = r#"
g : Int -> Int
g = |x| x + 1

f : Int -> String
f = |x| x * 2

main : () -> String
main = f(g(1))
"#;
    let result = parse_source(source, "<test>");
    assert_eq!(result.status, "failure", "Errors should cause failure");
    assert!(
        result.diagnostics.iter().any(|d| d.severity == Severity::Error),
        "Should have at least one error"
    );
    assert!(
        result.diagnostics.iter().any(|d| d.severity == Severity::Warning),
        "Should have at least one warning"
    );
}

#[test]
fn multi_arg_call_no_warning() {
    // f(x, g(y)) should NOT emit STY_001 (only single-arg calls trigger it)
    let source = r#"
g : Int -> Int
g = |x| x + 1

f : (Int, Int) -> Int
f = |a, b| a + b

main : () -> Int
main = f(1, g(2))
"#;
    let result = parse_source(source, "<test>");
    assert!(
        !result.diagnostics.iter().any(|d| d.code == "STY_001"),
        "Multi-arg calls should not trigger STY_001"
    );
}

// ============================================================
// Inline Test Type Checking (TYP_026)
// ============================================================

#[test]
fn inline_test_bool_ok() {
    check_ok(r#"
add : (Int, Int) -> Int
add = |a, b| a + b
  where
    test "basic" = add(1, 2) == 3
"#);
}

#[test]
fn inline_test_non_bool_errors() {
    check_has_error(r#"
add : (Int, Int) -> Int
add = |a, b| a + b
  where
    test "returns int" = add(1, 2)
"#, "TYP_026");
}

#[test]
fn inline_test_top_level_bool_ok() {
    check_ok("test \"sanity\" = 1 + 1 == 2");
}

#[test]
fn inline_test_top_level_non_bool_errors() {
    check_has_error("test \"bad\" = 42", "TYP_026");
}
