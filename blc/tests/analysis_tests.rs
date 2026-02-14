use blc::diagnostics::Severity;
use blc::parse::parse_source;

/// Helper: check source has no diagnostics
fn check_ok(source: &str) {
    let result = parse_source(source, "<test>");
    assert!(
        result.diagnostics.is_empty(),
        "Expected no diagnostics, got: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Helper: check source has a specific diagnostic code
fn check_has_error(source: &str, code: &str) {
    let result = parse_source(source, "<test>");
    assert!(
        result.diagnostics.iter().any(|d| d.code == code),
        "Expected diagnostic {}, got: {:?}",
        code,
        result
            .diagnostics
            .iter()
            .map(|d| format!("{}: {}", d.code, d.message))
            .collect::<Vec<_>>()
    );
}

/// Helper: check source has a specific warning code
fn check_has_warning(source: &str, code: &str) {
    let result = parse_source(source, "<test>");
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == code && d.severity == Severity::Warning),
        "Expected warning {}, got: {:?}",
        code,
        result
            .diagnostics
            .iter()
            .map(|d| format!("{}: {} ({:?})", d.code, d.message, d.severity))
            .collect::<Vec<_>>()
    );
}

/// Helper: check source has no errors (warnings are allowed)
fn check_no_errors(source: &str) {
    let result = parse_source(source, "<test>");
    let errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "Expected no errors, got: {:?}",
        errors.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

/// Helper: count diagnostics with given code prefix
fn count_errors(source: &str, prefix: &str) -> usize {
    let result = parse_source(source, "<test>");
    result
        .diagnostics
        .iter()
        .filter(|d| d.code.starts_with(prefix))
        .count()
}

// ============================================================
// Type Checker Tests
// ============================================================

#[test]
fn type_check_simple_function() {
    check_ok("fn add(a: Int, b: Int) -> Int = a + b");
}

#[test]
fn type_check_string_function() {
    check_ok("fn greet(name: String) -> String = name");
}

#[test]
fn type_check_bool_function() {
    check_ok("fn is_pos(n: Int) -> Bool = n > 0");
}

#[test]
fn type_check_float_literal() {
    check_ok("fn pi() -> Float = 3.14");
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
                  fn make_user() -> User = User { name: \"Alice\", age: 30 }",
    );
}

#[test]
fn type_check_struct_field_access() {
    check_ok(
        "type User = { name: String, age: Int }\n\
                  fn get_name(u: User) -> String = u.name",
    );
}

#[test]
fn type_check_struct_missing_field() {
    check_has_error(
        "type User = { name: String, age: Int }\n\
         fn make() -> User = User { name: \"Alice\" }",
        "TYP_012",
    );
}

#[test]
fn type_check_struct_wrong_field_type() {
    check_has_error(
        "type User = { name: String, age: Int }\n\
         fn make() -> User = User { name: \"Alice\", age: \"thirty\" }",
        "TYP_010",
    );
}

#[test]
fn type_check_if_expression_branch_mismatch() {
    check_has_error(
        "fn foo(x: Bool) -> Int = if x then 1 else \"two\"",
        "TYP_004",
    );
}

#[test]
fn type_check_if_condition_not_bool() {
    check_has_error("fn foo(x: Int) -> Int = if x then 1 else 2", "TYP_003");
}

#[test]
fn type_check_undefined_variable() {
    check_has_error("fn foo() -> Int = unknown_var", "TYP_002");
}

#[test]
fn type_check_arg_count_mismatch() {
    check_has_error(
        "fn add(a: Int, b: Int) -> Int = a + b\n\
         fn main() -> Int = add(1)",
        "TYP_007",
    );
}

#[test]
fn type_check_binary_op_type_mismatch() {
    check_has_error("fn foo() -> Int = 1 + \"two\"", "TYP_001");
}

#[test]
fn type_check_list_homogeneity() {
    check_has_error("fn foo() -> List<Int> = [1, \"two\", 3]", "TYP_016");
}

#[test]
fn type_check_list_ok() {
    check_ok("fn foo() -> List<Int> = [1, 2, 3]");
}

// ============================================================
// Builtin Type Signature Tests
// ============================================================

#[test]
fn type_check_println_wrong_arg_type() {
    // Console.println! expects String, passing Int should error
    check_has_error(
        "@prelude(script)\n\
         fn main!() -> {Console} () = Console.println!(42)",
        "TYP_008",
    );
}

#[test]
fn type_check_println_correct_arg() {
    // Console.println! with String should be fine
    assert_eq!(
        count_errors(
            "@prelude(script)\n\
                      fn main!() -> {Console} () = Console.println!(\"hello\")",
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
         fn main!() -> {Console} () = Console.println!(\"a\", \"b\")",
        "TYP_007",
    );
}

#[test]
fn type_check_read_line_returns_string() {
    // Console.read_line! returns String, assigning to Int-expecting context should error
    check_has_error(
        "@prelude(script)\n\
         fn main!() -> {Console} Int = Console.read_line!()",
        "TYP_006",
    );
}

#[test]
fn type_check_time_sleep_expects_int() {
    // Time.sleep! expects Int, passing String should error
    check_has_error(
        "@prelude(script)\n\
         fn main!() -> {Time} () = Time.sleep!(\"100\")",
        "TYP_008",
    );
}

#[test]
fn type_check_fs_exists_returns_bool() {
    // Fs.exists! returns Bool
    assert_eq!(
        count_errors(
            "@prelude(script)\n\
                      fn main!() -> {Fs} Bool = Fs.exists!(\"file.txt\")",
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
                      fn len(s: String) -> Int = String.length(s)",
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
         fn len() -> Int = String.length(42)",
        "TYP_008",
    );
}

// ============================================================
// Tuple Type Checking Tests
// ============================================================

#[test]
fn type_check_tuple_return_type() {
    // Function returning (Int, String) with correct body
    check_ok("fn pair() -> (Int, String) = (1, \"hello\")");
}

#[test]
fn type_check_tuple_return_mismatch() {
    // Function returning (Int, String) but body has (Int, Int)
    check_has_error("fn pair() -> (Int, String) = (1, 2)", "TYP_006");
}

#[test]
fn type_check_tuple_destructuring() {
    // Tuple destructuring binds correct types
    check_ok(
        "fn sum() -> Int = {\n\
                    let (x, y) = (10, 20)\n\
                    x + y\n\
                  }",
    );
}

#[test]
fn type_check_tuple_in_match() {
    // Match on a tuple expression with pattern binding
    check_ok(
        "fn first() -> Int = {\n\
                    let t = (10, \"hello\")\n\
                    match t\n\
                      (a, _) -> a\n\
                  }",
    );
}

// ============================================================
// Float Type Checking Tests
// ============================================================

#[test]
fn type_check_float_annotation_mismatch() {
    // let x: Int = 3.14 should report type mismatch
    check_has_error(
        "fn foo() -> () = {\n\
           let x : Int = 3.14\n\
         }",
        "TYP_021",
    );
}

#[test]
fn type_check_float_annotation_ok() {
    // let x: Float = 3.14 should pass
    check_ok(
        "fn foo() -> () = {\n\
                    let x : Float = 3.14\n\
                  }",
    );
}

#[test]
fn type_check_mixed_int_float_arithmetic() {
    // 1 + 3.14 should promote to Float
    check_ok("fn foo() -> Float = 1 + 3.14");
}

#[test]
fn type_check_float_arithmetic_pure() {
    // Float + Float = Float
    check_ok("fn foo() -> Float = 1.5 + 2.5");
}

#[test]
fn type_check_let_annotation_string() {
    // let s: String = 42 should error
    check_has_error(
        "fn foo() -> () = {\n\
           let s : String = 42\n\
         }",
        "TYP_021",
    );
}

// ============================================================
// Lambda Argument Inference Tests
// ============================================================

#[test]
fn type_check_lambda_inferred_from_higher_order() {
    // apply expects ((Int) -> Int, Int) -> Int; lambda arg x inferred as Int
    check_ok(
        "fn apply(f: (Int) -> Int, x: Int) -> Int = f(x)\n\
                  fn main() -> Int = apply(|x| x + 1, 5)",
    );
}

#[test]
fn type_check_lambda_inferred_body_mismatch() {
    // apply expects (Int) -> String callback, but lambda body returns Int (x + 1)
    check_has_error(
        "fn apply(f: (Int) -> String, x: Int) -> String = f(x)\n\
         fn main() -> String = apply(|x| x + 1, 5)",
        "TYP_008",
    );
}

#[test]
fn type_check_lambda_standalone_unknown_args() {
    // Standalone lambda without context — args remain Unknown, no error
    check_ok(
        "fn foo() -> () = {\n\
                    let f = |x| x\n\
                  }",
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
                  fn wrap(x: Int) -> Option = Some(x)",
    );
}

#[test]
fn type_check_nullary_constructor_expression() {
    check_ok(
        "type Color = | Red | Green | Blue\n\
                  fn get() -> Color = Red",
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
                  fn describe(c: Color) -> String = {\n\
                    match c\n\
                      Red -> \"red\"\n\
                      Green -> \"green\"\n\
                      Blue -> \"blue\"\n\
                  }",
    );
}

#[test]
fn type_check_constructor_pattern_match() {
    check_ok(
        "type Option = | Some(Int) | None\n\
                  fn unwrap(opt: Option, default: Int) -> Int = {\n\
                    match opt\n\
                      Some(v) -> v\n\
                      None -> default\n\
                  }",
    );
}

// ============================================================
// Effect Checker Tests
// ============================================================

#[test]
fn effect_check_no_effect_needed() {
    // Pure function should have zero effect diagnostics
    assert_eq!(
        count_errors("fn add(a: Int, b: Int) -> Int = a + b", "CAP"),
        0
    );
}

#[test]
fn effect_check_missing_capability() {
    check_has_error(
        "fn fetch!() -> String = Http.get!(\"http://example.com\")",
        "CAP_001",
    );
}

#[test]
fn effect_check_declared_capability() {
    // Function declares {Console} and uses Console.print! — should be ok
    assert_eq!(
        count_errors(
            "fn main!() -> {Console} () = Console.print!(\"hello\")",
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
                      fn test_port() -> () = {\n\
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
         fn test_port() -> () = {\n\
           let p : Port = 70000\n\
         }",
        "REF_001",
    );
}

#[test]
fn refinement_check_invalid_port_zero() {
    check_has_error(
        "type Port = Int where self > 0 && self < 65536\n\
         fn test_port() -> () = {\n\
           let p : Port = 0\n\
         }",
        "REF_001",
    );
}

#[test]
fn refinement_check_probability_range() {
    check_has_error(
        "type Prob = Int where self >= 0 && self <= 100\n\
         fn check_prob() -> () = {\n\
           let p : Prob = 101\n\
         }",
        "REF_001",
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
        "fn double(x: Int) -> Int = x + x\n\
                  fn main() -> Int = 5 |> double",
    );
}

#[test]
fn parse_match_expression() {
    check_ok(
        "fn foo(x: Int) -> String = {\n\
                    match x\n\
                      1 -> \"one\"\n\
                      _ -> \"other\"\n\
                  }",
    );
}

#[test]
fn parse_for_expression() {
    check_ok("fn foo() -> () = for x in [1, 2, 3] do ()");
}

#[test]
fn parse_string_interpolation() {
    check_ok("fn greet(name: String) -> String = \"Hello, ${name}!\"");
}

#[test]
fn parse_range_expression() {
    // Range expression parses correctly
    let result = parse_source("fn r() -> () = 1..10", "<test>");
    let syntax_errors = result
        .diagnostics
        .iter()
        .filter(|d| d.code.starts_with("SYN"))
        .count();
    assert_eq!(syntax_errors, 0);
}

#[test]
fn parse_effect_def() {
    check_ok(
        "effect Logger {\n\
                    log! : String -> ()\n\
                  }",
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
        "fn foo() -> Int = {\n\
                    let x = 1;\n\
                    let y = 2;\n\
                    x + y\n\
                  }",
    );
}

#[test]
fn parse_trailing_commas() {
    check_ok(
        "type User = { name: String, age: Int, }\n\
                  fn make() -> User = User { name: \"Alice\", age: 30, }",
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
fn wrap(x: Int) -> Option<Int> = Some(x)
"#;
    check_ok(source);
}

#[test]
fn type_check_option_type_mismatch() {
    // Returning a Result where Option<Int> is expected should error
    let source = r#"
@prelude(core)
fn wrap(x: Int) -> Option<Int> = Ok(x)
"#;
    check_has_error(source, "TYP_006");
}

#[test]
fn type_check_result_annotation() {
    // Result<Int, String> in a function return type — Ok(1) should be compatible
    let source = r#"
@prelude(core)
fn safe_div(a: Int, b: Int) -> Result<Int, String> = Ok(a)
"#;
    check_ok(source);
}

#[test]
fn type_check_option_sugar_question_mark() {
    // Int? desugars to Option<Int>
    let source = r#"
@prelude(core)
fn find(x: Int) -> Int? = Some(x)
"#;
    check_ok(source);
}

#[test]
fn type_check_option_sugar_mismatch() {
    // Returning plain Int where Int? (Option<Int>) is expected should error
    let source = r#"
@prelude(core)
fn find(x: Int) -> Int? = x
"#;
    check_has_error(source, "TYP_006");
}

#[test]
fn type_check_let_option_annotation() {
    // let x : Option<Int> = Some(1) — should pass
    let source = r#"
@prelude(core)
fn main() -> Int = {
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
fn main() -> Int = {
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
fn main() -> Int = {
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
    let syntax_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.code.starts_with("SYN"))
        .collect();
    assert!(
        syntax_errors.is_empty(),
        "hello.bl has syntax errors: {:?}",
        syntax_errors
    );
}

#[test]
fn integration_sum_type_test() {
    let source = std::fs::read_to_string("../examples/sum_type_test.bl").expect("Can't read");
    let result = parse_source(&source, "sum_type_test.bl");
    let syntax_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.code.starts_with("SYN"))
        .collect();
    assert!(
        syntax_errors.is_empty(),
        "sum_type_test.bl has syntax errors: {:?}",
        syntax_errors
    );
}

#[test]
fn integration_struct_test() {
    let source = std::fs::read_to_string("../examples/struct_test.bl").expect("Can't read");
    let result = parse_source(&source, "struct_test.bl");
    assert_eq!(
        result.status,
        "success",
        "struct_test.bl should pass: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn integration_refinement_test() {
    let source = std::fs::read_to_string("../examples/refinement_test.bl").expect("Can't read");
    let result = parse_source(&source, "refinement_test.bl");
    let syntax_errors: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.code.starts_with("SYN"))
        .collect();
    assert!(
        syntax_errors.is_empty(),
        "refinement_test.bl has syntax errors: {:?}",
        syntax_errors
    );
    // Should have refinement violations
    let ref_errors = result
        .diagnostics
        .iter()
        .filter(|d| d.code.starts_with("REF"))
        .count();
    assert!(
        ref_errors > 0,
        "Expected refinement errors for out-of-range values"
    );
}

// ============================================================
// Exhaustive Match Checking (TYP_022)
// ============================================================

#[test]
fn exhaustive_enum_all_variants() {
    let source = r#"
type Color = | Red | Green | Blue
fn describe(c: Color) -> String = {
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
fn describe(c: Color) -> String = {
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
fn describe(c: Color) -> String = {
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
fn describe(c: Color) -> String = {
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
fn unwrap_or(opt: Option, default: Int) -> Int = {
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
fn unwrap(opt: Option) -> Int = {
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
fn get_val(r: Result) -> Int = {
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
fn to_str(b: Bool) -> String = {
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
fn to_str(b: Bool) -> String = {
  match b
    true -> "yes"
}
"#;
    check_has_error(source, "TYP_022");
}

#[test]
fn exhaustive_int_with_wildcard() {
    let source = r#"
fn describe(n: Int) -> String = {
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
fn describe(n: Int) -> String = {
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
fn label(s: Status) -> String = {
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
    check_ok(
        r#"
fn main() -> Int = helper(1)

fn helper(x: Int) -> Int = x + 1
"#,
    );
}

#[test]
fn forward_mutual_recursion() {
    check_ok(
        r#"
fn is_even(n: Int) -> Bool = {
  match n
    0 -> true
    _ -> is_odd(n - 1)
}

fn is_odd(n: Int) -> Bool = {
  match n
    0 -> false
    _ -> is_even(n - 1)
}
"#,
    );
}

#[test]
fn forward_call_type_check() {
    check_ok(
        r#"
fn run() -> String = format(42)

fn format(n: Int) -> String = "hello"
"#,
    );
}

#[test]
fn truly_undefined_still_errors() {
    check_has_error(
        r#"
fn main() -> Int = nonexistent(1)
"#,
        "TYP_002",
    );
}

#[test]
fn forward_arg_type_mismatch() {
    check_has_error(
        r#"
fn main() -> Int = helper("wrong")

fn helper(x: Int) -> Int = x + 1
"#,
        "TYP_008",
    );
}

#[test]
fn local_let_stays_sequential() {
    check_has_error(
        r#"
fn main() -> Int = {
  let result = future_val
  let future_val = 42
  result
}
"#,
        "TYP_002",
    );
}

// ============================================================
// For Expression Type Checking (TYP_023)
// ============================================================

#[test]
fn for_over_list_ok() {
    check_ok(
        r#"
fn foo() -> () = for x in [1, 2, 3] do ()
"#,
    );
}

#[test]
fn for_body_non_unit_warns() {
    check_has_warning(
        r#"
fn foo() -> () = for x in [1, 2, 3] do x + 1
"#,
        "TYP_033",
    );
}

#[test]
fn for_over_non_list_errors() {
    check_has_error(
        r#"
fn foo() -> () = for x in 42 do x
"#,
        "TYP_023",
    );
}

// ============================================================
// Range Expression Type Checking (TYP_024)
// ============================================================

#[test]
fn range_int_ok() {
    check_ok(
        r#"
fn foo() -> () = {
  let r = 1..10
}
"#,
    );
}

#[test]
fn range_non_int_errors() {
    check_has_error(
        r#"
fn foo() -> () = {
  let r = "a".."z"
}
"#,
        "TYP_024",
    );
}

// ============================================================
// Unary Expression Type Checking (TYP_025)
// ============================================================

#[test]
fn unary_not_bool_ok() {
    check_ok(
        r#"
fn foo() -> Bool = not true
"#,
    );
}

#[test]
fn unary_negate_int_ok() {
    check_ok(
        r#"
fn foo() -> Int = -5
"#,
    );
}

#[test]
fn unary_not_int_errors() {
    check_has_error(
        r#"
fn foo() -> Bool = not 42
"#,
        "TYP_025",
    );
}

#[test]
fn unary_negate_string_errors() {
    check_has_error(
        r#"
fn foo() -> () = {
  let n = -"hello"
}
"#,
        "TYP_025",
    );
}

// ============================================================
// String Interpolation Type Checking
// ============================================================

#[test]
fn string_interpolation_checks_expr() {
    check_has_error(
        r#"
fn foo() -> String = "hello ${undefined_var}"
"#,
        "TYP_002",
    );
}

#[test]
fn string_interpolation_ok() {
    check_ok(
        r#"
fn foo() -> String = {
  let x = 1
  "val: ${x}"
}
"#,
    );
}

// ============================================================
// Warning Behavior Tests
// ============================================================

#[test]
fn warning_does_not_cause_failure() {
    // f(g(x)) produces STY_001 warning but no errors — status should be success
    let source = r#"
fn g(x: Int) -> Int = x + 1

fn f(x: Int) -> Int = x * 2

fn main() -> Int = f(g(1))
"#;
    let result = parse_source(source, "<test>");
    assert_eq!(
        result.status, "success",
        "Warnings should not cause failure"
    );
    assert!(
        result.diagnostics.iter().any(|d| d.code == "STY_001"),
        "Expected STY_001 warning"
    );
}

#[test]
fn nested_call_pipe_suggestion() {
    // f(g(x)) should emit STY_001 warning
    let source = r#"
fn g(x: Int) -> Int = x + 1

fn f(x: Int) -> Int = x * 2

fn main() -> Int = f(g(1))
"#;
    let result = parse_source(source, "<test>");
    let sty_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.code == "STY_001")
        .collect();
    assert!(!sty_warnings.is_empty(), "Expected STY_001 for nested call");
    assert_eq!(sty_warnings[0].severity, Severity::Warning);
}

#[test]
fn pipe_style_no_warning() {
    // x |> g |> f should NOT emit STY_001
    let source = r#"
fn g(x: Int) -> Int = x + 1

fn f(x: Int) -> Int = x * 2

fn main() -> Int = 1 |> g |> f
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
fn g(x: Int) -> Int = x + 1

fn f(x: Int) -> String = x * 2

fn main() -> String = f(g(1))
"#;
    let result = parse_source(source, "<test>");
    assert_eq!(result.status, "failure", "Errors should cause failure");
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error),
        "Should have at least one error"
    );
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Warning),
        "Should have at least one warning"
    );
}

#[test]
fn multi_arg_call_no_warning() {
    // f(x, g(y)) should NOT emit STY_001 (only single-arg calls trigger it)
    let source = r#"
fn g(x: Int) -> Int = x + 1

fn f(a: Int, b: Int) -> Int = a + b

fn main() -> Int = f(1, g(2))
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
    check_ok(
        r#"
fn add(a: Int, b: Int) -> Int = a + b

@test
test "basic" = add(1, 2) == 3
"#,
    );
}

#[test]
fn inline_test_non_bool_errors() {
    check_has_error(
        r#"
fn add(a: Int, b: Int) -> Int = a + b

@test
test "returns int" = add(1, 2)
"#,
        "TYP_026",
    );
}

#[test]
fn inline_test_top_level_bool_ok() {
    check_ok("test \"sanity\" = 1 + 1 == 2");
}

#[test]
fn inline_test_top_level_non_bool_errors() {
    check_has_error("test \"bad\" = 42", "TYP_026");
}

// ============================================================
// Type Inference Tests
// ============================================================

#[test]
fn infer_list_map_returns_typed_list() {
    // List.map([1,2,3], |x| x + 1) should type-check with inferred types
    check_ok(
        r#"
@prelude(core)
fn main() -> () = {
    let doubled = List.map([1, 2, 3], |x| x + 1)
}
"#,
    );
}

#[test]
fn infer_list_filter_preserves_element_type() {
    // List.filter preserves element type
    check_ok(
        r#"
@prelude(core)
fn main() -> () = {
    let xs = [1, 2, 3, 4]
    let evens = List.filter(xs, |x| x > 2)
}
"#,
    );
}

#[test]
fn infer_option_unwrap_extracts_inner() {
    // Option.unwrap(Some(42)) should return Int (nested call triggers STY_001 warning)
    check_no_errors(
        r#"
@prelude(core)
fn main() -> Int = {
    let x = Option.unwrap(Some(42))
    x + 1
}
"#,
    );
}

#[test]
fn infer_try_expression_option() {
    // Some(42)? should return Int
    check_ok(
        r#"
@prelude(core)
fn wrap() -> Option<Int> = {
    let x : Option<Int> = Some(42)
    let val = x?
    Some(val + 1)
}
"#,
    );
}

#[test]
fn infer_try_expression_result() {
    // Ok(42)? should return Int
    check_ok(
        r#"
@prelude(core)
fn wrap() -> Result<Int, String> = {
    let x : Result<Int, String> = Ok(42)
    let val = x?
    Ok(val + 1)
}
"#,
    );
}

#[test]
fn infer_list_head_concrete() {
    // List.head([1, 2, 3]) should return Int
    check_ok(
        r#"
@prelude(core)
fn main() -> Int = {
    let first = List.head([1, 2, 3])
    first + 1
}
"#,
    );
}

#[test]
fn infer_let_propagation() {
    // let x = List.head([1,2,3]) then x + 1 type-checks (Int + Int)
    check_ok(
        r#"
@prelude(core)
fn main() -> Int = {
    let x = List.head([1, 2, 3])
    x + 1
}
"#,
    );
}

#[test]
fn infer_nested_generic_calls() {
    // List.head(List.map([1,2], |x| x > 0)) should return Bool (nested call triggers STY_001)
    check_no_errors(
        r#"
@prelude(core)
fn main() -> Bool = List.head(List.map([1, 2], |x| x > 0))
"#,
    );
}

#[test]
fn infer_list_map_string_result() {
    // List.map with a lambda that returns String should produce List<String>
    check_ok(
        r#"
@prelude(core)
fn main() -> () = {
    let names = List.map([1, 2, 3], |x| "item")
}
"#,
    );
}

#[test]
fn infer_list_fold_accumulator() {
    // List.fold([1,2,3], 0, |acc, x| acc + x) should return Int
    check_ok(
        r#"
@prelude(core)
fn main() -> Int = List.fold([1, 2, 3], 0, |acc, x| acc + x)
"#,
    );
}

#[test]
fn infer_list_tail_preserves_type() {
    // List.tail([1,2,3]) should return List<Int>
    check_ok(
        r#"
@prelude(core)
fn main() -> () = {
    let rest = List.tail([1, 2, 3])
    let first = List.head(rest)
    let sum = first + 1
}
"#,
    );
}

#[test]
fn infer_list_concat_types() {
    // List.concat([1,2], [3,4]) should return List<Int>
    check_ok(
        r#"
@prelude(core)
fn main() -> Int = {
    let combined = List.concat([1, 2], [3, 4])
    List.head(combined)
}
"#,
    );
}

#[test]
fn infer_result_unwrap_extracts_inner() {
    // Result.unwrap(Ok(42)) should return Int (nested call triggers STY_001 warning)
    check_no_errors(
        r#"
@prelude(core)
fn main() -> Int = {
    let x = Result.unwrap(Ok(42))
    x + 1
}
"#,
    );
}

#[test]
fn infer_list_reverse_preserves_type() {
    check_ok(
        r#"
@prelude(core)
fn main() -> Int = {
    let rev = List.reverse([1, 2, 3])
    List.head(rev)
}
"#,
    );
}

#[test]
fn infer_list_sort_preserves_type() {
    check_ok(
        r#"
@prelude(core)
fn main() -> Int = {
    let sorted = List.sort([3, 1, 2])
    List.head(sorted)
}
"#,
    );
}

// ============================================================
// Generic Type Enforcement Tests
// ============================================================

#[test]
fn generic_some_produces_concrete_option() {
    // Some(42) should produce Option<Int>, not Option<Unknown>
    check_no_errors(
        r#"
@prelude(core)
fn main() -> Int = Option.unwrap(Some(42))
"#,
    );
}

#[test]
fn generic_ok_produces_concrete_result() {
    // Ok("hello") should produce Result<String, E>, not Result<Unknown, Unknown>
    check_no_errors(
        r#"
@prelude(core)
fn main() -> String = Result.unwrap(Ok("hello"))
"#,
    );
}

#[test]
fn generic_some_match_extracts_concrete_type() {
    // match on Some(42) should bind x as Int
    check_no_errors(
        "@prelude(core)\n\
         fn main() -> Int = {\n\
           match Some(42)\n\
             Some(x) -> x + 1\n\
             None -> 0\n\
         }",
    );
}

#[test]
fn generic_ok_match_extracts_concrete_type() {
    // match on Ok(42) should bind x as Int
    check_no_errors(
        "@prelude(core)\n\
         fn main() -> Int = {\n\
           match Ok(42)\n\
             Ok(x) -> x + 1\n\
             Err(_) -> 0\n\
         }",
    );
}

#[test]
fn generic_option_type_mismatch_in_match() {
    // Option<Int> matched — using x (Int) where String expected should error
    check_has_error(
        "@prelude(core)\n\
         fn main() -> Int = {\n\
           let opt : Option<Int> = Some(42)\n\
           match opt\n\
             Some(x) -> String.length(x)\n\
             None -> 0\n\
         }",
        "TYP_008",
    );
}

#[test]
fn generic_enum_payload_compatibility_checked() {
    // types_compatible should reject Option<Int> vs Option<String>
    check_has_error(
        "@prelude(core)\n\
         fn identity(x: Option<String>) -> Option<String> = x\n\
         \n\
         fn main() -> Option<String> = identity(Some(42))",
        "TYP_008",
    );
}

#[test]
fn generic_list_map_result_type_enforced() {
    // List.map([1,2,3], |x| x > 0) returns List<Bool>, not assignable to List<Int>
    check_has_error(
        "@prelude(core)\n\
         fn sum(xs: List<Int>) -> Int = List.fold(xs, 0, |acc, x| acc + x)\n\
         \n\
         fn main() -> Int = {\n\
           let bools = List.map([1, 2, 3], |x| x > 0)\n\
           sum(bools)\n\
         }",
        "TYP_008",
    );
}

#[test]
fn generic_try_expression_preserves_concrete_type() {
    // Some(42)? should produce Int, usable in arithmetic
    check_ok(
        "@prelude(core)\n\
                  fn main() -> Option<Int> = {\n\
                    let val = Some(42)?\n\
                    Some(val + 1)\n\
                  }",
    );
}

#[test]
fn generic_err_produces_concrete_result() {
    check_no_errors(
        "@prelude(core)\n\
         fn main() -> String = {\n\
           match Err(\"oops\")\n\
             Ok(_) -> \"ok\"\n\
             Err(msg) -> msg\n\
         }",
    );
}

// ============================================================
// Transitive Effect Checking Tests
// ============================================================

#[test]
fn transitive_effect_direct_still_works() {
    // Direct ! call without declared effect still produces CAP_001
    check_has_error(
        "fn fetch!() -> String = Http.get!(\"http://example.com\")",
        "CAP_001",
    );
}

#[test]
fn transitive_effect_via_helper() {
    // helper calls Console.println! (direct effect), foo calls helper (transitive)
    // foo should get CAP_001 for missing {Console}
    let source = "\
        fn helper() -> {Console} () = Console.println!(\"hi\")\n\
        \n\
        fn foo() -> () = helper()";
    check_has_error(source, "CAP_001");
    // Verify the transitive diagnostic mentions 'transitively'
    let result = parse_source(source, "<test>");
    let transitive_diags: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.code == "CAP_001" && d.message.contains("transitively"))
        .collect();
    assert!(
        !transitive_diags.is_empty(),
        "Expected a transitive CAP_001 diagnostic, got: {:?}",
        result
            .diagnostics
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn transitive_effect_declared_ok() {
    // foo calls helper which needs Console, but foo declares {Console} — no error
    check_ok(
        "fn helper() -> {Console} () = Console.println!(\"hi\")\n\
                  \n\
                  fn foo() -> {Console} () = helper()",
    );
}

#[test]
fn transitive_effect_chain() {
    // a calls b (pure), b calls c, c calls Console.println!
    // Effect should propagate: c -> b -> a
    let source = "\
        fn c() -> {Console} () = Console.println!(\"deep\")\n\
        \n\
        fn b() -> () = c()\n\
        \n\
        fn a() -> () = b()";
    // Both a and b should get transitive CAP_001
    let result = parse_source(source, "<test>");
    let transitive_diags: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.code == "CAP_001" && d.message.contains("transitively"))
        .collect();
    assert!(
        transitive_diags.len() >= 2,
        "Expected at least 2 transitive CAP_001 diagnostics (for a and b), got {}: {:?}",
        transitive_diags.len(),
        transitive_diags
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn transitive_effect_mutual_recursion() {
    // ping calls pong, pong calls ping — mutual recursion should not infinite loop
    // Neither has effects, so no diagnostics should be emitted
    check_ok(
        "fn ping() -> () = pong()\n\
                  \n\
                  fn pong() -> () = ping()",
    );
}

// ============================================================
// Record Update Syntax Tests
// ============================================================

#[test]
fn record_update_basic() {
    check_no_errors(
        "type Point = { x: Int, y: Int }\n\
         fn update(p: Point) -> Point = { ..p, x: 42 }",
    );
}

#[test]
fn record_update_preserves_type() {
    // The result of a record update should have the same type as the base
    check_no_errors(
        "type Point = { x: Int, y: Int }\n\
         fn shift(p: Point) -> Int = {\n\
           let p2 = { ..p, x: 99 }\n\
           p2.x\n\
         }",
    );
}

#[test]
fn record_update_wrong_field_type() {
    check_has_error(
        "type Point = { x: Int, y: Int }\n\
         fn update(p: Point) -> Point = { ..p, x: \"hello\" }",
        "TYP_028",
    );
}

#[test]
fn record_update_nonexistent_field() {
    check_has_error(
        "type Point = { x: Int, y: Int }\n\
         fn update(p: Point) -> Point = { ..p, z: 42 }",
        "TYP_029",
    );
}
