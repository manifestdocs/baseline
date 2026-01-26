/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

/**
 * Tree-sitter grammar for the Rocket programming language.
 *
 * This grammar implements the Rocket language specification v0.1.0.
 * Rocket is a fast, verifiable, LLM-native programming language.
 */

const PREC = {
  // From lowest to highest
  PIPE: 1,
  OR: 2,
  AND: 3,
  COMPARE: 4,
  CONCAT: 5,
  ADD: 6,
  MUL: 7,
  EXP: 8,
  UNARY: 9,
  POSTFIX: 10,
  CALL: 11,
  FIELD: 12,
};

module.exports = grammar({
  name: "rocket",

  extras: ($) => [/\s/, $.line_comment, $.block_comment],

  externals: ($) => [$.string_content, $.interpolation_start],

  word: ($) => $.lower_identifier,

  conflicts: ($) => [
    [$._type_expression, $._expression],
    [$._primary_expression, $.variant_pattern],
    [$.record_expression, $.record_pattern],
    [$.function_declaration, $.type_annotation],
    [$.variant, $.type_identifier],
    [$._type_body, $._type_expression],
    [$.record_expression, $.block_expression],
    [$._identifier, $.record_field_expression],
    [$._expression, $._block_statement],
    [$.lambda_parameter, $.or_pattern],
    [$.function_type, $.tuple_type],
    [$.function_type, $.effect_type],
    [$._expression, $.lambda_expression],
    [$._type_expression, $.tuple_type],
    [$._type_expression, $.function_type, $.tuple_type],
    [$.optional_type, $.result_type],
    [$.tuple_pattern, $.parenthesized_pattern],
    [$._literal, $.literal_pattern],
    [$._identifier, $.identifier_pattern],
    [$._identifier, $.variant_pattern],
    [$.argument, $.wildcard_pattern],
    [$.function_type],
    [$.record_expression, $.block_expression, $.record_pattern],
    [$.record_field_expression, $.record_field_pattern],
    [$._identifier, $.record_field_expression, $.record_field_pattern],
    [$.list_expression, $.list_pattern],
    [$.type_identifier, $._identifier],
    [$.record_type, $.record_expression, $.block_expression],
    [$.effect_reference, $._identifier],
    [$.effect_reference, $.qualified_identifier],
    [$.type_identifier, $.upper_identifier],
    [$.row_extension, $._identifier],
    [$.record_update, $._identifier],
    // BDD testing conflicts
    [$.test_fixture, $.let_expression],
    [$.effect_binding, $.record_field_expression],
    [$._identifier, $.constructor_expression],
  ],

  rules: {
    // =========================================================================
    // Source file
    // =========================================================================

    source_file: ($) =>
      seq(
        optional($.module_declaration),
        repeat($.import_statement),
        repeat($._declaration)
      ),

    // =========================================================================
    // Module system
    // =========================================================================

    module_declaration: ($) =>
      seq(
        optional($.attribute),
        "@module",
        $.module_path
      ),

    module_path: ($) =>
      prec.left(seq($.upper_identifier, repeat(seq(".", $.upper_identifier)))),

    import_statement: ($) =>
      seq(
        "import",
        $.module_path,
        optional($.import_specifier)
      ),

    import_specifier: ($) =>
      choice(
        seq(".", "{", commaSep1($._identifier), "}"),
        seq(".", "*"),
        seq("as", $.upper_identifier)
      ),

    // =========================================================================
    // Declarations
    // =========================================================================

    _declaration: ($) =>
      choice(
        $.type_declaration,
        $.effect_declaration,
        $.function_declaration,
        $.spec_declaration,
        $.describe_block
      ),

    // Type declarations
    type_declaration: ($) =>
      seq(
        optional("export"),
        "type",
        $.upper_identifier,
        optional($.type_parameters),
        "=",
        $._type_body,
        optional($.where_clause)
      ),

    type_parameters: ($) =>
      seq("<", commaSep1($.upper_identifier), ">"),

    _type_body: ($) =>
      choice($.variant_list, $.record_type, $._type_expression),

    variant_list: ($) =>
      seq(optional("|"), $.variant, repeat(seq("|", $.variant))),

    variant: ($) =>
      seq(
        $.upper_identifier,
        optional(seq("(", commaSep1($._type_expression), ")"))
      ),

    record_type: ($) =>
      seq(
        "{",
        optional(seq(commaSep1($.record_field), optional(","))),
        optional($.row_extension),
        "}"
      ),

    record_field: ($) =>
      seq(
        $.lower_identifier,
        ":",
        $._type_expression,
        optional($.refinement)
      ),

    row_extension: ($) => seq("..", $.lower_identifier),

    refinement: ($) => seq("where", $._expression),

    where_clause: ($) => seq("where", alias($._expression, $.refinement)),

    // Effect declarations
    effect_declaration: ($) =>
      seq(
        optional("export"),
        "effect",
        $.upper_identifier,
        optional($.type_parameters),
        "{",
        repeat($.effect_operation),
        "}"
      ),

    effect_operation: ($) =>
      seq(
        $.effectful_identifier,
        ":",
        $._type_expression
      ),

    // Function declarations
    // Supports both single-line `f : T = body` and ML-style `f : T` then `f = body`
    function_declaration: ($) =>
      seq(
        repeat($.attribute),
        optional("export"),
        choice($.lower_identifier, $.effectful_identifier),
        optional($.type_annotation),
        optional(choice($.lower_identifier, $.effectful_identifier)),  // Repeated name (ML-style)
        "=",
        $._expression,
        optional($.function_where_clause)
      ),

    type_annotation: ($) => seq(":", $._type_expression),

    function_where_clause: ($) =>
      seq("where", repeat1(choice($.test_block, $.property_block))),

    test_block: ($) =>
      seq("test", $.string_literal, "=", $._expression),

    property_block: ($) =>
      seq("property", $.string_literal, "=", $._expression),

    // Specification declarations
    spec_declaration: ($) =>
      seq(
        "@spec",
        $._identifier,
        repeat($.spec_attribute)
      ),

    spec_attribute: ($) =>
      choice(
        seq("@given", $.parameter_list),
        seq("@returns", $._type_expression),
        seq("@requires", $._expression),
        seq("@ensures", $._expression),
        seq("@effects", $.effect_set),
        "@pure",
        "@total"
      ),

    parameter_list: ($) =>
      commaSep1(seq($.lower_identifier, ":", $._type_expression, optional($.refinement))),

    // =========================================================================
    // BDD Testing
    // =========================================================================

    // describe "Name" { ... }
    describe_block: ($) =>
      seq(
        "describe",
        $.string_literal,
        "{",
        repeat($._describe_member),
        "}"
      ),

    _describe_member: ($) =>
      choice(
        $.describe_block,
        $.context_block,
        $.it_block,
        $.property_test,
        $.test_fixture,
        $.before_all_hook,
        $.before_each_hook,
        $.after_each_hook,
        $.after_all_hook
      ),

    // context "when something" { ... }
    context_block: ($) =>
      seq(
        "context",
        $.string_literal,
        "{",
        repeat($._describe_member),
        "}"
      ),

    // it "does something" { given ... when ... expect ... }
    // it.only "focused" { ... }
    // it.skip "pending" { ... }
    // it.skip("reason") "pending" { ... }
    it_block: ($) =>
      seq(
        "it",
        optional($.it_modifier),
        $.string_literal,
        "{",
        optional($.with_clause),
        optional($.given_clause),
        optional($.when_clause),
        repeat1($.expect_statement),
        "}",
        optional($.examples_block)
      ),

    it_modifier: ($) =>
      choice(
        ".only",
        ".skip",
        seq(".skip", "(", $.string_literal, ")")
      ),

    // with { http: mock_http, db: mock_db }
    with_clause: ($) =>
      seq(
        "with",
        "{",
        commaSep1($.effect_binding),
        "}",
        optional(",")
      ),

    effect_binding: ($) =>
      seq($.lower_identifier, ":", $._expression),

    // given expression or { record }
    given_clause: ($) =>
      seq("given", $._expression),

    // when function_name or |x| expression
    when_clause: ($) =>
      seq("when", choice($._identifier, $.qualified_identifier, $.lambda_expression)),

    // expect expression [matcher]
    // expect value to_equal 5
    // expect result to_be Ok(_)
    expect_statement: ($) =>
      seq(
        "expect",
        $._expression,
        optional($.matcher)
      ),

    matcher: ($) =>
      seq(
        $.matcher_name,
        optional($._expression)
      ),

    matcher_name: ($) =>
      choice(
        "to_equal",
        "to_be",
        "to_be_ok",
        "to_be_err",
        "to_be_some",
        "to_be_none",
        "to_be_empty",
        "to_be_type",
        "to_contain",
        "to_have_length",
        "to_start_with",
        "to_end_with",
        "to_match",
        "to_satisfy",
        "to_be_greater_than",
        "to_be_less_than",
        "to_be_between"
      ),

    // Table-driven tests: examples { (col1, col2) => (val1, val2) ... }
    // Alternative tuple-based syntax to avoid | conflicts
    examples_block: ($) =>
      seq(
        "examples",
        "{",
        $.examples_header,
        repeat1($.examples_row),
        "}"
      ),

    examples_header: ($) =>
      seq("(", commaSep1($.lower_identifier), ")"),

    examples_row: ($) =>
      seq("(", commaSep1($._expression), ")"),

    // let fixture = value (inside describe)
    test_fixture: ($) =>
      seq("let", $.lower_identifier, "=", $._expression),

    // Hooks
    before_all_hook: ($) =>
      seq("before_all", $.block_expression),

    before_each_hook: ($) =>
      seq("before_each", $.block_expression),

    after_each_hook: ($) =>
      seq("after_each", $.block_expression),

    after_all_hook: ($) =>
      seq("after_all", $.block_expression),

    // property "name" { forall x: Type ... expect ... }
    property_test: ($) =>
      seq(
        "property",
        $.string_literal,
        choice(
          // Short form: property "name" = expression
          seq("=", $._expression),
          // Block form with forall
          seq(
            "{",
            $.forall_clause,
            optional($.where_constraint),
            $.expect_statement,
            "}"
          )
        )
      ),

    forall_clause: ($) =>
      seq(
        "forall",
        commaSep1($.forall_binding)
      ),

    forall_binding: ($) =>
      seq($.lower_identifier, ":", $._type_expression),

    where_constraint: ($) =>
      seq("where", $._expression),

    // =========================================================================
    // Type expressions
    // =========================================================================

    _type_expression: ($) =>
      choice(
        $.primitive_type,
        $.type_identifier,
        $.generic_type,
        $.function_type,
        $.tuple_type,
        $.record_type,
        $.optional_type,
        $.result_type,
        $.effect_type,
        seq("(", $._type_expression, ")")
      ),

    primitive_type: ($) =>
      choice("Bool", "Int", "Float", "Char", "String", "Unit", "Never"),

    type_identifier: ($) => /[A-Z][a-zA-Z0-9_]*/,

    generic_type: ($) =>
      prec(PREC.CALL, seq($.upper_identifier, "<", commaSep1($._type_expression), ">")),

    function_type: ($) =>
      prec.right(
        seq(
          optional($.effect_set),
          $._type_expression,
          "->",
          $._type_expression
        )
      ),

    tuple_type: ($) =>
      seq("(", commaSep($._type_expression), ")"),

    optional_type: ($) =>
      prec(PREC.POSTFIX, seq($._type_expression, "?")),

    result_type: ($) =>
      prec.left(PREC.POSTFIX, seq($._type_expression, "!", optional($._type_expression))),

    effect_type: ($) =>
      seq($.effect_set, $._type_expression),

    effect_set: ($) =>
      seq("{", commaSep1($.effect_reference), "}"),

    effect_reference: ($) =>
      seq($.upper_identifier, optional(seq(".", $.lower_identifier))),

    // =========================================================================
    // Expressions
    // =========================================================================

    _expression: ($) =>
      choice(
        $._primary_expression,
        $.unary_expression,
        $.binary_expression,
        $.postfix_expression,
        $.pipe_expression,
        $.if_expression,
        $.match_expression,
        $.let_expression,
        $.block_expression,
        $.lambda_expression,
        $.for_expression,
        $.try_expression
      ),

    _primary_expression: ($) =>
      choice(
        $._literal,
        $._identifier,
        $.qualified_identifier,
        $.parenthesized_expression,
        $.tuple_expression,
        $.list_expression,
        $.record_expression,
        $.constructor_expression,
        $.call_expression,
        $.field_expression
      ),

    // Literals
    _literal: ($) =>
      choice(
        $.integer_literal,
        $.float_literal,
        $.string_literal,
        $.char_literal,
        $.boolean_literal,
        $.unit_literal
      ),

    integer_literal: ($) =>
      token(
        choice(
          seq(optional("-"), /[0-9]/, repeat(/[0-9_]/)),
          seq("0x", /[0-9a-fA-F]/, repeat(/[0-9a-fA-F_]/)),
          seq("0b", /[01]/, repeat(/[01_]/)),
          seq("0o", /[0-7]/, repeat(/[0-7_]/)
          )
        )
      ),

    float_literal: ($) =>
      token(
        seq(
          optional("-"),
          /[0-9]+/,
          ".",
          /[0-9]+/,
          optional(seq(/[eE]/, optional(/[+-]/), /[0-9]+/))
        )
      ),

    string_literal: ($) =>
      choice(
        $._simple_string,
        $._raw_string,
        $._multiline_string
      ),

    _simple_string: ($) =>
      seq(
        '"',
        repeat(choice($.string_content, $.escape_sequence, $.interpolation)),
        '"'
      ),

    _raw_string: ($) =>
      choice(
        seq('r"', /[^"]*/, '"'),
        seq('r#"', /[^"#]*/, '"#')
      ),

    _multiline_string: ($) =>
      seq(
        '"""',
        repeat(choice($.string_content, $.escape_sequence, $.interpolation)),
        '"""'
      ),

    escape_sequence: ($) =>
      token(
        seq(
          "\\",
          choice(
            /[nrt\\"${]/,
            seq("u{", /[0-9a-fA-F]+/, "}")
          )
        )
      ),

    interpolation: ($) =>
      seq($.interpolation_start, $._expression, "}"),

    char_literal: ($) =>
      seq("'", choice(/[^'\\]/, $.escape_sequence), "'"),

    boolean_literal: ($) => choice("true", "false"),

    unit_literal: ($) => prec(-1, "()"),

    // Identifiers
    _identifier: ($) => choice($.lower_identifier, $.upper_identifier),

    lower_identifier: ($) => /[a-z_][a-zA-Z0-9_]*/,

    upper_identifier: ($) => /[A-Z][a-zA-Z0-9_]*/,

    effectful_identifier: ($) => /[a-z_][a-zA-Z0-9_]*!/,

    qualified_identifier: ($) =>
      prec(PREC.FIELD, seq(
        $.upper_identifier,
        repeat1(seq(".", choice($.lower_identifier, $.upper_identifier, $.effectful_identifier)))
      )),

    // Compound expressions
    parenthesized_expression: ($) => seq("(", $._expression, ")"),

    tuple_expression: ($) =>
      seq("(", $._expression, ",", commaSep($._expression), ")"),

    list_expression: ($) =>
      seq(
        "[",
        optional(
          choice(
            commaSep1($._expression),
            seq($.spread_element, optional(seq(",", commaSep1($._expression)))),
            seq(commaSep1($._expression), ",", $.spread_element)
          )
        ),
        optional(","),
        "]"
      ),

    spread_element: ($) => seq("..", $._expression),

    record_expression: ($) =>
      seq(
        "{",
        optional(
          choice(
            // Fields first, then optional update: { name: "Alice", ..user }
            seq(
              commaSep1($.record_field_expression),
              optional(","),
              optional($.record_update)
            ),
            // Update first, then fields: { ..user, age: 31 }
            seq(
              $.record_update,
              optional(seq(",", commaSep1($.record_field_expression))),
              optional(",")
            )
          )
        ),
        "}"
      ),

    record_field_expression: ($) =>
      seq($.lower_identifier, optional(seq(":", $._expression))),

    record_update: ($) => seq("..", $._expression),

    // Constructor expression: TypeName { field: value, ... }
    constructor_expression: ($) =>
      seq(
        $.upper_identifier,
        "{",
        optional(seq(
          commaSep1($.record_field_expression),
          optional(",")
        )),
        "}"
      ),

    // Operations
    unary_expression: ($) =>
      prec(PREC.UNARY, choice(
        seq("-", $._expression),
        seq("!", $._expression)
      )),

    binary_expression: ($) =>
      choice(
        prec.left(PREC.OR, seq($._expression, "||", $._expression)),
        prec.left(PREC.AND, seq($._expression, "&&", $._expression)),
        prec.left(PREC.COMPARE, seq($._expression, choice("==", "!=", "<", ">", "<=", ">="), $._expression)),
        prec.left(PREC.CONCAT, seq($._expression, "++", $._expression)),
        prec.left(PREC.ADD, seq($._expression, choice("+", "-"), $._expression)),
        prec.left(PREC.MUL, seq($._expression, choice("*", "/", "%"), $._expression)),
        prec.right(PREC.EXP, seq($._expression, "**", $._expression)),
        prec.left(PREC.COMPARE, seq($._expression, "??", $._expression))
      ),

    postfix_expression: ($) =>
      prec(PREC.POSTFIX, choice(
        seq($._expression, "?"),
        seq($._expression, "!")
      )),

    pipe_expression: ($) =>
      prec.left(PREC.PIPE, choice(
        seq($._expression, "|>", $._expression),
        seq($._expression, "<|", $._expression),
        seq($._expression, ">>", $._expression),
        seq($._expression, "<<", $._expression)
      )),

    call_expression: ($) =>
      prec.left(PREC.CALL, seq(
        $._expression,
        "(",
        optional(commaSep1($.argument)),
        ")"
      )),

    argument: ($) =>
      choice(
        $._expression,
        seq($.lower_identifier, ":", $._expression),
        "_"
      ),

    field_expression: ($) =>
      prec.left(PREC.FIELD, seq(
        $._expression,
        ".",
        choice($.lower_identifier, $.integer_literal)
      )),

    // Control flow
    if_expression: ($) =>
      prec.right(seq(
        "if",
        $._expression,
        "then",
        $._expression,
        optional(seq("else", $._expression))
      )),

    match_expression: ($) =>
      prec.right(seq(
        "match",
        $._expression,
        repeat1($.match_arm)
      )),

    match_arm: ($) =>
      seq(
        $._pattern,
        optional(seq("if", $._expression)),
        "->",
        $._expression
      ),

    let_expression: ($) =>
      prec.right(seq(
        "let",
        $._pattern,
        optional($.type_annotation),
        "=",
        $._expression,
        optional(seq("in", $._expression))
      )),

    block_expression: ($) =>
      seq("{", repeat($._block_statement), optional($._expression), "}"),

    _block_statement: ($) =>
      choice(
        seq($.let_expression, ";"),
        seq($._expression, ";")
      ),

    lambda_expression: ($) =>
      prec.right(seq(
        "|",
        optional(commaSep1($.lambda_parameter)),
        "|",
        optional(seq("->", $._type_expression)),
        choice($._expression, $.block_expression)
      )),

    lambda_parameter: ($) =>
      choice(
        $._pattern,
        seq($.lower_identifier, ":", $._type_expression)
      ),

    for_expression: ($) =>
      seq(
        "for",
        $._pattern,
        "in",
        $._expression,
        "do",
        $._expression
      ),

    try_expression: ($) =>
      prec.right(seq(
        "try",
        $._expression,
        "catch",
        repeat1($.match_arm)
      )),

    // =========================================================================
    // Patterns
    // =========================================================================

    _pattern: ($) =>
      choice(
        $.wildcard_pattern,
        $.literal_pattern,
        $.identifier_pattern,
        $.tuple_pattern,
        $.list_pattern,
        $.record_pattern,
        $.variant_pattern,
        $.or_pattern,
        $.parenthesized_pattern
      ),

    wildcard_pattern: ($) => "_",

    literal_pattern: ($) =>
      choice(
        $.integer_literal,
        $.float_literal,
        $.string_literal,
        $.char_literal,
        $.boolean_literal
      ),

    identifier_pattern: ($) => $.lower_identifier,

    tuple_pattern: ($) =>
      seq("(", commaSep($._pattern), ")"),

    list_pattern: ($) =>
      seq(
        "[",
        optional(
          choice(
            commaSep1($._pattern),
            seq(commaSep1($._pattern), ",", $.rest_pattern)
          )
        ),
        "]"
      ),

    rest_pattern: ($) => seq("..", optional($.lower_identifier)),

    record_pattern: ($) =>
      seq(
        "{",
        optional(seq(
          commaSep1($.record_field_pattern),
          optional(","),
          optional(seq("..", optional($.lower_identifier)))
        )),
        "}"
      ),

    record_field_pattern: ($) =>
      seq(
        $.lower_identifier,
        optional(seq(":", $._pattern))
      ),

    variant_pattern: ($) =>
      seq(
        $.upper_identifier,
        optional(seq("(", commaSep1($._pattern), ")"))
      ),

    or_pattern: ($) =>
      prec.left(seq($._pattern, "|", $._pattern)),

    parenthesized_pattern: ($) =>
      seq("(", $._pattern, ")"),

    // =========================================================================
    // Attributes
    // =========================================================================

    attribute: ($) =>
      seq(
        "@",
        $.lower_identifier,
        optional($.attribute_arguments)
      ),

    attribute_arguments: ($) =>
      seq("(", commaSep($.attribute_argument), ")"),

    attribute_argument: ($) =>
      choice(
        $._literal,
        $._identifier,
        seq($.lower_identifier, ":", $._literal)
      ),

    // =========================================================================
    // Comments
    // =========================================================================

    line_comment: ($) =>
      token(choice(
        seq("//", /.*/),
        seq("///", /.*/)
      )),

    block_comment: ($) =>
      token(seq("/*", /[^*]*\*+([^/*][^*]*\*+)*/, "/"))
  },
});

/**
 * Creates a comma-separated list with at least one element.
 * @param {Rule} rule
 */
function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

/**
 * Creates a comma-separated list (possibly empty).
 * @param {Rule} rule
 */
function commaSep(rule) {
  return optional(commaSep1(rule));
}
