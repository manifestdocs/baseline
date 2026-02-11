/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

/**
 * Tree-sitter grammar for the Baseline programming language.
 *
 * This grammar implements the Baseline language specification v0.1.0.
 * Baseline is a fast, verifiable, LLM-native programming language.
 */

// Precedence levels (higher = binds tighter)
const PREC = {
  PIPE: 1,
  LOGICAL_OR: 2,
  LOGICAL_AND: 3,
  COMPARE: 4,
  ADDITIVE: 5,
  MULTIPLICATIVE: 6,
  RANGE: 7,
  UNARY: 8,
  CALL: 9,
  MEMBER: 10,
};

module.exports = grammar({
  name: 'baseline',

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
  ],

  externals: $ => [
    $.multiline_string_start,
    $.multiline_string_content,
    $.multiline_string_end,
    $.string_start,
    $.string_content,
    $.string_end,
    $.raw_string_start,
    $.raw_string_content,
    $.raw_string_end,
    $.raw_hash_string_start,
    $.raw_hash_string_content,
    $.raw_hash_string_end,
    $._error_sentinel,  // Never used in grammar; detects error recovery
    $._call_lparen,     // '(' that is NOT preceded by a newline (for call expressions)
  ],

  conflicts: $ => [
    // Ambiguity between () -> T as function_type vs tuple_type followed by ->
    [$.tuple_type, $.function_type],
    // Ambiguity between (expr) as tuple vs parenthesized
    [$.tuple_expression, $.parenthesized_expression],
    // Ambiguity between identifier as _name (expr) vs _pattern
    [$._name, $._pattern],
    // General ambiguity between expression and pattern (e.g. literals)
    [$._expression, $._pattern],
    // Ambiguity between () as tuple_expression vs tuple_pattern
    [$.tuple_expression, $.tuple_pattern],
    // type_identifier in expression context vs struct_expression start
    [$._expression, $.struct_expression],
    // type_identifier ( could be expression or constructor pattern
    [$._expression, $.constructor_pattern],
    // named_argument vs record_field_init (both are identifier : expression)
    [$.named_argument, $.record_field_init],
    // type_identifier in pattern could be nullary or start of constructor_pattern in matcher
    [$._pattern, $.constructor_pattern],
  ],

  rules: {
    source_file: $ => repeat($._top_level),

    _top_level: $ => choice(
      $.module_decl,
      $.prelude_decl, // @prelude(script)
      $.import_decl,
      $._definition,
      $.spec_block,
      $.inline_test,
      $.describe_block
    ),

    // --- Declarations ---

    // Spec 7.1: @module Name, allows optional @ to match examples
    module_decl: $ => seq(optional('@'), 'module', $.module_path),

    // Spec 6.7: @prelude(name)
    prelude_decl: $ => seq('@prelude', '(', $.identifier, ')'),

    import_decl: $ => seq(
      'import',
      field('path', $.import_path),
    ),

    import_path: $ => choice(
      seq($.type_identifier, repeat(seq('.', $.type_identifier)), '.', '{', commaSep1($.identifier), '}'),
      seq($.type_identifier, repeat(seq('.', $.type_identifier)), '.', '*'),
      seq($.type_identifier, repeat(seq('.', $.type_identifier))),
    ),

    _definition: $ => choice(
      $.function_def,
      $.type_def,
      $.effect_def
    ),

    // type Port = Int where self > 0
    // type Status = | Active | Inactive | Pending(String)
    type_def: $ => seq(
      optional('export'),
      'type', field('name', $.type_identifier),
      optional($.type_params),
      '=',
      choice(
        field('def', $.variant_list),
        seq(field('def', $._type_expr), optional($.refinement_clause))
      )
    ),

    // Sum type variants: | A | B(T) or A | B | C
    variant_list: $ => choice(
      repeat1(seq('|', $.variant)),
      seq($.variant, repeat1(seq('|', $.variant)))
    ),

    variant: $ => seq(
      field('name', $.type_identifier),
      optional(seq('(', commaSep1($._type_expr), ')'))
    ),

    // explicit node for highlighting
    refinement_clause: $ => seq('where', $.predicate),

    // fn fizzbuzz(n: Int) -> String = body
    // fn identity<T>(x: T) -> T = x
    function_def: $ => seq(
      optional('export'),
      'fn', field('name', $._name),
      optional($.type_params),
      '(', optional(field('params', $.param_list)), ')',
      optional(seq('->', optional(field('effects', $.effect_set)), field('return_type', $._type_expr))),
      '=', field('body', $._expression),
      optional($.where_block) // Inline tests
    ),

    param_list: $ => commaSep1($.param),
    param: $ => seq(field('name', $.identifier), ':', field('type', $._type_expr)),

    effect_def: $ => seq(
      optional('export'),
      'effect', $.type_identifier, '{', repeat($.function_signature), '}'
    ),

    // --- Specifications ---

    // @spec divide
    // @given numerator: Int, denominator: Int where denominator != 0
    // @returns Int
    // @ensures result * denominator <= numerator
    spec_block: $ => seq(
      repeat1($.spec_attribute),
      $._definition
    ),

    spec_attribute: $ => choice(
      $.spec_decl,
      $.given_clause,
      $.returns_clause,
      $.requires_clause,
      $.ensures_clause,
      $.assume_clause,
      $.pure_attribute,
      $.total_attribute
    ),

    // @spec function_name
    spec_decl: $ => seq('@spec', field('name', choice($.identifier, $.type_identifier))),

    // @given x: Int, y: Int where x > 0
    given_clause: $ => seq('@given', commaSep1($.param), optional(seq('where', field('condition', $._expression)))),

    // @returns Type
    returns_clause: $ => seq('@returns', field('type', $._type_expr)),

    // @requires condition
    requires_clause: $ => seq('@requires', field('condition', $._expression)),

    // @ensures condition (may reference 'result')
    ensures_clause: $ => seq('@ensures', field('condition', $._expression)),

    // @assume condition
    assume_clause: $ => seq('@assume', field('condition', $._expression)),

    // @pure — function has no effects
    pure_attribute: $ => '@pure',

    // @total — function terminates on all inputs
    total_attribute: $ => '@total',

    // --- Types ---

    _type_expr: $ => choice(
      $.type_identifier,
      $.generic_type,
      $.option_type,
      $.tuple_type,
      $.record_type,
      $.function_type
    ),

    type_signature: $ => seq(
      $._type_expr, '->',
      optional($.effect_set),
      $._type_expr
    ),

    generic_type: $ => seq($.type_identifier, '<', commaSep1($._type_expr), '>'),
    option_type: $ => prec.left(10, seq($._type_expr, '?')),
    tuple_type: $ => seq('(', commaSep($._type_expr), ')'),
    record_type: $ => seq('{', commaSep($.record_field_def), optional(seq(',', $.row_variable)), '}'),
    record_field_def: $ => seq($.identifier, ':', $._type_expr),
    row_variable: $ => seq('..', $.identifier),
    function_type: $ => seq('(', commaSep($._type_expr), ')', '->', $._type_expr),

    effect_set: $ => seq('{', commaSep1($.type_identifier), '}'),
    function_signature: $ => seq($._name, ':', $.type_signature),
    type_params: $ => seq('<', commaSep1($.type_identifier), '>'),
    type_annotation: $ => seq(':', $._type_expr),

    // --- Expressions ---

    _expression: $ => choice(
      $.pipe_expression,
      $.match_expression,
      $.if_expression,
      $.for_expression,
      $.with_expression,
      $.handle_expression,
      $.binary_expression,
      $.unary_expression,
      $.try_expression,
      $.call_expression,
      $.field_expression,
      $.range_expression,
      $.literal,
      $._name, // includes identifier and effect_identifier
      $.type_identifier, // for nullary constructors: Active, None
      $.list_expression,
      $.struct_expression, // User { id: 1 }
      $.tuple_expression,
      $.record_update,    // { ..record, field: newValue }
      $.record_expression,
      $.block,
      $.lambda,
      $.parenthesized_expression,
      $.hole_expression,
      $.expect_expression,
      $.map_literal,
      $.set_literal
    ),

    // [1, 2, 3]
    list_expression: $ => seq('[', commaSep($._expression), ']'),

    // #{ key: value, ... }
    map_literal: $ => seq('#{', commaSep($.map_entry), '}'),
    map_entry: $ => seq(field('key', $._expression), ':', field('value', $._expression)),

    // #{ value1, value2, ... } — at least 1 element to avoid ambiguity with empty map #{}
    set_literal: $ => seq('#{', commaSep1($._expression), '}'),

    // x |> f
    pipe_expression: $ => prec.left(PREC.PIPE, seq($._expression, '|>', $._expression)),

    // match x ...
    match_expression: $ => prec.right(seq('match', $._expression, repeat1($.match_arm))),
    match_arm: $ => seq($._pattern, '->', $._expression),

    // if cond then x else y
    if_expression: $ => prec.right(seq(
      'if', $._expression, 'then', $._expression,
      optional(seq('else', $._expression))
    )),

    // for x in iter do ...
    for_expression: $ => seq('for', $._pattern, 'in', $._expression, 'do', $._expression),

    // with Console.println! { body } — capture effect calls for testing
    // with { Console: handler } body — effect handler map
    with_expression: $ => prec.right(choice(
      seq('with', field('effect', $.field_expression), field('body', $.block)),
      seq('with', field('handlers', $.handler_map), field('body', $._expression)),
    )),

    handler_map: $ => seq('{', commaSep1($.handler_binding), optional(','), '}'),
    handler_binding: $ => seq(field('effect', $.type_identifier), ':', field('handler', $._expression)),

    // handle expr with { Effect.method!(args) -> resume(result) }
    handle_expression: $ => prec.right(seq(
      'handle',
      field('body', $._expression),
      'with',
      '{',
      repeat1($.handler_clause),
      '}',
    )),

    handler_clause: $ => seq(
      field('effect', $.type_identifier),
      '.',
      field('method', choice($.effect_identifier, $.identifier)),
      '(',
      commaSep($.identifier),
      ')',
      '->',
      field('handler_body', $._expression),
    ),

    // req.params.id or Log.info!
    field_expression: $ => prec.left(PREC.MEMBER, seq(
      $._expression,
      '.',
      choice($.identifier, $.effect_identifier)
    )),

    // f(x, y) or f(name: "Alice", age: 30)
    // $._call_lparen is an external token that matches '(' only when NO newline
    // appears between it and the preceding token. This prevents newline-separated
    // expressions from being parsed as call arguments: `42\n(a, b)` is NOT a call.
    call_expression: $ => prec.left(PREC.CALL, seq(
      $._expression, $._call_lparen, optional(commaSep(choice($.named_argument, $._expression))), ')'
    )),

    named_argument: $ => seq(field('name', $.identifier), ':', $._expression),

    // 1..100
    range_expression: $ => prec.left(PREC.RANGE, seq($._expression, '..', $._expression)),

    // Arithmetic & Logic
    binary_expression: $ => choice(
      prec.left(PREC.ADDITIVE, seq($._expression, choice('+', '-', '++'), $._expression)),
      prec.left(PREC.MULTIPLICATIVE, seq($._expression, choice('*', '/', '%'), $._expression)),
      prec.left(PREC.COMPARE, seq($._expression, choice('==', '!=', '<', '>', '<=', '>='), $._expression)),
      prec.left(PREC.LOGICAL_AND, seq($._expression, '&&', $._expression)),
      prec.left(PREC.LOGICAL_OR, seq($._expression, '||', $._expression))
    ),

    unary_expression: $ => prec(PREC.UNARY, seq(choice('not', '-'), $._expression)),

    // expr? — unwrap Option/Result or propagate error
    try_expression: $ => prec.left(PREC.CALL, seq($._expression, '?')),

    // Structures
    tuple_expression: $ => seq('(', commaSep($._expression), ')'),

    // User { id: 1 }
    struct_expression: $ => seq($.type_identifier, '{', commaSep($.record_field_init), '}'),

    // { id: 1 } (Anonymous)
    record_expression: $ => seq('{', commaSep($.record_field_init), '}'),
    record_field_init: $ => seq($.identifier, ':', $._expression),

    // { ..record, field: newValue }
    record_update: $ => seq('{', '..', $._expression, repeat(seq(',', $.record_field_init)), optional(','), '}'),

    // Lambdas: |x| x + 1
    lambda: $ => seq('|', commaSep($._pattern), '|', $._expression),

    block: $ => seq('{', repeat1(seq($._statement, optional(';'))), '}'),
    _statement: $ => choice(
      $.let_binding,
      $._expression
    ),
    let_binding: $ => seq('let', $._pattern, optional(field('type', $.type_annotation)), '=', $._expression),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    // ?? — typed hole placeholder
    hole_expression: $ => '??',

    // --- Patterns ---

    _pattern: $ => choice(
      $.identifier,
      $.type_identifier, // Matches nullary constructors: None, Active
      $.constructor_pattern, // Matches constructors with payload: Some(v)
      $.literal,
      $.tuple_pattern,
      $.wildcard_pattern
    ),
    constructor_pattern: $ => seq($.type_identifier, '(', commaSep($._pattern), ')'),
    tuple_pattern: $ => seq('(', commaSep($._pattern), ')'),
    wildcard_pattern: $ => '_',

    // --- Refinements & Testing ---

    predicate: $ => $._expression,

    where_block: $ => prec.right(seq('where', repeat1($.inline_test))),
    inline_test: $ => seq('test', $.string_literal, '=', $._expression),

    // --- BDD Testing ---

    describe_block: $ => seq(
      choice('describe', 'context'),
      field('name', $.string_literal),
      '{',
      repeat($._describe_item),
      '}'
    ),

    _describe_item: $ => choice(
      $.it_block,
      $.describe_block,     // nested describe/context
      $.before_each_block,
      $.after_each_block,
      $.inline_test,        // regular test blocks work inside describe
      $._definition,        // helper functions
      $.let_binding,        // shared bindings
    ),

    it_block: $ => seq(
      'it',
      optional(field('modifier', choice('.only', '.skip'))),
      field('name', $.string_literal),
      '=',
      field('body', $._expression)
    ),

    before_each_block: $ => seq('before_each', '=', $._expression),
    after_each_block: $ => seq('after_each', '=', $._expression),

    // expect expression with matchers
    expect_expression: $ => prec.right(seq(
      'expect',
      field('actual', $._expression),
      field('matcher', $.matcher)
    )),

    matcher: $ => choice(
      seq('to_equal', $._expression),
      seq('to_be', $._pattern),
      seq('to_contain', $._expression),
      seq('to_have_length', $._expression),
      'to_be_empty',
      seq('to_start_with', $._expression),
      seq('to_satisfy', $._expression),
      'to_be_ok',
      'to_be_some',
      'to_be_none',
    ),

    // --- Literals ---

    literal: $ => choice($.float_literal, $.integer_literal, $.multiline_string_literal, $.string_literal, $.raw_string_literal, $.raw_hash_string_literal, $.boolean_literal),
    float_literal: $ => token(seq(/\d+/, '.', /\d+/, optional(seq(/[eE]/, optional(/[+-]/), /\d+/)))),
    integer_literal: $ => token(choice(
      /0[xX][0-9a-fA-F][0-9a-fA-F_]*/,   // hex: 0xFF, 0x1A_2B
      /0[bB][01][01_]*/,                   // binary: 0b1010, 0b1111_0000
      /0[oO][0-7][0-7_]*/,                 // octal: 0o755, 0o77_77
      /[0-9][0-9_]*/,                      // decimal: 42, 1_000_000
    )),
    boolean_literal: $ => choice('true', 'false'),

    // Recursive Interpolation: "Hello ${name}"
    string_literal: $ => seq(
      $.string_start,
      repeat(choice(
        $.string_content,
        $.escape_sequence,
        $.interpolation
      )),
      $.string_end
    ),
    // Triple-quoted multi-line strings: """..."""
    multiline_string_literal: $ => seq(
      $.multiline_string_start,
      repeat(choice(
        $.multiline_string_content,
        $.escape_sequence,
        $.interpolation
      )),
      $.multiline_string_end,
    ),
    interpolation: $ => seq('${', $._expression, '}'),
    escape_sequence: $ => token.immediate(seq('\\', /./)),

    // Raw strings: r"..." (no escapes, no interpolation)
    raw_string_literal: $ => seq(
      $.raw_string_start,
      optional($.raw_string_content),
      $.raw_string_end
    ),
    // Raw hash strings: r#"..."# (can contain unescaped quotes)
    raw_hash_string_literal: $ => seq(
      $.raw_hash_string_start,
      optional($.raw_hash_string_content),
      $.raw_hash_string_end
    ),

    // --- Identifiers ---

    module_path: $ => prec.right(seq($.type_identifier, repeat(seq('.', $.type_identifier)))),

    identifier: $ => /[a-z_][a-z0-9_]*/,
    effect_identifier: $ => token(seq(/[a-z_][a-z0-9_]*/, '!')),
    _name: $ => choice($.identifier, $.effect_identifier),
    type_identifier: $ => /[A-Z][a-zA-Z0-9]*/,

    // --- Comments ---
    line_comment: $ => token(seq('//', /.*/)),
    block_comment: $ => token(seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')),
  }
});

function commaSep(rule) {
  return optional(seq(rule, repeat(seq(',', rule)), optional(',')));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)), optional(','));
}
