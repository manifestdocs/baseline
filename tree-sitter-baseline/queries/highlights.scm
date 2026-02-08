; Baseline syntax highlighting queries

; ============================================================================
; Comments
; ============================================================================

(line_comment) @comment
(block_comment) @comment

; ============================================================================
; Literals
; ============================================================================

(integer_literal) @number
(float_literal) @number.float
(boolean_literal) @boolean

; Strings
(string_literal) @string
(escape_sequence) @string.escape
(interpolation
  "${" @punctuation.special
  "}" @punctuation.special)

; ============================================================================
; Types
; ============================================================================

(type_identifier) @type
(type_params
  (type_identifier) @type.parameter)

; Generic types
(generic_type
  (type_identifier) @type)

; ============================================================================
; Keywords
; ============================================================================

[
  "let"
  "match"
  "if"
  "then"
  "else"
  "where"
  "for"
  "in"
  "do"
  "test"
] @keyword

[
  "fn"
  "type"
  "effect"
  "export"
] @keyword.declaration

[
  "true"
  "false"
] @boolean

; ============================================================================
; Functions
; ============================================================================

; Function definitions
(function_def
  name: (identifier) @function)

(function_def
  name: (effect_identifier) @function.effectful)

; Effect operations
(function_signature
  (effect_identifier) @function.method)

; Function calls - highlight the callable identifier
(call_expression
  (identifier) @function.call)

; Function parameters
(param
  name: (identifier) @variable.parameter)

; Return type arrow in function_def
(function_def
  "->" @punctuation.delimiter)

; Lambda
(lambda
  "|" @punctuation.delimiter)

; ============================================================================
; Variables and Identifiers
; ============================================================================

(identifier) @variable
(effect_identifier) @function.effectful

; Wildcard pattern
(wildcard_pattern) @variable.builtin

; Record fields
(record_field_def
  (identifier) @property)

(record_field_init
  (identifier) @property)

(field_expression
  "." @punctuation.delimiter
  (identifier) @property)

; ============================================================================
; Operators
; ============================================================================

[
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "&&"
  "||"
  "!"
  "|>"
  ".."
] @operator

(function_type
  "->" @punctuation.delimiter)

(match_arm
  "->" @punctuation.delimiter)

; ============================================================================
; Punctuation
; ============================================================================

["(" ")" "[" "]" "{" "}"] @punctuation.bracket
["," ";"] @punctuation.delimiter
"." @punctuation.delimiter
"|" @punctuation.delimiter
"=" @operator

; ============================================================================
; Effects
; ============================================================================

(effect_def
  "effect" @keyword.declaration
  (type_identifier) @type.effect)

(effect_set
  "{" @punctuation.bracket
  "}" @punctuation.bracket)

; ============================================================================
; Sum types (variants)
; ============================================================================

(variant
  name: (type_identifier) @constructor)

(constructor_pattern
  (type_identifier) @constructor)

; ============================================================================
; Modules
; ============================================================================

(module_decl) @keyword
(prelude_decl) @keyword
