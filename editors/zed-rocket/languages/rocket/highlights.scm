; Rocket syntax highlighting queries
; Based on tree-sitter highlight conventions

; ============================================================================
; Comments
; ============================================================================

(line_comment) @comment
(block_comment) @comment

; Doc comments get special treatment
((line_comment) @comment.documentation
  (#match? @comment.documentation "^///"))

; ============================================================================
; Literals
; ============================================================================

(integer_literal) @number
(float_literal) @number.float
(boolean_literal) @boolean
(char_literal) @character
(unit_literal) @constant.builtin

; Strings
(string_literal) @string
(escape_sequence) @string.escape
(interpolation
  (interpolation_start) @punctuation.special
  "}" @punctuation.special)

; ============================================================================
; Types
; ============================================================================

(primitive_type) @type.builtin
(type_identifier) @type
(type_parameters
  (upper_identifier) @type.parameter)

; Generic types
(generic_type
  (upper_identifier) @type)

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
  "try"
  "catch"
] @keyword

[
  "type"
  "effect"
  "@module"
  "import"
  "export"
  "as"
] @keyword.declaration

[
  "true"
  "false"
] @boolean

; ============================================================================
; Functions
; ============================================================================

; Function declarations - first identifier is the function name
(function_declaration
  (lower_identifier) @function.definition)

(function_declaration
  (effectful_identifier) @function.definition)

; Effect operations
(effect_operation
  (effectful_identifier) @function.method)

; Function calls - highlight the callable identifier
(call_expression
  (lower_identifier) @function.call)

(call_expression
  (qualified_identifier) @function.call)

; Lambda parameters - must come before generic variable captures
(lambda_parameter
  (identifier_pattern
    (lower_identifier) @variable.parameter))

(lambda_parameter
  (lower_identifier) @variable.parameter)

(lambda_expression
  "|" @punctuation.delimiter
  "|" @punctuation.delimiter)

; ============================================================================
; Variables and Identifiers
; ============================================================================

; Generic fallbacks - must come AFTER specific patterns
(lower_identifier) @variable
(upper_identifier) @type
(effectful_identifier) @function.effectful

; Pattern matching identifiers
(identifier_pattern
  (lower_identifier) @variable)

(wildcard_pattern) @variable.builtin

; Record fields
(record_field
  (lower_identifier) @property)

(record_field_expression
  (lower_identifier) @property)

(field_expression
  "." @punctuation.delimiter
  (lower_identifier) @property)

; ============================================================================
; Operators
; ============================================================================

[
  "+"
  "-"
  "*"
  "/"
  "%"
  "**"
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
  "<|"
  ">>"
  "<<"
  "?"
  "??"
  "++"
  ".."
] @operator

(type_annotation
  ":" @punctuation.delimiter)

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
; Attributes
; ============================================================================

(attribute
  "@" @attribute.delimiter
  (lower_identifier) @attribute)

(spec_declaration
  "@spec" @attribute)

(module_declaration
  "@module" @attribute)

; Spec attributes
[
  "@given"
  "@returns"
  "@requires"
  "@ensures"
  "@effects"
  "@pure"
  "@total"
] @attribute

; ============================================================================
; Effects
; ============================================================================

(effect_declaration
  "effect" @keyword.declaration
  (upper_identifier) @type.effect)

(effect_set
  "{" @punctuation.bracket
  "}" @punctuation.bracket)

(effect_reference
  (upper_identifier) @type.effect)

; ============================================================================
; Sum types (variants)
; ============================================================================

(variant
  (upper_identifier) @constructor)

(variant_pattern
  (upper_identifier) @constructor)

; ============================================================================
; Modules
; ============================================================================

(module_path
  (upper_identifier) @module)

(import_statement
  "import" @keyword.import)

(qualified_identifier
  (upper_identifier) @module)
