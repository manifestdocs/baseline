; Baseline syntax highlighting queries
; Matches tree-sitter-baseline grammar.js node types

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

; Strings
(string_literal) @string
(escape_sequence) @string.escape


; ============================================================================
; Types
; ============================================================================

(type_identifier) @type

(type_params
  (type_identifier) @type.parameter)

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
  "type"
  "effect"
  "module"
  "import"
  "export"
] @keyword.declaration

[
  "true"
  "false"
] @boolean

; ============================================================================
; Functions
; ============================================================================

; Function declarations - name field is the function name
(function_def
  name: (identifier) @function.definition)

(function_def
  name: (effect_identifier) @function.definition)

; Function signatures in effect blocks
(function_signature
  (identifier) @function.definition)

(function_signature
  (effect_identifier) @function.definition)

; Function calls
(call_expression
  (identifier) @function.call)

(call_expression
  (field_expression) @function.call)

; Lambda delimiters
(lambda
  "|" @punctuation.delimiter
  "|" @punctuation.delimiter)

; ============================================================================
; Variables and Identifiers
; ============================================================================

; Generic fallbacks - must come AFTER specific patterns
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

(field_expression
  "." @punctuation.delimiter
  (effect_identifier) @function.effectful)

; Let bindings
(let_binding
  (identifier) @variable)

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
  "?"
  ".."
] @operator

(type_annotation
  ":" @punctuation.delimiter)

(function_type
  "->" @punctuation.delimiter)

(type_signature
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

; String interpolation (must come AFTER generic punctuation to take priority)
(interpolation
  "${" @punctuation.special)
(interpolation
  "}" @punctuation.special)

; ============================================================================
; Attributes / Decorators
; ============================================================================

(prelude_decl
  "@prelude" @attribute)

(module_decl
  "@" @attribute)

; ============================================================================
; Effects
; ============================================================================

(effect_def
  "effect" @keyword.declaration
  (type_identifier) @type.effect)

(effect_set
  "{" @punctuation.bracket
  "}" @punctuation.bracket)

(effect_set
  (type_identifier) @type.effect)

; ============================================================================
; Sum types (variants)
; ============================================================================

(variant
  name: (type_identifier) @constructor)

(constructor_pattern
  (type_identifier) @constructor)

; Struct expressions
(struct_expression
  (type_identifier) @constructor)

; ============================================================================
; Modules
; ============================================================================

(module_path
  (type_identifier) @module)

(import_decl
  "import" @keyword.import)
