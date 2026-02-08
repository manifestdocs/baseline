; Baseline outline queries for symbol navigation

; Function declarations
(function_def
  name: (identifier) @name) @item

(function_def
  name: (effect_identifier) @name) @item

; Type declarations
(type_def
  name: (type_identifier) @name) @item

; Effect declarations
(effect_def
  (type_identifier) @name) @item

; Module declaration
(module_decl
  (module_path) @name) @item

; BDD describe/context blocks
(describe_block
  name: (string_literal) @name) @item

; BDD it blocks
(it_block
  name: (string_literal) @name) @item

; Spec blocks (show the spec name)
(spec_decl
  name: (_) @name) @item

; Inline tests
(inline_test
  (string_literal) @name) @item
