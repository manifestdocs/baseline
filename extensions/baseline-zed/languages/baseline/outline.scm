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
