; Rocket outline queries for symbol navigation

; Function declarations
(function_declaration
  (lower_identifier) @name) @item

(function_declaration
  (effectful_identifier) @name) @item

; Type declarations
(type_declaration
  (upper_identifier) @name) @item

; Effect declarations
(effect_declaration
  (upper_identifier) @name) @item

; Module declaration
(module_declaration
  (module_path) @name) @item
