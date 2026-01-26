; Rocket indentation queries

; Increase indent after opening braces/brackets
[
  (block_expression)
  (record_expression)
  (record_type)
  (list_expression)
  (effect_declaration)
  (match_expression)
  (lambda_expression)
] @indent

; Decrease indent at closing braces
[
  "}"
  "]"
  ")"
] @outdent

; Match arms get indentation
(match_arm) @indent
