; Baseline indentation queries

; Increase indent after opening braces/brackets
[
  (block)
  (record_expression)
  (record_type)
  (list_expression)
  (effect_def)
  (match_expression)
  (lambda)
] @indent

; Decrease indent at closing braces
[
  "}"
  "]"
  ")"
] @outdent

; Match arms get indentation
(match_arm) @indent
