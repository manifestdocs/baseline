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
  (describe_block)
  (handler_map)
  (handle_expression)
] @indent

; Decrease indent at closing braces
[
  "}"
  "]"
  ")"
] @outdent

; Match arms get indentation
(match_arm) @indent

; Handler clauses get indentation
(handler_clause) @indent
