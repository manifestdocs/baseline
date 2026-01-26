; Rocket locals queries for scope tracking

; ============================================================================
; Scopes
; ============================================================================

; Function bodies create a new scope
(function_declaration) @local.scope

; Lambda expressions create a new scope
(lambda_expression) @local.scope

; Block expressions create a new scope
(block_expression) @local.scope

; Match arms create a new scope (for pattern bindings)
(match_arm) @local.scope

; For expressions create a scope for the loop variable
(for_expression) @local.scope

; Let expressions create a scope for their body
(let_expression) @local.scope

; ============================================================================
; Definitions
; ============================================================================

; Function declarations define names at module scope
(function_declaration
  (lower_identifier) @local.definition.function)

(function_declaration
  (effectful_identifier) @local.definition.function)

; Type declarations define types at module scope
(type_declaration
  (upper_identifier) @local.definition.type)

; Effect declarations define effects at module scope
(effect_declaration
  (upper_identifier) @local.definition.type)

; Let bindings define variables
(let_expression
  (identifier_pattern
    (lower_identifier) @local.definition.variable))

; Lambda parameters define variables
(lambda_parameter
  (lower_identifier) @local.definition.parameter)

; Pattern bindings in match arms
(match_arm
  (identifier_pattern
    (lower_identifier) @local.definition.variable))

; Pattern bindings in variant patterns
(variant_pattern
  (identifier_pattern
    (lower_identifier) @local.definition.variable))

; For loop variable
(for_expression
  (identifier_pattern
    (lower_identifier) @local.definition.variable))

; Record field patterns define variables
(record_field_pattern
  (lower_identifier) @local.definition.variable)

; List pattern rest bindings
(rest_pattern
  (lower_identifier) @local.definition.variable)

; ============================================================================
; References
; ============================================================================

; Variable references
(lower_identifier) @local.reference

; Type references
(type_identifier) @local.reference

; Qualified identifier references
(qualified_identifier) @local.reference
