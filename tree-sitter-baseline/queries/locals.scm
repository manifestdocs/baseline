; Baseline locals queries for scope tracking

; ============================================================================
; Scopes
; ============================================================================

; Function bodies create a new scope
(function_def) @local.scope

; Lambda expressions create a new scope
(lambda) @local.scope

; Block expressions create a new scope
(block) @local.scope

; Match arms create a new scope (for pattern bindings)
(match_arm) @local.scope

; For expressions create a scope for the loop variable
(for_expression) @local.scope

; Let bindings create a scope
(let_binding) @local.scope

; ============================================================================
; Definitions
; ============================================================================

; Function definitions define names at module scope
(function_def
  name: (identifier) @local.definition.function)

(function_def
  name: (effect_identifier) @local.definition.function)

; Type definitions define types at module scope
(type_def
  name: (type_identifier) @local.definition.type)

; Effect definitions define effects at module scope
(effect_def
  (type_identifier) @local.definition.type)

; Let bindings define variables
(let_binding
  (identifier) @local.definition.variable)

; Pattern bindings in match arms
(match_arm
  (identifier) @local.definition.variable)

; Pattern bindings in constructor patterns
(constructor_pattern
  (identifier) @local.definition.variable)

; For loop variable
(for_expression
  (identifier) @local.definition.variable)

; ============================================================================
; References
; ============================================================================

; Variable references
(identifier) @local.reference

; Type references
(type_identifier) @local.reference
