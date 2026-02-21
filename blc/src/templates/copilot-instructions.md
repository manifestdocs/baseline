This project uses the Baseline programming language.

## Critical Syntax Rules
- `!` means EFFECTS, not negation. Use `not` for boolean negation. `!valid` is a parse error.
- NO method syntax. Write `String.to_upper(name)`, never `name.to_upper()`.
- NO `+` for string concatenation. Use string interpolation: `"Hello, ${name}!"`.
- NO classes. Use records and sum types.
- NO `try`/`catch`. Use `Result<T, E>` and `?`.
- NO `null` or `undefined`. Use `Option<T>`.

## Common Mistakes
WRONG: `user.to_string()`   -> RIGHT: `String.from(user)`
WRONG: `!valid`             -> RIGHT: `not valid`
WRONG: `"a" + "b"`          -> RIGHT: `"${a}${b}"`
WRONG: `return x`           -> RIGHT: `x` (last expression is the return value)

For the full language specification, **ALWAYS read `llms.txt`** in the project root.
