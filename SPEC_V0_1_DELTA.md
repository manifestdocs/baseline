# Baseline v0.1: Implementation Constraints

This document defines the subset of the Language Specification supported in the v0.1 Bootstrap Compiler.

## 1. Type System Constraints

### 1.1 Refinement Types

**Supported:** Integer Intervals only.
**Not Supported:** Regex, Custom Predicates, String length checks.

```baseline
// ✅ Supported in v0.1
type Port = Int where self > 0
type Small = Int where self < 100

// ❌ Deferred to v0.2
type Email = String where self.matches(r".+@.+")
```

### 1.2 Routing & Maps

**Constraint:** No "Magic Types" for Route parameters.
The compiler will not generate types derived from URL strings in v0.1.

```baseline
// ❌ Invalid in v0.1 (Requires dependent macros)
let id = req.params.id

// ✅ Required in v0.1 (Standard Map access)
let id = req.params.get("id")?
```

## 2. Linter vs. Parser Responsibilities

To ensure the Agent receives helpful error messages, the Grammar (Tree-sitter) allows more syntax than the Linter permits.

### 2.1 Nested Calls

The grammar *allows* `f(g(x))`, but the **Linter** must emit a warning:

> "Prefer pipe syntax: `x |> g |> f`"

### 2.2 Loops

The grammar *allows* `for` loops anywhere, but the **Effect Checker** must emit an error if the loop body does not contain an Effect (`!` function).

## 3. Memory Model (Temporary)

**Constraint:** The Region/Arena model is disabled.
**Implementation:** All reference types use Atomic Reference Counting (`Arc<T>`).

This is an implementation detail hidden from the user, ensuring memory safety without the complexity of a borrow checker for the v0.1 release.

## 4. Effect System Constraints

### 4.1 Built-in Effects Only

v0.1 supports only the following built-in effects:

- `Console` - stdin/stdout/stderr
- `Http` - HTTP client requests
- `Fs` - File system operations
- `Log` - Structured logging
- `Time` - Time/duration operations
- `Random` - Random number generation
- `Env` - Environment variables

### 4.2 No Effect Handlers

Custom effect handlers (`handle` blocks) are deferred to v0.2. Effects are dispatched directly to runtime implementations.

## 5. Module System Constraints

### 5.1 Single-File Modules

Each `.bl` file is a module. No nested module declarations within files.

### 5.2 No Circular Imports

The compiler will error on circular import dependencies. Topological ordering required.

## 6. Standard Library Scope

v0.1 ships with minimal stdlib:

| Module | Contents |
|--------|----------|
| `Core` | `Option`, `Result`, basic operators |
| `String` | `split`, `join`, `trim`, `contains` |
| `List` | `map`, `filter`, `fold`, `find` |
| `Math` | `abs`, `min`, `max`, `clamp` |

Full stdlib (Http, Json, Async, etc.) is v0.2+.
