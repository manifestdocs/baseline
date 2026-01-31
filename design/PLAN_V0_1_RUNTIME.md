# Baseline v0.1 Runtime Plan: From Static Checker to Runnable Language

## Status Assessment

### What Works (as of Phase 3 completion)

| Component | Status | Notes |
|-----------|--------|-------|
| Tree-sitter grammar | ~95% v0.1 | Parses all core syntax, 8 GLR conflicts |
| Type checker | ~75% | Functions, structs, sum types, lists, if/else |
| Effect checker | ~80% | Static capability analysis, heuristic mapping |
| Refinement checker | ~40% | Integer intervals only (per spec) |
| Interpreter | ~35% | Pure math, closures, pattern matching |
| Standard library | 0% | Nothing implemented |
| Module/prelude system | 0% | Parsed but ignored at runtime |
| LSP | 0% | Stub only |

**47/47 tests pass** (all static analysis / parsing tests).

### What Cannot Run

The interpreter currently fails on any program that:

1. **Uses effects** — `Console.print!("hello")` crashes with "Not a function"
2. **Uses boolean operators** — `&&` and `||` not handled in `binary_expression`
3. **Uses `<=` or `>=`** — only `<`, `>`, `==`, `!=` exist
4. **Uses string operations** — no `+` concatenation, no string comparison
5. **Uses ranges** — `1..10` evaluates to `Unit` (silently ignored)
6. **Uses tuples** — no tuple expression or tuple pattern matching
7. **Uses floats** — no `Float` runtime value or arithmetic
8. **Has an effectful main** — `main!` not found (only looks for `main`)
9. **Uses `@prelude(core)`** — parsed, ignored, stdlib not loaded
10. **Uses the `?` operator** — not in grammar, not in interpreter

This means `examples/hello.bl` — the project's showcase file — cannot actually execute.

---

## Implementation Plan

Seven phases, ordered by dependency. Each phase produces a testable, shippable increment.

---

### Phase 1: Interpreter Expression Completeness

**Goal:** Every expression the grammar can parse, the interpreter can evaluate.

**1.1 — Missing binary operators**

In `interpreter.rs` `binary_expression` handler:

- `<=` and `>=` for Int operands
- `&&` and `||` for Bool operands (with short-circuit evaluation)
- `+` for String operands (concatenation)
- `==` and `!=` for String and Bool operands
- `<`, `>`, `<=`, `>=` for String operands (lexicographic)

**1.2 — Unary expressions**

Add `unary_expression` handler:

- `!` (boolean negation)
- `-` (integer/float negation)

**1.3 — Range expressions**

Add `range_expression` handler:

- `a..b` evaluates to `RuntimeValue::List` of `Int` from `a` to `b-1` (exclusive end, matching Rust/Koka convention)
- This makes `for x in 1..10 do ...` work immediately since `for` already handles lists

**1.4 — Tuple expressions and patterns**

- Add `RuntimeValue::Tuple(Vec<RuntimeValue>)` variant
- Handle `tuple_expression` → evaluate each element
- Handle `tuple_pattern` in `match_pattern()` → match each position
- Support `()` as `RuntimeValue::Unit` (already works)

**1.5 — Float support**

- Add `RuntimeValue::Float(f64)` variant
- Handle `float_literal` in eval
- Extend binary operator handler for Float operands
- Support mixed Int/Float arithmetic (promote Int to Float)

**1.6 — Fix effectful main detection**

In `main.rs` run command:
- Look for both `main` and `main!` in the context
- `main!` is the effectful entry point (the standard convention)

**1.7 — Block expression improvements**

- Handle `for_expression` as an evaluable child in blocks
- Handle `match_expression` as an evaluable child in blocks
- Use a more robust child-kind check (currently fragile suffix matching)

**Deliverable:** `blc run` can execute any pure Baseline program with math, strings, booleans, tuples, ranges, pattern matching, closures, and control flow. For loops work over ranges.

**Test file:**
```baseline
main : () -> Int = || {
  let x = 10;
  let y = 20;
  let big = x >= 10 && y <= 30;
  let name = "world";
  let greeting = "Hello, " + name;
  let nums = 1..6;
  let sum = 0;
  for n in nums do
    sum + n;
  sum
}
```

---

### Phase 2: Built-in Effect Runtime

**Goal:** Effectful functions (`Console.print!`, `Log.info!`, etc.) execute real side effects.

**2.1 — Builtin function registry**

Add to `interpreter.rs`:

```rust
pub enum RuntimeValue<'a> {
    // ... existing variants ...
    BuiltinFn(String, fn(Vec<RuntimeValue>) -> Result<RuntimeValue, String>),
}
```

Or, more practically, a `builtins` HashMap on `Context`:

```rust
pub struct Context<'a> {
    scopes: Vec<HashMap<String, RuntimeValue<'a>>>,
    builtins: HashMap<String, fn(Vec<RuntimeValue>) -> Result<RuntimeValue, String>>,
}
```

The interpreter resolves `Console.print!` → looks up `"Console.print!"` in builtins.

**2.2 — Console effect**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `Console.print!` | `String -> {Console} ()` | Print to stdout (no newline) |
| `Console.println!` | `String -> {Console} ()` | Print to stdout with newline |
| `Console.read_line!` | `() -> {Console} String` | Read line from stdin |
| `Console.error!` | `String -> {Console} ()` | Print to stderr |

**2.3 — Log effect**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `Log.info!` | `String -> {Log} ()` | `[INFO] msg` to stderr |
| `Log.warn!` | `String -> {Log} ()` | `[WARN] msg` to stderr |
| `Log.error!` | `String -> {Log} ()` | `[ERROR] msg` to stderr |
| `Log.debug!` | `String -> {Log} ()` | `[DEBUG] msg` to stderr |

**2.4 — Time effect**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `Time.now!` | `() -> {Time} Int` | Unix timestamp in ms |
| `Time.sleep!` | `Int -> {Time} ()` | Sleep for N milliseconds |

**2.5 — Random effect**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `Random.int!` | `(Int, Int) -> {Random} Int` | Random int in range |
| `Random.bool!` | `() -> {Random} Bool` | Random boolean |

**2.6 — Env effect**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `Env.get!` | `String -> {Env} String` | Get env variable (or error) |
| `Env.set!` | `(String, String) -> {Env} ()` | Set env variable |

**2.7 — Fs effect**

| Function | Signature | Behavior |
|----------|-----------|----------|
| `Fs.read!` | `String -> {Fs} String` | Read file contents |
| `Fs.write!` | `(String, String) -> {Fs} ()` | Write string to file |
| `Fs.exists!` | `String -> {Fs} Bool` | Check if path exists |
| `Fs.delete!` | `String -> {Fs} ()` | Delete file |

**2.8 — Wire field_expression to builtins**

When the interpreter sees `Console.print!(arg)`:
1. `field_expression` evaluates `Console.print!` → looks up module identifier
2. If the module identifier is a known effect module, resolve to `BuiltinFn`
3. `call_expression` sees `BuiltinFn`, calls the native function

Implementation approach: when `field_expression` encounters an identifier that matches a known effect module name (`Console`, `Log`, `Time`, `Random`, `Env`, `Fs`), construct the qualified name (e.g., `"Console.print!"`) and look it up in the builtins registry.

**Deliverable:** `examples/hello.bl` runs and prints output. Any program using v0.1 built-in effects executes correctly.

**Test file:**
```baseline
main! : () -> {Console} () = || {
  Console.println!("Hello, Baseline!");
  let name = "World";
  Console.println!("Hello, ${name}!");
}
```

---

### Phase 3: Core Standard Library

**Goal:** Implement the four v0.1 stdlib modules specified in `SPEC_V0_1_DELTA.md`.

**3.1 — Option type**

Register in context as sum type constructors:
- `Some(x)` — already works via `Enum("Some", [x])`
- `None` — already works via `Enum("None", [])`

Add module functions as builtins:
- `Option.map` : `(T?, (T -> U)) -> U?`
- `Option.unwrap` : `T? -> T` (panics on None)
- `Option.unwrap_or` : `(T?, T) -> T`
- `Option.is_some` : `T? -> Bool`
- `Option.is_none` : `T? -> Bool`

**3.2 — Result type**

Register constructors:
- `Ok(x)` → `Enum("Ok", [x])`
- `Err(e)` → `Enum("Err", [e])`

Add module functions:
- `Result.map` : `(Result<T,E>, (T -> U)) -> Result<U,E>`
- `Result.unwrap` : `Result<T,E> -> T` (panics on Err)
- `Result.unwrap_or` : `(Result<T,E>, T) -> T`
- `Result.is_ok` : `Result<T,E> -> Bool`
- `Result.is_err` : `Result<T,E> -> Bool`

**3.3 — String module**

Builtin functions:
- `String.length` : `String -> Int`
- `String.split` : `(String, String) -> List<String>`
- `String.join` : `(List<String>, String) -> String`
- `String.trim` : `String -> String`
- `String.contains` : `(String, String) -> Bool`
- `String.starts_with` : `(String, String) -> Bool`
- `String.ends_with` : `(String, String) -> Bool`
- `String.to_upper` : `String -> String`
- `String.to_lower` : `String -> String`
- `String.slice` : `(String, Int, Int) -> String`

**3.4 — List module**

Builtin functions:
- `List.map` : `(List<T>, (T -> U)) -> List<U>`
- `List.filter` : `(List<T>, (T -> Bool)) -> List<T>`
- `List.fold` : `(List<T>, U, (U, T) -> U) -> U`
- `List.find` : `(List<T>, (T -> Bool)) -> T?`
- `List.length` : `List<T> -> Int`
- `List.head` : `List<T> -> T?`
- `List.tail` : `List<T> -> List<T>`
- `List.push` : `(List<T>, T) -> List<T>`
- `List.concat` : `(List<T>, List<T>) -> List<T>`
- `List.reverse` : `List<T> -> List<T>`
- `List.contains` : `(List<T>, T) -> Bool`
- `List.sort` : `List<Int> -> List<Int>` (v0.1: Int only)

**3.5 — Math module**

Builtin functions:
- `Math.abs` : `Int -> Int`
- `Math.min` : `(Int, Int) -> Int`
- `Math.max` : `(Int, Int) -> Int`
- `Math.clamp` : `(Int, Int, Int) -> Int`
- `Math.pow` : `(Int, Int) -> Int`

**Deliverable:** All four v0.1 stdlib modules callable from any `.bl` file. Programs can do string manipulation, list processing, math, and use Option/Result.

**Test file:**
```baseline
main! : () -> {Console} () = || {
  let words = String.split("hello world foo", " ");
  let upper = List.map(words, |w| String.to_upper(w));
  let result = String.join(upper, ", ");
  Console.println!(result);

  let nums = [3, 1, 4, 1, 5];
  let total = List.fold(nums, 0, |acc, n| acc + n);
  Console.println!("Sum: ${total}");

  let found = List.find(nums, |n| n > 3);
  match found {
    | Some(n) -> Console.println!("Found: ${n}")
    | None -> Console.println!("Not found")
  }
}
```

---

### Phase 4: Module & Prelude System

**Goal:** `@prelude(core)` and `@prelude(script)` inject the standard library into scope.

**4.1 — Prelude definitions**

| Prelude | Injects |
|---------|---------|
| `core` | Option, Result, Math, String, List constructors |
| `script` | `core` + Console, Log, Time, Random, Env |
| `pure` | `core` only (no effects) |
| `minimal` | Nothing (explicit imports required) |

**4.2 — Prelude loading in interpreter**

When `eval` encounters a `prelude_decl` node:
1. Extract the prelude name
2. Call `load_prelude(name, &mut context)` which registers the appropriate builtins
3. The prelude system is the *only* way stdlib functions enter scope in v0.1

**4.3 — Module namespace resolution**

Effect modules and stdlib modules are accessed via qualified names: `Console.println!`, `String.split`, `List.map`. The `field_expression` handler needs to resolve these:

1. If left-hand side is an identifier matching a known module name → construct qualified lookup
2. If the qualified name exists in builtins → return `BuiltinFn`
3. Otherwise → normal field access on runtime value

**4.4 — Import statements (basic)**

For v0.1, imports are informational only (single-file modules). The prelude is the mechanism for loading stdlib. Multi-file imports are deferred.

**Deliverable:** `@prelude(script)` at the top of a file makes all v0.1 stdlib + effect functions available. Example programs work with their declared preludes.

---

### Phase 5: Error Handling

**Goal:** The `?` operator works for Option and Result types.

**5.1 — Grammar addition (if needed)**

Check if `?` postfix operator exists in grammar.js. If not, add as a postfix expression:
```javascript
try_expression: $ => prec(PREC.CALL, seq(field('expr', $._expression), '?'))
```

**5.2 — Interpreter support**

When `?` is applied:
- On `Enum("Ok", [val])` → unwrap to `val`
- On `Enum("Err", [e])` → early return `Enum("Err", [e])`
- On `Enum("Some", [val])` → unwrap to `val`
- On `Enum("None", [])` → early return `Enum("None", [])`

Implementation note: early return requires a control-flow mechanism in the interpreter. The simplest approach is a special `Err` variant that propagates:

```rust
enum EvalResult<'a> {
    Value(RuntimeValue<'a>),
    EarlyReturn(RuntimeValue<'a>),
}
```

**5.3 — `unwrap!` operator**

- On `Ok(val)` or `Some(val)` → unwrap
- On `Err(e)` or `None` → runtime panic with message

**Deliverable:** Programs can use `?` for error propagation and `Result`/`Option` chaining.

---

### Phase 6: Type Checker Alignment

**Goal:** The type checker understands everything the interpreter can execute.

**6.1 — Builtin function types**

Register type signatures for all builtin functions in the symbol table during type checking. This means `Console.println!` has a known type `String -> {Console} ()` and the type checker can validate calls.

**6.2 — Lambda argument type inference**

When a lambda is passed to a known function (e.g., `List.map(xs, |x| x + 1)`), infer the lambda argument type from the function's expected parameter type.

**6.3 — Tuple types**

Add `Type::Tuple(Vec<Type>)` variant to the type checker. Check tuple construction and pattern matching.

**6.4 — Float type checking**

Extend binary operator type checking for Float and mixed Int/Float.

**6.5 — Option/Result type awareness**

The type checker should understand `T?` as sugar for `Option<T>` and validate `match` patterns for Option/Result.

**Deliverable:** `blc check` reports accurate diagnostics for all v0.1 features. No false positives or false negatives for programs using the stdlib.

---

### Phase 7: Integration Testing & Polish

**Goal:** Every example `.bl` file runs correctly. Comprehensive test coverage.

**7.1 — End-to-end test runner**

Add integration tests that run `blc run` on each example file and assert on stdout output:

```rust
#[test]
fn run_hello() {
    let output = Command::new("cargo")
        .args(["run", "--", "run", "examples/hello.bl"])
        .output().unwrap();
    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains("Hello"));
}
```

**7.2 — Update example files**

Ensure all examples use correct v0.1 syntax and can both `check` and `run`:

- `hello.bl` — String interpolation, Console output, sum types
- `run_test.bl` — Arithmetic, control flow, blocks
- `struct_test.bl` — Records, field access
- `sum_type_test.bl` — Enums, pattern matching
- `effects_test.bl` — All v0.1 effects
- `refinement_test.bl` — Integer interval validation

**7.3 — New showcase examples**

Create examples that demonstrate the complete v0.1 feature set:

- `fizzbuzz.bl` — Ranges, match, string interpolation, Console
- `list_ops.bl` — List stdlib (map, filter, fold)
- `option_result.bl` — Error handling patterns

**7.4 — Error message polish**

- Improve runtime error messages (include source location where possible)
- Ensure type checker suggestions are actionable
- Verify JSON output format matches the tech spec

**Deliverable:** All examples run. Test suite covers every v0.1 feature. The language is usable for writing small programs.

---

## Dependency Graph

```
Phase 1 (Expressions)
    │
    ├──→ Phase 2 (Effects)
    │        │
    │        ├──→ Phase 3 (Stdlib)
    │        │        │
    │        │        └──→ Phase 4 (Prelude)
    │        │                 │
    │        │                 └──→ Phase 5 (Error Handling)
    │        │                          │
    │        │                          └──→ Phase 7 (Integration)
    │        │
    └──→ Phase 6 (Type Checker) ──→ Phase 7 (Integration)
```

Phases 1→2→3→4→5 are the critical path.
Phase 6 can progress in parallel after Phase 1.

---

## Estimated Scope per Phase

| Phase | New/Modified Files | New Tests |
|-------|-------------------|-----------|
| 1 | `interpreter.rs`, `main.rs` | ~15 |
| 2 | `interpreter.rs` (builtins module) | ~12 |
| 3 | `interpreter.rs` or new `stdlib.rs` | ~20 |
| 4 | `interpreter.rs`, `main.rs` | ~6 |
| 5 | `grammar.js` (maybe), `interpreter.rs` | ~8 |
| 6 | `types.rs`, `effects.rs` | ~15 |
| 7 | `tests/`, `examples/` | ~10 |

---

## Architecture Decision: Builtins vs. Baseline-in-Baseline

For v0.1, all stdlib functions are **native Rust builtins** registered in the interpreter context. This is the pragmatic choice because:

1. The interpreter doesn't yet support enough features to self-host stdlib
2. Native builtins give predictable performance
3. They serve as the reference implementation for future self-hosted stdlib

In v0.2+, stdlib functions that don't require native I/O (like `List.map`, `String.split`) can be rewritten in Baseline itself, loaded from `.bl` files in a `stdlib/` directory. The native builtins become the "runtime kernel" — only I/O and platform-specific operations stay in Rust.

---

## Success Criteria

When this plan is complete, the following should all work:

```bash
# Static checking with structured output
$ blc check examples/hello.bl --json
{"status": "success", "diagnostics": []}

# Running the hello world example
$ blc run examples/hello.bl
Hello, Baseline!

# Running a fizzbuzz
$ blc run examples/fizzbuzz.bl
1
2
Fizz
4
Buzz
...

# Running a list processing program
$ blc run examples/list_ops.bl
Sum: 15
Evens: [2, 4]
Names: ALICE, BOB, CHARLIE

# All tests pass
$ cargo test
test result: ok. 90+ passed; 0 failed
```
