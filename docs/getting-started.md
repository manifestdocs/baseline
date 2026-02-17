# Getting Started with Baseline

Baseline is a strongly typed, effect-based programming language. This guide takes you from installation to running your first program in under 5 minutes.

## Installation

Baseline requires [Rust](https://rustup.rs/) (1.75+). Install the compiler:

```bash
# Clone the repository
git clone https://github.com/manifestdocs/baseline.git
cd baseline

# Install the compiler
cargo install --path blc
```

Verify the installation:

```bash
blc --version
```

### Optional features

```bash
# With Cranelift JIT (faster execution for numeric workloads)
cargo install --path blc --features jit

# With AOT compilation (compile to standalone native executables)
cargo install --path blc --features aot
```

## Hello World

Create a file called `hello.bl`:

```baseline
@prelude(script)

fn greet(name: String) -> String = "Hello, ${name}!"

fn main!() -> {Console} () = {
  Console.println!(greet("World"))
}
```

Run it:

```bash
blc run hello.bl
# Hello, World!
```

Let's break down the syntax:

- `@prelude(script)` imports the standard library for scripting (Console, Math, List, etc.)
- `fn greet(name: String) -> String` declares a pure function with typed parameters
- `"Hello, ${name}!"` uses string interpolation
- `fn main!()` the `!` suffix marks this function as effectful
- `-> {Console} ()` declares the return type `()` (Unit) and the effect set `{Console}`
- `Console.println!(msg)` calls the effectful print function

## Type Checking

Baseline checks types, effects, and refinements at compile time:

```bash
blc check hello.bl
# ✓ No errors found

blc check hello.bl --json
# {"status":"success","passes":["types","effects","refinements","specs","smt"],"diagnostics":[]}
```

If there are errors, you'll see diagnostics with file locations:

```bash
blc check broken.bl --json
# {"status":"failure","diagnostics":[{"code":"TYP_006","message":"...","location":{"file":"broken.bl","line":3,"column":0}}]}
```

### Check levels

```bash
blc check hello.bl --level types        # Type checking only
blc check hello.bl --level refinements   # Types + refinements (default)
blc check hello.bl --level full          # Types + refinements + SMT specs
```

## Running Programs

The default runtime is a bytecode VM:

```bash
blc run hello.bl
```

With the `jit` feature installed, you can use Cranelift JIT for faster execution:

```bash
blc run hello.bl --jit
```

## Compiling to Native

With the `aot` feature installed, compile to a standalone executable:

```bash
blc build hello.bl -o hello
./hello
# Hello, World!
```

The resulting binary has no runtime dependencies.

## Inline Testing

Baseline has built-in test support. Create `math.bl`:

```baseline
@prelude(core)

fn add(a: Int, b: Int) -> Int = a + b

fn double(x: Int) -> Int = x * 2

@test
test "basic arithmetic" = 1 + 1 == 2
test "adds positive numbers" = add(1, 2) == 3
test "handles zero" = add(0, 5) == 5
test "doubles positive" = double(3) == 6
test "doubles zero" = double(0) == 0
```

Run the tests:

```bash
blc test math.bl
# PASS  basic arithmetic (line 9)
# PASS  adds positive numbers (line 10)
# PASS  handles zero (line 11)
# PASS  doubles positive (line 12)
# PASS  doubles zero (line 13)
#
# 5 tests: 5 passed, 0 failed
```

Tests go in `@test` sections, conventionally at the bottom of the file. Top-level `test` declarations outside `@test` sections also work.

## Editor Support

### VS Code

Install the Baseline extension from `extensions/baseline-vscode/` for syntax highlighting:

```bash
cd extensions/baseline-vscode
code --install-extension .
```

### Zed

The Baseline extension for Zed provides tree-sitter-based syntax highlighting:

```bash
# Copy to Zed extensions directory
cp -r extensions/baseline-zed ~/.config/zed/extensions/
```

## Language Tour

### Types

Baseline has four core types: `Int`, `String`, `Bool`, and `Float`.

```baseline
fn add(a: Int, b: Int) -> Int = a + b
fn greet(name: String) -> String = "Hi, ${name}"
fn is_even(n: Int) -> Bool = n % 2 == 0
```

### Records

Records are named product types:

```baseline
type Point = { x: Int, y: Int }
type User = { name: String, age: Int }

fn main() -> Int = {
  let p = Point { x: 10, y: 20 }
  p.x    // 10
}
```

### Sum Types

Sum types (enums) model alternatives:

```baseline
type Color = | Red | Green | Blue
type Shape = | Circle(Int) | Square(Int)
```

### Pattern Matching

Use `match` to destructure sum types:

```baseline
fn describe(c: Color) -> String =
  match c
    Red -> "red"
    Green -> "green"
    Blue -> "blue"

fn area(s: Shape) -> Int =
  match s
    Circle(r) -> r * r * 3
    Square(side) -> side * side
```

### Option and Result

Safe error handling without exceptions:

```baseline
fn safe_divide(a: Int, b: Int) -> Result =
  if b == 0 then Err("Division by zero")
  else Ok(a / b)
```

Use pattern matching to handle results:

```baseline
match safe_divide(10, 2)
  Ok(v) -> Console.println!("Result: ${v}")
  Err(msg) -> Console.println!("Error: ${msg}")
```

The `?` operator propagates errors through function chains:

```baseline
fn chain(a: Int, b: Int, c: Int) -> Result = {
  let first = safe_divide(a, b)?
  let second = safe_divide(first, c)?
  Ok(first + second)
}
```

### Effects

Side effects are explicit capabilities declared in function signatures:

```baseline
@prelude(script)

// Pure function — no effects, no !
fn double(x: Int) -> Int = x * 2

// Effectful function — ! suffix, effect set in return type
fn main!() -> {Console} () = {
  Console.println!("Hello!")     // Console.println! requires {Console}
  Console.println!("${double(21)}")
}
```

The `!` suffix marks effectful functions. The effect set (e.g., `{Console}`) declares which capabilities the function requires.

### Lists and Higher-Order Functions

```baseline
@prelude(script)

fn main!() -> {Console} () = {
  let nums = [1, 2, 3, 4, 5]
  let doubled = List.map(nums, |x| x * 2)
  let evens = List.filter(nums, |x| x % 2 == 0)
  let sum = List.fold(nums, 0, |acc, x| acc + x)
  Console.println!("Sum: ${sum}")
}
```

### Pipes

The pipe operator `|>` passes the left-hand value as the first argument to the right-hand function:

```baseline
let result = [1, 2, 3, 4, 5]
  |> List.filter(|x| x > 2)
  |> List.map(|x| x * 10)
// result == [30, 40, 50]
```

You can also pipe into lambdas:

```baseline
let doubled = 10
  |> |x| x * 2
// doubled == 20
```

### Refinement Types

Constrain values at compile time:

```baseline
type Port = Int where self > 0 && self < 65536
type Percentage = Int where self >= 0 && self <= 100
```

The compiler catches violations:

```bash
# let p: Port = 8080    → OK
# let q: Port = 70000   → Compile error:
#   Refinement Violation: Value 70000 is out of bounds for type Port [REF_001]
#   Type Port requires value in range [1, 65535]
```

### Prelude Levels

Control which capabilities are available:

| Level | Use Case | Includes |
|-------|----------|----------|
| `core` | Pure computation | Types, pure functions, Option, Result |
| `pure` | Data processing | + String, List operations |
| `script` | CLI / scripting | + Console, Math |
| `server` | HTTP services | + Http, Fs, Env |

```baseline
@prelude(core)      // Pure functions only
@prelude(script)    // Console I/O, Math, etc.
@prelude(server)    // HTTP, file system, environment
```

### User-Defined Generics

Write generic functions with type parameters:

```baseline
// Explicit type parameters
fn identity<T>(x: T) -> T = x

// Higher-order generics
fn apply<A, B>(f: (A) -> B, x: A) -> B = f(x)

// Implicit (single uppercase letters)
fn const_fn(x: T, _y: U) -> T = x
```

Type parameters are inferred at call sites:

```baseline
identity(42)          // T = Int
identity("hello")     // T = String
apply(|n| n + 1, 41)  // A = Int, B = Int
```

## CLI Reference

| Command | Description |
|---------|-------------|
| `blc check <file>` | Type-check a source file |
| `blc run <file>` | Execute a program |
| `blc test <file>` | Run inline tests |
| `blc fmt <file>` | Format source code |
| `blc build <file> -o <output>` | Compile to native binary (requires `aot` feature) |
| `blc lsp` | Start language server |

### Common flags

| Flag | Commands | Description |
|------|----------|-------------|
| `--json` | check, test | Machine-readable JSON output |
| `--level <level>` | check | Verification depth: `types`, `refinements`, `full` |
| `--jit` | run | Use Cranelift JIT (requires `jit` feature) |
| `-o <path>` | build | Output binary path |
| `--check` | fmt | Verify formatting without modifying |

## Next Steps

- Browse the `examples/` directory for more programs
- Read the [Language Specification](../design/baseline-language-specification.md) for full details
- Try writing tests alongside your functions using `@test` sections
