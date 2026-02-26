# A Tour of Baseline

Baseline is a strongly typed, effect-based programming language. If you've used Rust, OCaml, or Kotlin, you'll feel at home. This tour covers everything you need to start writing Baseline code.

## Hello World

```baseline
@prelude(script)

fn main!() -> {Console} () = {
  Console.println!("Hello, World!")
}
```

Save this as `hello.bl` and run it:

```sh
blc run hello.bl
```

A few things to notice:
- `@prelude(script)` imports the standard library (types, I/O, collections).
- `fn main!()` is the entry point. The `!` suffix means this function performs side effects.
- `{Console}` declares which effects the function uses.
- `()` is the unit type — this function returns nothing.

## Types

Baseline has a small set of core types: `Int`, `Float`, `String`, `Bool`, `List<T>`, `Option<T>`, and `Result<T, E>`.

```baseline
@prelude(core)

fn add(a: Int, b: Int) -> Int = a + b

fn greet(name: String) -> String = "Hello, ${name}!"

fn is_even(n: Int) -> Bool = n % 2 == 0
```

Every function explicitly declares its parameter and return types. String interpolation uses `${expr}` syntax.

## Let Bindings

Variables are immutable by default and introduced with `let`.

```baseline
let x = 42
let name = "Baseline"
let (a, b) = (1, 2)       // tuple destructuring
let nums = [1, 2, 3, 4]   // list literal
```

Blocks create scopes and return their last expression:

```baseline
let y = {
  let a = 10
  let b = 20
  a + b    // y = 30
}
```

## Functions

Functions are defined with `fn`. The body is an expression after `=`.

```baseline
fn double(x: Int) -> Int = x * 2

fn factorial(n: Int) -> Int =
  if n <= 1 then 1
  else n * factorial(n - 1)
```

Lambdas use `|params| body` syntax:

```baseline
let triple = |x| x * 3
let sum = |a, b| a + b
```

Generics work with single uppercase letters or explicit `<A, B>` syntax:

```baseline
fn identity(x: T) -> T = x
fn apply<A, B>(f: (A) -> B, x: A) -> B = f(x)
```

## Pipes

The `|>` operator chains a value through a series of functions, making data transformations read left-to-right.

```baseline
fn double(x: Int) -> Int = x * 2
fn increment(x: Int) -> Int = x + 1

// Without pipes
let result = increment(double(5))

// With pipes
let result = 5 |> double |> increment     // 11
```

## Control Flow

`if/else` is an expression that returns a value:

```baseline
let label = if x > 10 then "big" else "small"
```

`for` loops iterate over ranges:

```baseline
for n in 1..101 do
  Console.println!("${n}")
```

## Pattern Matching

`match` expressions provide exhaustive pattern matching on enums, literals, and data structures.

```baseline
type Shape = | Circle(Int) | Rectangle(Int, Int)

fn area(s: Shape) -> Int =
  match s
    Circle(r) -> r * r * 3
    Rectangle(w, h) -> w * h
```

Match works with Option, Result, literals, and wildcards:

```baseline
fn describe(n: Int) -> String =
  match n
    0 -> "zero"
    1 -> "one"
    _ -> "other"

fn unwrap_or(opt: Option<Int>, default: Int) -> Int =
  match opt
    Some(x) -> x
    None -> default
```

## Records and Enums

Records are named product types with field access:

```baseline
type Point = { x: Int, y: Int }

let p = Point { x: 3, y: 4 }
let moved = { ..p, x: 10 }   // record update: { x: 10, y: 4 }
```

Enums (sum types) define alternatives:

```baseline
type Color = | Red | Green | Blue
type Result<T, E> = | Ok(T) | Err(E)    // built-in
type Option<T> = | Some(T) | None       // built-in
```

## Option and Result

`Option<T>` represents values that might not exist. `Result<T, E>` represents operations that might fail. Both are enums with standard library functions.

```baseline
fn safe_divide(a: Int, b: Int) -> Result<Int, String> =
  if b == 0 then Err("division by zero")
  else Ok(a / b)

// The ? operator propagates errors
fn chain(a: Int, b: Int, c: Int) -> Result<Int, String> = {
  let x = safe_divide(a, b)?
  let y = safe_divide(x, c)?
  Ok(y)
}
```

Common operations:

```baseline
Option.map(Some(5), |x| x * 2)          // Some(10)
Option.unwrap(Some(42))                   // 42
Result.unwrap_or(Err("fail"), 0)          // 0
Result.ensure(age >= 18, "too young")     // Ok(()) or Err("too young")
```

## Lists and Higher-Order Functions

Lists support `map`, `filter`, `fold`, `find`, and more — all in `Module.method(value, args)` style.

```baseline
let nums = [1, 2, 3, 4, 5]

let doubled = List.map(nums, |x| x * 2)           // [2, 4, 6, 8, 10]
let evens = List.filter(nums, |x| x % 2 == 0)     // [2, 4]
let sum = List.fold(nums, 0, |acc, x| acc + x)     // 15
let found = List.find(nums, |x| x > 3)             // Some(4)
```

## Effects

Effects make side effects explicit in the type system. A function that prints to the console, reads files, or makes HTTP calls must declare those capabilities.

```baseline
// Pure — no effects, no ! suffix
fn double(x: Int) -> Int = x * 2

// Effectful — ! suffix, effects in return type
fn greet!(name: String) -> {Console} () =
  Console.println!("Hello, ${name}!")
```

The `!` suffix on a function name means it performs effects. The `{Console}` annotation declares which effects are used. The compiler verifies that functions only use the effects they declare.

Built-in effects include `Console`, `Fs`, `Http`, `Time`, `Random`, `Env`, and `Log`.

## Refinement Types

Refinement types constrain values at the type level. The compiler statically checks that values satisfy their constraints.

```baseline
type Port = Int where self > 0 && self <= 65535
type Percentage = Int where 0 <= self && self <= 100

fn set_port(p: Port) -> Port = p
```

The compiler will reject `set_port(-1)` at compile time — no runtime checks needed.

## Modules and Imports

Files are modules. Use `import` to bring in other modules.

```baseline
// Util.bl
@prelude(core)

fn add(a: Int, b: Int) -> Int = a + b
fn double(x: Int) -> Int = x * 2
```

```baseline
// main.bl
@prelude(script)

import Util                          // qualified: Util.add(1, 2)
// or: import Util.*                 // wildcard: add(1, 2)
// or: import Util.{add, double}     // selective

fn main!() -> {Console} () =
  Console.println!("${Util.add(3, 7)}")
```

Module files use PascalCase names matching their module name (`Util.bl` for `import Util`).

## Testing

Tests are built into the language with `test` expressions. Each test is a boolean expression.

```baseline
@prelude(core)

fn add(a: Int, b: Int) -> Int = a + b

test "adds positive numbers" = add(1, 2) == 3
test "adds zero" = add(0, 5) == 5

test "block test" = {
  let result = add(10, 20)
  result == 30
}
```

Run tests with:

```sh
blc test myfile.bl
```

BDD-style grouping is also supported:

```baseline
describe "Math operations" {
  it "adds two numbers" = 1 + 1 == 2
  it "multiplies two numbers" = 3 * 4 == 12
}
```

## What's Next

- **Language spec**: `design/baseline-language-specification.md` for the full reference.
- **Examples**: The `examples/` directory has programs covering sorting, calculators, and more.
- **Conformance tests**: `tests/conformance/` serves as executable documentation for every language feature.
- **Editor support**: Extensions are available for Zed and VS Code in `extensions/`.
