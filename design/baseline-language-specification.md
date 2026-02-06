# Baseline Language Specification

**Version 0.1.0 — Draft**

> A fast, verifiable, LLM-native programming language.

> For aspirational features planned for v0.2 and beyond (memory model, compilation targets, advanced testing, concurrency, etc.), see [baseline-language-vision.md](baseline-language-vision.md).

---

## Table of Contents

1. [Introduction](#1-introduction)
   - 1.1 Design Philosophy
   - 1.2 Goals
   - 1.3 Non-Goals
   - 1.4 Influences
   - 1.5 LLM-Native Design Principles
   - 1.6 The One Way Principle
   - 1.7 Core Design Principles
2. [Lexical Structure](#2-lexical-structure)
3. [Types](#3-types)
4. [Expressions](#4-expressions)
5. [Functions](#5-functions)
6. [Effects](#6-effects)
7. [Modules](#7-modules)
8. [Specifications](#8-specifications)
9. [Testing](#9-testing)
    - 9.1 Testing Philosophy
    - 9.2 Inline Tests (Unit Tests)
    - 9.3 BDD Specifications
    - 9.4 Async and Effectful Tests
    - 9.5 Test Organization
    - 9.6 Assertions and Matchers
    - 9.7 Test Output for LLMs
    - 9.8 Best Practices
10. [Language Server Protocol and Compiler API](#10-language-server-protocol-and-compiler-api)
    - 10.1-10.6 Standard LSP, Query API, Sessions
11. [Standard Library](#11-standard-library)
12. [Grammar](#12-grammar)
- [Appendix A: Compiler Errors](#appendix-a-compiler-errors)
- [Appendix B: Trace Format](#appendix-b-trace-format)

---

## 1. Introduction

### 1.1 Design Philosophy

Baseline is designed around three core principles:

1. **The type is the spec** — Types encode enough information that correctness can be verified, tests can be generated, and documentation is always accurate.

2. **Effects are data** — Side effects are explicit, trackable, and mockable. Pure functions are the default; effects are opt-in capabilities.

3. **LLM-native** — The language is designed for machine generation and analysis. Syntax is unambiguous, errors are structured, and debugging is automated.

### 1.2 Goals

- **Fast compilation**: Development builds in <200ms
- **Fast execution**: Compiled to native with zero-cost effects and refinements
- **Small binaries**: Typical services <5MB
- **Verified correctness**: Specifications checked at compile time
- **Portable deployment**: Primary target is WebAssembly

### 1.3 Non-Goals

- Backward compatibility with existing languages
- Gradual typing or dynamic features
- Object-oriented programming
- Manual memory management (exposed to user)

### 1.4 Influences

Baseline draws from:

- **ML/OCaml**: Type inference, pattern matching, algebraic data types
- **Rust**: Ownership concepts (simplified), error handling, tooling quality
- **Koka**: Algebraic effects
- **Liquid Haskell**: Refinement types
- **Elm**: Friendly errors, simplicity
- **F#**: Pragmatic functional programming

### 1.5 LLM-Native Design Principles

Baseline is designed to be generated and analyzed by Large Language Models. This influences the language design in several ways:

#### 1.5.1 Constrained Solution Space

Traditional languages allow virtually unlimited ways to accomplish a task. Baseline constrains the solution space through:

- **Types as specifications**: The type signature encodes behavior, reducing defensive coding
- **Explicit effects**: Side effects are capabilities, preventing hallucinated I/O
- **Refinement types**: Boundary conditions are encoded in types, eliminating off-by-one errors

```baseline
// The type IS the specification
// An LLM doesn't need to "remember" to check boundaries
type Port = Int where 1 <= self <= 65535

listen : Port -> {Net} Server
listen = |port|
  // port is GUARANTEED to be 1-65535 here
  // No defensive coding needed
  ...
```

#### 1.5.2 Deterministic Verification

LLMs cannot step through debuggers or intuit runtime behavior. Baseline provides compile-time verification:

- **SMT-based spec checking**: Mathematical proof of correctness
- **Exhaustive pattern matching**: Compiler ensures all cases handled
- **Effect tracking**: Compiler prevents unauthorized side effects

#### 1.5.3 Structured Machine-Readable Output

All compiler output is structured JSON (see Appendix A), enabling automated self-correction:

```
LLM generates code
      ↓
baseline check --format=json
      ↓
Parse error JSON
      ↓
Apply suggested fix
      ↓
Retry
```

#### 1.5.4 Capability-Based Security for Agents

The effect system serves as a **security sandbox** for autonomous agents:

```baseline
// An agent with this signature CANNOT:
// - Access the filesystem
// - Make unauthorized network requests
// - Delete data
// - Perform any effect not in the set

agent_task! : Input -> {Log, Db.read} Output
agent_task! = |input|
  Log.info!("Processing ${input}")
  let data = Db.query!("SELECT * FROM items")
  // Fs.delete!("important.txt")  // COMPILE ERROR: Fs not in effect set
  transform(data)
```

This makes Baseline suitable for **untrusted AI-generated code** execution.

#### 1.5.5 Interactive Refinement Protocol

The compiler supports a dialogue-based refinement process for LLM integration:

```json
// LLM submits code
{ "action": "check", "code": "..." }

// Compiler responds with verification status
{
  "status": "unverified",
  "obligations": [
    {
      "location": { "line": 12, "col": 5 },
      "property": "user.age >= 0",
      "context": "Required by NonNegative refinement",
      "suggestions": [
        { "action": "assume", "code": "@assume user.age >= 0" },
        { "action": "guard", "code": "if user.age < 0 then return Err(InvalidAge)" },
        { "action": "query", "question": "What is the schema constraint on users.age?" }
      ]
    }
  ]
}

// LLM can query for more information
{ "action": "query", "question": "functions matching User -> String" }

// Compiler responds with available functions
{
  "matches": [
    { "name": "User.name", "type": "User -> String" },
    { "name": "User.email", "type": "User -> String" },
    { "name": "format_user", "type": "User -> String" }
  ]
}
```

This protocol enables LLMs to make informed decisions rather than guessing.

### 1.6 The One Way Principle

Baseline is opinionated by design. For every common operation, there is one obvious way to do it. This is not merely a convention—it is enforced by the compiler and formatter.

**Rationale**: For LLM code generation, every decision point is a potential error. Multiple equivalent syntaxes mean:
- More tokens spent choosing an approach
- Inconsistent codebases
- Conflicting patterns in training data
- Style arguments that waste human time

#### 1.6.1 Enforced Patterns

**String Formatting**: Interpolation only

```baseline
// THE way
let msg = "Hello, ${name}. You have ${count} messages."

// NOT supported - these don't exist in Baseline:
// format("Hello, {}", name)      // No format function
// "Hello, " + name               // No string concatenation with +
// sprintf, printf                // No C-style formatting
```

**Error Handling**: Result type with `?` propagation

```baseline
// THE way: propagate with ?
let user = get_user(id)?
let data = fetch_data(user)?
Ok(process(data))

// THE way: handle with match
match get_user(id)
  Ok(user) -> process(user)
  Err(e)   -> recover(e)

// NOT supported:
// try/catch              // Exceptions don't exist
// if err != nil          // Go-style doesn't exist
// .unwrap() in non-tests // Warning in production code
```

**Null Handling**: Option type, no null keyword

```baseline
// THE way
match user
  Some(u) -> u.name
  None    -> "Anonymous"

// Or with combinators
user |> Option.map(|u| u.name) |> Option.unwrap_or("Anonymous")

// Type system enforces it
find_user : Id -> User?  // Returns Option<User>

// NOT supported:
// null      // Keyword doesn't exist
// undefined // Keyword doesn't exist
// nil       // Keyword doesn't exist
```

**Data Transformation**: Combinators, not loops

```baseline
// THE way
let doubled = List.map(numbers, |x| x * 2)
let evens = List.filter(numbers, |x| x % 2 == 0)
let sum = List.fold(numbers, 0, |acc, x| acc + x)

// For loops are ONLY for effects
for item in items do
  Log.info!("Processing ${item}")
  process!(item)

// NOT allowed - compiler error:
let results = []
for x in items do
  List.push(results, transform(x))  // Error: use List.map() instead
```

**Chained Operations**: Pipes, not nesting

```baseline
// THE way
let result = input
  |> parse
  |> validate
  |> transform

// Discouraged - lint warning:
let result = transform(validate(parse(input)))
// Warning: prefer pipe operator for chained transformations
```

**Imports**: Explicit, not wildcard

```baseline
// THE way
import Http.{get!, post!}
import Json.{encode, decode}

// Discouraged - lint warning:
import Http.*
// Warning: prefer explicit imports; wildcard obscures dependencies
```

**Function Definition**: One syntax

```baseline
// THE way
greet : String -> String
greet = |name| "Hello, ${name}"

// NOT supported - these syntaxes don't exist:
// fn greet(name: String) -> String { }
// func greet(name) { }
// def greet(name):
// function greet(name) { }
```

**Equality**: One operator

```baseline
// THE way
if a == b then ...

// NOT supported:
// a === b    // Strict equality doesn't exist (no type coercion)
// a.equals(b) // Method form doesn't exist for primitives
// eq(a, b)    // Function form discouraged
```

#### 1.6.2 Permitted Flexibility

Some patterns genuinely have multiple valid forms based on context:

**Pattern Matching Depth**

```baseline
// Simple: inline match
let name = match user
  Some(u) -> u.name
  None    -> "Anonymous"

// Complex: explicit match
let result = match response
  Ok({data, meta}) -> process(data, meta)
  Err(Timeout)     -> retry()
  Err(NotFound)    -> default_value
  Err(e)           -> fail(e)

// Both are idiomatic - context determines choice
```

**Function Body Style**

```baseline
// Expression body (short functions)
double : Int -> Int
double = |x| x * 2

// Block body (multi-step)
process : Input -> Output
process = |input| {
  let parsed = parse(input)
  let validated = validate(parsed)
  transform(validated)
}
```

### 1.7 Core Design Principles

#### 1.7.1 Composition Over Inheritance

Baseline has no classes, no inheritance, no `extends`. Instead, it uses:

**Records for Data**

```baseline
// NOT this (doesn't exist):
// class User extends Entity { ... }

// THIS: composition via records
type Entity = { id: Id, created_at: Timestamp }
type User = { entity: Entity, name: String, email: Email }
```

**Structural Typing for Polymorphism**

```baseline
// NOT this (doesn't exist):
// interface Named { name: String }
// class User implements Named { }

// THIS: structural typing with row polymorphism
greet : { name: String, ..rest } -> String
greet = |entity| "Hello, ${entity.name}"

// Works for ANY record with a name field
type User = { name: String, email: Email, role: Role }
type Company = { name: String, employees: Int }
type Pet = { name: String, species: String }

greet(user)     // Works
greet(company)  // Works
greet(pet)      // Works
```

**Behavior via Functions**

```baseline
// NOT this (doesn't exist):
// class Animal { abstract speak(): String }
// class Dog extends Animal { speak() { return "woof" } }

// THIS: functions as values
type Speaker = { speak: () -> String }

let dog_speaker: Speaker = { speak: || "woof" }
let cat_speaker: Speaker = { speak: || "meow" }

make_noise : Speaker -> String
make_noise = |s| s.speak()
```

**Capabilities via Effects**

```baseline
// NOT this (doesn't exist):
// interface Loggable { log(msg: String): void }
// class Service implements Loggable { }

// THIS: effects as capabilities
effect Log {
  debug! : String -> ()
  info! : String -> ()
  error! : String -> ()
}

// Functions declare what capabilities they need
process! : Data -> {Log, Db} Result
process! = |data|
  Log.info!("Processing ${data.id}")
  // ...
```

#### 1.7.2 Parse, Don't Validate

This principle, articulated by Alexis King, states: **use types to make illegal states unrepresentable**. Instead of validating data and hoping it stays valid, parse it into a type that guarantees validity.

**The Problem with Validation**

```baseline
// BAD: Validation doesn't prevent future misuse
process_email_bad : String -> {Http} Result
process_email_bad = |email|
  if not String.contains(email, "@") then
    return Err(InvalidEmail)

  // email is still just a String
  // nothing prevents passing an invalid string here
  // the validation might not happen in all code paths
  send_welcome!(email)

// Somewhere else in the codebase...
send_newsletter!(user_input)  // Oops, forgot to validate!
```

**The Solution: Parse Into Types**

```baseline
// GOOD: The type IS the validation
type Email = String where String.matches(self, r".+@.+\..+")

process_email : Email -> {Http} Result
process_email = |email|
  // email is GUARANTEED valid by the type system
  // impossible to have an invalid Email
  send_welcome!(email)

// The "parsing" happens once, at the boundary
handle_signup! : Request -> {Http} Response
handle_signup! = |req|
  let raw_email = req.body.email  // String

  match Email.parse(raw_email)
    Ok(email) ->
      process_email(email)  // Type-safe, guaranteed valid
      Ok(Response.success())
    Err(_) ->
      Ok(Response.bad_request("Invalid email"))
```

**Key Benefits:**

| Aspect | Validate | Parse |
|--------|----------|-------|
| Invalid data can exist | Yes, as raw type | No, type prevents it |
| Re-validation needed | Every use site | Never |
| Documentation | Comments, maybe | Type signature |
| Compiler enforcement | None | Full |
| LLM error potential | High (might forget check) | Low (type system catches) |

**More Examples:**

```baseline
// Parse, don't validate: IDs
type UserId = Int where self > 0
type OrderId = Int where self > 0

// These are different types! Can't mix them up.
get_user : UserId -> {Db} User?
get_order : OrderId -> {Db} Order?

get_user(order_id)  // Compile error: expected UserId, got OrderId


// Parse, don't validate: Collections
type NonEmpty<T> = List<T> where List.len(self) > 0

head : NonEmpty<T> -> T
head = |list| Option.unwrap(List.get(list, 0))  // Safe! List is guaranteed non-empty

// Can't call head on empty list - type prevents it
head([])        // Compile error: [] is not NonEmpty
head([1, 2, 3]) // OK: [1, 2, 3] satisfies NonEmpty


// Parse, don't validate: State machines
type UnverifiedUser = { email: String, token: String }
type VerifiedUser = { email: Email, verified_at: Timestamp }

// Can't send to unverified users - different type!
send_newsletter! : VerifiedUser -> {Http} Result
send_newsletter! = |user| ...

// The only way to get a VerifiedUser is through verification
verify! : (UnverifiedUser, Token) -> {Db} VerifiedUser?
verify! = |user, token|
  if token == user.token then
    Some({ email: Result.unwrap(Email.parse(user.email)), verified_at: Time.now!() })
  else
    None
```

**Boundary Parsing Pattern:**

```baseline
// All external input enters through "parsing boundaries"
// Internal code only works with parsed types

// HTTP handler: the boundary
handle_request! : RawRequest -> {Http, Db} Response
handle_request! = |raw|
  // Parse at the boundary
  let request = Request.parse(raw)?        // Validates structure
  let user_id = UserId.parse(request.user_id)?  // Validates ID
  let email = Email.parse(request.email)?  // Validates email

  // Internal processing uses parsed types
  // No validation needed - types guarantee correctness
  process!(user_id, email)


// Internal function: no validation, just logic
process! : (UserId, Email) -> {Db} ProcessResult
process! = |user_id, email|
  // user_id is guaranteed positive
  // email is guaranteed well-formed
  // Just do the work
  let user = Db.get_user!(user_id)?
  send_notification!(user, email)
```

#### 1.7.3 Make Illegal States Unrepresentable

The combination of composition and parse-don't-validate leads to a broader principle: **design types so that invalid states cannot be constructed**.

```baseline
// BAD: This type allows invalid states
type ConnectionBad = {
  status: String,        // "connected", "disconnected", "error"
  socket: Socket?,       // Only valid when connected
  error: String?,        // Only valid when error
  retry_count: Int,      // Only meaningful when error
}
// Can construct: { status: "connected", socket: None, error: "oops", retry_count: -5 }
// Nonsensical!


// GOOD: Invalid states are unrepresentable
type Connection =
  | Disconnected
  | Connected(Socket)
  | Error({ message: String, retry_count: Int where self >= 0 })

// Can only construct valid states:
// Disconnected - no socket, no error
// Connected(socket) - has socket, no error
// Error({...}) - has error info, no socket


// Pattern matching ensures exhaustive handling
handle : Connection -> Action
handle = |conn| match conn
  Disconnected        -> reconnect()
  Connected(socket)   -> use_socket(socket)
  Error({ retry_count, .. }) if retry_count < 3 -> retry()
  Error(_)            -> give_up()
```

These principles are not suggestions—they are enabled and enforced by Baseline's type system. Inheritance is impossible. Unvalidated data at internal boundaries is a type error. Invalid states are unrepresentable. The compiler is your partner in maintaining these invariants.

---

## 2. Lexical Structure

### 2.1 Source Encoding

Baseline source files are UTF-8 encoded. The file extension is `.bl`.

### 2.2 Comments

```baseline
// Single line comment

/*
   Multi-line comment
   Can be nested /* like this */
*/

/// Documentation comment (attaches to following declaration)
/// Supports markdown formatting
```

### 2.3 Identifiers

```baseline
// Value identifiers: lowercase start, snake_case
user_name
get_user
x

// Type identifiers: uppercase start, PascalCase
User
HttpResponse
Option

// Effect identifiers: uppercase start, PascalCase, used with braces
{Http, Log, Db}

// Effectful function identifiers: end with !
fetch_user!
save!
```

Reserved words:

```
let match if then else where with
type alias effect module import export
true false not
```

### 2.4 Literals

#### Numeric Literals

```baseline
42          // Int
-17         // Int (negative)
3.14159     // Float
-0.001      // Float (negative)
1_000_000   // Int with separators
0xFF        // Int (hexadecimal)
0b1010      // Int (binary)
0o755       // Int (octal)
1.5e10      // Float (scientific)
```

#### String Literals

```baseline
"Hello, World"              // Basic string
"Line 1\nLine 2"            // Escape sequences
"Say \"hello\""             // Escaped quotes
"Path: C:\\Users"           // Escaped backslash
"Hello, ${name}"            // String interpolation
"${user.name} is ${age}yo"  // Multiple interpolations
"Compute: ${1 + 2 * 3}"     // Expression interpolation
```

Escape sequences:

| Sequence | Meaning |
|----------|---------|
| `\n` | Newline |
| `\r` | Carriage return |
| `\t` | Tab |
| `\\` | Backslash |
| `\"` | Double quote |
| `\{` | Literal `${` (escape interpolation) |
| `\u{XXXX}` | Unicode code point |

Raw strings (no escapes, no interpolation):

```baseline
r"C:\Users\name"            // Raw string
r#"Contains "quotes""#      // Raw string with delimiter
```

Multi-line strings:

```baseline
"""
This is a multi-line string.
  Indentation is preserved.
Interpolation ${works} here too.
"""
```

#### Other Literals

```baseline
true, false     // Bool
()              // Unit (the empty tuple)
'a', '😀'       // Char (Unicode scalar value)
```

### 2.5 Operators

#### Arithmetic

| Operator | Description | Precedence |
|----------|-------------|------------|
| `+` | Addition | 6 |
| `-` | Subtraction | 6 |
| `*` | Multiplication | 7 |
| `/` | Division | 7 |
| `%` | Modulo | 7 |
| `**` | Exponentiation | 8 (right-assoc) |
| `-` (unary) | Negation | 9 |

#### Comparison

| Operator | Description | Precedence |
|----------|-------------|------------|
| `==` | Equal | 4 |
| `!=` | Not equal | 4 |
| `<` | Less than | 4 |
| `>` | Greater than | 4 |
| `<=` | Less or equal | 4 |
| `>=` | Greater or equal | 4 |

#### Logical

| Operator | Description | Precedence |
|----------|-------------|------------|
| `&&` | Logical AND | 3 |
| `\|\|` | Logical OR | 2 |
| `not` | Logical NOT | 9 |

#### Pipeline and Composition

| Operator | Description | Precedence |
|----------|-------------|------------|
| `\|>` | Forward pipe | 1 |
| `<\|` | Backward pipe | 1 |
| `>>` | Forward compose | 1 |
| `<<` | Backward compose | 1 |

#### Error Handling

| Operator | Description |
|----------|-------------|
| `?` | Propagate error (postfix) |

### 2.6 Delimiters and Punctuation

```baseline
( )     // Grouping, function calls, tuples
[ ]     // Lists
{ }     // Records, blocks, effect sets
< >     // Type parameters (in type context)
,       // Separator
:       // Type annotation
=       // Binding, assignment
->      // Function type, match arm
=>      // Fat arrow (reserved for future use)
|       // Function parameters, match patterns, union types
.       // Field access
..      // Spread operator
@       // Attribute prefix
_       // Wildcard pattern
```

### 2.7 Diff-Friendly Syntax Rules

Baseline's syntax is designed to minimize cascading changes when code is modified, which is critical for LLM-generated patches and version control:

#### Trailing Commas

Trailing commas are **always allowed** and **recommended** in multi-line constructs:

```baseline
// Good: Adding a field only changes one line
type User = {
  name: String,
  email: String,
  age: Int,     // Trailing comma allowed
}

// Good: Adding an argument only changes one line
let result = some_function(
  arg1,
  arg2,
  arg3,         // Trailing comma allowed
)

// Good: Adding a variant only changes one line
type Status =
  | Active
  | Inactive
  | Pending     // No comma needed for variants (use |)
```

#### No Significant Whitespace

Indentation is **convention only**, not syntax. The following are equivalent:

```baseline
// Conventional formatting
let result = match x
  Some(v) -> v
  None -> default

// Unconventional but valid
let result = match x Some(v) -> v None -> default

// Also valid (explicit block)
let result = match x {
  Some(v) -> v
  None -> default
}
```

#### Statement Terminators

Baseline uses newlines as statement separators, but semicolons are available:

```baseline
// Without semicolons (preferred)
let x = 1
let y = 2
x + y

// With semicolons (equivalent)
let x = 1; let y = 2; x + y
```

#### Closing Delimiters

In multi-line constructs, closing delimiters should appear on their own line:

```baseline
// Recommended
let config = {
  host: "localhost",
  port: 8080,
}

// Not recommended (but valid)
let config = { host: "localhost",
  port: 8080 }
```

---

## 3. Types

### 3.1 Primitive Types

```baseline
Bool        // true or false
Int         // 64-bit signed integer
Float       // 64-bit IEEE 754 floating point
Char        // Unicode scalar value
String      // UTF-8 string (immutable)
Unit        // The unit type, written ()
Never       // The bottom type (no values)
```

### 3.2 Compound Types

#### Tuples

```baseline
(Int, String)               // Pair
(Int, String, Bool)         // Triple
()                          // Unit (empty tuple)

// Usage
let pair = (42, "hello")
let (x, y) = pair           // Destructuring
pair.0                      // Field access (returns 42)
```

#### Lists

```baseline
List<Int>                   // Homogeneous list
[1, 2, 3]                   // List literal
[]                          // Empty list (type inferred)
[1, ..rest]                 // List pattern with rest

// Usage
let nums = [1, 2, 3]
List.len(nums)              // 3
List.get(nums, 0)           // Some(1)
[0, ..nums]                 // Prepend: [0, 1, 2, 3]
[..nums, 4]                 // Append: [1, 2, 3, 4]
```

#### Records

Records are structurally typed with row polymorphism:

```baseline
{ name: String, age: Int }  // Record type

// Usage
let user = { name: "Alice", age: 30 }
user.name                   // "Alice"
{ ..user, age: 31 }         // Update: { name: "Alice", age: 31 }

// Row polymorphism
greet : { name: String, ..r } -> String
greet = |person| "Hello, ${person.name}"

greet({ name: "Bob", age: 25 })         // Works
greet({ name: "Carol", role: "Admin" }) // Also works
```

#### Functions

```baseline
Int -> String                   // Function taking Int, returning String
(Int, Int) -> Int               // Function taking two Ints
Int -> {Http} String            // Effectful function
```

### 3.3 Algebraic Data Types

#### Type Aliases

```baseline
type UserId = Int
type Pair<A, B> = (A, B)
type Handler<T> = Request -> Response<T>
```

#### Sum Types (Enums)

```baseline
type Option<T> =
  | Some(T)
  | None

type Result<T, E> =
  | Ok(T)
  | Err(E)

type Json =
  | Null
  | Bool(Bool)
  | Number(Float)
  | String(String)
  | Array(List<Json>)
  | Object(Map<String, Json>)

// Usage
let x: Option<Int> = Some(42)
let y: Option<Int> = None

match x
  Some(n) -> "Got ${n}"
  None    -> "Nothing"
```

#### Parameterized Types

```baseline
type Map<K, V>              // Built-in map type
type Set<T>                 // Built-in set type
type Tree<T> =
  | Leaf(T)
  | Node(Tree<T>, Tree<T>)
```

### 3.4 Refinement Types

Refinement types add constraints to base types:

```baseline
type Port = Int where 1 <= self <= 65535
type Email = String where String.matches(self, r".+@.+\..+")
type NonEmpty<T> = List<T> where List.len(self) > 0
type Positive = Int where self > 0
type Percentage = Float where 0.0 <= self <= 100.0
```

Refinements are checked at boundaries:

```baseline
// The refinement is checked when calling this function
listen : Port -> {Net} Server
listen = |port|
  // Inside here, port is guaranteed to be 1-65535
  ...

// Compile error: 70000 does not satisfy 1 <= self <= 65535
listen(70000)

// Runtime check at boundary
let user_input: Int = read_port()
listen(user_input)  // Checked at runtime, returns Result
```

Refinement operators:

```baseline
where self > 0                  // Comparison
where self != ""                // Inequality
where List.len(self) > 0        // Function call on self
where String.matches(self, regex) // Pattern matching
where String.contains(self, x)  // Collection membership
where self >= other             // Reference other fields (in records)
where predicate(self)           // Custom predicate function
```

#### Regex Subset for Refinements

When using `String.matches()` in refinement types, Baseline uses a **deterministic regex subset** to ensure consistent behavior between compile-time checking and runtime validation:

```baseline
// Supported regex features
/abc/           // Literal characters
/[a-z]/         // Character classes
/[^0-9]/        // Negated character classes
/./             // Any character (except newline)
/a*/            // Zero or more (greedy)
/a+/            // One or more (greedy)
/a?/            // Zero or one
/a{3}/          // Exactly N
/a{2,5}/        // Between N and M
/(ab)/          // Grouping
/a|b/           // Alternation
/^abc$/         // Anchors
/\d \w \s/      // Character class shortcuts
```

```baseline
// NOT supported (to ensure determinism and prevent ReDoS)
// No backreferences: \1, \2
// No lookahead: (?=...), (?!...)
// No lookbehind: (?<=...), (?<!...)
// No possessive quantifiers: a++
// No atomic groups: (?>...)
// No recursive patterns
```

For complex validation beyond this subset, use predicate functions:

```baseline
// Complex validation via predicate
type ValidEmail = String where is_valid_email(self)

// The predicate has full language power
is_valid_email : String -> Bool
is_valid_email = |s|
  let parts = String.split(s, "@")
  List.len(parts) == 2
    && List.get(parts, 0) |> Option.map(|p| String.len(p) > 0) == Some(true)
    && List.get(parts, 1) |> Option.map(|p| String.contains(p, ".")) == Some(true)
```

This ensures:
- **Consistency**: Same behavior at compile-time and runtime
- **Security**: No catastrophic backtracking (ReDoS attacks)
- **Portability**: Same behavior on all platforms

### 3.5 Optional and Result Types

These are so common they have special syntax:

```baseline
T?          // Sugar for Option<T>

// Usage
find : Id -> User?
find = |id| ...

parse : String -> Result<Int, ParseError>
parse = |s| ...
```

### 3.6 Type Inference

Baseline uses bidirectional type inference. Type annotations are required at:

- Module boundaries (exported functions)
- Effectful functions
- Ambiguous situations

Type annotations are optional for:

- Local variables
- Lambda expressions
- Private functions (inferred)

```baseline
// Annotation required (exported)
export greet : String -> String
greet = |name| "Hello, ${name}"

// Annotation optional (local)
let nums = [1, 2, 3]                        // Inferred as List<Int>
let doubled = List.map(nums, |x| x * 2)     // Inferred
```

---

### 3.7 Tiered Refinement Types

Baseline implements a **tiered refinement system** designed to balance verification power with ease of use.

#### Tier 1: Automatic Refinements (Zero Annotation)
Common safety properties are handled automatically by the compiler without user intervention:

- **Null Safety**: `T?` is checked exhaustively.
- **Division by Zero**: `x / y` requires `y` to be non-zero (via control flow analysis).
- **Array Bounds**: Static analysis eliminates bounds checks where possible.

#### Tier 2: Simple Refinements
Users can define refined types using simple predicates (Linear Arithmetic):

```baseline
type Port = Int where self > 0 && self < 65536
type Probability = Float where self >= 0.0 && self <= 1.0

connect : (Host, Port) -> Connection
connect = |host, port| ... // port is guaranteed valid
```

#### Tier 3: Contracts
For critical boundaries, explicit pre- and post-conditions can be added:

```baseline
// "Parse, Don't Validate"
// Instead of validating 'email' inside the function,
// require a valid Email type as input.
send_email! : Email -> {Http} Result
```

## 4. Expressions

### 4.1 Let Bindings

```baseline
let x = 42
let (a, b) = get_pair()
let { name, age } = get_user()
let [first, ..rest] = get_list()

// With type annotation
let x: Int = 42

// Let is an expression
let result =
  let x = 10
  let y = 20
  x + y           // result = 30
```

### 4.2 Conditionals

```baseline
if condition then expr1 else expr2

// Multi-line
if condition then
  expr1
else
  expr2

// Chained
if cond1 then
  expr1
else if cond2 then
  expr2
else
  expr3

// If is an expression
let max = if a > b then a else b
```

### 4.3 Pattern Matching

```baseline
match expr
  pattern1 -> result1
  pattern2 -> result2
  _        -> default

// Patterns
match value
  42              -> "exact value"
  x               -> "bind to x"
  _               -> "wildcard"
  (a, b)          -> "tuple"
  [x, y, z]       -> "list of 3"
  [head, ..tail]  -> "list with rest"
  { name, age }   -> "record"
  { name, .. }    -> "partial record"
  Some(x)         -> "variant"
  None            -> "variant"
  x if x > 0      -> "guard"
  1 | 2 | 3       -> "or pattern"
```

Pattern matching is exhaustive. The compiler verifies all cases are handled.

### 4.4 Blocks

```baseline
{
  let x = 10
  let y = 20
  x + y         // Block returns last expression
}

// Single expression blocks
{ x + y }

// Used for scoping
let result = {
  let temp = expensive_computation()
  transform(temp)
}
// temp is not visible here
```

### 4.5 Pipelines

```baseline
// Forward pipe: x |> f is f(x)
value
|> transform
|> validate
|> save

// Equivalent to
save(validate(transform(value)))

// With lambdas
numbers
|> List.filter(|n| n > 0)
|> List.map(|n| n * 2)
|> List.sum

// Backward pipe (less common)
print <| format <| compute(x)
```

### 4.6 Function Application

```baseline
// Standard application
f(x)
f(x, y)
f(x, y, z)

// Named arguments (order-independent)
create_user(name: "Alice", age: 30)
create_user(age: 30, name: "Alice")  // Same
```

### 4.7 Field Access

```baseline
user.name               // Record field
tuple.0                 // Tuple index
response.body.data.users.first  // Chained field access
```

### 4.8 Error Handling Expressions

```baseline
// Propagate error (return early if Err/None)
let value = fallible_operation()?

// Unwrap (panics if None/Err — use in tests or when logically guaranteed)
let value = Option.unwrap(maybe_none)
let value = Result.unwrap(fallible_result)

// Unwrap with default
let value = Option.unwrap_or(maybe_none, default_value)

// Try-catch style
try
  risky_operation()
catch
  NetworkError(e) -> handle_network(e)
  TimeoutError    -> retry()
  _               -> fail()
```

---

## 5. Functions

### 5.1 Function Definitions

```baseline
// Named function
greet : String -> String
greet = |name| "Hello, ${name}"

// Multiple parameters
add : (Int, Int) -> Int
add = |a, b| a + b

// Pattern matching in parameters
first : List<T> -> T?
first = |list| match list
  [x, .._] -> Some(x)
  []       -> None

// With where clause for local definitions
quicksort : List<Int> -> List<Int>
quicksort = |list| match list
  [] -> []
  [pivot, ..rest] ->
    let smaller = List.filter(rest, |x| x < pivot)
    let larger = List.filter(rest, |x| x >= pivot)
    quicksort(smaller) ++ [pivot] ++ quicksort(larger)
```

### 5.2 Anonymous Functions (Lambdas)

```baseline
|x| x + 1                   // Single parameter
|a, b| a + b                // Multiple parameters
|_| 42                      // Ignored parameter
|(x, y)| x + y              // Destructuring

// With type annotations
|x: Int| -> String { Int.to_string(x) }

// Multi-line
|request| {
  let user = authenticate(request)?
  let data = fetch_data(user)?
  Ok(format_response(data))
}
```

### 5.3 Effectful Functions

Functions that perform side effects are marked with `!`:

```baseline
// Declaration
fetch_user! : Id -> {Http, Log} User?
fetch_user! = |id|
  Log.debug!("Fetching user ${id}")
  Http.get!("/users/${id}")? |> Response.decode

// Calling effectful functions
main! : {Console, Http} ()
main! =
  let user = fetch_user!(42)?
  Console.print!("Got user: ${user.name}")
```

Rules for effectful functions:

1. Must be declared with `!` suffix
2. Must declare their effects in the type signature
3. Can only be called from other effectful functions
4. Can call pure functions freely

### 5.4 Generic Functions

```baseline
identity : T -> T
identity = |x| x

map : (List<A>, A -> B) -> List<B>
map = |list, f| match list
  []        -> []
  [x, ..xs] -> [f(x), ..map(xs, f)]

// With constraints
compare : (T, T) -> Ordering where T: Ord
compare = |a, b| Ord.compare(a, b)

// Multiple type parameters
zip : (List<A>, List<B>) -> List<(A, B)>
zip = |as, bs| ...
```

---

## 6. Effects

### 6.1 Effect Declarations

Effects are declared as capabilities:

```baseline
effect Console {
  print! : String -> ()
  read_line! : () -> String
}

effect Http {
  get! : String -> Result<Response, HttpError>
  post! : (String, Body) -> Result<Response, HttpError>
}

effect Time {
  now! : () -> Timestamp
  sleep! : Duration -> ()
}

effect Random {
  int! : (Int, Int) -> Int
  float! : () -> Float
  shuffle! : List<T> -> List<T>
}
```

### 6.2 Using Effects

Functions declare which effects they require:

```baseline
// Single effect
log_message! : String -> {Console} ()
log_message! = |msg| Console.print!("[LOG] ${msg}")

// Multiple effects
fetch_with_logging! : String -> {Http, Console} Result<Response, Error>
fetch_with_logging! = |url|
  Console.print!("Fetching ${url}")
  let response = Http.get!(url)?
  Console.print!("Got ${response.status}")
  Ok(response)

// Effect polymorphism
timed! : (() -> {e} T) -> {e, Time} (T, Duration)
timed! = |action|
  let start = Time.now!()
  let result = action()
  let elapsed = Time.now!() - start
  (result, elapsed)
```

### 6.3 Effect Handlers

Effects are provided at the edges of the program:

```baseline
main! : () -> ()
main! =
  let http = Http.default()
  let console = Console.stdout()
  let time = Time.system()

  run_app!() with { http, console, time }

// Custom handlers for testing
test "fetch user" =
  let mock_http = Http.mock([
    ("/users/1", Ok({ id: 1, name: "Alice" }))
  ])
  let mock_console = Console.buffer()

  let result = fetch_with_logging!("/users/1")
    with { http: mock_http, console: mock_console }

  expect result == Ok({ id: 1, name: "Alice" })
  expect String.contains(mock_console.output, "Fetching")
```

### 6.4 Effect Inference

When effects are not explicitly declared, they are inferred:

```baseline
// Effects inferred as {Http, Console}
fetch_and_print! = |url|
  let data = Http.get!(url)?
  Console.print!(data.body)
  Ok(())
```

### 6.5 Pure Functions

Functions without the `!` suffix are pure and cannot use effects:

```baseline
// Pure function: no effects allowed
add : (Int, Int) -> Int
add = |a, b| a + b

// This would be a compile error:
bad : Int -> Int
bad = |x|
  Console.print!("x is ${x}")  // Error: pure function cannot use effects
  x
```

### 6.6 Built-in Effects

| Effect | Description |
|--------|-------------|
| `Console` | Terminal I/O |
| `Http` | HTTP client |
| `Fs` | File system |
| `Net` | TCP/UDP sockets |
| `Db` | Database access |
| `Time` | Current time, delays |
| `Random` | Random number generation |
| `Env` | Environment variables |
| `Process` | Spawn processes |
| `Log` | Structured logging |
| `Metrics` | Observability metrics |

### 6.7 The Standard Prelude

To reduce boilerplate for scripts and simple programs, Baseline provides a **standard prelude** with default effect handlers:

```baseline
// WITHOUT prelude: explicit handler setup
module MyScript

import Baseline.Effects.*

main! : () -> ()
main! =
  let console = Console.stdout()
  let fs = Fs.system()
  let http = Http.default()
  let time = Time.system()
  let random = Random.system()
  let env = Env.system()

  run!() with { console, fs, http, time, random, env }

run! : () -> {Console, Fs, Http, Time, Random, Env} ()
run! =
  Console.print!("Hello!")
  // ... actual logic
```

```baseline
// WITH prelude: just write code
@prelude(script)
module MyScript

main! =
  Console.print!("Hello!")
  let data = Http.get!("https://api.example.com/data")?
  Fs.write_text!("output.txt", data.body)
```

#### Available Preludes

| Prelude | Effects Included | Use Case |
|---------|------------------|----------|
| `script` | Console, Fs, Http, Time, Random, Env, Process | CLI tools, scripts |
| `server` | Http, Db, Log, Time, Metrics, Env | Web servers |
| `minimal` | Console, Time | Simple programs |
| `pure` | (none) | Pure computation only |

```baseline
@prelude(server)
module MyApi

// Db, Http, Log, etc. are all available without setup
main! =
  let app = Router.new()
    |> Router.get("/health", || Ok({ status: "ok" }))
    |> Router.get("/users", || Db.query!("SELECT * FROM users"))

  Server.listen!(8080, app)
```

#### Custom Preludes

Projects can define their own preludes:

```baseline
// In baseline.toml
[prelude.mycompany]
effects = ["Console", "Log", "Http", "Db", "Metrics"]
handlers = {
  Log = "MyCompany.Logging.structured",
  Metrics = "MyCompany.Observability.datadog"
}
```

```baseline
@prelude(mycompany)
module MyService

// Uses company-standard logging and metrics automatically
main! =
  Log.info!("Starting service")
  Metrics.increment!("service.starts")
  ...
```

This dramatically reduces boilerplate for LLM-generated code while maintaining explicit effect tracking.

---

### 6.8 Row Polymorphism

Baseline's effect system is built on **row polymorphism**. This enables:

1.  **Inference**: Functions that pass effects through don't need explicit annotations.
2.  **Composition**: Effects combine naturally without the ordering issues of Monad Transformers.

```baseline
// 'e' is a row variable capturing "other effects"
// map works regardless of what effects 'f' performs
map : (List<a, e>, a -> {e} b) -> {e} List<b, e>
```

### 6.9 Direct Style

Baseline compiles algebraic effects to **Direct Style** code (using standard control flow or delimited continuations), avoiding the "colored function" problem of async/await.

- **No `async`/`await` keywords**: Asynchronous IO is just an effect.
- **Unified abstraction**: Async, Generators, and Exceptions are all just Effects.

## 7. Modules

### 7.1 Module Declaration

Each file is a module. The module name matches the file path:

```baseline
// File: src/api/users.bl

@module Api.Users

// Module contents...
```

### 7.2 Imports

```baseline
// Import entire module
import Http
Http.get!(url)

// Import specific items
import Http.{ get!, post! }
get!(url)

// Import with alias
import Http as H
H.get!(url)

// Import all (use sparingly)
import Http.*
get!(url)
```

### 7.3 Exports

By default, all top-level definitions are private. Use `export` to make them public:

```baseline
// Public
export greet : String -> String
greet = |name| "Hello, ${name}"

// Private (not exported)
helper : String -> String
helper = |s| String.trim(s)

// Export types
export type User = { name: String, age: Int }
export type Status = Active | Inactive

// Export effect
export effect MyEffect { ... }
```

### 7.4 Module Organization

```
my-project/
├── baseline.toml         // Project configuration
├── src/
│   ├── main.bl     // Entry point
│   ├── lib.bl      // Library root (optional)
│   ├── api/
│   │   ├── mod.bl  // Api module
│   │   ├── users.bl
│   │   └── posts.bl
│   └── util/
│       └── helpers.bl
└── test/
    └── api_test.bl
```

### 7.5 Visibility

```baseline
// Public: accessible from any module
export fn

// Private: accessible only within this module
fn

// Internal: accessible within the same package (future)
internal fn
```

---

## 8. Specifications

### 8.1 Function Specifications

Specifications declare the contract a function must satisfy:

```baseline
@spec divide
@given numerator: Int, denominator: Int where denominator != 0
@returns Int
@ensures result * denominator <= numerator
@ensures result * denominator > numerator - denominator

divide : (Int, Int) -> Int
divide = |n, d| n / d
```

Specification attributes:

| Attribute | Description |
|-----------|-------------|
| `@spec name` | Names the specification |
| `@given` | Declares inputs with optional refinements |
| `@returns` | Declares output type |
| `@requires` | Preconditions (alternative to refinements) |
| `@ensures` | Postconditions |
| `@effects` | Declares required effects |
| `@pure` | Asserts function is pure |
| `@total` | Asserts function terminates for all inputs |

### 8.2 Type Specifications

```baseline
@spec User
@invariant self.age >= 0
@invariant String.contains(self.email, "@")

type User = {
  name: String where String.len(self) > 0,
  age: Int where self >= 0,
  email: String where String.matches(self, r".+@.+"),
}
```

### 8.3 API Specifications

For web APIs, Baseline provides first-class API specs:

```baseline
@api UserApi

type Endpoints = {
  @route "GET /users"
  @returns List<User>
  list_users,

  @route "GET /users/:id"
  @param id: Int @positive
  @returns User
  @error 404 NotFound
  get_user,

  @route "POST /users"
  @body UserCreate
  @returns User
  @error 400 ValidationError
  create_user,

  @route "DELETE /users/:id"
  @param id: Int @positive
  @returns ()
  @error 404 NotFound
  delete_user,
}

// Implementation
implement Endpoints = {
  list_users = || Db.query!("SELECT * FROM users"),

  get_user = |id|
    Db.query_one!("SELECT * FROM users WHERE id = ?", id)
    |> Option.ok_or(NotFound),

  create_user = |body|
    body |> validate? |> Db.insert!("users"),

  delete_user = |id|
    Db.delete!("users", id) |> Bool.then(() | NotFound),
}
```

### 8.4 Specification Verification

The compiler verifies specifications using SMT solving:

```baseline
$ baseline check

Checking specifications...
  ✓ divide: precondition verified
  ✓ divide: postcondition verified
  ✓ User: invariants verified
  ✓ UserApi: all routes type-check

Specifications: 4 verified, 0 failed
```

When verification fails:

```baseline
$ baseline check

Checking specifications...
  ✗ divide: postcondition may fail

    Counter-example found:
      numerator = 7
      denominator = 3
      result = 2

    Postcondition violated:
      result * denominator <= numerator  // 6 <= 7 ✓
      result * denominator > numerator - denominator  // 6 > 4 ✓

    Note: Verification inconclusive, may be false positive.
    Add @assume or refine specification.
```

### 8.5 Verification Levels

Baseline supports multiple verification levels to balance thoroughness with speed:

```baseline
// Module-level default
@verify(level: refinements)
module MyModule

// Function-level override
@verify(level: full)
critical_function : Input -> Output
critical_function = |input| ...

// Skip verification (escape hatch)
@verify(skip: "Performance critical, manually audited 2024-01-15")
unsafe_but_fast : Data -> Data
unsafe_but_fast = |data| ...
```

#### Verification Level Definitions

| Level | Checks | Speed | Use Case |
|-------|--------|-------|----------|
| `types` | Type inference, exhaustiveness | Fast (~ms) | Rapid iteration |
| `refinements` | Types + refinement constraints | Medium (~100ms) | Development default |
| `full` | Types + refinements + all specs + SMT | Slow (~seconds) | CI, release |
| `skip` | Types only, specs unchecked | Fast | Escape hatch |

```bash
# Command line overrides
baseline check                    # Uses level from source annotations
baseline check --level=types      # Fast check only
baseline check --level=full       # Full verification
baseline check --timeout=30s      # Set SMT timeout
```

#### Handling SMT Timeouts

When the SMT solver cannot prove a property within the timeout:

```baseline
$ baseline check --level=full

Checking specifications...
  ? process_data: verification timeout after 10s

    The solver could not prove or disprove:
      @ensures result.len <= input.len * 2

    Options:
      1. Increase timeout: baseline check --timeout=60s
      2. Add intermediate lemma to help the solver
      3. Mark as assumed: @assume result.len <= input.len * 2
      4. Skip verification: @verify(skip: "...")
```

#### Assumptions

When you know something the solver cannot prove:

```baseline
@spec transform
@given data: List<Item>
@returns List<Result>
@assume db_items_are_valid  // External invariant
@ensures result.len == data.len

transform : List<Item> -> {Db} List<Result>
transform = |data| ...
```

Assumptions are:
- Tracked by the compiler
- Reported in verification summaries
- Candidates for future formal proofs

---

### 8.6 The Neurosymbolic Feedback Loop

Baseline is architected to close the loop between LLM generation and formal verification.

1.  **Prompt**: The Agent is given a type signature with refinements.
    `split_bill : (Total: Money, People: Int where self > 0) -> List<Money>`
2.  **Generation**: The Agent generates code.
3.  **Verification**: The compiler checks the code against the refinement types using SMT.
4.  **Feedback**: If verification fails, the compiler produces a **counter-example** (e.g., "Verification failed when People = 0").
5.  **Repair**: The Agent attempts to fix the code (e.g., adding a check `if people == 0`) based on the counter-example.

This turns the compiler into a **verifier** for the probabilistic output of the LLM.

## 9. Testing

Baseline provides a comprehensive testing framework inspired by RSpec and Vitest, with first-class support for BDD-style specifications and inline unit tests.

### 9.1 Testing Philosophy

Baseline testing follows three principles:

1. **Specs are documentation** — Tests should read like requirements
2. **Given-When-Then structure** — Separates setup, action, and assertion
3. **LLM-friendly** — Structured format enables AI generation and verification

### 9.2 Inline Tests (Unit Tests)

For simple pure functions, inline tests in `where` blocks:

```baseline
add : (Int, Int) -> Int
add = |a, b| a + b
where
  test "adds positive numbers" = add(1, 2) == 3
  test "handles negatives" = add(-1, 1) == 0
  test "zero identity" = add(0, 5) == 5
```

These are best for:
- Pure utility functions
- Simple transformations
- Quick sanity checks

### 9.3 BDD Specifications

For APIs, business logic, and complex behavior, use `describe`/`it` blocks with Given-When-Then:

#### Basic Structure

```baseline
describe "List.sort" {
  it "sorts numbers in ascending order" {
    given [3, 1, 4, 1, 5]
    when List.sort
    expect [1, 1, 3, 4, 5]
  }

  it "handles empty lists" {
    given []
    when List.sort
    expect []
  }

  it "handles single element" {
    given [42]
    when List.sort
    expect [42]
  }
}
```

#### Nested Contexts

```baseline
describe "UserService" {
  describe "create_user" {
    context "with valid data" {
      it "creates the user" {
        given { name: "Alice", email: "alice@example.com" }
        when create_user
        expect Ok(User { id: _, name: "Alice", email: "alice@example.com" })
      }

      it "assigns a unique id" {
        given { name: "Alice", email: "alice@example.com" }
        when create_user
        expect Ok(User { id: id }) where id > 0
      }
    }

    context "with invalid email" {
      it "returns validation error" {
        given { name: "Alice", email: "not-an-email" }
        when create_user
        expect Err(ValidationError { field: "email", .. })
      }
    }

    context "with duplicate email" {
      before {
        Db.insert!(User { id: 1, name: "Existing", email: "taken@example.com" })
      }

      it "returns conflict error" {
        given { name: "New User", email: "taken@example.com" }
        when create_user
        expect Err(EmailExists)
      }
    }
  }
}
```

#### Fixtures with `let`

Lazy-evaluated fixtures for test data:

```baseline
describe "PostService" {
  // Fixtures - evaluated lazily, cached per test
  let alice = User { id: 1, name: "Alice", role: Admin }
  let bob = User { id: 2, name: "Bob", role: Member }
  let alice_post = Post { id: 1, author_id: alice.id, content: "Hello" }
  let bob_post = Post { id: 2, author_id: bob.id, content: "World" }

  describe "delete_post" {
    context "when user owns the post" {
      it "deletes successfully" {
        given { user: alice, post: alice_post }
        when delete_post
        expect Ok(())
      }
    }

    context "when user does not own the post" {
      it "returns unauthorized" {
        given { user: bob, post: alice_post }
        when delete_post
        expect Err(Unauthorized)
      }
    }

    context "when user is admin" {
      it "can delete any post" {
        given { user: alice, post: bob_post }
        when delete_post
        expect Ok(())
      }
    }
  }
}
```

#### Setup and Teardown Hooks

```baseline
describe "Database operations" {
  // Runs once before all tests in this describe
  before_all {
    Db.migrate!()
  }

  // Runs before each test
  before_each {
    Db.begin_transaction!()
  }

  // Runs after each test
  after_each {
    Db.rollback!()
  }

  // Runs once after all tests
  after_all {
    Db.cleanup!()
  }

  it "inserts records" {
    given User { name: "Test", email: "test@test.com" }
    when Db.insert
    expect Ok(_)
  }
}
```

#### Mocking Effects with `with`

```baseline
describe "WeatherService" {
  let mock_http = Http.mock([
    ("https://api.weather.com/current", Ok({ temp: 72, conditions: "sunny" })),
    ("https://api.weather.com/forecast", Ok({ days: [...] })),
  ])

  it "fetches current weather" {
    with { http: mock_http }
    given "New York"
    when get_current_weather
    expect Ok({ temp: 72, conditions: "sunny" })
  }

  it "handles API errors gracefully" {
    with { http: Http.mock_error(Timeout) }
    given "New York"
    when get_current_weather
    expect Err(ServiceUnavailable)
  }
}
```

### 9.4 Async and Effectful Tests

```baseline
describe "async operations" {
  it "fetches data concurrently" {
    with { async: Async.runtime(), http: Http.default() }

    given [
      "https://api.example.com/users",
      "https://api.example.com/posts",
    ]
    when |urls| parallel!(List.map(urls, Http.get!))
    expect [Ok(_), Ok(_)]
  }

  it "times out slow requests" {
    with {
      async: Async.runtime(),
      http: Http.mock_delay(5.seconds),
    }

    given "https://slow.example.com"
    when |url| Http.get!(url) |> Result.timeout(1.second)
    expect Err(Timeout)
  }
}
```

### 9.5 Test Organization

#### File Structure

```
my-project/
├── src/
│   ├── user.bl         // Contains inline tests
│   └── api.bl          // Contains inline tests
└── test/
    ├── user_spec.bl    // BDD specs for user module
    ├── api_spec.bl     // BDD specs for api module
    ├── integration/
    │   └── full_flow_spec.bl
    └── fixtures/
        └── test_data.bl
```

#### Focused and Skipped Tests

```baseline
describe "Feature" {
  // Only run this test (for debugging)
  it.only "focused test" {
    ...
  }

  // Skip this test
  it.skip "not implemented yet" {
    ...
  }

  // Skip with reason
  it.skip("waiting on API v2") "new feature" {
    ...
  }
}
```

### 9.6 Assertions and Matchers

```baseline
describe "Matchers" {
  it "supports various matchers" {
    // Equality
    expect 1 + 1 to_equal 2
    expect result to_be Ok(_)

    // Comparison
    expect count to_be_greater_than 0
    expect age to_be_between 0 and 120

    // Collections
    expect list to_contain 42
    expect list to_have_length 3
    expect list to_be_empty

    // Strings
    expect message to_start_with "Error:"
    expect message to_contain "not found"
    expect message to_match r"user \d+"

    // Results and Options
    expect result to_be_ok
    expect result to_be_err
    expect option to_be_some
    expect option to_be_none

    // Types
    expect value to_be_type User
    expect error to_be_type ValidationError

    // Custom matchers
    expect user to_satisfy |u| u.age >= 18
  }
}
```

### 9.7 Test Output for LLMs

JSON output for LLM integration:

```bash
$ baseline test --format json
```

```json
{
  "summary": {
    "total": 10,
    "passed": 9,
    "failed": 1,
    "skipped": 0,
    "duration_ms": 142
  },
  "failures": [
    {
      "name": "UserService > delete_post > returns unauthorized",
      "location": "test/user_spec.bl:45",
      "given": { "user": "bob", "post": "alice_post" },
      "when": "delete_post",
      "expected": "Err(Unauthorized)",
      "actual": "Err(NotFound)",
      "diff": {
        "type": "variant_mismatch",
        "expected_variant": "Unauthorized",
        "actual_variant": "NotFound"
      },
      "suggestions": [
        "Check authorization logic runs before existence check",
        "Verify post lookup includes author_id"
      ]
    }
  ]
}
```

### 9.8 Best Practices

#### Spec-First Development (Recommended for LLMs)

```baseline
// 1. Write the spec first
describe "PasswordService" {
  describe "validate_password" {
    it "accepts strong passwords" {
      given "Str0ng!Pass"
      when validate_password
      expect Ok(())
    }

    it "rejects short passwords" {
      given "short"
      when validate_password
      expect Err(TooShort { min: 8, actual: 5 })
    }

    it "requires uppercase" {
      given "alllowercase1!"
      when validate_password
      expect Err(MissingUppercase)
    }

    it "requires number" {
      given "NoNumbersHere!"
      when validate_password
      expect Err(MissingNumber)
    }
  }
}

// 2. Then implement to satisfy the spec
validate_password : String -> Result<(), PasswordError>
validate_password = |password|
  if String.len(password) < 8 then
    Err(TooShort { min: 8, actual: String.len(password) })
  else if not String.any(password, Char.is_uppercase) then
    Err(MissingUppercase)
  else if not String.any(password, Char.is_digit) then
    Err(MissingNumber)
  else
    Ok(())
```

#### Test Naming

```baseline
// Good: Describes behavior
it "returns unauthorized when user lacks permission" { ... }
it "retries failed requests up to 3 times" { ... }
it "caches results for 5 minutes" { ... }

// Bad: Describes implementation
it "calls check_permission function" { ... }
it "uses retry loop" { ... }
it "sets cache TTL" { ... }
```

#### One Assertion Per Test

```baseline
// Good: One clear expectation
it "creates user with correct name" {
  given { name: "Alice", email: "alice@test.com" }
  when create_user
  expect Ok(User { name: "Alice", .. })
}

it "creates user with correct email" {
  given { name: "Alice", email: "alice@test.com" }
  when create_user
  expect Ok(User { email: "alice@test.com", .. })
}

// Acceptable: Related assertions in one test
it "creates user with provided data" {
  given { name: "Alice", email: "alice@test.com" }
  when create_user
  expect Ok(user)
  expect user.name to_equal "Alice"
  expect user.email to_equal "alice@test.com"
  expect user.id to_be_greater_than 0
}
```

---

## 10. Language Server Protocol and Compiler API

### 10.1 Overview

Baseline exposes its compiler internals via a queryable API, enabling IDEs, LLMs, and automated tools to interact with the language semantically. This goes beyond traditional LSP to support AI-assisted development.

### 10.2 Standard LSP Features

Baseline implements the full Language Server Protocol:

- **Diagnostics**: Errors, warnings, hints
- **Completion**: Context-aware suggestions
- **Hover**: Type information and documentation
- **Go to Definition**: Navigate to declarations
- **Find References**: All usages of a symbol
- **Rename**: Safe refactoring
- **Code Actions**: Quick fixes and refactorings
- **Formatting**: baseline fmt integration

### 10.3 Extended Query API

Beyond standard LSP, Baseline provides semantic queries:

#### Type Queries

```json
// Request: What type does this expression have?
{
  "method": "baseline/typeAt",
  "params": {
    "file": "src/api.bl",
    "position": { "line": 42, "character": 15 }
  }
}

// Response
{
  "type": "Option<User>",
  "expanded": "Some(User) | None",
  "refinements": [],
  "effects": []
}
```

#### Function Search

```json
// Request: Find functions matching a type signature
{
  "method": "baseline/searchByType",
  "params": {
    "signature": "List<A> -> (A -> B) -> List<B>",
    "scope": "visible"  // or "all", "module", "project"
  }
}

// Response
{
  "matches": [
    {
      "name": "List.map",
      "module": "Baseline.Collections",
      "signature": "List<A> -> (A -> B) -> List<B>",
      "doc": "Applies a function to each element..."
    },
    {
      "name": "List.filter_map",
      "module": "Baseline.Collections",
      "signature": "List<A> -> (A -> B?) -> List<B>",
      "doc": "Maps and filters in one pass..."
    }
  ]
}
```

#### Effect Queries

```json
// Request: What effects are available in this scope?
{
  "method": "baseline/availableEffects",
  "params": {
    "file": "src/api.bl",
    "position": { "line": 42, "character": 0 }
  }
}

// Response
{
  "effects": [
    { "name": "Http", "methods": ["get!", "post!", "put!", "delete!"] },
    { "name": "Db", "methods": ["query!", "execute!", "transaction!"] },
    { "name": "Log", "methods": ["debug!", "info!", "warn!", "error!"] }
  ]
}
```

#### Specification Queries

```json
// Request: What spec does this function need to satisfy?
{
  "method": "baseline/specFor",
  "params": {
    "function": "Api.Users.create_user"
  }
}

// Response
{
  "spec": {
    "given": [
      { "name": "body", "type": "UserCreate", "refinements": ["name.len > 0"] }
    ],
    "returns": { "type": "User | ValidationError" },
    "ensures": ["result.id > 0 when Ok"],
    "effects": ["Db"]
  }
}
```

### 10.4 Programmatic Compilation API

For build tools and LLM integrations:

```json
// Request: Check code without writing to disk
{
  "method": "baseline/checkSource",
  "params": {
    "source": "let x: Int = \"hello\"",
    "context": {
      "imports": ["Baseline.Core.*"],
      "effects": ["Console"]
    }
  }
}

// Response
{
  "status": "error",
  "diagnostics": [
    {
      "severity": "error",
      "code": "E0308",
      "message": "Type mismatch",
      "location": { "line": 1, "col": 14, "span": 7 },
      "expected": "Int",
      "actual": "String",
      "suggestions": [
        {
          "action": "parse",
          "code": "let x: Int = \"hello\".parse()?",
          "confidence": 0.6
        },
        {
          "action": "change_type",
          "code": "let x: String = \"hello\"",
          "confidence": 0.8
        }
      ]
    }
  ]
}
```

### 10.5 Interactive Refinement Session

For LLM integration, a stateful session API:

```json
// Start a session
{ "method": "baseline/session/start", "params": { "project": "myapp" } }
// Response: { "sessionId": "abc123" }

// Submit code for checking
{
  "method": "baseline/session/check",
  "params": {
    "sessionId": "abc123",
    "file": "src/api.bl",
    "source": "..."
  }
}

// Ask about verification failures
{
  "method": "baseline/session/query",
  "params": {
    "sessionId": "abc123",
    "question": "Why can't you prove user.age >= 0?"
  }
}

// Response
{
  "answer": "The field `age` in type `User` has type `Int` with no refinement. The database schema shows `age INT` which allows negative values.",
  "suggestions": [
    "Add CHECK constraint to database: age >= 0",
    "Add refinement to type: age: Int where self >= 0",
    "Add runtime guard: if user.age < 0 then return Err(InvalidAge)"
  ]
}

// End session
{ "method": "baseline/session/end", "params": { "sessionId": "abc123" } }
```

### 10.6 Bulk Operations for LLM Agents

For agents processing multiple files:

```json
// Request: Analyze entire module
{
  "method": "baseline/analyzeModule",
  "params": {
    "module": "Api.Users",
    "include": ["types", "functions", "specs", "tests"]
  }
}

// Response: Complete module information for context
{
  "module": "Api.Users",
  "types": [...],
  "functions": [...],
  "specs": [...],
  "tests": [...],
  "dependencies": [...],
  "dependents": [...]
}
```

---

## 11. Standard Library

### 11.1 Core Types

```baseline
module Baseline.Core

// Primitive operations
export (+), (-), (*), (/), (%), (**)
export (==), (!=), (<), (>), (<=), (>=)
export (&&), (||), not

// Option
export type Option<T> = Some(T) | None
export Option.map : (Option<T>, T -> U) -> Option<U>
export Option.and_then : (Option<T>, T -> Option<U>) -> Option<U>
export Option.unwrap : Option<T> -> T  // panics if None
export Option.unwrap_or : (Option<T>, T) -> T
export Option.ok_or : (Option<T>, E) -> Result<T, E>

// Result
export type Result<T, E> = Ok(T) | Err(E)
export Result.map : (Result<T, E>, T -> U) -> Result<U, E>
export Result.map_err : (Result<T, E>, E -> F) -> Result<T, F>
export Result.and_then : (Result<T, E>, T -> Result<U, E>) -> Result<U, E>
export Result.unwrap : Result<T, E> -> T  // panics if Err
export Result.unwrap_or : (Result<T, E>, T) -> T
```

### 11.2 Collections

```baseline
module Baseline.Collections

// List
export type List<T>
export List.empty : () -> List<T>
export List.singleton : T -> List<T>
export List.len : List<T> -> Int
export List.get : (List<T>, Int) -> T?
export List.first : List<T> -> T?
export List.last : List<T> -> T?
export List.map : (List<T>, T -> U) -> List<U>
export List.filter : (List<T>, T -> Bool) -> List<T>
export List.fold : (List<T>, U, (U, T) -> U) -> U
export List.find : (List<T>, T -> Bool) -> T?
export List.any : (List<T>, T -> Bool) -> Bool
export List.all : (List<T>, T -> Bool) -> Bool
export List.sort : List<T> -> List<T> where T: Ord
export List.reverse : List<T> -> List<T>
export List.concat : (List<T>, List<T>) -> List<T>
export (++) : (List<T>, List<T>) -> List<T>

// Map
export type Map<K, V>
export Map.empty : () -> Map<K, V>
export Map.singleton : (K, V) -> Map<K, V>
export Map.insert : (Map<K, V>, K, V) -> Map<K, V>
export Map.get : (Map<K, V>, K) -> V?
export Map.remove : (Map<K, V>, K) -> Map<K, V>
export Map.contains : (Map<K, V>, K) -> Bool
export Map.keys : Map<K, V> -> List<K>
export Map.values : Map<K, V> -> List<V>

// Set
export type Set<T>
export Set.empty : () -> Set<T>
export Set.singleton : T -> Set<T>
export Set.insert : (Set<T>, T) -> Set<T>
export Set.remove : (Set<T>, T) -> Set<T>
export Set.contains : (Set<T>, T) -> Bool
export Set.union : (Set<T>, Set<T>) -> Set<T>
export Set.intersection : (Set<T>, Set<T>) -> Set<T>
```

### 11.3 Text

```baseline
module Baseline.Text

export String.len : String -> Int
export String.is_empty : String -> Bool
export String.chars : String -> List<Char>
export String.bytes : String -> List<Int>
export String.split : (String, String) -> List<String>
export String.join : (List<String>, String) -> String
export String.trim : String -> String
export String.starts_with : (String, String) -> Bool
export String.ends_with : (String, String) -> Bool
export String.contains : (String, String) -> Bool
export String.replace : (String, String, String) -> String
export String.to_upper : String -> String
export String.to_lower : String -> String
export String.matches : (String, Regex) -> Bool
export String.find_all : (String, Regex) -> List<Match>

export type Regex
export Regex.new : String -> Result<Regex, RegexError>
export Regex.is_match : (Regex, String) -> Bool
export Regex.captures : (Regex, String) -> List<String>?
```

### 11.4 IO

```baseline
module Baseline.IO

export effect Fs {
  read! : Path -> Result<Bytes, IoError>
  read_text! : Path -> Result<String, IoError>
  write! : (Path, Bytes) -> Result<(), IoError>
  write_text! : (Path, String) -> Result<(), IoError>
  append! : (Path, Bytes) -> Result<(), IoError>
  delete! : Path -> Result<(), IoError>
  exists! : Path -> Bool
  list_dir! : Path -> Result<List<Path>, IoError>
  create_dir! : Path -> Result<(), IoError>
  metadata! : Path -> Result<Metadata, IoError>
}

export type Path
export Path.from_str : String -> Path
export Path.join : (Path, String) -> Path
export Path.parent : Path -> Path?
export Path.file_name : Path -> String?
export Path.extension : Path -> String?

export type Metadata = {
  size: Int,
  is_file: Bool,
  is_dir: Bool,
  modified: Timestamp,
  created: Timestamp,
}
```

---

## 12. Grammar

### 12.1 Notation

This grammar uses the following notation:

- `'text'` — Terminal (literal text)
- `Name` — Non-terminal
- `A | B` — Alternative
- `A?` — Optional
- `A*` — Zero or more
- `A+` — One or more
- `(A B)` — Grouping

### 12.2 Lexical Grammar

```ebnf
(* Whitespace and comments *)
whitespace = ' ' | '\t' | '\n' | '\r'
line_comment = '//' (~'\n')* '\n'
block_comment = '/*' (block_comment | ~'*/')* '*/'

(* Identifiers *)
lower_ident = lower (lower | upper | digit | '_')*
upper_ident = upper (lower | upper | digit | '_')*
effect_ident = upper_ident '!'?

lower = 'a'..'z'
upper = 'A'..'Z'
digit = '0'..'9'

(* Literals *)
int_lit = '-'? digit ('_'? digit)*
        | '0x' hex_digit ('_'? hex_digit)*
        | '0b' bin_digit ('_'? bin_digit)*
        | '0o' oct_digit ('_'? oct_digit)*

float_lit = '-'? digit+ '.' digit+ (('e' | 'E') ('+' | '-')? digit+)?

string_lit = '"' string_char* '"'
           | 'r"' (~'"')* '"'
           | 'r#"' (~'"#')* '"#'
           | '"""' (~'"""')* '"""'

string_char = ~('"' | '\\' | '$')
            | '\\' escape_char
            | '${' expression '}'

char_lit = '\'' (char_char | escape_char) '\''

bool_lit = 'true' | 'false'

(* Operators *)
operator = '+' | '-' | '*' | '/' | '%' | '**'
         | '==' | '!=' | '<' | '>' | '<=' | '>='
         | '&&' | '||' | 'not'
         | '|>' | '<|' | '>>' | '<<'
         | '?'
         | '++' | '..'
```

### 12.3 Syntactic Grammar

```ebnf
(* Module *)
module = module_decl? import* declaration*

module_decl = '@module' module_path

module_path = upper_ident ('.' upper_ident)*

import = 'import' module_path import_spec?

import_spec = '.' '{' ident (',' ident)* '}'
            | '.' '*'
            | 'as' upper_ident

(* Declarations *)
declaration = type_decl
            | effect_decl
            | function_decl
            | spec_decl

(* Type declarations *)
type_decl = 'export'? 'type' upper_ident type_params? '=' type_body where_clause?

type_params = '<' upper_ident (',' upper_ident)* '>'

type_body = type_expr
          | variant_list
          | record_type

variant_list = '|'? variant ('|' variant)*

variant = upper_ident ('(' type_expr (',' type_expr)* ')')?

record_type = '{' record_field (',' record_field)* ','? '}'

record_field = lower_ident ':' type_expr refinement?

refinement = 'where' expression

(* Type expressions *)
type_expr = type_primary ('->' type_expr)?
          | type_primary '?'

type_primary = upper_ident type_args?
             | lower_ident
             | '(' type_expr (',' type_expr)* ')'
             | '{' effect_list '}' type_expr
             | record_type

type_args = '<' type_expr (',' type_expr)* '>'

effect_list = upper_ident (',' upper_ident)*

(* Effect declarations *)
effect_decl = 'export'? 'effect' upper_ident '{' effect_method* '}'

effect_method = lower_ident '!' ':' type_expr

(* Function declarations *)
function_decl = 'export'? lower_ident '!'? ':' type_expr
                lower_ident '!'? '=' expression where_clause?

(* Specification declarations *)
spec_decl = '@spec' lower_ident spec_attr*

spec_attr = '@given' param_list
          | '@returns' type_expr
          | '@requires' expression
          | '@ensures' expression
          | '@effects' '{' effect_list '}'
          | '@pure'
          | '@total'

(* Expressions *)
expression = let_expr
           | if_expr
           | match_expr
           | try_expr
           | lambda_expr
           | pipe_expr

let_expr = 'let' pattern (':' type_expr)? '=' expression expression?

if_expr = 'if' expression 'then' expression 'else' expression

match_expr = 'match' expression match_arm+

match_arm = pattern guard? '->' expression

guard = 'if' expression

try_expr = 'try' expression 'catch' match_arm+

lambda_expr = '|' param_list '|' expression

param_list = pattern (',' pattern)*

pipe_expr = or_expr ('|>' or_expr)*

or_expr = and_expr ('||' and_expr)*

and_expr = cmp_expr ('&&' cmp_expr)*

cmp_expr = add_expr (cmp_op add_expr)*

cmp_op = '==' | '!=' | '<' | '>' | '<=' | '>='

add_expr = mul_expr (('+' | '-' | '++') mul_expr)*

mul_expr = pow_expr (('*' | '/' | '%') pow_expr)*

pow_expr = unary_expr ('**' pow_expr)?

unary_expr = ('not' | '-')? postfix_expr

postfix_expr = primary_expr postfix_op*

postfix_op = '(' arg_list ')'
           | '.' lower_ident
           | '.' int_lit
           | '?'

primary_expr = lower_ident
             | upper_ident
             | literal
             | '(' expression (',' expression)* ')'
             | '[' (expression (',' expression)*)? ']'
             | '{' (record_init (',' record_init)*)? '}'
             | '{' expression '}'

record_init = lower_ident ':' expression
            | '..' expression

arg_list = (arg (',' arg)*)?

arg = expression
    | lower_ident ':' expression

(* Patterns *)
pattern = '_'
        | lower_ident
        | literal
        | upper_ident ('(' pattern (',' pattern)* ')')?
        | '(' pattern (',' pattern)* ')'
        | '[' (pattern (',' pattern)*)? ('..' lower_ident?)? ']'
        | '{' field_pattern (',' field_pattern)* ('..' )? '}'
        | pattern '|' pattern

field_pattern = lower_ident (':' pattern)?

(* Where clauses *)
where_clause = 'where' where_item+

where_item = 'test' string_lit '=' expression
           | 'property' string_lit '=' expression
           | 'contract' string_lit '=' contract_body
           | 'fuzz' string_lit '=' expression

contract_body = 'given' expression 'when' expression 'then' expression

(* Literals *)
literal = int_lit | float_lit | string_lit | char_lit | bool_lit | '()'
```

---

## Appendix A: Compiler Errors

All compiler errors follow this JSON schema:

```json
{
  "type": "object",
  "properties": {
    "error": {
      "type": "object",
      "properties": {
        "type": { "type": "string" },
        "code": { "type": "string" },
        "location": {
          "type": "object",
          "properties": {
            "file": { "type": "string" },
            "line": { "type": "integer" },
            "col": { "type": "integer" },
            "span": { "type": "integer" }
          }
        },
        "message": { "type": "string" },
        "context": { "type": "object" },
        "suggestions": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "action": { "type": "string" },
              "code": { "type": "string" },
              "confidence": { "type": "number" }
            }
          }
        }
      }
    }
  }
}
```

---

## Appendix B: Trace Format

Traces are stored in a binary format for efficiency. The schema:

```
TraceFile {
  magic: [u8; 4] = "RKTC"
  version: u16
  flags: u16
  event_count: u64
  events: [TraceEvent; event_count]
}

TraceEvent {
  timestamp_ns: u64
  location: Location
  event_type: EventType
  payload: [u8; *]
}

Location {
  file_id: u16
  line: u32
  col: u16
}

EventType {
  FunctionCall = 0x01
  FunctionReturn = 0x02
  EffectExecuted = 0x03
  VariableAssign = 0x04
  BranchTaken = 0x05
  Allocation = 0x06
  Deallocation = 0x07
  Error = 0x08
}
```

---

*End of Specification*
