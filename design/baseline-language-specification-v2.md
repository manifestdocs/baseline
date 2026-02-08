# Baseline Language Specification

**Version 0.2.0 — Draft**

> A fast, verifiable, dual-audience programming language: native to both LLMs and humans.

> For aspirational features planned for v0.3 and beyond (memory model, compilation targets, advanced testing, concurrency, etc.), see [baseline-language-vision.md](baseline-language-vision.md).

---

## Revision Notes (v0.1 → v0.2)

This revision incorporates findings from empirical LLM code generation research, constrained decoding literature, and experimental LLM-native language projects (MoonBit, NERD, ALaS). Key changes:

- **Added**: Constrained Generation Protocol (type-constrained decoding during token generation)
- **Added**: LLM Bootstrap Kit (`llms.txt`, canonical few-shot examples, training corpus strategy)
- **Added**: Explicit dual-audience design rationale throughout
- **Added**: Typed holes with compiler-guided completion
- **Revised**: Function definition syntax (empirical compatibility with LLM statistical priors)
- **Revised**: Testing syntax (removed `given`/`when` ambiguity, explicit application)
- **Revised**: Verification level interaction with LLM feedback loop
- **Revised**: Row polymorphism scoping (bounded at module boundaries)
- **Preserved**: Effect system as capability-based security (strongest feature)
- **Preserved**: Refinement types and Parse Don't Validate
- **Preserved**: One Way Principle and structured compiler feedback
- **Preserved**: Interactive Refinement Protocol (extended)

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Lexical Structure](#2-lexical-structure)
3. [Types](#3-types)
4. [Expressions](#4-expressions)
5. [Functions](#5-functions)
6. [Effects](#6-effects)
7. [Modules](#7-modules)
8. [Specifications](#8-specifications)
9. [Testing](#9-testing)
10. [Language Server Protocol and Compiler API](#10-language-server-protocol-and-compiler-api)
11. [Constrained Generation Protocol](#11-constrained-generation-protocol)
12. [LLM Bootstrap Kit](#12-llm-bootstrap-kit)
13. [Standard Library](#13-standard-library)
14. [Grammar](#14-grammar)
- [Appendix A: Compiler Errors](#appendix-a-compiler-errors)
- [Appendix B: Trace Format](#appendix-b-trace-format)
- [Appendix C: Dual-Audience Design Rationale](#appendix-c-dual-audience-design-rationale)

---

## 1. Introduction

### 1.1 Design Philosophy

Baseline is designed around three core principles:

1. **The type is the spec** — Types encode enough information that correctness can be verified, tests can be generated, and documentation is always accurate.

2. **Effects are data** — Side effects are explicit, trackable, and mockable. Pure functions are the default; effects are opt-in capabilities.

3. **Dual-audience native** — The language is designed for both machine generation and human comprehension. Syntax is unambiguous, errors are structured, and every design choice is evaluated against both audiences. Where the interests of LLMs and humans align (which is most of the time), we optimize aggressively. Where they conflict, we document the tradeoff explicitly.

### 1.2 Goals

- **Fast compilation**: Development builds in <200ms
- **Fast execution**: Compiled to native with zero-cost effects and refinements
- **Small binaries**: Typical services <5MB
- **Verified correctness**: Specifications checked at compile time
- **Portable deployment**: Primary target is WebAssembly
- **Constrained generation**: Type system supports real-time token filtering during LLM generation
- **Zero-shot generability**: Syntax close enough to ML-family languages that LLMs can generate valid Baseline without fine-tuning

### 1.3 Non-Goals

- Backward compatibility with existing languages
- Gradual typing or dynamic features
- Object-oriented programming
- Manual memory management (exposed to user)
- Syntax optimization at the expense of human readability
- Novel syntax where established conventions exist

### 1.4 Influences

Baseline draws from:

- **ML/OCaml**: Type inference, pattern matching, algebraic data types
- **Rust**: Ownership concepts (simplified), error handling, tooling quality, `fn` keyword
- **Koka**: Algebraic effects
- **Liquid Haskell**: Refinement types
- **Elm**: Friendly errors, simplicity
- **F#**: Pragmatic functional programming, pipeline operators
- **Go**: `gofmt`-style canonical formatting, simplicity
- **Dafny/SPARK**: First-class specifications and verification

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

fn listen!(port: Port) -> {Net} Server =
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
Apply suggested fix (highest confidence first)
      ↓
Retry (with verification level context)
```

#### 1.5.4 Capability-Based Security for Agents

The effect system serves as a **security sandbox** for autonomous agents:

```baseline
// An agent with this signature CANNOT:
// - Access the filesystem
// - Make unauthorized network requests
// - Delete data
// - Perform any effect not in the set

fn agent_task!(input: Input) -> {Log, Db.read} Output =
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

// Compiler responds with verification status and active verification level
{
  "status": "unverified",
  "verification_level": "refinements",
  "higher_level_warnings": [
    "Full verification has not been run. Use --level=full before merge."
  ],
  "obligations": [
    {
      "location": { "line": 12, "col": 5 },
      "property": "user.age >= 0",
      "context": "Required by NonNegative refinement",
      "suggestions": [
        { "action": "assume", "code": "@assume user.age >= 0", "confidence": 0.3 },
        { "action": "guard", "code": "if user.age < 0 then return Err(InvalidAge)", "confidence": 0.8 },
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

#### 1.5.6 Token-Efficient Design

Baseline's syntax is designed to minimize token consumption while maintaining readability:

- **Common ASCII keywords** that map to single BPE tokens (`fn`, `let`, `if`, `match`)
- **No exotic Unicode symbols** in core syntax
- **Type inference** at local scope (types are enforced, not repeatedly spelled out)
- **Canonical formatting** eliminates tokens wasted on stylistic decisions
- **Pipeline operators** reduce parenthesis nesting (fewer bracket-matching tokens)

Design guideline: when two syntax options are equally readable, prefer the one that uses fewer tokens. This is measured empirically against common BPE tokenizers (cl100k_base, o200k_base).

### 1.6 Dual-Audience Design

Baseline rejects the premise that LLM-optimized and human-optimized are opposing goals. The shared enemy is **ambiguity, implicitness, and action-at-a-distance** — which have always been the enemies of good software engineering regardless of who writes the code.

#### 1.6.1 Where Interests Align (Optimize Aggressively)

| Feature | LLM Benefit | Human Benefit |
|---------|-------------|---------------|
| Pipeline syntax | Left-to-right generation | Top-to-bottom reading |
| Explicit effects | Prevents hallucinated I/O | Makes side effects visible |
| Radical locality | Self-contained context windows | Self-contained comprehension |
| Canonical formatting | No tokens wasted on style | No style arguments |
| Structured errors | Machine-parseable feedback | Actionable diagnostics |
| Refinement types | Compile-time constraint checking | Self-documenting contracts |
| One Way Principle | Fewer decision points | Consistent codebases |

#### 1.6.2 Where Interests Diverge (Document the Tradeoff)

| Decision | LLM Preference | Human Preference | Baseline Choice | Rationale |
|----------|---------------|------------------|-----------------|-----------|
| Function syntax | Familiar `fn` keyword | ML-style binding or `fn` | `fn` keyword | Training data compatibility (§5.1) |
| Row polymorphism | Closed types easier to constrain | Open types more flexible | Bounded at module boundaries | Best of both (§3.2) |
| Verification levels | Should know which level is active | May not care during dev | Level surfaced in all feedback | Prevents generate-then-surprise (§8.5) |
| Test syntax | Explicit function application | BDD natural language | Explicit with BDD structure | Clarity over sugar (§9.3) |
| Formatting | Stripped (minimal tokens) | Generous whitespace | Canonical with whitespace | `gofmt` proved humans adapt; small token cost |

#### 1.6.3 The Legibility Layer

Baseline supports two views of the same source file, rendered by tooling:

**Spec View** (for human review of LLM-generated code):
```
fn create_user!(body: UserCreate) -> {Db} Result<User, ValidationError>
  @requires body.name.len > 0
  @ensures result.id > 0 when Ok
  @effects Db.write
```

**Impl View** (full implementation):
```baseline
fn create_user!(body: UserCreate) -> {Db} Result<User, ValidationError> =
  let user = User {
    id: Db.next_id!(),
    name: body.name,
    email: body.email,
  }
  Db.insert!(user)?
  Ok(user)
```

Both views are derived from the same source. The spec view is generated by the compiler from type signatures, specifications, and effect annotations. Tooling (IDE, code review, CI) can render either view. A human auditing LLM output reads the spec view first, dips into impl only where something looks wrong.

### 1.7 The One Way Principle

Baseline is opinionated by design. For every common operation, there is one obvious way to do it. This is not merely a convention—it is enforced by the compiler and formatter.

**Rationale**: For LLM code generation, every decision point is a potential error. Multiple equivalent syntaxes mean more tokens spent choosing an approach, inconsistent codebases, conflicting patterns in training data, and style arguments that waste human time.

#### 1.7.1 Enforced Patterns

**String Formatting**: Interpolation only

```baseline
// THE way
let msg = "Hello, ${name}. You have ${count} messages."

// NOT supported:
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

// NOT supported:
// null, undefined, nil   // Keywords don't exist
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
```

**Imports**: Explicit, not wildcard

```baseline
// THE way
import Http.{get!, post!}
import Json.{encode, decode}

// Discouraged - lint warning:
import Http.*
```

**Function Definition**: One syntax

```baseline
// THE way
fn greet(name: String) -> String =
  "Hello, ${name}"

// NOT supported:
// greet : String -> String / greet = |name| ...  // v0.1 split style
// func greet(name) { }
// def greet(name):
```

**Equality**: One operator

```baseline
// THE way
if a == b then ...

// NOT supported:
// a === b, a.equals(b), eq(a, b)
```

#### 1.7.2 Permitted Flexibility

Some patterns genuinely have multiple valid forms based on context:

```baseline
// Expression body (short functions)
fn double(x: Int) -> Int = x * 2

// Block body (multi-step)
fn process(input: Input) -> Output = {
  let parsed = parse(input)
  let validated = validate(parsed)
  transform(validated)
}
```

### 1.8 Core Design Principles

#### 1.8.1 Composition Over Inheritance

Baseline has no classes, no inheritance, no `extends`. Instead:

**Records for Data**

```baseline
type Entity = { id: Id, created_at: Timestamp }
type User = { entity: Entity, name: String, email: Email }
```

**Structural Typing for Polymorphism**

```baseline
fn greet(entity: { name: String, ..rest }) -> String =
  "Hello, ${entity.name}"

// Works for ANY record with a name field
greet(user)     // Works
greet(company)  // Works
greet(pet)      // Works
```

**Capabilities via Effects**

```baseline
effect Log {
  debug! : String -> ()
  info! : String -> ()
  error! : String -> ()
}

fn process!(data: Data) -> {Log, Db} Result =
  Log.info!("Processing ${data.id}")
  ...
```

#### 1.8.2 Parse, Don't Validate

Use types to make illegal states unrepresentable. Instead of validating data and hoping it stays valid, parse it into a type that guarantees validity.

```baseline
// GOOD: The type IS the validation
type Email = String where String.matches(self, r".+@.+\..+")

fn process_email!(email: Email) -> {Http} Result =
  // email is GUARANTEED valid by the type system
  send_welcome!(email)

// The "parsing" happens once, at the boundary
fn handle_signup!(req: Request) -> {Http} Response =
  match Email.parse(req.body.email)
    Ok(email) ->
      process_email!(email)
      Ok(Response.success())
    Err(_) ->
      Ok(Response.bad_request("Invalid email"))
```

| Aspect | Validate | Parse |
|--------|----------|-------|
| Invalid data can exist | Yes, as raw type | No, type prevents it |
| Re-validation needed | Every use site | Never |
| Documentation | Comments, maybe | Type signature |
| Compiler enforcement | None | Full |
| LLM error potential | High (might forget check) | Low (type system catches) |

**More Examples:**

```baseline
// Different ID types can't be mixed
type UserId = Int where self > 0
type OrderId = Int where self > 0
get_user(order_id)  // Compile error: expected UserId, got OrderId

// Non-empty lists
type NonEmpty<T> = List<T> where List.len(self) > 0
fn head(list: NonEmpty<T>) -> T =
  Option.unwrap(List.get(list, 0))  // Safe!

// State machines
type UnverifiedUser = { email: String, token: String }
type VerifiedUser = { email: Email, verified_at: Timestamp }
fn send_newsletter!(user: VerifiedUser) -> {Http} Result = ...
// Can't pass UnverifiedUser — different type!
```

#### 1.8.3 Make Illegal States Unrepresentable

```baseline
// BAD: Allows nonsensical combinations
type ConnectionBad = {
  status: String,        // "connected", "disconnected", "error"
  socket: Socket?,       // Only valid when connected
  error: String?,        // Only valid when error
}

// GOOD: Invalid states are unrepresentable
type Connection =
  | Disconnected
  | Connected(Socket)
  | Error({ message: String, retry_count: Int where self >= 0 })

fn handle(conn: Connection) -> Action =
  match conn
    Disconnected        -> reconnect()
    Connected(socket)   -> use_socket(socket)
    Error({ retry_count, .. }) if retry_count < 3 -> retry()
    Error(_)            -> give_up()
```

These principles are not suggestions—they are enabled and enforced by Baseline's type system.

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
fn let match if then else where with
type alias effect module import export
true false not for do in
```

**Design note**: All reserved words are common English words that map to single tokens in standard BPE tokenizers (cl100k_base, o200k_base). The keyword `fn` was chosen over `fun`, `func`, or `function` because it is the most token-efficient while remaining immediately recognizable from Rust, Kotlin, and other modern languages.

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
=       // Binding, definition
->      // Function type, match arm
|       // Lambda parameters, match patterns, union types
.       // Field access
..      // Spread operator
@       // Attribute prefix
_       // Wildcard pattern
```

### 2.7 Canonical Formatting

Baseline ships with `baseline fmt`, which enforces a single canonical format. Like Go's `gofmt`, this is not optional — CI pipelines should reject unformatted code.

The canonical format:
- 2-space indentation
- Generous vertical whitespace between top-level declarations
- Trailing commas in multi-line constructs
- Closing delimiters on their own line in multi-line constructs
- Single blank line between functions
- No trailing whitespace
- Newline at end of file

```baseline
// Trailing commas: always allowed, always recommended in multi-line
type User = {
  name: String,
  email: String,
  age: Int,     // Trailing comma
}

// Indentation is convention, not syntax. These are equivalent:
let result = match x
  Some(v) -> v
  None -> default

let result = match x { Some(v) -> v; None -> default }
```

**Rationale**: Canonical formatting eliminates an entire class of token-prediction decisions for LLMs. The Go community proved that humans adapt to enforced formatting within weeks. The small token overhead of whitespace is justified by the elimination of formatting inconsistency.

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

let nums = [1, 2, 3]
List.len(nums)              // 3
List.get(nums, 0)           // Some(1)
[0, ..nums]                 // Prepend: [0, 1, 2, 3]
[..nums, 4]                 // Append: [1, 2, 3, 4]
```

#### Records

Records are structurally typed with **bounded row polymorphism**:

```baseline
{ name: String, age: Int }  // Record type

let user = { name: "Alice", age: 30 }
user.name                   // "Alice"
{ ..user, age: 31 }         // Update: { name: "Alice", age: 31 }

// Row polymorphism (within a module)
fn greet(person: { name: String, ..r }) -> String =
  "Hello, ${person.name}"

greet({ name: "Bob", age: 25 })         // Works
greet({ name: "Carol", role: "Admin" }) // Also works
```

**Bounded Row Polymorphism (v0.2 revision)**

Row variables (`..r`) are permitted in function signatures but are **resolved to concrete types at module boundaries**. Exported functions must have fully concrete record types or use named type aliases:

```baseline
// INTERNAL: row polymorphism allowed
fn greet(person: { name: String, ..r }) -> String =
  "Hello, ${person.name}"

// EXPORTED: must use concrete type or named alias
export type Named = { name: String }

export fn greet(person: Named) -> String =
  "Hello, ${person.name}"
```

**Rationale**: Unbounded row variables at module boundaries create an open-ended type inference problem during constrained generation — the set of valid completions depends on unknown call sites. Bounding row variables at exports gives the LLM a concrete type to work against while preserving flexibility internally. This follows the MoonBit pattern of "mandatory type signatures at module boundaries with inference locally."

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

let x: Option<Int> = Some(42)
match x
  Some(n) -> "Got ${n}"
  None    -> "Nothing"
```

#### Parameterized Types

```baseline
type Map<K, V>
type Set<T>
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
fn listen!(port: Port) -> {Net} Server =
  // Inside here, port is guaranteed to be 1-65535
  ...

listen!(70000)  // Compile error: 70000 does not satisfy 1 <= self <= 65535

let user_input: Int = read_port()
listen!(user_input)  // Checked at runtime, returns Result
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

When using `String.matches()` in refinement types, Baseline uses a **deterministic regex subset** to ensure consistent behavior:

```baseline
// Supported
/abc/  /[a-z]/  /[^0-9]/  /./  /a*/  /a+/  /a?/  /a{3}/  /a{2,5}/
/(ab)/  /a|b/  /^abc$/  /\d \w \s/

// NOT supported (determinism and ReDoS prevention)
// No backreferences, lookahead, lookbehind, possessive quantifiers,
// atomic groups, or recursive patterns
```

For complex validation beyond this subset, use predicate functions:

```baseline
type ValidEmail = String where is_valid_email(self)

fn is_valid_email(s: String) -> Bool =
  let parts = String.split(s, "@")
  List.len(parts) == 2
    && List.get(parts, 0) |> Option.map(|p| String.len(p) > 0) == Some(true)
    && List.get(parts, 1) |> Option.map(|p| String.contains(p, ".")) == Some(true)
```

### 3.5 Optional and Result Types

```baseline
T?          // Sugar for Option<T>

fn find(id: Id) -> User? = ...
fn parse(s: String) -> Result<Int, ParseError> = ...
```

### 3.6 Type Inference

Baseline uses bidirectional type inference. Type annotations are required at module boundaries (exported functions) and effectful functions. Type annotations are optional for local variables, lambda expressions, and private functions.

```baseline
// Annotation required (exported)
export fn greet(name: String) -> String =
  "Hello, ${name}"

// Annotation optional (local)
let nums = [1, 2, 3]                        // Inferred as List<Int>
let doubled = List.map(nums, |x| x * 2)     // Inferred
```

**Design note**: Mandatory signatures at module boundaries serve both audiences. For LLMs, they provide the "contract boundary" that catches hallucinated methods and wrong types at compile time. For humans, they serve as documentation that is always accurate.

### 3.7 Tiered Refinement Types

#### Tier 1: Automatic Refinements (Zero Annotation)
- **Null Safety**: `T?` is checked exhaustively.
- **Division by Zero**: `x / y` requires `y` to be non-zero.
- **Array Bounds**: Static analysis eliminates bounds checks where possible.

#### Tier 2: Simple Refinements

```baseline
type Port = Int where self > 0 && self < 65536
type Probability = Float where self >= 0.0 && self <= 1.0
```

#### Tier 3: Contracts

```baseline
fn send_email!(email: Email) -> {Http} Result = ...
```

---

## 4. Expressions

### 4.1 Let Bindings

```baseline
let x = 42
let (a, b) = get_pair()
let { name, age } = get_user()
let [first, ..rest] = get_list()
let x: Int = 42              // With type annotation

// Let is an expression
let result =
  let x = 10
  let y = 20
  x + y           // result = 30
```

### 4.2 Conditionals

```baseline
if condition then expr1 else expr2

if cond1 then
  expr1
else if cond2 then
  expr2
else
  expr3

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
```

### 4.5 Pipelines

```baseline
value
|> transform
|> validate
|> save

// With lambdas
numbers
|> List.filter(|n| n > 0)
|> List.map(|n| n * 2)
|> List.sum
```

### 4.6 Function Application

```baseline
f(x)
f(x, y)

// Named arguments (order-independent)
create_user(name: "Alice", age: 30)
create_user(age: 30, name: "Alice")  // Same
```

### 4.7 Field Access

```baseline
user.name
tuple.0
response.body.data.users.first
```

### 4.8 Error Handling Expressions

```baseline
let value = fallible_operation()?          // Propagate
let value = Option.unwrap_or(maybe, default)  // Default
```

### 4.9 Typed Holes

The `??` operator represents code to be generated later. Unlike comments or TODO markers, typed holes are **valid syntax** that the compiler reasons about:

```baseline
fn process_order!(order: Order) -> {Db, Http} Result<Receipt, OrderError> =
  let validated = validate(order)?
  let payment = charge_payment!(validated.total, validated.payment_method)?
  let receipt = ??  // Hole: expected type Receipt
  Ok(receipt)
```

The compiler reports typed holes as structured information:

```json
{
  "holes": [
    {
      "location": { "line": 4, "col": 17 },
      "expected_type": "Receipt",
      "available_bindings": [
        { "name": "validated", "type": "ValidatedOrder" },
        { "name": "payment", "type": "PaymentConfirmation" }
      ],
      "available_functions": [
        { "name": "Receipt.from_payment", "type": "(ValidatedOrder, PaymentConfirmation) -> Receipt" }
      ],
      "constraints": ["Must satisfy return type Result<Receipt, OrderError>"],
      "effects_available": ["Db", "Http"]
    }
  ]
}
```

Holes can carry constraints:

```baseline
let receipt = ?? where Receipt.total(self) == validated.total
```

Typed holes enable partial compilation, LLM collaboration (the compiler tells the LLM exactly what type and constraints a hole needs), incremental development, and verified control / generated data separation.

---

## 5. Functions

### 5.1 Function Definitions

```baseline
fn greet(name: String) -> String =
  "Hello, ${name}"

fn add(a: Int, b: Int) -> Int = a + b

fn first<T>(list: List<T>) -> T? =
  match list
    [x, .._] -> Some(x)
    []       -> None

fn quicksort(list: List<Int>) -> List<Int> =
  match list
    [] -> []
    [pivot, ..rest] ->
      let smaller = List.filter(rest, |x| x < pivot)
      let larger = List.filter(rest, |x| x >= pivot)
      quicksort(smaller) ++ [pivot] ++ quicksort(larger)
```

**Design rationale (v0.1 → v0.2 change)**:

The v0.1 syntax split signature and implementation across two lines (`greet : String -> String` / `greet = |name| ...`). This was changed to `fn` keyword syntax for three reasons:

1. **Training data compatibility**: LLMs have trained on billions of lines using `fn`/`func`/`def` + parameters-in-parens. The v0.1 ML-style split declaration fights the statistical prior of the entire training corpus. Empirical testing showed current models frequently merge the two lines or generate one without the other.

2. **Single-token keyword**: `fn` maps to a single BPE token in all major tokenizers. It serves as an unambiguous "function definition starts here" signal.

3. **Parameter type locality**: `fn greet(name: String)` co-locates the parameter name and type. The v0.1 style required mentally zipping together the type signature with parameter names across two lines.

The tradeoff: v0.1's ML-style was more theoretically elegant and separated the type from the implementation. This capability is preserved through specifications (§8.1) and typed holes (§4.9).

### 5.2 Anonymous Functions (Lambdas)

```baseline
|x| x + 1                   // Single parameter
|a, b| a + b                // Multiple parameters
|_| 42                      // Ignored parameter
|(x, y)| x + y              // Destructuring

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
fn fetch_user!(id: Id) -> {Http, Log} User? =
  Log.debug!("Fetching user ${id}")
  Http.get!("/users/${id}")? |> Response.decode

fn main!() -> {Console, Http} () =
  let user = fetch_user!(42)?
  Console.print!("Got user: ${user.name}")
```

Rules:
1. Must be declared with `!` suffix
2. Must declare their effects in the type signature
3. Can only be called from other effectful functions
4. Can call pure functions freely

### 5.4 Generic Functions

```baseline
fn identity<T>(x: T) -> T = x

fn map<A, B>(list: List<A>, f: A -> B) -> List<B> =
  match list
    []        -> []
    [x, ..xs] -> [f(x), ..map(xs, f)]

fn compare<T: Ord>(a: T, b: T) -> Ordering = Ord.compare(a, b)
```

---

## 6. Effects

### 6.1 Effect Declarations

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

```baseline
fn log_message!(msg: String) -> {Console} () =
  Console.print!("[LOG] ${msg}")

fn fetch_with_logging!(url: String) -> {Http, Console} Result<Response, Error> =
  Console.print!("Fetching ${url}")
  let response = Http.get!(url)?
  Console.print!("Got ${response.status}")
  Ok(response)

// Effect polymorphism
fn timed!<T, e>(action: () -> {e} T) -> {e, Time} (T, Duration) =
  let start = Time.now!()
  let result = action()
  let elapsed = Time.now!() - start
  (result, elapsed)
```

### 6.3 Effect Handlers

```baseline
fn main!() -> () =
  let http = Http.default()
  let console = Console.stdout()
  run_app!() with { http, console }

// Custom handlers for testing
test "fetch user" =
  let mock_http = Http.mock([
    ("/users/1", Ok({ id: 1, name: "Alice" })),
  ])
  let result = fetch_with_logging!("/users/1")
    with { http: mock_http, console: Console.buffer() }

  expect result == Ok({ id: 1, name: "Alice" })
```

### 6.4 Effect Inference

When effects are not explicitly declared, they are inferred:

```baseline
fn fetch_and_print!(url: String) =
  let data = Http.get!(url)?
  Console.print!(data.body)
  Ok(())
```

### 6.5 Pure Functions

Functions without `!` are pure and cannot use effects:

```baseline
fn add(a: Int, b: Int) -> Int = a + b

fn bad(x: Int) -> Int =
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

```baseline
// WITH prelude: just write code
@prelude(script)
module MyScript

fn main!() =
  Console.print!("Hello!")
  let data = Http.get!("https://api.example.com/data")?
  Fs.write_text!("output.txt", data.body)
```

| Prelude | Effects Included | Use Case |
|---------|------------------|----------|
| `script` | Console, Fs, Http, Time, Random, Env, Process | CLI tools |
| `server` | Http, Db, Log, Time, Metrics, Env | Web servers |
| `minimal` | Console, Time | Simple programs |
| `pure` | (none) | Pure computation |

Custom preludes via `baseline.toml`:

```toml
[prelude.mycompany]
effects = ["Console", "Log", "Http", "Db", "Metrics"]
handlers = {
  Log = "MyCompany.Logging.structured",
  Metrics = "MyCompany.Observability.datadog"
}
```

### 6.8 Row Polymorphism

Baseline's effect system is built on row polymorphism:

```baseline
// 'e' is a row variable capturing "other effects"
fn map<A, B, e>(list: List<A>, f: A -> {e} B) -> {e} List<B> = ...
```

### 6.9 Direct Style

Baseline compiles algebraic effects to Direct Style code, avoiding the "colored function" problem of async/await.

- **No `async`/`await` keywords**: Asynchronous IO is just an effect.
- **Unified abstraction**: Async, Generators, and Exceptions are all just Effects.

---

## 7. Modules

### 7.1 Module Declaration

Each file is a module. The module name matches the file path:

```baseline
// File: src/api/users.bl
@module Api.Users
```

### 7.2 Imports

```baseline
import Http                    // Import entire module
import Http.{get!, post!}      // Import specific items
import Http as H               // Import with alias
import Http.*                  // Import all (lint warning)
```

### 7.3 Exports

By default, all top-level definitions are private:

```baseline
export fn greet(name: String) -> String =
  "Hello, ${name}"

fn helper(s: String) -> String = String.trim(s)  // Private

export type User = { name: String, age: Int }
export type Status = Active | Inactive
export effect MyEffect { ... }
```

### 7.4 Module Organization

```
my-project/
├── baseline.toml
├── src/
│   ├── main.bl
│   ├── lib.bl
│   ├── api/
│   │   ├── mod.bl
│   │   ├── users.bl
│   │   └── posts.bl
│   └── util/
│       └── helpers.bl
└── test/
    └── api_test.bl
```

**Design note**: Flat module structure is preferred over deep nesting. Each module should be comprehensible independently, reducing the context needed to generate correct code. This respects both LLM context window limitations and human cognitive load.

### 7.5 Visibility

```baseline
export fn    // Public: accessible from any module
fn           // Private: accessible only within this module
internal fn  // Internal: accessible within the same package (future)
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

fn divide(n: Int, d: Int) -> Int = n / d
```

| Attribute | Description |
|-----------|-------------|
| `@spec name` | Names the specification |
| `@given` | Declares inputs with optional refinements |
| `@returns` | Declares output type |
| `@requires` | Preconditions |
| `@ensures` | Postconditions |
| `@effects` | Declares required effects |
| `@pure` | Asserts function is pure |
| `@total` | Asserts function terminates for all inputs |

**Type-first development**: Specifications let you write the contract before the implementation. An LLM can be given just the spec and typed hole:

```baseline
@spec sort_descending
@given list: List<Int>
@returns List<Int>
@ensures List.len(result) == List.len(list)
@ensures List.all(List.zip(result, List.drop(result, 1)), |(a, b)| a >= b)

fn sort_descending(list: List<Int>) -> List<Int> = ??
```

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

```
$ baseline check

Checking specifications...
  ✓ divide: precondition verified
  ✓ divide: postcondition verified
  ✓ User: invariants verified
  ✓ UserApi: all routes type-check

Specifications: 4 verified, 0 failed
```

### 8.5 Verification Levels

**v0.2 revision**: Verification levels are surfaced in all compiler output to prevent the "worked locally, broke in CI" problem.

```baseline
@verify(level: refinements)
module MyModule

@verify(level: full)
fn critical_function(input: Input) -> Output = ...

@verify(skip: "Performance critical, manually audited 2024-01-15")
fn unsafe_but_fast(data: Data) -> Data = ...
```

| Level | Checks | Speed | Use Case |
|-------|--------|-------|----------|
| `types` | Type inference, exhaustiveness | Fast (~ms) | Rapid iteration |
| `refinements` | Types + refinement constraints | Medium (~100ms) | Development default |
| `full` | Types + refinements + all specs + SMT | Slow (~seconds) | CI, release |
| `skip` | Types only, specs unchecked | Fast | Escape hatch |

```bash
baseline check                    # Uses level from source annotations
baseline check --level=types      # Fast check only
baseline check --level=full       # Full verification
baseline check --timeout=30s      # Set SMT timeout
```

#### Verification Level Awareness

**Every** compiler response includes the active verification level:

```json
{
  "status": "ok",
  "verification_level": "refinements",
  "checked": ["types", "refinements"],
  "unchecked": ["specs", "smt"],
  "warning": "Postconditions in @spec divide are unchecked at level 'refinements'. Run with --level=full to verify.",
  "diagnostics": []
}
```

This ensures an LLM agent always knows what level of verification passed, what properties remain unverified, and what command to run for full verification.

**Rationale**: In v0.1, an LLM could generate code that passed `--level=refinements`, then CI running `--level=full` would find spec violations. The LLM received confusing feedback — the code "worked" but now "doesn't." Surfacing the level in every response lets the agent proactively run higher checks or annotate its output with the verification level achieved.

#### Handling SMT Timeouts

```json
{
  "status": "timeout",
  "verification_level": "full",
  "property": "@ensures result.len <= input.len * 2",
  "timeout_seconds": 10,
  "suggestions": [
    { "action": "increase_timeout", "command": "baseline check --timeout=60s", "confidence": 0.3 },
    { "action": "add_lemma", "description": "Add intermediate lemma to help the solver", "confidence": 0.5 },
    { "action": "assume", "code": "@assume result.len <= input.len * 2", "confidence": 0.2 },
    { "action": "skip", "code": "@verify(skip: \"SMT timeout\")", "confidence": 0.1 }
  ]
}
```

#### Assumptions

```baseline
@spec transform
@given data: List<Item>
@returns List<Result>
@assume db_items_are_valid  // External invariant
@ensures result.len == data.len

fn transform!(data: List<Item>) -> {Db} List<Result> = ...
```

Assumptions are tracked by the compiler, reported in summaries, and flagged in constrained generation mode.

### 8.6 The Neurosymbolic Feedback Loop

Baseline closes the loop between LLM generation and formal verification:

1. **Prompt**: Agent receives type signature with refinements and specifications.
2. **Generation**: Agent generates code (optionally with constrained generation — §11).
3. **Verification**: Compiler checks code against refinement types using SMT. Verification level is explicitly reported.
4. **Feedback**: On failure, compiler produces a **counter-example** with structured suggestions.
5. **Repair**: Agent fixes code based on counter-example.

This turns the compiler into a **verifier** for the probabilistic output of the LLM. Formal verification as a feedback signal is categorically more powerful than testing alone.

### 8.7 Verified Control / Generated Data Separation

Baseline encourages architecturally separating **verified control flow** from **LLM-generated data transformations**:

```baseline
// VERIFIED: Control flow is specified and formally checked
@spec order_pipeline
@ensures all orders processed or error logged
@verify(level: full)

fn order_pipeline!(orders: List<Order>) -> {Db, Log, Http} List<Result<Receipt, OrderError>> =
  orders
  |> List.map(|order| {
    let validated = validate(order)?
    let charged = charge_payment!(validated)?
    let receipt = generate_receipt(validated, charged)  // LLM-generated
    Ok(receipt)
  })

// GENERATED: Data transformation, verified by types + tests
fn generate_receipt(order: ValidatedOrder, payment: PaymentConfirmation) -> Receipt =
  Receipt {
    id: ReceiptId.new(),
    order_id: order.id,
    amount: payment.amount,
    timestamp: payment.timestamp,
    items: order.items |> List.map(format_line_item),
  }
```

The control flow is formally verified. The data transformation is type-checked and tested. Typed holes (§4.9) make this explicit: verified control flow is concrete code, data transformations start as holes.

---

## 9. Testing

### 9.1 Testing Philosophy

1. **Specs are documentation** — Tests should read like requirements
2. **Explicit over implicit** — Test behavior is clear from syntax alone
3. **LLM-friendly** — Structured format enables AI generation and verification

### 9.2 Inline Tests (Unit Tests)

```baseline
fn add(a: Int, b: Int) -> Int = a + b
where
  test "adds positive numbers" = add(1, 2) == 3
  test "handles negatives" = add(-1, 1) == 0
  test "zero identity" = add(0, 5) == 5
```

### 9.3 BDD Specifications

**v0.2 revision**: The `given`/`when`/`expect` syntax from v0.1 was revised to use explicit function application. The v0.1 `when List.sort` was ambiguous — is `List.sort` being called on the given value, or is it a label? Explicit application eliminates this while preserving BDD structure.

```baseline
describe "List.sort" {
  it "sorts numbers in ascending order" {
    expect List.sort([3, 1, 4, 1, 5]) to_equal [1, 1, 3, 4, 5]
  }

  it "handles empty lists" {
    expect List.sort([]) to_equal []
  }

  it "handles single element" {
    expect List.sort([42]) to_equal [42]
  }
}
```

#### Setup-Act-Assert with Let Bindings

```baseline
describe "UserService" {
  describe "create_user" {
    context "with valid data" {
      it "creates the user" {
        let input = { name: "Alice", email: "alice@example.com" }
        let result = create_user(input)
        expect result to_be Ok(User { id: _, name: "Alice", email: "alice@example.com" })
      }
    }

    context "with invalid email" {
      it "returns validation error" {
        let input = { name: "Alice", email: "not-an-email" }
        let result = create_user(input)
        expect result to_be Err(ValidationError { field: "email", .. })
      }
    }

    context "with duplicate email" {
      before {
        Db.insert!(User { id: 1, name: "Existing", email: "taken@example.com" })
      }

      it "returns conflict error" {
        let input = { name: "New User", email: "taken@example.com" }
        let result = create_user(input)
        expect result to_be Err(EmailExists)
      }
    }
  }
}
```

#### Fixtures

```baseline
describe "PostService" {
  let alice = User { id: 1, name: "Alice", role: Admin }
  let bob = User { id: 2, name: "Bob", role: Member }
  let alice_post = Post { id: 1, author_id: alice.id, content: "Hello" }

  describe "delete_post" {
    context "when user owns the post" {
      it "deletes successfully" {
        expect delete_post(alice, alice_post) to_be Ok(())
      }
    }

    context "when user does not own the post" {
      it "returns unauthorized" {
        expect delete_post(bob, alice_post) to_be Err(Unauthorized)
      }
    }
  }
}
```

#### Hooks

```baseline
describe "Database operations" {
  before_all { Db.migrate!() }
  before_each { Db.begin_transaction!() }
  after_each { Db.rollback!() }
  after_all { Db.cleanup!() }

  it "inserts records" {
    let user = User { name: "Test", email: "test@test.com" }
    expect Db.insert!(user) to_be Ok(_)
  }
}
```

#### Mocking Effects

```baseline
describe "WeatherService" {
  let mock_http = Http.mock([
    ("https://api.weather.com/current", Ok({ temp: 72, conditions: "sunny" })),
  ])

  it "fetches current weather" {
    with { http: mock_http }
    expect get_current_weather!("New York") to_be Ok({ temp: 72, conditions: "sunny" })
  }

  it "handles API errors gracefully" {
    with { http: Http.mock_error(Timeout) }
    expect get_current_weather!("New York") to_be Err(ServiceUnavailable)
  }
}
```

### 9.4 Focused and Skipped Tests

```baseline
describe "Feature" {
  it.only "focused test" { ... }
  it.skip "not implemented yet" { ... }
  it.skip("waiting on API v2") "new feature" { ... }
}
```

### 9.5 Assertions and Matchers

```baseline
expect 1 + 1 to_equal 2
expect result to_be Ok(_)
expect count to_be_greater_than 0
expect age to_be_between 0 and 120
expect list to_contain 42
expect list to_have_length 3
expect list to_be_empty
expect message to_start_with "Error:"
expect message to_match r"user \d+"
expect result to_be_ok
expect option to_be_some
expect value to_be_type User
expect user to_satisfy |u| u.age >= 18
```

### 9.6 Test Output for LLMs

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
    "duration_ms": 142,
    "verification_level": "refinements"
  },
  "failures": [
    {
      "name": "UserService > delete_post > returns unauthorized",
      "location": "test/user_spec.bl:45",
      "expression": "delete_post(bob, alice_post)",
      "expected": "Err(Unauthorized)",
      "actual": "Err(NotFound)",
      "diff": {
        "type": "variant_mismatch",
        "expected_variant": "Unauthorized",
        "actual_variant": "NotFound"
      },
      "suggestions": [
        {
          "action": "check_ordering",
          "description": "Check authorization logic runs before existence check",
          "confidence": 0.7
        }
      ]
    }
  ]
}
```

### 9.7 Best Practices

#### Spec-First Development (Recommended for LLMs)

```baseline
// 1. Write the spec first
describe "PasswordService" {
  describe "validate_password" {
    it "accepts strong passwords" {
      expect validate_password("Str0ng!Pass") to_be Ok(())
    }

    it "rejects short passwords" {
      expect validate_password("short") to_be Err(TooShort { min: 8, actual: 5 })
    }

    it "requires uppercase" {
      expect validate_password("alllowercase1!") to_be Err(MissingUppercase)
    }

    it "requires number" {
      expect validate_password("NoNumbersHere!") to_be Err(MissingNumber)
    }
  }
}

// 2. Then implement to satisfy the spec
fn validate_password(password: String) -> Result<(), PasswordError> =
  if String.len(password) < 8 then
    Err(TooShort { min: 8, actual: String.len(password) })
  else if not String.any(password, Char.is_uppercase) then
    Err(MissingUppercase)
  else if not String.any(password, Char.is_digit) then
    Err(MissingNumber)
  else
    Ok(())
```

---

## 10. Language Server Protocol and Compiler API

### 10.1 Overview

Baseline exposes its compiler internals via a queryable API, enabling IDEs, LLMs, and automated tools to interact with the language semantically.

### 10.2 Standard LSP Features

Diagnostics (with verification level context), Completion, Hover, Go to Definition, Find References, Rename, Code Actions, Formatting (`baseline fmt` integration).

### 10.3 Extended Query API

#### Type Queries

```json
{ "method": "baseline/typeAt", "params": { "file": "src/api.bl", "position": { "line": 42, "character": 15 } } }

// Response
{ "type": "Option<User>", "expanded": "Some(User) | None", "refinements": [], "effects": [] }
```

#### Function Search

```json
{ "method": "baseline/searchByType", "params": { "signature": "List<A> -> (A -> B) -> List<B>", "scope": "visible" } }

// Response
{
  "matches": [
    { "name": "List.map", "module": "Baseline.Collections", "signature": "List<A> -> (A -> B) -> List<B>" }
  ]
}
```

#### Effect Queries

```json
{ "method": "baseline/availableEffects", "params": { "file": "src/api.bl", "position": { "line": 42, "character": 0 } } }

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
{ "method": "baseline/specFor", "params": { "function": "Api.Users.create_user" } }

// Response
{
  "spec": {
    "given": [{ "name": "body", "type": "UserCreate", "refinements": ["name.len > 0"] }],
    "returns": { "type": "User | ValidationError" },
    "ensures": ["result.id > 0 when Ok"],
    "effects": ["Db"]
  }
}
```

### 10.4 Programmatic Compilation API

```json
{
  "method": "baseline/checkSource",
  "params": {
    "source": "let x: Int = \"hello\"",
    "context": { "imports": ["Baseline.Core.*"], "effects": ["Console"] },
    "verification_level": "refinements"
  }
}

// Response
{
  "status": "error",
  "verification_level": "refinements",
  "diagnostics": [
    {
      "severity": "error",
      "code": "E0308",
      "message": "Type mismatch",
      "location": { "line": 1, "col": 14, "span": 7 },
      "expected": "Int",
      "actual": "String",
      "suggestions": [
        { "action": "parse", "code": "let x: Int = Int.parse(\"hello\")?", "confidence": 0.6 },
        { "action": "change_type", "code": "let x: String = \"hello\"", "confidence": 0.8 }
      ]
    }
  ]
}
```

### 10.5 Interactive Refinement Session

```json
// Start session
{ "method": "baseline/session/start", "params": { "project": "myapp" } }
// Response: { "sessionId": "abc123", "default_verification_level": "refinements" }

// Submit code
{ "method": "baseline/session/check", "params": { "sessionId": "abc123", "file": "src/api.bl", "source": "...", "verification_level": "full" } }

// Query verification failures
{ "method": "baseline/session/query", "params": { "sessionId": "abc123", "question": "Why can't you prove user.age >= 0?" } }

// Response
{
  "answer": "The field `age` in type `User` has type `Int` with no refinement.",
  "suggestions": [
    { "action": "add_refinement", "code": "age: Int where self >= 0", "confidence": 0.7 },
    { "action": "add_guard", "code": "if user.age < 0 then return Err(InvalidAge)", "confidence": 0.6 }
  ]
}

// End session
{ "method": "baseline/session/end", "params": { "sessionId": "abc123" } }
```

### 10.6 Bulk Operations for LLM Agents

```json
{ "method": "baseline/analyzeModule", "params": { "module": "Api.Users", "include": ["types", "functions", "specs", "tests"] } }

// Response
{ "module": "Api.Users", "types": [...], "functions": [...], "specs": [...], "tests": [...], "dependencies": [...], "dependents": [...] }
```

---

## 11. Constrained Generation Protocol

**New in v0.2**

This section specifies how Baseline's type system can be used to **constrain LLM token generation in real time**, preventing ill-typed code from ever being produced.

### 11.1 Motivation

The standard LLM code generation loop is: Generate → Compile → Get errors → Fix → Retry. Each cycle costs time, tokens, and money. Type errors account for 33.6% of all LLM code generation failures.

Research from ETH Zurich demonstrated that type-constrained decoding using prefix automata can cut compilation errors by more than half and increase functional correctness by 3.5–5.5%.

Baseline's type system — with its simple decidable refinements, explicit effect annotations, and mandatory module-boundary signatures — is specifically designed to support this:

```
Generate (with type constraints at each token) → Compile → Verify specs
```

The first step produces code **guaranteed to type-check**. The second step only verifies specifications and refinements.

### 11.2 Protocol Specification

The Constrained Generation Protocol (CGP) extends the Interactive Refinement Protocol with token-level guidance:

```json
// Initialize constrained generation session
{
  "method": "baseline/cgp/start",
  "params": {
    "sessionId": "abc123",
    "context": {
      "file": "src/api.bl",
      "position": { "line": 42 },
      "scope": {
        "bindings": [
          { "name": "user", "type": "User" },
          { "name": "config", "type": "ServerConfig" }
        ],
        "effects": ["Http", "Log", "Db"],
        "expected_type": "Result<Response, ApiError>"
      }
    }
  }
}

// Response: Initial valid token set
{
  "sessionId": "abc123",
  "valid_tokens": ["let", "match", "if", "user", "config", "Http", "Log", "Db", "Ok", "Err"],
  "invalid_tokens": ["Fs", "Process", "Random"],
  "reason": { "Fs": "Effect Fs not in scope", "Process": "Effect Process not in scope" }
}
```

#### Token-by-Token Constraint Updates

```json
// LLM generates: "let result = Http."
{ "method": "baseline/cgp/advance", "params": { "sessionId": "abc123", "tokens": ["let", " result", " =", " Http", "."] } }

// Response: Valid completions after "Http."
{
  "valid_tokens": ["get!", "post!", "put!", "delete!", "head!"],
  "type_context": "Http method call, expecting: String -> Result<Response, HttpError>",
  "partial_type": "Http.??? : String -> {Http} Result<Response, HttpError>"
}
```

#### Effect Enforcement During Generation

```json
// LLM attempts: "Fs"
{ "method": "baseline/cgp/advance", "params": { "sessionId": "abc123", "tokens": ["Fs"] } }

// Response: Token rejected
{
  "status": "rejected",
  "reason": "Effect Fs is not in the current capability set {Http, Log, Db}",
  "valid_alternatives": ["Http", "Log", "Db", "let", "match", "if"],
  "security_note": "This constraint enforces the capability-based security model"
}
```

#### Refinement-Aware Generation

```json
// Context: fn listen!(port: Port) where Port = Int where 1 <= self <= 65535
// LLM generates: "listen!(70000)"
{ "method": "baseline/cgp/advance", "params": { "sessionId": "abc123", "tokens": ["listen!", "(", "70000", ")"] } }

// Response: Refinement violation
{
  "status": "refinement_violation",
  "constraint": "1 <= self <= 65535",
  "actual_value": 70000,
  "suggestion": "Use a value between 1 and 65535, or use a variable with Port type"
}
```

### 11.3 Implementation Requirements

For the CGP to work efficiently, Baseline's type system must satisfy:

1. **Incremental type checking**: The type of a partial program must be computable after each token. Baseline's bidirectional type inference supports this — the expected type flows down from annotations, and the synthesized type flows up from expressions.

2. **Decidable type membership**: Given a partial token sequence, the set of valid next tokens must be computable in bounded time. Baseline's refinement types use linear arithmetic (decidable) and a restricted regex subset (decidable).

3. **Effect set closure**: The set of valid effect calls is statically known from the function signature. No dynamic dispatch on effects.

4. **Tokenizer alignment**: Baseline's keywords are chosen to align with common BPE tokenizers. Each keyword maps to a single token, preventing the "token misalignment problem" identified in the Domino paper.

### 11.4 Graceful Degradation

Not all LLM deployment environments support constrained generation. The CGP is designed as an optional optimization:

| Mode | Guarantee | Speed | Requirement |
|------|-----------|-------|-------------|
| Unconstrained | None | Fastest generation | Standard LLM inference |
| Grammar-constrained | Syntactically valid | Slight overhead | Grammar-aware sampler |
| Type-constrained | Type-correct | Moderate overhead | CGP server running |
| Full-constrained | Type-correct + effects | Higher overhead | CGP server + effect checker |

Each mode is strictly more powerful than the previous. Projects choose their constraint level based on criticality and available infrastructure.

### 11.5 Integration with Existing Infrastructure

The CGP integrates with existing constrained generation frameworks:

- **Outlines/Guidance**: CGP can export Baseline's grammar as a JSON Schema or regex for use with existing structured generation libraries.
- **vLLM/TGI**: CGP token masks can be provided as logit bias arrays.
- **Custom inference**: The CGP server provides a simple HTTP API for token validation.

---

## 12. LLM Bootstrap Kit

**New in v0.2**

New languages face a cold-start problem: LLMs generate better code in languages with massive training corpora, but corpora only grow if languages are widely adopted.

### 12.1 `llms.txt` Specification File

Every Baseline installation includes an `llms.txt` file designed for inclusion in LLM system prompts:

```
# Baseline Language Quick Reference

## Syntax Summary
- Functions: fn name(param: Type) -> ReturnType = body
- Effects: fn name!(param: Type) -> {Effect1, Effect2} ReturnType = body
- Types: type Name = Int where self > 0
- Pipes: value |> transform |> validate
- Lambdas: |x| x + 1
- Pattern matching: match expr { pattern -> result }
- Error propagation: fallible_operation()?
- Let bindings: let x = expr
- Conditionals: if cond then expr1 else expr2
- Records: { field: value, field2: value2 }
- Sum types: type T = | Variant1(Type) | Variant2
- Typed holes: ?? (compiler reports expected type)

## Key Rules
- Pure functions have no ! suffix and cannot use effects
- Effectful functions end with ! and declare effects: {Http, Db}
- All exported functions require type annotations
- Pattern matching is exhaustive
- Trailing commas always allowed
- String interpolation: "Hello, ${name}"
- No null/undefined/nil — use Option<T> (sugar: T?)
- No exceptions — use Result<T, E> with ? propagation
- No classes/inheritance — use records + sum types + effects

## Common Patterns
fn pure_function(x: Int) -> Int = x * 2
fn effectful!(x: Int) -> {Db} Result<String, Error> = Db.query!(x)?
type Validated = String where String.len(self) > 0
let result = input |> parse |> validate |> transform
```

### 12.2 Canonical Few-Shot Examples

The distribution includes curated examples covering common patterns:

```baseline
// Example: CRUD API endpoint
@prelude(server)
module Examples.Crud

export type Todo = {
  id: Int where self > 0,
  title: String where String.len(self) > 0,
  completed: Bool,
}

type CreateTodo = {
  title: String where String.len(self) > 0,
}

export fn list_todos!() -> {Db} List<Todo> =
  Db.query!("SELECT * FROM todos")

export fn get_todo!(id: Int) -> {Db} Result<Todo, NotFound> =
  Db.query_one!("SELECT * FROM todos WHERE id = ?", id)
  |> Option.ok_or(NotFound)

export fn create_todo!(body: CreateTodo) -> {Db} Todo =
  Db.insert!("todos", { title: body.title, completed: false })

export fn delete_todo!(id: Int) -> {Db} Result<(), NotFound> =
  let deleted = Db.delete!("todos", id)
  if deleted then Ok(()) else Err(NotFound)

// Example: Pure data transformation with inline tests
fn summarize(todos: List<Todo>) -> { total: Int, completed: Int, pending: Int } =
  let total = List.len(todos)
  let completed = todos |> List.filter(|t| t.completed) |> List.len
  { total: total, completed: completed, pending: total - completed }
where
  test "empty list" = summarize([]) == { total: 0, completed: 0, pending: 0 }
  test "mixed" = summarize([
    { id: 1, title: "A", completed: true },
    { id: 2, title: "B", completed: false },
  ]) == { total: 2, completed: 1, pending: 1 }
```

### 12.3 Training Corpus Strategy

To maximize LLM generability without requiring fine-tuning:

1. **Syntactic proximity**: Baseline's `fn`/`let`/`match`/`if-then-else` syntax is deliberately close to Rust, OCaml, F#, and Kotlin — languages with substantial representation in training corpora.

2. **Standard library naming**: Function names follow widely-adopted conventions (`map`, `filter`, `fold`, `unwrap`, `ok_or`) shared across Rust, Haskell, OCaml, and Scala.

3. **Progressive complexity**: Few-shot examples are ordered from simple (pure functions with inline tests) to complex (effectful APIs with specifications).

4. **Negative examples**: The `llms.txt` explicitly shows what syntax does NOT exist, helping LLMs avoid generating patterns from other languages.

### 12.4 Model Compatibility Testing

The project maintains a benchmark suite:

```bash
baseline benchmark --model claude-sonnet --tasks standard
baseline benchmark --model gpt-4 --tasks standard
```

This produces structured output comparing pass rates, token efficiency, and feedback loop iterations across models, enabling data-driven syntax decisions.

---

## 13. Standard Library

### 13.1 Core Types

```baseline
module Baseline.Core

export (+), (-), (*), (/), (%), (**)
export (==), (!=), (<), (>), (<=), (>=)
export (&&), (||), not

// Option
export type Option<T> = Some(T) | None
export fn Option.map<T, U>(opt: Option<T>, f: T -> U) -> Option<U>
export fn Option.and_then<T, U>(opt: Option<T>, f: T -> Option<U>) -> Option<U>
export fn Option.unwrap<T>(opt: Option<T>) -> T
export fn Option.unwrap_or<T>(opt: Option<T>, default: T) -> T
export fn Option.ok_or<T, E>(opt: Option<T>, err: E) -> Result<T, E>

// Result
export type Result<T, E> = Ok(T) | Err(E)
export fn Result.map<T, U, E>(res: Result<T, E>, f: T -> U) -> Result<U, E>
export fn Result.map_err<T, E, F>(res: Result<T, E>, f: E -> F) -> Result<T, F>
export fn Result.and_then<T, U, E>(res: Result<T, E>, f: T -> Result<U, E>) -> Result<U, E>
export fn Result.unwrap<T, E>(res: Result<T, E>) -> T
export fn Result.unwrap_or<T, E>(res: Result<T, E>, default: T) -> T
```

### 13.2 Collections

```baseline
module Baseline.Collections

// List
export fn List.empty<T>() -> List<T>
export fn List.len<T>(list: List<T>) -> Int
export fn List.get<T>(list: List<T>, index: Int) -> T?
export fn List.first<T>(list: List<T>) -> T?
export fn List.last<T>(list: List<T>) -> T?
export fn List.map<T, U>(list: List<T>, f: T -> U) -> List<U>
export fn List.filter<T>(list: List<T>, f: T -> Bool) -> List<T>
export fn List.fold<T, U>(list: List<T>, init: U, f: (U, T) -> U) -> U
export fn List.find<T>(list: List<T>, f: T -> Bool) -> T?
export fn List.any<T>(list: List<T>, f: T -> Bool) -> Bool
export fn List.all<T>(list: List<T>, f: T -> Bool) -> Bool
export fn List.sort<T: Ord>(list: List<T>) -> List<T>
export fn List.reverse<T>(list: List<T>) -> List<T>
export fn List.concat<T>(a: List<T>, b: List<T>) -> List<T>
export fn (++)<T>(a: List<T>, b: List<T>) -> List<T>

// Map
export fn Map.empty<K, V>() -> Map<K, V>
export fn Map.insert<K, V>(m: Map<K, V>, key: K, val: V) -> Map<K, V>
export fn Map.get<K, V>(m: Map<K, V>, key: K) -> V?
export fn Map.remove<K, V>(m: Map<K, V>, key: K) -> Map<K, V>
export fn Map.contains<K, V>(m: Map<K, V>, key: K) -> Bool
export fn Map.keys<K, V>(m: Map<K, V>) -> List<K>
export fn Map.values<K, V>(m: Map<K, V>) -> List<V>

// Set
export fn Set.empty<T>() -> Set<T>
export fn Set.insert<T>(s: Set<T>, val: T) -> Set<T>
export fn Set.remove<T>(s: Set<T>, val: T) -> Set<T>
export fn Set.contains<T>(s: Set<T>, val: T) -> Bool
export fn Set.union<T>(a: Set<T>, b: Set<T>) -> Set<T>
export fn Set.intersection<T>(a: Set<T>, b: Set<T>) -> Set<T>
```

### 13.3 Text

```baseline
module Baseline.Text

export fn String.len(s: String) -> Int
export fn String.is_empty(s: String) -> Bool
export fn String.chars(s: String) -> List<Char>
export fn String.split(s: String, sep: String) -> List<String>
export fn String.join(parts: List<String>, sep: String) -> String
export fn String.trim(s: String) -> String
export fn String.starts_with(s: String, prefix: String) -> Bool
export fn String.ends_with(s: String, suffix: String) -> Bool
export fn String.contains(s: String, sub: String) -> Bool
export fn String.replace(s: String, from: String, to: String) -> String
export fn String.to_upper(s: String) -> String
export fn String.to_lower(s: String) -> String
export fn String.matches(s: String, re: Regex) -> Bool

export type Regex
export fn Regex.new(pattern: String) -> Result<Regex, RegexError>
export fn Regex.is_match(re: Regex, s: String) -> Bool
export fn Regex.captures(re: Regex, s: String) -> List<String>?
```

### 13.4 IO

```baseline
module Baseline.IO

export effect Fs {
  read_text! : Path -> Result<String, IoError>
  write_text! : (Path, String) -> Result<(), IoError>
  delete! : Path -> Result<(), IoError>
  exists! : Path -> Bool
  list_dir! : Path -> Result<List<Path>, IoError>
  create_dir! : Path -> Result<(), IoError>
  metadata! : Path -> Result<Metadata, IoError>
}

export type Path
export fn Path.from_str(s: String) -> Path
export fn Path.join(p: Path, s: String) -> Path
export fn Path.parent(p: Path) -> Path?
export fn Path.file_name(p: Path) -> String?
export fn Path.extension(p: Path) -> String?

export type Metadata = {
  size: Int,
  is_file: Bool,
  is_dir: Bool,
  modified: Timestamp,
  created: Timestamp,
}
```

---

## 14. Grammar

### 14.1 Notation

- `'text'` — Terminal (literal text)
- `Name` — Non-terminal
- `A | B` — Alternative
- `A?` — Optional
- `A*` — Zero or more
- `A+` — One or more
- `(A B)` — Grouping

### 14.2 Lexical Grammar

```ebnf
whitespace = ' ' | '\t' | '\n' | '\r'
line_comment = '//' (~'\n')* '\n'
block_comment = '/*' (block_comment | ~'*/')* '*/'

lower_ident = lower (lower | upper | digit | '_')*
upper_ident = upper (lower | upper | digit | '_')*

lower = 'a'..'z'
upper = 'A'..'Z'
digit = '0'..'9'

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

operator = '+' | '-' | '*' | '/' | '%' | '**'
         | '==' | '!=' | '<' | '>' | '<=' | '>='
         | '&&' | '||' | 'not'
         | '|>' | '<|' | '>>' | '<<'
         | '?'
         | '++' | '..'
```

### 14.3 Syntactic Grammar

```ebnf
(* Module *)
module = module_decl? import* declaration*
module_decl = '@module' module_path
module_path = upper_ident ('.' upper_ident)*
import = 'import' module_path import_spec?
import_spec = '.' '{' ident (',' ident)* ','? '}'
            | '.' '*'
            | 'as' upper_ident

(* Declarations *)
declaration = type_decl | effect_decl | function_decl | spec_decl

(* Type declarations *)
type_decl = 'export'? 'type' upper_ident type_params? '=' type_body where_clause?
type_params = '<' upper_ident (',' upper_ident)* '>'
type_body = type_expr | variant_list | record_type
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

(* Function declarations — v0.2 syntax *)
function_decl = 'export'? 'fn' lower_ident '!'? type_params?
                '(' param_list ')' ('->' type_expr)? '=' expression
                where_clause?

param_list = (param (',' param)* ','?)?
param = lower_ident ':' type_expr

(* Specification declarations *)
spec_decl = '@spec' lower_ident spec_attr*
spec_attr = '@given' param_list
          | '@returns' type_expr
          | '@requires' expression
          | '@ensures' expression
          | '@effects' '{' effect_list '}'
          | '@pure'
          | '@total'
          | '@assume' lower_ident

(* Expressions *)
expression = let_expr | if_expr | match_expr | lambda_expr | pipe_expr | hole_expr

let_expr = 'let' pattern (':' type_expr)? '=' expression expression?
if_expr = 'if' expression 'then' expression ('else' 'if' expression 'then' expression)* 'else' expression
match_expr = 'match' expression match_arm+
match_arm = pattern guard? '->' expression
guard = 'if' expression
lambda_expr = '|' lambda_params '|' expression
lambda_params = pattern (',' pattern)*
hole_expr = '??' ('where' expression)?

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
             | '[' (expression (',' expression)* ','?)? ']'
             | '{' (record_init (',' record_init)* ','?)? '}'
             | '{' expression '}'

record_init = lower_ident ':' expression
            | '..' expression

arg_list = (arg (',' arg)* ','?)?
arg = expression
    | lower_ident ':' expression

(* Patterns *)
pattern = '_'
        | lower_ident
        | literal
        | upper_ident ('(' pattern (',' pattern)* ')')?
        | '(' pattern (',' pattern)* ')'
        | '[' (pattern (',' pattern)*)? ('..' lower_ident?)? ']'
        | '{' field_pattern (',' field_pattern)* ('..')? '}'
        | pattern '|' pattern

field_pattern = lower_ident (':' pattern)?

(* Where clauses *)
where_clause = 'where' where_item+
where_item = 'test' string_lit '=' expression
           | 'property' string_lit '=' expression
           | 'contract' string_lit '=' contract_body
           | 'fuzz' string_lit '=' expression

contract_body = 'given' expression 'when' expression 'then' expression

(* Testing — BDD syntax *)
test_decl = 'describe' string_lit '{' test_item* '}'
test_item = test_decl
          | 'context' string_lit '{' test_item* '}'
          | 'it' ('.' 'only' | '.' 'skip' ('(' string_lit ')')?)? string_lit '{' test_body '}'
          | 'before_all' '{' expression '}'
          | 'before_each' '{' expression '}'
          | 'after_all' '{' expression '}'
          | 'after_each' '{' expression '}'
          | 'let' lower_ident '=' expression
          | 'with' '{' record_init (',' record_init)* '}'

test_body = (let_expr | with_clause | expect_expr)*
with_clause = 'with' '{' record_init (',' record_init)* '}'
expect_expr = 'expect' expression matcher
matcher = 'to_equal' expression
        | 'to_be' pattern
        | 'to_be_greater_than' expression
        | 'to_be_between' expression 'and' expression
        | 'to_contain' expression
        | 'to_have_length' expression
        | 'to_be_empty'
        | 'to_start_with' expression
        | 'to_match' expression
        | 'to_be_ok'
        | 'to_be_err'
        | 'to_be_some'
        | 'to_be_none'
        | 'to_be_type' upper_ident
        | 'to_satisfy' lambda_expr

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
        "verification_level": { "type": "string" },
        "suggestions": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "action": { "type": "string" },
              "code": { "type": "string" },
              "confidence": { "type": "number" },
              "description": { "type": "string" }
            }
          }
        }
      }
    }
  }
}
```

**v0.2 addition**: Every error response includes `verification_level` indicating which level of checking detected the error, and `suggestions` are sorted by `confidence` (highest first) to optimize LLM repair loops.

---

## Appendix B: Trace Format

Traces are stored in a binary format for efficiency:

```
TraceFile {
  magic: [u8; 4] = "BLTC"
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

## Appendix C: Dual-Audience Design Rationale

This appendix documents the empirical research that informed v0.2 design decisions.

### C.1 Function Syntax Change

**v0.1**: `greet : String -> String` / `greet = |name| "Hello, ${name}"`
**v0.2**: `fn greet(name: String) -> String = "Hello, ${name}"`

**Evidence**: The MultiPL-E benchmark found training data abundance is the strongest predictor of LLM code generation accuracy. The `fn name(params) -> Type = body` pattern appears in Rust (~15% of GitHub code repos), Kotlin, and Swift. The ML-style split declaration appears primarily in OCaml and Haskell (<2% combined). Models generate the `fn` pattern correctly at significantly higher rates without fine-tuning.

**Human impact**: Minimal. Both syntaxes are readable. The `fn` keyword provides a visual anchor for scanning code. Parameter-type co-location reduces eye movement when reading function signatures.

### C.2 Test Syntax Change

**v0.1**: `given [3, 1, 4] / when List.sort / expect [1, 1, 3, 4, 5]`
**v0.2**: `expect List.sort([3, 1, 4, 1, 5]) to_equal [1, 1, 3, 4, 5]`

**Evidence**: The v0.1 `when` clause created ambiguity about whether the identifier is being called or is a label. This is exactly the kind of ambiguity that type-constrained decoding cannot resolve (it's semantic, not syntactic). Explicit function application is unambiguous in both directions — the LLM knows it's generating a function call, and the human reader knows they're reading one.

**Human impact**: Slightly more verbose, but BDD structure (`describe`/`context`/`it`) provides the narrative scaffolding. The actual assertion line is clearer.

### C.3 Bounded Row Polymorphism

**v0.1**: Unbounded row polymorphism everywhere
**v0.2**: Row variables resolved to concrete types at module boundaries

**Evidence**: ETH Zurich's type-constrained decoding work requires the set of valid completions to be computable at each token. Unbounded row variables make this set infinite (any field access could be valid if some call site provides it). Bounding at module boundaries makes the valid completion set finite and computable.

**Human impact**: Minimal. Internal functions retain full flexibility. Exported functions require either concrete types or named aliases, which is better documentation practice regardless.

### C.4 Verification Level Surfacing

**v0.1**: Verification level was a CLI flag
**v0.2**: Verification level is reported in every compiler response

**Evidence**: The METR study found a 39-point perception gap between actual and perceived productivity with AI tools. A major source of this gap is delayed feedback — code appears to work at one level but fails at another. Making the verification level explicit in every response enables LLM agents to reason about what has and hasn't been verified.

**Human impact**: Positive. Developers also benefit from knowing whether their code has passed type checking only vs. full specification verification.

### C.5 Constrained Generation Protocol

**v0.1**: Not present
**v0.2**: Full protocol specification

**Evidence**: Type errors account for 33.6% of all LLM code generation failures. Type-constrained decoding cuts compilation errors by more than half and increases functional correctness by 3.5–5.5%. Baseline's type system (decidable refinements, explicit effects, mandatory boundary signatures) was specifically designed to enable this.

**Human impact**: None (the CGP operates during LLM inference and is invisible to human users).

---

*End of Specification*
