# Baseline Language Vision

**Aspirational Features — v0.2 and Beyond**

> These features are part of Baseline's long-term vision but are not yet implemented. They are extracted from the language specification to keep the core spec focused on what exists today. Designs here are subject to change.

---

## Table of Contents

1. [Memory Model](#1-memory-model)
2. [Compilation](#2-compilation)
3. [Tracing and Debugging](#3-tracing-and-debugging)
4. [SARIF Diagnostics](#4-sarif-diagnostics)
5. [Constrained Decoding API](#5-constrained-decoding-api)
6. [Advanced Testing](#6-advanced-testing)
7. [Extended Standard Library](#7-extended-standard-library)
8. [WebAssembly Interface](#8-webassembly-interface)
9. [Concurrency Model](#9-concurrency-model)
10. [Future Considerations](#10-future-considerations)
11. [LLM Training Bootstrap](#11-llm-training-bootstrap)

---

## 1. Memory Model

### 1.1 Perceus Reference Counting

Baseline utilizes **Perceus**, a precise, deterministic reference counting system that enables functional programming with C-like performance.

**Key Characteristics:**
- **No Garbage Collection**: Memory is freed immediately when the last reference is dropped.
- **Deterministic Latency**: No stop-the-world pauses, making Baseline suitable for real-time systems.
- **Reuse Analysis**: The compiler detects when a unique reference is dropped and immediately reuses its memory for a new allocation of the same size.

### 1.2 FBIP (Functional But In Place)

Perceus enables **FBIP**, where purely functional algorithms compile to efficient in-place mutations.

```baseline
// Source: Pure functional map
map : (List<T>, T -> U) -> List<U>
map = |list, f| match list
  Cons(head, tail) -> Cons(f(head), map(tail, f))
  Nil -> Nil

// Compiled (Conceptual):
// If 'list' is unique (ref_count == 1), 'Cons' cell is reused!
// No new allocation occurs.
```

### 1.3 Region-Based Local Memory

For short-lived scopes (like HTTP requests), Baseline employs **Region Inference**.

- **Arena Allocation**: Data that does not escape the request handler is allocated in a linear arena.
- **O(1) Deallocation**: The entire region is freed at once when the handler returns.
- **Thread Safety**: Regions are thread-local by default, eliminating synchronization overhead for request-local data.

### 1.4 Ownership and Borrowing

While Perceus handles deallocation, Baseline enforces ownership rules to ensure safety:

- **Values are owned**: Passing a value translates to a move or a copy (if ref count > 1).
- **Borrowing is implicit**: The compiler optimizes moves to borrows where possible.
- **No explicit lifetimes**: Unlike Rust, lifetimes are inferred from lexical scopes and function boundaries.
3. Regions can be nested
4. **The compiler tracks region lifetimes statically—no annotations required**

### 1.5 Why Regions Are Simpler Than Borrow Checking

| Aspect | Rust Borrow Checker | Baseline Regions |
|--------|---------------------|----------------|
| Syntax overhead | Lifetime parameters everywhere | None |
| Learning curve | Steep (weeks to months) | Gentle (hours) |
| Error messages | Often cryptic | Straightforward |
| Refactoring | May require signature changes | Localized changes |
| LLM generation | Difficult (lifetime inference) | Easy (scoped thinking) |

Baseline achieves memory safety through a simpler model:

1. **Immutability by default**: Most values can't be mutated after creation
2. **Copy-on-write**: Large structures use efficient persistent data structures
3. **Region scoping**: Temporary allocations are freed in bulk
4. **Reference counting**: Long-lived shared data uses atomic RC (optimized away when possible)

```baseline
// This "just works" without lifetime annotations
process_all : List<User> -> List<r>
process_all = |users|
  users
  |> List.map(transform)      // Intermediate list allocated
  |> List.filter(is_valid)    // Another intermediate
  |> List.map(format)         // Final result
  // Intermediates freed when function returns
```

### 1.6 Persistent Data Structures

For data that outlives a region, use persistent data structures:

```baseline
type AppState = Persistent {
  users: Map<Id, User>,
  cache: LRU<String, Response>,
}

// Copy-on-write semantics
update_state! : (Id, User) -> {State<AppState>} ()
update_state! = |id, user|
  State.modify!(|s| { ..s, users: s.users.insert(id, user) })
```

### 1.7 Memory Guarantees

| Guarantee | How |
|-----------|-----|
| No memory leaks | Regions freed automatically |
| No use-after-free | Compiler tracks lifetimes |
| No data races | Immutability + effects |
| Predictable latency | No GC pauses |
| Bounded memory | Regions have size limits |

**Known limitation:** Reference counting leaks on cycles. Immutable functional data rarely creates cycles, but graph structures and closure cycles require `Weak<T>` references (see Section 10.1). Region-scoped data is unaffected — bulk-freed regardless of cycles.

### 1.8 Arenas

For high-performance scenarios, explicit arenas:

```baseline
@arena(size: 1.mb, overflow: fail)
process_batch! : List<Item> -> List<Result>
process_batch! = |items|
  List.map(items, |item| process(item))
// Arena freed, all intermediate allocations gone
```

### 1.9 FBIP: Functional-But-In-Place

Baseline implements **Perceus-style reference counting with FBIP optimization**, enabling purely functional code to execute with in-place mutation semantics when data is uniquely owned.

#### 1.9.1 The Core Insight

When a function pattern-matches on a data structure and then constructs a new structure of the same size, the compiler can **reuse the memory** if the original has a reference count of 1 (uniquely owned).

```baseline
// This purely functional code...
map_tree : (a -> b) -> Tree<a> -> Tree<b>
map_tree = |f, tree| match tree
  | Leaf(x) -> Leaf(f(x))
  | Node(l, r) -> Node(map_tree(f, l), map_tree(f, r))

// ...compiles to in-place mutation when tree is uniquely owned
// No allocation, no copying—just overwriting memory
```

#### 1.9.2 Reuse Credits

The compiler tracks "reuse credits" statically:

| Operation | Credit Change |
|-----------|---------------|
| Pattern match on constructor | +1 credit (same size) |
| Construct new value | -1 credit |
| Function marked `@fip` | Must end at 0 credits |

```baseline
// Earns 1 credit (Leaf match), spends 1 credit (Leaf construct)
| Leaf(x) -> Leaf(f(x))  // Net: 0, can reuse memory

// Earns 1 credit (Node match), spends 1 credit (Node construct)
| Node(l, r) -> Node(...)  // Net: 0, can reuse memory
```

#### 1.9.3 FBIP Annotations

```baseline
// @fip: Fully in-place (compile error if allocation required)
@fip
reverse : List<a> -> List<a>
reverse = |list| reverse_acc(list, [])

@fip
reverse_acc : (List<a>, List<a>) -> List<a>
reverse_acc = |list, acc| match list
  | [] -> acc
  | [x, ..xs] -> reverse_acc(xs, [x, ..acc])

// @fbip: Functional-but-in-place (compiler optimizes, no guarantee)
@fbip
transform : Tree<a> -> Tree<b>
transform = |tree| ...  // Compiler will optimize if possible
```

#### 1.9.4 Drop Specialization and Dup/Drop Fusion

The compiler performs aggressive optimization on reference counting operations:

**Drop Specialization**: Inline and specialize `drop` operations for each type, enabling further optimization.

**Dup/Drop Fusion**: When a value is duplicated (`dup`) and then one copy is immediately dropped, eliminate both operations.

```baseline
// Before optimization
let x = some_value
let y = x       // dup(x)
process(y)
// x goes out of scope: drop(x)

// After dup/drop fusion
let x = some_value
process(x)      // No dup, no drop—direct use
```

#### 1.9.5 Thread Safety

| Scenario | RC Type | Cost |
|----------|---------|------|
| Thread-local data | Non-atomic RC | Cheap integer ops |
| Data escaping to effect handler | Atomic RC | Moderate |
| Explicitly shared (`Shared<T>`) | Atomic RC | Moderate |

The compiler infers which data is thread-local and uses cheap non-atomic reference counting by default. Atomic RC is only used when data provably escapes to another thread or handler.

#### 1.9.6 Performance Characteristics

| Algorithm | Traditional FP | FBIP-Optimized | C++ Mutable |
|-----------|----------------|----------------|-------------|
| Tree map | O(n) allocs | 0 allocs | 0 allocs |
| List reverse | O(n) allocs | 0 allocs | 0 allocs |
| Red-black insert | O(log n) allocs | 0-1 allocs | 0 allocs |

Research benchmarks show FBIP achieves **within 10% of C++** for tree-heavy algorithms while maintaining purely functional semantics and memory safety.

---

## 2. Compilation

### 2.1 Compilation Pipeline

```
Source (.bl files)
        |
    Parsing (Tree-sitter)
        |
    Type Checking (Bidirectional)
        |
    Effect Inference
        |
    Specification Verification (SMT)
        |
    Baseline IR (verified intermediate representation)
        |
    Optimization
        |
    +-----------------------------------+
    |               |                   |
Cranelift       WebAssembly           LLVM
(dev builds)    (production)       (max perf)
    |               |                   |
Native binary   .wasm file        Native binary
```

### 2.2 Build Modes

```bash
# Development: fast compilation, debug info
baseline build
# Output: target/debug/myapp
# Compile time: ~100-200ms

# Release: optimized, smaller binary
baseline build --release
# Output: target/release/myapp
# Compile time: ~1-3s

# WebAssembly: portable binary
baseline build --wasm
# Output: target/wasm/myapp.wasm
# Compile time: ~500ms

# Cross-compilation
baseline build --target=linux-arm64
baseline build --target=macos-x64
baseline build --target=windows-x64
```

### 2.3 Incremental Compilation

Baseline supports fine-grained incremental compilation:

```bash
$ baseline build
Compiling src/main.bl... done (145ms)

# Edit src/api/users.bl

$ baseline build
Recompiling src/api/users.bl... done (23ms)
Linking... done (12ms)
```

### 2.4 Binary Output

| Target | Typical Size | Startup | Notes |
|--------|--------------|---------|-------|
| Native (Cranelift) | 5-10 MB | <5ms | Fast builds |
| Native (LLVM) | 3-7 MB | <5ms | Best performance |
| WebAssembly | 1-3 MB | <10ms | Portable |
| WebAssembly + WASI | 1-3 MB | <10ms | Server-side |

### 2.5 Embedding

Baseline can be embedded in other languages:

```c
// C/C++ embedding
#include "baseline.h"

baseline_runtime_t* rt = baseline_init();
baseline_module_t* mod = baseline_load(rt, "myapp.wasm");
baseline_value_t result = baseline_call(mod, "greet", "World");
printf("%s\n", baseline_to_string(result));  // "Hello, World"
baseline_free(rt);
```

```python
# Python embedding
import baseline

mod = baseline.load("myapp.wasm")
result = mod.greet("World")
print(result)  # "Hello, World"
```

---

## 3. Tracing and Debugging

### 3.1 Execution Tracing

Baseline programs can emit structured traces:

```bash
$ baseline build --trace
$ ./myapp
# Output: result
# Trace: myapp.trace
```

Trace format (binary, but shown as JSON for clarity):

```json
{
  "events": [
    {
      "timestamp_ns": 142000,
      "location": { "file": "api.bl", "line": 47, "col": 12 },
      "event": {
        "type": "function_call",
        "name": "get_user",
        "args": { "id": 42 }
      }
    },
    {
      "timestamp_ns": 891000,
      "location": { "file": "api.bl", "line": 48, "col": 5 },
      "event": {
        "type": "effect_executed",
        "effect": "Db.query",
        "input": ["SELECT * FROM users WHERE id = ?", [42]],
        "output": { "Ok": [{ "id": 42, "name": "Alice" }] },
        "duration_ns": 749000
      }
    }
  ]
}
```

### 3.2 Trace Analysis

```bash
$ baseline trace analyze myapp.trace

Summary:
  Duration: 1.24s
  Events: 1,247
  Effects: 89 (Http: 12, Db: 45, Log: 32)
  Allocations: 2.3 MB

Anomalies:
  ! Db.query at api.bl:48 took 2.3s (expected <100ms)
    Hypothesis: missing index on users.email

  ! Unexpected None at api.bl:52
    get_user returned None for id=42
    Prior call showed user 42 exists
    Hypothesis: race condition or cache inconsistency

Hot paths:
  1. handle_request -> get_user -> Db.query (45% of time)
  2. handle_request -> render -> Json.encode (23% of time)
```

### 3.3 Time-Travel Debugging

Replay execution deterministically:

```bash
$ baseline trace replay myapp.trace --until "api.bl:52"

State at api.bl:52:
  locals:
    id: 42
    user: None
    request: { method: "GET", path: "/users/42" }

  call_stack:
    main!
    handle_request!
    get_user!

  effects_executed:
    Db.query("SELECT * FROM users WHERE id = ?", [42]) -> []

$ baseline trace replay myapp.trace --query "why is user None?"

Analysis:
  The Db.query returned an empty result set.

  Possible causes:
  1. User with id=42 does not exist
  2. Query executed against wrong database
  3. Prior delete not visible (isolation level)

  Suggestion: Check database state at query time with --capture-db
```

### 3.4 Hypothesis Testing

Test fixes against captured traces:

```bash
$ baseline trace replay myapp.trace --patch fix.bl --dry-run

Original outcome: Error("unwrap on None at api.bl:52")
Patched outcome: Ok(NotFound)

Spec compliance: pass
Side effects changed: none

Verdict: Fix resolves the issue without changing success behavior
```

### 3.5 Structured Error Output

All errors are machine-parseable:

```json
{
  "error": {
    "type": "type_mismatch",
    "location": { "file": "api.bl", "line": 47, "col": 12 },
    "expected": "User",
    "actual": "Option<User>",
    "context": {
      "function": "handle_request",
      "expression": "get_user(id)"
    },
    "suggestions": [
      {
        "action": "unwrap_with_match",
        "code": "match get_user(id)\n  Some(u) -> u\n  None -> return NotFound",
        "confidence": 0.9
      },
      {
        "action": "propagate_option",
        "code": "get_user(id)?",
        "signature_change": "returns User?",
        "confidence": 0.7
      }
    ]
  }
}
```

### 3.6 Debugging Commands

```bash
baseline trace <file>             # Analyze trace file
baseline trace replay <file>      # Replay execution
baseline trace diff <a> <b>       # Compare two traces
baseline trace bisect <files...>  # Find regression
baseline trace query <file> "..." # Ask questions
baseline trace export <file>      # Export to JSON/Chrome trace
```

---

## 4. SARIF Diagnostics

For LLM repair loops and CI/CD integration, Baseline supports **SARIF (Static Analysis Results Interchange Format)** output—the industry standard for machine-parseable diagnostics.

### 4.1 Why SARIF

| Format | Human Readable | Machine Parseable | Context-Rich | Standard |
|--------|----------------|-------------------|--------------|----------|
| Plain text | yes | no | no | no |
| JSON (custom) | no | yes | yes | no |
| SARIF | no | yes | yes | yes |

SARIF is supported by GitHub, VS Code, and major CI systems. Using SARIF means Baseline diagnostics integrate automatically with existing toolchains.

### 4.2 CLI Usage

```bash
# Human-readable output (default)
baseline check src/

# SARIF output for tools
baseline check src/ --format=sarif > results.sarif

# JSON output (Baseline's native format)
baseline check src/ --format=json
```

### 4.3 SARIF Structure

```json
{
  "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
  "version": "2.1.0",
  "runs": [{
    "tool": {
      "driver": {
        "name": "baseline",
        "version": "0.1.0",
        "rules": [{
          "id": "REF_001",
          "name": "RefinementViolation",
          "shortDescription": { "text": "Value does not satisfy type refinement" },
          "helpUri": "https://baseline-lang.dev/errors/REF_001"
        }]
      }
    },
    "results": [{
      "ruleId": "REF_001",
      "level": "error",
      "message": {
        "text": "Cannot prove `port >= 1`: value could be 0"
      },
      "locations": [{
        "physicalLocation": {
          "artifactLocation": { "uri": "src/server.bl" },
          "region": {
            "startLine": 42,
            "startColumn": 15,
            "endColumn": 19,
            "byteOffset": 1847,
            "byteLength": 4
          }
        }
      }],
      "relatedLocations": [{
        "physicalLocation": {
          "artifactLocation": { "uri": "src/types.bl" },
          "region": { "startLine": 5 }
        },
        "message": { "text": "Port type defined here with refinement: 1 <= self <= 65535" }
      }],
      "codeFlows": [{
        "message": { "text": "Execution path leading to violation" },
        "threadFlows": [{
          "locations": [
            { "location": { "message": { "text": "port assigned from user input" } } },
            { "location": { "message": { "text": "no validation before use" } } }
          ]
        }]
      }],
      "fixes": [{
        "description": { "text": "Add guard clause" },
        "artifactChanges": [{
          "artifactLocation": { "uri": "src/server.bl" },
          "replacements": [{
            "deletedRegion": { "startLine": 42, "startColumn": 1, "endColumn": 1 },
            "insertedContent": { "text": "if port < 1 then return Err(InvalidPort)\n" }
          }]
        }]
      }]
    }]
  }]
}
```

### 4.4 Fix Confidence Levels

Following Rust's model, suggestions include confidence levels:

| Level | Meaning | LLM Action |
|-------|---------|------------|
| `MachineApplicable` | Safe to auto-apply | Apply directly |
| `HasPlaceholders` | Requires user input | Fill placeholders, then apply |
| `MaybeIncorrect` | Might not match intent | Present to user for confirmation |
| `Unspecified` | Unknown confidence | Treat as suggestion only |

```json
{
  "fixes": [{
    "description": { "text": "Parse string to Int" },
    "properties": {
      "confidence": "MachineApplicable"
    },
    "artifactChanges": [...]
  }]
}
```

### 4.5 SMT Counter-Examples in codeFlows

For refinement type failures, the SMT solver's counter-example is included:

```json
{
  "codeFlows": [{
    "message": { "text": "Counter-example from SMT solver" },
    "properties": {
      "counterExample": {
        "port": 0,
        "reason": "Division by zero when port=0 reaches line 47"
      }
    }
  }]
}
```

---

## 5. Constrained Decoding API

For advanced LLM integration, Baseline exposes type-checker state to enable **type-constrained token generation**—guiding LLM sampling toward well-typed code.

### 5.1 Motivation

Research shows that type-constrained decoding reduces compilation errors by **more than 50%**. LLMs treat programs as plain text and cannot reliably infer type constraints. By exposing the type-checker's knowledge, we guide generation toward valid programs.

### 5.2 Valid Next Tokens

```json
// Request: What tokens are valid at this position?
{
  "method": "baseline/validNextTokens",
  "params": {
    "source": "let x: Int = ",
    "cursorOffset": 13
  }
}

// Response
{
  "validTokens": {
    "literals": ["<integer>", "<hex_integer>", "<binary_integer>"],
    "identifiers": ["y", "config.port", "DEFAULT_VALUE"],
    "expressions": ["(", "-", "if"],
    "excluded": ["\"<string>\"", "true", "false", "[]"]
  },
  "expectedType": "Int",
  "refinements": []
}
```

### 5.3 Type Holes

```json
// Request: What type is expected at the placeholder?
{
  "method": "baseline/typeHole",
  "params": {
    "source": "let result = List.map(users, |u| _)",
    "holeOffset": 28
  }
}

// Response
{
  "expectedType": "B",
  "constraints": [
    "B is the return type of the map function",
    "u has type User"
  ],
  "availableFields": ["u.name: String", "u.email: Email", "u.age: Int"],
  "suggestedCompletions": [
    { "code": "u.name", "type": "String" },
    { "code": "u.email", "type": "Email" },
    { "code": "format_user(u)", "type": "String" }
  ]
}
```

### 5.4 Completion Constraints

```json
// Request: What refinements must hold at this position?
{
  "method": "baseline/completionConstraints",
  "params": {
    "source": "let port: Port = _",
    "holeOffset": 17
  }
}

// Response
{
  "expectedType": "Port",
  "baseType": "Int",
  "refinements": ["1 <= value <= 65535"],
  "validExamples": [1, 80, 443, 8080, 65535],
  "invalidExamples": [0, -1, 65536, 100000]
}
```

### 5.5 Streaming Integration

For real-time generation with streaming LLMs:

```json
// Request: Incremental check during generation
{
  "method": "baseline/streamCheck",
  "params": {
    "sessionId": "gen_abc123",
    "partialSource": "let x = List.fil",
    "newTokens": "ter"
  }
}

// Response
{
  "valid": true,
  "partialType": "(User -> Bool) -> List<User>",
  "nextExpected": ["("],
  "confidence": 0.95
}
```

### 5.6 Performance Requirements

| Operation | Target Latency | Notes |
|-----------|----------------|-------|
| `validNextTokens` | <10ms | Per-token during generation |
| `typeHole` | <20ms | Single query |
| `completionConstraints` | <15ms | Single query |
| `streamCheck` | <5ms | Must not block generation |

Implementation requires:
- Incremental parsing (tree-sitter based)
- Resumable type inference from partial AST
- Cached constraint solving

---

## 6. Advanced Testing

### 6.1 Table-Driven Tests

For testing many input/output combinations:

```baseline
describe "Port.parse" {
  it "validates port numbers" {
    given port
    when Port.parse
    expect result
  } examples {
    | port   | result              |
    | 80     | Ok(80)              |
    | 443    | Ok(443)             |
    | 8080   | Ok(8080)            |
    | 0      | Err(OutOfRange)     |
    | -1     | Err(OutOfRange)     |
    | 65535  | Ok(65535)           |
    | 65536  | Err(OutOfRange)     |
  }
}

describe "String.trim" {
  it "removes whitespace" {
    given input
    when String.trim
    expect output
  } examples {
    | input          | output    |
    | "  hello  "    | "hello"   |
    | "hello"        | "hello"   |
    | "  "           | ""        |
    | ""             | ""        |
    | "\t\nhello\n"  | "hello"   |
  }
}
```

### 6.2 Property-Based Testing

Properties define invariants that must hold for all inputs:

```baseline
describe "List.sort" {
  property "preserves length" {
    forall xs: List<Int>
    expect List.len(sort(xs)) == List.len(xs)
  }

  property "is idempotent" {
    forall xs: List<Int>
    expect sort(sort(xs)) == sort(xs)
  }

  property "produces ordered output" {
    forall xs: List<Int>
    expect List.all(List.windows(sort(xs), 2), |[a, b]| a <= b)
  }

  property "is a permutation of input" {
    forall xs: List<Int>
    expect List.to_multiset(sort(xs)) == List.to_multiset(xs)
  }
}
```

#### Conditional Properties

```baseline
describe "List.head" {
  property "returns first element for non-empty lists" {
    forall xs: List<Int>
    where List.len(xs) > 0
    expect List.first(xs) == Some(Option.unwrap(List.get(xs, 0)))
  }
}
```

#### Custom Generators

```baseline
describe "User validation" {
  let valid_email = Gen.string_matching(r"[a-z]+@[a-z]+\.[a-z]{2,}")
  let valid_name = Gen.string(1, 100)

  property "valid users are accepted" {
    forall name: valid_name, email: valid_email
    expect Result.is_ok(validate_user({ name, email }))
  }
}
```

#### Auto-Generated Properties from Refinement Types

```baseline
// This type definition:
type Port = Int where 1 <= self <= 65535

// Automatically generates:
property "Port satisfies refinement" {
  forall p: Port
  expect 1 <= p && p <= 65535
}
```

#### Standard Property Library

```baseline
import Baseline.Test.Properties.*

describe "reverse" {
  property "is an involution" = involution(reverse)
  property "preserves length" = preserves_len(reverse)
}

describe "sort" {
  property "is idempotent" = idempotent(sort)
  property "preserves elements" = permutation(sort)
  property "produces ordered output" = ordered(sort)
}
```

### 6.3 Doc Tests

Documentation examples are executable:

```baseline
/// Reverses a list.
///
/// ```
/// reverse([1, 2, 3]) == [3, 2, 1]
/// reverse([]) == []
/// ```
///
/// Works with any element type:
///
/// ```
/// reverse(["a", "b"]) == ["b", "a"]
/// ```
reverse : List<T> -> List<T>
reverse = |list| ...
```

### 6.4 Snapshot Testing

For complex outputs that are tedious to write manually:

```baseline
describe "Template rendering" {
  it "renders user profile" {
    given User { name: "Alice", bio: "Developer", posts: 42 }
    when render_profile
    expect snapshot("user_profile")
  }
}

describe "JSON serialization" {
  it "serializes complex objects" {
    given complex_object
    when Json.encode_pretty
    expect snapshot("complex_json")
  }
}
```

```bash
$ baseline test --update-snapshots
Updated 2 snapshots:
  - user_profile
  - complex_json
```

### 6.5 Test Execution CLI

```bash
$ baseline test

Running tests...

  src/math.bl
    pass add: adds positive numbers (0.1ms)
    pass add: handles negatives (0.1ms)
    pass add: zero identity (0.1ms)

  test/user_spec.bl
    UserService
      create_user
        with valid data
          pass creates the user (1.2ms)
          pass assigns a unique id (0.8ms)
        with invalid email
          pass returns validation error (0.5ms)
        with duplicate email
          pass returns conflict error (1.1ms)

  Properties
    pass List.sort: preserves length (100 cases)
    pass List.sort: is idempotent (100 cases)
    pass List.sort: produces ordered output (100 cases)

Tests: 10 passed, 0 failed
Properties: 3 passed (300 cases)
Time: 142ms
```

#### Filtering Tests

```bash
$ baseline test --filter "UserService"
$ baseline test --filter "create_user"
$ baseline test user_spec.bl
$ baseline test --tag integration
$ baseline test --tag "not slow"
```

#### Watch Mode

```bash
$ baseline test --watch

Watching for changes...
[12:34:56] Running tests affected by src/user.bl...
  pass 4 tests passed (23ms)

[12:35:12] Running tests affected by src/api.bl...
  fail 1 test failed

  UserService > delete_post > when user does not own the post
    Expected: Err(Unauthorized)
    Got: Err(NotFound)
```

### 6.6 Coverage and Mutation Testing

```bash
$ baseline test --coverage

Coverage: 87% (Lines: 412/474)

  src/user.bl        94%  xxxxxxxxxxxxxxxxoo
  src/api.bl         82%  xxxxxxxxxxxxxxxxoooo
  src/db.bl          71%  xxxxxxxxxxxxxxoooooo

Uncovered:
  src/api.bl:47-52 (error handler branch)
  src/db.bl:89 (connection retry logic)

$ baseline test --mutate

Mutation testing...
Generated 156 mutants
  Killed: 149 (95.5%)
  Survived: 7

Surviving mutants:
  src/math.bl:12
    - Changed `+` to `-`
    - No test catches this!

  src/api.bl:34
    - Changed `>=` to `>`
    - Edge case not tested
```

---

## 7. Extended Standard Library

### 7.1 Networking

```baseline
module Baseline.Net

export effect Http {
  get! : String -> Result<Response, HttpError>
  post! : (String, Body) -> Result<Response, HttpError>
  put! : (String, Body) -> Result<Response, HttpError>
  delete! : String -> Result<Response, HttpError>
  request! : Request -> Result<Response, HttpError>
}

export type Request = {
  method: Method,
  url: String,
  headers: Map<String, String>,
  body: Body?,
  timeout: Duration?,
}

export type Response = {
  status: Int,
  headers: Map<String, String>,
  body: Bytes,
}

export Response.text : Response -> Result<String, DecodeError>
export Response.json : Response -> Result<Json, DecodeError>
export Response.decode : Response -> Result<T, DecodeError> where T: Decode

export type Method = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS

export type HttpError =
  | ConnectionFailed(String)
  | Timeout
  | InvalidUrl(String)
  | TlsError(String)
```

### 7.2 JSON

```baseline
module Baseline.Json

export type Json =
  | Null
  | Bool(Bool)
  | Number(Float)
  | String(String)
  | Array(List<Json>)
  | Object(Map<String, Json>)

export Json.parse : String -> Result<Json, ParseError>
export Json.to_string : Json -> String
export Json.to_string_pretty : Json -> String

// Encoding/decoding via traits
export trait Encode {
  encode : Self -> Json
}

export trait Decode {
  decode : Json -> Result<Self, DecodeError>
}

// Derive implementations automatically
@derive(Encode, Decode)
type User = { name: String, age: Int }
```

### 7.3 Time

```baseline
module Baseline.Time

export effect Time {
  now! : () -> Timestamp
  sleep! : Duration -> ()
}

export type Timestamp
export Timestamp.from_unix : Int -> Timestamp
export Timestamp.to_unix : Timestamp -> Int
export Timestamp.format : (Timestamp, String) -> String
export Timestamp.parse : (String, String) -> Result<Timestamp, ParseError>

export type Duration
export Duration.from_secs : Int -> Duration
export Duration.from_millis : Int -> Duration
export Duration.from_nanos : Int -> Duration
export Duration.to_secs : Duration -> Int
export Duration.to_millis : Duration -> Int

export Int.seconds : Int -> Duration
export Int.milliseconds : Int -> Duration
export Int.minutes : Int -> Duration
export Int.hours : Int -> Duration
```

### 7.4 Concurrency

```baseline
module Baseline.Concurrent

// Strict structured concurrency — spawn requires a scope
export scope! : (Scope -> {e, Async} T) -> {e, Async} T
export Scope.spawn! : (Scope, () -> {e} T) -> {e, Async} Cell<T>
export Cell.await! : Cell<T> -> {Async} T
export Cell.cancel! : Cell<T> -> {Async} ()

// Combinators (built on scope! — cannot leak tasks)
export parallel! : List<() -> {e} T> -> {e, Async} List<T>
export race! : List<() -> {e} T> -> {e, Async} T
export scatter_gather! : (List<() -> {e} T>, List<T> -> R) -> {e, Async} R

// Bounded channels only (no unbounded)
export type Channel<T>
export Channel.bounded : Int -> (Channel<T>, Channel<T>)
export Channel.send! : (Channel<T>, T) -> {Async} ()
export Channel.recv! : Channel<T> -> {Async} Option<T>
export Channel.close! : Channel<T> -> ()

// Supervision
export supervise! : (SupervisionStrategy, Scope -> {e, Async} T) -> {e, Async} T

// Example
process_all! : List<Url> -> {Http, Async} List<Response>
process_all! = |urls|
  urls
  |> List.map(|url| || Http.get!(url))
  |> parallel!
```

---

## 8. WebAssembly Interface

Baseline modules export a standard interface:

```wat
(module
  ;; Memory
  (memory (export "memory") 1)

  ;; Allocation
  (func (export "alloc") (param i32) (result i32))
  (func (export "dealloc") (param i32 i32))

  ;; Entry point
  (func (export "_start"))

  ;; Effect handlers (imported)
  (import "baseline" "effect_http_get" (func $http_get (param i32 i32) (result i32)))
  (import "baseline" "effect_console_print" (func $console_print (param i32 i32)))
  ;; ... etc
)
```

---

## 9. Concurrency Model

Baseline uses **strict structured concurrency** paired with algebraic effects. All concurrency is scoped — fire-and-forget tasks are illegal. This is not a stylistic preference; it is a deliberate design decision that enables zero-cost memory regions across fiber boundaries, capability-based sandboxing, and SMT verification of concurrent code.

**Why not Go-style goroutines?** Go's `go func()` is unstructured concurrency — essentially a `goto` for control flow. Unstructured spawns can outlive their callers, forcing data into the heap with ARC and destroying region optimizations. LLMs are also notoriously bad at non-local temporal reasoning: they forget cancellation contexts, channel closures, and WaitGroups, leading to silent memory leaks and deadlocks. SMT solvers cannot verify properties about unbound, dynamic execution graphs.

**Why not Erlang/Scala actors?** Erlang relies on untyped mailboxes and runtime "let it crash" supervisors — fundamentally incompatible with compile-time SMT verification. Making actors type-safe (Akka Typed) requires massive state-machine hierarchies that bloat the LLM context window. Actors also encourage stateful, object-oriented-adjacent architectures that conflict with FBIP (Section 1.9). Baseline achieves Erlang's isolation and resilience through structured scopes, channels, and effect handlers — without a separate actor runtime.

### 9.1 Cells and Scopes

Cells are lightweight cooperative threads (initial stack: 2KB, growable). **Cells can only be spawned inside a `scope!` block.** The scope guarantees all children complete (or are cancelled) before execution continues past the scope boundary.

```baseline
process! : List<Item> -> {Async} List<Result>
process! = |items|
  scope! |s|
    for item in items do
      s.spawn!(|| process_item!(item))
    // Implicit: scope waits for all spawned cells
    // If any cell panics, others are cancelled
```

There is no top-level `spawn!`. This is intentional:

```baseline
// ILLEGAL — compile error: spawn! requires a scope
let cell = spawn!(|| background_task!())

// LEGAL — all concurrency is scoped
scope! |s|
  let cell = s.spawn!(|| background_task!())
  Cell.await!(cell)
```

### 9.2 Scopes as Zero-Cost Memory Regions

Structured concurrency maps the temporal lifetime of a fiber to a spatial AST node. Because the compiler statically knows that all fibers die before the `scope!` exits, fibers can safely borrow region-allocated data **without ARC or lifetime annotations**.

```baseline
handle_request! : Request -> {Http, Async} Response
handle_request! = |req|
  // Region allocated for this request scope
  let user_data = parse_body(req)

  scope! |s|
    // Both fibers can safely read user_data — the compiler proves
    // they will die before the scope exits and user_data is freed.
    let auth = s.spawn!(|| validate_auth!(user_data.token))
    let enriched = s.spawn!(|| enrich_profile!(user_data.id))
    build_response(auth.await!(), enriched.await!())
  // Region freed in O(1) here — no per-object deallocation
```

This is a **massive competitive advantage** over `Arc<Mutex<T>>`. The compiler proves safety at compile time; the runtime pays zero synchronization cost.

### 9.3 Parallel Combinators

For the common case, combinators eliminate manual scope/spawn/await boilerplate. LLMs write map-reduce pipelines reliably — these should be the primary concurrency API.

```baseline
// Run multiple tasks, collect all results
let results = parallel!([
  || fetch_users!(),
  || fetch_posts!(),
  || fetch_comments!(),
])

// Race: return first to complete, cancel others
let fastest = race!([
  || fetch_from_primary!(),
  || fetch_from_replica!(),
])

// Scatter-gather: fan out, aggregate results
let consensus = scatter_gather!(
  [|| ask_claude!(q), || ask_gpt!(q), || ask_llama!(q)],
  |results| majority_vote(results)
)
```

All combinators are implemented in terms of `scope!` — they cannot leak tasks.

### 9.4 Channels

Bounded channels with backpressure. **Only bounded channels exist** — unbounded channels are a memory leak waiting to happen.

```baseline
scope! |s|
  let (tx, rx) = Channel.bounded<Int>(100)

  // Producer
  s.spawn! ||
    for i in 1..1000 do
      Channel.send!(tx, i)   // Blocks if buffer full
    Channel.close!(tx)

  // Consumer
  s.spawn! ||
    while let Some(n) = Channel.recv!(rx) do
      process!(n)
```

### 9.5 FBIP Agents: Erlang Isolation at Native Speed

Baseline does not need a dedicated Actor primitive. Stateful concurrent entities are just **tail-recursive loops processing a channel**, using FBIP for zero-allocation in-place mutation.

```baseline
type AgentMsg =
  | Ask(String, Channel<String>)
  | UpdateContext(String)

// @fip guarantees this compiles to a zero-allocation, in-place mutable loop.
@fip
fn run_agent!(context: List<String>, inbox: Channel<AgentMsg>) -> {Async, Llm} () =
  match Channel.recv!(inbox)
    Some(Ask(q, reply_tx)) ->
      Channel.send!(reply_tx, Llm.infer!(context, q))
      run_agent!(context, inbox)           // Tail-call optimized

    Some(UpdateContext(info)) ->
      let next = List.append(context, info) // FBIP: mutates in-place!
      run_agent!(next, inbox)

    None -> ()                              // Channel closed, agent stops
```

This gives you all the benefits of Erlang actors — isolated state, message passing, graceful shutdown — but it is 100% statically typed, SMT-verifiable, uses standard pattern matching, and runs at native C speeds with no per-message allocation.

### 9.6 Supervision via Effect Handlers

Erlang's true superpower is supervision trees and resilience. In an era where AI agents rely on flaky external APIs, code will fail constantly. Baseline achieves fault tolerance through **effect handlers that wrap scopes**, not a separate supervisor runtime.

```baseline
fetch_consensus! : Query -> {Http, Async} Result<Answer, Error>
fetch_consensus! = |query|
  // Erlang's resilience, 100% statically typed and direct-style
  supervise!(strategy: Restarts(max: 3, delay: 1.second)) |s|
    let claude = s.spawn!(|| ask_claude!(query))
    let gpt = s.spawn!(|| ask_gpt!(query))

    Ok(compare_answers(claude.await!(), gpt.await!()))
```

`supervise!` is an effect handler that catches panics from child fibers and restarts the scope according to the strategy. Because it composes with the effect system, you can nest supervisors, combine them with other effects, and verify their behavior with SMT.

### 9.7 Capability-Based Agent Sandboxing

Because `Async` is an effect and all effects are capabilities, structured concurrency enables **compile-time sandboxing of untrusted code**. If an orchestrator spawns a fiber to execute LLM-generated code, the type system mathematically restricts that fiber's capabilities.

```baseline
scope! |s|
  // Worker can compute concurrently, but is mathematically proven
  // to CANNOT touch {Db}, {Fs}, or {Http}.
  let result = s.spawn!(|| execute_untrusted_prompt(input)) with { Async, Pure }
  Cell.await!(result)
```

Neither Go nor Erlang can do this natively. By treating concurrency and I/O as explicit capabilities, Baseline provides the strongest safety guarantees of any language for autonomous AI code execution.

### 9.8 Async as an Effect

Async is an effect, not function coloring. There are no `async`/`await` keywords — asynchronous I/O, generators, and exceptions are all just effects:

```baseline
fetch_all! : List<Url> -> {Http, Async} List<Response>
fetch_all! = |urls|
  parallel!(List.map(urls, |url| || Http.get!(url)))

// Provide runtime at the edge
fn main!() -> {Console} () =
  let runtime = Async.runtime({ threads: num_cpus() })
  let results = fetch_all!(urls) with { runtime }
  Console.println!(results)
```

### 9.9 Runtime Configuration

```baseline
Async.runtime({
  threads: 4,                    // Worker threads (default: num_cpus)
  stack_size: 2.kb,              // Initial cell stack
  max_stack: 1.mb,               // Maximum stack growth
  scheduler: WorkStealing,       // Or: SingleThreaded, Custom
})
```

### 9.10 WebAssembly Considerations

| Environment | Concurrency Model |
|-------------|-------------------|
| Native | Full thread pool, work-stealing |
| WASI Threads | Thread pool (where supported) |
| Browser Wasm | Single-threaded event loop |
| Edge (Workers) | Single-threaded, async I/O |

Code is identical — the runtime adapts:

```baseline
@runtime(wasm_threads: auto)  // Use threads if available
module MyService
```

---

## 10. Future Considerations

### 10.1 Design Decisions

The following decisions were informed by a comparative analysis with Koka, Austral, Roc, Unison, Dafny, and Zig. Each decision addresses a specific gap in Baseline's "Safe Agent" story.

#### Decision: `comptime` over Macros

Baseline will use **Zig-style `comptime`** instead of hygienic macros for metaprogramming. Macros create a second "meta-language" that increases LLM hallucination rates and violates the One Way principle. `comptime` runs standard Baseline code at compile time, keeping one syntax for both runtime and compile-time logic.

```baseline
// comptime: same syntax, evaluated at compile time
comptime {
  let schema = Json.parse_schema!(read_file!("schema.json"))
  generate_validator(schema)
}
```

**Timeline:** Requires a real compiler (not interpreter). Targeted for post-bootstrap phase.

#### Decision: Scoped Resources over Full Linear Types

Baseline will use **scoped resource blocks** as the primary resource safety mechanism, rather than Austral/Rust-style full linear types. Effects control *permission* ("can I open a file?"), but don't enforce *obligation* ("must I close this file?"). Scoped blocks solve this with a pattern LLMs already know:

```baseline
// file cannot escape the closure — structurally impossible to leak
Fs.with_file!("data.txt", |file| {
  let content = Fs.read!(file)
  transform(content)
})
// file guaranteed closed here
```

Full linear type annotations (`Linear<T>`) are available as an opt-in escape hatch for advanced use cases where scoped blocks are insufficient (e.g., resources that must be transferred between functions).

**Rationale:** Scoped blocks achieve 90% of linear type safety with 10% of the complexity. Rust's borrow checker is notoriously difficult for LLMs (vision doc Section 1.5 comparison table). Austral's model is simpler but still adds annotation burden.

#### Decision: Platforms (Evolving Preludes)

Preludes will be formalized as **Platform security boundaries** that map to Wasm Component Model worlds. Currently preludes are a configuration-level convenience. Platforms make the enforcement a linker guarantee:

- `@prelude(server)` maps to a Component Model world that imports `Http`, `Db`, `Log`, `Time`, `Metrics`, `Env`
- If the host binary doesn't link `effect_fs_read`, the import doesn't resolve — hard failure
- Ergonomic syntax unchanged; enforcement moves from convention to linker guarantee

**Rationale:** An agent targeting a "serverless" Platform *physically cannot* access the filesystem if the Platform binary doesn't link that primitive. This upgrades security from language convention to linker guarantee, directly supporting the "Safe Agent" goal (Spec Section 1.5.4).

#### Decision: Weak References for Cycle Safety

Perceus reference counting leaks memory on cycles (A→B→A). While functional immutable data rarely creates cycles, long-running agents with persistent state (caches, connection pools, graph structures) are at risk. Baseline will add `Weak<T>` references following Swift's proven model:

```baseline
type Node = {
  value: Int,
  parent: Weak<Node>,   // does not prevent deallocation
  children: List<Node>,  // strong references
}
```

A diagnostic cycle detector (Bacon-Rajan style) may be added in debug builds as a development tool, but will never be a runtime dependency — the "no GC pauses" guarantee is a core differentiator.

#### Decision: Reject Content-Addressed Imports

Unison-style content addressing (imports by AST hash) was evaluated and **rejected**. Content hashes are opaque to LLMs, directly contradicting the LLM-native design principle. The actual problem — LLMs hallucinating imports — is better solved by the Constrained Decoding API (Section 5) and the Interactive Refinement Protocol (Spec Section 1.5.5), which tell the LLM exactly what's available. Supply chain security is addressed at the package manager level (lock files, integrity hashes, SBOM), not the language level.

#### Decision: Ghost Code (Deferred)

Dafny-style `ghost` expressions — code that exists only during verification and is erased at runtime — are a useful mechanism for helping SMT solvers prove complex properties. However, the current refinement checker only supports integer intervals, and the spec already has tiered verification levels (Spec Section 8.5) with `@assume` annotations and `--timeout` flags. Ghost code becomes relevant when the specification language supports regex, custom predicates, and richer properties. Targeted for post-v0.9.

### 10.2 Planned Features

Features under consideration for future versions, ordered by priority:

1. **Type inference** — Hindley-Milner or bidirectional inference. **Critical path** for reducing annotation burden on LLMs and enabling row polymorphism for effects. Prerequisite for most other type system features.
2. **Scoped resource blocks** — `with_file!`, `with_connection!` patterns that guarantee resource cleanup at compile time. Simpler than linear types, familiar to LLMs.
3. **Weak references** — `Weak<T>` for breaking reference cycles in long-lived data structures.
4. **Platform security boundaries** — Formalize preludes as Wasm Component Model worlds with linker-enforced capability sets.
5. **Traits/Typeclasses** — Ad-hoc polymorphism beyond structural typing.
6. **`comptime` evaluation** — Run standard Baseline code at compile time for type generation and schema validation.
7. **Dependent types** — Full dependent typing for more expressive specifications.
8. **Ghost code** — Verification-only expressions erased at runtime to help SMT solvers with complex proofs.
9. **Distributed effects** — First-class support for distributed systems.
10. **Hot code reloading** — Update running systems without restart.
11. **Formal verification** — Integration with proof assistants.

---

## 11. LLM Training Bootstrap

Strategy and tooling for bootstrapping LLM training data for Baseline code generation.

### 11.1 The Cold Start Problem

New languages face a fundamental challenge: LLMs perform poorly on languages with little training data, but training data requires code written in the language. Research shows low-resource languages have **significantly worse** LLM performance than high-resource languages like Python or JavaScript.

### 11.2 Bootstrap Pipeline

The recommended pipeline for Baseline training data:

```
+---------------------------------------------------------------+
|  Phase 1: Seed Corpus (100-1000 examples)                       |
|  Hand-written, verified Baseline code covering core patterns      |
+---------------------------------------------------------------+
                              |
+---------------------------------------------------------------+
|  Phase 2: Translation (5K-10K examples)                         |
|  TransCoder-style translation from TypeScript/Rust              |
+---------------------------------------------------------------+
                              |
+---------------------------------------------------------------+
|  Phase 3: OSS-Instruct Expansion (10K-20K examples)             |
|  Generate instruction prompts from existing Baseline snippets     |
+---------------------------------------------------------------+
                              |
+---------------------------------------------------------------+
|  Phase 4: Self-Instruct Amplification (20K-75K examples)        |
|  Use existing examples to generate synthetic pairs              |
+---------------------------------------------------------------+
                              |
+---------------------------------------------------------------+
|  Phase 5: Execution-Based Filtering                             |
|  Run through compiler + test suite, keep only verified          |
+---------------------------------------------------------------+
```

### 11.3 Tooling

```bash
# Translate TypeScript to Baseline (best-effort)
baseline synth translate src/api.ts
# Output: src/api.bl (may need manual review)

# Generate instruction prompt for existing code
baseline synth instruct src/validated.bl
# Output: "Write a function that validates email addresses
#          and returns a typed Email value..."

# Verify corpus and filter to passing examples
baseline synth verify ./corpus/
# Output: Verified: 847/1000 (84.7%)
#         Failed type check: 98
#         Failed refinement: 55

# Analyze corpus coverage
baseline synth stats ./corpus/
# Output:
#   Syntax coverage:
#     Pattern matching: 89%
#     Pipe operators: 76%
#     Effect declarations: 45%
#
#   Type coverage:
#     Refinement types: 34%
#     Row polymorphism: 28%
#     Result/Option: 92%
#
#   Effect coverage:
#     Http: 67%
#     Db: 45%
#     Console: 89%
```

### 11.4 Seed Corpus Requirements

The initial 100-1000 examples should cover:

| Category | Examples | Priority |
|----------|----------|----------|
| Basic functions | 50+ | Required |
| Pattern matching | 30+ | Required |
| Refinement types | 30+ | Required |
| Effect usage | 30+ | Required |
| Error handling (Result/?) | 30+ | Required |
| Pipe operators | 20+ | Required |
| Records and row types | 20+ | Required |
| Specifications (@spec) | 20+ | High |
| Inline tests | 20+ | High |
| Module structure | 10+ | Medium |

### 11.5 Quality Gates

| Phase | Minimum Requirement |
|-------|---------------------|
| Seed corpus | 100% pass `--verify=full` |
| Translation | 80%+ pass type check |
| OSS-Instruct | 90%+ pass type check |
| Self-Instruct | 85%+ pass type check |
| Final corpus | 95%+ pass `--verify=types` |

### 11.6 Translation Strategy

Baseline's syntax is designed to be translatable from well-supported languages:

| Source | Target Pattern | Confidence |
|--------|----------------|------------|
| TypeScript interfaces | Baseline records | High |
| TypeScript functions | Baseline functions | High |
| Rust Result/Option | Baseline Result/Option | Very High |
| Rust pattern matching | Baseline pattern matching | Very High |
| Python type hints | Baseline types | Medium |
| Go error handling | Baseline Result + ? | Medium |

```typescript
// TypeScript source
interface User {
  name: string;
  email: string;
  age: number;
}

function greet(user: User): string {
  return `Hello, ${user.name}`;
}
```

```baseline
// Baseline translation
type User = {
  name: String,
  email: String,
  age: Int,
}

greet : User -> String
greet = |user| "Hello, ${user.name}"
```

### 11.7 Research Basis

This strategy is based on:

- **Self-Instruct** (Code Alpaca): 20 seeds -> 20K+ examples
- **OSS-Instruct** (Magicoder): Code snippets -> instruction pairs, 75K examples
- **TransCoder** (NeurIPS 2020): Unsupervised translation, 74.8% accuracy C++->Java
- Empirical finding: **100 real examples + synthetic expansion yields 3-26% improvement**

Beyond 1000 real examples, synthetic augmentation provides diminishing returns—focus on quality over quantity for the seed corpus.

---

*End of Vision Document*
