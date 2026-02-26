# Error Message Catalog

Every diagnostic code emitted by the Baseline compiler (`blc`), with descriptions and example triggers.

## Severity Levels

- **Error** — Compilation fails; must be fixed
- **Warning** — Compilation succeeds; suggest improvement

## Syntax Errors (SYN)

| Code | Severity | Description |
|------|----------|-------------|
| SYN_001 | Error | Unexpected token in source |
| SYN_002 | Error | Missing expected syntax element |

```baseline
// SYN_001: Unexpected token
let x = ;           // unexpected `;`

// SYN_002: Missing expected element
fn add(a: Int, b    // missing `)` and return type
```

## Type Errors (TYP)

| Code | Severity | Description |
|------|----------|-------------|
| TYP_001 | Error | Binary operator type mismatch (e.g., Int + String) |
| TYP_002 | Error | Undefined variable |
| TYP_003 | Error | If condition must be Boolean |
| TYP_004 | Error | Match arm type mismatch |
| TYP_006 | Error | Return type doesn't match body |
| TYP_007 | Error | Wrong number of arguments |
| TYP_008 | Error | Argument type mismatch |
| TYP_009 | Error | Called expression is not a function |
| TYP_010 | Error | Record field type mismatch |
| TYP_011 | Error | Struct has no such field |
| TYP_012 | Error | Missing required field |
| TYP_013 | Error | Unknown type name |
| TYP_014 | Error | Field access on non-record type |
| TYP_015 | Error | Method not found on type |
| TYP_016 | Error | For loop requires a List |
| TYP_017 | Error | Range operand must be Int |
| TYP_018 | Error | Pipe function expects wrong argument count |
| TYP_019 | Error | Pipe argument type mismatch |
| TYP_020 | Error | Pipe target is not a function |
| TYP_021 | Error | Let binding type mismatch |
| TYP_022 | Error | Pattern match arm type mismatch |
| TYP_023 | Error | Non-exhaustive pattern match |
| TYP_024 | Error | List literal element type mismatch |
| TYP_025 | Error | Logical NOT requires Bool |
| TYP_026 | Error | Test expression must be Bool |
| TYP_027 | Error | Record update on non-record type |
| TYP_028 | Error | Record update field type mismatch |
| TYP_029 | Error | Record update: field not found |
| TYP_030 | Error | Positional argument after named argument |
| TYP_031 | Error | Duplicate named argument |
| TYP_032 | Error | Unknown named argument |
| TYP_033 | Error | Lambda parameter type mismatch |
| TYP_040 | Error | Row.decode argument error |
| TYP_041 | Error | Row.decode: unknown struct type |
| TYP_042 | Error | Row.decode: field type not SQL-decodable |

```baseline
// TYP_001: Binary operator mismatch
let x = 1 + "hello"         // Int + String

// TYP_002: Undefined variable
let y = unknown_var          // `unknown_var` not defined

// TYP_003: Non-boolean condition
if 42 then "yes" else "no"   // condition must be Bool

// TYP_006: Return type mismatch
fn bad(x: Int) -> String = x + 1  // body returns Int, declared String

// TYP_007: Wrong argument count
fn add(a: Int, b: Int) -> Int = a + b
let r = add(1)               // expects 2 args, got 1

// TYP_011: Unknown field
type User = { name: String }
let u = User { name: "Alice", age: 30 }  // no field `age`

// TYP_021: Binding type mismatch
let x: String = 42           // declared String, found Int
```

## Effect Errors (CAP)

| Code | Severity | Description |
|------|----------|-------------|
| CAP_001 | Error | Unauthorized side effect — calling an effectful function without declaring the effect |
| CAP_002 | Error | Effect not permitted in restrict block |
| CAP_003 | Error | @pure function transitively uses effects |

```baseline
// CAP_001: Unauthorized effect
fn pure_fn(x: Int) -> {Http} Int = {
  let _ = Console.println!("hi")  // Console not in declared {Http}
  x
}

// CAP_003: @pure violation
@pure
fn bad(x: Int) -> Int = {
  let _ = Console.println!("oops")  // @pure cannot use effects
  x
}
```

## Import Errors (IMP)

| Code | Severity | Description |
|------|----------|-------------|
| IMP_001 | Error | Symbol not found in module |
| IMP_002 | Error | Cannot resolve module path |
| IMP_003 | Error | Cyclic import dependency |
| IMP_004 | Error | Named symbol not found in module |

```baseline
// IMP_001: Symbol not found
import Math { nonexistent }   // `nonexistent` not in Math

// IMP_002: Module not found
import FakeModule             // no FakeModule.bl file
```

## Refinement Errors (REF)

| Code | Severity | Description |
|------|----------|-------------|
| REF_001 | Error | Integer value out of bounds for refinement type |
| REF_002 | Error | String value violates refinement constraint |

```baseline
// REF_001: Out of bounds
type Port = Int where self > 0 && self <= 65535
let p: Port = 70000          // 70000 out of [1, 65535]

// REF_002: String violation
type Email = String where String.matches(self, "^[^@]+@[^@]+$")
let e: Email = "not-email"   // doesn't match pattern
```

## Trait Errors (TRT)

| Code | Severity | Description |
|------|----------|-------------|
| TRT_001 | Error | Unknown trait name |
| TRT_002 | Error | Impl block missing trait methods |
| TRT_004 | Error | No impl of trait for type |
| TRT_005 | Error | Method signature doesn't match trait |
| TRT_006 | Error | Supertrait not satisfied |
| TRT_007 | Error | Cannot impl trait for non-concrete type |
| TRT_008 | Error | Unknown supertrait |
| TRT_009 | Error | Cyclic trait inheritance |
| TRT_010 | Error | Missing method in impl |
| TRT_011 | Error | Type doesn't satisfy trait bound |

## Other Errors

| Code | Severity | Description |
|------|----------|-------------|
| PRE_001 | Error | Prelude level configuration error |
| RES_001 | Error | Scoped resource handle escapes its closure |
| STY_001 | Warning | Nested call could use pipe syntax |

```baseline
// STY_001: Pipe suggestion (warning, not an error)
let r = f(g(x))              // Suggests: x |> g |> f

// RES_001: Resource escape
fn bad() -> () = {
  let handle = open_resource!()
  let closure = || handle     // handle cannot escape scope
}
```

## Error Code Ranges

| Prefix | Category | Count |
|--------|----------|-------|
| SYN | Syntax parsing | 2 |
| TYP | Type checking | 32 |
| CAP | Effect checking | 3 |
| IMP | Import resolution | 4 |
| REF | Refinement checking | 2 |
| TRT | Trait checking | 10 |
| PRE | Prelude loading | 1 |
| RES | Resource safety | 1 |
| STY | Style suggestions | 1 |
| **Total** | | **56** |
