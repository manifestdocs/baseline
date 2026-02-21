# RFC: Ergonomic Guard Clauses in an Expression-Oriented Language

## 1. Summary

Baseline's strict adherence to expression-oriented design and the "One Way" principle explicitly forbids the `return` keyword. While this enforces functional purity and simplifies LLM generation by removing control-flow edge cases, it introduces ergonomic friction for developers accustomed to using "guard clauses" (early returns) to prevent nested code blocks. 

This RFC proposes three mechanisms to recover the ergonomics of traditional guard clauses without compromising Baseline's expression-oriented nature or introducing imperative jumps.

## 2. Motivation

In imperative languages, guard clauses are pivotal for keeping the "happy path" linear:

```python
def process(user):
    if user.age < 18:
        return "Too young"
    
    # Linear happy path continues without indentation...
    data = fetch(user)
    return validate(data)
```

In Baseline (v0.2), achieving this requires either an `if/else` expression (which introduces rightward indentation) or pushing validation into a complex `match` statement. The lack of a flat, linear "bail out" mechanism leads to rightward drift in complex functions.

We need a solution that:
1. Preserves the rule that "a block evaluates to its last expression."
2. Introduces minimal to no new keywords.
3. Aligns perfectly with the existing `?` error-propagation operator.

## 3. Proposed Solutions

### Option 1: The `ensure` + `?` Pattern (Least Invasive)

This approach requires **zero new syntax**. We add an `ensure` function to the `core` prelude that converts a boolean condition into an errorable `Result`.

```baseline
// In prelude(core)
fn ensure<E>(condition: Bool, err: E) -> Result<(), E> =
  if condition then Ok(()) else Err(err)
```

**Usage:**
Developers use the existing `?` operator to short-circuit if the condition fails.

```baseline
fn process_user!(user: User) -> {Db} Result<Data, String> = {
  // Guard clauses!
  ensure(user.age >= 18, "User is too young")?
  ensure(user.status == Active, "User is inactive")?
  
  // Linear happy path
  Ok(fetch_data!(user))
}
```

* **Pros:** No new keywords, reuses the existing mental model for error propagation (`?`), keeps the happy path flat, and fully respects expression-oriented purity.
* **Cons:** Feels slightly like a function call rather than a native language construct.

### Option 2: Block-Desugared `guard` Expression

Depending on `ensure` might feel slightly unintuitive. We could introduce a `guard` construct that **semantically desugars** into a nested `if/else` block at compile time.

```baseline
fn process!(user: User) -> {Db} Result<Data, Error> = {
  guard user.age >= 18 else Err(TooYoung)
  
  // The rest of the block implicitly becomes the `then` branch
  Ok(fetch_data!(user))
}
```

Under the hood, the compiler treats this exactly as:

```baseline
if not (user.age >= 18) then 
  Err(TooYoung)
else {
  Ok(fetch_data!(user))
}
```

* **Pros:** Looks identical to Swift's guard clauses, providing a familiar imperative "feel". Mathmatically, it remains a pure expression because the rest of the block is simply appended to the `else` branch during lowering.
* **Cons:** Introduces a new keyword. Might conceptually collide with the concept of `match` guards.

### Option 3: Pattern-Destructuring Guards (`let ... else`)

Often, a guard clause isn't just checking a boolean; it's trying to extract a value from an `Option` or `Result` (e.g., pulling a user from a database) and bailing if it fails. We could introduce a `let ... else` binding that, like `guard`, desugars the rest of the block into a `match`.

```baseline
fn process!(id: Int) -> {Db} Result<Data, Error> = {
  // Destructuring guard!
  let Some(user) = get_user!(id) else Err(NotFound)
  
  // The rest of the block is only evaluated if the match succeeded
  Ok(fetch_data!(user))
}
```

**Desugars to:**
```baseline
match get_user!(id)
  Some(user) -> {
    Ok(fetch_data!(user))
  }
  _ -> Err(NotFound)
```

* **Pros:** Extremely elegant for destructuring "happy paths". Solves the `Option`/`Result` extraction nesting problem entirely.
* **Cons:** Requires new parser rules and desugaring logic in the compiler.

## 4. Recommendation

1. **Immediate Action:** Adopt **Option 1 (`ensure`)** immediately in the core prelude as an idiom. It requires no compiler changes and fully solves the boolean guard clause problem.
2. **Long-Term Consideration:** Explore **Option 3 (`let ... else`)** for v0.3, as the need to destructure `Option`/`Result` types without indenting the entire function is a genuine ergonomic hurdle in ML-family languages. The concept of "Block Desugaring Bindings" provides the visual linearity of imperative code while preserving functional purity.
