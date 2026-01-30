Here are two idiomatic examples based on the Rocket v0.1.0 specification.

### 1. FizzBuzz

This example demonstrates the **"One Way" Principle** (Section 1.6) and the strict separation of **Pure Logic** from **Side Effects**.

* **Logic**: The `fizzbuzz` function is pure (no `!`). It uses **Tuple Matching** to avoid nested `if/else` chains.
* **Effects**: The `main!` function handles the I/O. Note that `for` loops are *only* allowed for effectful iteration (Section 1.6.1).
* **Formatting**: String interpolation (`"${n}"`) is the only formatting syntax.

```rocket
@prelude(script) // Auto-imports Console, standard types
module FizzBuzz

// 1. Pure Logic
// No side effects allowed here. Compiler verifies purity.
fizzbuzz : Int -> String
fizzbuzz = |n| match (n % 3, n % 5)
  (0, 0) -> "FizzBuzz"
  (0, _) -> "Fizz"
  (_, 0) -> "Buzz"
  _      -> "${n}"

// 2. Effectful Entry Point
// Explicitly declares the {Console} capability.
main! : {Console} ()
main! = 
  // Range is exclusive (1 to 100)
  for n in 1..101 do
    Console.print!(fizzbuzz(n))

// 3. Inline Unit Tests (Spec 9.2)
where
  test "15 is FizzBuzz" = fizzbuzz(15) == "FizzBuzz"
  test "3 is Fizz"      = fizzbuzz(3) == "Fizz"

```

---

### 2. Minimal REST API

This example leverages the **Server Prelude** (Section 6.7) and **Refinement Types** (Section 3.4).

It demonstrates the **"Parse, Don't Validate"** philosophy (Section 1.7.2). Instead of writing defensive code inside the handler (e.g., `if id < 0 return 400`), we parse the input into a `UserId` type. If parsing fails, the error propagates automatically.

```rocket
@prelude(server) // Imports Http, Router, Log, Json, Server
module Api

// 1. Types as Specifications
// The type system guarantees a UserId is always positive.
// Invalid IDs are mathematically impossible to represent in the domain logic.
type UserId = Int where self > 0

type User = {
  id: UserId,
  username: String,
  email: String,
}

// 2. Effectful Handler
// Explicitly declares it needs {Log}. It CANNOT access the filesystem ({Fs}).
get_user! : Request -> {Log} Response
get_user! = |req|
  // Boundary Parsing:
  // We attempt to parse the string param into a UserId.
  // If parsing fails (e.g. "-5"), the '?' operator propagates an Error,
  // which the framework automatically converts to 400 Bad Request.
  let id = UserId.parse(req.params.id)?
  
  Log.info!("Fetching user ${id}")
  
  // Automatic JSON serialization for Records
  Response.ok({ 
    id: id, 
    username: "Agent_007", 
    email: "agent@rocket.lang" 
  })

// 3. Server Composition
main! : {Http, Log} ()
main! =
  Log.info!("Starting API on port 8080...")

  // The Pipe Operator (|>) is standard for chaining logic
  let app = Router.new()
    |> Router.get("/health", || Response.ok({ status: "up" }))
    |> Router.get("/users/:id", get_user!)

  // Server.listen! is a blocking effect
  Server.listen!(8080, app)

```
