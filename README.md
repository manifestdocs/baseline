# Baseline

A fast, safe functional language that is easy to learn but hard to break, whether you're writing the code yourself or with an AI agent.

- **Simple to learn:** Small, consistent syntax that reads the same everywhere.
- **Fast and lightweight:** Compiled to native code with low memory usage.
- **Safe by default:** The compiler checks types, effects, and error handling before your code runs.

```baseline
fn fetch_user!(id: Int) -> {Http, Console} Result<String, String> =
  Console.print!("Fetching user ${id}")
  let response = Http.get!("/users/${id}")?
  match response.status
    200 -> Ok(response.body)
    404 -> Err("User not found")
    code -> Err("HTTP ${code}")
```

You can read this function's entire contract from the first line:

- The `!` suffix means it has side effects, and `{Http, Console}` declares exactly which ones. This function can talk to the network and print output, nothing else.
- It returns `Result<String, String>` instead of a bare `String`, so the caller knows it can fail and must handle both cases.
- The `?` after `Http.get!` passes errors up to the caller automatically.

Remove `{Http}` from the declaration and the compiler rejects the program.

## Install

```bash
brew install baseline-lang/tap/baseline
```

Or [download a binary](https://github.com/baseline-lang/baseline/releases), or build from source:

```bash
git clone https://github.com/baseline-lang/baseline.git
cd baseline && cargo install --path blc --features jit
```

## Hello World

```baseline
@prelude(script)

fn main!() -> {Console} () =
  Console.print!("Hello, World!")
```

```bash
$ blc run hello.bl
Hello, World!
```

## Types as Specs

Refinement types let you state a constraint once. The compiler proves it at every call site.

```baseline
type Port = Int where self >= 1 && self <= 65535
type Percentage = Int where self >= 0 && self <= 100

fn listen(port: Port) -> Unit =
  // port is guaranteed 1..65535. No validation needed.
  ...
```

No `null`, no exceptions, no `undefined`. Model your domain with algebraic types and the compiler holds you to it.

```baseline
type Connection =
  | Disconnected
  | Connected(Socket)
  | Error(String)

fn status(conn: Connection) -> String =
  match conn
    Disconnected      -> "offline"
    Connected(_)     -> "online"
    Error(msg)       -> "error: ${msg}"
```

## Effects as Permissions

Side effects go in the type signature. If a function doesn't declare `{Fs}`, it can't touch the filesystem. No `{Http}` means no network requests. The compiler enforces this, not a linter.

```baseline
// Pure: no effects, no surprises
fn add(a: Int, b: Int) -> Int = a + b

// Effectful: declares exactly what it does
fn save!(data: String) -> {Fs} Unit =
  Fs.write!("out.txt", data)
```

Built-in effects:

| Effect | Purpose | Effect | Purpose |
|--------|---------|--------|---------|
| `Console` | Terminal I/O | `Http` | Network requests |
| `Fs` | Filesystem | `Random` | Random numbers |
| `Env` | Environment vars | `Sqlite` | Database |
| `Log` | Logging (ambient) | `Time` | Clock (ambient) |

## One Way to Do Each Thing

There's one syntax per operation. Not a style guide, the grammar itself.

| Concept | Baseline | Not supported |
|---------|----------|---------------|
| Chaining | `x \|> f \|> g` | method chaining, composition |
| Errors | `Result<T, E>` + `?` | try/catch, exceptions |
| Optionals | `Option<T>` | null, undefined, nil |
| Calls | `Module.fn(value)` | value.method() |
| Negation | `not x` | `!x` |

## Machine-Readable Diagnostics

The compiler outputs structured JSON so AI agents don't need to scrape error messages.

```json
$ blc check app.bl --json
{
  "status": "failure",
  "diagnostics": [{
    "code": "TYP_002",
    "message": "Undefined variable `nme`",
    "suggestions": [{
      "description": "Did you mean `name`?",
      "confidence": 0.8
    }]
  }]
}
```

## Toolchain

| Command | Description |
|---------|-------------|
| `blc check <file>` | Static analysis (types, effects, refinements). `--json` for structured output. |
| `blc run <file>` | Execute via Cranelift JIT. |
| `blc test <file>` | Run inline tests. |
| `blc fmt <file>` | Format source code. |
| `blc build <file>` | Compile to standalone native binary (AOT). |
| `blc lsp` | Start the language server. |

## Status

Baseline is in **v0.1 (Bootstrap Phase)**. The core language, type system, effect system, and developer tooling are functional. See the [language specification](design/baseline-language-specification.md) for the full reference and the [getting started guide](docs/getting-started.md) for a walkthrough.

## Why Baseline

Most languages make you choose: lightweight types that don't enforce much (TypeScript's `type Port = number` compiles with `listen(-1)`), or powerful proofs that require a PhD (Idris, Agda). Baseline sits in between.

**Correctness without ceremony.** Refinement types encode constraints in the type system. The compiler proves them at every call site. No Zod, no branded types, no runtime assertions.

**Refinements + effects together.** Refinements check *data invariants* (is this port in range?). Effects check *behavior invariants* (does this function touch the network?). Together they answer: is this function honest about what it accepts AND what it does? No other practical language combines both.

**Machine-actionable diagnostics.** `blc check --json` produces structured errors with patch suggestions across three verification layers (types, effects, refinements). LLMs can parse the output and fix errors in one shot instead of three.

Baseline is early (v0.1). Refinements are integer intervals only, there's no concurrency yet, and the ecosystem is 38 stdlib modules. See [design/why-baseline.md](design/why-baseline.md) for the full rationale and honest comparison with TypeScript, Rust, OCaml, and Koka.

## License

MIT
