use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct RosettaEntry {
    pub concept: &'static str,
    pub category: &'static str,
    pub keywords: &'static [&'static str],
    pub python: &'static str,
    pub typescript: &'static str,
    pub baseline: &'static str,
    pub explanation: &'static str,
}

static ENTRIES: &[RosettaEntry] = &[
    // ================================================================
    // Control Flow
    // ================================================================
    RosettaEntry {
        concept: "if/else",
        category: "control_flow",
        keywords: &["if", "else", "conditional", "branch", "ternary"],
        python: "if x > 0:\n    \"pos\"\nelse:\n    \"neg\"",
        typescript: "x > 0 ? \"pos\" : \"neg\"",
        baseline: "if x > 0 then \"pos\" else \"neg\"",
        explanation: "Baseline uses `if/then/else` as an expression — no braces, no colon.",
    },
    RosettaEntry {
        concept: "pattern matching",
        category: "control_flow",
        keywords: &["match", "switch", "case", "pattern", "destructure"],
        python: "match status:\n    case \"ok\": handle_ok()\n    case _: handle_err()",
        typescript: "switch (status) {\n  case \"ok\": handleOk(); break;\n  default: handleErr();\n}",
        baseline: "match status\n  \"ok\" -> handle_ok()\n  _ -> handle_err()",
        explanation: "Pattern matching is exhaustive. Use `_` as wildcard. Arms use `->` not `:`.",
    },
    RosettaEntry {
        concept: "match with destructuring",
        category: "control_flow",
        keywords: &[
            "match",
            "destructure",
            "option",
            "result",
            "unwrap",
            "some",
            "none",
        ],
        python: "if val is not None:\n    use(val)\nelse:\n    fallback",
        typescript: "val !== null ? use(val) : fallback",
        baseline: "match opt\n  Some(v) -> use(v)\n  None -> fallback",
        explanation: "Option<T> replaces null. Destructure with match to extract the value.",
    },
    RosettaEntry {
        concept: "for loop / iteration",
        category: "control_flow",
        keywords: &["for", "loop", "iterate", "each", "foreach", "map"],
        python: "for x in items:\n    process(x)",
        typescript: "items.forEach(x => process(x))",
        baseline: "List.map(items, |x| process(x))",
        explanation: "No for loops. Use List.map, List.filter, List.fold for iteration.",
    },
    RosettaEntry {
        concept: "while loop / recursion",
        category: "control_flow",
        keywords: &["while", "loop", "recursion", "recursive", "repeat"],
        python: "while n > 0:\n    n -= 1",
        typescript: "while (n > 0) { n--; }",
        baseline: "fn countdown(n: Int) -> () =\n  if n > 0 then countdown(n - 1) else ()",
        explanation: "No while loops or mutation. Use recursion instead.",
    },
    RosettaEntry {
        concept: "early return / guard",
        category: "control_flow",
        keywords: &["return", "early", "guard", "bail", "ensure"],
        python: "if not valid:\n    return Err(\"invalid\")\nprocess()",
        typescript: "if (!valid) return { error: \"invalid\" };\nprocess();",
        baseline: "Result.ensure(valid, \"invalid\")?\nprocess()",
        explanation: "Use `Result.ensure(cond, err)?` for guard clauses. Returns Err if false, continues if true.",
    },
    RosettaEntry {
        concept: "logical negation",
        category: "control_flow",
        keywords: &["not", "negation", "negate", "bang", "exclamation"],
        python: "if not valid:",
        typescript: "if (!valid)",
        baseline: "if not valid then ...",
        explanation: "`not` is a keyword. `!` means effects, never boolean negation.",
    },
    RosettaEntry {
        concept: "logical operators",
        category: "control_flow",
        keywords: &["and", "or", "not", "logical", "boolean"],
        python: "a and b or c",
        typescript: "a && b || c",
        baseline: "a && b || c",
        explanation: "Same as most languages: `&&`, `||`, `not` (keyword, not `!`).",
    },
    // ================================================================
    // Error Handling
    // ================================================================
    RosettaEntry {
        concept: "try/catch → Result",
        category: "error_handling",
        keywords: &["try", "catch", "throw", "exception", "error", "result"],
        python: "try:\n    val = risky()\nexcept:\n    val = default",
        typescript: "try {\n  val = risky();\n} catch {\n  val = defaultVal;\n}",
        baseline: "match risky()\n  Ok(v) -> v\n  Err(_) -> default",
        explanation: "No exceptions. Use Result<T, E> with pattern matching.",
    },
    RosettaEntry {
        concept: "error propagation",
        category: "error_handling",
        keywords: &["propagate", "question mark", "rethrow", "bubble", "?"],
        python: "val = risky()  # raises on error",
        typescript: "const val = risky(); // throws on error",
        baseline: "let val = risky()?",
        explanation: "`?` propagates Err. Function must return Result<T, E>.",
    },
    RosettaEntry {
        concept: "null check → Option",
        category: "error_handling",
        keywords: &[
            "null",
            "none",
            "nil",
            "undefined",
            "optional",
            "some",
            "nothing",
        ],
        python: "if x is not None:\n    use(x)",
        typescript: "if (x !== null) {\n  use(x);\n}",
        baseline: "match x\n  Some(v) -> use(v)\n  None -> fallback",
        explanation: "No null. Use Option<T> = Some(value) | None.",
    },
    RosettaEntry {
        concept: "unwrap Option/Result",
        category: "error_handling",
        keywords: &["unwrap", "force", "bang", "assert"],
        python: "val = opt  # assume not None",
        typescript: "const val = opt!;",
        baseline: "let val = Option.unwrap(opt)",
        explanation: "Use Option.unwrap() or Result.unwrap(). Panics if None/Err.",
    },
    RosettaEntry {
        concept: "create error",
        category: "error_handling",
        keywords: &["error", "err", "raise", "throw", "fail"],
        python: "raise ValueError(\"bad\")",
        typescript: "throw new Error(\"bad\")",
        baseline: "Err(\"bad\")",
        explanation: "Return Err(value) — no throwing. Caller decides how to handle it.",
    },
    RosettaEntry {
        concept: "create success",
        category: "error_handling",
        keywords: &["ok", "success", "return", "result"],
        python: "return value",
        typescript: "return value;",
        baseline: "Ok(value)",
        explanation: "Wrap in Ok(value) when function returns Result<T, E>.",
    },
    RosettaEntry {
        concept: "optional value",
        category: "error_handling",
        keywords: &["some", "optional", "maybe", "just"],
        python: "x = value  # or None",
        typescript: "const x: T | null = value;",
        baseline: "let x: Option<Int> = Some(42)",
        explanation: "Some(value) wraps a present value. None represents absence.",
    },
    RosettaEntry {
        concept: "default on error",
        category: "error_handling",
        keywords: &["default", "fallback", "or", "unwrap_or", "coalesce"],
        python: "val = risky() or default",
        typescript: "const val = risky() ?? defaultVal;",
        baseline: "let val = match risky()\n  Ok(v) -> v\n  Err(_) -> default",
        explanation: "No `??` or `or`. Use match to provide a default on Err/None.",
    },
    RosettaEntry {
        concept: "option to result",
        category: "error_handling",
        keywords: &["ok_or", "option", "result", "convert", "none", "error"],
        python: "val = opt or raise ValueError(\"missing\")",
        typescript: "const val = opt ?? throw new Error(\"missing\");",
        baseline: "let val = Option.ok_or(opt, \"missing\")?",
        explanation: "Option.ok_or converts Some(x) to Ok(x) and None to Err(e). Chain with ? to propagate.",
    },
    // ================================================================
    // Data Structures
    // ================================================================
    RosettaEntry {
        concept: "class → record",
        category: "data_structures",
        keywords: &[
            "class",
            "struct",
            "record",
            "object",
            "dataclass",
            "interface",
        ],
        python: "class User:\n    name: str\n    age: int",
        typescript: "interface User {\n  name: string;\n  age: number;\n}",
        baseline: "type User = { name: String, age: Int }",
        explanation: "No classes. Use record types for data. No methods — use module functions.",
    },
    RosettaEntry {
        concept: "create record instance",
        category: "data_structures",
        keywords: &["new", "create", "instance", "construct", "object literal"],
        python: "user = User(\"Alice\", 30)",
        typescript: "const user: User = { name: \"Alice\", age: 30 };",
        baseline: "let user = User { name: \"Alice\", age: 30 }",
        explanation: "Record literal with type name prefix and field: value pairs.",
    },
    RosettaEntry {
        concept: "field access",
        category: "data_structures",
        keywords: &["field", "property", "attribute", "dot", "access", "member"],
        python: "user.name",
        typescript: "user.name",
        baseline: "user.name",
        explanation: "Dot access works the same way — `record.field`.",
    },
    RosettaEntry {
        concept: "record update",
        category: "data_structures",
        keywords: &["update", "spread", "copy", "with", "modify", "immutable"],
        python: "user2 = User(user.name, 31)",
        typescript: "const user2 = { ...user, age: 31 };",
        baseline: "let user2 = { ..user, age: 31 }",
        explanation: "Spread syntax `..base` copies all fields, then override specific ones.",
    },
    RosettaEntry {
        concept: "enum / sum type",
        category: "data_structures",
        keywords: &[
            "enum",
            "union",
            "variant",
            "sum type",
            "tagged union",
            "discriminated",
        ],
        python: "class Shape(Enum):\n    CIRCLE = auto()\n    RECT = auto()",
        typescript: "type Shape = \n  | { kind: \"circle\"; r: number }\n  | { kind: \"rect\"; w: number; h: number };",
        baseline: "type Shape = | Circle(Float) | Rectangle(Float, Float)",
        explanation: "Sum types with `|` separated variants. Variants can carry payload.",
    },
    RosettaEntry {
        concept: "tuple",
        category: "data_structures",
        keywords: &["tuple", "pair", "triple"],
        python: "(1, \"hello\")",
        typescript: "[1, \"hello\"] as const",
        baseline: "(1, \"hello\")",
        explanation: "Tuples use parentheses. Note: `(expr)` creates a 1-element tuple, not grouping.",
    },
    RosettaEntry {
        concept: "list / array",
        category: "data_structures",
        keywords: &["list", "array", "vector", "sequence"],
        python: "[1, 2, 3]",
        typescript: "[1, 2, 3]",
        baseline: "[1, 2, 3]",
        explanation: "Lists use square brackets. Type is List<T>. Immutable.",
    },
    RosettaEntry {
        concept: "type alias",
        category: "data_structures",
        keywords: &["alias", "typedef", "newtype"],
        python: "UserId = int  # type alias",
        typescript: "type UserId = number;",
        baseline: "type UserId = Int",
        explanation: "Simple type alias with `type Name = ExistingType`.",
    },
    // ================================================================
    // IO and Effects
    // ================================================================
    RosettaEntry {
        concept: "print / console output",
        category: "io",
        keywords: &[
            "print", "println", "console", "log", "output", "stdout", "write",
        ],
        python: "print(\"hello\")",
        typescript: "console.log(\"hello\")",
        baseline: "Console.println!(\"hello\")",
        explanation: "Console.println! is effectful (note the `!`). Caller must also be effectful.",
    },
    RosettaEntry {
        concept: "read user input",
        category: "io",
        keywords: &["input", "read", "stdin", "readline", "prompt"],
        python: "name = input(\"Name: \")",
        typescript: "const name = await readline(\"Name: \")",
        baseline: "let name = Console.readline!(\"Name: \")",
        explanation: "Console.readline! reads a line from stdin. Returns String.",
    },
    RosettaEntry {
        concept: "read file",
        category: "io",
        keywords: &["file", "read", "open", "fs", "filesystem"],
        python: "text = open(\"f.txt\").read()",
        typescript: "const text = fs.readFileSync(\"f.txt\", \"utf8\")",
        baseline: "let text = Fs.read!(\"f.txt\")",
        explanation: "Fs.read! reads file contents. Requires Fs effect.",
    },
    RosettaEntry {
        concept: "write file",
        category: "io",
        keywords: &["file", "write", "save", "fs", "filesystem"],
        python: "open(\"f.txt\", \"w\").write(data)",
        typescript: "fs.writeFileSync(\"f.txt\", data)",
        baseline: "Fs.write!(\"f.txt\", data)",
        explanation: "Fs.write! writes string data to file. Requires Fs effect.",
    },
    RosettaEntry {
        concept: "HTTP GET request",
        category: "io",
        keywords: &["http", "get", "request", "fetch", "api", "url"],
        python: "resp = requests.get(url)",
        typescript: "const resp = await fetch(url)",
        baseline: "let resp = Http.get!(url)",
        explanation: "Http.get! makes an HTTP GET. Requires Http effect.",
    },
    RosettaEntry {
        concept: "log message",
        category: "io",
        keywords: &["log", "info", "debug", "warn", "error", "logging"],
        python: "logging.info(\"msg\")",
        typescript: "console.log(\"msg\")",
        baseline: "Log.info!(\"msg\")",
        explanation: "Log.info!, Log.warn!, Log.error! for structured logging. Log is ambient.",
    },
    RosettaEntry {
        concept: "random number",
        category: "io",
        keywords: &["random", "rand", "generate", "dice"],
        python: "import random\nn = random.randint(1, 6)",
        typescript: "const n = Math.floor(Math.random() * 6) + 1",
        baseline: "let n = Random.int!(1, 6)",
        explanation: "Random.int! generates a random integer. Random is NOT ambient — must be declared.",
    },
    RosettaEntry {
        concept: "get current time",
        category: "io",
        keywords: &["time", "now", "clock", "timestamp", "date"],
        python: "import time\nt = time.time()",
        typescript: "const t = Date.now()",
        baseline: "let t = Time.now!()",
        explanation: "Time.now! returns current timestamp. Time is ambient.",
    },
    RosettaEntry {
        concept: "environment variable",
        category: "io",
        keywords: &["env", "environment", "getenv", "config", "variable"],
        python: "val = os.environ.get(\"KEY\")",
        typescript: "const val = process.env.KEY",
        baseline: "let val = Env.get!(\"KEY\")",
        explanation: "Env.get! reads an environment variable. Requires Env effect.",
    },
    // ================================================================
    // Effects
    // ================================================================
    RosettaEntry {
        concept: "effectful function",
        category: "effects",
        keywords: &["effect", "side effect", "impure", "async", "io", "bang"],
        python: "def fetch(url):  # implicit side effects",
        typescript: "async function fetch(url) { ... }",
        baseline: "fn fetch!(url: String) -> String = Http.get!(url)",
        explanation: "Effectful functions end with `!`. The `!` is part of the name.",
    },
    RosettaEntry {
        concept: "pure function",
        category: "effects",
        keywords: &[
            "pure",
            "no effects",
            "deterministic",
            "referential transparency",
        ],
        python: "def add(a, b): return a + b  # pure by convention",
        typescript: "function add(a: number, b: number) { return a + b; }",
        baseline: "fn add(a: Int, b: Int) -> Int = a + b",
        explanation: "No `!` suffix = pure. Cannot call effectful functions.",
    },
    RosettaEntry {
        concept: "effect annotation",
        category: "effects",
        keywords: &["effect type", "annotation", "capability", "declare"],
        python: "# no equivalent",
        typescript: "// no equivalent",
        baseline: "fn fetch!(url: String) -> {Http, Log} String = ...",
        explanation: "Optional effect annotations restrict which effects a function may use.",
    },
    RosettaEntry {
        concept: "calling effectful from pure",
        category: "effects",
        keywords: &["effect", "pure", "call", "forbidden", "error"],
        python: "# no restriction in Python",
        typescript: "// no restriction in TypeScript",
        baseline: "// COMPILE ERROR: pure fn cannot call effectful fn!",
        explanation: "Pure functions (no `!`) cannot call effectful functions (with `!`). This is enforced by the compiler.",
    },
    RosettaEntry {
        concept: "effect naming convention",
        category: "effects",
        keywords: &["naming", "convention", "module", "capitalize"],
        python: "# no equivalent",
        typescript: "// no equivalent",
        baseline: "fn greet!() = Console.println!(\"hi\")\n// requires {Console} effect",
        explanation: "Effect name comes from the module: Console.println! requires {Console}. Local fn! creates {Fn_name}.",
    },
    RosettaEntry {
        concept: "ambient effects",
        category: "effects",
        keywords: &["ambient", "implicit", "auto", "default"],
        python: "# no equivalent",
        typescript: "// no equivalent",
        baseline: "Log.info!(\"msg\")  // Log and Time are ambient",
        explanation: "Log and Time effects don't need declaration. Random is NOT ambient.",
    },
    // ================================================================
    // Strings
    // ================================================================
    RosettaEntry {
        concept: "string interpolation",
        category: "strings",
        keywords: &["interpolation", "template", "format", "fstring", "concat"],
        python: "f\"Hello, {name}!\"",
        typescript: "`Hello, ${name}!`",
        baseline: "\"Hello, ${name}!\"",
        explanation: "Use `${expr}` inside double-quoted strings. No f-prefix needed.",
    },
    RosettaEntry {
        concept: "string concatenation",
        category: "strings",
        keywords: &["concat", "join", "plus", "append", "combine"],
        python: "\"a\" + \"b\"",
        typescript: "\"a\" + \"b\"",
        baseline: "\"${a}${b}\"",
        explanation: "No `+` for strings. Use interpolation: `\"${a}${b}\"`.",
    },
    RosettaEntry {
        concept: "string to upper",
        category: "strings",
        keywords: &["upper", "uppercase", "toupper", "capitalize"],
        python: "name.upper()",
        typescript: "name.toUpperCase()",
        baseline: "String.to_upper(name)",
        explanation: "No method calls on values. Use `Module.function(value)` style.",
    },
    RosettaEntry {
        concept: "string length",
        category: "strings",
        keywords: &["length", "len", "size", "count"],
        python: "len(s)",
        typescript: "s.length",
        baseline: "String.length(s)",
        explanation: "String.length(s) returns the character count.",
    },
    RosettaEntry {
        concept: "string contains",
        category: "strings",
        keywords: &["contains", "includes", "in", "search", "find"],
        python: "\"x\" in s",
        typescript: "s.includes(\"x\")",
        baseline: "String.contains(s, \"x\")",
        explanation: "Module-qualified function call, not method syntax.",
    },
    RosettaEntry {
        concept: "string split",
        category: "strings",
        keywords: &["split", "tokenize", "divide"],
        python: "s.split(\",\")",
        typescript: "s.split(\",\")",
        baseline: "String.split(s, \",\")",
        explanation: "String.split(str, delimiter) returns List<String>.",
    },
    RosettaEntry {
        concept: "to string conversion",
        category: "strings",
        keywords: &["tostring", "str", "convert", "cast", "format"],
        python: "str(42)",
        typescript: "String(42)",
        baseline: "String.from(42)",
        explanation: "String.from(value) converts to string. Not `value.to_string()`.",
    },
    // ================================================================
    // Collections
    // ================================================================
    RosettaEntry {
        concept: "map / transform",
        category: "collections",
        keywords: &["map", "transform", "select", "project"],
        python: "[x * 2 for x in items]",
        typescript: "items.map(x => x * 2)",
        baseline: "List.map(items, |x| x * 2)",
        explanation: "List.map(list, fn) applies fn to each element.",
    },
    RosettaEntry {
        concept: "filter",
        category: "collections",
        keywords: &["filter", "where", "select", "grep"],
        python: "[x for x in items if x > 0]",
        typescript: "items.filter(x => x > 0)",
        baseline: "List.filter(items, |x| x > 0)",
        explanation: "List.filter(list, predicate) keeps matching elements.",
    },
    RosettaEntry {
        concept: "reduce / fold",
        category: "collections",
        keywords: &["reduce", "fold", "aggregate", "accumulate", "inject"],
        python: "from functools import reduce\nreduce(lambda a, x: a + x, items, 0)",
        typescript: "items.reduce((acc, x) => acc + x, 0)",
        baseline: "List.fold(items, 0, |acc, x| acc + x)",
        explanation: "List.fold(list, initial, fn) reduces left-to-right.",
    },
    RosettaEntry {
        concept: "find element",
        category: "collections",
        keywords: &["find", "first", "search", "lookup"],
        python: "next((x for x in items if x > 5), None)",
        typescript: "items.find(x => x > 5)",
        baseline: "List.find(items, |x| x > 5)",
        explanation: "List.find returns Option<T> — Some(first match) or None.",
    },
    RosettaEntry {
        concept: "list head / first",
        category: "collections",
        keywords: &["head", "first", "front", "car"],
        python: "items[0] if items else None",
        typescript: "items[0]",
        baseline: "List.head(items)",
        explanation: "List.head returns Option<T>. No index operator.",
    },
    RosettaEntry {
        concept: "list tail / rest",
        category: "collections",
        keywords: &["tail", "rest", "cdr", "slice"],
        python: "items[1:]",
        typescript: "items.slice(1)",
        baseline: "List.tail(items)",
        explanation: "List.tail returns List<T> without the first element.",
    },
    RosettaEntry {
        concept: "list length",
        category: "collections",
        keywords: &["length", "len", "size", "count"],
        python: "len(items)",
        typescript: "items.length",
        baseline: "List.length(items)",
        explanation: "List.length(list) returns Int.",
    },
    RosettaEntry {
        concept: "pipe chain",
        category: "collections",
        keywords: &["pipe", "chain", "compose", "pipeline", "fluent"],
        python: "result = step3(step2(step1(data)))",
        typescript: "const result = step3(step2(step1(data)))",
        baseline: "let result = data |> step1 |> step2 |> step3",
        explanation: "Pipe `|>` inserts left value as first argument to right function.",
    },
    // ================================================================
    // Patterns
    // ================================================================
    RosettaEntry {
        concept: "lambda / anonymous function",
        category: "patterns",
        keywords: &["lambda", "anonymous", "closure", "arrow", "callback"],
        python: "lambda x: x + 1",
        typescript: "(x) => x + 1",
        baseline: "|x| x + 1",
        explanation: "Lambdas use `|params| body`. No `fn` keyword needed.",
    },
    RosettaEntry {
        concept: "let binding",
        category: "patterns",
        keywords: &["let", "var", "const", "assign", "bind", "declare"],
        python: "x = 42",
        typescript: "const x = 42;",
        baseline: "let x = 42",
        explanation: "All bindings are immutable. No `var`, `mut`, or reassignment.",
    },
    RosettaEntry {
        concept: "function definition",
        category: "patterns",
        keywords: &["function", "def", "fn", "define", "method"],
        python: "def add(a: int, b: int) -> int:\n    return a + b",
        typescript: "function add(a: number, b: number): number {\n  return a + b;\n}",
        baseline: "fn add(a: Int, b: Int) -> Int = a + b",
        explanation: "fn name(params) -> ReturnType = body. Last expression is the return value.",
    },
    RosettaEntry {
        concept: "multi-line function",
        category: "patterns",
        keywords: &["block", "multiline", "body", "braces"],
        python: "def process(x):\n    y = x + 1\n    return y * 2",
        typescript: "function process(x: number) {\n  const y = x + 1;\n  return y * 2;\n}",
        baseline: "fn process(x: Int) -> Int = {\n  let y = x + 1\n  y * 2\n}",
        explanation: "Use `{ }` for multi-expression bodies. Last expression is the return value.",
    },
    RosettaEntry {
        concept: "method call style",
        category: "patterns",
        keywords: &["method", "dot", "call", "invoke", "oop"],
        python: "name.upper()",
        typescript: "name.toUpperCase()",
        baseline: "String.to_upper(name)",
        explanation: "No method calls on values. Always `Module.function(value, args)`.",
    },
    RosettaEntry {
        concept: "main entry point",
        category: "patterns",
        keywords: &["main", "entry", "start", "run", "program"],
        python: "if __name__ == \"__main__\":\n    main()",
        typescript: "main();",
        baseline: "fn main!() =\n  Console.println!(\"Hello!\")",
        explanation: "main!() is the entry point. Effectful since it does I/O.",
    },
    RosettaEntry {
        concept: "inline tests",
        category: "patterns",
        keywords: &["test", "assert", "unittest", "spec", "verify"],
        python: "def test_add():\n    assert add(1, 2) == 3",
        typescript: "test(\"add\", () => expect(add(1, 2)).toBe(3))",
        baseline: "@test\ntest \"add\" = add(1, 2) == 3",
        explanation: "Tests live in the same file after @test. Run with `blc test file.bl`.",
    },
    RosettaEntry {
        concept: "sequencing statements",
        category: "patterns",
        keywords: &["semicolon", "sequence", "statement", "then", "next"],
        python: "a = 1\nb = 2\nreturn a + b",
        typescript: "const a = 1;\nconst b = 2;\nreturn a + b;",
        baseline: "{\n  let a = 1\n  let b = 2\n  a + b\n}",
        explanation: "No semicolons. Use `let _ = expr` to sequence effects. Last expr is result.",
    },
    // ================================================================
    // Types
    // ================================================================
    RosettaEntry {
        concept: "type annotation",
        category: "types",
        keywords: &["type", "annotation", "declare", "hint"],
        python: "x: int = 42",
        typescript: "const x: number = 42;",
        baseline: "let x: Int = 42",
        explanation: "Type annotations use `: Type` after the name.",
    },
    RosettaEntry {
        concept: "generic type",
        category: "types",
        keywords: &["generic", "template", "parameterized", "type parameter"],
        python: "def first(items: list[T]) -> T: ...",
        typescript: "function first<T>(items: T[]): T { ... }",
        baseline: "// Built-in generics: List<T>, Option<T>, Result<T, E>",
        explanation: "Generics exist for built-in types. User-defined generics are v0.2.",
    },
    RosettaEntry {
        concept: "refinement type",
        category: "types",
        keywords: &[
            "refinement",
            "constraint",
            "where",
            "predicate",
            "range",
            "validate",
        ],
        python: "# no equivalent (use assert)",
        typescript: "// no equivalent (use branded types)",
        baseline: "type Port = Int where self > 0 && self <= 65535",
        explanation: "Refinement types add constraints checked at compile time. Integer intervals only in v0.1.",
    },
    RosettaEntry {
        concept: "row polymorphism",
        category: "types",
        keywords: &[
            "row",
            "polymorphism",
            "open record",
            "structural",
            "duck typing",
            "spread",
        ],
        python: "# duck typing (implicit)",
        typescript: "function greet(p: { name: string }) { ... }",
        baseline: "fn greet(p: { name: String, ..r }) -> String = p.name",
        explanation: "Open records with `..r` accept extra fields. Strict structural typing.",
    },
    // ================================================================
    // Modules
    // ================================================================
    RosettaEntry {
        concept: "import module",
        category: "modules",
        keywords: &["import", "require", "include", "use", "module"],
        python: "import math",
        typescript: "import * as math from \"./math\";",
        baseline: "import Math",
        explanation: "import ModuleName — file must be PascalCase (Math.bl). Qualified: Math.sqrt(9).",
    },
    RosettaEntry {
        concept: "selective import",
        category: "modules",
        keywords: &["import", "from", "destructure", "named"],
        python: "from math import sqrt, pow",
        typescript: "import { sqrt, pow } from \"./math\";",
        baseline: "import Math { sqrt, pow }",
        explanation: "Import specific functions with `import Module { fn1, fn2 }`.",
    },
    RosettaEntry {
        concept: "wildcard import",
        category: "modules",
        keywords: &["import", "wildcard", "star", "all", "glob"],
        python: "from math import *",
        typescript: "import * from \"./math\";",
        baseline: "import Math *",
        explanation: "Import everything with `import Module *`.",
    },
    RosettaEntry {
        concept: "prelude / standard imports",
        category: "modules",
        keywords: &["prelude", "stdlib", "standard", "built-in", "auto import"],
        python: "# builtins always available",
        typescript: "// need explicit imports",
        baseline: "@prelude(script)",
        explanation: "Preludes auto-import stdlib. Levels: core, pure, script, server.",
    },
    RosettaEntry {
        concept: "HTTP server setup",
        category: "modules",
        keywords: &[
            "http", "server", "router", "listen", "handler", "endpoint", "route",
        ],
        python: "@app.get(\"/health\")\ndef health(): return \"ok\"",
        typescript: "app.get(\"/health\", (req, res) => res.send(\"ok\"));",
        baseline: "@prelude(server)\n\nfn health(req: {...}) -> Result<{...}, String> =\n  Ok(Response.ok(\"ok\"))\n\nfn main!() =\n  Router.new()\n    |> Router.get(\"/health\", health)\n    |> Server.listen!(8080)",
        explanation: "Use @prelude(server) for Router/Server. Handler returns Result<Response, Error>.",
    },
];

pub fn search(
    query: &str,
    source_language: Option<&str>,
    limit: usize,
) -> Vec<&'static RosettaEntry> {
    let query_lower = query.to_lowercase();
    let terms: Vec<&str> = query_lower.split_whitespace().collect();

    let mut scored: Vec<(&'static RosettaEntry, i32)> = ENTRIES
        .iter()
        .filter_map(|entry| {
            let mut score = 0i32;
            let concept_lower = entry.concept.to_lowercase();

            for term in &terms {
                // Exact concept match
                if concept_lower == *term {
                    score += 100;
                } else if concept_lower.contains(term) {
                    score += 50;
                }

                // Keyword match
                for kw in entry.keywords {
                    if *kw == *term {
                        score += 30;
                    } else if kw.contains(term) {
                        score += 15;
                    }
                }

                // Category match
                if entry.category.contains(term) {
                    score += 10;
                }

                // Source language snippet match
                if let Some(lang) = source_language {
                    let snippet = match lang {
                        "python" | "py" => entry.python,
                        "typescript" | "ts" | "javascript" | "js" => entry.typescript,
                        _ => "",
                    };
                    if !snippet.is_empty() && snippet.to_lowercase().contains(term) {
                        score += 30;
                    }
                }

                // Explanation match
                if entry.explanation.to_lowercase().contains(term) {
                    score += 10;
                }
            }

            if score > 0 {
                Some((entry, score))
            } else {
                None
            }
        })
        .collect();

    scored.sort_by(|a, b| b.1.cmp(&a.1));
    scored.into_iter().take(limit).map(|(e, _)| e).collect()
}
