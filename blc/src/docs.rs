//! Documentation generator for the Baseline standard library.
//!
//! Walks the builtin registry, native function list, and type inference schemas
//! to emit structured JSON or human-readable markdown describing all available
//! modules and their functions.
//!
//! This is the **single source of truth** for API documentation. Both
//! `blc docs` (CLI) and the website consume the same data model.

use std::collections::{BTreeMap, HashMap};

use serde::Serialize;

use crate::analysis::infer::{builtin_generic_schemas, InferCtx};
use crate::analysis::types::Type;
use crate::prelude::Prelude;

// ---------------------------------------------------------------------------
// Output types
// ---------------------------------------------------------------------------

#[derive(Serialize, Clone)]
pub struct DocsOutput {
    pub modules: Vec<ModuleDoc>,
}

#[derive(Serialize, Clone)]
pub struct ModuleDoc {
    pub name: String,
    pub description: String,
    pub category: String,
    pub functions: Vec<FunctionDoc>,
}

#[derive(Serialize, Clone)]
pub struct FunctionDoc {
    pub name: String,
    pub signature: String,
    pub effects: Vec<String>,
    pub prelude_level: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub example: Option<String>,
}

// ---------------------------------------------------------------------------
// Module descriptions
// ---------------------------------------------------------------------------

fn module_descriptions() -> HashMap<&'static str, &'static str> {
    let mut m = HashMap::new();
    m.insert("Console", "Print output to the terminal, read user input, and write errors to stderr.");
    m.insert("Crypto", "Hash data, compute HMACs, and perform constant-time comparisons.");
    m.insert("DateTime", "Parse, format, and manipulate dates and times.");
    m.insert("Db", "Connect to SQLite and run queries with parameter binding.");
    m.insert("Env", "Read configuration from environment variables and set them for child processes.");
    m.insert("Fs", "Read files, write data to disk, and list directory contents.");
    m.insert("Http", "Make HTTP requests to external APIs and fetch remote data.");
    m.insert("HttpError", "Create structured HTTP error values for use with error middleware.");
    m.insert("Int", "Parse integers from strings and format numbers as text.");
    m.insert("Json", "Decode JSON from APIs and encode your data structures back to JSON.");
    m.insert("List", "Transform collections with map and filter, reduce values with fold, and search for elements.");
    m.insert("Log", "Record structured events at different severity levels for debugging and monitoring.");
    m.insert("Map", "Store and retrieve values by key in immutable dictionaries.");
    m.insert("Math", "Perform common calculations like absolute value, min/max, and exponentiation.");
    m.insert("Middleware", "Extract authentication credentials, configure CORS, and set up rate limiting.");
    m.insert("Option", "Represent values that might be missing without using null.");
    m.insert("Random", "Generate random numbers, booleans, and unique identifiers.");
    m.insert("Request", "Extract HTTP method, headers, body, and attached state from incoming requests.");
    m.insert("Response", "Build HTTP responses with custom status codes, headers, and JSON or text bodies.");
    m.insert("Result", "Handle operations that can fail with explicit error values instead of exceptions.");
    m.insert("Router", "Define URL patterns, register handlers for HTTP methods, and organize routes into groups.");
    m.insert("Server", "Start an HTTP server listening on a specific port.");
    m.insert("Set", "Store unique elements and perform operations like union, intersection, and membership testing.");
    m.insert("String", "Split, trim, search, replace, and convert between strings and individual characters.");
    m.insert("Time", "Get the current timestamp and pause execution for a specified duration.");
    m.insert("Weak", "Break reference cycles with weak pointers that don't prevent garbage collection.");
    m.insert("Sqlite", "Connect to SQLite databases, execute statements, and run typed queries.");
    m.insert("Row", "Extract typed values from database query result rows without string parsing.");
    m
}

/// Map each module to its package category.
fn module_categories() -> HashMap<&'static str, &'static str> {
    let mut m = HashMap::new();
    // Web framework
    m.insert("Router", "web");
    m.insert("Server", "web");
    m.insert("Request", "web");
    m.insert("Response", "web");
    m.insert("Middleware", "web");
    m.insert("HttpError", "web");
    // Database
    m.insert("Db", "database");
    m.insert("Sqlite", "database");
    m.insert("Postgres", "database");
    m.insert("Mysql", "database");
    m.insert("Sql", "database");
    m.insert("Row", "database");
    // Everything else is "language"
    m
}

// ---------------------------------------------------------------------------
// Prelude level detection
// ---------------------------------------------------------------------------

/// All prelude levels in order from most restrictive to least.
const PRELUDE_LEVELS: &[(Prelude, &str)] = &[
    (Prelude::Minimal, "minimal"),
    (Prelude::Pure, "pure"),
    (Prelude::Core, "core"),
    (Prelude::Script, "script"),
    (Prelude::Server, "server"),
];

/// Determine the earliest prelude level that includes a given module.
fn earliest_prelude_for_module(module: &str) -> &'static str {
    for (prelude, label) in PRELUDE_LEVELS {
        let in_native = prelude.native_modules().contains(&module);
        let in_builtin = prelude.builtin_modules().contains(&module);
        if in_native || in_builtin {
            return label;
        }
    }
    "server"
}

// ---------------------------------------------------------------------------
// Effect inference for functions
// ---------------------------------------------------------------------------

/// Determine the effects required by a function based on its name.
fn effects_for_function(qualified_name: &str) -> Vec<String> {
    if !qualified_name.ends_with('!') {
        return vec![];
    }

    let module = qualified_name
        .split('.')
        .next()
        .unwrap_or(qualified_name);

    let effect = match module {
        "Server" => "Http",
        other => other,
    };

    vec![effect.to_string()]
}

// ---------------------------------------------------------------------------
// Known function registry (single source of truth)
// ---------------------------------------------------------------------------

/// All known stdlib functions with descriptions and optional usage examples.
///
/// This is the single source of truth derived from:
/// - NativeRegistry::register_all() in vm/natives/mod.rs
/// - builtin_type_signatures() in analysis/types/builtins.rs
/// - builtin_generic_schemas() in analysis/infer.rs
///
/// Entries: (module, function_name, description, example).
fn known_functions() -> Vec<(&'static str, &'static str, &'static str, Option<&'static str>)> {
    vec![
        // -- Console --
        ("Console", "println!", "Print a string to stdout followed by a newline.",
         Some("Console.println!(\"Hello, world!\")")),
        ("Console", "print!", "Print a string to stdout without a trailing newline.",
         Some("Console.print!(\"Enter name: \")")),
        ("Console", "error!", "Print a string to stderr.",
         Some("Console.error!(\"Something went wrong\")")),
        ("Console", "read_line!", "Read a line of input from stdin.",
         Some("let name = Console.read_line!()")),
        // -- Log --
        ("Log", "info!", "Log a message at info level.",
         Some("Log.info!(\"Server started on port 3000\")")),
        ("Log", "warn!", "Log a message at warning level.",
         Some("Log.warn!(\"Cache miss for key: ${key}\")")),
        ("Log", "error!", "Log a message at error level.",
         Some("Log.error!(\"Failed to connect: ${msg}\")")),
        ("Log", "debug!", "Log a message at debug level.",
         Some("Log.debug!(\"Request payload: ${body}\")")),
        // -- Math --
        ("Math", "abs", "Return the absolute value of an integer.",
         Some("Math.abs(-5)  // => 5")),
        ("Math", "min", "Return the smaller of two integers.",
         Some("Math.min(3, 7)  // => 3")),
        ("Math", "max", "Return the larger of two integers.",
         Some("Math.max(3, 7)  // => 7")),
        ("Math", "clamp", "Constrain a value between a minimum and maximum.",
         Some("Math.clamp(15, 0, 10)  // => 10")),
        ("Math", "pow", "Raise a base to an exponent.",
         Some("Math.pow(2, 10)  // => 1024")),
        // -- String --
        ("String", "length", "Return the number of characters in a string.",
         Some("String.length(\"hello\")  // => 5")),
        ("String", "to_upper", "Convert all characters to uppercase.",
         Some("String.to_upper(\"hello\")  // => \"HELLO\"")),
        ("String", "to_lower", "Convert all characters to lowercase.",
         Some("String.to_lower(\"HELLO\")  // => \"hello\"")),
        ("String", "trim", "Remove leading and trailing whitespace.",
         Some("String.trim(\"  hi  \")  // => \"hi\"")),
        ("String", "contains", "Check if a string contains a substring.",
         Some("String.contains(\"hello world\", \"world\")  // => true")),
        ("String", "starts_with", "Check if a string starts with a prefix.",
         Some("String.starts_with(\"hello\", \"hel\")  // => true")),
        ("String", "ends_with", "Check if a string ends with a suffix.",
         Some("String.ends_with(\"hello\", \"llo\")  // => true")),
        ("String", "split", "Split a string by a delimiter into a list of strings.",
         Some("\"a,b,c\" |> String.split(\",\")  // => [\"a\", \"b\", \"c\"]")),
        ("String", "join", "Join a list of strings with a separator.",
         Some("[\"a\", \"b\", \"c\"] |> String.join(\", \")  // => \"a, b, c\"")),
        ("String", "slice", "Extract a substring by start and end index.",
         Some("String.slice(\"hello\", 1, 4)  // => \"ell\"")),
        ("String", "chars", "Split a string into a list of single-character strings.",
         Some("String.chars(\"hi\")  // => [\"h\", \"i\"]")),
        ("String", "char_at", "Return the character at a given index.",
         Some("String.char_at(\"hello\", 0)  // => \"h\"")),
        ("String", "index_of", "Return the index of the first occurrence of a substring, or -1.",
         Some("String.index_of(\"hello\", \"ll\")  // => 2")),
        ("String", "to_int", "Parse a string as an integer.",
         Some("String.to_int(\"42\")  // => 42")),
        ("String", "from_char_code", "Create a single-character string from a Unicode code point.",
         Some("String.from_char_code(65)  // => \"A\"")),
        ("String", "char_code", "Return the Unicode code point of the first character.",
         Some("String.char_code(\"A\")  // => 65")),
        ("String", "replace", "Replace all occurrences of a pattern with a replacement.",
         Some("String.replace(\"hello world\", \"world\", \"there\")  // => \"hello there\"")),
        // -- List --
        ("List", "length", "Return the number of elements in a list.",
         Some("List.length([1, 2, 3])  // => 3")),
        ("List", "head", "Return the first element, or None if the list is empty.",
         Some("List.head([1, 2, 3])  // => Some(1)")),
        ("List", "tail", "Return all elements except the first.",
         Some("List.tail([1, 2, 3])  // => [2, 3]")),
        ("List", "reverse", "Return the list in reverse order.",
         Some("List.reverse([1, 2, 3])  // => [3, 2, 1]")),
        ("List", "sort", "Return the list sorted in ascending order.",
         Some("List.sort([3, 1, 2])  // => [1, 2, 3]")),
        ("List", "concat", "Concatenate two lists.",
         Some("List.concat([1, 2], [3, 4])  // => [1, 2, 3, 4]")),
        ("List", "contains", "Check if a list contains a given element.",
         Some("List.contains([1, 2, 3], 2)  // => true")),
        ("List", "get", "Return the element at an index, or None if out of bounds.",
         Some("List.get([10, 20, 30], 1)  // => Some(20)")),
        ("List", "map", "Apply a function to every element, returning a new list.",
         Some("[1, 2, 3] |> List.map(|x| x * 2)\n// => [2, 4, 6]")),
        ("List", "filter", "Return elements that satisfy a predicate.",
         Some("[1, 2, 3, 4] |> List.filter(|x| x > 2)\n// => [3, 4]")),
        ("List", "fold", "Reduce a list to a single value using an accumulator function.",
         Some("[1, 2, 3] |> List.fold(0, |acc, x| acc + x)\n// => 6")),
        ("List", "find", "Return the first element that satisfies a predicate, or None.",
         Some("[1, 2, 3] |> List.find(|x| x > 1)\n// => Some(2)")),
        // -- Option --
        ("Option", "unwrap", "Extract the value from Some, or panic on None.",
         Some("Some(42) |> Option.unwrap  // => 42")),
        ("Option", "unwrap_or", "Extract the value from Some, or return a default.",
         Some("None |> Option.unwrap_or(0)  // => 0")),
        ("Option", "is_some", "Return true if the option contains a value.",
         Some("Option.is_some(Some(1))  // => true")),
        ("Option", "is_none", "Return true if the option is None.",
         Some("Option.is_none(None)  // => true")),
        ("Option", "map", "Apply a function to the contained value, if present.",
         Some("Some(5) |> Option.map(|x| x * 2)\n// => Some(10)")),
        ("Option", "flat_map", "Apply a function that returns an Option, flattening the result.",
         Some("Some(5) |> Option.flat_map(|x|\n  if x > 0 then Some(x) else None\n)\n// => Some(5)")),
        // -- Result --
        ("Result", "unwrap", "Extract the Ok value, or panic on Err.",
         Some("Ok(42) |> Result.unwrap  // => 42")),
        ("Result", "unwrap_or", "Extract the Ok value, or return a default.",
         Some("Err(\"fail\") |> Result.unwrap_or(0)  // => 0")),
        ("Result", "is_ok", "Return true if the result is Ok.",
         Some("Result.is_ok(Ok(1))  // => true")),
        ("Result", "is_err", "Return true if the result is Err.",
         Some("Result.is_err(Err(\"fail\"))  // => true")),
        ("Result", "map", "Apply a function to the Ok value, if present.",
         Some("Ok(5) |> Result.map(|x| x * 2)\n// => Ok(10)")),
        ("Result", "and_then", "Chain a function that returns a Result, flattening the result.",
         Some("Ok(5) |> Result.and_then(|x|\n  if x > 0 then Ok(x) else Err(\"negative\")\n)\n// => Ok(5)")),
        ("Result", "map_err", "Apply a function to the Err value, if present.",
         Some("Err(\"fail\") |> Result.map_err(|e| \"wrapped: \" ++ e)\n// => Err(\"wrapped: fail\")")),
        ("Result", "context", "Add context to an error, wrapping it in a record.",
         Some("Err(\"db fail\") |> Result.context(\"loading user\")\n// => Err({ error: \"db fail\", context: \"loading user\" })")),
        // -- Int --
        ("Int", "to_string", "Convert an integer to its string representation.",
         Some("Int.to_string(42)  // => \"42\"")),
        ("Int", "parse", "Parse a string as an integer.",
         Some("Int.parse(\"42\")  // => 42")),
        // -- Json --
        ("Json", "parse", "Parse a JSON string into a value.",
         Some("let data = Json.parse(\"{\\\"name\\\": \\\"Alice\\\"}\")")),
        ("Json", "to_string", "Serialize a value to a compact JSON string.",
         Some("Json.to_string({ name: \"Alice\", age: 30 })\n// => \"{\\\"name\\\":\\\"Alice\\\",\\\"age\\\":30}\"")),
        ("Json", "to_string_pretty", "Serialize a value to a pretty-printed JSON string.",
         Some("Json.to_string_pretty({ name: \"Alice\" })")),
        // -- Map --
        ("Map", "empty", "Create an empty map.", None),
        ("Map", "insert", "Insert a key-value pair, returning a new map.",
         Some("Map.empty()\n|> Map.insert(\"name\", \"Alice\")\n|> Map.insert(\"age\", \"30\")")),
        ("Map", "get", "Look up a key, returning Some(value) or None.",
         Some("map |> Map.get(\"name\")  // => Some(\"Alice\")")),
        ("Map", "remove", "Remove a key, returning a new map.",
         Some("map |> Map.remove(\"age\")")),
        ("Map", "contains", "Check if a key exists in the map.",
         Some("Map.contains(map, \"name\")  // => true")),
        ("Map", "keys", "Return all keys as a list.",
         Some("Map.keys(map)  // => [\"age\", \"name\"]")),
        ("Map", "values", "Return all values as a list.",
         Some("Map.values(map)  // => [\"30\", \"Alice\"]")),
        ("Map", "len", "Return the number of entries in the map.",
         Some("Map.len(map)  // => 2")),
        ("Map", "from_list", "Create a map from a list of (key, value) pairs.",
         Some("Map.from_list([(\"a\", 1), (\"b\", 2)])")),
        // -- Set --
        ("Set", "empty", "Create an empty set.", None),
        ("Set", "insert", "Add an element, returning a new set.",
         Some("Set.empty() |> Set.insert(1) |> Set.insert(2)")),
        ("Set", "remove", "Remove an element, returning a new set.", None),
        ("Set", "contains", "Check if an element exists in the set.",
         Some("Set.contains(s, 1)  // => true")),
        ("Set", "union", "Return the union of two sets.", None),
        ("Set", "intersection", "Return the intersection of two sets.", None),
        ("Set", "len", "Return the number of elements in the set.", None),
        ("Set", "from_list", "Create a set from a list of elements.",
         Some("Set.from_list([1, 2, 3, 2])  // duplicates removed")),
        // -- Weak --
        ("Weak", "downgrade", "Create a weak reference from a strong reference.", None),
        ("Weak", "upgrade", "Attempt to upgrade a weak reference to a strong one, returning an Option.", None),
        // -- Time --
        ("Time", "now!", "Return the current Unix timestamp in milliseconds.",
         Some("let start = Time.now!()")),
        ("Time", "sleep!", "Pause execution for the given number of milliseconds.",
         Some("Time.sleep!(1000)  // wait 1 second")),
        // -- DateTime --
        ("DateTime", "now!", "Return the current date and time as a DateTime record.",
         Some("let now = DateTime.now!()")),
        ("DateTime", "parse", "Parse a date string into a DateTime record.",
         Some("DateTime.parse(\"2025-01-15T10:30:00Z\")")),
        ("DateTime", "to_string", "Format a DateTime as an ISO 8601 string.",
         Some("DateTime.to_string(dt)")),
        ("DateTime", "add", "Add a duration to a DateTime.",
         Some("DateTime.add(dt, \"hours\", 2)")),
        ("DateTime", "diff", "Calculate the difference between two DateTimes.",
         Some("DateTime.diff(end, start, \"seconds\")")),
        // -- Random --
        ("Random", "int!", "Generate a random integer between min and max (inclusive).",
         Some("Random.int!(1, 100)")),
        ("Random", "bool!", "Generate a random boolean.",
         Some("if Random.bool!() then \"heads\" else \"tails\"")),
        ("Random", "uuid!", "Generate a random UUID v4 string.",
         Some("let id = Random.uuid!()")),
        // -- Crypto --
        ("Crypto", "sha256", "Compute the SHA-256 hash of a string.",
         Some("Crypto.sha256(\"hello\")")),
        ("Crypto", "hmac_sha256", "Compute an HMAC-SHA256 signature.",
         Some("Crypto.hmac_sha256(\"secret\", \"message\")")),
        ("Crypto", "constant_time_eq", "Compare two strings in constant time to prevent timing attacks.",
         Some("Crypto.constant_time_eq(provided, expected)")),
        // -- Env --
        ("Env", "get!", "Read an environment variable, returning None if unset.",
         Some("let port = Env.get!(\"PORT\") |> Option.unwrap_or(\"3000\")")),
        ("Env", "set!", "Set an environment variable.",
         Some("Env.set!(\"NODE_ENV\", \"production\")")),
        // -- Fs --
        ("Fs", "read!", "Read a file's contents as a string.",
         Some("let config = Fs.read!(\"config.json\")")),
        ("Fs", "write!", "Write a string to a file, creating or overwriting it.",
         Some("Fs.write!(\"output.txt\", \"Hello!\")")),
        ("Fs", "exists!", "Check if a file path exists.",
         Some("if Fs.exists!(\"config.json\") then\n  Fs.read!(\"config.json\")\nelse\n  \"{}\"")),
        ("Fs", "read_file!", "Read a file's contents as a string.", None),
        ("Fs", "write_file!", "Write a string to a file, creating or overwriting it.", None),
        ("Fs", "list_dir!", "List the entries in a directory.",
         Some("Fs.list_dir!(\"./src\")")),
        ("Fs", "with_file!", "Open a file, pass it to a callback, and close it automatically.", None),
        // -- Http --
        ("Http", "get!", "Send an HTTP GET request to a URL.",
         Some("let resp = Http.get!(\"https://api.example.com/users\")?")),
        ("Http", "post!", "Send an HTTP POST request with a body.",
         Some("Http.post!(\"https://api.example.com/users\", body)?")),
        ("Http", "put!", "Send an HTTP PUT request with a body.", None),
        ("Http", "delete!", "Send an HTTP DELETE request to a URL.", None),
        ("Http", "request!", "Send a custom HTTP request from a request record.", None),
        // -- Response --
        ("Response", "ok", "Create a 200 OK response with a text body.",
         Some("Response.ok(\"Hello!\")")),
        ("Response", "json", "Create a 200 OK response with a JSON body.",
         Some("Response.json({ users: users, count: List.length(users) })")),
        ("Response", "created", "Create a 201 Created response with a text body.",
         Some("Response.created(user)")),
        ("Response", "no_content", "Create a 204 No Content response.",
         Some("Response.no_content()")),
        ("Response", "bad_request", "Create a 400 Bad Request response.",
         Some("Response.bad_request(\"Missing required field: name\")")),
        ("Response", "not_found", "Create a 404 Not Found response.",
         Some("Response.not_found(\"User not found\")")),
        ("Response", "error", "Create a 500 Internal Server Error response.",
         Some("Response.error(\"Internal error\")")),
        ("Response", "status", "Create a response with a custom status code and body.",
         Some("Response.status(429, \"Rate limit exceeded\")")),
        ("Response", "with_header", "Add a single header to a response.",
         Some("Response.ok(\"hi\")\n|> Response.with_header(\"X-Request-Id\", id)")),
        ("Response", "with_headers", "Add multiple headers to a response.", None),
        ("Response", "redirect", "Create a 302 temporary redirect to a URL.",
         Some("Response.redirect(\"/login\")")),
        ("Response", "redirect_permanent", "Create a 301 permanent redirect to a URL.", None),
        // -- Request --
        ("Request", "header", "Read a header value from the request.",
         Some("let token = Request.header(req, \"Authorization\")")),
        ("Request", "method", "Return the HTTP method of the request.",
         Some("let method = Request.method(req)")),
        ("Request", "body_json", "Parse the request body as JSON.",
         Some("let data = Request.body_json(req)")),
        // -- Router --
        ("Router", "new", "Create a new empty router.",
         Some("let app = Router.new()")),
        ("Router", "routes", "Return the configured route table.", None),
        ("Router", "get", "Register a handler for GET requests at a path.",
         Some("Router.new()\n|> Router.get(\"/hello\", |req| Response.ok(\"Hello!\"))")),
        ("Router", "post", "Register a handler for POST requests at a path.",
         Some("router |> Router.post(\"/users\", create_user)")),
        ("Router", "put", "Register a handler for PUT requests at a path.", None),
        ("Router", "delete", "Register a handler for DELETE requests at a path.", None),
        ("Router", "patch", "Register a handler for PATCH requests at a path.", None),
        ("Router", "options", "Register a handler for OPTIONS requests at a path.", None),
        ("Router", "head", "Register a handler for HEAD requests at a path.", None),
        ("Router", "any", "Register a handler for all HTTP methods at a path.", None),
        ("Router", "use", "Add middleware to the router.",
         Some("router |> Router.use(auth_middleware)")),
        ("Router", "group", "Group routes under a shared path prefix.",
         Some("router |> Router.group(\"/api\", |r|\n  r |> Router.get(\"/users\", list_users)\n    |> Router.post(\"/users\", create_user)\n)")),
        // -- Middleware --
        ("Middleware", "extract_bearer", "Extract a Bearer token from the Authorization header.",
         Some("let token = Middleware.extract_bearer(req)")),
        ("Middleware", "extract_basic", "Extract Basic auth credentials from the Authorization header.",
         Some("let creds = Middleware.extract_basic(req)")),
        ("Middleware", "cors_config", "Create a CORS configuration record.", None),
        ("Middleware", "rate_limit_config", "Create a rate limiting configuration record.", None),
        // -- HttpError --
        ("HttpError", "bad_request", "Create a 400 Bad Request error.",
         Some("HttpError.bad_request(\"Invalid email\")")),
        ("HttpError", "not_found", "Create a 404 Not Found error.",
         Some("HttpError.not_found(\"User not found\")")),
        ("HttpError", "unauthorized", "Create a 401 Unauthorized error.", None),
        ("HttpError", "forbidden", "Create a 403 Forbidden error.", None),
        ("HttpError", "conflict", "Create a 409 Conflict error.", None),
        ("HttpError", "unprocessable", "Create a 422 Unprocessable Entity error.", None),
        ("HttpError", "internal", "Create a 500 Internal Server Error.", None),
        ("HttpError", "method_not_allowed", "Create a 405 Method Not Allowed error.", None),
        ("HttpError", "too_many_requests", "Create a 429 Too Many Requests error.", None),
        ("HttpError", "bad_gateway", "Create a 502 Bad Gateway error.", None),
        ("HttpError", "service_unavailable", "Create a 503 Service Unavailable error.", None),
        ("HttpError", "gateway_timeout", "Create a 504 Gateway Timeout error.", None),
        // -- Db --
        ("Db", "connect!", "Open a connection to a SQLite database.",
         Some("let db = Db.connect!(\"app.db\")")),
        ("Db", "execute!", "Execute a SQL statement with parameter binding.",
         Some("Db.execute!(db, \"INSERT INTO users (name) VALUES (?)\", [name])")),
        ("Db", "query!", "Run a SQL query and return matching rows.",
         Some("let users = Db.query!(db, \"SELECT * FROM users WHERE active = ?\", [1])")),
        // -- Sqlite --
        ("Sqlite", "connect!", "Open a connection to a SQLite database file.",
         Some("Sqlite.connect!(\"app.db\")")),
        ("Sqlite", "execute!", "Execute a SQL statement with parameter binding. Returns affected row count.",
         Some("Sqlite.execute!(\"INSERT INTO users (name) VALUES (?1)\", [name])")),
        ("Sqlite", "query!", "Run a SQL query and return all matching rows as typed Row values.",
         Some("let rows = Sqlite.query!(\"SELECT * FROM users WHERE active = ?1\", [\"1\"])")),
        ("Sqlite", "query_one!", "Run a SQL query and return the first row as Option<Row>.",
         Some("let user = Sqlite.query_one!(\"SELECT * FROM users WHERE id = ?1\", [id])")),
        ("Sqlite", "query_map!", "Run a SQL query and map each row with a function, returning a typed list.",
         Some("Sqlite.query_map!(\"SELECT * FROM users\", [], |row|\n  User { name: Row.string(row, \"name\"), age: Row.int(row, \"age\") }\n)")),
        // -- Row --
        ("Row", "string", "Extract a column value as a String.",
         Some("Row.string(row, \"name\")  // => \"Alice\"")),
        ("Row", "int", "Extract a column value as an Int (no string parsing).",
         Some("Row.int(row, \"id\")  // => 42")),
        ("Row", "float", "Extract a column value as a Float.",
         Some("Row.float(row, \"price\")  // => 9.99")),
        ("Row", "bool", "Extract a column value as a Boolean (Int 1 = true, 0 = false).",
         Some("Row.bool(row, \"active\")  // => true")),
        ("Row", "optional_string", "Extract a column value as Option<String> (Null becomes None).",
         Some("Row.optional_string(row, \"email\")  // => Some(\"alice@example.com\")")),
        ("Row", "optional_int", "Extract a column value as Option<Int> (Null becomes None).",
         Some("Row.optional_int(row, \"age\")  // => Some(30)")),
        // -- Server --
        ("Server", "listen!", "Start the HTTP server on the given port.",
         Some("let app = Router.new()\n  |> Router.get(\"/\", |req| Response.ok(\"Hello!\"))\n\nServer.listen!(app, 3000)")),
    ]
}

// ---------------------------------------------------------------------------
// Signature building
// ---------------------------------------------------------------------------

/// Build the type signatures map. This mirrors builtin_type_signatures()
/// from the type checker, using Server prelude (most permissive).
fn build_type_signatures() -> HashMap<String, Type> {
    let mut sigs = HashMap::new();

    // Console
    sigs.insert("Console.println!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Console.print!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Console.error!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Console.read_line!".into(), Type::Function(vec![], Box::new(Type::String)));

    // Log
    sigs.insert("Log.info!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Log.warn!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Log.error!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Log.debug!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));

    // Time
    sigs.insert("Time.now!".into(), Type::Function(vec![], Box::new(Type::Int)));
    sigs.insert("Time.sleep!".into(), Type::Function(vec![Type::Int], Box::new(Type::Unit)));

    // DateTime
    sigs.insert("DateTime.now!".into(), Type::Function(vec![], Box::new(Type::Unknown)));
    sigs.insert("DateTime.parse".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("DateTime.to_string".into(), Type::Function(vec![Type::Unknown], Box::new(Type::String)));
    sigs.insert("DateTime.add".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Int], Box::new(Type::Unknown)));
    sigs.insert("DateTime.diff".into(), Type::Function(vec![Type::Unknown, Type::Unknown, Type::String], Box::new(Type::Int)));

    // Random
    sigs.insert("Random.int!".into(), Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)));
    sigs.insert("Random.bool!".into(), Type::Function(vec![], Box::new(Type::Bool)));
    sigs.insert("Random.uuid!".into(), Type::Function(vec![], Box::new(Type::String)));

    // Crypto
    sigs.insert("Crypto.sha256".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("Crypto.hmac_sha256".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::String)));
    sigs.insert("Crypto.constant_time_eq".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)));

    // Env
    let option_string = Type::Enum(
        "Option".to_string(),
        vec![("Some".to_string(), vec![Type::String]), ("None".to_string(), vec![])],
    );
    sigs.insert("Env.get!".into(), Type::Function(vec![Type::String], Box::new(option_string)));
    sigs.insert("Env.set!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)));

    // Fs
    sigs.insert("Fs.read!".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("Fs.write!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)));
    sigs.insert("Fs.exists!".into(), Type::Function(vec![Type::String], Box::new(Type::Bool)));
    sigs.insert("Fs.read_file!".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("Fs.write_file!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unit)));
    sigs.insert("Fs.list_dir!".into(), Type::Function(vec![Type::String], Box::new(Type::List(Box::new(Type::String)))));

    // Math
    sigs.insert("Math.abs".into(), Type::Function(vec![Type::Int], Box::new(Type::Int)));
    sigs.insert("Math.min".into(), Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)));
    sigs.insert("Math.max".into(), Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)));
    sigs.insert("Math.clamp".into(), Type::Function(vec![Type::Int, Type::Int, Type::Int], Box::new(Type::Int)));
    sigs.insert("Math.pow".into(), Type::Function(vec![Type::Int, Type::Int], Box::new(Type::Int)));

    // String
    sigs.insert("String.length".into(), Type::Function(vec![Type::String], Box::new(Type::Int)));
    sigs.insert("String.trim".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("String.contains".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)));
    sigs.insert("String.starts_with".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)));
    sigs.insert("String.ends_with".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Bool)));
    sigs.insert("String.to_upper".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("String.to_lower".into(), Type::Function(vec![Type::String], Box::new(Type::String)));
    sigs.insert("String.split".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::List(Box::new(Type::String)))));
    sigs.insert("String.join".into(), Type::Function(vec![Type::List(Box::new(Type::String)), Type::String], Box::new(Type::String)));
    sigs.insert("String.slice".into(), Type::Function(vec![Type::String, Type::Int, Type::Int], Box::new(Type::String)));
    sigs.insert("String.chars".into(), Type::Function(vec![Type::String], Box::new(Type::List(Box::new(Type::String)))));
    sigs.insert("String.char_at".into(), Type::Function(vec![Type::String, Type::Int], Box::new(Type::String)));
    sigs.insert("String.index_of".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Int)));
    sigs.insert("String.to_int".into(), Type::Function(vec![Type::String], Box::new(Type::Int)));
    sigs.insert("String.from_char_code".into(), Type::Function(vec![Type::Int], Box::new(Type::String)));
    sigs.insert("String.char_code".into(), Type::Function(vec![Type::String], Box::new(Type::Int)));
    sigs.insert("String.replace".into(), Type::Function(vec![Type::String, Type::String, Type::String], Box::new(Type::String)));

    // Int
    sigs.insert("Int.to_string".into(), Type::Function(vec![Type::Int], Box::new(Type::String)));
    sigs.insert("Int.parse".into(), Type::Function(vec![Type::String], Box::new(Type::Int)));

    // Json
    sigs.insert("Json.parse".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Json.to_string".into(), Type::Function(vec![Type::Unknown], Box::new(Type::String)));
    sigs.insert("Json.to_string_pretty".into(), Type::Function(vec![Type::Unknown], Box::new(Type::String)));

    // Option (non-generic fallbacks)
    sigs.insert("Option.is_some".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Bool)));
    sigs.insert("Option.is_none".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Bool)));

    // Result (non-generic fallbacks)
    sigs.insert("Result.is_ok".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Bool)));
    sigs.insert("Result.is_err".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Bool)));

    // List (non-generic)
    sigs.insert("List.length".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Int)));
    sigs.insert("List.contains".into(), Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Bool)));

    // Map (non-generic)
    sigs.insert("Map.len".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Int)));

    // Set (non-generic)
    sigs.insert("Set.len".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Int)));

    // Http
    sigs.insert("Http.get!".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Http.post!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Http.put!".into(), Type::Function(vec![Type::String, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Http.delete!".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Http.request!".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));

    // Response
    sigs.insert("Response.ok".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.json".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Response.created".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.no_content".into(), Type::Function(vec![], Box::new(Type::Unknown)));
    sigs.insert("Response.bad_request".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.not_found".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.error".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.status".into(), Type::Function(vec![Type::Int, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.with_header".into(), Type::Function(vec![Type::Unknown, Type::String, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.with_headers".into(), Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Response.redirect".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Response.redirect_permanent".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));

    // Request
    sigs.insert("Request.header".into(), Type::Function(vec![Type::Unknown, Type::String], Box::new(Type::Unknown)));
    sigs.insert("Request.method".into(), Type::Function(vec![Type::Unknown], Box::new(Type::String)));
    sigs.insert("Request.body_json".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Request.with_state".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Request.state".into(), Type::Function(vec![Type::Unknown, Type::String], Box::new(Type::Unknown)));

    // Router
    sigs.insert("Router.new".into(), Type::Function(vec![], Box::new(Type::Unknown)));
    sigs.insert("Router.routes".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.get".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.post".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.put".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.delete".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.patch".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.options".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.head".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.any".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.use".into(), Type::Function(vec![Type::Unknown, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Router.group".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));

    // Middleware
    sigs.insert("Middleware.extract_bearer".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Middleware.extract_basic".into(), Type::Function(vec![Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Middleware.cors_config".into(), Type::Function(vec![], Box::new(Type::Unknown)));
    sigs.insert("Middleware.rate_limit_config".into(), Type::Function(vec![], Box::new(Type::Unknown)));

    // HttpError
    sigs.insert("HttpError.bad_request".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.not_found".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.unauthorized".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.forbidden".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.conflict".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.unprocessable".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.internal".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.method_not_allowed".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.too_many_requests".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.bad_gateway".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.service_unavailable".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("HttpError.gateway_timeout".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));

    // Db
    sigs.insert("Db.connect!".into(), Type::Function(vec![Type::String], Box::new(Type::Unknown)));
    sigs.insert("Db.execute!".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));
    sigs.insert("Db.query!".into(), Type::Function(vec![Type::Unknown, Type::String, Type::Unknown], Box::new(Type::Unknown)));

    // Sqlite
    let query_row_ty = Type::List(Box::new(Type::Row));
    let option_row = Type::Enum(
        "Option".to_string(),
        vec![("Some".to_string(), vec![Type::Row]), ("None".to_string(), vec![])],
    );
    sigs.insert("Sqlite.connect!".into(), Type::Function(vec![Type::String], Box::new(Type::Unit)));
    sigs.insert("Sqlite.execute!".into(), Type::Function(vec![Type::String, Type::List(Box::new(Type::String))], Box::new(Type::Int)));
    sigs.insert("Sqlite.query!".into(), Type::Function(vec![Type::String, Type::List(Box::new(Type::String))], Box::new(query_row_ty)));
    sigs.insert("Sqlite.query_one!".into(), Type::Function(vec![Type::String, Type::List(Box::new(Type::String))], Box::new(option_row)));

    // Row accessors
    sigs.insert("Row.string".into(), Type::Function(vec![Type::Row, Type::String], Box::new(Type::String)));
    sigs.insert("Row.int".into(), Type::Function(vec![Type::Row, Type::String], Box::new(Type::Int)));
    sigs.insert("Row.float".into(), Type::Function(vec![Type::Row, Type::String], Box::new(Type::Float)));
    sigs.insert("Row.bool".into(), Type::Function(vec![Type::Row, Type::String], Box::new(Type::Bool)));

    // Server
    sigs.insert("Server.listen!".into(), Type::Function(vec![Type::Unknown, Type::Int], Box::new(Type::Unit)));

    sigs
}

// ---------------------------------------------------------------------------
// Signature formatting with generic type variables
// ---------------------------------------------------------------------------

/// Build a signature from a generic schema using readable type parameter names.
fn build_signature_from_schema(qualified_name: &str) -> Option<String> {
    let schemas = builtin_generic_schemas();
    let schema = schemas.get(qualified_name)?;

    let mut ctx = InferCtx::new();
    let fn_type = (schema.build)(&mut ctx);

    let var_names = extract_var_names(qualified_name, schema.type_params);

    let func_name = qualified_name
        .rsplit('.')
        .next()
        .unwrap_or(qualified_name);

    match &fn_type {
        Type::Function(params, ret) => {
            let params_str = params
                .iter()
                .map(|p| format_type_with_vars(p, &var_names))
                .collect::<Vec<_>>()
                .join(", ");
            let ret_str = format_type_with_vars(ret, &var_names);
            Some(format!("fn {}({}) -> {}", func_name, params_str, ret_str))
        }
        _ => Some(format!("fn {}() -> {}", func_name, fn_type)),
    }
}

/// Pick readable type parameter names based on the function context.
fn extract_var_names(qualified_name: &str, type_params: u32) -> HashMap<u32, String> {
    let mut names = HashMap::new();

    let param_names: Vec<&str> = if qualified_name.starts_with("Map.") {
        vec!["K", "V", "T", "U"]
    } else if qualified_name.starts_with("Result.")
        || qualified_name.contains("Result")
    {
        match type_params {
            2 => vec!["T", "E"],
            3 => vec!["T", "U", "E"],
            _ => vec!["T", "E", "U", "V"],
        }
    } else {
        vec!["T", "U", "V", "W"]
    };

    for i in 0..type_params {
        let name = param_names.get(i as usize).unwrap_or(&"T");
        names.insert(i, name.to_string());
    }

    names
}

/// Format a type, replacing Var(id) with readable names.
fn format_type_with_vars(ty: &Type, var_names: &HashMap<u32, String>) -> String {
    match ty {
        Type::Var(id) => var_names
            .get(id)
            .cloned()
            .unwrap_or_else(|| format!("?{}", id)),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::String => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Unknown => "<unknown>".to_string(),
        Type::Function(params, ret) => {
            let params_str = params
                .iter()
                .map(|p| format_type_with_vars(p, var_names))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "({}) -> {}",
                params_str,
                format_type_with_vars(ret, var_names)
            )
        }
        Type::List(inner) => {
            format!("List<{}>", format_type_with_vars(inner, var_names))
        }
        Type::Map(k, v) => format!(
            "Map<{}, {}>",
            format_type_with_vars(k, var_names),
            format_type_with_vars(v, var_names)
        ),
        Type::Set(inner) => {
            format!("Set<{}>", format_type_with_vars(inner, var_names))
        }
        Type::Weak(inner) => {
            format!("Weak<{}>", format_type_with_vars(inner, var_names))
        }
        Type::Scoped(inner) => {
            format!("Scoped<{}>", format_type_with_vars(inner, var_names))
        }
        Type::Enum(name, variants) => match name.as_str() {
            "Option" => {
                if let Some((_, payload)) = variants.first()
                    && let Some(inner) = payload.first()
                {
                    return format!(
                        "Option<{}>",
                        format_type_with_vars(inner, var_names)
                    );
                }
                "Option".to_string()
            }
            "Result" => {
                let ok_t = variants.first().and_then(|(_, p)| p.first());
                let err_t = variants.get(1).and_then(|(_, p)| p.first());
                match (ok_t, err_t) {
                    (Some(ok), Some(err)) => format!(
                        "Result<{}, {}>",
                        format_type_with_vars(ok, var_names),
                        format_type_with_vars(err, var_names)
                    ),
                    _ => "Result".to_string(),
                }
            }
            _ => name.clone(),
        },
        Type::Tuple(elems) => {
            let elems_str = elems
                .iter()
                .map(|e| format_type_with_vars(e, var_names))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", elems_str)
        }
        _ => ty.to_string(),
    }
}

/// Build a human-readable signature string from a Type.
fn build_signature(func_name: &str, ty: &Type) -> String {
    match ty {
        Type::Function(params, ret) => {
            let params_str = params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn {}({}) -> {}", func_name, params_str, ret)
        }
        _ => format!("fn {}() -> {}", func_name, ty),
    }
}

// ---------------------------------------------------------------------------
// Core: collect all documentation
// ---------------------------------------------------------------------------

/// Collect documentation for all standard library modules and functions.
pub fn generate_docs() -> DocsOutput {
    let type_sigs = build_type_signatures();
    let functions = known_functions();
    let mod_descs = module_descriptions();
    let mod_cats = module_categories();

    let mut modules: BTreeMap<String, Vec<FunctionDoc>> = BTreeMap::new();

    for (module, func_name, desc, example) in &functions {
        let qualified = format!("{}.{}", module, func_name);
        let base_name = func_name.trim_end_matches('!');

        // Build signature: prefer generic schema, then type signature, then fallback
        let signature = build_signature_from_schema(&qualified)
            .or_else(|| {
                type_sigs
                    .get(&qualified)
                    .map(|ty| build_signature(base_name, ty))
            })
            .unwrap_or_else(|| format!("fn {}(...)", base_name));

        let effects = effects_for_function(&qualified);
        let prelude_level = earliest_prelude_for_module(module).to_string();

        modules
            .entry(module.to_string())
            .or_default()
            .push(FunctionDoc {
                name: func_name.to_string(),
                signature,
                effects,
                prelude_level,
                description: Some(desc.to_string()),
                example: example.map(|e| e.to_string()),
            });
    }

    let modules_vec: Vec<ModuleDoc> = modules
        .into_iter()
        .map(|(name, functions)| {
            let description = mod_descs
                .get(name.as_str())
                .unwrap_or(&"")
                .to_string();
            let category = mod_cats
                .get(name.as_str())
                .unwrap_or(&"language")
                .to_string();
            ModuleDoc {
                name,
                description,
                category,
                functions,
            }
        })
        .collect();

    DocsOutput {
        modules: modules_vec,
    }
}

// ---------------------------------------------------------------------------
// Search / filter
// ---------------------------------------------------------------------------

/// Filter documentation to entries matching a query string.
///
/// Matches against module names, function names (qualified and unqualified),
/// descriptions, signatures, and examples. Case-insensitive substring match.
pub fn filter_docs(docs: &DocsOutput, query: &str) -> DocsOutput {
    let q = query.to_lowercase();

    // Check for exact qualified name match first (e.g. "List.map")
    let is_exact = query.contains('.');

    let modules: Vec<ModuleDoc> = docs
        .modules
        .iter()
        .filter_map(|module| {
            let matching_fns: Vec<FunctionDoc> = module
                .functions
                .iter()
                .filter(|func| {
                    let qualified = format!("{}.{}", module.name, func.name).to_lowercase();

                    if is_exact {
                        // Exact qualified match
                        qualified == q || qualified.contains(&q)
                    } else {
                        // Fuzzy: search across name, description, signature, example
                        qualified.contains(&q)
                            || func.name.to_lowercase().contains(&q)
                            || module.name.to_lowercase().contains(&q)
                            || func
                                .description
                                .as_ref()
                                .map_or(false, |d| d.to_lowercase().contains(&q))
                            || func.signature.to_lowercase().contains(&q)
                            || func
                                .example
                                .as_ref()
                                .map_or(false, |e| e.to_lowercase().contains(&q))
                    }
                })
                .cloned()
                .collect();

            if matching_fns.is_empty() {
                None
            } else {
                Some(ModuleDoc {
                    name: module.name.clone(),
                    description: module.description.clone(),
                    category: module.category.clone(),
                    functions: matching_fns,
                })
            }
        })
        .collect();

    DocsOutput { modules }
}

// ---------------------------------------------------------------------------
// Markdown rendering
// ---------------------------------------------------------------------------

/// Render docs as human-readable markdown.
pub fn render_markdown(docs: &DocsOutput) -> String {
    let mut out = String::new();
    out.push_str("# Baseline Standard Library Reference\n\n");

    for module in &docs.modules {
        out.push_str(&format!("## {}\n\n", module.name));

        if !module.description.is_empty() {
            out.push_str(&format!("{}\n\n", module.description));
        }

        for func in &module.functions {
            out.push_str(&format!("### `{}.{}`\n\n", module.name, func.name));
            out.push_str(&format!("```\n{}\n```\n\n", func.signature));

            if !func.effects.is_empty() {
                out.push_str(&format!(
                    "**Effects:** {}\n\n",
                    func.effects.join(", ")
                ));
            }

            out.push_str(&format!(
                "**Prelude:** `@prelude({})`\n\n",
                func.prelude_level
            ));

            if let Some(desc) = &func.description {
                out.push_str(&format!("{}\n\n", desc));
            }

            if let Some(example) = &func.example {
                out.push_str(&format!("**Example:**\n```baseline\n{}\n```\n\n", example));
            }

            out.push_str("---\n\n");
        }
    }

    out
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_docs_has_modules_and_functions() {
        let docs = generate_docs();
        assert!(!docs.modules.is_empty(), "should have modules");

        let total_fns: usize = docs.modules.iter().map(|m| m.functions.len()).sum();
        assert!(total_fns > 50, "should have many functions, got {}", total_fns);
    }

    #[test]
    fn test_module_descriptions_present() {
        let docs = generate_docs();
        for module in &docs.modules {
            assert!(
                !module.description.is_empty(),
                "module {} should have a description",
                module.name
            );
        }
    }

    #[test]
    fn test_examples_present() {
        let docs = generate_docs();
        let example_count: usize = docs
            .modules
            .iter()
            .flat_map(|m| &m.functions)
            .filter(|f| f.example.is_some())
            .count();

        assert!(
            example_count > 30,
            "should have many examples, got {}",
            example_count
        );
    }

    #[test]
    fn test_filter_exact_match() {
        let docs = generate_docs();
        let filtered = filter_docs(&docs, "List.map");
        assert_eq!(filtered.modules.len(), 1);
        assert_eq!(filtered.modules[0].name, "List");
        assert!(filtered.modules[0].functions.iter().any(|f| f.name == "map"));
    }

    #[test]
    fn test_filter_fuzzy_search() {
        let docs = generate_docs();
        let filtered = filter_docs(&docs, "filter");
        let total: usize = filtered.modules.iter().map(|m| m.functions.len()).sum();
        assert!(total > 0, "should find functions matching 'filter'");
    }

    #[test]
    fn test_filter_no_results() {
        let docs = generate_docs();
        let filtered = filter_docs(&docs, "xyznonexistent");
        assert!(filtered.modules.is_empty());
    }

    #[test]
    fn test_markdown_includes_examples() {
        let docs = generate_docs();
        let md = render_markdown(&docs);
        assert!(md.contains("**Example:**"), "markdown should include examples");
        assert!(md.contains("```baseline"), "example code should be fenced");
    }

    #[test]
    fn test_json_serialization() {
        let docs = generate_docs();
        let json = serde_json::to_string_pretty(&docs).unwrap();
        assert!(json.contains("\"example\""));
        assert!(json.contains("\"description\""));
    }
}
