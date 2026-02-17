# Baseline Standard Library Reference

## Console

Print output to the terminal, read user input, and write errors to stderr.

### `Console.println!`

```
fn println(String) -> Unit
```

**Effects:** Console

**Prelude:** `@prelude(script)`

Print a string to stdout followed by a newline.

**Example:**
```baseline
Console.println!("Hello, world!")
```

---

### `Console.print!`

```
fn print(String) -> Unit
```

**Effects:** Console

**Prelude:** `@prelude(script)`

Print a string to stdout without a trailing newline.

**Example:**
```baseline
Console.print!("Enter name: ")
```

---

### `Console.error!`

```
fn error(String) -> Unit
```

**Effects:** Console

**Prelude:** `@prelude(script)`

Print a string to stderr.

**Example:**
```baseline
Console.error!("Something went wrong")
```

---

### `Console.read_line!`

```
fn read_line() -> String
```

**Effects:** Console

**Prelude:** `@prelude(script)`

Read a line of input from stdin.

**Example:**
```baseline
let name = Console.read_line!()
```

---

## Crypto

Hash data, compute HMACs, and perform constant-time comparisons.

### `Crypto.sha256`

```
fn sha256(String) -> String
```

**Prelude:** `@prelude(pure)`

Compute the SHA-256 hash of a string.

**Example:**
```baseline
Crypto.sha256("hello")
```

---

### `Crypto.hmac_sha256`

```
fn hmac_sha256(String, String) -> String
```

**Prelude:** `@prelude(pure)`

Compute an HMAC-SHA256 signature.

**Example:**
```baseline
Crypto.hmac_sha256("secret", "message")
```

---

### `Crypto.constant_time_eq`

```
fn constant_time_eq(String, String) -> Bool
```

**Prelude:** `@prelude(pure)`

Compare two strings in constant time to prevent timing attacks.

**Example:**
```baseline
Crypto.constant_time_eq(provided, expected)
```

---

## DateTime

Parse, format, and manipulate dates and times.

### `DateTime.now!`

```
fn now() -> <unknown>
```

**Effects:** DateTime

**Prelude:** `@prelude(script)`

Return the current date and time as a DateTime record.

**Example:**
```baseline
let now = DateTime.now!()
```

---

### `DateTime.parse`

```
fn parse(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Parse a date string into a DateTime record.

**Example:**
```baseline
DateTime.parse("2025-01-15T10:30:00Z")
```

---

### `DateTime.to_string`

```
fn to_string(<unknown>) -> String
```

**Prelude:** `@prelude(script)`

Format a DateTime as an ISO 8601 string.

**Example:**
```baseline
DateTime.to_string(dt)
```

---

### `DateTime.add`

```
fn add(<unknown>, String, Int) -> <unknown>
```

**Prelude:** `@prelude(script)`

Add a duration to a DateTime.

**Example:**
```baseline
DateTime.add(dt, "hours", 2)
```

---

### `DateTime.diff`

```
fn diff(<unknown>, <unknown>, String) -> Int
```

**Prelude:** `@prelude(script)`

Calculate the difference between two DateTimes.

**Example:**
```baseline
DateTime.diff(end, start, "seconds")
```

---

## Db

Connect to SQLite, run queries, and parse result rows into typed values.

### `Db.connect!`

```
fn connect(String) -> <unknown>
```

**Effects:** Db

**Prelude:** `@prelude(server)`

Open a connection to a SQLite database.

**Example:**
```baseline
let db = Db.connect!("app.db")
```

---

### `Db.execute!`

```
fn execute(<unknown>, String, <unknown>) -> <unknown>
```

**Effects:** Db

**Prelude:** `@prelude(server)`

Execute a SQL statement with parameter binding.

**Example:**
```baseline
Db.execute!(db, "INSERT INTO users (name) VALUES (?)", [name])
```

---

### `Db.query!`

```
fn query(<unknown>, String, <unknown>) -> <unknown>
```

**Effects:** Db

**Prelude:** `@prelude(server)`

Run a SQL query and return matching rows.

**Example:**
```baseline
let users = Db.query!(db, "SELECT * FROM users WHERE active = ?", [1])
```

---

### `Db.require`

```
fn require(Map<String, String>, String) -> String
```

**Prelude:** `@prelude(server)`

Get a required string field from a row. Returns "" if the key is missing.

**Example:**
```baseline
let name = Db.require(row, "name")
```

---

### `Db.optional`

```
fn optional(Map<String, String>, String) -> Option<String>
```

**Prelude:** `@prelude(server)`

Get an optional string field. Returns None if the key is missing or the value is empty.

**Example:**
```baseline
let email = Db.optional(row, "email")
```

---

### `Db.int_field`

```
fn int_field(Map<String, String>, String) -> Int
```

**Prelude:** `@prelude(server)`

Parse an integer field from a row. Returns 0 if the key is missing or unparseable.

**Example:**
```baseline
let age = Db.int_field(row, "age")
```

---

### `Db.bool_field`

```
fn bool_field(Map<String, String>, String) -> Bool
```

**Prelude:** `@prelude(server)`

Parse a boolean field from a row. Returns true only if the value is "1".

**Example:**
```baseline
let active = Db.bool_field(row, "active")
```

---

### `Db.first_row`

```
fn first_row(List<Map<String, String>>) -> Option<Map<String, String>>
```

**Prelude:** `@prelude(server)`

Return the first row from a query result, or None if empty.

**Example:**
```baseline
let user = Db.first_row(Db.query!(db, "SELECT * FROM users WHERE id = ?", [id]))
```

---

### `Db.has_rows`

```
fn has_rows(List<Map<String, String>>) -> Bool
```

**Prelude:** `@prelude(server)`

Check whether a query result contains any rows.

**Example:**
```baseline
let exists = Db.has_rows(Db.query!(db, "SELECT 1 FROM users WHERE email = ?", [email]))
```

---

## Env

Read configuration from environment variables and set them for child processes.

### `Env.get!`

```
fn get(String) -> Option<String>
```

**Effects:** Env

**Prelude:** `@prelude(script)`

Read an environment variable, returning None if unset.

**Example:**
```baseline
let port = Env.get!("PORT") |> Option.unwrap_or("3000")
```

---

### `Env.set!`

```
fn set(String, String) -> Unit
```

**Effects:** Env

**Prelude:** `@prelude(script)`

Set an environment variable.

**Example:**
```baseline
Env.set!("NODE_ENV", "production")
```

---

## Fs

Read files, write data to disk, and list directory contents.

### `Fs.read!`

```
fn read(String) -> String
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

Read a file's contents as a string.

**Example:**
```baseline
let config = Fs.read!("config.json")
```

---

### `Fs.write!`

```
fn write(String, String) -> Unit
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

Write a string to a file, creating or overwriting it.

**Example:**
```baseline
Fs.write!("output.txt", "Hello!")
```

---

### `Fs.exists!`

```
fn exists(String) -> Bool
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

Check if a file path exists.

**Example:**
```baseline
if Fs.exists!("config.json") then
  Fs.read!("config.json")
else
  "{}"
```

---

### `Fs.read_file!`

```
fn read_file(String) -> String
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

Read a file's contents as a string.

---

### `Fs.write_file!`

```
fn write_file(String, String) -> Unit
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

Write a string to a file, creating or overwriting it.

---

### `Fs.list_dir!`

```
fn list_dir(String) -> List<String>
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

List the entries in a directory.

**Example:**
```baseline
Fs.list_dir!("./src")
```

---

### `Fs.with_file!`

```
fn with_file!(String, (Scoped<String>) -> T) -> T
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

Open a file, pass it to a callback, and close it automatically.

---

## Http

Make HTTP requests to external APIs and fetch remote data.

### `Http.get!`

```
fn get(String) -> <unknown>
```

**Effects:** Http

**Prelude:** `@prelude(script)`

Send an HTTP GET request to a URL.

**Example:**
```baseline
let resp = Http.get!("https://api.example.com/users")?
```

---

### `Http.post!`

```
fn post(String, String) -> <unknown>
```

**Effects:** Http

**Prelude:** `@prelude(script)`

Send an HTTP POST request with a body.

**Example:**
```baseline
Http.post!("https://api.example.com/users", body)?
```

---

### `Http.put!`

```
fn put(String, String) -> <unknown>
```

**Effects:** Http

**Prelude:** `@prelude(script)`

Send an HTTP PUT request with a body.

---

### `Http.delete!`

```
fn delete(String) -> <unknown>
```

**Effects:** Http

**Prelude:** `@prelude(script)`

Send an HTTP DELETE request to a URL.

---

### `Http.request!`

```
fn request(<unknown>) -> <unknown>
```

**Effects:** Http

**Prelude:** `@prelude(script)`

Send a custom HTTP request from a request record.

---

## HttpError

Create structured HTTP error values for use with error middleware.

### `HttpError.bad_request`

```
fn bad_request(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 400 Bad Request error.

**Example:**
```baseline
HttpError.bad_request("Invalid email")
```

---

### `HttpError.not_found`

```
fn not_found(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 404 Not Found error.

**Example:**
```baseline
HttpError.not_found("User not found")
```

---

### `HttpError.unauthorized`

```
fn unauthorized(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 401 Unauthorized error.

---

### `HttpError.forbidden`

```
fn forbidden(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 403 Forbidden error.

---

### `HttpError.conflict`

```
fn conflict(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 409 Conflict error.

---

### `HttpError.unprocessable`

```
fn unprocessable(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 422 Unprocessable Entity error.

---

### `HttpError.internal`

```
fn internal(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 500 Internal Server Error.

---

### `HttpError.method_not_allowed`

```
fn method_not_allowed(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 405 Method Not Allowed error.

---

### `HttpError.too_many_requests`

```
fn too_many_requests(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 429 Too Many Requests error.

---

### `HttpError.bad_gateway`

```
fn bad_gateway(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 502 Bad Gateway error.

---

### `HttpError.service_unavailable`

```
fn service_unavailable(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 503 Service Unavailable error.

---

### `HttpError.gateway_timeout`

```
fn gateway_timeout(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 504 Gateway Timeout error.

---

## Int

Parse integers from strings and format numbers as text.

### `Int.to_string`

```
fn to_string(Int) -> String
```

**Prelude:** `@prelude(core)`

Convert an integer to its string representation.

**Example:**
```baseline
Int.to_string(42)  // => "42"
```

---

### `Int.parse`

```
fn parse(String) -> Int
```

**Prelude:** `@prelude(core)`

Parse a string as an integer.

**Example:**
```baseline
Int.parse("42")  // => 42
```

---

## Json

Decode JSON from APIs and encode your data structures back to JSON.

### `Json.parse`

```
fn parse(String) -> <unknown>
```

**Prelude:** `@prelude(pure)`

Parse a JSON string into a value.

**Example:**
```baseline
let data = Json.parse("{\"name\": \"Alice\"}")
```

---

### `Json.to_string`

```
fn to_string(<unknown>) -> String
```

**Prelude:** `@prelude(pure)`

Serialize a value to a compact JSON string.

**Example:**
```baseline
Json.to_string({ name: "Alice", age: 30 })
// => "{\"name\":\"Alice\",\"age\":30}"
```

---

### `Json.to_string_pretty`

```
fn to_string_pretty(<unknown>) -> String
```

**Prelude:** `@prelude(pure)`

Serialize a value to a pretty-printed JSON string.

**Example:**
```baseline
Json.to_string_pretty({ name: "Alice" })
```

---

## List

Transform collections with map and filter, reduce values with fold, and search for elements.

### `List.length`

```
fn length(<unknown>) -> Int
```

**Prelude:** `@prelude(pure)`

Return the number of elements in a list.

**Example:**
```baseline
List.length([1, 2, 3])  // => 3
```

---

### `List.head`

```
fn head(List<T>) -> Option<T>
```

**Prelude:** `@prelude(pure)`

Return the first element, or None if the list is empty.

**Example:**
```baseline
List.head([1, 2, 3])  // => Some(1)
```

---

### `List.tail`

```
fn tail(List<T>) -> List<T>
```

**Prelude:** `@prelude(pure)`

Return all elements except the first.

**Example:**
```baseline
List.tail([1, 2, 3])  // => [2, 3]
```

---

### `List.reverse`

```
fn reverse(List<T>) -> List<T>
```

**Prelude:** `@prelude(pure)`

Return the list in reverse order.

**Example:**
```baseline
List.reverse([1, 2, 3])  // => [3, 2, 1]
```

---

### `List.sort`

```
fn sort(List<T>) -> List<T>
```

**Prelude:** `@prelude(pure)`

Return the list sorted in ascending order.

**Example:**
```baseline
List.sort([3, 1, 2])  // => [1, 2, 3]
```

---

### `List.concat`

```
fn concat(List<T>, List<T>) -> List<T>
```

**Prelude:** `@prelude(pure)`

Concatenate two lists.

**Example:**
```baseline
List.concat([1, 2], [3, 4])  // => [1, 2, 3, 4]
```

---

### `List.contains`

```
fn contains(<unknown>, <unknown>) -> Bool
```

**Prelude:** `@prelude(pure)`

Check if a list contains a given element.

**Example:**
```baseline
List.contains([1, 2, 3], 2)  // => true
```

---

### `List.get`

```
fn get(List<T>, Int) -> Option<T>
```

**Prelude:** `@prelude(pure)`

Return the element at an index, or None if out of bounds.

**Example:**
```baseline
List.get([10, 20, 30], 1)  // => Some(20)
```

---

### `List.map`

```
fn map(List<T>, (T) -> U) -> List<U>
```

**Prelude:** `@prelude(pure)`

Apply a function to every element, returning a new list.

**Example:**
```baseline
[1, 2, 3] |> List.map(|x| x * 2)
// => [2, 4, 6]
```

---

### `List.filter`

```
fn filter(List<T>, (T) -> Bool) -> List<T>
```

**Prelude:** `@prelude(pure)`

Return elements that satisfy a predicate.

**Example:**
```baseline
[1, 2, 3, 4] |> List.filter(|x| x > 2)
// => [3, 4]
```

---

### `List.fold`

```
fn fold(List<T>, U, (U, T) -> U) -> U
```

**Prelude:** `@prelude(pure)`

Reduce a list to a single value using an accumulator function.

**Example:**
```baseline
[1, 2, 3] |> List.fold(0, |acc, x| acc + x)
// => 6
```

---

### `List.find`

```
fn find(List<T>, (T) -> Bool) -> Option<T>
```

**Prelude:** `@prelude(pure)`

Return the first element that satisfies a predicate, or None.

**Example:**
```baseline
[1, 2, 3] |> List.find(|x| x > 1)
// => Some(2)
```

---

## Log

Record structured events at different severity levels for debugging and monitoring.

### `Log.info!`

```
fn info(String) -> Unit
```

**Effects:** Log

**Prelude:** `@prelude(script)`

Log a message at info level.

**Example:**
```baseline
Log.info!("Server started on port 3000")
```

---

### `Log.warn!`

```
fn warn(String) -> Unit
```

**Effects:** Log

**Prelude:** `@prelude(script)`

Log a message at warning level.

**Example:**
```baseline
Log.warn!("Cache miss for key: ${key}")
```

---

### `Log.error!`

```
fn error(String) -> Unit
```

**Effects:** Log

**Prelude:** `@prelude(script)`

Log a message at error level.

**Example:**
```baseline
Log.error!("Failed to connect: ${msg}")
```

---

### `Log.debug!`

```
fn debug(String) -> Unit
```

**Effects:** Log

**Prelude:** `@prelude(script)`

Log a message at debug level.

**Example:**
```baseline
Log.debug!("Request payload: ${body}")
```

---

## Map

Store and retrieve values by key in immutable dictionaries.

### `Map.empty`

```
fn empty() -> Map<K, V>
```

**Prelude:** `@prelude(core)`

Create an empty map.

---

### `Map.insert`

```
fn insert(Map<K, V>, K, V) -> Map<K, V>
```

**Prelude:** `@prelude(core)`

Insert a key-value pair, returning a new map.

**Example:**
```baseline
Map.empty()
|> Map.insert("name", "Alice")
|> Map.insert("age", "30")
```

---

### `Map.get`

```
fn get(Map<K, V>, K) -> Option<V>
```

**Prelude:** `@prelude(core)`

Look up a key, returning Some(value) or None.

**Example:**
```baseline
map |> Map.get("name")  // => Some("Alice")
```

---

### `Map.remove`

```
fn remove(Map<K, V>, K) -> Map<K, V>
```

**Prelude:** `@prelude(core)`

Remove a key, returning a new map.

**Example:**
```baseline
map |> Map.remove("age")
```

---

### `Map.contains`

```
fn contains(Map<K, V>, K) -> Bool
```

**Prelude:** `@prelude(core)`

Check if a key exists in the map.

**Example:**
```baseline
Map.contains(map, "name")  // => true
```

---

### `Map.keys`

```
fn keys(Map<K, V>) -> List<K>
```

**Prelude:** `@prelude(core)`

Return all keys as a list.

**Example:**
```baseline
Map.keys(map)  // => ["age", "name"]
```

---

### `Map.values`

```
fn values(Map<K, V>) -> List<V>
```

**Prelude:** `@prelude(core)`

Return all values as a list.

**Example:**
```baseline
Map.values(map)  // => ["30", "Alice"]
```

---

### `Map.len`

```
fn len(Map<K, V>) -> Int
```

**Prelude:** `@prelude(core)`

Return the number of entries in the map.

**Example:**
```baseline
Map.len(map)  // => 2
```

---

### `Map.from_list`

```
fn from_list(...)
```

**Prelude:** `@prelude(core)`

Create a map from a list of (key, value) pairs.

**Example:**
```baseline
Map.from_list([("a", 1), ("b", 2)])
```

---

## Math

Perform common calculations like absolute value, min/max, and exponentiation.

### `Math.abs`

```
fn abs(Int) -> Int
```

**Prelude:** `@prelude(pure)`

Return the absolute value of an integer.

**Example:**
```baseline
Math.abs(-5)  // => 5
```

---

### `Math.min`

```
fn min(Int, Int) -> Int
```

**Prelude:** `@prelude(pure)`

Return the smaller of two integers.

**Example:**
```baseline
Math.min(3, 7)  // => 3
```

---

### `Math.max`

```
fn max(Int, Int) -> Int
```

**Prelude:** `@prelude(pure)`

Return the larger of two integers.

**Example:**
```baseline
Math.max(3, 7)  // => 7
```

---

### `Math.clamp`

```
fn clamp(Int, Int, Int) -> Int
```

**Prelude:** `@prelude(pure)`

Constrain a value between a minimum and maximum.

**Example:**
```baseline
Math.clamp(15, 0, 10)  // => 10
```

---

### `Math.pow`

```
fn pow(Int, Int) -> Int
```

**Prelude:** `@prelude(pure)`

Raise a base to an exponent.

**Example:**
```baseline
Math.pow(2, 10)  // => 1024
```

---

## Middleware

Extract authentication credentials, configure CORS, and set up rate limiting.

### `Middleware.extract_bearer`

```
fn extract_bearer(<unknown>) -> <unknown>
```

**Prelude:** `@prelude(script)`

Extract a Bearer token from the Authorization header.

**Example:**
```baseline
let token = Middleware.extract_bearer(req)
```

---

### `Middleware.extract_basic`

```
fn extract_basic(<unknown>) -> <unknown>
```

**Prelude:** `@prelude(script)`

Extract Basic auth credentials from the Authorization header.

**Example:**
```baseline
let creds = Middleware.extract_basic(req)
```

---

### `Middleware.cors_config`

```
fn cors_config() -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a CORS configuration record.

---

### `Middleware.rate_limit_config`

```
fn rate_limit_config() -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a rate limiting configuration record.

---

## Option

Represent values that might be missing without using null.

### `Option.unwrap`

```
fn unwrap(Option<T>) -> T
```

**Prelude:** `@prelude(minimal)`

Extract the value from Some, or panic on None.

**Example:**
```baseline
Some(42) |> Option.unwrap  // => 42
```

---

### `Option.unwrap_or`

```
fn unwrap_or(Option<T>, T) -> T
```

**Prelude:** `@prelude(minimal)`

Extract the value from Some, or return a default.

**Example:**
```baseline
None |> Option.unwrap_or(0)  // => 0
```

---

### `Option.is_some`

```
fn is_some(<unknown>) -> Bool
```

**Prelude:** `@prelude(minimal)`

Return true if the option contains a value.

**Example:**
```baseline
Option.is_some(Some(1))  // => true
```

---

### `Option.is_none`

```
fn is_none(<unknown>) -> Bool
```

**Prelude:** `@prelude(minimal)`

Return true if the option is None.

**Example:**
```baseline
Option.is_none(None)  // => true
```

---

### `Option.map`

```
fn map(Option<T>, (T) -> U) -> Option<U>
```

**Prelude:** `@prelude(minimal)`

Apply a function to the contained value, if present.

**Example:**
```baseline
Some(5) |> Option.map(|x| x * 2)
// => Some(10)
```

---

### `Option.flat_map`

```
fn flat_map(Option<T>, (T) -> Option<U>) -> Option<U>
```

**Prelude:** `@prelude(minimal)`

Apply a function that returns an Option, flattening the result.

**Example:**
```baseline
Some(5) |> Option.flat_map(|x|
  if x > 0 then Some(x) else None
)
// => Some(5)
```

---

## Random

Generate random numbers, booleans, and unique identifiers.

### `Random.int!`

```
fn int(Int, Int) -> Int
```

**Effects:** Random

**Prelude:** `@prelude(script)`

Generate a random integer between min and max (inclusive).

**Example:**
```baseline
Random.int!(1, 100)
```

---

### `Random.bool!`

```
fn bool() -> Bool
```

**Effects:** Random

**Prelude:** `@prelude(script)`

Generate a random boolean.

**Example:**
```baseline
if Random.bool!() then "heads" else "tails"
```

---

### `Random.uuid!`

```
fn uuid() -> String
```

**Effects:** Random

**Prelude:** `@prelude(script)`

Generate a random UUID v4 string.

**Example:**
```baseline
let id = Random.uuid!()
```

---

## Request

Extract HTTP method, headers, body, and attached state from incoming requests.

### `Request.header`

```
fn header(<unknown>, String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Read a header value from the request.

**Example:**
```baseline
let token = Request.header(req, "Authorization")
```

---

### `Request.method`

```
fn method(<unknown>) -> String
```

**Prelude:** `@prelude(script)`

Return the HTTP method of the request.

**Example:**
```baseline
let method = Request.method(req)
```

---

### `Request.body_json`

```
fn body_json(<unknown>) -> <unknown>
```

**Prelude:** `@prelude(script)`

Parse the request body as JSON.

**Example:**
```baseline
let data = Request.body_json(req)
```

---

## Response

Build HTTP responses with custom status codes, headers, and JSON or text bodies.

### `Response.ok`

```
fn ok(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 200 OK response with a text body.

**Example:**
```baseline
Response.ok("Hello!")
```

---

### `Response.json`

```
fn json(<unknown>) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 200 OK response with a JSON body.

**Example:**
```baseline
Response.json({ users: users, count: List.length(users) })
```

---

### `Response.created`

```
fn created(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 201 Created response with a text body.

**Example:**
```baseline
Response.created(user)
```

---

### `Response.no_content`

```
fn no_content() -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 204 No Content response.

**Example:**
```baseline
Response.no_content()
```

---

### `Response.bad_request`

```
fn bad_request(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 400 Bad Request response.

**Example:**
```baseline
Response.bad_request("Missing required field: name")
```

---

### `Response.not_found`

```
fn not_found(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 404 Not Found response.

**Example:**
```baseline
Response.not_found("User not found")
```

---

### `Response.error`

```
fn error(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 500 Internal Server Error response.

**Example:**
```baseline
Response.error("Internal error")
```

---

### `Response.status`

```
fn status(Int, String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a response with a custom status code and body.

**Example:**
```baseline
Response.status(429, "Rate limit exceeded")
```

---

### `Response.with_header`

```
fn with_header(<unknown>, String, String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Add a single header to a response.

**Example:**
```baseline
Response.ok("hi")
|> Response.with_header("X-Request-Id", id)
```

---

### `Response.with_headers`

```
fn with_headers(<unknown>, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(script)`

Add multiple headers to a response.

---

### `Response.redirect`

```
fn redirect(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 302 temporary redirect to a URL.

**Example:**
```baseline
Response.redirect("/login")
```

---

### `Response.redirect_permanent`

```
fn redirect_permanent(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 301 permanent redirect to a URL.

---

## Result

Handle operations that can fail with explicit error values instead of exceptions.

### `Result.unwrap`

```
fn unwrap(Result<T, E>) -> T
```

**Prelude:** `@prelude(minimal)`

Extract the Ok value, or panic on Err.

**Example:**
```baseline
Ok(42) |> Result.unwrap  // => 42
```

---

### `Result.unwrap_or`

```
fn unwrap_or(Result<T, E>, T) -> T
```

**Prelude:** `@prelude(minimal)`

Extract the Ok value, or return a default.

**Example:**
```baseline
Err("fail") |> Result.unwrap_or(0)  // => 0
```

---

### `Result.is_ok`

```
fn is_ok(<unknown>) -> Bool
```

**Prelude:** `@prelude(minimal)`

Return true if the result is Ok.

**Example:**
```baseline
Result.is_ok(Ok(1))  // => true
```

---

### `Result.is_err`

```
fn is_err(<unknown>) -> Bool
```

**Prelude:** `@prelude(minimal)`

Return true if the result is Err.

**Example:**
```baseline
Result.is_err(Err("fail"))  // => true
```

---

### `Result.map`

```
fn map(Result<T, ?2>, (T) -> E) -> Result<E, ?2>
```

**Prelude:** `@prelude(minimal)`

Apply a function to the Ok value, if present.

**Example:**
```baseline
Ok(5) |> Result.map(|x| x * 2)
// => Ok(10)
```

---

### `Result.and_then`

```
fn and_then(Result<T, E>, (T) -> Result<U, E>) -> Result<U, E>
```

**Prelude:** `@prelude(minimal)`

Chain a function that returns a Result, flattening the result.

**Example:**
```baseline
Ok(5) |> Result.and_then(|x|
  if x > 0 then Ok(x) else Err("negative")
)
// => Ok(5)
```

---

### `Result.map_err`

```
fn map_err(Result<T, U>, (U) -> E) -> Result<T, E>
```

**Prelude:** `@prelude(minimal)`

Apply a function to the Err value, if present.

**Example:**
```baseline
Err("fail") |> Result.map_err(|e| "wrapped: " ++ e)
// => Err("wrapped: fail")
```

---

### `Result.context`

```
fn context(Result<T, E>, String) -> Result<T, { context: String, error: ?1 }>
```

**Prelude:** `@prelude(minimal)`

Add context to an error, wrapping it in a record.

**Example:**
```baseline
Err("db fail") |> Result.context("loading user")
// => Err({ error: "db fail", context: "loading user" })
```

---

## Router

Define URL patterns, register handlers for HTTP methods, and organize routes into groups.

### `Router.new`

```
fn new() -> <unknown>
```

**Prelude:** `@prelude(server)`

Create a new empty router.

**Example:**
```baseline
let app = Router.new()
```

---

### `Router.routes`

```
fn routes(<unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Return the configured route table.

---

### `Router.get`

```
fn get(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Register a handler for GET requests at a path.

**Example:**
```baseline
Router.new()
|> Router.get("/hello", |req| Response.ok("Hello!"))
```

---

### `Router.post`

```
fn post(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Register a handler for POST requests at a path.

**Example:**
```baseline
router |> Router.post("/users", create_user)
```

---

### `Router.put`

```
fn put(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Register a handler for PUT requests at a path.

---

### `Router.delete`

```
fn delete(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Register a handler for DELETE requests at a path.

---

### `Router.patch`

```
fn patch(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Register a handler for PATCH requests at a path.

---

### `Router.options`

```
fn options(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Register a handler for OPTIONS requests at a path.

---

### `Router.head`

```
fn head(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Register a handler for HEAD requests at a path.

---

### `Router.any`

```
fn any(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Register a handler for all HTTP methods at a path.

---

### `Router.use`

```
fn use(<unknown>, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Add middleware to the router.

**Example:**
```baseline
router |> Router.use(auth_middleware)
```

---

### `Router.group`

```
fn group(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Group routes under a shared path prefix.

**Example:**
```baseline
router |> Router.group("/api", |r|
  r |> Router.get("/users", list_users)
    |> Router.post("/users", create_user)
)
```

---

## Server

Start an HTTP server listening on a specific port.

### `Server.listen!`

```
fn listen(<unknown>, Int) -> Unit
```

**Effects:** Http

**Prelude:** `@prelude(server)`

Start the HTTP server on the given port.

**Example:**
```baseline
let app = Router.new()
  |> Router.get("/", |req| Response.ok("Hello!"))

Server.listen!(app, 3000)
```

---

## Set

Store unique elements and perform operations like union, intersection, and membership testing.

### `Set.empty`

```
fn empty() -> Set<T>
```

**Prelude:** `@prelude(core)`

Create an empty set.

---

### `Set.insert`

```
fn insert(Set<T>, T) -> Set<T>
```

**Prelude:** `@prelude(core)`

Add an element, returning a new set.

**Example:**
```baseline
Set.empty() |> Set.insert(1) |> Set.insert(2)
```

---

### `Set.remove`

```
fn remove(Set<T>, T) -> Set<T>
```

**Prelude:** `@prelude(core)`

Remove an element, returning a new set.

---

### `Set.contains`

```
fn contains(Set<T>, T) -> Bool
```

**Prelude:** `@prelude(core)`

Check if an element exists in the set.

**Example:**
```baseline
Set.contains(s, 1)  // => true
```

---

### `Set.union`

```
fn union(Set<T>, Set<T>) -> Set<T>
```

**Prelude:** `@prelude(core)`

Return the union of two sets.

---

### `Set.intersection`

```
fn intersection(Set<T>, Set<T>) -> Set<T>
```

**Prelude:** `@prelude(core)`

Return the intersection of two sets.

---

### `Set.len`

```
fn len(Set<T>) -> Int
```

**Prelude:** `@prelude(core)`

Return the number of elements in the set.

---

### `Set.from_list`

```
fn from_list(List<T>) -> Set<T>
```

**Prelude:** `@prelude(core)`

Create a set from a list of elements.

**Example:**
```baseline
Set.from_list([1, 2, 3, 2])  // duplicates removed
```

---

## String

Split, trim, search, replace, and convert between strings and individual characters.

### `String.length`

```
fn length(String) -> Int
```

**Prelude:** `@prelude(pure)`

Return the number of characters in a string.

**Example:**
```baseline
String.length("hello")  // => 5
```

---

### `String.to_upper`

```
fn to_upper(String) -> String
```

**Prelude:** `@prelude(pure)`

Convert all characters to uppercase.

**Example:**
```baseline
String.to_upper("hello")  // => "HELLO"
```

---

### `String.to_lower`

```
fn to_lower(String) -> String
```

**Prelude:** `@prelude(pure)`

Convert all characters to lowercase.

**Example:**
```baseline
String.to_lower("HELLO")  // => "hello"
```

---

### `String.trim`

```
fn trim(String) -> String
```

**Prelude:** `@prelude(pure)`

Remove leading and trailing whitespace.

**Example:**
```baseline
String.trim("  hi  ")  // => "hi"
```

---

### `String.contains`

```
fn contains(String, String) -> Bool
```

**Prelude:** `@prelude(pure)`

Check if a string contains a substring.

**Example:**
```baseline
String.contains("hello world", "world")  // => true
```

---

### `String.starts_with`

```
fn starts_with(String, String) -> Bool
```

**Prelude:** `@prelude(pure)`

Check if a string starts with a prefix.

**Example:**
```baseline
String.starts_with("hello", "hel")  // => true
```

---

### `String.ends_with`

```
fn ends_with(String, String) -> Bool
```

**Prelude:** `@prelude(pure)`

Check if a string ends with a suffix.

**Example:**
```baseline
String.ends_with("hello", "llo")  // => true
```

---

### `String.split`

```
fn split(String, String) -> List<String>
```

**Prelude:** `@prelude(pure)`

Split a string by a delimiter into a list of strings.

**Example:**
```baseline
"a,b,c" |> String.split(",")  // => ["a", "b", "c"]
```

---

### `String.join`

```
fn join(List<String>, String) -> String
```

**Prelude:** `@prelude(pure)`

Join a list of strings with a separator.

**Example:**
```baseline
["a", "b", "c"] |> String.join(", ")  // => "a, b, c"
```

---

### `String.slice`

```
fn slice(String, Int, Int) -> String
```

**Prelude:** `@prelude(pure)`

Extract a substring by start and end index.

**Example:**
```baseline
String.slice("hello", 1, 4)  // => "ell"
```

---

### `String.chars`

```
fn chars(String) -> List<String>
```

**Prelude:** `@prelude(pure)`

Split a string into a list of single-character strings.

**Example:**
```baseline
String.chars("hi")  // => ["h", "i"]
```

---

### `String.char_at`

```
fn char_at(String, Int) -> String
```

**Prelude:** `@prelude(pure)`

Return the character at a given index.

**Example:**
```baseline
String.char_at("hello", 0)  // => "h"
```

---

### `String.index_of`

```
fn index_of(String, String) -> Int
```

**Prelude:** `@prelude(pure)`

Return the index of the first occurrence of a substring, or -1.

**Example:**
```baseline
String.index_of("hello", "ll")  // => 2
```

---

### `String.to_int`

```
fn to_int(String) -> Int
```

**Prelude:** `@prelude(pure)`

Parse a string as an integer.

**Example:**
```baseline
String.to_int("42")  // => 42
```

---

### `String.from_char_code`

```
fn from_char_code(Int) -> String
```

**Prelude:** `@prelude(pure)`

Create a single-character string from a Unicode code point.

**Example:**
```baseline
String.from_char_code(65)  // => "A"
```

---

### `String.char_code`

```
fn char_code(String) -> Int
```

**Prelude:** `@prelude(pure)`

Return the Unicode code point of the first character.

**Example:**
```baseline
String.char_code("A")  // => 65
```

---

### `String.replace`

```
fn replace(String, String, String) -> String
```

**Prelude:** `@prelude(pure)`

Replace all occurrences of a pattern with a replacement.

**Example:**
```baseline
String.replace("hello world", "world", "there")  // => "hello there"
```

---

## Time

Get the current timestamp and pause execution for a specified duration.

### `Time.now!`

```
fn now() -> Int
```

**Effects:** Time

**Prelude:** `@prelude(script)`

Return the current Unix timestamp in milliseconds.

**Example:**
```baseline
let start = Time.now!()
```

---

### `Time.sleep!`

```
fn sleep(Int) -> Unit
```

**Effects:** Time

**Prelude:** `@prelude(script)`

Pause execution for the given number of milliseconds.

**Example:**
```baseline
Time.sleep!(1000)  // wait 1 second
```

---

## Weak

Break reference cycles with weak pointers that don't prevent garbage collection.

### `Weak.downgrade`

```
fn downgrade(T) -> Weak<T>
```

**Prelude:** `@prelude(core)`

Create a weak reference from a strong reference.

---

### `Weak.upgrade`

```
fn upgrade(Weak<T>) -> Option<T>
```

**Prelude:** `@prelude(core)`

Attempt to upgrade a weak reference to a strong one, returning an Option.

---

