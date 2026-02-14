# Baseline Standard Library Reference

## Console

### `Console.println!`

```
fn println(String) -> Unit
```

**Effects:** Console

**Prelude:** `@prelude(script)`

Print a string to stdout followed by a newline.

---

### `Console.print!`

```
fn print(String) -> Unit
```

**Effects:** Console

**Prelude:** `@prelude(script)`

Print a string to stdout without a trailing newline.

---

### `Console.error!`

```
fn error(String) -> Unit
```

**Effects:** Console

**Prelude:** `@prelude(script)`

Print a string to stderr.

---

### `Console.read_line!`

```
fn read_line() -> String
```

**Effects:** Console

**Prelude:** `@prelude(script)`

Read a line of input from stdin.

---

## Env

### `Env.get!`

```
fn get(String) -> Option<String>
```

**Effects:** Env

**Prelude:** `@prelude(script)`

Read an environment variable, returning None if unset.

---

### `Env.set!`

```
fn set(String, String) -> Unit
```

**Effects:** Env

**Prelude:** `@prelude(script)`

Set an environment variable.

---

## Fs

### `Fs.read!`

```
fn read(String) -> String
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

Read a file's contents as a string.

---

### `Fs.write!`

```
fn write(String, String) -> Unit
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

Write a string to a file, creating or overwriting it.

---

### `Fs.exists!`

```
fn exists(String) -> Bool
```

**Effects:** Fs

**Prelude:** `@prelude(script)`

Check if a file path exists.

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

### `Http.get!`

```
fn get(String) -> <unknown>
```

**Effects:** Http

**Prelude:** `@prelude(script)`

Send an HTTP GET request to a URL.

---

### `Http.post!`

```
fn post(String, String) -> <unknown>
```

**Effects:** Http

**Prelude:** `@prelude(script)`

Send an HTTP POST request with a body.

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

## Int

### `Int.to_string`

```
fn to_string(Int) -> String
```

**Prelude:** `@prelude(core)`

Convert an integer to its string representation.

---

### `Int.parse`

```
fn parse(String) -> Int
```

**Prelude:** `@prelude(core)`

Parse a string as an integer.

---

## Json

### `Json.parse`

```
fn parse(String) -> <unknown>
```

**Prelude:** `@prelude(pure)`

Parse a JSON string into a value.

---

### `Json.to_string`

```
fn to_string(<unknown>) -> String
```

**Prelude:** `@prelude(pure)`

Serialize a value to a compact JSON string.

---

### `Json.to_string_pretty`

```
fn to_string_pretty(<unknown>) -> String
```

**Prelude:** `@prelude(pure)`

Serialize a value to a pretty-printed JSON string.

---

## List

### `List.length`

```
fn length(<unknown>) -> Int
```

**Prelude:** `@prelude(pure)`

Return the number of elements in a list.

---

### `List.head`

```
fn head(List<T>) -> T
```

**Prelude:** `@prelude(pure)`

Return the first element, or None if the list is empty.

---

### `List.tail`

```
fn tail(List<T>) -> List<T>
```

**Prelude:** `@prelude(pure)`

Return all elements except the first.

---

### `List.reverse`

```
fn reverse(List<T>) -> List<T>
```

**Prelude:** `@prelude(pure)`

Return the list in reverse order.

---

### `List.sort`

```
fn sort(List<T>) -> List<T>
```

**Prelude:** `@prelude(pure)`

Return the list sorted in ascending order.

---

### `List.concat`

```
fn concat(List<T>, List<T>) -> List<T>
```

**Prelude:** `@prelude(pure)`

Concatenate two lists.

---

### `List.contains`

```
fn contains(<unknown>, <unknown>) -> Bool
```

**Prelude:** `@prelude(pure)`

Check if a list contains a given element.

---

### `List.get`

```
fn get(List<T>, Int) -> Option<T>
```

**Prelude:** `@prelude(pure)`

Return the element at an index, or None if out of bounds.

---

### `List.map`

```
fn map(List<T>, (T) -> U) -> List<U>
```

**Prelude:** `@prelude(pure)`

Apply a function to every element, returning a new list.

---

### `List.filter`

```
fn filter(List<T>, (T) -> Bool) -> List<T>
```

**Prelude:** `@prelude(pure)`

Return elements that satisfy a predicate.

---

### `List.fold`

```
fn fold(List<T>, U, (U, T) -> U) -> U
```

**Prelude:** `@prelude(pure)`

Reduce a list to a single value using an accumulator function.

---

### `List.find`

```
fn find(List<T>, (T) -> Bool) -> Option<T>
```

**Prelude:** `@prelude(pure)`

Return the first element that satisfies a predicate, or None.

---

## Log

### `Log.info!`

```
fn info(String) -> Unit
```

**Effects:** Log

**Prelude:** `@prelude(script)`

Log a message at info level.

---

### `Log.warn!`

```
fn warn(String) -> Unit
```

**Effects:** Log

**Prelude:** `@prelude(script)`

Log a message at warning level.

---

### `Log.error!`

```
fn error(String) -> Unit
```

**Effects:** Log

**Prelude:** `@prelude(script)`

Log a message at error level.

---

### `Log.debug!`

```
fn debug(String) -> Unit
```

**Effects:** Log

**Prelude:** `@prelude(script)`

Log a message at debug level.

---

## Map

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

---

### `Map.get`

```
fn get(Map<K, V>, K) -> Option<V>
```

**Prelude:** `@prelude(core)`

Look up a key, returning Some(value) or None.

---

### `Map.remove`

```
fn remove(Map<K, V>, K) -> Map<K, V>
```

**Prelude:** `@prelude(core)`

Remove a key, returning a new map.

---

### `Map.contains`

```
fn contains(Map<K, V>, K) -> Bool
```

**Prelude:** `@prelude(core)`

Check if a key exists in the map.

---

### `Map.keys`

```
fn keys(Map<K, V>) -> List<K>
```

**Prelude:** `@prelude(core)`

Return all keys as a list.

---

### `Map.values`

```
fn values(Map<K, V>) -> List<V>
```

**Prelude:** `@prelude(core)`

Return all values as a list.

---

### `Map.len`

```
fn len(Map<K, V>) -> Int
```

**Prelude:** `@prelude(core)`

Return the number of entries in the map.

---

### `Map.from_list`

```
fn from_list(...)
```

**Prelude:** `@prelude(core)`

Create a map from a list of (key, value) pairs.

---

## Math

### `Math.abs`

```
fn abs(Int) -> Int
```

**Prelude:** `@prelude(pure)`

Return the absolute value of an integer.

---

### `Math.min`

```
fn min(Int, Int) -> Int
```

**Prelude:** `@prelude(pure)`

Return the smaller of two integers.

---

### `Math.max`

```
fn max(Int, Int) -> Int
```

**Prelude:** `@prelude(pure)`

Return the larger of two integers.

---

### `Math.clamp`

```
fn clamp(Int, Int, Int) -> Int
```

**Prelude:** `@prelude(pure)`

Constrain a value between a minimum and maximum.

---

### `Math.pow`

```
fn pow(Int, Int) -> Int
```

**Prelude:** `@prelude(pure)`

Raise a base to an exponent.

---

## Option

### `Option.unwrap`

```
fn unwrap(Option<T>) -> T
```

**Prelude:** `@prelude(minimal)`

Extract the value from Some, or panic on None.

---

### `Option.unwrap_or`

```
fn unwrap_or(Option<T>, T) -> T
```

**Prelude:** `@prelude(minimal)`

Extract the value from Some, or return a default.

---

### `Option.is_some`

```
fn is_some(<unknown>) -> Bool
```

**Prelude:** `@prelude(minimal)`

Return true if the option contains a value.

---

### `Option.is_none`

```
fn is_none(<unknown>) -> Bool
```

**Prelude:** `@prelude(minimal)`

Return true if the option is None.

---

### `Option.map`

```
fn map(Option<T>, (T) -> U) -> Option<U>
```

**Prelude:** `@prelude(minimal)`

Apply a function to the contained value, if present.

---

### `Option.flat_map`

```
fn flat_map(Option<T>, (T) -> Option<U>) -> Option<U>
```

**Prelude:** `@prelude(minimal)`

Apply a function that returns an Option, flattening the result.

---

## Random

### `Random.int!`

```
fn int(Int, Int) -> Int
```

**Effects:** Random

**Prelude:** `@prelude(script)`

Generate a random integer between min and max (inclusive).

---

### `Random.bool!`

```
fn bool() -> Bool
```

**Effects:** Random

**Prelude:** `@prelude(script)`

Generate a random boolean.

---

### `Random.uuid!`

```
fn uuid() -> String
```

**Effects:** Random

**Prelude:** `@prelude(script)`

Generate a random UUID v4 string.

---

## Request

### `Request.header`

```
fn header(<unknown>, String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Read a header value from the request.

---

### `Request.method`

```
fn method(<unknown>) -> String
```

**Prelude:** `@prelude(script)`

Return the HTTP method of the request.

---

### `Request.body_json`

```
fn body_json(<unknown>) -> <unknown>
```

**Prelude:** `@prelude(script)`

Parse the request body as JSON.

---

### `Request.with_state`

```
fn with_state(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(script)`

Attach a named state value to the request.

---

### `Request.state`

```
fn state(<unknown>, String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Retrieve a named state value from the request.

---

## Response

### `Response.ok`

```
fn ok(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 200 OK response with a text body.

---

### `Response.json`

```
fn json(<unknown>) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 200 OK response with a JSON body.

---

### `Response.created`

```
fn created(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 201 Created response with a text body.

---

### `Response.no_content`

```
fn no_content() -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 204 No Content response.

---

### `Response.bad_request`

```
fn bad_request(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 400 Bad Request response.

---

### `Response.not_found`

```
fn not_found(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 404 Not Found response.

---

### `Response.error`

```
fn error(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 500 Internal Server Error response.

---

### `Response.status`

```
fn status(Int, String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a response with a custom status code and body.

---

### `Response.with_header`

```
fn with_header(<unknown>, String, String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Add a single header to a response.

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

---

### `Response.redirect_permanent`

```
fn redirect_permanent(String) -> <unknown>
```

**Prelude:** `@prelude(script)`

Create a 301 permanent redirect to a URL.

---

## Result

### `Result.unwrap`

```
fn unwrap(Result<T, E>) -> T
```

**Prelude:** `@prelude(minimal)`

Extract the Ok value, or panic on Err.

---

### `Result.unwrap_or`

```
fn unwrap_or(Result<T, E>, T) -> T
```

**Prelude:** `@prelude(minimal)`

Extract the Ok value, or return a default.

---

### `Result.is_ok`

```
fn is_ok(<unknown>) -> Bool
```

**Prelude:** `@prelude(minimal)`

Return true if the result is Ok.

---

### `Result.is_err`

```
fn is_err(<unknown>) -> Bool
```

**Prelude:** `@prelude(minimal)`

Return true if the result is Err.

---

### `Result.map`

```
fn map(Result<T, ?2>, (T) -> E) -> Result<E, ?2>
```

**Prelude:** `@prelude(minimal)`

Apply a function to the Ok value, if present.

---

### `Result.and_then`

```
fn and_then(Result<T, E>, (T) -> Result<U, E>) -> Result<U, E>
```

**Prelude:** `@prelude(minimal)`

Chain a function that returns a Result, flattening the result.

---

## Router

### `Router.new`

```
fn new() -> <unknown>
```

**Prelude:** `@prelude(server)`

Create a new empty router.

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

---

### `Router.post`

```
fn post(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Register a handler for POST requests at a path.

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

---

### `Router.group`

```
fn group(<unknown>, String, <unknown>) -> <unknown>
```

**Prelude:** `@prelude(server)`

Group routes under a shared path prefix.

---

## Server

### `Server.listen!`

```
fn listen(<unknown>, Int) -> Unit
```

**Effects:** Http

**Prelude:** `@prelude(server)`

Start the HTTP server on the given port.

---

## Set

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

---

## String

### `String.length`

```
fn length(String) -> Int
```

**Prelude:** `@prelude(pure)`

Return the number of characters in a string.

---

### `String.to_upper`

```
fn to_upper(String) -> String
```

**Prelude:** `@prelude(pure)`

Convert all characters to uppercase.

---

### `String.to_lower`

```
fn to_lower(String) -> String
```

**Prelude:** `@prelude(pure)`

Convert all characters to lowercase.

---

### `String.trim`

```
fn trim(String) -> String
```

**Prelude:** `@prelude(pure)`

Remove leading and trailing whitespace.

---

### `String.contains`

```
fn contains(String, String) -> Bool
```

**Prelude:** `@prelude(pure)`

Check if a string contains a substring.

---

### `String.starts_with`

```
fn starts_with(String, String) -> Bool
```

**Prelude:** `@prelude(pure)`

Check if a string starts with a prefix.

---

### `String.ends_with`

```
fn ends_with(String, String) -> Bool
```

**Prelude:** `@prelude(pure)`

Check if a string ends with a suffix.

---

### `String.split`

```
fn split(String, String) -> List<String>
```

**Prelude:** `@prelude(pure)`

Split a string by a delimiter into a list of strings.

---

### `String.join`

```
fn join(List<String>, String) -> String
```

**Prelude:** `@prelude(pure)`

Join a list of strings with a separator.

---

### `String.slice`

```
fn slice(String, Int, Int) -> String
```

**Prelude:** `@prelude(pure)`

Extract a substring by start and end index.

---

### `String.chars`

```
fn chars(String) -> List<String>
```

**Prelude:** `@prelude(pure)`

Split a string into a list of single-character strings.

---

### `String.char_at`

```
fn char_at(String, Int) -> String
```

**Prelude:** `@prelude(pure)`

Return the character at a given index.

---

### `String.index_of`

```
fn index_of(String, String) -> Int
```

**Prelude:** `@prelude(pure)`

Return the index of the first occurrence of a substring, or -1.

---

### `String.to_int`

```
fn to_int(String) -> Int
```

**Prelude:** `@prelude(pure)`

Parse a string as an integer.

---

### `String.from_char_code`

```
fn from_char_code(Int) -> String
```

**Prelude:** `@prelude(pure)`

Create a single-character string from a Unicode code point.

---

### `String.char_code`

```
fn char_code(String) -> Int
```

**Prelude:** `@prelude(pure)`

Return the Unicode code point of the first character.

---

### `String.replace`

```
fn replace(String, String, String) -> String
```

**Prelude:** `@prelude(pure)`

Replace all occurrences of a pattern with a replacement.

---

## Time

### `Time.now!`

```
fn now() -> Int
```

**Effects:** Time

**Prelude:** `@prelude(script)`

Return the current Unix timestamp in milliseconds.

---

### `Time.sleep!`

```
fn sleep(Int) -> Unit
```

**Effects:** Time

**Prelude:** `@prelude(script)`

Pause execution for the given number of milliseconds.

---

## Weak

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

