# Todo API — Baseline Showcase

A JSON REST API built in Baseline, demonstrating the language's core features in a real application.

## Run

```bash
blc run examples/showcase/main.bl
```

The server starts on `http://localhost:8080`.

## Test

```bash
blc test examples/showcase/Todo.bl
```

## Endpoints

| Method | Path | Description |
|--------|------|-------------|
| GET | `/health` | Service health check |
| GET | `/todos` | List all todos |
| GET | `/todos/:id` | Get a todo by ID |
| POST | `/todos` | Create a todo (JSON body with `title`) |
| PUT | `/todos/:id` | Update a todo (JSON body with `title` and `status`) |
| DELETE | `/todos/:id` | Delete a todo |

## Try it

```bash
curl http://localhost:8080/health
curl http://localhost:8080/todos
curl http://localhost:8080/todos/1
curl -X POST -H "Content-Type: application/json" -d '{"title":"Buy milk"}' http://localhost:8080/todos
curl -X PUT -H "Content-Type: application/json" -d '{"title":"Buy milk","status":"done"}' http://localhost:8080/todos/1
curl -X DELETE http://localhost:8080/todos/1
```

## Language Features Demonstrated

- **Entity module** — `Todo.bl` encapsulates data, constructors, and parsing ("parse, don't validate")
- **Multiple modules** — `main.bl` imports from `Handlers.bl`, which imports `Todo.bl`
- **Effect tracking** — `main!` declares `{Http, Log, Random}`, handlers declare `{Http}` or `{Http, Random}`
- **? error propagation** — handlers compose Result-returning parsers with `?`, errors carry proper HTTP status codes
- **Pattern matching** — `match` on `Option`/`Result` for request parsing and domain logic
- **Pipe composition** — router built with `|>` chains: `Router.new() |> Router.get(...) |> ...`
- **JSON serialization** — `Response.json(record)` and `Json.to_string(record)` for structured responses
- **@test sections** — `Todo.bl` has inline tests for the parsing functions

## Project Structure

```
examples/showcase/
├── main.bl          # Entry point — router setup + Server.listen!
├── Handlers.bl      # HTTP layer — request parsing + route handlers
├── Todo.bl          # Entity — data, constructors, parsing + @test
├── Middleware.bl     # Logger, CORS, timer (reference; see note)
└── README.md
```
