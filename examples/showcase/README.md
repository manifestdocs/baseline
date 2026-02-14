# Todo API — Baseline Showcase

A JSON REST API built in Baseline, demonstrating the language's core features in a real application.

## Run

```bash
blc run examples/showcase/main.bl
```

The server starts on `http://localhost:8080`.

## Test

```bash
blc test examples/showcase/Validate.bl
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

- **Multiple modules** — `main.bl` imports from `Handlers.bl` and `Validate.bl` via selective imports
- **Effect tracking** — `main!` declares `{Http, Log, Random}`, handlers declare `{Http}` or `{Http, Random}`
- **Pattern matching** — nested `match` on `Option`/`Result` for request parsing and error handling
- **Input validation** — `Validate.title` and `Validate.status` return `Result<String, String>`
- **Pipe composition** — router built with `|>` chains: `Router.new() |> Router.get(...) |> ...`
- **JSON serialization** — `Response.json(record)` and `Json.to_string(record)` for structured responses
- **@test sections** — `Validate.bl` has 5 inline tests for the validation functions

## Project Structure

```
examples/showcase/
├── main.bl          # Entry point — router setup + Server.listen!
├── Handlers.bl      # CRUD handlers (imports Validate)
├── Validate.bl      # Input validation with @test section
├── Middleware.bl     # Logger, CORS, timer (reference; see note)
└── README.md
```
