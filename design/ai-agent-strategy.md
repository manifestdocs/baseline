# RFC-0001: AI Agent Integration Strategy

**Status:** Draft
**Author:** Alastair
**Created:** 2026-02-16
**Target:** Baseline v1.0

## Summary

Define the strategy for making Baseline usable by AI coding agents (Claude Code, GitHub Copilot, OpenAI Codex, Cursor, Windsurf) that have zero training data for the language. The approach uses layered context injection and tool integration rather than model fine-tuning, leveraging the `blc` compiler's existing structured output capabilities.

## Motivation

Baseline is a new language. No current LLM has seen it in training. Users adopting Baseline will expect AI-assisted development from day one. Fine-tuning is not viable: it's expensive, goes stale between model releases, and degrades the model's general capabilities. Research demonstrates that agentic compile-check-repair loops achieve 96% compilation success on unseen languages — better than most fine-tuning results — and improve automatically as base models improve.

The `blc` compiler already produces structured JSON diagnostics with confidence-scored fix suggestions, has built-in API documentation lookup, runs inline tests with JSON output, and offers a Constrained Generation Protocol. These features exist but are not yet wired into the tool-calling interfaces that coding agents use.

## Design Principles

1. **Meet agents where they are.** Every major coding agent has a different context injection mechanism. Ship integration for all of them rather than betting on one.
2. **Compiler as oracle.** The agent does not need to know Baseline perfectly. It needs to write something, check it, read the error, and fix it. The compiler closes the loop.
3. **Passive + active.** Passive context (llms.txt, instruction files) prevents common mistakes. Active tools (MCP server) fix the rest.
4. **Zero configuration for users.** Running `blc init` should scaffold all agent integration files. Users should not need to understand MCP configuration or prompt engineering.

## Architecture Overview

```
+-----------------------------------------------------------+
|                    AI Coding Agent                         |
|              (Claude Code / Copilot / Codex)               |
+----------+------------------------+-----------------------+
           |                        |
    Passive Context            Active Tools
           |                        |
           v                        v
+------------------+  +----------------------------------+
|  Layer 1: Files  |  |  Layer 2: MCP Server             |
|  - llms.txt      |  |  - baseline/check                |
|  - CLAUDE.md     |  |  - baseline/test                 |
|  - .cursorrules  |  |  - baseline/docs                 |
|  - copilot-inst  |  |  - baseline/reference            |
|                  |  |  - baseline/rosetta              |
+------------------+  +----------+-----------------------+
                                 |
                                 v
                        +------------------+
                        |   blc compiler   |
                        |   (binary)       |
                        +------------------+
```

The strategy is organized into four layers, ordered by implementation priority.

---

## Layer 1: Enhanced `llms.txt`

**Effort:** Small (hours)
**Reach:** All agents (any tool that reads project files)
**Status:** Exists, needs enhancement

### Current State

The `llms.txt` file covers syntax, types, patterns, the "NOT Supported" section, and common patterns in ~200 lines. It fits comfortably in a context window.

### Changes

Add a **Rosetta Stone** section at the top of the file, immediately after the header. Research on cross-lingual transfer in programming languages shows that parallel corpus examples are the single highest-leverage technique for zero-shot code generation. The model maps its existing Python/TypeScript knowledge onto Baseline syntax rather than learning from scratch.

#### Proposed Rosetta Stone Section

```
## Rosetta Stone (Python -> Baseline)

### Variable Declaration
Python:    x = 10
Baseline:  let x = 10

### Function Definition
Python:    def add(a: int, b: int) -> int: return a + b
Baseline:  fn add(a: Int, b: Int) -> Int = a + b

### Side Effects (Print)
Python:    print("hello")
Baseline:  Console.println!("hello")

### List Operations
Python:    [x * 2 for x in items if x > 0]
Baseline:  items |> List.filter(|x| x > 0) |> List.map(|x| x * 2)

### Error Handling
Python:    try: val = risky() / except: val = default
Baseline:  let val = match risky() { Ok(v) -> v, Err(_) -> default }

### Null Handling
Python:    if x is not None: use(x)
Baseline:  match x { Some(v) -> use(v), None -> fallback }

### String Concatenation
Python:    greeting = "Hello, " + name + "!"
Baseline:  let greeting = "Hello, ${name}!"

### Classes -> Records + Sum Types
Python:    class User: def __init__(self, name, age): ...
Baseline:  type User = { name: String, age: Int }

### Method Calls
Python:    name.upper()
Baseline:  String.to_upper(name)

### Boolean Negation
Python:    if not valid:
Baseline:  if not valid then ...

### HTTP Handler
Python:    @app.get("/health") / def health(): return "ok"
Baseline:  fn health(req: {...}) -> Result<{...}, String> = Ok(Response.ok("healthy"))
```

### Additional Enhancement: Common Mistakes Section

Append a section that explicitly lists what agents get wrong most often. Negative examples are as valuable as positive ones:

```
## Common Agent Mistakes

WRONG: user.to_string()   -> RIGHT: String.from(user) (no method calls on values)
WRONG: !valid             -> RIGHT: not valid
WRONG: "a" + "b"          -> RIGHT: "${a}${b}"
WRONG: return x           -> RIGHT: x (last expression is the return value)
WRONG: let mut x = 0      -> RIGHT: (no mutation -- restructure with recursion or fold)
WRONG: try { ... }        -> RIGHT: match expr { Ok(v) -> ..., Err(e) -> ... }
WRONG: async fn fetch()   -> RIGHT: fn fetch!() (effects replace async)
WRONG: x; y; z            -> RIGHT: remove semicolons, use { } blocks for sequencing
```

### File Locations

The `llms.txt` file should exist at two locations:

- **Project root** (`./llms.txt`) -- picked up by agents reading workspace files
- **Hosted at** `https://baseline-lang.dev/llms.txt` -- for Context7 and remote retrieval

Additionally, generate an `llms-full.txt` at the hosted URL containing the complete standard library reference for agents that support fetching extended documentation.

---

## Layer 2: MCP Server

**Effort:** Medium (1-2 weeks)
**Reach:** Claude Code, Codex, Cursor, Windsurf, any MCP-compatible agent
**Status:** New

### Overview

Ship an MCP server as a subcommand of `blc`:

```
blc mcp
```

This starts a stdio-based MCP server that exposes the compiler's capabilities as tools. The server is a thin wrapper around existing `blc` subcommands, translating between MCP's JSON-RPC protocol and the compiler's JSON output.

### Why MCP

MCP (Model Context Protocol) is supported by Claude Code, OpenAI Codex CLI, Cursor, Windsurf, and VS Code (via Copilot). It is the closest thing to a universal agent tool interface. Shipping MCP as a subcommand of the compiler binary means:

- No separate install step
- No Node.js/Python dependency
- Version-locked to the compiler (tools always match the language version)
- Single binary distribution

### Tools

#### `baseline/check`

Runs `blc check --json` on a file or inline source. Returns diagnostics with fix suggestions.

```json
{
  "name": "baseline/check",
  "description": "Type-check and validate Baseline source code. Returns structured diagnostics with fix suggestions ranked by confidence.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "file": {
        "type": "string",
        "description": "Path to .bl file to check"
      },
      "source": {
        "type": "string",
        "description": "Inline Baseline source code to check (alternative to file)"
      },
      "level": {
        "type": "string",
        "enum": ["types", "refinements", "full"],
        "default": "types",
        "description": "Verification depth. Use 'types' for fast iteration, 'refinements' for final validation."
      }
    }
  }
}
```

**Response:** The raw JSON output from `blc check --json`, unchanged. The compiler's output is already designed for machine consumption.

**Agent workflow:** Generate code -> call `baseline/check` -> read `suggestions` array -> apply highest-confidence fix -> repeat.

#### `baseline/test`

Runs `blc test --json` on a file. Returns pass/fail results for inline `@test` blocks.

```json
{
  "name": "baseline/test",
  "description": "Run inline @test blocks in a Baseline file. Returns structured pass/fail results.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "file": {
        "type": "string",
        "description": "Path to .bl file containing @test blocks"
      }
    },
    "required": ["file"]
  }
}
```

**Response:** The raw JSON output from `blc test --json`.

#### `baseline/docs`

Queries the standard library documentation. Supports lookup by name and keyword search.

```json
{
  "name": "baseline/docs",
  "description": "Look up Baseline standard library functions. Search by exact name (e.g. 'List.map') or keyword (e.g. 'filter').",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": {
        "type": "string",
        "description": "Function name (e.g. 'List.map') or search keyword (e.g. 'sort')"
      },
      "module": {
        "type": "string",
        "description": "Optional: restrict search to a specific module (e.g. 'List', 'String', 'Http')"
      }
    },
    "required": ["query"]
  }
}
```

**Response:** JSON from `blc docs --json`. Includes signature, description, effects, prelude level, and examples.

#### `baseline/reference`

Returns the `llms.txt` content. Allows agents to pull the language reference on demand rather than requiring it pre-loaded in context.

```json
{
  "name": "baseline/reference",
  "description": "Get the Baseline language quick reference. Call this before writing Baseline code for the first time in a session.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "section": {
        "type": "string",
        "enum": ["full", "syntax", "types", "effects", "patterns", "not_supported", "rosetta"],
        "default": "full",
        "description": "Which section of the reference to return. Use 'rosetta' for Python-to-Baseline mappings."
      }
    }
  }
}
```

**Response:** The relevant section of `llms.txt` as plain text.

#### `baseline/rosetta`

Given a programming concept or a code snippet in a known language, returns the Baseline equivalent.

```json
{
  "name": "baseline/rosetta",
  "description": "Translate a programming concept or code pattern to Baseline. Accepts a concept name or a code snippet in Python/TypeScript/Rust.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "concept": {
        "type": "string",
        "description": "A programming concept (e.g. 'error handling', 'list comprehension', 'http server', 'pattern matching', 'nullable value')"
      },
      "snippet": {
        "type": "string",
        "description": "A code snippet in another language to translate to Baseline"
      },
      "source_language": {
        "type": "string",
        "default": "python",
        "description": "Language of the input snippet"
      }
    }
  }
}
```

**Implementation:** This tool maintains a static lookup table of ~80-100 concept-to-pattern mappings compiled into the binary. Each entry contains:

```rust
struct RosettaEntry {
    concept: &'static str,        // "error_handling"
    keywords: &'static [&str],    // ["try", "catch", "exception", "error"]
    python: &'static str,         // "try:\n  val = risky()\nexcept:\n  ..."
    typescript: &'static str,     // "try { ... } catch (e) { ... }"
    baseline: &'static str,       // "match risky() {\n  Ok(v) -> ...\n  Err(e) -> ...\n}"
    explanation: &'static str,    // "Baseline uses Result<T,E> instead of exceptions..."
}
```

Matching is fuzzy: the tool searches `concept`, `keywords`, and the source snippet against all entries and returns the top 3 matches. This is lightweight (no LLM needed, no network calls) and deterministic.

**Fallback:** If no match is found, return a structured response saying "No direct mapping found" along with the Rosetta Stone section from `llms.txt` so the agent can reason about the translation itself.

### MCP Resources

In addition to tools, the MCP server should expose one resource:

```json
{
  "uri": "baseline://reference/llms.txt",
  "name": "Baseline Language Reference",
  "description": "Complete language reference for Baseline. Include in context when writing Baseline code.",
  "mimeType": "text/markdown"
}
```

This allows agents that support MCP resources (as opposed to only tools) to automatically load the reference into context at session start.

### Configuration

Users add the MCP server to their agent's config:

**Claude Code** (`~/.claude/settings.json` or project `.mcp.json`):
```json
{
  "mcpServers": {
    "baseline": {
      "command": "blc",
      "args": ["mcp"]
    }
  }
}
```

**Cursor** (`.cursor/mcp.json`):
```json
{
  "mcpServers": {
    "baseline": {
      "command": "blc",
      "args": ["mcp"]
    }
  }
}
```

**VS Code / Copilot** (`.vscode/mcp.json`):
```json
{
  "servers": {
    "baseline": {
      "type": "stdio",
      "command": "blc",
      "args": ["mcp"]
    }
  }
}
```

The `blc init` scaffolding command should generate the appropriate config file based on detected tooling, or all of them by default.

### Implementation Notes

- **Transport:** stdio (JSON-RPC over stdin/stdout). This is the most widely supported MCP transport and requires no port management.
- **Stateless tools:** `check`, `test`, `docs`, `reference`, and `rosetta` are all stateless request-response. No session management needed.
- **No CGP over MCP (yet):** The Constrained Generation Protocol requires token-level integration that current coding agents don't support. CGP remains available via `blc cgp` for custom toolchains but is out of scope for the MCP server in v1.
- **Binary size:** The Rosetta lookup table and `llms.txt` content are compiled into the binary. Estimated overhead: <100KB.

---

## Layer 3: Agent Instruction Files

**Effort:** Small (hours)
**Reach:** Agent-specific, high impact for each
**Status:** New

### Overview

Ship template instruction files that `blc init` generates in the project root. These files tell each agent how to use Baseline, where to find documentation, and that the MCP server is available.

### Files

#### `CLAUDE.md` (Claude Code)

```markdown
# Baseline Project

This project uses the Baseline programming language (.bl files).

## Language Reference

Read the llms.txt file in this directory for the complete language
quick reference before writing or modifying Baseline code.

## Key Rules

- Use fn name(args) -> Type = expr for functions
- Effectful functions end with ! (e.g. Console.println!("hello"))
- No classes, no null, no try/catch, no async/await, no semicolons
- Use Result<T, E> with ? for error handling
- Use Option<T> instead of null
- Use Module.function(value) not value.method()
- Use not for boolean negation, never !
- String concatenation: "${a}${b}" not a + b

## Tools

The baseline MCP server is configured for this project.
Use baseline/check to validate code after writing it.
Use baseline/docs to look up standard library functions.
Use baseline/test to run inline tests.
Use baseline/rosetta to translate patterns from other languages.

## Workflow

1. Call baseline/reference if this is your first time writing Baseline in this session
2. Write code
3. Call baseline/check with level "types" for fast feedback
4. Fix errors using the suggestions in the diagnostic output
5. Write @test blocks alongside implementations
6. Call baseline/test to verify
7. Call baseline/check with level "refinements" for final validation
```

#### `.github/copilot-instructions.md` (GitHub Copilot)

```markdown
This project uses the Baseline programming language. Files use the .bl extension.

Baseline is a strongly-typed, effect-tracked functional language. See llms.txt
in the project root for the complete syntax reference.

Critical rules:
- Functions: fn name(a: Type) -> ReturnType = expression
- Effects use ! suffix: Console.println!("hello")
- No classes, null, try/catch, async/await, semicolons, or mutable variables
- Error handling: Result<T, E> with ? propagation
- Null safety: Option<T> with Some/None
- Function calls: Module.function(value), not value.method()
- Boolean negation: not x, never !x
- String concat: "${a}${b}", never "a" + "b"
- Last expression is the return value (no return keyword)
```

#### `.cursor/rules` (Cursor)

Same content as the Copilot instructions. Cursor reads rules from `.cursor/rules` or `.cursorrules`.

#### `.ai-instructions.md` (Generic Fallback)

Same content as Copilot instructions with an additional note:

```markdown
If you have access to an MCP server named "baseline", use the baseline/check
tool to validate all generated Baseline code before presenting it.
```

### Scaffolding

`blc init` generates these files. The command detects which agents are likely in use:

- If `.claude/` exists or `CLAUDE.md` exists -> generate `CLAUDE.md`
- If `.github/` exists -> generate `.github/copilot-instructions.md`
- If `.cursor/` exists -> generate `.cursor/rules`
- Always generate `.ai-instructions.md` as fallback
- Always generate `.mcp.json` with the baseline server config
- Always copy `llms.txt` to project root

Users can opt out with `blc init --no-ai`.

---

## Layer 4: Context7 Entry

**Effort:** Minimal (hours)
**Reach:** Cursor users, any tool with Context7 integration
**Status:** New

### Overview

Create a Context7 entry for Baseline that serves the `llms.txt` content (and optionally the full stdlib reference) via the Context7 API. This allows agents with Context7 integration to pull Baseline docs on demand.

### Implementation

1. Register `baseline-lang` on Context7
2. Publish `llms.txt` as the primary context document
3. Publish `llms-full.txt` (extended stdlib reference) as supplementary
4. Set up CI to update the Context7 entry on each compiler release

### Content Structure

```
baseline-lang/
  llms.txt          <- quick reference (what we have now + rosetta)
  llms-full.txt     <- full stdlib docs (generated from blc docs --json)
  examples/         <- categorized code examples
    http-server.bl
    data-processing.bl
    error-handling.bl
    testing.bl
```

---

## What We Are NOT Building

### Fine-Tuned Models

The research is clear: agentic loops with compiler feedback outperform fine-tuning for unseen languages (96% vs. variable results). Fine-tuning also creates a maintenance burden -- every new model release requires re-training. Our strategy improves automatically as base models get better at tool use and instruction following.

### CGP Integration for External Agents

The Constrained Generation Protocol (`blc cgp`) provides token-level type-constrained guidance. Current coding agents (Claude Code, Copilot, Codex) do not support plugging in external token constraints during inference. CGP remains available for custom inference pipelines and a future Baseline IDE plugin, but is out of scope for this RFC.

### Custom IDE/Editor Plugin (v1)

A full LSP + inline completion plugin is valuable but is a separate effort. The MCP server provides the bridge for now. An LSP server is a natural follow-on that can reuse the same compiler interfaces.

---

## Implementation Plan

### Phase 1: Quick Wins (Week 1)

| Task | Deliverable | Effort |
|------|------------|--------|
| Add Rosetta Stone section to llms.txt | Updated llms.txt | 2 hours |
| Add Common Mistakes section to llms.txt | Updated llms.txt | 1 hour |
| Write agent instruction file templates | CLAUDE.md, copilot-instructions.md, .cursorrules, .ai-instructions.md | 3 hours |
| Add --no-ai flag discussion to blc init design | Decision documented | 1 hour |

### Phase 2: MCP Server (Weeks 2-3)

| Task | Deliverable | Effort |
|------|------------|--------|
| Implement blc mcp subcommand with stdio transport | Binary with MCP server | 3 days |
| Implement baseline/check tool | Tool wrapping blc check --json | 0.5 days |
| Implement baseline/test tool | Tool wrapping blc test --json | 0.5 days |
| Implement baseline/docs tool | Tool wrapping blc docs --json | 0.5 days |
| Implement baseline/reference tool | Tool returning embedded llms.txt | 0.5 days |
| Build Rosetta lookup table | ~80 concept-to-pattern entries | 2 days |
| Implement baseline/rosetta tool | Tool with fuzzy concept matching | 1 day |
| Generate MCP config files in blc init | .mcp.json for each agent | 0.5 days |
| Write MCP server tests | Test suite | 1 day |

### Phase 3: Distribution (Week 4)

| Task | Deliverable | Effort |
|------|------------|--------|
| Register Context7 entry | Live Context7 listing | 2 hours |
| Host llms.txt and llms-full.txt at baseline-lang.dev | Hosted docs | 2 hours |
| CI pipeline to update Context7 on release | GitHub Action | 3 hours |
| Update blc init to scaffold all agent files | Updated init command | 4 hours |
| Write "Baseline for AI Agents" guide | User-facing docs page | 4 hours |

---

## Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| First-attempt compilation rate (with MCP) | >70% | Automated benchmark: 50 tasks, measure blc check pass rate on first generation |
| Compilation rate after repair loop (with MCP) | >95% | Same benchmark, allow 5 repair iterations |
| Agent setup time for new users | <5 minutes | blc init to first successful baseline/check call |
| Context window overhead of llms.txt | <4K tokens | Measure with tiktoken |

### Benchmark Suite

Create a set of 50 coding tasks spanning:

- Pure functions (arithmetic, string manipulation, list processing)
- Effect-tracking (console output, HTTP handlers)
- Error handling (Result propagation, Option matching)
- Record types and sum types
- Pipe chains and data transformation
- Inline test writing

Run each task against Claude Code, Copilot, and Codex with and without the MCP server. Measure compilation success rate, test pass rate, and iterations to success.

---

## Open Questions

1. **Rosetta table maintenance:** Should the Rosetta entries be compiled into the binary or loaded from a file at runtime? Compiled-in is simpler and has no failure modes. File-based allows community contributions without recompiling.

2. **MCP server discovery:** Should `blc init` auto-detect the user's agent and only generate the relevant config, or generate all configs unconditionally? Generating all is safer but noisier.

3. **llms.txt versioning:** When the language evolves, should llms.txt be versioned alongside the compiler? Likely yes -- it should be generated or validated by CI to prevent drift.

4. **Prompt caching:** Claude Code supports prompt caching for system prompts. Should the MCP server set cache-control headers on the baseline/reference response to hint at cacheability?

5. **Telemetry:** Should the MCP server optionally report anonymized usage data (which tools are called most, common error codes) to inform future llms.txt improvements? Only with explicit opt-in.

---

## References

- [Teaching LLMs New Programming Languages](./Teaching_LLMs_New_Programming_Languages.pdf) -- Research synthesis on ICL, RAG, GCD, and agentic loops for unseen languages
- [Baseline AI Agent Tooling Guide](./AI_Agent_Tooling_Guide___Baseline.pdf) -- Existing blc compiler capabilities for AI agents
- [MCP Specification](https://modelcontextprotocol.io/) -- Model Context Protocol spec
- [Context7](https://context7.com/) -- Hosted documentation context for AI agents
- [CompileAgent](https://aclanthology.org/2025.acl-long.103.pdf) -- 25% to 96% compilation success via agentic loops
- [XPL Study](https://openreview.net/forum?id=1PRBHKgQVM) -- Cross-lingual transfer in programming languages
