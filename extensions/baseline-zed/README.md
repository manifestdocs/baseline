# Baseline Language Extension for Zed

Syntax highlighting and language support for the [Baseline programming language](https://github.com/manifestdocs/baseline).

## Features

- **Full syntax highlighting** for `.bl` files
  - All language keywords and operators
  - String interpolation, raw strings, multiline strings
  - Effect identifiers (`println!`, `read!`)
  - Type annotations and generic types
- **BDD Testing support**
  - `describe` / `context` blocks
  - `it` / `it.only` / `it.skip` blocks
  - `before_each` / `after_each` hooks
  - `expect` expressions with all matchers (`to_equal`, `to_be`, `to_contain`, `to_have_length`, `to_be_empty`, `to_start_with`, `to_satisfy`, `to_be_ok`, `to_be_some`, `to_be_none`)
  - `test` inline tests (including in `where` blocks)
- **Spec/Contract attributes**
  - `@spec`, `@given`, `@returns`, `@requires`, `@ensures`, `@assume`
  - `@pure`, `@total`
- **Standard library awareness**
  - Prelude levels: `minimal`, `pure`, `core`, `script`, `server`
  - Module highlighting for Option, Result, String, List, Map, Set, Json, Math, Console, Log, Time, Random, Env, Fs, Http, Response, Request, Router, Server, Db, Metrics
- **Effect system highlighting**
  - Effect definitions and effect sets
  - `handle`/`with` expressions and handler clauses
- **Code outline** — navigate functions, types, effects, modules, describe/it blocks, specs, and inline tests
- **Bracket matching** — including lambdas `|...|` and map literals `#{...}`
- **Auto-indentation** — describe blocks, handler maps, match arms
- **Comment toggling** (`//`, `/* */`)

## Installation

### From Zed Extensions (when published)

1. Open Zed
2. Open the extensions panel (`cmd+shift+x`)
3. Search for "Baseline"
4. Click Install

### Local Development

1. Build the tree-sitter grammar WASM:

```bash
cd tree-sitter-baseline
npm install
tree-sitter build --wasm
```

2. Link the extension to Zed's dev extensions:

```bash
ln -s /path/to/baseline/extensions/baseline-zed ~/Library/Application\ Support/Zed/extensions/installed/baseline
```

3. Restart Zed or run "zed: reload extensions" from the command palette

## File Associations

- `.bl` (primary)

## Configuration

The extension uses 2-space soft tabs by default. You can override this in your Zed settings.

## Future Plans

- LSP integration (when Baseline compiler LSP is ready)
- Inline diagnostics
- Code actions
- Semantic token support
