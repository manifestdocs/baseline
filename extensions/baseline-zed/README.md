# Baseline Language Extension for Zed

Syntax highlighting and language support for the [Baseline programming language](https://github.com/baseline-lang/baseline).

## Features

- Syntax highlighting for `.bl` files
- Bracket matching
- Auto-indentation
- Code outline (symbols)
- Comment toggling (`//`, `/* */`)

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
ln -s /path/to/baseline-lang/extensions/baseline-zed ~/Library/Application\ Support/Zed/extensions/installed/baseline
```

3. Restart Zed or run "zed: reload extensions" from the command palette

## File Associations

- `.bl` (primary)

## Configuration

The extension uses 2-space soft tabs by default. You can override this in your Zed settings.

## Future Plans

- LSP integration (when Baseline compiler API is ready)
- Inline diagnostics
- Code actions
