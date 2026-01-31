# Rocket Language Extension for Zed

Syntax highlighting and language support for the [Rocket programming language](https://github.com/rocket-lang/rocket-lang).

## Features

- Syntax highlighting for `.rk` and `.rocket` files
- Bracket matching
- Auto-indentation
- Code outline (symbols)
- Comment toggling (`//`, `/* */`)

## Installation

### From Zed Extensions (when published)

1. Open Zed
2. Open the extensions panel (`cmd+shift+x`)
3. Search for "Rocket"
4. Click Install

### Local Development

1. Build the tree-sitter grammar WASM:

```bash
cd crates/tree-sitter-rocket
npm install
tree-sitter build --wasm
```

2. Link the extension to Zed's dev extensions:

```bash
ln -s /path/to/rocket-lang/editors/zed-rocket ~/Library/Application\ Support/Zed/extensions/installed/rocket
```

3. Restart Zed or run "zed: reload extensions" from the command palette

## File Associations

- `.rk` (primary)
- `.rocket` (also supported)

## Configuration

The extension uses 2-space soft tabs by default. You can override this in your Zed settings.

## Future Plans

- LSP integration (when Rocket compiler API is ready)
- Inline diagnostics
- Code actions
