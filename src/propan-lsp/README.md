# Propan language server

Build from the repository root with `zig-0.16.0 build install`. Run
`zig-out/bin/propan-lsp` with an LSP client over stdin/stdout. The binary uses
Propan directly for parsing, assembly metadata, builtin documentation, and
formatting; lsp-kit provides the protocol and transport.

Supported requests:

- Hover: assembler directives and declarations (syntax and behavior), mnemonics
  (placeholder documentation), builtin functions and arguments,
  constant values/types, and label usage/addresses/reference counts.
  Value hovers and function defaults show `type`, `usage`, and `value`. Integers
  include decimal and signed hexadecimal on separate lines; strings use JSON
  escapes; sequences use JSON arrays; registers include their number and special
  name where available. Addresses show hub/local addresses and any byte offset
  within a long. Enumerators use `#name`, and pointer expressions use assembly
  syntax. Evaluated instruction operands and constant expressions also support
  value hover, while function and parameter documentation retains precedence.
- Go to definition: constants and code/variable labels, including scoped locals.
- Semantic tokens: distinct mnemonic, code label, variable label, and constant
  types; the VS Code extension maps them to theme scopes.
- Document formatting: Propan's canonical formatter, also usable with format on save.
- Completion: directives, mnemonics, conditions, permitted instruction effects,
  symbols in the current scope, builtin functions, and unfilled named parameters.
  Directive suggestions include documentation. Effects such as `:wc` and `:wz`
  are offered after complete operands and when typing a colon prefix.
- Label CodeLens: hub address, PC/local address and reference count.
- Inlay hints: a listing prefix (`$00000 | 000 | `) at the start of every line,
  including comments and empty lines. Hub addresses are hexadecimal byte offsets;
  local PCs are hexadecimal long addresses (LUT PCs start at `$200`). Hub/data
  segments leave the local field blank; register-space labels leave the hub field
  blank. Lines without an assembled address, such as comments and constants, have
  both fields blank (`       |     | `) to keep source text aligned. If assembly
  fails, all lines retain blank prefixes. Hints honor the requested document range.

The server synchronizes open documents with full or incremental updates and
negotiates UTF-8, UTF-16, or UTF-32 positions. Invalid syntax is recovered by
blanking up to 32 faulty physical lines for navigation and completion. Formatting
is withheld on invalid syntax. Evaluated values and addresses require successful
assembly; unavailable addresses are shown explicitly rather than estimated.
Analysis currently covers each open document independently; `.import` files and
external `FILE` payloads are not loaded. Reference counts cover the current file.

Run the protocol regression checks after building:

```sh
python3 tests/propan-lsp/test_protocol.py
```

These checks exercise the installed server, including its stdio framing,
completion contexts, local scopes, formatting, incremental edits, and Unicode
positions. `zig-0.16.0 build install test` runs the Propan suites, including the
shared metadata fixture and unfinished-string tokenizer regression.

The [VS Code extension](../../vscode-extension/README.md) includes a desktop
client with a hardcoded executable path.
