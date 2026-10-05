# Propan Assembler

The desktop extension starts `propan-lsp` over stdio for `.propan` and `.p2asm`
files. It supports hover, definitions, semantic highlighting, completion,
formatting, label CodeLens, and address inlay hints. The browser extension provides grammar support.

Build the server from the repository root with `zig-0.16.0 build install`.
In this directory, run `npm install` and `npm run compile`. Copy
`launch.example.json` to `.vscode/launch.json`, open this directory in VS Code,
and launch the **Run Desktop Extension** debug configuration.

The server executable path is currently hardcoded in
`src/desktop/extension.ts` to
`/home/felix/projects/nerdgruppe/propeller2-wg/zig-out/bin/propan-lsp`.
Change that path if the repository lives elsewhere.

Use **Format Document**, or enable VS Code's `editor.formatOnSave` for Propan.
Use **Propan: Restart LSP** in the Command Palette after rebuilding the server
or if it has stopped following a crash.

Hover over directives (including `.cogexec`, `.org`, `.pack`, and `LONG`) or
`const`/`var` to see their syntax and behavior. Directive completion suggestions
include the same documentation. Effect completion offers the instruction's
permitted suffixes, such as `:wc`, `:wz`, and `:wcz`, after complete operands or
while typing `:` / `:w`.

Address inlay hints appear before source lines as `$00000 | 000 | ` (hub byte
address, then local PC in longs; both hexadecimal). The PC is blank for hub/data
segments. Register-space labels have a blank hub address. Every line receives a
prefix; comments, empty lines, constants, and directives without emitted data use
blank fields (`       |     | `) so the source stays aligned. Addresses require
successful assembly; otherwise every line has blank fields.
Enable them with `"[propan]": { "editor.inlayHints.enabled": "on" }`
if your editor settings disable inlay hints.

The language server supplies semantic categories; your theme supplies their colors.
Currently it emits these categories for definitions and references:

| Token type | Meaning |
| --- | --- |
| `propanMnemonic` | Instructions and directives |
| `propanCodeLabel` | Code labels |
| `propanVarLabel` | Variable labels |
| `propanConstant` | Constants |
| `function` | Builtin functions, including intrinsics |

Symbol definitions also carry the `declaration` modifier; constants carry
`readonly`. The `parameter` type is in the protocol legend but is not emitted yet.
Custom token types have theme fallback scopes, so themes can assign the same color
to multiple categories. To distinguish them explicitly, add rules to VS Code's
`settings.json`, for example:

```json
"editor.semanticTokenColorCustomizations": {
  "enabled": true,
  "rules": {
    "propanMnemonic:propan": "#569CD6",
    "propanCodeLabel:propan": "#DCDCAA",
    "propanVarLabel:propan": "#9CDCFE",
    "propanConstant:propan": "#B5CEA8",
    "function:propan": "#C586C0",
    "*.declaration:propan": { "bold": true }
  }
}
```

Colors are arbitrary RGB foreground colors, with optional bold, italic, and
underline styles. Additional token categories and modifiers can be added to the
server and extension; there is no fixed semantic color palette. For example,
directives could be separated from instructions, and registers, conditions,
effects, and parameter names could each have their own category. Ordinary syntax
highlighting still covers strings, numbers, and comments.

See the [VS Code semantic highlighting guide](https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide).
**Developer: Inspect Editor Tokens and Scopes** shows the category and active
color rule under the cursor. Inlay hints use the separate theme colors
`editorInlayHint.foreground` and `editorInlayHint.background`.
