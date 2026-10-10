# Propan Assembler

The desktop extension starts `propan-lsp` over stdio for `.propan` and `.p2asm`
files. It supports hover, definitions, semantic highlighting, completion,
formatting, label CodeLens, and address inlay hints. The browser extension provides grammar support.

Build the server from the repository root with `zig-0.16.0 build install`.
In this directory, run `npm install` and `npm run compile`. Copy
`launch.example.json` to `.vscode/launch.json`, open this directory in VS Code,
and launch the **Run Desktop Extension** debug configuration.

Configure the native tools in VS Code settings, for example when opening the
repository root as your workspace:

```json
{
  "propeller2.propanLsp.path": "./zig-out/bin/propan-lsp",
  "propeller2.propanLsp.arguments": [],
  "propeller2.propan.path": "./zig-out/bin/propan",
  "propeller2.propan.arguments": [],
  "propeller2.p2aas.endpoint": "ws://127.0.0.1:12880/?baudrate=115200&timeout_ms=5000"
}
```

Executable settings accept absolute paths, paths relative to the selected file's
workspace folder, or bare commands on `PATH`. The LSP defaults to `propan-lsp` on
`PATH`; changing its path or arguments restarts it automatically. The LSP uses the
active editor's workspace settings when starting (or the first workspace folder).
An empty assembler path uses `propan` beside a configured LSP path, or on `PATH`.
Use assembler arguments for options such as `["-I", "include"]`.

For `.propan` and `.p2asm` files, **Propan: Check** and **Propan: Run** appear in
the editor toolbar, editor and Explorer context menus, and Command Palette.
Explorer actions use the clicked file; other actions use the current file.
Actions save edited files before assembling. Check assembles without writing a
binary and updates the Problems panel, including diagnostics for imported files.
Run assembles a flat binary, updates diagnostics, and uploads only if assembly
succeeds. Each action opens a tool terminal; Run streams the board's UTF-8 output
there. Closing an active terminal cancels assembly or disconnects from P2AAS.
Successful actions leave the terminal open so you can read the output.
One action runs at a time; the Problems panel shows the latest assembly.

P2AAS endpoints must use `ws://` or `wss://`. Optional `baudrate` and `timeout_ms`
query parameters default to 115200 and 5000; other query parameters are preserved.
Images are padded to a long boundary and sent with a little-endian 32-bit length
prefix, matching P2AAS. Native tools and these actions require a trusted desktop
workspace; browser hosts provide syntax highlighting only. A missing LSP does
not prevent Check or Run when the assembler is available.

Run the desktop action regression checks with `npm run test-desktop`.

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
