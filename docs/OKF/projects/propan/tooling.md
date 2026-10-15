---
type: "Reference"
title: "Propan command-line and output behavior"
description: "Current single-root assembly, source imports, CLI options, output formats, diagnostics, and listings."
tags: ["propan", "cli", "imports", "output", "json", "spin2", "list-file"]
status: "draft"
source_confidence: "high"
---
# Propan command-line and output behavior

This reference describes the implementation merged from `main` on 2026-10-15. Its primary sources are `src/propan/propan.zig`, `src/propan/frontend/imports.zig`, `src/propan/emit/` and `src/propan/test_suite.zig`. It supersedes the earlier multi-root/overlay description.

## Input: one root source

The assembler currently accepts **exactly one positional input file**. `-` reads the root source from standard input. Multiple positional files produce `error: multiple input files are not supported` before either source is analyzed; they are not independent modules overlaid together.

There is no enforced filename extension. `.propan` is conventional, not mandatory.

To assemble code split over files, use source-level `.import` directives. All imported source is expanded into the root's instruction/definition sequence and analyzed as **one module**, not separately linked modules. Symbol definitions and references can therefore cross import boundaries. The original imported path is retained in diagnostics and emitted source metadata.

### Imports

`.import "relative/path.propan"` inserts another Propan source at that point. The mnemonic is matched case-insensitively. The argument must be **one string literal**, with no condition or effect.

`.import once` is an in-file marker: the containing file is expanded at most once during the entire root expansion. The marker emits nothing and is not a path import. It is detected before descending into that file's imports, allowing it to stop otherwise recursive import-once graphs. Re-importing a file without this marker can repeat its contents; active import cycles are rejected with a diagnostic.

For each relative path, lookup checks the **containing source file's directory first**, then `-I` / `--include-path` directories in command-line order. The option is repeatable, including `--include-path=directory`. The expander uses canonical filesystem identities for import-once tracking so differently spelled paths to the same file do not bypass it. Absolute imported paths are resolved directly. Missing/unreadable files and cycles are errors.

For example:

```propan
// root.propan
.import "common.propan"
.hubexec
entry:
    NOP
```

```propan
// common.propan
.import once
const BUFFER_SIZE = 64
```

`-I dir` is an import search-path option; it does not make the assembler accept multiple roots. The root file's identity/path matters when resolving relative imports, including when supplying source through standard input.

**Evidence:** `frontend/imports.zig`; `test_suite.zig` tests for containing-directory precedence, include-path order, canonical import-once identity, imported diagnostics, imported listing labels and JSON paths; `tests/propan/sema/import-basic.propan`.

## Output formats

`--format` (`-f`) selects:

| Format | Output |
|---|---|
| `flat` (default) | contiguous assembled HUB bytes |
| `json` | structured segments, symbols, and source-range metadata |
| `spin2` | generated human-readable Spin2 `CON`/`DAT` listing of the assembled image |
| `none` | analyze without writing the main assembly output |

Only `flat` is marked binary by the implementation. Without `-o`, implicit binary output to the terminal is rejected. Use `-o -` explicitly to send flat bytes to standard output. Text formats can be emitted to standard output with no output filename. Output files are atomically replaced by the CLI.

The `spin2` emitter attempts readable PASM2 instruction spellings where the source/encoding metadata supports them, falling back to byte/word/long data for other regions. It preserves assembled padding, emits constants and usable symbol names, and carries through selected source comments. This is an **export of the assembled image**, not a guarantee of a lossless reconstruction of the original Propan source. The test suite includes a FlexSpin byte-for-byte round-trip check for semantic fixtures when its external tool configuration is available.

### Flat layout and fill

The semantic analyzer produces segments; the CLI copies their bytes into one flat output image using their absolute HUB offsets. It grows the buffer to accommodate the last emitted address, fills newly allocated gaps with `--fill-byte`, and copies each segment in order. Default fill is `0x00`; `-F` is the short option.

The image-composition code does not currently enforce non-overlap: later segment writes can overwrite existing ranges. This is a **single-module segment overlap risk**, not a multi-positional-file overlay feature. Padding during segment emission also uses the configured fill byte (see `src/propan/sema.zig` and the `fill-byte.propan` regression).

### JSON metadata

JSON includes `total_size`, `segments`, `symbols` and `line_map`. Segments contain HUB offset, byte size, encoded data, identity and execution mode. Symbols contain name, kind, segment and address information, with source locations retained for imported files. The mode model now also includes `data` and `regspace` in addition to COG/LUT/HUB; not every mode has an executable PC. Source locations in the line map refer to the actual containing source file.

The separate-module segment-ID rebasing described in the pre-merge OKF no longer applies to the single-root CLI path.

## List files

`--list-file <path>` produces the human-readable symbol, segment and per-segment source/byte tables. `--list-file -` writes to standard output. The listing correlates HUB offsets, execution-local PCs when applicable, labels/constants and original source lines. Imported paths and scoped local-label debug names can appear in these views; see `test_suite.zig` import/local-label tests.

## Diagnostics and exit status

Parser, import and semantic errors normally cause exit status `1`. CLI usage errors (including multiple roots or an unspecified flat-binary output destination) also return `1`. Warnings alone do not fail assembly. `--no-warnings` suppresses warnings in CLI rendering, without changing semantic acceptance. Some internal errors can still escape as Zig runtime errors and should not be misrepresented as stable language diagnostics.

## Other CLI options

| Option | Purpose |
|---|---|
| `-o`, `--output` | destination; `-` means standard output |
| `-f`, `--format` | `flat`, `json`, `spin2`, `none` |
| `-F`, `--fill-byte` | gap/padding byte |
| `-I`, `--include-path` | repeatable import search directory |
| `--list-file` | textual listing |
| `--pretty-print <path>` | parse and pretty-print a single source file (or `-` for stdin) to stdout; separate from normal semantic analysis and import expansion |
| `--no-warnings` | suppress warnings in CLI output |
| `--render-stdlib-docs` | render P2 standard-library metadata as HTML |
| `-v`, `--verbose` | debug logging |
| `-h`, `--help` | CLI usage |

`--test-mode` and `--compare-to` are internal testing options.

## Evidence boundaries

- **Implemented:** single-root CLI validation, recursive imports/import-once, option parsing, single-module image assembly, and `spin2` selection.
- **Tested:** rejection of multiple roots; import path precedence, identity, source metadata; fill-byte behavior; selected Spin2 readability and configured FlexSpin round-trip coverage.
- **Open:** this page does not claim every generated Spin2 construct round-trips under every external assembler or that overlapping segments are valid by design.

For syntax and data directives, see [/projects/propan/directives-and-data.md](/projects/propan/directives-and-data.md). For evidence/test roles, see [/references/propan-coverage.md](/references/propan-coverage.md).
