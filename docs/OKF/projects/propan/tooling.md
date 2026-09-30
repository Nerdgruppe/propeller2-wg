---
type: "Reference"
title: "Propan command-line and output behavior"
description: "Current source-file loading, multi-file overlay, output formats, fill behavior, list files, diagnostics, and CLI conventions."
tags: ["propan", "cli", "output", "json", "list-file", "multi-file"]
status: "draft"
source_confidence: "high"
---
# Propan command-line and output behavior

This page documents the current command-line assembler behavior implemented by `src/propan/propan.zig`, `emit.zig`, and `listfile.zig`.

## Input files

The CLI accepts one or more positional source paths. Each input is read, parsed, and semantically analyzed as an independent module. `-` may be used as an input path to read source from standard input.

The CLI does not enforce a filename extension. `.propan` is the repository's normal extension for current Propan source, but extension acceptance is not a language rule in the current command-line implementation.

There is no current source-level include/import facility in the parser/semantic pipeline. Multi-file assembly is performed by passing multiple source files to the CLI, not by including one source file from another.

Each module has its own user symbol/constant namespace. Symbols from one positional input are not imported into another module for expression or instruction resolution.

## Multi-file assembly and overlay behavior

Inputs are analyzed in command-line order. After each module is produced, every emitted segment is copied into one shared flat output buffer at its HUB offset.

The merge algorithm is currently mechanical:

1. grow the shared output to cover the segment end;
2. fill newly grown bytes with `--fill-byte`;
3. copy the segment bytes into `output[hub_offset..]`.

There is currently no cross-module overlap rejection in this path. If a later input emits bytes over a range already written by an earlier input, the later segment overwrites those bytes in the flat output. This is current behavior, not a guarantee that overlapping modules are intended as a stable linking model.

Within the JSON and list-file views, modules and their segments remain individually represented even though flat output bytes may overlap.

## Output selection

`--format` selects one of the implementation enum values:

| Format | Behavior |
|---|---|
| `flat` | writes the merged flat byte buffer |
| `json` | writes structured module/segment/symbol/line metadata with segment data base64-encoded |
| `none` | performs assembly without emitting the main output |

`flat` is the default format.

For binary/flat output, omitting `-o` is a usage error because the tool refuses to emit binary data to the terminal implicitly. Use `-o -` to force flat output to standard output. JSON is not classified as binary and may be written to standard output when no output path is supplied.

When an output path is supplied, the implementation uses atomic replacement for the main output file.

## `--fill-byte`

`--fill-byte` (short form `-F`) sets the byte used when the shared flat output buffer grows across previously undefined address space. The default is `0x00`.

It does not pre-fill a fixed address space. Bytes are added only as needed to reach the end of an emitted segment. Segment data then overwrites the corresponding range.

If a later segment starts beyond the current end, the gap and newly allocated range are initialized with the fill byte before the segment is copied.

## JSON format

The JSON document contains:

- `total_size`: length of the merged flat output buffer;
- `segments`: all module segments;
- `symbols`: all exported code/data symbols;
- `line_map`: source locations for emitted ranges.

Each segment contains:

- a numeric `id`;
- HUB `offset`;
- byte `size`;
- base64-encoded `data`;
- execution `mode` (`hub`, `cog`, or `lut`).

Segment IDs are local to semantic modules, so JSON emission rebases IDs between modules to keep them distinct in the combined document.

Each symbol records its name, type, rebased segment ID, HUB offset, execution mode, and mode-specific jump/local address. User constants are not emitted in the JSON symbol list.

The line map records emitted HUB offset/size plus source file, line, and column.

## List files

`--list-file <path>` writes a human-readable listing after all modules have been analyzed. `--list-file -` writes it to standard output. A file path is written through atomic replacement.

The list has three layers:

1. a `symbols` table;
2. a `segments` table;
3. one detailed body table per segment.

### Symbol table

Labels show:

- execution kind (`hub`, `cog`, or `lut`);
- HUB address;
- local PC for COG/LUT symbols;
- symbol name;
- original source line.

User constants appear as `const` rows with their evaluated value and source line. HUB/local address columns are placeholders for constants.

### Segment table

Each segment receives a display index across all input modules and reports execution mode, HUB start, and byte size.

### Segment bodies

Each body row reports:

- HUB address;
- COG/LUT local PC where applicable;
- emitted bytes for the source line;
- original source text.

Zero-length label rows are associated with a segment by semantic segment identity; emitted rows are associated by HUB range.

The list file is therefore the most direct user-facing view for correlating source lines with HUB addresses, local COG/LUT PCs, symbols, constants, and emitted bytes.

## Diagnostics and exit status

Parser failures and semantic errors cause the CLI to return exit status `1`. Usage errors such as missing input files or attempting implicit flat-binary output to the terminal also return `1`.

Warnings are rendered through the diagnostic collection but do not by themselves make assembly fail. In internal test mode, warning/info rendering is suppressed.

Some internal/runtime failures remain ordinary Zig errors rather than source diagnostics; documented implementation findings should not be interpreted as guaranteed exit-status behavior for panic paths.

## Other useful CLI options

| Option | Purpose |
|---|---|
| `-o`, `--output` | main output path; `-` means standard output |
| `-f`, `--format` | `flat`, `json`, or `none` |
| `-F`, `--fill-byte` | fill byte for newly created gaps/ranges |
| `--list-file` | write the human-readable listing; `-` means stdout |
| `--render-stdlib-docs` | render P2 standard-library metadata as HTML; `-` means stdout |
| `-v`, `--verbose` | enable debug logging |
| `-h`, `--help` | print CLI help |

`--test-mode` and `--compare-to` are marked internal-use-only in the CLI metadata and are not normal user-facing assembly options.

## Current overlap limitation

The assembler's current output-composition loop does not reject overlapping HUB ranges between modules or segments at this stage. Later writes win in flat output. Documentation and tests should distinguish this observed behavior from any intended future segment-validation/linking policy.
