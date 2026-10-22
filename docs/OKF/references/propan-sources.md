---
type: "Source Inventory"
title: "Propan documentation sources"
description: "Inventory and role classification for sources used to document the current Propan assembler and language."
tags: ["propan", "sources", "provenance", "documentation"]
status: "draft"
source_confidence: "high"
---
# Propan documentation sources

This inventory records the main evidence used for the current Propan OKF documentation. See `/projects/propan/status-and-source-precedence.md` for precedence rules.

## Current implementation

### `src/propan/frontend/parser.zig`

Role: authoritative current lexical grammar and parser behavior.

Use for:

- tokens and keywords;
- identifier/literal syntax;
- comment syntax;
- expression precedence;
- source-line grammar;
- condition/effect parsing;
- function-call grammar;
- string/character unescaping and recovery behavior.

Caution: parsing an instruction-shaped line does not establish that semantic analysis recognizes the mnemonic.

### `src/propan/sema.zig`

Role: authoritative current semantic behavior for symbols, expressions, segments, directive/pseudo-instruction handling, instruction selection, assertions, and code layout.

Use for:

- supported assembler mnemonics/directives;
- symbol declaration/reference behavior;
- address-domain conversion;
- instruction selection;
- segment and cursor semantics;
- semantic diagnostics.

Caution: active-development TODOs and process panics exist; implementation presence is not equivalent to stability.

### `src/propan/stdlib/`

Role: authoritative definitions for current builtin/P2 constants, functions, and generated instruction entries loaded by semantic analysis.

Use for:

- builtin function signatures and behavior;
- standard constants;
- special register/configuration values;
- instruction operand/effect metadata used by Propan.

### `src/propan/propan.zig`, `frontend/imports.zig`, `emit/`, and `listfile.zig`

Role: authoritative current single-root CLI, recursive source imports, output-format, fill-byte, and list-file behavior.

Use for:

- single positional root validation and stdin input;
- `.import` expansion, `.import once`, and include search paths;
- flat/JSON/Spin2 output;
- list-file rendering;
- exit/error behavior;
- generated stdlib-reference entrypoint.

## Tests

### `tests/propan/parser/`

Role: grammar examples and parser acceptance tests.

Strong evidence for lexical/source grammar. Weak evidence for semantic support of a mnemonic or directive.

### `tests/propan/sema/`

Role: semantic behavior and regression evidence.

Use for:

- constants and expressions;
- label addressing;
- segment management;
- stdlib behavior;
- instruction selection;
- alignment and selected address/layout behavior.

The previously cited `tests/propan/sema/segment_management.propan` is **absent from the current branch**. It was historical evidence and must not be treated as a present-day passing fixture. Use current `tests/propan/sema/` layout and diagnostics fixtures plus `src/propan/sema.zig` instead.

### `tests/propan/regressions/`

Role: focused assembler/CLI regression evidence.

Use for output behavior, import handling, single-root rejection, fill byte behavior, JSON/Spin2 output, and fixed defects. The in-source `src/propan/test_suite.zig` now contains many relevant CLI, import, local-label, and Spin2-export tests.

### `tests/propan/equivalence/`

Role: comparison material against PASM2/FlexSpin encodings and syntax forms.

Use for instruction operand mapping, pointer/address forms, branch behavior, effects, augmentation, and PASM2-to-Propan comparisons. Individual files are not by themselves proof that every possible variant is exhaustively covered.

## Generated and canonical instruction data

### `data/encoding/`

Role: processed instruction metadata used by generation/tooling.

Relevant files include instruction TSV/YAML/JSON data, aliases, metadata, and smart-pin data.

### `utility/Parallax Propeller 2 Instructions v35 - Rev B_C Silicon - Sheet1.tsv`

Role: repository-pinned official instruction source for exact PASM2 syntax/encoding comparison.

Use as canonical external hardware/instruction reference when documenting exact P2 instruction encodings or reconciling generated data.

### `utility/gen_propan_instructions.zig.py`

Role: generation path into Propan's instruction definitions.

Use when documenting which generated source feeds the assembler and when designing generated documentation checks.

## Existing repository documentation outside OKF

### Root `README.md`

Role: user-facing historical/current overview.

Useful for intent and existing syntax explanations, but not authoritative when it conflicts with the Zig implementation. Known conflicts are tracked in `/projects/propan/documentation-discrepancies.md`.

### `docs/propan/semantics.md`

Role: a **mixed historical/design and implementation-note** document. It contains original conceptual `.section` mappings alongside newer explanations of `.org`, `.fit`, `.pack`, `.pic`, conditional directives and imports.

Do not treat all sections as universally current or universally obsolete; validate each syntax claim against semantic code.

### `examples/*.propan`

Role: realistic source examples with **different test registration states**, not an undifferentiated legacy set.

Current review (2026-10-15):

- `rgbx.propan` uses the now-implemented `.org`, `RES` and `.fit`, but is listed as a **support file**, not in the active `examples` fixture group; full assembly status is not yet established.
- `propio-client.propan` and `sumloop.propan` are both included in `tests/propan/cases.zig`'s active `examples` group. This establishes regression registration, not an execution result in the current OKF audit or proof of hardware behavior.
- `sumloop.propan` now constructs ALTI state with `alti.state(...)` rather than unsupported bitwise operations on raw address labels.

Use `/projects/propan/documentation-discrepancies.md` for the maintained per-file classification.

## Historical implementation/tooling

### `tools/nerdgruppe/p2/propan/`

Role: older Python Propan implementation/tooling.

Useful for archaeology and design intent only. It must not override current Zig behavior.

## Official/community P2 documentation

Material under `docs/p2/` and the pinned p2docs submodule may be used to explain P2 hardware semantics. When exact PASM2 encoding/syntax is involved, prefer the repository-pinned official instruction sheet and clearly distinguish official statements from community-derived explanations.

## Current coverage state

The September source inventory was built against the pre-merge implementation. Source and test roles above were refreshed for the 2026-10-15 CLI/import/output changes, but other language areas must still be rechecked against the merged sources.

The major current-state reference areas that originally motivated this inventory are now covered: lexical grammar, expressions, symbols, address/segment behavior, directives/data, instruction syntax/selection, pointer addressing, standard library, PASM2 differences, CLI/output behavior, examples, and coverage mapping.

Remaining work is tracked in `/TODO.md`, including a post-merge semantics/directives audit before the full first-pass reference can again be considered current.
