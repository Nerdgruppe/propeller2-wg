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

### `src/propan/propan.zig`, `emit.zig`, and `listfile.zig`

Role: authoritative current CLI, multi-file composition, output-format, fill-byte, and list-file behavior.

Use for:

- positional source processing;
- module overlay order;
- flat/JSON output;
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

The `segment_management.propan` fixture mixes implemented and planned syntax and is not blanket evidence that every directive appearing in it is current.

### `tests/propan/regressions/`

Role: focused assembler/CLI regression evidence.

Use for output behavior, multi-file assembly, fill byte behavior, JSON output, and fixed defects.

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

Role: historical/design note about segment/address concepts and proposed directive mappings.

Do not treat as a current syntax reference without implementation verification.

### `examples/*.propan`

Role: realistic source examples with mixed current/legacy status.

Current review:

- `rgbx.propan` uses unsupported `.org`, `.RES`, and `.fit` and is confirmed legacy/stale relative to the current semantic directive table;
- `propio-client.propan` is broadly aligned with current syntax but is not part of the current conformance/regression suite;
- `sumloop.propan` avoids the known obsolete directives but still contains packed label-valued expressions whose full semantic validity is not regression-established.

Use `/projects/propan/documentation-discrepancies.md` for the maintained per-file classification.

## Historical implementation/tooling

### `tools/nerdgruppe/p2/propan/`

Role: older Python Propan implementation/tooling.

Useful for archaeology and design intent only. It must not override current Zig behavior.

## Official/community P2 documentation

Material under `docs/p2/` and the pinned p2docs submodule may be used to explain P2 hardware semantics. When exact PASM2 encoding/syntax is involved, prefer the repository-pinned official instruction sheet and clearly distinguish official statements from community-derived explanations.

## Current coverage state

The first-pass source inventory has now been exercised across the current implementation, parser/sema/equivalence/regression tests, generated instruction data, examples, root README, and historical `docs/propan/semantics.md` material.

The major current-state reference areas that originally motivated this inventory are now covered: lexical grammar, expressions, symbols, address/segment behavior, directives/data, instruction syntax/selection, pointer addressing, standard library, PASM2 differences, CLI/output behavior, examples, and coverage mapping.

Remaining work is narrower and is tracked explicitly in `/TODO.md`: unresolved language/design choices, missing focused regressions, documentation validation automation, and eventual cleanup/replacement of older non-OKF documentation.
