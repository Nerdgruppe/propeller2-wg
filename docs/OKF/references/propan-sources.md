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
- function-call grammar.

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
- regression cases such as unary plus and alignment.

### `tests/propan/regressions/`

Role: focused assembler/CLI regression evidence.

Use for output behavior, multi-file assembly, fill byte behavior, JSON output, and fixed defects.

### `tests/propan/equivalence/`

Role: comparison material against PASM2/FlexSpin encodings and syntax forms.

Use for instruction operand mapping and PASM2-to-Propan comparisons. Individual files are not by themselves proof that every possible variant is exhaustively covered.

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

Role: realistic source examples.

Useful for style and intended workflows, but some directive spellings appear older than the current semantic mnemonic table and require validation before reuse in normative examples.

## Historical implementation/tooling

### `tools/nerdgruppe/p2/propan/`

Role: older Python Propan implementation/tooling.

Useful for archaeology and design intent only. It must not override current Zig behavior.

## Official/community P2 documentation

Material under `docs/p2/` and the pinned p2docs submodule may be used to explain P2 hardware semantics. When exact PASM2 encoding/syntax is involved, prefer the repository-pinned official instruction sheet and clearly distinguish official statements from community-derived explanations.

## Coverage gaps to address

- complete stdlib inventory;
- complete directive semantics;
- address/segment model;
- exact instruction-selection and augmentation behavior;
- CLI/output formats;
- validation of existing examples against the current assembler;
- generated documentation consistency checks.
