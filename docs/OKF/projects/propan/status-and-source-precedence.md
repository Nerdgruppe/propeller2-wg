---
type: "Reference"
title: "Propan source precedence"
description: "Defines documentation authority and source precedence for current Propan behavior."
tags: ["propan", "provenance", "source-precedence"]
status: "draft"
source_confidence: "high"
timestamp: "2026-09-30T11:15:00+02:00"
---
# Propan source precedence

## Documentation status

Documents under `/projects/propan/` marked `status: "draft"` are current-state references assembled from the implementation and tests.

When a page is incomplete, it should say so explicitly rather than filling gaps from PASM2, Spin2, historical Propan code, or design intent.

## Source precedence

For questions about **what the current Propan assembler accepts or emits**, use this precedence:

1. current Zig implementation under `src/propan/`;
2. semantic/regression tests that exercise the current Zig implementation;
3. generated instruction/standard-library data consumed by the current implementation;
4. parser-only tests, which prove grammar acceptance but not semantic support;
5. repository examples;
6. root `README.md` and material under `docs/propan/`;
7. older Python Propan tooling under `tools/nerdgruppe/p2/propan/`.

For questions about **Propeller 2 hardware behavior or canonical PASM2 encoding**, official P2 documentation and the repository's canonical instruction data take precedence over Propan implementation choices.

## Evidence labels

Use these labels where a distinction matters:

- **Implemented:** directly established by current `src/propan/` code.
- **Tested:** exercised by current automated tests.
- **Generated:** derived from generated instruction or standard-library data used by the assembler.
- **Documented externally:** stated by repository documentation outside `docs/OKF/`, but not yet independently established by implementation/tests.
- **Historical/design note:** describes an earlier implementation or proposed design.
- **Conflict:** external documentation disagrees with current implementation.
- **Open:** behavior or intent still requires verification or a project decision.

## Parser tests are not semantic conformance tests

The parser intentionally accepts any identifier in the instruction position. Therefore a parser test containing a mnemonic or directive proves only that the line can be parsed. It does not prove that semantic analysis recognizes or implements that mnemonic.

This distinction is particularly important for directive-looking names such as `.huborg`, `.cogorg`, `.lutorg`, `.cogfit`, `.regs`, `.RES`, `.fit`, and `.org` found in parser tests or examples. These must be checked against the semantic mnemonic table before being documented as supported.

## Change policy

When code changes:

- update the affected OKF page in the same development cycle when practical;
- record meaningful documentation changes in `/projects/propan/log.md`;
- preserve discovered conflicts with non-OKF documentation until those files are intentionally updated;
- do not silently rewrite current-state documentation to match design intent when the implementation still behaves differently.
