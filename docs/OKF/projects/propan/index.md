---
type: "Index"
title: "Propan"
description: "Index for curated Propan assembler and language documentation."
tags: ["index", "propan", "assembler", "pasm2"]
status: "draft"
source_confidence: "high"
---
# Propan

This scope contains current-state documentation for the Propan assembler and source language.

Propan is under active development. These pages describe the repository state they were verified against; they are not a language-stability or backward-compatibility promise. Where repository documentation outside the OKF conflicts with current code, the conflict is recorded explicitly rather than silently normalized.

## Start here

- [/projects/propan/status-and-source-precedence.md](/projects/propan/status-and-source-precedence.md) — documentation authority, instability policy, and source precedence.
- [/projects/propan/lexical-and-source-grammar.md](/projects/propan/lexical-and-source-grammar.md) — current tokenizer/parser-level language reference.
- [/projects/propan/expressions.md](/projects/propan/expressions.md) — current semantic value model, operators, and address/encoding helper functions.
- [/projects/propan/symbols-and-declarations.md](/projects/propan/symbols-and-declarations.md) — constants, code/data labels, `var`, namespaces, and reference/evaluation order.
- [/projects/propan/addresses-and-segments.md](/projects/propan/addresses-and-segments.md) — HUB/COG/LUT address domains, segment identity, and exec-mode directives.
- [/projects/propan/directives-and-data.md](/projects/propan/directives-and-data.md) — currently implemented assembler directives, data emission, alignment, and assertions.
- [/projects/propan/instruction-syntax.md](/projects/propan/instruction-syntax.md) — instruction grammar, conditions, effects, operand categories, and variant selection.
- [/projects/propan/pointer-addressing.md](/projects/propan/pointer-addressing.md) — PTRA/PTRB pointer expressions, update/index ranges, branch-S relative operands, and CALLD overlap rules.
- [/projects/propan/stdlib.md](/projects/propan/stdlib.md) — active predefined constants, builtin helpers, P2 configuration functions, enumerator domains, and generated-reference support.
- [/projects/propan/tooling.md](/projects/propan/tooling.md) — CLI inputs, multi-file overlay, flat/JSON output, fill behavior, list files, and exit status.
- [/projects/propan/pasm2-differences.md](/projects/propan/pasm2-differences.md) — compact PASM2-to-Propan migration table covering immediates, conditions/effects, addressing, directives, expressions, and source structure.
- [/projects/propan/examples.md](/projects/propan/examples.md) — small current-syntax examples for HUB/COG execution, pointer access, flags, REP-relative labels, and data layout.
- [/projects/propan/documentation-discrepancies.md](/projects/propan/documentation-discrepancies.md) — known conflicts with README, old docs, parser-only examples, semantic fixtures, and repository examples.
- [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md) — assembler issues/limitations found while documenting behavior.
- [/references/propan-sources.md](/references/propan-sources.md) — source inventory and evidence roles.
- [/references/propan-coverage.md](/references/propan-coverage.md) — map from reference areas to implementation, parser/sema/equivalence/regression validation, and known gaps.

## Review status

The first-pass current-state reference set and repository-example classification are complete and have passed an OKF-only integrity audit for internal links, navigation, front-matter consistency, terminology, and stale coordination wording. The set is ready for project review.

Remaining work is limited to unresolved language/design choices, focused code/test fixes or regressions, documentation-validation automation, and eventual cleanup of non-OKF documentation. These items remain tracked in `/TODO.md`.
