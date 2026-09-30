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
- [/projects/propan/documentation-discrepancies.md](/projects/propan/documentation-discrepancies.md) — known conflicts with README, old docs, parser-only examples, semantic fixtures, and repository examples.
- [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md) — assembler issues/limitations found while documenting behavior.
- [/references/propan-sources.md](/references/propan-sources.md) — source inventory and evidence roles.

## Planned coverage

The remaining work includes:

- ambiguous/PC-relative instruction-selection details and PASM2 differences;
- detailed pointer-addressing reference;
- standard-library constants/functions;
- multi-file/output/tooling behavior;
- worked examples and migration aids.

See [/TODO.md](/TODO.md) for the temporary worklist.
