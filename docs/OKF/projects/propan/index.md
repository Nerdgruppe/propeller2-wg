---
type: "Index"
title: "Propan"
description: "Index for curated Propan assembler and language documentation."
tags: ["index", "propan", "assembler", "pasm2"]
status: "draft"
source_confidence: "high"
timestamp: "2026-09-30T11:15:00+02:00"
---
# Propan

This scope contains current-state documentation for the Propan assembler and source language.

Propan is under active development. These pages describe the repository state they were verified against; they are not a language-stability or backward-compatibility promise. Where repository documentation outside the OKF conflicts with current code, the conflict is recorded explicitly rather than silently normalized.

## Start here

- [/projects/propan/status-and-source-precedence.md](/projects/propan/status-and-source-precedence.md) — documentation authority, instability policy, and source precedence.
- [/projects/propan/lexical-and-source-grammar.md](/projects/propan/lexical-and-source-grammar.md) — current tokenizer/parser-level language reference.
- [/projects/propan/documentation-discrepancies.md](/projects/propan/documentation-discrepancies.md) — known conflicts with README, old docs, parser-only examples, and repository examples.
- [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md) — assembler issues/limitations found while documenting behavior.
- [/references/propan-sources.md](/references/propan-sources.md) — source inventory and evidence roles.

## Planned coverage

The remaining work includes:

- semantic expressions and values;
- symbols, labels, variables, and address domains;
- segment and execution-mode semantics;
- directives and data layout;
- instruction grammar, conditions, effects, and PASM2 differences;
- pointer addressing;
- standard-library constants/functions;
- assembler/tooling behavior and worked examples.

See [/TODO.md](/TODO.md) for the temporary worklist.
