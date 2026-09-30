---
type: "Reference"
title: "Propan documentation discrepancies"
description: "Tracks known conflicts between current Propan implementation and repository documentation outside the OKF."
tags: ["propan", "documentation", "conflicts", "migration"]
status: "draft"
source_confidence: "high"
timestamp: "2026-09-30T11:15:00+02:00"
---
# Propan documentation discrepancies

This register records cases where repository documentation or examples outside `docs/OKF/` do not accurately describe the current Zig implementation. These are not automatically bugs in either direction: some may represent stale documentation, some unfinished implementation, and some intended future syntax.

Do not resolve these by assumption. Until the project explicitly chooses an intended contract, the OKF should state the current implementation behavior and preserve the discrepancy.

## Current discrepancies

### Root README: ternary operator

**External documentation:** the root `README.md` lists `? :` as a Propan ternary operator.

**Current implementation:** `?` and `:` tokens exist, but the current Zig expression parser has no ternary parsing production.

**Classification:** conflict; likely stale/future documentation.

**Action:** determine whether ternary expressions are intended to be implemented or removed from the public syntax table.

### Root README: octal literals omitted

**External documentation:** the visible syntax material demonstrates decimal, binary, quaternary, and hexadecimal numbers.

**Current implementation:** the tokenizer and integer parser explicitly accept `0o...` octal literals.

**Classification:** implemented but under-documented.

**Action:** include octal syntax in the canonical language reference; later update the root README if octal support is intended to remain.

### `docs/propan/semantics.md`: directive model does not match current implementation

**External documentation/design note:** `docs/propan/semantics.md` describes mappings involving `.section`, `.org`, `.cogexec [hub_address]`, `.lutexec [hub_address]`, and `.hubexec [hub_address]`.

**Current implementation:** semantic analysis currently registers `.cogexec`, `.lutexec`, `.hubexec`, `.align`, `.assert`, `BYTE`, `WORD`, and `LONG` as assembler mnemonics/pseudo-instructions. The old document reads as a design exploration rather than a current language reference.

**Classification:** historical/design note with partial conceptual value, not normative syntax documentation.

**Action:** preserve useful segment/address concepts in OKF current-state pages, but do not copy its proposed directive spellings without implementation verification.

### Parser directive examples overstate semantic support

**External/test material:** `tests/propan/parser/directives.propan` contains directive-looking forms including `.huborg`, `.cogorg`, `.lutorg`, `.cogfit`, and `.regs`.

**Current implementation:** parser tests prove these tokens can be parsed as instruction mnemonics. The semantic mnemonic table does not currently register those names as implemented directives.

**Classification:** test-scope mismatch, not necessarily a test bug.

**Action:** document parser tests as grammar evidence only. Verify semantic support separately before listing any directive in the canonical reference.

### Examples use legacy-looking directives

**External examples:** existing `.propan` examples include forms such as `.org`, `.RES`, and `.fit`.

**Current implementation:** these names are not present in the current hard-coded semantic directive table. They may be historical syntax, aliases generated elsewhere, or currently invalid; this needs explicit verification before examples are treated as canonical.

**Classification:** open conflict.

**Action:** run/inspect example coverage and classify each spelling as supported, legacy, or stale.

## Maintenance rule

When another conflict is discovered, add it here with:

- the external statement/example;
- the current implementation evidence;
- whether the discrepancy is confirmed or still open;
- the follow-up needed to resolve intent.
