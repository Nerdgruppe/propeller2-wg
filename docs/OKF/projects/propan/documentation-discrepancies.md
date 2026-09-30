---
type: "Reference"
title: "Propan documentation discrepancies"
description: "Tracks known conflicts between current Propan implementation and repository documentation outside the OKF."
tags: ["propan", "documentation", "conflicts", "migration"]
status: "draft"
source_confidence: "high"
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

### Root README: effect aliases omitted

**External documentation:** the effect table lists `:and_c`, `:and_z`, `:or_c`, `:or_z`, `:xor_c`, `:xor_z`, `:wc`, `:wcz`, and `:wz`.

**Current implementation:** the parser additionally accepts the compact aliases `:andc`, `:andz`, `:orc`, `:orz`, `:xorc`, and `:xorz`, plus `:wzc` as an alias of `:wcz`. Effect matching is case-insensitive.

**Classification:** implemented aliases are under-documented, not an implementation conflict.

**Action:** keep the full accepted spelling set in the OKF instruction reference; later decide whether the README should list aliases or only point to the canonical reference.

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

### `tests/propan/sema/segment_management.propan` mixes implemented and planned semantics

**External/test material:** this file describes `.org`, `.reserve`, `.regspace`, and `.data` alongside `.cogexec`, `.lutexec`, and `.hubexec`, with comments that read like intended semantics.

**Current implementation:** the hard-coded semantic mnemonic table registers the three exec-mode directives but not `.org`, `.reserve`, `.regspace`, or `.data`.

**Classification:** mixed design/fixture material. The file must not be interpreted as a passing semantic-conformance test for every form it contains.

**Action:** determine whether the unsupported forms are planned language features, obsolete names, or unfinished implementation. Until then, only the independently verified forms belong in the current-state reference.

### Repository `examples/*.propan` have mixed current/legacy status

The current `examples/` directory contains three `.propan` files. They should not be treated as a uniform current-language conformance suite.

#### `examples/rgbx.propan`

This file uses `.org`, `.RES`, and `.fit`. None of those names is registered by the current semantic directive table.

**Classification:** confirmed legacy/stale syntax relative to the current Zig semantic implementation.

**Action:** do not use this file as canonical current syntax. If the example is retained, it should eventually be migrated to the current segment/data model or explicitly marked as a legacy example outside the OKF.

#### `examples/propio-client.propan`

The file does not use the unsupported origin/reservation directives above and its active source uses current-looking constructs such as `const`, `var`, `aug()`, current smart-pin helper calls, `if(...)`, effect suffixes, `REP @label`, and current instruction mnemonics.

It also uses dotted identifiers such as `.end`. In the current language these are ordinary global identifiers, not specially scoped local labels. The example happens to use such names without demonstrating reusable local-label scope.

**Classification:** broadly aligned with current source syntax, but not established as a current conformance example because `examples/` is not part of the semantic/equivalence regression suite.

**Action:** retain as an implementation example only with that caveat until an automated assembly regression validates it on the current branch.

#### `examples/sumloop.propan`

The file uses current directive names (`LONG`, `.align`) and generated instructions, with no obsolete origin/reservation directive spellings. It also forms packed values using label-valued expressions such as `buf_c << 18`, `buf_b << 9`, and `buf_a << 0`.

The current documentation does not establish those mixed address/integer binary-expression forms as a supported general arithmetic interface. Without a dedicated regression run, this file should therefore not be promoted as a canonical current-language example solely because its directive spellings are current.

**Classification:** compatibility/unverified example: no confirmed stale directive syntax, but semantic validity of all expressions is not established by the reviewed evidence.

**Action:** add a current assembler regression or rewrite the packed-address construction using explicitly supported address conversion once intended semantics are confirmed.

## Maintenance rule

When another conflict is discovered, add it here with:

- the external statement/example;
- the current implementation evidence;
- whether the discrepancy is confirmed or still open;
- the follow-up needed to resolve intent.
