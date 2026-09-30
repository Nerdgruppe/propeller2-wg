---
type: "Finding Register"
title: "Propan implementation findings"
description: "Potential assembler issues and implementation limitations discovered while documenting current Propan behavior."
tags: ["propan", "assembler", "findings", "issues"]
status: "draft"
source_confidence: "high"
timestamp: "2026-09-30T11:15:00+02:00"
---
# Propan implementation findings

This page records potential assembler defects or limitations discovered during documentation work. It is not a substitute for the repository issue tracker. Findings remain here while they materially affect how current behavior should be documented.

## Confirmed code-level findings

### Pointer-register ambiguity check contains a copy/paste defect

Instruction selection computes `any_ptrreg_prev` and `any_ptrreg_now`, but the ambiguity guard currently tests `any_ptrreg_prev` twice:

```zig
if (any_ptrreg_prev == true and any_ptrreg_prev == true) {
    @panic("incredibly amgigious instructions, should check the setup");
}
```

This means the guard does not express the apparent intended condition of both competing variants being pointer-register variants. It can also terminate the assembler with a process panic rather than a source diagnostic if reached.

**Documentation impact:** ambiguous instruction families, especially CALLD/pointer-register forms, should not yet be presented as fully hardened selection behavior.

### Binary operations on address values and strings can panic

After type equality is established, binary expression evaluation currently handles integer/register/enumerator/pointer-expression values but uses `@panic` for `.address` and `.string` operands.

**Documentation impact:** do not imply that all syntactically accepted binary expressions are safely diagnosed. Address/string binary expressions are currently an implementation hazard.

### Segment-overlap validation is disabled

Location assignment contains a commented-out overlap check with a `TODO: Reinclude the overlap check!`. Moving a new HUB segment backwards emits a warning, but the intended overlap error is not active there.

**Documentation impact:** current multi-segment/overlay behavior must be described carefully. Do not state that accidental overlap is rejected until this is implemented and tested.

### `.assert` wrong-type message reports the condition type

When a second `.assert` argument is present but is not a string, the diagnostic formats `@tagName(condition.value)` rather than the type of the message expression.

**Documentation impact:** no language-semantic impact, but diagnostics may be misleading for this error case.

## Findings requiring verification

### Unknown escape recovery may drop the escaped character

The string unescaper warns for an invalid escape but appends the variable holding the original backslash rather than the unrecognized escape character. A source such as `"\\q"` therefore appears likely to retain the backslash and drop `q` during recovery.

**Next step:** add a focused regression test or otherwise execute the current assembler before classifying the exact output behavior as confirmed.

### `@` relative-offset operator does not validate segment identity

The evaluator contains an explicit TODO to verify that the current location and target address belong to the same segment before calculating the longword delta from HUB addresses.

**Documentation impact:** document the implemented calculation and the missing cross-segment validation separately when the address model page is written.

## Documentation policy for findings

- Describe current behavior, not desired behavior.
- Mark unexecuted code-reading conclusions as potential until verified by a test or reproducible invocation.
- If a finding is fixed, retain a short historical note only if it explains documentation/version differences; otherwise remove it from the current-state reference and note the change in `log.md`.
