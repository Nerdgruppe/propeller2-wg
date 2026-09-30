---
type: "Finding Register"
title: "Propan implementation findings"
description: "Potential assembler issues and implementation limitations discovered while documenting current Propan behavior."
tags: ["propan", "assembler", "findings", "issues"]
status: "draft"
source_confidence: "high"
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

After type equality is established, binary expression evaluation handles integer/register/enumerator/pointer-expression values but uses `@panic` for `.address` and `.string` operands.

**Documentation impact:** do not imply that all syntactically accepted binary expressions are safely diagnosed. Address/string binary expressions are currently an implementation hazard.

### Raw string and enumerator data emission can panic

The generic value-to-data cast used by `BYTE`, `WORD`, and `LONG` contains `@panic("string emission not supported yet")` for strings and an internal-error panic for unresolved enumerators.

**Documentation impact:** strings are valid expression values but are not currently supported as raw data-emission arguments. Enumerator values require a consuming instruction enumeration context rather than direct emission.

### Layout directives cannot currently use ordinary user constants

Location assignment runs before `evaluate_constant_values()`. `.align` and explicit `.cogexec`/`.lutexec`/`.hubexec` HUB-address operands are evaluated during that earlier layout pass, when user constant symbols exist but their values are still unset.

As a result, a declared user constant cannot currently be used for those layout values. `.align` also maps the resulting `UndefinedSymbol` evaluation failure to a message saying labels cannot be referenced, which is misleading for this case.

**Documentation impact:** directive documentation must distinguish expressions accepted grammatically from values available during layout. This ordering may warrant implementation changes if user constants are intended for layout directives.

### Segment-overlap validation is disabled

Location assignment contains a commented-out overlap check with a `TODO: Reinclude the overlap check!`. Moving a new HUB segment backwards emits a warning, but the intended overlap error is not active there.

**Documentation impact:** current multi-segment/overlay behavior must be described carefully. Do not state that accidental overlap is rejected until this is implemented and tested.

### `.assert` wrong-type message reports the condition type

When a second `.assert` argument is present but is not a string, the diagnostic formats `@tagName(condition.value)` rather than the type of the message expression.

**Documentation impact:** no language-semantic impact, but diagnostics may be misleading for this error case.

### `TaggedAddress.init()` initializes the wrong field name

`TaggedAddress` contains a `hub_address` field, but its generic `init()` helper currently returns a struct literal using `.hub = hub`. Repository search found no current `TaggedAddress.init(` call site, so Zig's lazy function analysis can allow this latent defect to remain unnoticed while the helper is unused.

**Documentation impact:** none for currently exercised address construction, which uses the specialized `init_hub`/`init_cog`/`init_lut` helpers. Treat the generic helper as broken until corrected or removed.

## Findings requiring verification

### Unknown escape recovery may drop the escaped character

The string unescaper warns for an invalid escape but appends the variable holding the original backslash rather than the unrecognized escape character. A source such as `"\\q"` therefore appears likely to retain `\` while dropping `q` during recovery.

**Next step:** add a focused regression test or otherwise execute the current assembler before classifying the exact output behavior as confirmed.

### `@` relative-offset operator does not validate segment identity

The evaluator contains an explicit TODO to verify that the current location and target address belong to the same segment before calculating the longword delta from HUB addresses.

**Documentation impact:** the current address reference documents the implemented HUB-address calculation and this missing validation separately.

### Cross-local-segment jump checking is incomplete

`get_offset_for_exec_mode()` rejects COG↔LUT execution-domain transitions and warns for HUB↔local transitions, but its own comment notes that segment identity still needs to be incorporated into the protection logic so unrelated local-exec segments cannot be confused merely because their local addresses overlap.

**Documentation impact:** address-domain diagnostics should be described as current safeguards, not as complete segment-safety validation.

## Documentation policy for findings

- Describe current behavior, not desired behavior.
- Mark unexecuted code-reading conclusions as potential until verified by a test or reproducible invocation.
- If a finding is fixed, retain a short historical note only if it explains documentation/version differences; otherwise remove it from the current-state reference and note the change in `log.md`.
