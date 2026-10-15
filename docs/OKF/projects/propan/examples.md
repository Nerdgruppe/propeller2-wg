---
type: "Reference"
title: "Propan worked examples"
description: "Small current-syntax examples for execution modes, pointer access, flags, REP-relative labels, and data layout."
tags: ["propan", "examples", "assembler", "pasm2"]
status: "draft"
source_confidence: "high"
---
# Propan worked examples

These examples use the current Zig frontend syntax documented by the surrounding Propan reference. They are deliberately small and focus on assembler behavior rather than board startup, clock setup, or application-specific initialization.

The individual forms are grounded in current parser, semantic, and equivalence fixtures. They should still be treated as examples of source/assembly behavior, not as complete hardware applications.

## Minimal HUB-exec loop

```propan
.hubexec 0

start:
    NOP
    JMP start
```

`.hubexec 0` starts a HUB-exec segment at HUB byte address zero. `start` is therefore a HUB-exec code label. The plain `JMP start` uses the current automatic address-selection rules; for a target in the same segment it selects relative addressing.

For code where absolute addressing is required explicitly, use `nrel(start)` instead.

## COG-exec code with HUB-resident data

```propan
.cogexec 0

start:
    RDLONG DIRA, &payload
    JMP start

.hubexec

var payload:
    LONG 0x12345678
```

The first segment executes in COG mode while its encoded bytes are emitted into HUB memory beginning at the selected HUB address. `.hubexec` starts a later HUB-exec segment at the current HUB cursor.

`payload` is a data label. `&payload` requests literal/address usage so that `RDLONG` consumes the memory address rather than the data label's default register-usage hint.

This example intentionally does not imply that the assembler reserves or uploads COG RAM automatically; execution-mode segments describe addressing/encoding and HUB emission as documented in [/projects/propan/addresses-and-segments.md](/projects/propan/addresses-and-segments.md).

## COG/LUT/HUB layout and register-like slots

```propan
.cogexec 0x1000
cog_start:
    NOP
var cog_temp:
    LONG 0

.lutexec
lut_start:
    NOP
var lut_temp:
    LONG 0

.hubexec
hub_data:
    LONG 0x12345678
```

COG local addressing starts at `0`, LUT local addressing starts at `0x200`, and HUB addressing uses the HUB byte address directly. Each `NOP` or `LONG` is four emitted bytes, so it advances a COG/LUT local address by one.

`var` only marks the current address; the following `LONG` creates storage and advances the HUB/local cursors. Non-emitting register reservation is **now implemented** as `RES` (without a leading dot) in COG/register-space modes; it is not interchangeable with an initialized `LONG`.

## Pointer memory access

```propan
    RDLONG dst, PTRA
    RDLONG dst, PTRA++
    RDLONG dst, ++PTRA
    RDLONG dst, PTRB[-1]
    WRLONG dst, PTRA--[4]

var dst:
    LONG 0
```

Current pointer syntax is a dedicated operand form, not general arithmetic on the PTRA/PTRB register values.

- `PTRA` uses the pointer without updating it;
- `PTRA++` updates after the memory operation;
- `++PTRA` updates before the memory operation;
- `PTRB[-1]` uses a non-updating signed index;
- `PTRA--[4]` uses an updating pointer expression with magnitude four.

Non-updating indexes are limited to `-32...31`. Updating forms use a positive magnitude of `1...16`; the `++`/`--` position and sign select pre/post and increment/decrement behavior.

The repository equivalence suite exercises these families with `RDBYTE`, `RDWORD`, `RDLONG`, `WRBYTE`, `WRWORD`, `WRLONG`, and LUT accesses.

## Conditions and effects

```propan
    CMP DIRA, 0 :wcz
if(Z) MOV OUTA, 1
if(!Z) MOV OUTA, 0
```

Effects follow the operand list as `:name`. Conditions precede the mnemonic as `if(...)`.

The parser also accepts the complete C/Z condition algebra documented in [/projects/propan/instruction-syntax.md](/projects/propan/instruction-syntax.md), for example:

```propan
if(C & !Z) ADD DIRA, 1
if(C == Z) ADD DIRA, 1
if(>=) ADD DIRA, 1
```

Do not use an explicitly conditioned `NOP`. The parser recognizes it, but the merged semantic analyzer **rejects** `if(...) NOP` and `return NOP` with `err_nop_cannot_have_condition`. The conditional-ROR encoding described by the September reference is historical.

Whether a particular effect is legal is determined by the selected generated instruction variant. A recognized effect token is not automatically valid on every instruction.

## REP with a relative label

```propan
    REP @loop_end, 3
    ADD DIRA, 1
loop_end:
    NOP
```

`@loop_end` computes a relative longword displacement from the current expression location to the label. `@` is therefore location-dependent and cannot be used in an ordinary location-independent constant declaration.

Repository examples also use `REP @label, count`; this is the current Propan counterpart to carrying over PASM2 relative-label intent while using Propan's expression syntax.

## Data layout and alignment

The following example uses `.data` to give `.align` byte-oriented HUB semantics:

```propan
.data 0
    BYTE 0
.align 4
aligned:
.assert hubaddr(aligned) == 4
    LONG 0
```

In `.data`/HUB mode, `.align 4` advances the HUB byte cursor to address four. In COG/LUT modes `.align` instead aligns local **long/register indices**. Data emission and padding use the configured `--fill-byte` (default `0x00`), not a fixed `0xFF`.

## Address-domain contrast

A tagged label may have both a HUB byte address and an execution-local COG/LUT address. Use explicit conversion when the required representation matters:

```propan
.cogexec 0x1000

entry:
    LONG hubaddr(entry)
    LONG cogaddr(entry)
    LONG localaddr(entry)
```

For this COG-exec label, `hubaddr(entry)` denotes its HUB emission address, while `cogaddr(entry)` and `localaddr(entry)` denote its local COG address. This distinction is central to Propan's address model and should be made explicit in stored tables or interfaces rather than relying on contextual conversion.

## Imports and non-emitting register reservation

Source files can be combined as **one module** via `.import "file.propan"`, with optional `.import once` inside imported files. Repeating `-I directory` supplies search paths; the CLI rejects multiple independent root paths.

```propan
.regspace
var first: RES 2
var second: RES 1
.fit 3

.cogexec 0x100
MOV first, second
```

Register-space labels have local register indices but no HUB storage; `RES` reserves non-emitting register positions. See `tests/propan/sema/res-labels.propan`.

In `tests/propan/cases.zig`, `examples/propio-client.propan` and `examples/sumloop.propan` are in the active example fixture group. `examples/rgbx.propan` is a supporting file rather than an active example fixture, so its complete assembly/runtime behavior is not established by that list.

## Migration reminders

When adapting PASM2/Spin2 assembly into these examples:

- do not copy PASM2 `#` immediate markers; Propan obtains literal/register usage from the expression value and uses `#name` for enumerators;
- rewrite condition prefixes into `if(...)` or `return` forms;
- rewrite flag effects as `:wc`, `:wz`, `:wcz`, or the supported TEST-family forms;
- use `&`, `*`, `hubaddr()`, `cogaddr()`, `lutaddr()`, `localaddr()`, `nrel()`, and `aug()` where the intended address/encoding interpretation must be explicit;
- use the implemented `.hubexec`, `.cogexec`, `.lutexec`, `.regspace`, `.data`, `.org`, `RES`, `.fit`, `.align`, `.assert`, `BYTE`, `WORD`, and `LONG` with their documented operand/mode restrictions.

See [/projects/propan/pasm2-differences.md](/projects/propan/pasm2-differences.md) for the compact migration table.
