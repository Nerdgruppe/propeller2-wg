---
type: "Reference"
title: "Propan addresses and segments"
description: "Current address-domain, segment, and execution-mode semantics implemented by Propan."
tags: ["propan", "addresses", "segments", "cog", "lut", "hub"]
status: "draft"
source_confidence: "high"
---
# Propan addresses and segments

Historical design notes and parser fixtures contain additional planned syntax that is not currently implemented; those conflicts are called out explicitly below.

## Tagged addresses

Propan does not represent a label as only one integer. The semantic layer uses a tagged address containing:

- an absolute HUB byte address;
- a segment identifier;
- an execution-local address domain: HUB, COG, or LUT.

For COG/LUT addresses the local value is a register/instruction address while the HUB address records where the corresponding bytes are emitted. For HUB-exec addresses the local value is the HUB address itself.

This lets one source label retain both its emitted HUB location and the address value meaningful to code executing from COG/LUT/HUB space.

## Label defaults

Code labels and `var`/data labels both carry tagged addresses, but they receive different default usage hints:

- a code label evaluates as an address intended for **literal** use;
- a data label evaluates as an address intended for **register** use.

The unary operators `*` and `&` switch that usage hint:

- `*label` requests register-style use;
- `&label` requests literal/address-style use.

They do not relocate the label or change its tagged address.

## Address helper functions

| Expression | Meaning in the current implementation |
|---|---|
| `label` | tagged address, encoded according to its default/current usage and the consuming operand |
| `hubaddr(label)` | absolute HUB byte address |
| `cogaddr(label)` | COG-local address; requires a COG-tagged address |
| `lutaddr(label)` | LUT-local address; requires a LUT-tagged address |
| `localaddr(label)` | local address for whichever execution domain owns the label |
| `@label` | current-location-relative longword displacement based on HUB addresses |
| `*label` | same tagged address with register usage requested |
| `&label` | same tagged address with literal usage requested |

The semantic tests demonstrate the distinction directly. In the initial COG segment, a label at HUB byte address `8` has local COG address `2`; `localaddr(data)` therefore emits `2` while `hubaddr(data)` emits `8`.

`localaddr(register)` is not currently a successful interface: the evaluator unconditionally emits an error for register input even though its TODO suggests intended COG-only acceptance. See [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md).

## Address units

HUB addresses are byte addresses.

COG/LUT local addresses are register/instruction-long addresses. Advancing one encoded instruction or one `LONG` advances the local COG/LUT address by one while advancing emitted HUB storage by four bytes.

That distinction is visible in semantic fixtures: after three emitted LONGs, a COG-local end label has local address `3` and HUB address `12`.

## Execution modes

The semantic address model has three execution modes:

- `cog`
- `lut`
- `hub`

Starting a new execution segment determines how local addresses are interpreted.

### `.cogexec [hub_address]`

Starts a new COG-exec segment. Its local PC starts at `0x000` while bytes are emitted at the current HUB cursor or at the explicitly supplied HUB address.

### `.lutexec [hub_address]`

Starts a new LUT-exec segment. Its local PC starts at `0x200` while bytes are emitted at the current HUB cursor or at the explicitly supplied HUB address.

### `.hubexec [hub_address]`

Starts a new HUB-exec segment. Local execution address and HUB byte address are the same; an explicit operand chooses the new HUB address.

All three forms currently accept zero or one operand. Supplying more operands is diagnosed.

## COG/LUT allocation versus HUB emission

The current implementation does not have a separate COG/LUT reservation allocator. Local register/PC movement is derived directly from emitted byte count inside a COG- or LUT-exec segment.

For COG/LUT segments:

- four emitted HUB bytes advance the local address by one;
- an ordinary encoded instruction is four bytes and therefore advances the local PC by one;
- `LONG` emits four bytes and advances the local address by one;
- `WORD`/`BYTE` advance HUB by their byte size; local-address calculation is still derived from the segment's HUB-byte offset, so layouts intended to represent register/instruction slots should remain longword-aligned;
- `var name:` only records the current tagged address and gives the symbol register-oriented default usage; it does **not** reserve a register or emit bytes.

Consequently a current source pattern such as:

```propan
.cogexec
var temp:
    LONG 0
```

places `temp` at local COG register `0` and emits a four-byte initialized slot into the segment's HUB image. A following `var next:` would be at local COG register `1` only after that `LONG` has advanced the cursor.

There is currently no implemented equivalent of a non-emitting `.RES`/`.reserve` directive. Historical examples using `.RES`, `.regs`, `.reserve`, or `.regspace` must not be interpreted as current reservation syntax. Under the current directive set, representing register/data slots means emitting their backing bytes with `BYTE`/`WORD`/`LONG` or arranging layout externally.

The same rule applies to LUT-exec, except the local address starts at `0x200`.

## Segment identity

Each execution-mode transition creates a distinct segment identifier. The tag survives on labels so the implementation can distinguish addresses that happen to have similar local values but originate in different logical emitted regions.

Not every code path currently enforces that distinction correctly. In particular, `@label` contains an explicit TODO to verify same-segment use before computing its displacement.

## Crossing execution domains

When an address is converted for instruction encoding, the current implementation compares the target address's native execution mode with the instruction's execution mode.

- HUB ↔ COG/LUT mismatches emit a warning because the transition may be intentional but needs attention.
- COG ↔ LUT mismatches are currently rejected as errors.
- the implementation comments that stronger segment-identity checking is still needed to prevent jumps between unrelated local-exec segments.

## Moving the HUB cursor and overlap handling

An explicit HUB address supplied to `.cogexec`, `.lutexec`, or `.hubexec` can move emission to a new HUB location. Moving backwards below the previous HUB position emits a warning.

The code contains a disabled overlap-validation block (`TODO: Reinclude the overlap check!`). Therefore the OKF must **not** claim that overlapping segments are rejected. Accidental overlap remains an implementation risk.

## Verified sequential-segment example

`tests/propan/sema/addressing-modes.propan` exercises four sequential segments. Each segment emits three `LONG` values followed by a two-argument `LONG`, for 20 HUB bytes total per segment.

With no explicit HUB relocation, the resulting layout is:

| Segment | HUB start | Local start | `_endN` after three LONGs | HUB `_endN` |
|---|---:|---:|---:|---:|
| first COG | `0x00000` | COG `0x000` | COG `0x003` | `0x0000C` |
| second COG | `0x00014` | COG `0x000` | COG `0x003` | `0x00020` |
| LUT | `0x00028` | LUT `0x200` | LUT `0x203` | `0x00034` |
| HUB | `0x0003C` | HUB `0x0003C` | HUB `0x00048` | `0x00048` |

This demonstrates two important current rules:

1. HUB emission continues sequentially unless a new exec directive supplies an explicit HUB address;
2. each new COG segment restarts local addressing at `0`, each new LUT segment at `0x200`, while HUB-exec local addresses equal HUB byte addresses.

## Complete mixed-layout example

The following example combines COG code/register-like data, LUT code/data, and HUB-resident data using only currently implemented directives:

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

Its layout is:

| Symbol | Domain/local address | HUB address | Why |
|---|---:|---:|---|
| `cog_start` | COG `0x000` | `0x1000` | COG segment starts at local zero |
| `cog_temp` | COG `0x001` | `0x1004` | one 4-byte NOP precedes it |
| `lut_start` | LUT `0x200` | `0x1008` | new LUT segment restarts local PC at `0x200` |
| `lut_temp` | LUT `0x201` | `0x100C` | one 4-byte NOP precedes it |
| `hub_data` | HUB `0x1010` | `0x1010` | HUB segment begins at current HUB cursor |

The two `var` declarations themselves consume no space; their following `LONG 0` directives create the emitted slots. This is the current replacement for examples that only need initialized register-like storage. It is **not** equivalent to a non-emitting reservation directive, because the four zero bytes are part of the HUB image.

## Planned/historical directive syntax is not current language

Several repository files outside the OKF describe or demonstrate a broader segment model:

- `docs/propan/semantics.md` discusses `.section` and `.org` mappings;
- `tests/propan/sema/segment_management.propan` describes `.org`, `.reserve`, `.regspace`, and `.data` in addition to the implemented exec-mode directives;
- parser fixtures and examples contain `.huborg`, `.cogorg`, `.lutorg`, `.regs`, `.cogfit`, `.RES`, `.fit`, and similar forms.

The current semantic mnemonic table does not register those names as implemented directives. They should therefore be treated as **design/historical or parser-only material until independently verified**, not as supported syntax.

See [/projects/propan/documentation-discrepancies.md](/projects/propan/documentation-discrepancies.md).

## Current implementation issues affecting this model

- `@label` does not yet validate same-segment identity.
- cross-local-segment protection is explicitly incomplete in `get_offset_for_exec_mode()` comments.
- segment-overlap rejection is disabled.
- the generic `TaggedAddress.init()` helper is unused and malformed (`.hub` instead of `.hub_address`).

These findings are tracked in [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md).
