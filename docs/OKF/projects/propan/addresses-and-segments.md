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

## Current examples

The semantic addressing fixture demonstrates four sequential segments:

1. COG-exec segment at HUB `0`, local COG `0`;
2. another COG-exec segment later in HUB memory, again local COG `0`;
3. LUT-exec segment later in HUB memory, local LUT `0x200`;
4. HUB-exec segment where local and HUB addresses match.

This is important: local COG/LUT addresses restart according to execution mode when a new segment begins even though HUB emission continues elsewhere.

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
- the generic `TaggedAddress.init()` helper appears to initialize a non-existent `.hub` field instead of `.hub_address`; no current call site was found in repository search, so this is a latent helper defect unless the function remains unused.

These findings are tracked in [/projects/propan/implementation-findings.md](/projects/propan/implementation-findings.md).
