# TODO-List

## Overall TODOs

- Cleanup stray `@import("..").field` imports in the codebase

## Propeller 2 Documentation / P2DB

### Random Task Collection

- Collection of all known CPU hazards
- Determine explicit behavior of the "hidden" registers behind INA/INB/...
- What happens with `AUGD; MODCZ`, `ALTD; MODCZ`, `ALTR; MODCZ`
- SETQ2 clarifications: document which instructions recognize SETQ versus SETQ2,
  how long their prefix state lasts, and which operations preserve or replace Q.
  Cover cancellation consuming prefixes, decode-time Q clearing, SETQ2 making
  immediate WRLONG copy LUT words, and the adjacent-instruction effects on
  COGINIT. See the [hardware regression](tests/windtunnel/state/setq2-coginit.propan)
  and [bug report](BUGREPORT.md).
  Include hardware-verified examples such as this bit-span difference:

  ```propan
                  // SETQ overrides the bit span with Q[4:0].
                  SETQ    3
                  BITH    via_setq,   0   // via_setq = 0x0f

                  // SETQ2 does not override the bit span.
                  SETQ2   3
                  BITH    via_setq2,  0   // via_setq2 = 0x01
  var via_setq:   LONG    0
  var via_setq2:  LONG    0
  ```

  Also explain this hardware-verified COGINIT distinction (run each sequence
  with a worker image at `worker`):

  ```propan
                  // With a register source, SETQ2 wins the adjacent Q clear.
                  MOV     entry, aug(hubaddr(worker))
                  SETQ2   3
                  COGINIT 7, entry                  // Worker's PTRA = 3

                  // The intervening AUGS permits COGINIT's decode to clear Q.
                  SETQ2   3
                  COGINIT 7, aug(hubaddr(worker))   // Worker's PTRA = 0
  var entry:      LONG    0
  ```

## Propan

### Random Task Collection

- Configuration File for analyzer options
- `COGBRK ##S` seems to be unsupported in flexspin
- <https://github.com/totalspectrum/spin2cpp/issues/485>
- `.address => @panic("TODO: Implement binary operators on offsets."),`
- `.string => @panic("TODO: Implement binary operators on strings."),`
- Plan to allow access to local labels somehow?
- Enable "-Dx=y" on the CLI
- Fully define the compatibility matrix for implicit jumping/referencing between segments.

### New Features

#### Groups

Groups are similar to ELF sections with garbage collection.

A group is only emitted into the binary if a symbol from the group is referenced from the outside. Groups can transitively
depend on other groups and form a graph.

The default code is not in a group.

- `.group`: Starts a new group.
- `.endgroup`: Ends the current group and swaps back to "always emitted" context.

#### Relocations

Required for the Ashet Home Computer:

Allows creation of metadata that is attached to a memory location. This metadata can encode "relocations" which
can later be applied to dynamically link the resulting binary to another location, or use other pins.

```propan

.relocation ##base     pin
.relocation ##offset   hub

RDPIN dst, reloc(##base, 10)     // emits the info that this field must be patched by the "base"

RDLONG dst, reloc(##offset, 256) // reads from ##offset+256
```

- `.relocation <tag> <type>`: Declares a new relocation key named `<tag>` of the given `<type>` hint.
  - `<tag>` is an enumerator which uniquely identifies the relocation
  - `<type>` is one of:
    - `bit`: The relocation is assumed to be a pin number (0…31).
    - `pin`: The relocation is assumed to be a pin number (0…63).
    - `hub`: The relocation is assumed to be a hub address (0…0x7FFFF).
    - `lut`: The relocation is assumed to be a lut address (0…511).
    - `cog`: The relocation is assumed to be a cog address/register (0…511).
    - `int`: The relocation is assumed to be any kind of 32 bit integer.
- `reloc(<tag>, <value>)`: Attaches relocation metadata to `<value>`, such that `<value>` can later be patched by the
  relocation identified by `<tag>`.
- Relocations can still have operations applied to them, but these are only additions/subtractions and shifts.
  - `2 * reloc(…)` is considered a shift.
- There can be multiple relocations applied to a single location

### Under Consideration

#### SPIN DEBUG

Currently, Propan does not support the "debug()" syntax from SPIN2

## Windtunnel

### Random Task Collection

- Validate hub execution below `$400`: wraparound behavior makes this possible.
  Is relative branching possible in this area?
- Prove or disprove that every kind of branch (relative or absolute) restores
  the PC rules and execution behavior, including cogexec with PC ≥ `$400`
  and hubexec with PC < `$400`.
- Verify the software FIFO write path's cycle claim: `write_fifo` currently
  makes newly queued bytes eligible after two clocks, but actual visibility in
  hub RAM is governed by the cog's rotating hub RAM slice, partial-long
  placement, and contention. Characterize enqueue-to-write timing across all
  eight cog/bank phases and under contention, then correct any misleading
  comments or implementation assumptions. See the existing FIFO write-visibility
  hardware probes.
- Model "previous SETQ" as pipeline state
- Debug interface
  - Additional Breakpoints
    - Hazards
    - Memory Breakpoints
    - Live Inspection
- IO Interface Improvement
  - Configuration file support for I/O setup
    - Defaults to "stdio routed to pins 62/63"
  - Validate physical pad timing with a logic analyzer
  - Optional instruction-level pin logging (VCD and collision diagnostics exist)
- P2AAS server
  - Feature Parity with hw oracle
- Validate/implement cog startup behavior
  - perfect timing clone so hub writes to the coginit'd segment is both detected/logged and
- Hardware Chip Reset and ROM Boot
- Cache `hub.random.word` to not compute it multiple times per clock
- Implement Streamer FIFO bypass erratum
- Extend test runner to store/print the time per test fixture and test run to find outliers.
- Create a concept for how to react to misbehavior/undefined behavior.
- Refactor code to adapt to better coding standards.
- Improve CLI user experience
- Refactor test suit to enable `macro: name ...` which replaces `{{name}}` or `{{ name }}` with "..."
- forbid things like `LONG 0xfe900000 | (operand & 0xfffff)` in the testsuite unless there's a really good reason for it.
- Implement auto-indentation for `.if` ... `.endif` blocks
- Use CLI seeding through electrical noise for RNG seed
- GUI file dialog support via nfd-zig
- New rule for test suite things:
  - Allow empty `//?` lines
  - `//? run:` must be prefixed by an empty line

### Analog Subsystem

- Add controllable drift/noise to analog subsystem
  - Consider how to handle floating pins.
  - Ingest actual P2 characteristics like voltage leaking, ...

### Virtual Device Support

- SPI/DSPI/QSPI flash support
- I²C EEPROM support
- PAL/NTSC/SECAM/HDMI decoder with automatic sync/timing detection
- Speaker / Audio Recorder
- Microphone / Audio Playback
- GUI: Virtual Scope / Logic Analyzer

Stretch Goal:

- PS/2 mouse/keyboard device emulation
- USB mouse/keyboard device emulation?

### Known Bugs

#### Shared Random Generator

- **Status: partial; deferred/known bug.**
- Retain the shared xoroshiro approximation and measured cross-cog relations.
  Exact tap numbering, absolute polarities, production seed lane and internal
  delay remain open. This applies to `GETRND`, `BITRND`, `DIRRND`, `OUTRND`,
  `FLTRND`, `DRVRND`, randomized `WAITX`, `HUBSET` seeding and physical noise paths.
- Investigation aid if revisited: a reproducible seed/reset anchor or stronger
  state-recovery constraints. Existing seed-hammer experiments did not resolve
  the mapping; captures and analysis are retained in [data/random](data/random/).

### Out Of Scope

#### Overly detailled electrical sim

No protection-diode I–V/clamping, inductance, supply-power/ground-network simulation, thermal behavior, ESD or cumulative damage model. Absolute-rating diagnostics report violations but do not implement these effects.