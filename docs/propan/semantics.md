# Propan Semantics

- Segments
  - Each segment contains a span of data
  - Segments are always anchored with a hub address
  - Each segment knows its dedicated execution mode
    - Hub exec starts at the same address as the segment
    - Cog exec starts at PC = $000
    - Lut exec starts at PC = $200
- Labels
  - Labels are associated with a segment
  - Thus, a label knows its "native" execution mode and can warn if it switches
  - Thus, labels know their "home location" and a warning can be emitted if a jump between different segments happens

Requirements:

- annotate start of "logical code block" (cogexec, hubexec, lutexec)
- warning when jumps from one cogexec/lutexec block to different block
  - from cogexec to lutexec and back might be fine when the blocks are "related"
- warning when jump from cogexec/lutexec to hubexec or back
  - this is always a thing to consider
- no warning for inter-hub jumps, these are always fine
- labels have a "native" interpretation (storage offset or PC)
- means to "relocate" the cursor should be available
  - only change PC
  - forward seek to a given offset in cogexec/lutexec
  - spawn new segment at position

Problems:

- Lut Exec segments must use a cogexec segment for register addresses

Solutions:

- `ORGH $address`                  maps to `.section $mode, [$address]`
- `ORGH [$hub_address] \ ORG $000` maps to `.cogexec [$hub_address]`, sets PC=0x000
- `ORGH [$hub_address] \ ORG $200` maps to `.lutexec [$hub_address]`, sets PC=0x200
- `ORGH [$hub_address]`            maps to `.hubexec [$hub_address]`, sets PC=$hub_address, warns if FIFO or Streamer instructions are used
- `ORG $pc`                        maps to `.org $pc` and can only move forward in current segment

Original PASM:

- `ALIGNW` Align to next word in Hub
- `ALIGNL` Align to next long in Hub
- `BYTE` Insert byte data
- `WORD` Insert word data
- `LONG` Insert long data
- `ORG` Set code for Cog RAM
- `ORGH` Set code for Hub RAM
- `ORGF` Fill Cog RAM with zeros
- `FIT` Validate that code fits within Cog RAM or Hub RAM

Propan's `.fit limit[, "message"]` checks that the current cog/LUT/register PC or hub/data byte address is at most `limit`. The limit may be a compatible address label. `.fit` emits no data and does not replace automatic execution-space bounds checks.
- `RES` Reserve long registers for symbol

`.pack off` uses natural alignment (`BYTE` 1, `WORD` 2, `LONG` and instructions 4). `.pack byte`, `.pack word`, and `.pack long` align the start of each subsequent data or instruction line to 1, 2, or 4 bytes respectively, until the next `.pack`. Values listed on one `BYTE`, `WORD`, or `LONG` line remain contiguous. An explicit `.align` still advances the location. The first instruction emitted under `.pack byte` or `.pack word` warns about potentially unaligned code. A cog/LUT instruction whose register position is not divisible by four is an error.

`.pic` controls instructions whose branch operand has both absolute and relative encodings. It emits no bytes and remains active across segment changes until another `.pic` directive. `.pic default` (the initial mode) uses the assembler's `AnalyzeOptions`; `.pic prefer` favors relative addressing wherever the assembler considers it valid; `.pic avoid` favors absolute addressing; `.pic force` requires relative addressing and errors if an absolute encoding is selected. `nrel(...)` explicitly selects absolute addressing in every mode except `force`, where it is an error. Other instruction operands and data values are unaffected.

For numeric branch targets, cog and LUT share one relative-address domain. `AnalyzeOptions.use_relative_jmp_for_same_mode_nonlabel_address` sets the default for that domain, and `AnalyzeOptions.calld_ambiguous_encoding` selects how a pointer-register `CALLD` resolves when both instruction forms fit.

Outside conditional compilation, constants may refer to other constants in either source order, provided their dependencies have no cycle. A constant needed to determine layout, such as an origin, alignment, array value, or repetition count, must be evaluatable without referring to any label through its dependencies. Constants used only after layout may refer to labels through address functions.

`.if expression`, `.elif expression`, `.else`, and `.endif` select which lines are analyzed and emitted. Use these directives without an instruction condition or effect. Conditions are integer expressions: zero is false and any nonzero value is true. They may use builtins and constants declared earlier in active code, but not labels, `$`, or forward references. An `.elif` is evaluated only when its parent block is active and no earlier branch in the same block matched. Conditions under inactive parents are not evaluated. Inactive lines still need valid syntax, but they do not declare symbols, affect layout, or produce semantic diagnostics. Every `.if` requires a matching `.endif`; each block allows at most one `.else`.

`.import "path"` parses another Propan source file and inserts its lines at the directive. Relative paths are searched beside the file containing the directive, then in the `-I` / `--include-path` directories in command-line order. A file may be imported repeatedly; place `.import once` in that file to include it only on its first visit. Recursive imports without `once` are errors. Imports expand before conditional compilation, so imported files must exist and parse even when their `.import` line is in an inactive branch.

Labels bind to the location before a following value's implicit padding. Place `.align` before a label when the label must name that aligned value.

`byteoffset(label)` returns the byte position within a cog/LUT register (0–3). `wordoffset(label)` returns that position divided by two (0–1). Both require a cog, LUT, or regspace label. These can supply the selector for `GETBYTE`/`SETBYTE` or `GETWORD`/`SETWORD` when accessing packed cog/LUT data. A word beginning at an odd byte position spans two word fields; `wordoffset()` errors at byte offsets 1 and 3 to prevent truncation.

`@label` returns the signed execution distance in longs from the PC after the current instruction to the label. Cog and LUT distances use execution addresses, independent of hub storage origins. Hub byte distances are divided by four; a distance not divisible by four is an error. References between different execution modes are errors. References between segments in the same execution mode are allowed and warn. `.data` and `.regspace` have no execution PC for this operator.

Pointer expressions such as `PTRA++` and `PTRB[2]` are accepted only by instruction operands of type `pointer_expr`. Ordinary register, immediate, and register/immediate operands reject them; neither operand of `MOV` can take a pointer expression.

`localaddr(register)` returns the register number only in `.cogexec` mode; `cogaddr(register)` returns it in every mode. These rules apply to all register values, including those produced by `register` and `regoffset`. Constants use their declaration's execution mode. Register arguments retain the advisory warning that an address function expected an offset.

`NOP` has exactly the word `0x00000000` and accepts no explicit instruction condition, including `return`. Nonzero words in the ROR encoding are ROR instructions; `ROR register(0), register(0)` encodes as `0xF0000000`.

`aug(value)` requires an instruction operand and an immediate value (an integer or address with literal usage). Parentheses are transparent wrapping, so `MOV PA, (aug(266))` is valid; operators and function arguments still introduce nesting and cannot contain `aug()`. Register values and values with register usage, such as `aug(PB)`, `aug(slot)` for a data label, and `aug(*label)`, are errors. It is also invalid in constant declarations, data, layout directives, assertions, and conditional-compilation expressions. An ordinary immediate constant may supply its value, for example `MOV PA, aug(BIG)`. In pointer expressions, augmentation applies to the index: `PTRA[aug(256)]` and `PTRA++[aug(256)]` are valid; `aug(PTRA[256])` and `aug(PTRA++)` are errors.

`pcaddr(label)` returns an execution PC: 0–0x1FF for cog labels, 0x200–0x3FF for LUT labels, and the hub byte address for hub labels. Hub addresses at or below 0x400 are errors. Data and regspace labels have no execution PC. `localaddr(label)` instead returns the local storage index, so LUT indices remain 0–0x1FF.

Starting a `.hubexec` segment below 0x400 warns that regular branches cannot reach that PC. A `.cogexec` or `.regspace` cursor reaching 496 warns about interrupt registers, reaching 502 warns about pointer registers, and reaching 506 is an error because those locations are I/O registers. Each boundary is diagnosed once per segment, including explicit origins and layout directives.

`bitrange(low, high, wrap=...)` supports ranges wrapping from bit 31 to bit 0, matching `pinrange` within a pin group. A reversed range warns when `wrap` is omitted, is allowed with `wrap=#true`, and errors with `wrap=#false`.

Implicit AUGS/AUGD instructions inherit their instruction's condition. Instructions that only update C/Z, such as CMP, TEST, and MODCZ, warn when no effect is supplied.
