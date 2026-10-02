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

Labels bind to the location before a following value's implicit padding. Place `.align` before a label when the label must name that aligned value.

`byteoffset(label)` returns the byte position within a cog/LUT register (0–3). `wordoffset(label)` returns that position divided by two (0–1). Both require a cog, LUT, or regspace label. These can supply the selector for `GETBYTE`/`SETBYTE` or `GETWORD`/`SETWORD` when accessing packed cog/LUT data. A word beginning at an odd byte position spans two word fields and cannot be accessed as one word with that selector.
