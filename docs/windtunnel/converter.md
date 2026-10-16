# Digital color converter

Each cog owns a `ColorConverter`. SETDACS and streamer routing produce
`raw_dacs: [4]u8`; conversion publishes `dacs: [4]u8`, ordered DAC0 through DAC3.
Both arrays are observable state. IO routes the final `dacs` bytes into configured constant-impedance DAC pads.

SETCY, SETCI and SETCQ supply three coefficient bytes and one output offset each.
SETCFRQ supplies the carrier frequency. SETCMOD writes nine bits: sync polarity,
DAC0 additions, signed coefficients, converter mode, and independent TMDS controls.
The complete CMOD value lives in the converter; the streamer reads its TMDS bits.

## Arithmetic and output modes

The matrix multiplies unsigned DAC3/DAC2/DAC1 by the corresponding coefficient
bytes. CMOD bit 4 selects signed or unsigned coefficients. Component Y/I/Q
results use an arithmetic right shift by seven and eight-bit wrapping.

The chroma rotator retains three fractional matrix bits: its initial coordinates
are signed eleven-bit slices of the I/Q sums shifted right by four. Five CORDIC
iterations use angle increments 32, 19, 10, 5 and 3, arithmetic coordinate shifts,
and twelve-bit wrapping after each iteration. Quadrant normalization uses the
upper eight carrier-phase bits. The final IQ byte is the rotated coordinate
shifted right by four, with eight-bit wrapping. This produces about 0.823 times
the signed component I/Q amplitude; the documentation's 1.646 gain describes the
rotator without accounting for this input/output scaling.

Mode 00 bypasses conversion. Mode 01 produces FY/FI/FQ/FS for component output.
Mode 10 produces two composite outputs, chroma and luma. Mode 11 produces four
copies of composite output. The silicon-defined offsets, DAC0 additions,
sync inversion, blanking and luma-sync selection apply with byte wrapping.
NTSC/PAL coefficient selection and line alternation remain software decisions.

## Clocking and instruction effects

Conversion continues independently of instruction execution, stalls, waits,
interrupts and streamer activity. Measured output delays are five clocks for raw
DAC changes, four for coefficient changes, and three for carrier-phase changes.
Offsets and final output selections are live. SETCMOD selects its final mux one
clock before the ordinary parameter-write boundary; other setters use writeback.

The input latch follows raw DAC state in bypass. Disabled arithmetic stages retain
their contents; carrier phase is zero. Re-enabling conversion exposes retained
pipeline data until fresh values arrive. Cog stop/restart and hub reset clear
converter parameters, arithmetic state and both DAC arrays.

The implementation groups the five rotation iterations into clocked cells to
match the measured external delays. This register partition is an equivalent
model, not a claim about unpublished physical RTL placement.

GETBRK WCZ bit 22 reports conversion enabled, independently of TMDS. The converter
has no additional event. Streamer pin output, FIFO transfers, TMDS encoding and
existing streamer events operate independently of these DAC transformations.

## Inspection and validation

The GUI displays raw and final DAC bytes, configuration, phase and retained
arithmetic cells without clocking them. Existing checklist DAC assertions inspect
final output and remain local-only because the native digital oracle cannot read
DAC bytes.

`converter-vectors.propan` retains 144 independent two-P2 physical validation
vectors, covering signed/unsigned coefficients, all active modes, offsets, sync
states, wrapping and held carrier phases. Focused Zig tests cover fractional
chroma inputs, coordinate overflow and individual clock delays. Additional
fixtures cover instruction cancellation, GETBRK, register operands and streamer
background restoration through an active converter.

The measurement setup, calibrated control comparisons and limitations are
recorded in [Accumulated P2 Knowledge](../accumulated.md#color-converter-arithmetic-and-transport-2026-10-14).
The original measurement firmware remains independent of Windtunnel. The electrical
frontend now supports ADC/scope conversion and capture; DDS/Goertzel consume its raw bitstream.

Final validation used `zig-0.16.0 build install test -Doptimize=ReleaseSafe
-Dcoverage=false`: all 36 build steps succeeded, with 386 tests passing and two
optional tests skipped. The final registered fixture run passed all 3,576 runs
across 228 files. Native regressions covered 125 distinct digital cases, including
converter-active status and simultaneous TMDS/conversion, alongside the 144
independent two-P2 converter vectors.
