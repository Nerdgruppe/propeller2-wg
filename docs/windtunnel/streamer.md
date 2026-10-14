# Digital streamer and DAC state

Each cog owns a clocked `Streamer` and exposes `dacs: [4]u8`, ordered DAC0 through
DAC3. These bytes are effective DAC output state, independent of digital pin
output. No DAC byte is consumed by the pin or analog simulation.

SETDACS retains four background bytes. Active streamer commands selectively
replace these using the documented sixteen DAC routing selectors, including
complemented channels. Background writes remain effective on unselected channels;
ending an override restores the latest background byte. Cog stop/restart and
hub reset clear both background and effective bytes.

## Commands and modes

XINIT replaces current and buffered commands and zeros NCO phase. XSTOP is the
existing XINIT 0,0 alias. XZERO and XCONT buffer one command; issuing a third
waits for buffer space while retaining its operands, augmentation and SETQ
frequency. The buffered frequency takes effect when that command starts.
XZERO resets phase at the boundary; XCONT continues it. SETXFRQ changes the
live frequency. Cog initialization sets frequency to `$80000000`.

Commands output/capture an initial sample. Finite counts stop after their NCO
rollovers; `$FFFF` runs perpetually and hands off a pending command at the next
rollover. Frequency zero retains the initial sample without rollovers.
The NCO computes `(phase & $7FFFFFFF) + frequency` with 32-bit wrapping, using
the resulting MSB as its rollover signal.

Implemented modes include immediate and FIFO raw output, immediate/FIFO LUT
lookup, LUMA8/RGBI8/RGB8/RGB16/RGB24 unpacking, and digital pin capture with
optional FIFO recording. Narrow samples pack within bytes, with the documented
alternate order; immediate output repeats its last sample. Pin groups align to
the selected width and wrap through P63 to P0. Streamer output ORs with each
cog's architectural OUT registers before direction gating. It does not modify
OUTA/OUTB or automatically enable DIR.

FIFO modes share the existing RDFAST/WRFAST engine, GETPTR, block reloads, FBW
and hub grants. Recording disabled on pin-input modes does not engage the FIFO.
FIFO modes require matching prior configuration and cannot use hub instruction
fetch concurrently. Physical writes retain the issuing streamer's provenance. Software WF instructions
displace coincident automatic capture writes. Partial narrow capture bytes are
discarded at command boundaries rather than flushed or carried into the next command.
Companion LUT writes have priority over streamer reads on the shared port; the
simulator retains the previous LUT result when an ongoing read is displaced;
the initial lookup is represented as prefetched. Hardware collision observations
and the limits of this model are recorded in the accumulated knowledge.

## Events and digital video

XMT reports XZERO/XCONT becoming active, including from idle; it does not repeatedly assert while
idle or merely because XINIT left the buffer empty. XFI reports exhausting the
command stream. XRO reports NCO rollovers. XRL reports streamer reads of LUT
address `$1FF`; ordinary RDLUT and inspector reads do not produce XRL.
Streamer LUT reads also notify matching selectable LUT-read sensors. These
sources use the existing POLL/WAIT/J/JN consumers, interrupts and debug traps.
GETBRK WCZ includes the streamer-active bit.

SETCMOD selects normal/forward/reverse digital video independently of the
[color converter](converter.md). TMDS maintains separate running disparities for RGB,
encodes pixel bytes or passes literal ten-bit symbols, and serializes LSB first
with clock and differential pairs. Use NCO frequency `$0CCCCCCD` for ten system
clocks per pixel. This digital mechanism does not transform DAC bytes.

DDS/Goertzel, GETXACC, scope/ADC commands and ADC capture modes remain unsupported.
Converter modes transform the mixed DAC bytes without changing digital streamer pins.

## Inspection and verification

Cog GUI inspectors show four read-only decimal/hex DAC bytes and the streamer
engine, including its active command, pending command and NCO. Inspection does
not clock engines or consume events. Local checklist fixtures may assert:

```cpp
//? oracle: off "DAC output state has no digital silicon readback"
//? post: cog[*].dacs == u8 [0x12 0x34 0x56 0x78]
```

Exactly four u8 values are required. DAC preconditions and hardware oracle
observations are rejected; the hardware reporting protocol is unchanged.

Registered streamer fixtures compare digital output, capture, events, FIFO
pointers, command timing and TMDS sequences against P2AAS at nominal 20 MHz
RCFAST. Local tests cover DAC routes/restoration, lifecycle, LUT wrap and TMDS
encoding/disparity invariants. Hardware findings are recorded in
[Accumulated P2 Knowledge](../accumulated.md).

The pin model uses measured digital transport boundaries: raw streamer data has
one additional clock before the ordinary GPIO OUT path; TMDS has four. The
initial TMDS output publishes clock-positive, zero-data pairs before the first
character arrives. These delays describe observed boundaries rather than
identified physical registers. Analog thresholds, drive strengths and waveform
electrical behavior remain outside the model.

Validation on 2026-10-14 includes 20 registered streamer fixtures with 137 runs:
120 digital hardware comparisons at RCFAST and 17 local-only DAC-state checks.
The complete project suite includes 3,426 Windtunnel fixture runs and 376 passing
unit/harness/GUI tests, with two optional Propan tests skipped. Use
`zig-0.16.0 build install test -Doptimize=ReleaseSafe -Dcoverage=false` to repeat
local validation; hardware comparisons require the existing P2AAS oracle.
