# Accumulated Propeller 2 Knowledge

## CORDIC computer

Investigated against the local P2AAS board on 2026-10-10/11. Windtunnel implements
these findings in [Cordic.zig](../src/windtunnel/sim/Cordic.zig), with instruction
dispatch in [execute.zig](../src/windtunnel/sim/execute.zig). The implementation
uses integer arithmetic throughout; host floating-point transcendental functions
do not reproduce the last result bits.

The [CORDIC measurement archive](../data/cordic/README.md) preserves the 1,080
arithmetic test rows, original source/image/UART capture artifacts, and a script
for offline replay or recapture from physical hardware.

The operation definitions come from the shipped [instruction summary](p2/original/Instructions.xlsx)
and [silicon documentation](p2/silicon-documentation.md). Parallax's
[PNut QLOG/QEXP resolver](https://github.com/parallaxinc/P2_PNut_Public/blob/3e96396d8ecb412d612d73c9b627da3cac56bd18/p2com.asm#L10220)
provides the hyperbolic algorithm, constants, gain corrections and rounding rules.
The circular algorithm, division overflow behavior, read timing and latch/reset
behavior below were inferred from silicon measurements. They form a model that
matches the tested inputs, rather than a claim to have recovered the chip's RTL.

### Operands and results

Here `Q` normally means the third operand supplied by a preceding `SETQ`, or zero
when absent; late Q-register writes are described below. `{high,low}` means an
unsigned 64-bit concatenation. All result words are
32 bits. Coordinates are signed two's-complement values; lengths are unsigned.

| Command | Computation | GETQX | GETQY |
|---|---|---|---|
| `QMUL D,S` | Unsigned `D*S` | Low product | High product |
| `QDIV D,S` | Unsigned `{Q,D}/S` | Quotient | Remainder |
| `QFRAC D,S` | Unsigned `{D,Q}/S` | Quotient | Remainder |
| `QSQRT D,S` | Square root of unsigned `{S,D}` | Root, rounded down | Zero |
| `QROTATE D,S` | Rotate signed `(D,Q)` by binary angle `S` | Rotated X | Rotated Y |
| `QVECTOR D,S` | Convert signed `(D,S)` to polar coordinates | Length | Angle |
| `QLOG D` | Approximate `log2(D)` | Unsigned 5.27 fixed-point logarithm | Zero |
| `QEXP D` | Approximate `2^(D/2^27)` | Unsigned integer | Zero |

An angle of `0x40000000` is a positive quarter turn, `0x80000000` is a half
turn, and a full turn wraps at `2^32`. Positive rotation maps `(1,0)` toward
`(0,1)`. For `QVECTOR`, **length is X and angle is Y**; the community CORDIC
page in this checkout has these reversed in its prose/formula.

`GETQX/GETQY :wc` report bit 31 of the returned word. `:wz` reports whether
that word is zero. This also applies to an empty read returning a retained word.
Commands leave C and Z unchanged.

### Division overflow and zero divisors

Ordinary host division is sufficient only when the quotient fits in 32 bits.
Overflow is neither saturation nor truncation of an exact 64-bit quotient.
The following non-restoring divide reproduces the measured results, including
zero divisors:

1. Set signed 33-bit remainder `r = high`, and unsigned 32-bit quotient `v = low`.
2. Repeat 32 times. Remember the old sign of `r`; shift `r` left, inserting
   `v[31]`, and wrap to 33 bits. Add the unsigned divisor if the old remainder
   was negative; otherwise subtract it. Wrap this arithmetic to 33 bits.
   Shift `v` left and insert one when the new remainder is nonnegative, else zero.
3. If `r` is negative, add the divisor once, again wrapping to 33 bits.
   Return `v` as X and the low 32 bits of `r` as Y.

Use `(high,low) = (Q,D)` for `QDIV`, and `(D,Q)` for `QFRAC`.

| Command and prefix | X | Y |
|---|---|---|
| `QDIV 1,0`, no prefix | `ffffffff` | `00000001` |
| `SETQ 10; QDIV 123,0` | `fffffff5` | `0000007b` |
| `SETQ 1; QDIV $ffffffff,1` | `fffffffe` | `00000001` |
| `QFRAC 7,2`, no prefix | `fffffffa` | `0000000c` |

The zero-divisor quotient is therefore not always `ffffffff`. The arithmetic
fixture includes overflowing dividends and divisors with bit 31 set.

### Circular CORDIC precision

`QROTATE` and `QVECTOR` match a circular CORDIC with 32 iterations, seven
coordinate guard bits after normalization, and eight angle guard bits. The
inferred algorithm is:

1. Interpret input coordinates as signed 32-bit values. Compute
   `m = (abs(x) | abs(y)) & 0x7fffffff`, using unsigned absolute magnitudes.
   If `m == 0`, return `(0,0)`. Otherwise `n = clz(m)-1`.
2. Shift both coordinates left by `n` **within 32 bits**, then perform the
   quadrant correction **within signed 32 bits with wrapping negation**.
   For rotation, negate both coordinates for angles in the middle half-turn
   (`(angle ^ (angle << 1)) & 0x80000000 != 0`), and adjust the angle by a
   half-turn toward zero. For vectoring, negate both if normalized X is negative
   and initialize the angle accumulator to a half-turn.
3. Widen the coordinates and shift them left by seven. The angle accumulator
   uses `2^40` units per turn (signed rotation input shifted left by eight).
4. For `i = 0..31`, first apply the gain correction described below if selected.
   Use `delta[i] = round(atan(2^-i) * 2^40 / (2*pi))`. If rotation's angle
   accumulator is nonnegative, or vectoring's Y is negative, update
   `(x,y,z) = (x-(y>>i), y+(x>>i), z-delta[i])`. Otherwise use the opposite
   signs. All right shifts are arithmetic and both coordinate updates use the
   old coordinates.
5. Round an output coordinate with `(value + 2^(n+6)) >> (n+7)` and retain
   its low 32 bits. This rounds half ties toward positive infinity. Vectoring
   returns the rounded X length and low 32 bits of `(z+0x20)>>8` as its angle.

The `0x20` angle bias is an empirical fit, checked against the measured corpus
and 5,000 additional deterministic random vectors. It is not derived from a
published hardware implementation. Using a mathematical `atan2`, or ordinary
round-to-nearest on the final angle, differs on some inputs.

Gain correction is **interleaved**, rather than one multiplication by an ideal
CORDIC compensation constant. Immediately before steps
`2,3,4,7,8,10,12,14,16,19,20,22,23,24,25,30`, perform:

```text
x = x - (x >> i)
y = y - (y >> i)
```

These are the same correction indices used by PNut's hyperbolic resolver. Their
placement reproduces circular results that an initial/final gain multiplication
misses in the low bits. The circular delta table is preserved in `Cordic.zig`.

#### Signed-minimum coordinate behavior

`0x80000000` is a special case. Its absolute magnitude loses bit 31 in the
normalization mask. It can disappear during the wrapping left shift, or survive
at `n=0` with a wrapping negation that still leaves it negative. Do not handle
all occurrences of this coordinate by returning zero.

| Vector input `(D,S)` | Length X | Angle Y |
|---|---|---|
| `(80000000,00000000)` | `00000000` | `00000000` |
| `(00000000,80000000)` | `00000000` | `00000000` |
| `(80000000,00000001)` | `00000001` | `40000000` |
| `(00000001,80000000)` | `00000001` | `00000000` |
| `(80000000,7fffffff)` | `9411e955` | `38f8dc69` |
| `(7fffffff,80000000)` | `b504f333` | `e0000000` |

The minimum-coordinate fixture also covers rotations, reversed coordinates and
negative neighbours. These results are hardware observations, even where they
do not resemble the ideal mathematical answer.

### Hyperbolic CORDIC precision

The PNut resolver agrees with measured `QLOG/QEXP` outputs and supplies a more
precise specification than rounding a host `log2` or `exp2`. Windtunnel translates
its integer algorithm directly:

- `QLOG`: `m = min(clz(D),31)`, `y = ((D<<m)&0x7fffffff)<<6`,
  `x = y+2^38`, `z = 0`.
- `QEXP`: `m = 31-(D>>27)`, `x = 0x7f42e61c5a`, `y = 0`,
  `z = (D&0x07ffffff)<<11`.
- Run steps `1..31`, repeating steps 4 and 13. Apply the same gain corrections
  as above once before each selected numbered step. Use PNut's delta table,
  `atanh(2^-i)/ln(2)` scaled by `2^38`.
- With arithmetic shifts, positive steering updates
  `(x,y,z) = (x+(y>>i), y+(x>>i), z-delta[i])`; otherwise reverse the signs.
  Positive steering means `y<0` for log and `z>=0` for exp.
- `QEXP` returns `((x>>m)+(y>>m)+0x40)>>7`.
- For `QLOG`, set `z = ((z>>2)&~0x7f)+0x80+((31-m)<<35)`.
  If `m==0` and bit 39 of this value is clear, return `ffffffff`;
  otherwise return the low 32 bits of `z>>8`.

Examples: `QLOG 0` and `QLOG 1` both return zero; `QLOG $ffffffff` saturates
to `ffffffff`; `QEXP 0` returns one; `QEXP $ffffffff` returns `ffffffe8`.
Each operation publishes Y=0 with normal readiness, even though the instruction
description only promises its X result.

### Scheduling, result latches and QMT

CORDIC commands use the hub-command slot `CT[2:0] == cog ID`, independently of
the rotating hub-RAM slice. A command takes 2..9 clocks in cog execution, and
each cog can submit another command every eight clocks. Eight requests can be
in flight per cog. Commands do not wait for their computations to finish.

For a directly adjacent `GETCT start; Qxxx ...; GETCT stop` sequence using
registers or short immediates in cog RAM, the measured timestamp difference is:

```text
stop - start = 4 + ((cog_id - start - 2) & 7)
```

The extra two clocks belong to timestamp sampling. The checked-in timing fixture
sweeps all eight phases and 0..64 clocks of work before a read. It confirms the
same schedule for every operation, and for `QMUL` on all eight cog IDs.

Windtunnel delivers a usable result at `grant+58` in its execution-clock
convention. An immediately following `GETQX` takes 58 clocks, giving 60 clocks
for command plus read after the command begins on an eligible slot; surrounding
`GETCT`s measure 62. The documented 54-stage pipeline / result 55 clocks later
describes the solver; the simulator's calibrated deadline includes transport and
read timing. It does not imply a 58-stage arithmetic pipeline.

Each cog has one retained `(X,Y)` pair and separate X/Y readiness bits:

- A new submission clears **both** readiness bits, even if an older result was
  unread. An immediately following read waits for a result instead of returning
  the old pair as ready.
- Every arriving computation replaces both words and sets both readiness bits.
  Unread results can be overwritten. There is no queue of completed pairs.
- `GETQX` consumes only X readiness; `GETQY` consumes only Y readiness. Reading X
  twice does not consume Y.
- An unready read waits while the cog tracks any in-flight command. With none
  tracked, it takes the normal two clocks, returns the retained word and sets
  QMT. The retained words are initially zero after chip/simulator initialization.
- QMT is sticky. Submitting or successfully reading another operation does not
  clear it. `POLLQMT` reports its previous state in requested C/Z flags and clears
  it. `JQMT` and `JNQMT` both sample and clear it, even when the branch is not
  taken.

Results belong to the submitting cog. The cross-cog isolation fixture tests
**all 56 distinct issuer/reader pairs**, including adjacent cog IDs and the 7/0
boundary. While X has a command pending, Y's unready `GETQX/GETQY` return its own
retained words in two clocks and set Y's QMT. After X's result arrives, it still
does not become ready in Y. After Y has computed and consumed its own result,
further computations in X neither overwrite Y's retained words nor make them
ready again. Each issuer also checks that the reader's local computation did not
overwrite the issuer's result. These are hardware-verified visibility properties,
independent of how the simulator implements routing.

The pipeline fixture submits eight requests, reads pairs eight clocks apart,
then submits another batch and lets all unread pairs be overwritten. It runs
on all eight cog IDs.

### Shared pipeline and inspection

There is exactly **one** `hub.cordic` arithmetic computer. It has 54 real clocked
stages, matching the documented depth, and accepts at most one command per hub
clock. Each entry carries its submitting cog ID, operation and current arithmetic
state. A cog contains only `cordic_result`: retained words, independent readiness
bits and a count of logically pending local commands. No cog owns an arithmetic
pipeline or a queue of precomputed answers.

Inspectable fields in `Cordic.zig` are:

| Field | Meaning |
|---|---|
| `hub.cordic.input` | Newly accepted command and raw D/S/Q operands |
| `hub.cordic.pipeline[0..54]` | Optional live entry in each arithmetic stage; null means a bubble |
| `Cordic.stage_layout[index]` | Decode, normalize, quadrant, iteration, scale correction, repeated hyperbolic step, or final packing |
| `entry.cog`, `entry.operation`, `entry.tracked` | Result destination, arithmetic mode and whether the cog still tracks the request |
| `entry.data` | Tagged operands, partial product, division remainder/quotient, square-root state, circular/hyperbolic X/Y/Z, or final words |
| `hub.cordic.returns[0..3]` | Tagged result transport after leaving the arithmetic pipeline |
| `hub.cogs[id].cordic_result` | Cog-local result words and read status |

Each edge advances old entries simultaneously by one stage. Stages 0, 1 and 2
decode operands, normalize and correct quadrants. Stages 3..52 contain 32 numbered
iteration cells, 16 gain-correction cells, and the two repeated hyperbolic cells.
Stage 53 rounds/packs the result. Multiplication performs one shift/add bit per
numbered iteration; division performs one non-restoring step; square root performs
one restoring two-bit step. Circular/hyperbolic operations perform their actual
microrotations and gain corrections in separate cells. Mode-inapplicable cells
carry the state forward, like a bypass in a shared datapath.
Each occupied slot holds that cell's output state after the most recent edge;
`stage_layout[index]` identifies the work that produced the stored state.

The official documentation specifies the total depth, not the allocation of work
to each individual physical cell. This allocation is the simulator's inferred
decomposition of the verified algorithms, **not a recovered RTL netlist**. It
retains real intermediate arithmetic for inspection. A unit test changes a live
partial product and verifies that the later result changes accordingly; another
fills all 54 stages and checks 64 consecutive tagged results, one per clock.
The shared-cog fixture also runs all eight cogs concurrently on hardware: each
submits eight distinguishable multiplications before fetching results, then
publishes all 64 pairs to separate hub-memory blocks for comparison.

A grant at G enters stage 0 at G+1, occupies stage 53 at G+54, and exits the solver
at G+55. The separate three-clock read transport delivers to the owning cog at
G+58, preserving the measured instruction timing. Cog reset marks only that
cog's in-flight entries as no longer tracked; it does not remove entries from
this shared pipeline or its return transport. Because the pipeline advances by
edges rather than absolute deadlines, counter rollover does not change its age.

### Prefix and cog-reset interactions

`SETQ` supplies the third operand to `QDIV`, `QFRAC` and `QROTATE`, including
through `AUGS/AUGD`. A conditionally cancelled command submits nothing and leaves
old results available, but consumes its prefix. Decode-time Q clearing can still
occur for cancelled or subsequently discarded instructions; existing pipeline-Q
fixtures cover that behavior.

An immediately adjacent **`SETQ2`** also supplies Q to these CORDIC commands.
This comes from the late SETQ2 write overlapping command decode, not general
SETQ2 support: with intervening `AUGS/AUGD`, decode clears Q to zero. For example:

| Sequence | X | Y |
|---|---|---|
| `SETQ2 1; QDIV 6,7` | `613566757` | `3` |
| `SETQ2 10; QROTATE 3,0` | `3` | `10` |
| `SETQ2 10; QROTATE 3,aug(0x40000000)` | `0` | `3` |
| `SETQ2 1; QDIV aug(0xffffffff),aug(0x200)` | `8388607` | `511` |

An adjacent `CRCNIB` also writes Q late enough to win decode clearing, even
though it consumes the preceding SETQ prefix. The arithmetic command must use
the actual Q register after decode, not gate it on the remaining prefix flag.
For example, `SETQ 10; CRCNIB scratch,0; QROTATE 3,0` returns `(3,160)` because
CRCNIB shifts Q left by four. The prefix fixture verifies this for `QDIV` and
`QFRAC` too.

Cog reset clears readiness and the cog's logical command tracking but preserves
the result words and computations already handed to the shared solver. An old
computation can therefore repopulate the result latches after `COGINIT`, including
during the 504-long reload. `COGSTOP` uses the same cog reset in Windtunnel; a
whole-chip reset creates fresh CORDIC state.

Measured self-restart cases (`QMUL 6,7`, on cogs 0, 3 and 7):

| Restart | First GETQX / QMT | After waiting 100 clocks |
|---|---|---|
| No-load restart with computation pending | `0 / 1` | X=`42`, Y=`0`, QMT=`0` |
| No-load restart after result arrived | `42 / 1` | Retained X=`42`, Y=`0`, QMT=`1` |
| Reload restart with computation pending | `42 / 0` | Retained X=`42`, Y=`0`, QMT=`1` |

The table polls QMT after each indicated group of reads. In the first case, the
initial read does not wait: reset abandoned logical tracking. The later physical
result still makes both words ready, even for a stopped or restarted cog.

### Reproducing the investigation

The `tests/windtunnel/state/cordic-*.propan` fixtures are part of the normal
test suite. They include 1,080 captured arithmetic vectors (boundary cases and
seeded random words), flags, zero divisors, signed minima, a full eight-request
pipeline, QMT, prefix clearing, restart behavior and cross-cog visibility.
Arithmetic runs use cogs 0, 3 and 7; timing/pipeline runs collectively cover all
eight cog IDs, and visibility runs cover all 56 distinct cog pairs.

```sh
zig-0.16.0 build install test -Doptimize=ReleaseFast -Dcoverage=false
zig-out/bin/windtunnel-tests --oracle tests/windtunnel/state/cordic-*.propan
```

The oracle command uses the default `P2AAS_ENDPOINT=ws://localhost:12880/` and
requires the physical hardware service. Use `--characterize --artifact-dir DIR`
instead of `--oracle` to capture measurements without executing or asserting the
simulator. For a new probe, name observable result registers in its checklist;
the artifacts contain hardware registers and their observation offsets.

During development, an additional 5,000 deterministic random inputs per operation
for `QROTATE`, `QVECTOR`, `QLOG` and `QEXP` matched hardware exactly (20,000
comparisons beyond the captured regression corpus). That sample used a single
Python `Random(0xc04d1c)` stream, generating 32-bit D, S and Q words in that order, with 5,000
rows for each operation in the order just listed. This is strong sampled
evidence, not exhaustive verification over all 32-bit operand combinations.


## Event latch consumption

The official [silicon documentation](p2/silicon-documentation.md#events) specifies
16 event flags. `POLLxxx` reports presence in requested C/Z flags and clears the
selected flag. Both `Jxxx` and `JNxxx` clear that flag whether or not the branch
is taken. A sensor whose condition remains true can set the flag again; CT1..3
compare the sign bit of the wrapping low-32-bit `CT - target` on every clock.

`WAITxxx` clears the selected flag on completion. An immediately preceding
`SETQ` supplies an absolute low-32-bit CT timeout; requested C/Z flags report
timeout rather than event presence. There are only 15 wait instructions: no
`WAITQMT` exists, because an empty CORDIC read cannot occur while the cog waits.
The documentation's later mention of `WAITQMT` in the QMT clearing list is
inconsistent with its explicit instruction inventory and no-WAITQMT explanation.
These are documentation-backed semantics, not new hardware measurements.

## Pin pattern event edges

Two-cog hardware probes in `tests/windtunnel/state/pattern-events.propan` verify
SETPAT/PAT using held levels on pins 0..3 and 32..35, with receiving cogs 0, 3
and 7. C selects INA (0) or INB (1), and Z selects mismatch (0) or equality (1)
for `(INx & D) == S`. D masks only the input: bits in S outside D remain
significant. The instruction preserves its operands and C/Z, and later flag
changes do not affect the captured comparison.

PAT is edge-sensitive: a false-to-true comparison transition sets the sticky
latch. SETPAT clears PAT and establishes the current condition as its baseline,
so configuring an already-true condition does not produce an event. After a poll,
branch or wait consumes PAT, a held true condition does not reassert it. A later
false-to-true transition generates another event. Changes between two unequal
patterns in mismatch mode do not generate an event. A latched PAT survives the
condition becoming false, and reconfiguration clears it. Zero-mask and unmasked-S
comparisons that are always true or always false have no subsequent edges.

These measurements refine the silicon documentation's phrase "set whenever"
for PAT; it must not be modeled as a continuously asserted level. The fixture
also checks WAITPAT wakeup and SETQ timeouts. Its mailbox acknowledgments follow
64-clock settling waits, so these observations do not characterize I/O propagation
delays.

## Ordinary interrupts (INT1–INT3)

Hardware measurements in `tests/windtunnel/state/interrupt-*.propan` cover all
three ordinary levels, all eight cogs, eight CT arrival phases, every
cog/LUT/hub caller/handler pair, and all four hub byte alignments. They use the
P2AAS device at `ws://localhost:12880/`; INT0/debug is excluded.

An interrupt injects `CALLD IRETx,IJMPx WCZ` into an issue slot without consuming
the held instruction or hub FIFO bytes. Older instructions finish first. IRETx
saves the held PC and interrupted C/Z; IJMPx supplies target PC and incoming
C/Z. Entry does not use the hardware stack. RETIx is `CALLD INB,IRETx WCZ`;
RESIx uses IJMPx as D, saving an ISR continuation for its next entry. Any D is
valid for completion, provided the effective S is a register IRETx and both
C/Z writes are selected. ALTS-selected IRET sources work; canceled CALLD and
partial flag modifiers do not complete the interrupt.

INT1 has priority over INT2 over INT3. Higher levels nest within lower handlers.
Each level has independent idle, pending and active states (GETBRK encodes them
as 0, 2 and 3). Events while pending/active are ignored. STALLI masks entry,
including forced requests, while sensors and request latches continue operating.
SETINTx uses D[3:0]; zero disables the source rather than selecting INT. Source
changes, including disabling, preserve pending requests. Enabling a source with
an already-set event latch does not create a request; polling that event does
not cancel a request already received. TRGINTx works with a disabled source and
has no effect on an already-pending/active level. NIXINTx clears pending **or
active** state without changing control flow. This active cancellation extends
the manual's description of canceling a pending request.

For straight local code, `GETCT start; TRGINTx; MOV ...` lets the first MOV
complete before entry; the handler's first GETCT reads `start + 10`. Its saved
return points to the second following instruction. CT-triggered local entry
reads eight or nine clocks after the target, according to the issue phase.
There is no universal interrupt-latency constant: instruction stalls, prefixes,
issue phase and target refill all contribute. A zero AUGS/AUGD still shields;
a canceled prefix does not. SETQ/SETQ2, ALTx, CRCNIB, SCA/SCAS, GETCT WC and
XORO32 protect the following consumer. A block transfer completes all beats
before entry. STALLI immediately following TRGINT prevents entry; NIXINT at
that position cannot retract an already-scheduled CALLD.

For a synthetic CALLD executing at clock `t`, local target entry is `t + 4`.
Hub target entry is `t + 13 + ((bank - t - 5 - cog) & 7)`, with
`bank = (target >> 2) & 7`; all four byte alignments matched this formula.
Ordinary RETI/RESI target refill follows the same branch mechanism.

**WAITINT differs from the manual on the measured device.** It waits for any
pending ordinary interrupt request, independently of STALLI. A previously
latched INT event alone does not satisfy it: a bounded wait times out and
clears that flag. A canceled WAITINT leaves the flag intact. A pending request
wakes WAITINT under STALLI without entering a handler; with entry allowed, the
wait completes before the injected CALLD executes. Other WAIT instructions
hold pending interrupts until their own event/timeout completes. The tests
measure the two-clock resume delay and the following instruction's clock.
POLLINT/JINT/JNINT continue to consume the sticky interrupt-occurred latch.

GETBRK WCZ reports the ordinary interrupt selectors/states, STALLI, and the
cog's **startup** execution mode (which survives subsequent branches between
memory domains). GETBRK WC reads event flags without consuming them; measured
CT1/CT2/CT3 bits are 1/2/3, correcting their reversed order in the silicon
manual's GETBRK table. GETBRK WZ returns zero/Z=1 without a queued skip pattern.
On a newly started cog, CT targets reset to zero and their flags assert until
rearmed. Cog 0's bootloader may additionally leave a selectable event latched;
status probes normalize these sensors before asserting individual event bits.

## Debug interrupt (INT0)

The silicon documentation describes INT0 as higher priority than INT1..INT3
and unaffected by STALLI. HUBSET enables it per cog. A debug-enabled COGINIT
starts with IJMP0 pointing at execute-only cog ROM $1F8; the ROM saves cog
registers $000..$00F to the cog's upper-hub-RAM slot, loads a 16-long debug
handler from the adjacent slot, and restores the original registers on exit
through $1FD..$1FF. The slot base is `$FF800 + ((!cog & 15) << 7)`.

Within INT0, INA and INB are IJMP0 and IRET0 unless BRK control bit 11 exposes
the normal pin inputs. The injected `CALLD IRET0,IJMP0 WCZ` saves the held PC
and flags. `RETI0` and any register-D `CALLD D,IRET0 WCZ` return from INT0;
`RESI0` stores the next debug instruction in IJMP0 before returning. The
debug handler must execute BRK to arm any subsequent breakpoint: control bits
0..11 are cleared on entry. Outside INT0, BRK can request an interrupt even
when its condition is false; the condition still controls updating its code.
These rules are represented by the `program/debug-*.propan` simulator fixtures;
they have not been replayed against the hardware oracle in this session.

HUBSET debug configuration can also protect the last 16 KiB of hub RAM. In
that mode the lower `$7C000..$7FFFF` alias reads as zero, writes to it are
blocked, and `$FC000..$FFFFF` writes are allowed only from a debug ISR. The
lock bit prevents later configuration changes until chip reset. A queued hub
write retains the issuing instruction's privilege even if the cog enters or
leaves INT0 before the write reaches RAM.

## Selectable pin events (SE1–SE4)

Hardware probes in `tests/windtunnel/state/selectable-pin-events.propan` and
`selectable-pin-arming.propan` use a companion cog to hold input levels. Their
112 runs cover all four trackers, all eight receiving cogs for arming, INA/INB,
and pins 0, 31, 32 and 59 for event consumption and interrupt integration.
Every input change settles for 64 clocks before acknowledgment; these probes
do not characterize physical I/O propagation timing.

SETSEn uses only D[8:0]. The pin encodings are `%001_PPPPPP` for rising,
`%010_PPPPPP` for falling, `%011_PPPPPP` for either edge, `%10x_PPPPPP` for low,
and `%11x_PPPPPP` for high. P selects INA[0..31] or INB[0..31]; the x bit is
ignored. Each tracker keeps its own selection and edge history.

Rising/falling/change events do not reassert while the input is held steady.
Low/high events continuously reassert while their level matches. All are
sticky: leaving a matching level does not clear a previously raised flag, and
polling it while still matched allows it to reassert before a later input change.
POLLSEn, both JSEn/JNSEn polarities, WAITSEn with SETQ timeouts, and ordinary
interrupt source selections 4..7 work with every pin mode.

Reconfiguration clears the selected latch and establishes the new input as its
edge baseline. Selecting another already-high/low input does not invent an edge.
An immediately following local POLLSEn after configuring a matching low/high
level reads clear; later polls see the level and keep seeing it after consumption.
An old level notification already in flight can arrive after switching to an
inactive edge sensor. After that notification is consumed, the steady edge
sensor stays clear. This also explains why initial pin arming can appear to
generate an event when a previous level sensor was active; normalize the old
selection and drain its notifications before measuring the new source.

## Software hub FIFO

Measured on the attached P2AAS board on 2026-10-11. The nine `fifo-*` fixtures in
`tests/windtunnel/state/` cover all twelve FIFO opcodes, all eight cogs, every RAM
bank and CT phase, all byte alignments, mixed access widths, variable encodings,
block reloads, event/interrupt delivery, reconfiguration and hub-execution
handover. The visibility fixture uses a second cog to sample both portions of
cross-long writes around their physical commit edges. These claims concern the
software FIFO and instruction-fetch interface; streamer consumption is separate.

### Buffering, blocks and pointers

The shared FIFO has nineteen longs. Reads stop issuing when fifteen longs are
occupied; already-issued reads return five clocks after their rotating RAM-slice
grant. Software reads consume this buffered data, so editing hub RAM after
prefetch does not edit data already in the FIFO. Grants resume as consumption
releases space.

RDFAST/WRFAST use D[13:0] as a count of 64-byte blocks. Zero means 262144 longs,
covering the entire 1 MiB address map. D[30:14] and S[31:20] are ignored; D[31]
selects nonblocking setup. Finite wrapping requires a long-aligned start, as
specified in the silicon manual. Transfers obey the normal RAM hole and top
16 KiB mirror, including address rollover and byte enables.

FBW is raised on the grant of the last long in a block. In read mode this is a
prefetch event: it can occur before software consumes any bytes. In write mode
it occurs at the final physical write, rather than when WFLONG enqueues it.
RDFAST, WRFAST and FBLOCK clear the FBW latch when accepted, once per instruction;
blocking retries do not clear it again. FBW also feeds ordinary interrupt source 9.
WAITFBW has a direct FIFO wake path: after a pending wait sees FBW, the next
GETCT executes two clocks later, without the extra two-clock resume used by
other modeled waits. A pending WAITFBW timeout resumes one clock after its
absolute SETQ deadline; the following GETCT reads deadline + 3. An already-set
FBW adds no stall. Read/write wraps, polls and timed waits were checked separately.

FBLOCK replaces the descriptor used on subsequent reloads and leaves existing
buffered data alone. If a nonblocking configuration is still waiting for prior
writes to drain, FBLOCK instead replaces that pending initial descriptor.

A newly loaded cog starts with GETPTR = 0. GETPTR advances by the number of
bytes software consumes/enqueues and wraps at
20 bits. It advances linearly across FBLOCK reloads even when the new physical
block is at a noncontiguous address. Background reads/writes do not advance this
logical pointer. During hub execution GETPTR reports the next instruction's byte
address. On returning to cog/LUT code, the FIFO retains its prefetched data and
pointer, including the fetched word discarded by the branch. Software can then
consume that buffered instruction stream with RF instructions.

RFVAR/RFVARS consume up to four bytes. The first three contribute seven payload
bits and use bit 7 as continuation; the fourth contributes all eight bits. RFVAR
zero-extends 7/14/21/29 payload bits; RFVARS sign-extends them. RFVAR WC clears C;
RFVARS WC reports the decoded sign. Fixed-width RF reads report the width's top
bit in C. All RF instructions report zero in Z when requested.

### Setup and write timing

Let `t` be the first GETCT value in `GETCT start; instruction; GETCT finish`,
`b = (S >> 2) & 7`, and `c` be the cog ID. With no pending old writes:

| Operation | Raw `finish - start` |
| --- | --- |
| Blocking RDFAST | `12 + ((b - t - c - 5) & 7)` |
| Blocking WRFAST | `5` |
| Nonblocking RDFAST/WRFAST | `4` |
| Buffered RF/WF operation | `4` |

A queued FIFO byte becomes eligible two clocks after its WF execution edge. It
commits when `(CT + cog_id) & 7 == (address >> 2) & 7`. Only ready lanes in that
physical long commit; a wide unaligned write uses the following slice for its
remaining bytes. Neighbouring lanes are preserved, and ordinary hub accesses
lose any RAM window used by the FIFO.

Reconfiguration drains old writes before discarding/resetting the buffer. Write
setup overlaps that drain. For `WFLONG; GETCT start; WRFAST/RDFAST; GETCT finish`,
with no operand-augmentation instruction between the measured instructions,
let `w = (b - t - c) & 7` refer to the old write address. Blocking WRFAST measures
`max(5, w + 2 + (old_address_unaligned ? 1 : 0))`; blocking RDFAST measures
`w + 15`. Entering hub execution overlaps setup with pending writes. Its first
read can use a slice no earlier than `max(branch_clock + 5, last_write_clock + 2)`;
normal bank selection and the five-clock read response then apply. Stop/restart clears uncommitted FIFO
state; committed RAM writes remain.

### Nonblocking read startup erratum

The silicon manual warns about premature ordinary reads after a nonblocking
RDFAST. Distinct source/target addresses and an explicitly seeded ordinary read
bus reproduce the following ordering:

- A FIFO read before its initial data is available returns zero and does **not**
  advance GETPTR. The first data remains available for a later read.
- The initial software-read FIFO response shares the ordinary hub-read completion
  strobe. If an ordinary read is pending on that edge, it completes with the
  retained ordinary read-bus value instead of performing its own access. This
  includes an ordinary read starting on that edge.
- An ordinary read that completes before the initial FIFO response is unaffected.
  An ordinary write is not lost in the corresponding measured window.

`fifo-startup.propan` checks both sides of these boundaries for every cog, source
bank, CT phase and eight instruction gaps. The simulator models the response
ordering explicitly, rather than delaying all subsequent hub accesses. As the
manual requires, programs using nonblocking setup must allow setup to finish
before relying on normal FIFO data; these premature-access probes characterize
the measured silicon behavior.

## Shared RNG cog taps and instruction sampling (2026-10-11)

The retained probes and analysis are in [data/random](../data/random/README.md).
These measurements establish relations and sampling behavior, **not** a recovered
generator. The numbered xoroshiro output map, transition replay and HUBSET seed
lane/edge remain unresolved.

Synchronized GETRND captures from all eight cogs divide their 256 output bits into
64 classes. Each class appears in exactly four distinct cogs, with fixed relative
inversions. Each cog pair (0,1), (2,3), (4,5) and (6,7) covers all 64 classes without
overlap. Calibrating on 512 words/cog and checking eight independent uploads of
1,024 words/cog preserves all these relations across CT phases and both parities.
The anonymous numbering in `taps.json` does not identify canonical output bits.

For `GETCT before; random_consumer`, GETRND D, flags-only GETRND, BITRND,
DIRRND, OUTRND, FLTRND and DRVRND use the cog's word at `before + 2` in all 32
measured consumer observations. Port-B OUTRND uses the same word as port A.
GETRND C is bit 31; Z is bit 30. Its flags-only encoding sets instruction bit 18,
has D=0, and suppresses the register write.

For `GETCT before; WAITX D WC/WZ/WCZ; GETCT after`, all 24 measured cases give
`after - before = 4 + (D & RND(before + 2))`, with masks 0, 1, 3, 7, 15 and 31.
Requested flags clear; unrequested flags retain their values. Observers covered
both parities so that waits ending on odd clocks did not require interpolation.

The cog-0 HUBSET probe with even GETCT phases shows the following words delayed
by 2/4/6/8 clocks according to the hub-command slot. This observation does not
identify the internal overwrite edge or the scrambler pipeline delay. Official
documentation describes a partial 32-bit seed of 128 state bits; assuming that
HUBSET resets a standalone 32-bit or 64-bit generator would be unjustified.


### Seed-hammer recovery attempt and shared model (2026-10-11)

The proposed 4–7-cog repeated-zero-seed experiment is prepared as
[`data/random/seed-hammer.propan`](../data/random/seed-hammer.propan), with a
phase-wise modal-pattern analyzer. The first upload was rejected with
"No time quota left." After the user restored P2AAS availability, two independent
uploads succeeded. Raw frames and per-upload/pooled statistics are retained in
`data/random/`.

The experiment records 40,960 GETRND words: all eight cogs serve as observer,
with 0/4/5/6/7 other cogs repeatedly seeding `0x80000000`, across all CT phases.
Pooling by cog/count/actual phase gives 128 words per condition. No individual
bit or bit-pair XOR reaches 90% agreement, and no full word repeats within a
condition. Maximum byte-mode counts are 7/128 for baseline and 5/128 for each
seeded configuration. No useful seed-induced subset was identified. This is a
negative result for the measured outlier checks, not proof that all smaller or
higher-order seed correlations are absent.

Windtunnel now uses the documented/reference xoroshiro128** transition and a
pipelined scrambler, with a measured cog-incidence map and relative inversions.
The assignment of those anonymous taps to numbered output bits is arbitrary.
The historical upper-S1 seed lane and command-edge/pipeline ordering are explicit
simulation approximations. They must not be cited as recovered production
hardware facts. Exact numbering, absolute polarity, seed lane and internal delay
remain open; see [the research notes](../data/random/README.md).

## REP and instruction skipping (2026-10-12)

The `repeat-*` and `skip-*` fixtures in `tests/windtunnel/state/` contain
hardware-derived results and timing assertions. REP uses D[8:0] as its length;
zero length disables repetition and zero S repeats indefinitely. A new REP
replaces the single context. Local repetition has no branch overhead; hub
repetition performs a hidden branch with the normal FIFO refill delay. Every
taken explicit branch cancels REP, including branches in the middle of the body
and absolute branches. A conditionally canceled branch does not cancel it.

Relative branches use the next PC after REP wrapping or SKIPF stepping. Saved
CALL return addresses use that same nonlinear fallthrough. At the last body
instruction of an intermediate iteration, the relative base is the repeat start;
on the final iteration it is sequential. REP preparation occurs before execution:
a REP 0 at an older repeat boundary cannot undo an already-prefetched wrapped
instruction. Fast skip stepping consumes its bits before REP wrapping, even when
that wrapping overrides the stepped PC.

SKIP cancels each selected pipeline instruction in two clocks, suppressing
register, shared-command, branch and unsupported-instruction effects. SKIPF
steps over up to seven following selected local instructions per encounter.
A leading all-one pattern therefore cancels instructions 1, 9, 17 and 25 and
steps over the others. With `GETCT; AUGD; SKIP/F; 32 ADDs; GETCT`, SKIP takes
70 clocks for every measured pattern. SKIPF with 0/1/7/8/9/16/32 leading ones
takes 70/70/58/56/56/42/14 clocks. Both execute `32 - popcount(pattern)` ADDs.
SKIPF in hub execution falls back to ordinary cancellation; it does not fault.
EXECF installs its 22-bit pattern and branches to D[9:0] in cog/LUT execution.

JMP continues consuming the skip pattern at its destination. Hardware-stack
CALL/CALLPA/CALLPB suspend it; CALLA/CALLB/CALLD do not. Suspension uses a
separate **four-bit counter**, incremented by CALL when the remaining pattern is
nonzero and decremented by RET/_RET_ when the counter is nonzero. Sixteen calls
wrap the counter to zero; RET at zero does not underflow. Replacing the skip
pattern resets the counter. Fast stepping can consume the remaining pattern
before CALL executes, in which case that CALL does not increment it.

**POP does not resume skipping.** POP of the return address, PUSH/POP pairs,
restoring the popped return address and overwriting all eight stack entries leave
the suspension counter unchanged. A synthetic `PUSH destination; RET` decrements
it. These observations distinguish a separate counter from flags stored in stack
entries; see `skip-stack.propan` for the measured cases.

Interrupt service routines do not consume skip bits, but CALL/RET inside them
still update the suspension counter. GETBRK WC returns depth in bits 31..28 and
SKIP mode in bit 27, with C equal to the current pattern's low bit. GETBRK WZ
returns the remaining pattern and sets Z when it is empty. REP suppresses
ordinary interrupt scheduling until the final body slot has passed read/decode;
the queued CALLD then executes after that slot and before the following ordinary
instruction. The repeat/skip interrupt fixture checks the boundary and nested
CALL behavior against hardware.

For the hub timing sweep `GETCT start; REP 4,3; WAITX phase; ALTD index,0;
GETCT stamp; ADD index,1; GETCT finish`, the first stamp is
`start + 8 + phase`. Each later stamp is the preceding stamp plus
`19 + phase + ((bank(body) - preceding_stamp - 7 - cog) & 7)`, and finish is
four clocks after the third stamp. The fixture sweeps all eight cogs and banks
at long alignment, plus the three unaligned offsets at bank zero for every cog;
every run checks eight phases (704 timing cases in 88 runs). When a hub repeat starts at address
zero after twenty-bit wrap, its hidden branch selects cog execution, just like
an ordinary branch to zero; sequential hub fetching across wrap stays in hub
execution.

The 56 hardware runs in `branch-a-low-bits.propan` exercise raw immediate-relative
JMP and CALL A fields in cog and LUT execution, including byte offsets +1..+3,
+5..+7, -9..-11, and aligned controls. A local branch sign-extends the 20-bit
byte displacement, then shifts it arithmetically right by two before adding it
to the prepared next PC. Thus +1..+3 select the same long as zero and -9..-11
select the long three positions before the prepared next PC. CALL saves the
prepared next PC together with C/Z; neither instruction changes C/Z. All tested
paths took 24 clocks from the common GETCT start to GETCT at the destination
handler, and all 56 Windtunnel oracle comparisons passed. The probe also checks
cog 0 and cog 7 for representative signed offsets.

The original local-PC overflow mismatch was subsequently recovered and fixed;
see [the overflow measurements](#local-pc-overflow-and-loc-2026-10-12).

## XBYTE bytecode execution (2026-10-12)

Measured with the local P2AAS board. The six `xbyte-*.propan` fixtures in
`tests/windtunnel/state/` cover 104 runs. The lookup matrix checks every one of
512 nine-bit mode values against every bytecode, for 131,072 dispatches, spread
across all eight cogs. Its independently calculated digests include the selected
LUT address and C/Z. Each dispatch also checks PA, PB and the GETBRK mode field.
[Measurement summaries and replay instructions](../data/timing-data/xbyte/README.md)
are retained with the fixtures.

A hardware-stack RET or completed `_RET_` operation with the `$001FF` sentinel
starts XBYTE and retains that stack entry. POP consumes the sentinel normally;
an ordinary nested CALL/RET uses its own return entry. XBYTE replaces the prior
skip pattern and its call depth with a fresh fast-skip context. The LUT long
supplies a ten-bit cog/LUT branch target and a 22-bit SKIPF pattern. Trailing
pattern bits are canceled on the next bytecode dispatch. Cog and LUT handlers
produce the same observed results and timing.

Mode lookup rules, with `m` the nine-bit mode and `b` the bytecode:

| `m[3:2]` | Index width | Selection and base |
| --- | --- | --- |
| `00` | 8, with optional compression | Bank is `m[8]`. Let `t=m[7:4]` and `g=b[7:4]`. For `t=0` or `g<t`, use `b`; otherwise use `(t<<4) + g - t`. Bit 1 is ignored. |
| `01` | 7 if bit 4 is clear, otherwise 6 | Bit 1 selects the highest or lowest bits of `b`; the remaining upper address bits come from `m`. |
| `10` | 5 | Bit 1 selects the highest or lowest five bits; upper address bits come from `m`. |
| `11` | 4 | Bit 1 selects the highest or lowest four bits; upper address bits come from `m`. |

Bit 1 selects the highest bits when set. Bit 0 enables writing LUT-address bit 1
to C and bit 0 to Z; otherwise both flags retain their prior values. PA always
receives the complete zero-extended bytecode, including bits not used for lookup.
The duplicated five-bit lower/upper selector in the shipped mode table is a
text error: modes with low nibble `$8/$9` select the low five bits, and `$A/$B`
select the high five bits. Six-bit selection uses low five mode bits `$14..$17`.

`_RET_ SETQ` establishes the persistent mode. `_RET_ SETQ2` selects a mode for
one dispatch, then a subsequent RET/_RET_ restores the persistent mode. GETBRK
WC reports the effective mode in bits 24:16, including the temporary mode while
its handler runs; bit 25 reports the stack sentinel. Mode operands are masked
to nine bits. An ordinary SETQ followed by a separate RET changes Q without
changing the persistent mode. The phantom lookup does not replace Q with the
LUT long, unlike an ordinary RDLUT.

The measured overhead is six clocks. A single `_RET_ ADD` handler repeats every
eight clocks; ADD followed by an explicit RET repeats every ten. PA and PB are
available to the first handler instruction; PB is the FIFO pointer after the
byte read. An empty FIFO dispatches byte zero without advancing PB or waiting
for a refill. The nonblocking-RDFAST probe checks both sides of the first-response
boundary for cogs 0 and 7. Blocking `_RET_ RDFAST` redirects the byte stream and
starts the next dispatch only after setup completes.

A CT1 phase sweep checks ordinary interrupt entry around dispatch and within
multi-instruction handlers, including saved return PCs and ISR snapshots of PA,
PB and GETBRK. The phantom sequence prevents interrupt injection until its branch
completes. Local PC overflow has separate coverage below; interactions with the
absent streamer remain outside this evidence. Reset cancellation during each phantom
phase and direct debug-ROM fetches are covered by simulator unit tests.

## Local PC overflow and LOC (2026-10-12)

Measured through the local P2AAS board, using LOC for non-branching observations,
CALLD for saved return addresses, and timed CT1 interrupts for long instruction
streams. Eleven registered `local-pc-*` fixtures provide 207 cases. Each case has
two independent retained reporter captures and a third independent oracle
comparison of its register signatures and CT differences. The captures and replay
instructions are in [local-pc-overflow](../data/timing-data/local-pc-overflow/README.md).

### Logical PC and instruction source

Sequential cog/LUT execution increments the full **20-bit PC by one**. Neither
crossing `$3FF` nor a higher carry changes execution mode. Twenty-bit rollover
returns the PC to zero. The instruction source is a separate selection:

| PC condition | Instruction source |
| --- | --- |
| Bit 9 set, including above `$3FF` | LUT at `PC[8:0]` |
| PC below `$200` | Cog instruction address `PC[8:0]`, with the normal execute-only debug-ROM overlay at `$1F8..$1FF` |
| PC at least `$400`, bit 9 clear | Architectural cog S read port of the preceding instruction |

The last selection follows the preceding instruction's nine-bit S **address**,
including immediate operands and conditionally canceled instructions. ALTS changes
that address; ALTD changes only D. AUGS changes the immediate value without changing
the fetch address. Thus NOP at `$3FF` selects cog register zero at logical PC `$400`;
`ADD value,#3` there selects cog register 3 for the next instruction. A CALLD fetched
at logical PC `$401` saves `$402`, regardless of its physical source register.

The S-port path reads architectural special registers. Controlled branches in
PA/PB/PTRA/PTRB/OUTA/OUTB select the architectural value rather than a different
instruction installed in the underlying tail RAM. Selecting PA/PB through this
port does not execute debug ROM. INA/INB are live pin inputs; uncontrolled pin
levels are not asserted as fixed instruction words in these fixtures.

A same-clock cog RAM result write and overflow S-port fetch returns the **old RAM
word**. A probe copies an ADD into cog register 1, then fetches register 1 on the
write clock; the original NOP executes before the new ADD becomes visible. Timed
interrupts check the resulting instruction counts on cogs 0 and 7. This evidence
is specific to the overflow read-port path; it does not establish a universal
ordinary instruction-fetch collision value for every physical chip.

Higher carries retain the same selection. With an ADD/NOP S-port loop and an
explicitly initialized all-NOP LUT, CT1 captures at PC `$800`, `$C00`, `$8400`,
`$80400` and `$FFFFE` expose the intervening LUT fetch windows. A separate probe
runs through the entire twenty-bit rollover, then CALLD from cog register 1 saves
`$002`; its measured GETCT difference is **2,095,126 clocks**.

### REP, skip, branches and interrupts

REP retains its existing countdown and local rewind behavior. Its prepared
fallthrough can differ from the executing instruction's PC. Sixty LOC cases
cover lengths 0..4, counts 1..3, and paths through either a direct S-selected
CALLD or a non-branching LOC at cog register zero, on cogs 0 and 7.

The original retained `REP 2,3` at `$3FE`, `ADD value,#1` at `$3FF` mismatch now
produces **one**, matching silicon. That ADD selects cog register 1, which holds
the exit branch. It does not first execute the ADD at cog register zero. The
exit branch cancels repetition. Changing REP's countdown alone would give the
wrong explanation and would not fix ordinary overflow dispatch.

SKIPF increments the full logical PC while fast stepping across the boundary;
SKIP cancellation and EXECF use the same physical source selection. Thirty-six
cases cover patterns 0, 1, 2, 3, 6 and 14. Absolute and relative CALLD, explicit
RET and implicit return preserve the full saved/fallthrough PC. A branch or return
then selects its normal target domain: targets below `$400` select local execution,
and larger targets select hub execution. RETI1 and RESI1 returning to a saved
`$404` therefore execute the controlled hub instruction at `$404`, rather than
continuing the overflow local stream.

Interrupt entry records the full PC. CT1 phase probes check saved PC, LOC results,
counts and clock differences; interrupt exit and FIFO refill follow the existing
branch paths. The fixture harness validates physical local code ownership while
keeping logical PCs in traces and exit checks. A local PC numerically equal to
the reporter's hub address is not a fixture exit.

### LOC displacement

LOC adds the encoded twenty-bit A field **directly in PC units**, modulo twenty
bits, in both local and hub execution. It does not apply JMP/CALLD's local
arithmetic right shift by two. Eighteen raw-encoding cases check A values `$004`,
`$204` and `$FFE04` (−508), in ordinary cog execution, at LUT `$3FF`, and at logical
PC `$400`, on cogs 0 and 7. For example, `LOC PA` with relative A=`4` at `$3FF`
produces `$404`, and at `$400` produces `$405`. Windtunnel's previous reuse of the
branch displacement helper for LOC was corrected with this evidence.

Propan now encodes relative LOC in execution-PC units separately from branch A
displacements. A local `LOC PA,target` at PC zero, with target at PC two, emits
`$FE900001`; the old `$FE900004` located PC five on hardware. The assembler fixture
`tests/propan/sema/loc-addresses.propan` checks labels, numeric addresses, relocated
cog/LUT segments, hub byte offsets and PIC selection. The original simulator
measurement fixtures retain their raw LOC words to preserve the captured probes.

### Interrupt return can change the execution domain

The retained `local-pc-resume` measurements already show an unmodified RETI1 or
RESI1 returning to hub `$404` after interrupting the overflow local stream on cogs
0 and 7: two ADDs, saved PC `$404`, hub landing marker one, and 55 elapsed clocks.
The original source never patches IRET1. The return branch chooses its execution
domain from the saved numeric PC; the register does not preserve the old mode.

`tests/windtunnel/state/local-pc-iret.propan` adds a finite reproduction with a
no-interrupt control and INT1/INT2/INT3 on cogs 0 and 7. Two NOPs at LUT `$3FE..$3FF`
enter a cog S-port chain at logical `$400`. Without interruption, six ADDs produce
21 and exit locally. CT1 interrupts before local `$404`, after four ADDs produce
10. The ISR copies the full IRETx (`$C0000404`, including C/Z), disables its event
source, and executes RETIx without changing the return register. Hub `$404` sets
a distinct marker and LOC observes hub PC `$40C`; both flags are restored and the
original IRETx remains intact. All eight cases pass in simulation. Live hardware
validation of this expanded fixture was unavailable in this session: the existing
oracle probes failed with a transport error at `ws://localhost:12880/`. These new
assertions are not additional silicon measurements.

For future assembler hardening, local fallthrough across `$3FF` is hazardous even
when the core stays in local execution: an interrupt or call can save a PC that a
later return interprets as a hub byte address. Detecting only explicit local-to-hub
branches would miss this path. The simulator preserves the behavior so a future
diagnostic can be tested against a reproducible failure.

These measurements cover the observable overflow paths above on the connected
board. They do not claim every combination of ALTI substitution, missing streamer
activity, or uncontrolled pin-driven instruction words, nor a board/revision-wide
rule for unrelated RAM collision paths.
