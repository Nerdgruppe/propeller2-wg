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
| `hub.cordic.returns.pending()` | Tagged result transport after leaving the arithmetic pipeline |
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
The simulator regression fixtures remain in `tests/windtunnel/state/`;
the measured rules are summarized below.

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
comparison of its register signatures and CT differences. Regression fixtures
remain in `tests/windtunnel/state/local-pc-*.propan`.

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
original IRETx remains intact. The original eight cases passed in simulation. Two additional INT1 cases (cogs
0 and 7) target the first overflowed PC, `$400`, using CT1 phase 12. The HUB
instruction at `$400` branches to a distinct landing marker (2); continued
local execution would instead run the cog S-port chain. Both cases assert the
untouched saved IRET1 (`$C0000400`), zero completed ADDs at interrupt entry,
and the HUB marker. The additional cases have not been run against hardware or
the simulator in this change. Live hardware
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

## Address domains used by debugger inspection

The GUI/debugger implementation follows the simulator's existing address rules:
architectural `$1F8..$1FF` values, the underlying eight tail-RAM longs, and
execute-only ROM are distinct views. A carried local PC can fetch from the LUT
or reuse the operand S port; its physical source is not always `PC & $1FF`.
Saved return words carry C/Z in bits 31/30 and the target in bits 19..0; flags
must be removed before classifying cog, LUT or hub references. Every physical
stack slot can retain a matching address, so inspecting these slots does not
establish a call depth. GPIO pad output enable follows delayed physical DIR;
smart modes use TT independently of the DIR-controlled engine enable.

These are inspection rules derived from the existing model, not new hardware
measurements. See [debugger.md](windtunnel/debugger.md) and the headless
[regressions](../src/windtunnel/debugger_tests.zig).

## Non-smart adjacent-feedback input selection (2026-10-13)

The official [hardware manual's pad-mode table](p2/hardware-manual.md#m-pin-mode)
specifies **Adj Logic** input for pad mode `%0010` and **Adj Schmitt** input for
`%0101`; both modes also drive their output from that input. This selection is
part of the physical pad mode, independently of the later A/B relative selectors.
It also applies while DIR disables the output.

The Windtunnel survey found that its output feedback selects the adjacent pad,
but its input route still selects the local pad. A simulator probe with P0 high,
P1 low, P0 DIR=0 and mode `%0010` returned IN0=1 instead of the documented adjacent
input value 0. This is a source/specification discrepancy, not a new silicon
measurement; see [STATE.md](../STATE.md).


## COGID, protected hub stacks, and digital input filters (2026-10-13)

Measured using the local P2AAS board at `ws://localhost:12880/`. The findings are
summarized below; input regressions remain in `tests/windtunnel/state/input-*.propan`.

### COGID

An immediate operand without WC completes without changing either flags or the
register named by the encoded D field, including with AUGD. This form does not
have the additional two-clock result delay of register ID and WC status forms.
The phase sweep measures GETCT-to-GETCT intervals of 4–11 clocks for immediate
without WC, versus 6–13 for the two result forms. All eight executing cog IDs
were exercised. Timing checks derive the next grant from the measured CT, since
WAITCT1 may resume with a different issue phase. WC queries use D[3:0]; IDs 8–15 return zero rather than aliasing
IDs 0–7. WC preserves D, and augmentation does not change the four-bit ID rule.

### CALLA/CALLB inside INT0

Both immediate-target and register-target CALLA/CALLB write their packed return
address through the protected upper alias when executed inside a debug ISR.
The same four forms in application code leave protected stack memory unchanged.
Authorization belongs to the issued transfer, as for ordinary WRLONG; simulator
regressions also cover a debug-state change while that transfer is stalled.
These measurements do not resolve FIFO write privilege.

### Adjacent feedback and unanimous input filters

Pad modes `%0010` and `%0101` read the paired pad before input inversion and
A/B relative selection. The hardware fixture makes the driven local pad the
opposite of its paired pad using output inversion, and tests both parities,
Schmitt/logic variants, synchronous sampling, input inversion, and a remote
consumer of the routed input. Output-disabled routing is covered with synthetic
external drivers in the simulator.

Boolean modes produce `(A,B)`, `(A AND B,B)`, `(A OR B,B)`, and `(A XOR B,B)`.
Filter modes retain independent A/B eight-bit histories and outputs, using the
selected global setting's 2/3/5/8 low history bits for unanimous decisions.
WRPIN, DIR transitions, switching filter settings, and HUBSET reconfiguration
preserve history. Bypassing the filter stops its sampling. Shortening a frozen
partial history can change the output without another input sample. A newly
enabled frozen filter retains its initialized low state rather than adopting A.

CT-aligned traces for taps 0, 1, 2, and 5 fit a sampled timing tap half a sampling
period ahead of GETCT's phase (tap 0 samples every clock). Windtunnel detects both
transitions of that phased tap. History comparison and two output sampling
stages use preceding-edge values. A threshold sweep distinguishes the two-clock
OUT-selector input sampling delay from the filter delay: after OUTH, TESTP first
sees a bypassed OUT input with two intervening NOPs, or filt0 length two with four.
The same latency model matches rising/falling traces across the tested lengths
and taps. WAITCT1 can resume the probe four clocks later; long-tap traces normalize
that observed CT phase before comparison rather than assuming a fixed wakeup. Tap 31 and counter rollover are checked synthetically; no multi-second
silicon capture of the highest tap periods is claimed.

## RCFAST UART without startup waits (2026-10-13)

`HUBSET #$F0` selects RCFAST, the boot clock source. RCFAST is specified as
20 MHz or higher, so assuming exactly 20 MHz for a UART divisor is insufficient.
The [ROM loader](p2/bootrom/bootrom.spin2) calibrates TX pin 62's X register
against the host's serial timing. Before launching the image, its `reset_pins`
routine clears DIR and WRPIN modes but leaves that calibrated X register intact.
Hardware confirms that restoring asynchronous TX mode without WXPIN reuses the
calibration successfully while the clock remains on RCFAST.

[hello-world-no-pll.propan](../examples/hello-world-no-pll.propan) keeps `$F0`,
restores TX mode, and sends immediately using that inherited divider. It contains
no WAITX, requires neither a crystal nor the PLL, and polls RQPIN's busy flag
before each byte and after the last stop bit. This version requires ROM serial
loading and a receiver that stays at the upload baud rate. P2AAS uses 2 Mbaud for
uploads, so request `baudrate=2000000`. Other upload rates can also be inherited;
SPI/SD boot does not provide this serial calibration.

Matching the upload baud removes a host baud-change race. An earlier crystal
probe at 115,200 baud, without its 250 ms host startup wait, captured complete
output in 10/10 runs with 1,000,000 RCFAST settling clocks and 10/10 with 600,000,
but only 6/10 with 300,000 and 6/10 with 150,000. The failed captures contained
corrupted bytes. Matching 2 Mbaud avoids that change instead of estimating a
safe host delay.

The final RCFAST example captured exactly the 14 bytes `Hello world!\r\n` in
100 consecutive reset/upload runs through `ws://localhost:12880/`, with no
missing or extra bytes. The focused simulator regression in `test_suite.zig`
seeds the ROM handoff's X register with three different integer/fractional baud
dividers and verifies preservation, complete output, `$F0`, and final drain.
Loaded images now receive that nominal serial-ROM handoff in the fixture runner,
CLI, and simulator server: `$F0`, cleared modes/DIR, and X=`$000A0007` on RX/TX.
Raw `Hub.init` still models hardware reset. The shared oracle and program fixtures
retain those dividers and need no initial WAITX. UART drains poll busy instead of
assuming a frame duration. The timing-data fixture preserves its original DUT
origin (`$2C`), since shifting the source image changes COGINIT loading latency.
Tools were built with `zig-0.16.0`. Recheck with:

```sh
zig-0.16.0 build test-windtunnel-unit -Doptimize=ReleaseFast -Dcoverage=false
zig-out/bin/windtunnel-tests --oracle tests/windtunnel/program/hello-world.propan
```

During this refactor, `debug-context.propan` produced only its first expected
long followed by zeros on silicon. The same result occurred with its original
200 MHz PLL/115,200-baud setup and a conservative 100 ms UART wait, and with
RCFAST/2 Mbaud using explicit cog-completion polling. Thus the discrepancy
predates the UART change; its exact debugger semantics remain unresolved. The
fixture retains its simulator assertions and explicitly disables hardware
comparison; the measured discrepancy is summarized here for follow-up.

The RCFAST conversion exposed two GPIO boundary expectations derived at 200 MHz:
`OUTL; WAITX 4; TESTP` and `OUTL; WAITX 5; MOV INB` now observe the new low level.
Both were low across all eight phases and ten repeated uploads. A separate
PLL sweep found both old high samples stable at 170, 180 and 200 MHz, and both
new low samples stable at 40, 80, 100, 120, 140, 160, 165, 166 and 167 MHz.
Samples at 168 and 169 MHz varied. These are loopback measurements; they do not
separate physical output and input propagation. The simulator initially represented the
stable bands with an effective GPIO drive edge at execution+4 below 170 MHz
or +5 at/above 170 MHz. That approximation has since been removed: the optimistic
digital model uses execution+4 at every frequency, excluding electrical pad delays.
The 82 timing cases/results in `data/timing-data/smart-pin-{cases,results}.yaml`
use RCFAST expectations.

Final validation used `zig-0.16.0` ReleaseFast: all 2,564 registered fixture runs
passed, with 309 unit tests passed and two skipped. Hardware comparisons passed
163 cog/pipeline/input runs, 82 smart-pin runs, and 51 program runs. The 11
instruction-timing probes, all 82 eight-phase smart-pin timing probes, and the
terminal/full-size-loader protocol checks also passed at 2 Mbaud.

## Bench wiring verification

On 2026-10-13, the two-board bench passed slow GPIO continuity and mapping
checks: Logic 16 channels 0–7 map to the P2AAS board's P0–P7, and channels
8–15 map to P32–P39. P8–P15 connect in order between that board and the
Parallax P2-EVAL with serial ID `PLX6ZJKWX`; all 256 byte patterns matched
in both directions using 1.5 kOhm drive and a 150 kOhm receiver pulldown.
The analyzer mapping matched three walking-one/walking-zero frames at
500 kS/s. These establish slow digital connectivity, not high-speed signal
integrity; the mapping above records the verified connections.


## Digital smart pins: measured engine and return-bus behavior

The 2026-10-13/14 two-board validation covers all twenty digital modes (4–23)
at RCFAST and 200 MHz. Simulator regressions live in
`tests/windtunnel/state/smart-pin-*.propan`.
The following details matter when separating a smart engine from its physical pad:

- Pulse/transition base dividers include an extra startup clock. Y requests are
  accepted after the current divider boundary. Transition Z reports the parity
  of the completed Y request, which can differ from the physical output level
  after successive requests.
- NCO Z is an overflow publication bus, not a continuously readable accumulator.
  Without publication, earlier Z remains visible. A single arithmetic operation
  per base boundary does not explain short-divider readback and reset retention.
  The model derived from consecutive reads and waveform samples separates the
  base counter, registered accumulator, physical adder output and result latch.
  The accumulator consumes the preceding divider boundary; carry publication at
  the current boundary captures that registered value. DIR sampling gates these
  paths separately. Phase writes replace the high sixteen accumulator bits.
- PWM duty is captured at frame boundaries. Z duplicates the frame-length field
  in both halves. Reducing an active sawtooth frame below its current position
  does not cause an immediate boundary: the equality comparator must be reached
  after wrapping the sixteen-bit position. SMPS A stalls the next frame and B
  inhibits the remainder of the current frame.
- Gated/up-down rise counters use the preceding sampled B at an A rise. Continuous
  counter Z precedes the current adder update. Window completion carries the
  boundary event into the next window. Count arithmetic wraps through 32 bits;
  duration measurements saturate at 0x80000000. State-duration C is the completed
  state, independently of the result sign bit.
- X=0 means a full 2^32 count/window for fixed-period and minimum-window modes,
  rather than an immediate completion. These long limits are exercised with
  synthetic engine state, not a claimed 2^32-clock hardware capture.
- Selectable pin events see an input stage one clock fresher than TESTP/INA. Clear
  SETSE and consume the pending event before measuring a new rising-edge source;
  otherwise a loader-era notification can satisfy WAITSE immediately.

The stable RCFAST/200 MHz acceptance bands do not establish exact electrical
behavior at every PLL setting. With a 1 MHz PLL reference and the analyzer removed
from P40, later GPIO sweeps already showed the longer path at 166/167 MHz. The
frequency-only 170 MHz simulation boundary was an approximation and has since been removed. Changing
clocked output selection with DIR high can expose one-clock physical latch
transients; exact transient phase is not modeled. Configure with DIR low and
allow the pipelines to settle for deterministic comparisons.


Final software validation with `zig-0.16.0` passed all 3,718 registered runs,
323 unit tests (two skipped), and 220 Debug harness/debugger tests. The final
physical-pin VCD matched 368 three-channel frames at 100 MS/s and 552 four-channel
frames at 50 MS/s within two samples, after independent GPIO-marker calibration.
The external board checks passed 276 observations. The optional NCO first-completion probe then expired its requested one-second
runtime allowance. That close was initially misclassified as shared quota exhaustion;
the configured P2AAS allowance is 60 seconds per run. The additional ten-repeat
campaign and optional NCO first-completion measurement were not collected. Those are explicitly uncollected; saved silicon
traces cover the final NCO return-bus corrections. The evidence manifest records
this boundary rather than claiming fresh post-correction uploads.


## Serial smart pins: live configuration and retained state (2026-10-14)

An additional 36-case GPIO-to-idle-UART-to-GPIO probe (144 observations) checks
immediate WRPIN output selection at RCFAST and 200 MHz. With cog OUT already high,
selecting UART TX without WYPIN keeps the observed pad high. With OUT low, the first
high TESTP sample follows `WAITX 4` at RCFAST or `WAITX 5` at 200 MHz; the preceding
offset still reads low. Returning to GPIO with OUT low first reads low after
`WAITX 3` or `WAITX 4`, respectively. GPIO OUT remains unchanged throughout.
These are instruction-visible boundaries, including input sampling; they establish
delayed selection in both directions without proving an exact physical pipeline
topology. Filling an output queue with UART's idle level on WRPIN is not justified
by these measurements. The electrical timing observations above remain distinct
from the optimistic fixed-cycle simulator model.

The UART/USART campaign uses primary P2AAS exclusively and the independently
clocked secondary on the existing P8–P15 wiring. Simulator regressions live in
`tests/windtunnel/state/smart-pin-serial-*.propan`.
These are measured properties, including undocumented behavior relevant to RMII:

- Both RX engines compare the live X word-length field during reception. Extending
  4 to 8 or 8 to 16 bits retains already shifted data; shortening before the new
  terminal count completes at that count. Shortening below the current count stalls
  in the tested long streams: over 64 synchronous edges and over 5,000 UART clocks
  did not produce completion. Reset recovers reception.
- UART's live length comparison remains open after the nominal final data sample,
  through the following bit interval before Z capture. Extending eight to sixteen
  bits in that interval still produces the original sixteen-bit word. Single-clock
  writes around the capture boundary distinguish this from committing the length
  at the last sample or waiting until the delayed IN publication.
- UART divider writes alter a running reception without restarting the current
  interval. Subsequent reloads use the new divider. The retained 64-to-32 and
  64-to-128 captures distinguish these cases from fixed whole-frame deadlines.
- RX Z and the underlying 32-bit shift register survive DIR reset. With synchronous
  eight-bit all-one words separated by reset, Z progresses from `0xff000000` to
  `0xffff0000`. UART's second such word is `0xff7fc000`, exposing intervening
  start/stop samples. Only the configured high bits constitute the current word.
- Same-mode WRPIN during a partial synchronous or asynchronous word preserves
  receive state while acknowledging IN. A mode-only host configuration must not
  silently reset reception on those writes.
- USART X[5] selects the preceding or coincident A sample relative to registered B.
  Output buffering differs between continuous and start/stop modes. During reset
  the Y buffer primes the shifter; re-enabling after a partial word restores its
  initial buffered bit without another WYPIN. UART WYPIN while held reset does
  not queue a transmission.
- TX shifts routed A back into the high bits of its serial shifter. RQPIN exposes
  the running UART shifter, including an unfinished frame retained on reset;
  USART captures the pre-final-shift word at the word boundary. A forced-high A
  source distinguishes this from rotating the transmitted payload.
- Buffered UART words retain the fractional baud phase across the stop/start
  boundary. Quarter-clock and half-clock periods with odd/even word widths
  distinguish continuous phase from restarting a rounded frame deadline.
- USART TX/RX loopback at clock/2, clock/4 and clock/8 matches the on-chip captures
  at both RCFAST and 200 MHz, with both X[5] receive positions and 8/16/32-bit words.
  These fast checks use NCO-generated clocks and register observations, not the
  bandwidth-limited analyzer.
- UART busy is separate from Z's sign and clears in reset even when retained Z
  has bit 31 set. IN acknowledges buffer-to-shifter movement, not TX completion.
  UART receive-ready publication and selectable events have measured pipeline
  delays; the eight-phase fixture fixes their instruction-visible boundaries.
- Fractional UART periods apply only below 64 integer clocks. The campaign covers
  periods 3, 4, 5, 7.25, 7.5, 63.5, 64.5 (fraction ignored), and 65,535 clocks. At the
  minimum period, idle sampling can populate the unused low receive bits; tests
  retain the observed `0xef3fffff` result instead of assuming zero padding.

The independent-board checks cover all 1–32-bit UART/USART widths and a live
8-to-16-bit receive extension. The clone analyzer uses four channels at 50 MS/s;
500 complete UART/USART frames match the simulated physical pin edges within two
samples, using independent GPIO markers to calibrate clock scale. Fast serial
behavior is additionally checked on-chip rather than inferred from overloaded
analyzer captures.

### GPIO loopback sampling crossover, 2026-10-14

Fresh hardware-only GPIO probes reject 170 MHz as a universal architectural timing
edge. With fast drive on P40 and eight-clock OUT-to-TESTP instruction-start spacing,
rising-edge samples were mixed already at 164–166 MHz; falling-edge samples were
mixed at 168–169 MHz. At 167 MHz, every rising edge was missed and every falling edge
observed in 1,024 trials per polarity and lead-in phase. Nine-clock spacing observed
both polarities throughout the fast-drive sweep. The internal OUT-input selector
showed no corresponding crossover. At fixed 120 MHz, 1.5 kΩ drive moved TESTP's first
consistently changed sample from eight to ten clocks. These observations are
consistent with electrical propagation crossing an input sampling boundary, not
proof of an additional registered output stage. OUT-to-input loopback does not
isolate DIR-gate delay. The simulator's former 170 MHz output/DIR branch was an
approximation, rather than a measured hardware cutoff; it has been removed.
The optimistic model retains fixed cycle delays (one clock for selected output,
three for optional synchronization, three for DIR after writeback), without adding
an electrical model. Nominal clock decoding and wall-time conversion remain.

The original falling-edge-only sweep used `OUTL; WAITX 4; TESTP` and separately
`OUTL; WAITX 5; MOV INB`: instruction-start spacings eight/nine, since WAITX takes
its operand plus two clocks. Fresh GETCT checks confirmed this arithmetic, and a
25 MS/s two-channel clone-analyzer capture independently confirmed the 13 main
PLL settings within 0.018% of nominal. The analyzer did not resolve single-clock
pad propagation. The campaign collected 1,720,320 timing samples from 169 hardware runs.

A repeat on the second P2-EVAL (PLX6ZJKWX), using P0..P7 with only their stub traces
attached and no analyzer, collected a further 3,686,400 samples in 360 direct-serial
hardware runs. At eight-clock spacing, P6 observed exactly half the falling edges
at nominal 160 MHz in both lead-in phases; P1..P5 and P7 observed all of them.
At nominal 165 MHz, P0 rising-edge observations ranged 803–904/1,024, P1 was
1,024/1,024, and P6 was 0/1,024. At nine-clock spacing every pin consistently
observed both polarities throughout the 40–200 MHz fast-drive sweep. Internal OUT
showed no corresponding crossover. These pin-dependent results further reject a
universal 170 MHz threshold. Since chip, bank and PCB changed, cross-board timing
differences cannot be attributed solely to loading. Second-board clock values are
nominal PLL settings; the first board's analyzer check does not independently
verify them.

### Constant-current GPIO slew and loopback timing, 2026-10-14

On unloaded P0..P7 of the second P2-EVAL PLX6ZJKWX, source and sink both set to
1 mA (`WRPIN 0x2400`), 100 µA (`0x2d00`) or 10 µA (`0x3600`) produce sharply different
instruction-visible GPIO timing. At nominal 200 MHz, first consistently changed
OUT-to-TESTP instruction-start spacings were 14 rising/15 falling clocks at 1 mA
on every pin; 66–68 rising/73–76 falling at 100 µA; and candidate boundaries
593–622 rising/654–701 falling at 10 µA. The latter intervals include the final
one/two-clock sampling resolution. Both polarities settled for 100 µs before
being driven in the opposite direction. Pins were individually measured with
no external wire or analyzer attached.

Across nominal 40/120/200 MHz, the 144 final pin/current/polarity boundaries fit
a shared approximately seven-clock base plus per-pin/current/polarity analog
threshold delay (0.24-clock RMS residual). Fitted analog terms were approximately
33/37 ns at 1 mA, 293–303/328–343 ns at 100 µA, and 2.93–3.07/3.24–3.47 µs at 10 µA
(rising/falling). The roughly inverse-current scaling and unchanged six-clock
internal OUT-input controls strongly support pad slew/threshold crossing, without
requiring a frequency-triggered transport stage. This fit includes the complete
loopback path; it does not isolate actual physical register placement, pad 10–90%
rise time or DIR-gate latency. Current, threshold voltage and clock frequency
were not independently calibrated on this board. 552 native direct-serial hardware
runs produced 1,999,872 binary samples; all 5,256 GETCT spacing calibrations and
native frame CRC checks passed.

### DAC-comparator searches and retained pad charge, 2026-10-14

On second-board P0..P7, the disabled adjacent pin can observe the DUT using
`P_LEVEL_B_FBP | (code << 8)` while its DIR stays low. The DUT retains its selected
drive strength. Using the comparator on the DUT itself would force 1.5 kΩ output
drive. The reference is the comparator's internal R-2R DAC; nominal conversion is
3.3 V × code/256, not an externally calibrated voltmeter.

After `OUT; WAITX 4; TESTP`, direct threshold searches are limited by the comparator/
reference path's own dynamic response. At nominal 120 MHz, ordinary fast-drive GPIO
reads the new level on all eight pins while the comparator searches still return
old-endpoint codes. Those codes therefore cannot be called instantaneous pad voltage.

A separate pulse-and-hold experiment configures the inactive polarity to float,
drives for eight clocks using two OUT commands separated by WAITX 4, then waits
1 µs before searching the retained voltage. Common OUT transport cancels from the
pulse length; this is a different time origin from the direct read. At 200 MHz,
P0's nominal held estimates after a 40 ns pulse were approximately 3.25/0.08 V for
fast drive, 2.04/1.28 V at 1 mA, and 0.19/3.13 V at 100 µA (rising/falling). At 40 MHz,
the 200 ns pulse yielded approximately 1.01/2.32 V at 100 µA and 0.09/3.22 V at 10 µA.
This current × time scaling strongly supports electrical charging/slew. An apparent
capacitance of order 20 pF follows only if nominal currents and DAC scale are assumed.

Hold-time controls at 1/10/100 µs moved by up to five codes (~65 mV); zero-pulse
controls stayed at endpoints on all pins. Offset, charge injection, internal observer
loading and retention prevent interpreting the 13 mV nominal code spacing as absolute
accuracy. All 464 voltage cases passed cycle, settled-rail and native-frame CRC checks;
2,784 searches collected 1,793,632 binary samples. Twenty independent code-neighbor
reprobes disagreed near a boundary, reinforcing the finite-code uncertainty.
The method and limitations are summarized above.

The original constant-current timing campaign's nominal 40 MHz setting used a
40 MHz VCO below the documented recommended range; its clock was not independently
verified on this board. The later voltage experiment uses 160 MHz VCO divided by
four for that clock. Earlier fixed-cycle-plus-nanosecond fits remain descriptive,
rather than isolated calibrated transport measurements.

## Streamer event latches at RCFAST (2026-10-14)

Native P2AAS cog fixtures at nominal 20 MHz RCFAST sampled POLLXMT/POLLXFI
before commands, immediately after XINIT, while a command was buffered, and after
WAITXMT/WAITXFI. Initial flags are clear. XINIT alone did not produce XMT.
A buffered XCONT eventually produced XMT, consumed by WAITXMT. Polling again
while the buffer remained empty returned clear. XFI likewise remained clear
following WAITXFI, including after an additional WAITX 10. These are transition
latches, not continuously reasserted idle/empty levels. Two chained commands of
8 and 4 samples at XFREQ=$08000000 took 202 clocks from GETCT before XINIT
(including its AUGD) to GETCT after WAITXFI. Temporary capture artifacts are in
`.zig-cache/streamer-probes`; retained regressions should assert the observations.

RCFAST two-clock MOV snapshots at both instruction phases resolve raw streamer
output one clock behind the simulator's existing GPIO OUT input path. TMDS
literal/RGB output has four total clocks of latency at this boundary. Before the first encoded
character arrives, TMDS publishes clock-positive, zero-data differential pairs
($56 forward / $6A reverse). Coarser MOV/AND snapshots initially suggested a
shared four-clock delay; the finer two-phase probe distinguishes the paths.
Forward/reverse ordering and ten-clock TMDS sequences agree with digital
snapshots. This models observable boundaries, not identified physical registers.

Zero-count stop probes distinguish idle from active commands. XINIT 0,0 from
idle leaves XMT/XFI/XRO clear. XCONT 0,0 from idle produces XMT only. From an
active eight-sample command at XFREQ=$40000000, immediate XINIT 0,0 leaves XFI
and XRO set; a buffered zero-count XCONT leaves all three set after completion.
The extra XRO can occur between command acceptance/flag clearing and command
port delivery. XCONT/XZERO third-command stalls and captured SETQ frequency
handoffs agree with four retained fractional/perpetual chain timings.

Eight nonblocking RDFAST startup phases followed immediately by a frequency-zero
RFLONG streamer command all produced zero output and left GETPTR at the starting
address after WAITX 100. The initial empty read does not advance the pointer or
retry when the FIFO subsequently fills. Full/slow-rate 64-long streams consumed
or enqueued 256 bytes. Mixing 20 software WFLONG instructions with a full-rate
64-long capture enqueued only 304 bytes, rather than 336: the eight software
writes overlapping capture displaced eight capture samples. This identifies a
shared access-port collision, not a RAM bandwidth or FIFO-capacity limit.
Streamer LUT reads of $1FF also trigger an armed local SETSE1 3 read sensor;
XRL and selectable LUT-read sensors both apply to streamer accesses.

Partial one-pin captures do not flush a partial byte or carry it across queued
commands: count 3 alone and chained counts 3+5 leave GETPTR unchanged and the
sentinel byte untouched; 3+8 enqueues exactly one $FF byte for a held-high input.
The paired-cog LUT probe samples both observer and command-start phases while
the companion writes a different LUT address every two clocks. The initial
lookup remains available at command publication and subsequent constant lookup
output is retained. The simulator represents the initial lookup as prefetched,
and retains the previous result for displaced ongoing reads. This is a timing
model consistent with the retained probes, not a measurement of individual RAM
port registers or arbitrary collision data.

## Color converter arithmetic and transport, 2026-10-14

Two P2 boards were used: the DUT exclusively through P2AAS and the P2-EVAL
PLX6ZJKWX as an independent ADC observer over direct serial. Recorded wiring
connects P8..P15 one-to-one. DUT P8..P11 output DAC0..DAC3 using the 75-ohm/2 V
channel mode; observer P8..P11 are ADC inputs. P12 carries a DUT timing marker;
observer P13 provides capture readiness. Measurement firmware releases all
experiment pins afterwards. This adds no analog simulation.

The observer runs at nominal 160 MHz. Settled observations use SINC2 periods of
8,192 observer clocks, discard three initial conversions, and average 512 or
1,024 samples. Difficult code comparisons use 8,192 samples. Initial 16-sample
averages and a single global calibration were insufficient: noise and common
ADC drift could exceed one DAC code. Calibrations sweep all 256 bypass codes on
all four channels; decisive checks compare each converter result against nearby
bypass controls on the same physical DAC/ADC channel. Calibration converts to
DAC code, not externally calibrated voltage.

For timing, the DUT was slowed to nominal 1 MHz with PLL setting `$0100009B`
(20 MHz VCO divided by 20); the 160 MHz observer captures raw ADC bits plus marker
at every observer clock through the physical streamer's digital-input mode.
Repeated marker durations agreed to observer sampling resolution. This PLL VCO
is below the documented recommended 100..200 MHz range: its observed repeatability
supports this measurement session, not a general operating guarantee. RCFAST and
160 MHz DUT controls were also measured. The DUT's RCFAST control was about
25.7 MHz, rather than assuming nominal 20 MHz. Marker alignment and bypass
controls separate ADC feedback/transport from converter delay.

Eight input I/Q pairs were swept over all 256 held phase angles, with a fresh
256-code bypass calibration per pair. A further 108 fractional and overflow
vectors distinguished these integer rules:

- Matrix sums use unsigned DAC samples and signed or zero-extended coefficient
  bytes. Component output extracts `(sum >> 7) & $FF`, with arithmetic shift for
  negative sums; output additions wrap, rather than saturate.
- The chroma coordinates start as signed eleven-bit slices `(sum >> 4) & $7FF`,
  retaining three fractional bits relative to the signed component bytes.
- Normalize the upper phase byte to -64..63 by reversing both coordinates when
  needed. Five CORDIC iterations use quantized angles 32, 19, 10, 5 and 3 and
  coordinate shifts 0..4. Each coordinate addition/subtraction wraps to twelve
  bits. The final IQ byte extracts `(rotated_y >> 4) & $FF`.
- Thus chroma amplitude is approximately 0.823 times signed component I/Q, rather
  than applying the documented 1.646 directly to component bytes. Wider internal
  coordinates or discarded input fractions fail distinguishing measurements.
- One globally calibrated vector appeared a code too high. Four longer,
  interleaved comparisons established its output as 131: DAC3=255,
  CI=`$FF000000`, CQ=0, CY=128, CMOD=`$60`, held phase `$D0000000`.

Independent validation used 144 fresh seeded vectors: 24 each in component,
signed component, S-Video, signed S-Video, composite and signed composite modes.
DAC0 sync states, coefficient/offset bytes, addition controls and held phases vary.
All matched interleaved physical bypass references. One short-average comparison
was repeated with three longer converter/reference pairs and then passed.
These input/expected-byte vectors are retained in `converter-vectors.propan`.
Analog comparison uncertainty is not an exhaustive proof for every possible
coefficient, input or phase; the simulator arithmetic remains deterministic.

Repeated 1 MHz transitions establish external delays of five clocks from a DAC
change, four from a coefficient change, and three from carrier phase. Low-byte
CY offsets affect the output immediately. DAC0 sync/blank controls follow the
five-clock input path. SETCMOD's final mux selection precedes the ordinary setter
output boundary by one clock. Register-source setters and bypass controls avoid
confusing AUGD instruction clocks with peripheral latency. The second timing
campaign collected 32 raw ADC captures per case, including a quarter-turn carrier
step distinguishing the phase boundary.

Disabling conversion clears carrier phase. Rewriting the same enabled mode
preserves phase; frequency pulses while disabled do not leave a retained phase.
Arithmetic cells retain data while disabled and can expose stale values during
re-enable. Input, matrix, grouped rotation and output registers provide an
observable-equivalent model of these delays; their allocation is inferred, not
published physical RTL. TMDS remains independent of DAC conversion.

A separate second-P2 observation of cog 1 confirmed that both COGSTOP followed
by COGINIT and direct replacement COGINIT clear the configured coefficient words.
The replacement image wrote SETDACS and enabled component mode without rewriting
CY/CI/CQ; its matrix outputs were zero. Stop itself restored all four DAC
outputs to zero while the pin-driving cog remained running.

A replacement cog image which set CY/CI/CQ and enabled composite output without
writing CFRQ produced the expected stationary Q-only level. Both stop/start and
direct replacement therefore cleared the previous `$80000000` carrier frequency.
A signed-coefficient CMOD transition with DAC3=64 and coefficient 128 changed
64 to 192 at the four-clock coefficient boundary, independently of the earlier
final-mode mux boundary.

## DDS/Goertzel integer datapath, 2026-10-15

The DUT runs at nominal 1 MHz with PLL `$0100009B`, with the independent
P2-EVAL PLX6ZJKWX at 160 MHz driving deterministic digital levels on P8..P11.
UART exchanges use RCFAST outside the measured region. The low VCO limitation
of this PLL setting is the same as in the converter measurements above.

An initial 136-vector sweep covers 96 signed coefficient, input selection,
inversion and SINC1/SINC2 cases, plus 40 LUT size/offset combinations. The input
sum treats each selected zero as -1 and one as +1, applies per-channel inversion,
and divides by two for exactly two or four selected channels. All signed byte
coefficient cases agree, including -128 and +127.

DDS lookup uses the NCO phase after adding frequency, including on the first
active clock. Table size selects bits 30..22 through 30..29; the low address
bits add the configured offset and wrap within the selected table, leaving its
upper base bits fixed. All eight sizes were checked at five offsets using a
ramp table whose upper bytes expose the selected index in GETXACC.

Fresh finite commands with constant coefficients establish a one-clock stage
before accumulation: N active clocks return `(N-1)*coefficient` in SINC1 and
`N*(N-1)/2*coefficient` in SINC2. This stage retains its state between commands.
GETXACC while active clears the final accumulators, retaining the first SINC2
integrator. After a finite command stops, GETXACC can read the accumulated state
but does not clear it; the arithmetic registers are clock gated. Repeated idle
reads return the same pair, and starting a later command resumes the retained
state. These observations qualify the manual's unconditional "clear" wording.

Raw ADC observations on the 160 MHz EVN board resolve DDS DAC transport at up to
160 observer samples per DUT clock. Across 32 captures each of SETDACS, DDS start
and XSTOP, marker-relative transition areas are respectively 1.984..2.013,
3.990..4.016 and 4.000..4.024 DUT clocks across four channels (one SETDACS capture
lacked sufficient marker lead-in). The marker precedes each tested instruction
by the same two-clock register-operand sequence. DDS start and stop therefore
add two clocks relative to SETDACS: the command-delivery boundary and one further
DAC override register. These are observable transport boundaries; the ADC is
used only for physical characterization and is absent from simulator DAC output.

All sixteen DDS DAC selectors were compared at two distinct signed LUT words
against interleaved SETDACS references, on each of four physical DAC/ADC channel
pairs, with the DUT held at 1 MHz during each settled 160 MHz observer reading.
Three short-average cases were drift-sensitive (common offsets up to 1.09 DAC
codes). Repeating each four times with 8,192-sample averages and bracketing
same-code SETDACS references reduces the mean differences below 0.10 DAC code
on every channel. These observations support the signed-byte MSB inversion and
the existing DAC route/complement table, without adding voltage-valued outputs.

GETXACC's arithmetic capture/clear boundary precedes the simulator's ordinary
execute-stage result boundary by three clocks. Eight active-clear/forwarding/
SINC-switch/stop sequences, eight fixed-delay controls, and eight timestamp
controls distinguish this from clearing on register writeback. The simulator
retains a short arithmetic history and retires valid instructions against the
early boundary, preserving work accumulated afterwards. Cancelled instructions
therefore have no speculative accumulator side effects.

A separate 24-case XMT wake sweep covers ordinary immediate output and DDS.
With GETCT surrounding WAITXMT, adjacent probe delays return 6, 5 and then 4 clocks.
The original interpretation as a WAIT-only qualification delay was incorrect:
the paired POLL/WAIT follow-up below instead establishes delayed event visibility.
This also resolves the active Goertzel capture sequence at an exact command boundary.

Thirty-four fresh finite-command cases cover signed -128/+127 coefficients,
three summed inputs, counts through 65,534, both integrators, and frequencies
`$40000000`, `$7FFFFFFF`, `$80000000`, `$87654321` and `$FFFFFFFF` (the last with
one rollover only). Every result agrees with wrapping 32-bit integration and
the documented masked-phase NCO, including SINC2 accumulator overflow. A high
frequency word is not an ideal unbounded phase carry: `$FFFFFFFF` rolls over
once initially, then the masked accumulator decrements below its MSB for a
very long interval. Finite-duration reference generators must use the actual
NCO bit rule rather than assuming `count/frequency` alone.

Twelve changing-frequency ramp-table vectors establish a registered SETXFRQ
update one clock after ordinary writeback. DDS must retain its advancing phase
independently of the preceding rollover consumed by command counting; recomputing
an apparent phase as `old_count_phase + new_frequency` incorrectly shifts a live
waveform. In particular, setting frequency to zero holds the accumulated phase
rather than moving it backwards by an old frequency step.

Buffered command readiness has one clock of lookahead to the releasing NCO
rollover. Eighteen timestamp cases around a blocked buffered stop return one
clock less than waiting for the occupied buffer to become visibly empty.
Seventy-two ramp-table command/capture/timestamp vectors (SINC1, SINC2, phase
reset, continued phase, changed buffered frequency, and a longer nonzero command
that also blocks) agree when acceptance is forecast one clock before release.
Instruction operands and SETQ frequency remain captured throughout the stall.

Shared-LUT contention was checked with a paired cog writing either the DDS
lookup address or another address every two clocks, at both instruction phases
and in both integrator modes. The first SINC1 capture is **not repeatable across
sessions** even for byte-identical complete uploaded images. For example, writing
`$070B0000` to LUT address 100 while DDS reads `$05030000` at address 0 yielded
first cosine/sine pairs 17/17 and 9/15 on separate runs of the same bytes.
Writing the lookup address itself yielded 25/17 and 33/21. Both original and
uploaded binary images were compared byte-for-byte; no source, oracle layout,
serial payload or clock configuration changed. A 44-case first-capture delay
sweep also found phase-dependent startup effects around an immediately issued
GETXACC; later captures were stable in these measurements.

The deterministic port model retains the previous LUT sample when a companion
write displaces an ongoing read, with the initial sample represented as
prefetched. The collision regression checks the stable later captures in both
SINC modes and both phases, without encoding an arbitrary first power-up/read
bus state. These data do not establish physical latch contents, RAM port
implementation or repeatability for every collision and startup condition.

Raw ADC pad configuration reinterprets GPIO control fields: the manual's
`%100000_OHHHLLL` through `%100111_OHHHLLL` select GND, VIO, float and five input
gains. In particular, `P_ADC_1X` (`$00118000`) sets bits that the generic GPIO
structure names synchronization and input inversion. They select the ADC source
and gain here, and must not invert or synchronize a host-supplied raw bit density.
The independent A/B selector inversion remains applicable after the raw input.

A final four-entry waveform test uses unsigned levels 0, 64, 128 and 255, rotated
across the four DAC channels, with DDS frequency `$20000000` and table size 4.
Thirty-two EVN captures sample the 1 MHz DUT at 160 MHz; 31 contain sufficient
marker lead-in and playback for the comparison. From marker +4 clocks onward,
the outputs follow table entries 1, 2, 3, 0 on successive system clocks, on all
four channels. Central 120-of-160-sample windows avoid the transition edges.
Within-channel normalization between the observed minimum and maximum gives
middle-code deviations below 0.79 DAC code. This validates sample-per-clock DDS
playback and its initial phase separately from GETXACC arithmetic and settled
routing, without adding analog values or DAC feedback to the simulator.

Twelve changing-input correlation cases use duty-NCO digital densities 1/4,
1/2 and 3/4, two instruction phases, both SINC modes, and four distinct signed
quadrature weights. Ordinary IN snapshots agree with the existing digital pad
model, while Goertzel samples the IN pipeline **one clock earlier** than the
ordinary cog register-read tap. Using the ordinary IN tap rotates these
correlations by one sample, despite passing all constant-input arithmetic tests.
The model therefore uses the existing earlier input stage also observed by
selectable pin sensors, without altering GPIO transport or introducing voltages.

The retained DDS/Goertzel regression set has eleven fixtures and 298 runs,
including 264 silicon comparisons at the 1 MHz DUT clock. The other 34 checks
inspect DAC byte state locally, backed by the two-board route and waveform
measurements above. Final project validation passes 3,874 fixture runs and 392
unit/harness/GUI tests, with two optional Propan tests skipped, using
`zig-0.16.0 build install test -Doptimize=ReleaseSafe -Dcoverage=false`.

### ADC-capable pad output and streamer event edges (2026-10-15)

The silicon documentation's WRPIN pin-configuration table retains DIR-controlled
OUT drive, output inversion, and independent HHH/LLL drive selectors in all eight
raw ADC source/gain modes. ADC mode does **not** imply a floating output. Its source/
gain bits reuse the GPIO input-inversion and synchronization positions, so those
GPIO interpretations must be ignored. A 32-case nominal 20 MHz RCFAST P2AAS sweep
on unloaded P40, observed with P41's adjacent comparator at DAC threshold 128 and
DIR low, confirms all 64 settled low/high observations: eight ADC selectors,
both output polarities, and fast/1.5 kOhm drive. Both pins are restored to GPIO
with DIR low before reporting. This experiment establishes simultaneous ADC
configuration and digital output drive, without modeling electrical conversion.

Both DAC PadMode groups are also ADC-capable. Sixteen additional RCFAST cases
cover all four DAC ranges and four TT settings on unloaded P40. With DIR high,
DAC code $88 (and SETDACS $88888888 for channel selection), 256 TESTP samples
return zero ones for OUT low. OUT high yields 40..136 ones for TT=00/01, whereas
TT=10/11 yields zero. Thus plain DAC mode permits simultaneous DAC output and
raw ADC sampling; bit-DAC selection disables that raw input in these cases.
Counts are uncalibrated observations of ADC enable, not reference voltages.
The documented ADC enable is independent of DIR, which enables the DAC output.
The simulator uses OUT and TT to gate a separately supplied bit density, retaining
only the four per-cog DAC bytes and adding no electrical DAC-to-ADC feedback.

A paired streamer event campaign at the original nominal 1 MHz DUT clock compares
POLLXMT/POLLXFI/POLLXRO with the corresponding WAIT at identical instruction
positions, in separate fresh runs. Both ordinary immediate output and DDS agree.
With frequency $10000000, `WAITX delay; GETCT; consumer; GETCT` gives these edges:

| Event | First delay with POLL true | GETCT span around WAIT at delays 0,1,2,3,4 |
| --- | --- | --- |
| XMT | 2 | 6, 5, 4, 4, 4 |
| XFI | 4 | 8, 7, 6, 5, 4 |
| XRO | 1 | 5, 4, 4, 4, 4 |

XMT/XRO use XINIT followed by XCONT; XFI uses finite XINIT count 1 followed by
NOP in place of XCONT. The four-clock baseline includes the consumer and the
following GETCT; it is not a four-clock WAIT execution time. No measured offset
has POLL true while the same-position WAIT still adds a stall. A POLL consumes
the flag, so POLL followed by WAIT in one run is not an equivalent experiment.

The initial 144 edge observations and 60 additional early/late probes at
$08000000/$20000000 exposed simulator publication that was two clocks early for
XMT/XFI and one early for XRO. The XMT-specific wait adjustment masked only part
of that discrepancy. Publication now uses those measured delays, and these
registered event sources do not add a second resume delay for an arriving event.
Other events retain their separately measured timing, including WAITFBW's direct
wake path. These delays describe simulator boundaries, not an established physical
register topology. The retained `streamer-event-edge.propan` fixture checks twelve
offsets in each of 36 combinations of frequency, mode, event and consumer; all
432 comparisons also pass on the physical chip. This evidence does not establish
identical edge behavior for every other event source.

### Event delivery and simultaneous acknowledgement (2026-10-15)

A further 102 native P2AAS probes use finite ordinary/DDS streamers, frequency
$10000000 at the nominal 1 MHz DUT clock, counts 1/2/3, and seventeen offsets
before three consecutive `POLLXRO WC; WRC result` pairs. Both modes agree at every
offset. At count 2 and delay 12, the three results are `1,1,0`; at delay 13 they
are `1,0,0`. A count-1 control gives `1,0,0` at both offsets. Thus the second
result in the delay-12 case represents a second occurrence, not rereading the
same sticky flag. The first poll samples the older flag; the second rollover
arrives at its writeback acknowledgement edge and survives. At the next offset,
both occurrences have already coalesced before acknowledgement.

`streamer-event-ack.propan` retains 24 cases around this boundary, including
one-/three-rollover controls and both streamer modes. A simulator unit test also
injects an older XRO plus a new delivery at the exact acknowledgement writeback
clock and verifies that the new flag remains available to the next poll.
The cog now owns pending event stages. Consumer acknowledgements commit before
new deliveries on the same clock; source-specific latch/request transport
replaces the former post-WAIT resume-delay helper. Interrupt/debug request
transport is separate from architectural flag transport, as required to retain
the measured counter-interrupt and selectable-pin timings. These are model
boundaries, rather than claims about the chip's internal register topology.

Thirty-four additional paired CT1 POLL/WAIT probes at nominal 20 MHz RCFAST
schedule a target 32 clocks after GETCT and vary the probe delay from 16 through
32. POLL becomes true at delay 24; WAIT spans 12,11,...,5,4 clocks at delays
16..24 and remains at its four-clock baseline thereafter. Both consumers agree
with a two-clock latch-delivery stage, without an extra WAIT resume stage.
Rearming must discard latch pulses from the old counter comparison, including
the startup target of zero, while preserving already-pending interrupt requests.
This differs from a selectable input sample already in flight, which survives
selector reconfiguration.

A separate unloaded-P40 GPIO-to-selectable-input probe found the event already
available at its earliest sampled consumer position on hardware, while the
existing fixed digital GPIO transport model delivers it later. The event changes
retain the previously validated P4 smart/GPIO event fixtures; they do not resolve
that broader pad-loopback approximation or introduce electrical signal modeling.

Follow-up validation: `zig-0.16.0 build install test -Doptimize=ReleaseSafe
-Dcoverage=false --summary all` succeeds in all 36 build steps, with 398 tests
passed and two optional tests skipped. All 241 registered Windtunnel fixtures
pass their 3,934 runs. The two retained streamer event fixtures also pass all
60 P2AAS oracle runs (432 edge comparisons plus 24 acknowledgement scenarios).
