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
result still makes both words ready. Idle fast-forwarding must not skip that
arrival, even for a stopped or restarted cog.

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
