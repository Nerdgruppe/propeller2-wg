# Shared P2 random generator research

Status: **shared xoroshiro model implemented; exact silicon tap numbering and
seed behavior explicitly open**. Windtunnel now uses one hub-owned xoroshiro128**
source advancing every simulated clock. The measured cog tap relationships and
relative inversions are preserved, with an arbitrary assignment of anonymous
classes to numbered output bits. No captured stream has been predicted from a
recovered xoroshiro state. The retained captures do not establish bit-exact replay.

## Reproduce the measurements

From the repository root, build with `zig-0.16.0`, then run a probe through the
native hardware harness (one upload at a time):

```sh
zig-out/bin/windtunnel-tests --characterize --artifact-dir data/random/captures data/random/capture.propan
```

`P2AAS_ENDPOINT` defaults to `ws://localhost:12880/`. Characterization records
hardware without trying to compare its random data to the simulator. The large
zero-valued hub assertions in these research probes request observation ranges;
they are deliberately not expected hardware values. They are not build fixtures.

Each capture retains its source, assembled/uploaded images, request, raw framed
output, decoded hardware observation and transport report. `analyze.py` checks the
raw frame's length, observation count and CRC before using its hub data.

```sh
python data/random/analyze.py data/random/captures/capture.propan-* \
  --check data/random/captures/validation.propan-* \
  --consumers data/random/captures/consumers.propan-* \
  --waitx data/random/captures/waitx.propan-* \
  --seed data/random/captures/seed-taps.propan-* \
  --output data/random/taps.json
```

| Probe | Retained observations | Purpose |
| --- | --- | --- |
| `capture.propan` | 512 words/cog, eight synchronized cogs | Calibrate equivalence classes and relative inversions. Four bursts change CT phase and parity. |
| `validation.propan` | Eight independent uploads, 1,024 words/cog each | Reserve 8,192 words/cog across all eight CT phases and both parities for eventual generator replay. Cross-cog relations already pass. |
| `consumers.propan` | Four windows, eight consumers/window | Compare GETRND D, flags-only GETRND, BITRND and all four pin RND families to companion cogs sampling the same edge. Includes port B. |
| `waitx.propan` | Four windows, six masks/window | Observers cover both parities. Check random delays and WC/WZ/WCZ effects. |
| `seed.propan` | Four windows, changing HUBSET seeds | Observe command-slot delay and its effect on subsequent random words. |
| `seed-taps.propan` | 64 seed windows, 16 words/cog/window | Exercise all CT phases with known seed operands; retained for seed-lane and pipeline recovery. |
| `seed-hammer.propan` | Two independent uploads, 40,960 words total | Rotate all eight observers with 4/5/6/7 seeders and an unseeded baseline; analyze bias and modal partial patterns by CT phase. No usable subset identified. |

## Established on the attached chip

- The 256 cog output bits form **64 equivalence classes**, each appearing in
  exactly four distinct cogs, as either the same or complemented value.
- Cog pairs (0,1), (2,3), (4,5) and (6,7) each cover all 64 classes without overlap.
- `taps.json` lists `[cog, destination_bit, invert]` entries for each anonymous
  class. Its polarity makes the first calibration observation zero. Class numbers
  are **not** numbered xoroshiro output bits. Reuse this calibration polarity
  across uploads instead of normalizing each boot independently.
- All 32 measured consumer operations match companion-cog words on the same
  execute edge. In `GETCT before; consumer`, the sampled word belongs to CT
  `before + 2`. GETRND C and Z are word bits 31 and 30.
- All 24 measured randomized WAITX operations satisfy
  `GETCT after - GETCT before = 4 + (D & RND(before + 2))`.
  The tested masks are 0, 1, 3, 7, 15 and 31. Only requested flags clear.
- For even `GETCT before` phases in the cog-0 HUBSET probe, aligning the following
  words against companion cogs gives HUBSET durations 2/4/6/8 clocks according
  to the cog's hub-command slot. This does not establish the seed's internal
  write edge or output-pipeline delay.

## Candidate recovery and remaining work

The [reference xoroshiro128** algorithm](https://prng.di.unimi.it/xoroshiro128starstar.c)
uses transition constants `[24,16,37]` and output `rotl(S0 * 5, 7) * 9`.
[Contemporary P2 collaborators describe](https://forums.parallax.com/discussion/166176/random-lfsr-on-p2/p65)
these constants and pipelined additions. The [silicon manual](../../docs/p2/silicon-documentation.md)
states that HUBSET writes 32 of the 128 state bits, with bit 31 forced high.
The boot ROM seeds it fifty times from ADC thermal noise. These sources motivate
the model; the retained hardware streams have not yet verified it.

`recover.py` is optional research tooling using an already installed `z3`
executable. It is not a runtime/build dependency. It tests eight numbered output
taps first: bit 7 of `(output_low_byte * 57) mod 256` equals S0 bit 0 under the
candidate scrambler. Gaussian elimination of the candidate linear transition
generates state-independent parity checks. A solver fit is accepted only if the
recovered state also predicts all 64 anonymous histories over the full capture.

```sh
python data/random/recover.py data/random/captures/capture.propan-* \
  --timeout 600 --output /tmp/p2-rng-recovery
python data/random/recover.py --self-check
python data/random/recover.py data/random/captures/waitx.propan-* \
  --interleaved-taps data/random/taps.json --fit-samples 192 \
  --timeout 600 --output /tmp/p2-rng-consecutive
python data/random/recover.py data/random/captures/waitx.propan-* \
  --interleaved-taps data/random/taps.json --strategy word-histories \
  --timeout 600 --output /tmp/p2-rng-histories
```

Current blind full-word and eight-tap fits timed out without a recovered model.
A timeout neither proves nor disproves a candidate. The historical `[55,14,36]`
transition has also been considered. The historical 16-cog/63-tap table retained
in `published-mapping.json` comes from [the designer's earlier mapping
discussion](https://forums.parallax.com/discussion/166176/random-lfsr-on-p2/p39)
and does not match this chip's observed incidence pattern; it must not be installed as the production
map.

The WAITX probe's observer pairs (2,3) and (4,5) additionally reconstruct 256
consecutive clocks per window. `--interleaved-taps` uses this format. Fitting a
prefix with `--fit-samples` still requires the state to predict every word in
the capture before accepting a model. Synthetic self-checks validate the recovery
algebra and permutation/inversion handling independently of hardware hypotheses.
The `word-histories` strategy instead fits the full initial state by requiring
each numbered output-bit history to occur among the anonymous true/complement
histories. It also verifies the entire capture before accepting a permutation.

## Last recovery attempt: repeated zero seeding

`seed-hammer.propan` rotates the observer through all eight cogs. In each round,
4, 5, 6 or 7 companions repeatedly issue `HUBSET 0x80000000` (zero payload with
mandatory bit 31 set). A separate baseline uses no seeders. Each observer records
eight 64-word bursts, visiting all CT phases after 4,096 settling clocks. Seeders
use unrolled groups of sixteen seed writes and continue through the entire
sampling window; only the observer writes the burst
to hub RAM afterward. The output retains 20,800 longs, including CT stamps.

The initial upload on 2026-10-11 was rejected by P2AAS with **"No time quota
left."** After the user restored P2AAS availability (up to 60 seconds), two
independent uploads completed successfully using the probe's existing 10-second
request timeout. Both raw frames, sources and transport reports are retained in
`captures/seed-hammer.propan-*`, alongside the earlier rejected attempt.

```sh
zig-out/bin/windtunnel-tests --characterize --artifact-dir data/random/captures data/random/seed-hammer.propan
python data/random/seed_hammer.py \
  data/random/captures/seed-hammer.propan-ef8b460a51d8d517-986e7f328925940b-1791741373544816645 \
  data/random/captures/seed-hammer.propan-ef8b460a51d8d517-986e7f328925940b-1791741440069615488 \
  --output data/random/seed-hammer-statistics.json \
  --pooled-output data/random/seed-hammer-pooled.json
python data/random/seed_hammer.py --self-check
```

The analyzer verifies frame CRCs, observation counts and all 640 CT stamps. Each
upload contains 20,480 random words: 512 per cog per seeder count, including the
baseline. Pooling uploads by actual clock phase yields **128 words per cog,
seeder count and phase**, or 40,960 random words altogether.

No individual bit or bit-pair XOR reaches 90% agreement, either per upload or in
the pooled results. No full word repeats within any pooled cog/count/phase
condition. The following maxima cover all eight observers and all eight phases:

| Seeders | Words | Largest single-bit agreement | Largest bit-pair XOR agreement | Largest byte-mode count / 128 |
| --- | ---: | ---: | ---: | ---: |
| 0 (baseline) | 8,192 | 65.625% | 67.969% | 7 |
| 4 | 8,192 | 64.844% | 67.969% | 5 |
| 5 | 8,192 | 64.844% | 67.188% | 5 |
| 6 | 8,192 | 64.844% | 68.750% | 5 |
| 7 | 8,192 | 63.281% | 69.531% | 5 |

The bit/pair/byte checks found no useful seed-induced outlier over the baseline.
**This attempt did not identify a low 32-bit subset or canonical tap numbering.**
It does not rule out smaller effects or higher-order relationships: the checks
are not an exhaustive statistical test of all possible partial patterns.

`seed-hammer-statistics.json` retains each upload's per-bit counts, phase-wise
bias masks, modal patterns, pair-agreement maxima and byte-mode counts;
`seed-hammer-pooled.json` retains the same statistics combined by actual CT
phase. A zero biased mask has no partial mode. The chosen arbitrary numbered
output assignment remains in place, with the exact production map explicitly
open as authorized by the user.

## Implemented approximation and explicit open questions

[`Random.zig`](../../src/windtunnel/sim/Random.zig) uses the reference [24,16,37]
transition and a two-stage `S0 * 5`, then `rotl(...,7) * 9` scrambler. Synthetic
initial state is `{1,0}`, with cleared output pipeline registers. `Hub.step`
advances it once per clock, including stopped-cog clocks and CT rollover. Reads
are side-effect-free. No independent per-cog random generator remains.

For distribution, anonymous class N is assigned to output bit N. The 8×32 tap
table and inversion masks exactly preserve the measured cross-cog relationships,
but **this numbering and the absolute polarities are arbitrary**. This assumes
roughly interchangeable output-bit statistics; temporal and seed-dependent
statistics are not established as hardware-equivalent.

HUBSET overwrites the upper 32 bits of S1 at its cog's command grant, retaining
the other 96 bits and the scrambler registers. This follows the historical
[designer description](https://forums.parallax.com/discussion/166176/random-lfsr-on-p2/p3),
not a recovered production seed lane. Its output delay is a consequence of the
chosen pipeline model, not a newly measured timing claim. Non-seeding clock
configuration does not modify RNG state. Physical pad noise borrows the shared
output using an approximate pad-bit assignment; physical smart-pin/DAC mapping
and boot ADC entropy remain outside the characterized scope. XORO32 remains the
independent register-based instruction.

Explicitly open:

1. Production numbered output permutation and absolute inversions.
2. Production HUBSET overwritten lane, seed ordering and internal pipeline delay.
3. Bit-exact replay of independent uploads from fitted initial states; the eight
   held-out captures remain available for eventual verification.

The final seed-hammer experiment is now measured; it did not resolve these
questions. No further recovery is implied by the approximate implementation.

The reference vectors, measured projection words, shared-clock progression,
partial-seed preservation and consumer semantics have simulator tests. Those
checks validate the implemented approximation, not recovered production timing.

Current validation used `zig-0.16.0`: ReleaseFast installation, 87 native harness
units, and five targeted fixtures comprising 139 runs pass. The standalone RNG
reference/projection units and the analyzer's synthetic checks also pass.
The full fixture matrix has not been rerun after clock-skip removal.
