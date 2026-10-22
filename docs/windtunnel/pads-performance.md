# SIMD pad performance — 2026-10-15

The SIMD bank reduces full-step time by about 9–15× for busy pad banks on this
machine. Sparse Realistic activity benefits much less; one toggling realistic
pin has essentially the same total step time as before.

## Method

- AMD Ryzen 9 7940HS, native target with AVX-512, `zig-0.16.0`, LLVM, ReleaseSafe.
- One logical CPU (2), sequential execution, nine paired trials with randomized
  variant order. Compilation, initialization, 5000 warmup clocks and reporting
  excluded. No GUI, VCD, debugger, terminal output, hardware or oracle execution.
- Identical input-independent cog instruction loops at 20 MHz. All variants
  finished each paired trial with identical cog/register/PC/RNG/counter/output/
  direction checksums. The ablated build intentionally omits pad observations.
- Original: previous scalar bank and its general-purpose breakpoint sort.
  Tiny-sort: same scalar implementation, replacing only that sort with insertion
  sort. SIMD: whole 64-lane vectors, selected-drive caching and the linear shortcut.
- Analog share comes from a separate nine-pair comparison of SIMD against the
  same current Hub with only its pad-resolution pass bypassed. It includes drive
  mapping, cache checks, solving, diagnostics and thresholding.

These are observed medians, not hardware-independent guarantees. The gain over
tiny-sort measures the remaining refactor together: SIMD, data layout and driver
caching; it does not isolate vector arithmetic alone.

## Full-step results

| Workload | Preset | Original µs | Tiny-sort µs | SIMD µs | Original/SIMD | Tiny-sort/SIMD | Remaining analog share |
|---|---|---:|---:|---:|---:|---:|---:|
| Idle, all floating | ideal | 2.520 | 2.535 | 2.058 | 1.22× | 1.23× | 8.0% |
| One cog, one fast pin | ideal | 3.335 | 2.752 | 2.487 | 1.34× | 1.11× | 22.1% |
| One cog, 64 fast pins | ideal | 38.589 | 4.186 | 2.664 | 14.48× | 1.57× | 26.5% |
| Eight cogs, 64 fast pins | ideal | 40.139 | 5.103 | 3.571 | 11.24× | 1.43× | 19.4% |
| 64 × 15 kΩ + 100 pF | ideal | 38.551 | 4.162 | 2.628 | 14.67× | 1.58× | 26.9% |
| 64 × 100 µA + 100 pF | ideal | 39.094 | 4.800 | 3.320 | 11.77× | 1.45× | 42.1% |
| 64 mixed fast/resistor/current/float | ideal | 30.493 | 3.987 | 3.319 | 9.19× | 1.20× | 41.8% |
| Idle, all floating | realistic | 2.519 | 2.521 | 2.066 | 1.22× | 1.22× | 14.2% |
| One cog, one fast pin | realistic | 3.332 | 2.810 | 3.267 | 1.02× | 0.86× | 40.5% |
| One cog, 64 fast pins | realistic | 39.343 | 8.901 | 3.417 | 11.51× | 2.60× | 43.9% |
| Eight cogs, 64 fast pins | realistic | 40.137 | 9.779 | 4.324 | 9.28× | 2.26× | 34.8% |
| 64 × 15 kΩ + 100 pF | realistic | 38.698 | 4.198 | 2.627 | 14.73× | 1.60× | 27.3% |
| 64 × 100 µA + 100 pF | realistic | 39.036 | 4.682 | 3.340 | 11.69× | 1.40× | 42.2% |
| 64 mixed fast/resistor/current/float | realistic | 31.064 | 5.223 | 3.427 | 9.06× | 1.52× | 43.7% |

## Kernel choice

Always using the general piecewise kernel regressed Ideal's one-pin workload by
26.8%, its 64-fast-pin workload by 23.3%, and the resistor-capacitor workload by
24.0%. This exceeds the planned 3% budget, so the direct-linear shortcut remains.
Realistic's piecewise cases use the fixed fifteen-candidate path. Settled banks
skip solving; other banks mask inactive lanes without scalar electrical fallbacks.

The old pad sort's 4 KiB scratch initialization is absent from the SIMD solver.
Native disassembly contains packed ZMM add/subtract/multiply/divide instructions.
A baseline x86-64 CLI build and both profile smoke tests also passed.

A one-pin Realistic workload is about 16% slower than the *tiny-sort scalar*
variant: the full-width piecewise bank does work for inactive lanes. It remains
approximately equal to the original implementation. This is the sparse-workload
tradeoff of the requested full `@Vector(64, ...)` design.

## Reproduction and verification

The permanent benchmark and its CSV format are documented in
[the I/O documentation](io.md#simd-electrical-integration). Its `hub` mode times
whole Hub clocks; `scalar`, `scalar_small_sort`, `simd` and `simd_uniform` compare
isolated electrical passes with a shared drive pattern. Their absolute timings
have different denominators from the full-Hub comparisons above.

The isolated old/new full-Hub snapshots, driver programs, paired timing scripts,
raw trials and checksums used here are retained temporarily in
`/tmp/windtunnel-simd-cost/` (`results.json`, `share.json`, `run.py`, `share.py`).

Validation: 347 Windtunnel tests including GUI tests, 241 fixtures / 3934 runs,
CLI/GUI/benchmark builds, both-profile VCD/FST round-trips, and 16 focused electrical
tests including customized curve/supply/capacitance/leakage parameters. No physical
model or source validation rules changed; f64 results allow normal rounding differences.

## Analog subsystem follow-up — 2026-10-16

Adding physical receivers, feedback, electrical ADC conversion and smart analog
engines adds 18–34% to these GPIO full-Hub workloads (about 0.7–0.9 µs per clock).
This compares the completed frontend against the SIMD implementation at
`608af02`; it is additional feature cost, not a change to the electrical solver.
The bank still uses SIMD thresholds for ordinary logic receivers, caches drive
identities, and rebuilds drivers between substeps only for asynchronous feedback.

The comparison uses the same machine, CPU affinity and native LLVM ReleaseSafe
flags as above: nine randomized pairs, 10,000 measured Hub clocks after 5,000
warmup clocks, with compilation, GUI, tracing, hardware and tests excluded. Both
snapshots use the same benchmark and P2 type module. These workloads measure the
ordinary GPIO cost; they do not bound the cost of running 64 FIR filters or
raising the analog oversampling factor.

| Workload | Preset | Previous µs | Analog frontend µs | Increase |
|---|---|---:|---:|---:|
| Idle | ideal | 2.810 | 3.634 | 29.3% |
| One fast pin | ideal | 2.499 | 3.212 | 28.6% |
| 64 fast pins | ideal | 2.671 | 3.396 | 27.1% |
| Eight cogs, 64 fast pins | ideal | 3.581 | 4.302 | 20.1% |
| 64 RC loads | ideal | 2.681 | 3.418 | 27.5% |
| 64 current loads | ideal | 3.335 | 4.087 | 22.6% |
| 64 mixed loads | ideal | 3.319 | 4.070 | 22.6% |
| Idle | realistic | 2.056 | 2.757 | 34.1% |
| One fast pin | realistic | 3.260 | 3.982 | 22.2% |
| 64 fast pins | realistic | 3.448 | 4.187 | 21.4% |
| Eight cogs, 64 fast pins | realistic | 4.385 | 5.188 | 18.3% |
| 64 RC loads | realistic | 2.647 | 3.398 | 28.4% |
| 64 current loads | realistic | 3.333 | 4.068 | 22.1% |
| 64 mixed loads | realistic | 3.428 | 4.220 | 23.1% |

Temporary baseline/current executables, snapshot sources, paired runner and all
raw trials are in `/tmp/p2-analog/performance/` (`results.json`,
`final-results.log`, `run.py`). The permanent benchmark can be rebuilt and run
using the commands in the I/O documentation.
