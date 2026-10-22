# Clock sources and measured PLL behavior

`--sim ideal` uses exact typical values. `--sim realistic` selects an initial
frequency within each source's min/max range and applies bounded variation on
each simulation step. Both modes retain cycle-based instruction semantics;
physical time follows the selected clock. `hub.io.clock.frequency()` supplies
the current rate directly; there is no cached integer frequency mirror.

| Source | Ideal / typical | Realistic min–max |
| --- | --- | --- |
| RCFAST | 24 MHz | 20–30 MHz |
| RCSLOW | 20 kHz | 12–30 kHz |
| Board crystal | 20 MHz | 20 MHz, fixed unless overridden |

The RC values come from the local [datasheet](../p2/datasheet.md). The crystal's
actual tolerance is unknown; no default tolerance is invented. The measured
room-temperature RCFAST value of one board does not replace the datasheet typical.

## CLI and reproducibility

These options work in the CLI, GUI and `--serve` mode:

```sh
windtunnel --sim realistic --seed 42 --rcfast 24-4+6 --image program.bin
windtunnel --sim realistic --seed 42 --rcfast 25.23+3.2ppm \
  --rcslow 20kHz-8kHz+10kHz --crystal 20+30ppm --image program.bin
```

| Syntax | Meaning |
| --- | --- |
| Omit a clock option | Retain the selected preset's typical value and range |
| `--rcfast 25.23` | Fixed 25.23 MHz, even in realistic mode |
| `--rcfast 25.23+3.2ppm` | 25.23 MHz ±80.736 Hz |
| `--rcfast 24-4+6` | Nominal 24 MHz, allowed 20–30 MHz |
| `--crystal 20-2+3ppm` | 20 MHz −2/+3 ppm; trailing relative unit applies to both bounds |
| `--rcslow 20kHz+1%` | 20 kHz ±200 Hz |

All bare frequencies and absolute bounds are in **MHz**, including RCSLOW.
Explicit `Hz`, `kHz`, `MHz` and `unit` (MHz) are accepted. Relative ranges accept
`ppm`, `ppb` and `%`. Bounds must be finite and remain between 1 Hz and 1 GHz.
Explicit ranges apply in either mode; omit an option to retain its preset default.
The literal `datasheet` is not an accepted command-line value. Clock behavior
is derived from these ranges: all three sources with zero lower/upper bounds use
ideal PLL/XI behavior; any nonzero bound enables the empirical behavior where
characterized. There is no separate clock-profile setting.

`--seed N` seeds a clock-only RNG, independent of the simulated P2 RNG. If omitted,
a seed is generated from host entropy. Startup prints `clock seed: N (--seed N)`.
Reusing the seed, configuration, inputs and simulation steps reproduces the clock
sequence. Ideal clocks without explicit ranges are independent of the seed.
Fresh server sessions and resets restart the configured sequence.

Each ranged oscillator starts uniformly within its bounds. Each simulation step
adds a uniform increment within ±(max−min)/4096, reflected at the endpoints.
This is a reproducible uncertainty model, **not a measured phase-noise spectrum,
physical drift rate or temperature model**. Its rate depends on simulation steps.
The GUI's System Configuration editor exposes nominal/minus/plus values and seed;
apply them by reinitializing the system.

## PLL coverage

With all oscillator bounds zero, HUBSET divider/multiplier arithmetic applies
immediately. With any nonzero bound, the model uses frozen, phase-consistent empirical curves for the primary specimen's
nominal 20 MHz crystal with the 15 pF loading selection:

| Sequence | Implemented response |
| --- | --- |
| Crystal and PLL enabled cold, D=1/M=8 | Composite startup profile, approximately 0.94 ms acquisition in the measured sequence |
| PLL enabled after crystal has run ≥1 ms, D=1/M=8 | Prewarmed acquisition profile; measured 32 µs block-end settling bound |
| D=1, M=8→10 | Measured 160→200 MHz trajectory, including overshoot |
| D=1, M=10→8 | Measured 200→160 MHz trajectory |
| M=8, D=1→2 | Measured 160→80 MHz trajectory |
| Same requested VCO, changed D/M | Preserve rate; no invented reacquisition delay |
| P-only or source-selection change | Preserve background loop state and response age |

Profiles store integrated normalized phase, with 0.5 µs knots for live changes
and 1 µs knots for startup. Each system cycle integrates frequency across profile
boundaries; rates are not rounded to MHz. The crystal and PLL acquire while an RC
source clocks the CPU. Switching to the already-acquired PLL does not restart it.

The four characterized nontracking requests use specimen-specific observed tails:
D/M = 1/20, 2/39, 8/160 and 32/640 produce about 385.64–385.75 MHz VCO.
Requested and effective rates remain separate. These are sparse observations,
not a universal upper clamp or a prediction of temperature-dependent limits.

Uncovered transitions or reference conditions use ideal arithmetic and report
`uncharacterized` coverage. The model does not identify a general analog PLL
transfer function. Retargeting a measured curve mid-acquisition begins at the
current modeled VCO rate, but that transient has not been independently measured.
The 1 ms warm/cold classification is a modeling boundary, not a safe-startup guarantee.

A direct P=2→1 change at D=1/M=16 raises an unsafe-sequence diagnostic because the
hardware experiment returned incorrect elapsed data. The simulator does not inject
unmeasured instruction corruption or pretend that correct final rate implies
correct CPU execution. GUI diagnostics expose seed, coverage, acquisition and this
warning; unsupported PLL commands also log a fallback diagnostic.

## Physical time

Hub, electrical pad integration, VCD and server pacing consume the same Q64
femtosecond intervals, retaining fractional carry. During an empirically unready
source, physical time and analog pads advance in bounded quiet steps (at most
100 ns); CT, cogs, smart-pin engines and the P2 RNG receive no fabricated edges.
Cancellation remains possible between steps. GUI single-step waits for a real
completed CPU edge.

An output with no transitions only bounds source availability. Treating that gap
as no usable CPU edges is an explicit empirical approximation. Internal clock
duty, individual malformed pulses and phase noise were not measured; the VCD
system-clock waveform retains its conventional 50% duty representation.

Loaded images inherit UART dividers calculated from the initial actual configured
RCFAST rate. ROM upload drives host bits against accumulated physical time.
The functional terminal still does not reproduce analog baud-error corruption.

## Validation

`utility/gen_clock_profiles.py` freezes six curves from retained Saleae captures
and nine compact rising-edge records with SHA256 provenance. Regeneration needs
the original ignored capture directory; normal builds use checked-in Zig/JSON
fixtures and require no instrument, Python runtime or external service.

The runtime test advances actual `Clock.step()` intervals against three training
records and **six held-out repeats / faster PWM exporter controls**, without
refitting transient shape. Initial/tail frequency normalization removes common
instrument/crystal scale; only a constant phase offset is fitted per record.
Frozen-profile residuals are at most **6.1 ns RMS / 20.9 ns peak**, within the
10 ns RMS / 30 ns peak gates. These are divided-output phase residuals, not a
single-cycle jitter measurement. Startup profiles have weaker validation than
the live-retune controls; their block-end bounds remain the appropriate claim.

Tests also cover deterministic seeds and bounds for all three oscillators,
crystal-to-PLL drift propagation, background acquisition, postdivider state,
quiet-time pad/CT/RNG behavior and agreement between VCD and clock elapsed time.
A full firmware smoke check exports PWM/32 and a marker, writes VCD, samples
the pad transitions on a 10 ns grid and uses the same external rate-fit helper:
the 160→200 MHz step settles by the 16 µs block end and peaks at 204.33 MHz
in a 2 µs window (hardware control: 205.18 MHz). This checks the exporter and
marker path as well as the clock state. It does not qualify individual pulses.
Existing CPU, UART, fixture, debugger and GUI checks provide integration coverage.
Broader silicon dynamics, analog loop constants, environmental dependencies and
instruction failures remain open as described in the [implementation plan](vco-simulation-plan.md).


Final verification used `zig-0.16.0`, ReleaseFast, coverage disabled:

```sh
zig-0.16.0 build build-windtunnel test-windtunnel -Doptimize=ReleaseFast \
  -Dcoverage=false -Dwindtunnel-jobs=2 --summary all
zig-0.16.0 build test-libp2 -Doptimize=ReleaseFast -Dcoverage=false --summary all
```

All 262 fixtures / 5,368 firmware runs passed, along with 208 harness, 190 debugger,
2 GUI and 12 libp2 tests. CLI checks confirmed same-seed VCD identity, different-seed
realistic variation, generated-seed replay, seed-independent ideal clocks, mixed
units and invalid-range rejection. Profile regeneration was byte-reproducible
following Zig formatting. The default all-core run hit the harness's wall-time
guard on long FIFO cases; the bounded-worker run completed without widening it.
The new 24 MHz timestep also exposed a DC-threshold floating-point bias; solving
the pad voltage increment removes it without changing the circuit equation or
hardware golden expectations (all 30 DAC pad-mode cases passed).
