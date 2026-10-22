# Plan: stateful clock and VCO behavior for Windtunnel

This historical plan describes the former live VCD path. The simulator now records
native Scope captures; `windtunnel-export` owns VCD conversion. References to
`sim/Vcd.zig` below document the earlier design and its validation.

Status: implemented within the measured coverage, 18 October 2026. See the
[clock model and CLI reference](clocks.md) for the shipped behavior and limits.
The design below records the rationale and remaining identification work. Basis: [100 MS/s PLL characterization](../measurements/pll-100ms.md), its [portable aggregates](../measurements/pll-100ms-summary.json), and retained raw/firmware evidence.

## Intended behavior

Add an optional empirical clock model that reproduces measured system-clock trajectories and elapsed physical time while retaining Windtunnel's cycle-based instruction semantics. Preserve the present documented arithmetic model as an explicit ideal profile. An empirical profile must identify its specimen, conditions, measured coverage and uncertainty.

The model should distinguish:

1. Crystal/PLL software enable with the crystal initially disabled: quiet/low-rate interval, large excursion and acquisition over roughly 0.94 ms in the measured 160 MHz sequence.
2. PLL enable with an already running crystal: acquisition within the measured 32 µs block-end bound.
3. Live D/M changes: finite response, direction-dependent trajectory, slew and overshoot; 16–24 µs measured 1% bounds for the tested changes.
4. Postdivider changes with unchanged requested VCO: preserve the loop's state and change the divider; no resolved long acquisition in the first 8 µs. Exact phase/pulse behavior remains open.
5. Requests outside the measured tracking region: retain the requested target separately from observed/modelled output, and diagnose an unsupported or nontracking condition.

Direct P-up has returned incorrect data despite correct final frequency. Report a diagnostic for the observed sequence; do not manufacture register corruption, instruction errors or hangs from these limited observations.

## Integration points (pre-change behavior and implementation responsibility)

| Location | Pre-change behavior | Implemented responsibility |
| --- | --- | --- |
| `src/libp2/types.zig`, `ClockMode.get_frequency` | Pure nominal field decoding; fixed 24 MHz/20 kHz typical RC sources | Retain as nominal target calculation and encoding validation. Physical state belongs in Windtunnel. |
| `src/windtunnel/sim/IO.zig`, `setClock` | Replaces `clock_frequency` immediately on HUBSET | Submit a clock command at the correct execution boundary; expose requested and effective state separately. |
| `src/windtunnel/sim/Hub.zig`, `configure` / `step` | Executes one system edge and increments CT | Own/advance a stateful clock source and consume a shared physical interval for that edge. Preserve existing instruction and smart-pin ordering. |
| `src/windtunnel/sim/IO.zig`, `resolvePads` | Computes electrical dt as `1 / clock_frequency / oversample` | Consume the actual edge interval, split consistently over analog substeps. |
| `src/windtunnel/sim/Vcd.zig` | Generates half-period timestamps from a frequency; rejects zero | Emit the same integrated intervals used by the simulation. Represent quiet intervals without invented system edges. |
| `src/windtunnel/p2aas_server.zig` | Computes virtual time and scheduling batches from frequency/CT | Pace by accumulated physical time, including acquisition intervals without CPU edges. Avoid division by zero and CT-based busy loops. |
| Serial boot, terminal and GUI clock consumers | Read `clock_frequency` directly in several places | Audit consumers; distinguish nominal configuration, instantaneous/effective rate, and physical elapsed time. Keep ideal ROM handoff behavior deterministic. |

Do not modify generated instruction dispatch merely to add physics: HUBSET already routes through shared Hub configuration. Inspect all callers before choosing the exact clock-state ownership and API.

## State and time representation

Keep crystal enable/loading, PLL enable, selected system source, decoded D/M/P fields, nominal requested VCO, physical elapsed time and empirical acquisition state separately. Crystal and PLL state must evolve while RCFAST, RCSLOW or XI is selected. Selecting PLL must not restart a loop that has already acquired in the background.

Use one shared state for all cogs and smart pins. Optional measured RC values belong to a clock/specimen profile, not the chip's field decoder or default nominal constants. Initially support the primary board's known 20 MHz crystal condition; other crystal frequencies remain ideal or explicitly uncharacterized until validated.

Maintain fractional time and phase. Determine each system edge by integrating selected-source frequency until one system cycle has elapsed, rather than rounding frequency to integer MHz or assigning an arbitrary fixed delay. Clock phase, waveform half-cycle assumptions and divided-output phase must be distinguished: these captures do not identify internal clock duty or malformed pulse shapes.

A clock-not-ready episode must allow **physical time to advance without advancing CT, cogs or synchronous peripherals**. A bounded event advance can reach the next acquisition/profile event. Analog pads and external events may still need physical-time advancement. Prevent a stopped simulated CPU from freezing oscillator acquisition or cancellation handling.

The observed no-transition gaps are only bounds from divided outputs. Approximating an unready source as producing no usable edges is an explicit profile assumption, not proof that the silicon VCO has frequency zero. Do not encode the crystal-only PWM missing-pulse record as an oscillator state.

## Choose the response model from phase evidence

**Do not start with a universal first-order exponential or one fixed lock timer.** Separately fitted first-order models leave ~75 ns phase RMS for D-only, while a critical second-order step still leaves ~56 ns. Both monotonic forms miss the 160→200 MHz overshoot. Fitted ~0.8–1.3 µs delays partly compensate for model error and are not established physical dead times.

Model-selection sequence:

1. Package compact, versioned empirical response fixtures for the exact measured transitions. Retain clock fields, source/target state, enable/select markers, exporter configuration, phase observations and sampling uncertainty. Preserve an independent repeat and faster-exporter/NCO controls for validation.
2. Fit a minimal **slew-limited response with damped recovery**, or an underdamped state-space candidate if it demonstrably fits better. Allow rise/fall response and operating point to differ only where evidence requires it. Use continuous state across retargeting rather than restarting from the old nominal target.
3. Fit to integrated divided-clock phase and to block-rate shape. A controller's “settled” label alone is insufficient. Treat arbitrary phase offset as a nuisance parameter; constrain marker-latency assumptions from firmware and GPIO uncertainty rather than allowing a microsecond delay to conceal a wrong trajectory.
4. Freeze parameters on training records, then **predict** other repeats and faster-exporter controls without refitting response parameters. Current fit tables are separately fitted comparisons, not successful held-out predictions.
5. If a compact parametric model fails, ship an empirical replay/profile mode for the **measured configurations only**, using a phase-consistent interpolation constrained by the captured data. Flag uncovered D/M transitions explicitly and use the selected documented fallback. Do not imply that sparse profiles identify all D=1…64 and M=1…1024 dynamics.

For a response described as VCO frequency `Fvco(t)`, the selected output's cycle count follows the integral of `Fvco(t)/P`. For the measured composite startup, an empirical source-availability/profile sequence may be necessary before ordinary retuning applies. No charge-pump current, loop capacitance, VCO gain or internal feedback phase has been measured; assigning those physical circuit values would be speculative.

Parameterize any upper-rate behavior by the specimen profile. The 390/400 MHz requests produced derived VCO around 386.5–386.6 MHz before the marker and 385.6–385.7 MHz in later fits, whereas prior measurements were near 384.44 MHz. There is no identified universal clamp or calibrated drift law. Initially reproduce exact characterized nontracking cases or warn; do not extrapolate a temperature curve or a guaranteed maximum.

## Implementation stages

### 1. Time and state plumbing, ideal behavior preserved

Introduce a clock-state component with explicit command submission and physical interval advancement. Keep pure field decoding independent. Route Hub, pad integration, VCD and P2AAS pacing through the same interval. Provide exact nominal/ideal behavior first so current tests can detect scheduling regressions. Reset must discard profile state, restore the selected reset/ROM policy and clear phase/time remainders intentionally.

### 2. Measured acquisition and retune profiles

Add the characterized primary-board profile and its coverage metadata. Separate crystal enable, PLL enable, source select, D/M retune and P-only changes. Integrate during RC waits, preserve state during unchanged commands, handle mid-acquisition commands continuously, and retain an unsupported-coverage diagnostic. Implement only the model family that passes the frozen-parameter checks above.

Use the 32,768-RC-tick control to verify background acquisition: after the observed ~1,269 µs wait, source selection is already within the first 8 µs rate block. Do not reinterpret that one successful workload as a minimum universally safe startup wait.

### 3. Diagnostics and user-facing state

Expose requested VCO/system rates, effective modelled rate, profile name, acquisition status and coverage/unsafe-sequence diagnostics. These are simulator diagnostics, not invented hardware lock-register semantics. A nontracking PLL and a CPU running beyond a validated region are separate conditions. Keep speculative execution-failure injection out of the functional core.

## Validation and acceptance

- **Retain ideal regressions:** divider arithmetic, HUBSET/reset semantics, CT/WAITX, UART baud configuration, smart-pin/streamer timing and current VCD timestamp behavior where the ideal profile applies.
- **Time conservation:** oscillator acquisition during RC selection, source selection after background acquisition, one shared interval across all cogs/pads/VCD, no invented CT increments during quiet intervals, and no drift from interval rounding. Check phase/time carry across source and postdivider changes.
- **Replay the instrument:** render the actual fixture's PWM/NCO division and marker convention from the candidate clock trajectory, sample at 100 MS/s, then run the same analyzer. Compare 8 µs bounds, 2 µs shape, overshoot and integrated phase. Do not compare a model's internal lock time directly with an external block-end statistic.
- **Quantify mismatch:** report phase RMS **and peak**, frequency/block error, common phase/marker alignment assumptions, and forward-sampling error. Establish acceptance tolerances from stable-control residuals and the sampling bounds; a 5 ns edge-quantization uncertainty is not permission for 200 ns systematic phase error.
- **Separate gates:** UART checksum/seed/workload, elapsed plausibility and exported-pulse consistency are distinct. CT/PWM agreement is an internal consistency check, not independent calibration of the P2. Only the Saleae waveform supplies external timing here. P-up must remain a recorded correctness failure even though its rate profile passes.
- **Hold out actual evidence:** training on one PWM/32 record must be followed by frozen-parameter prediction of another repeat and the PWM/8 or NCO control. Report sparse coverage and specimen dependence rather than widening tolerances until every case passes.
- **Complete normal required checks after implementation:** read `build.zig.zon`, then use its required version (currently `zig-0.16.0`) for the applicable Windtunnel test/build targets. Implementation validation and frozen-profile results are recorded in [clocks.md](clocks.md#validation).

## Remaining identification work

Broader D/M directions, different initial/target rates, repeated P-only edge behavior, precisely controlled oscillator-off history, independent supply/temperature observations and additional specimens are needed before claiming a general physical PLL model. Mid-acquisition retargeting and brief enable/disable pulses need their own captures before their dynamics can be calibrated. Internal phase noise, single-cycle glitching, analog loop parameters and guaranteed hardware limits require better equipment or internal access and remain outside this plan's measured claims.

The existing setup is sufficient for later profile expansion at qualified divided-output rates. No immediate rewiring or additional hardware action is required to implement and validate the characterized scope above.
