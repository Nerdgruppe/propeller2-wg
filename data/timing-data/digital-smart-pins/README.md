# Digital smart pin validation

Bench: 2026-10-13/14. The implementation covers digital modes 4–23. UART,
USART, USB, ADC and DAC changes are outside this work; existing UART endpoints
carry the test reporter and host commands. All downloads use RAM, never flash.

## Bench and clocks

The primary board is accessed exclusively through `ws://localhost:12880/`.
The secondary is `/dev/serial/by-id/usb-Parallax_Inc_Propeller_P2-EVAL_PLX6ZJKWX-if00-port0`.
P8–P15 connect straight between boards. Logic 16 channels 0–7 observe primary
P0–P7 and channels 8–15 observe P32–P39. The preceding
[wiring validation](../wiring-check/README.md) checked all byte patterns in both
directions and the analyzer channel order.

Primary acceptance uses RCFAST and 200 MHz. Additional GPIO probes use 160,
166, 167, 168, 169, 170 and 200 MHz. The registered stable-band regression uses
160/170/200 MHz; the complete sweep is retained as research.

The clone was limited to 100 MS/s for three channels (0, 4, 5) and 50 MS/s for
four (0, 3, 4, 5). Although the user established 109 MS/s as a three-channel
limit, this device/API rejected that allocation and advertised 100 MS/s.
The user's eight-channel ceiling is 25 MS/s. No digital timing claims rely on
an overloaded full-channel capture.

## Validation results

- ReleaseFast: **191 fixtures, 3,718/3,718 runs passed**; 323 unit tests passed,
  two existing skips. Final Debug harness/debugger checks: **220/220 passed**.
- The last live oracle campaign matched **1,457** cases. Two local NCO assertions
  prevented upload in that campaign; the final implementation resolves them against
  independently captured isolate/reset traces. The separate 80-run reset capture,
  nine consecutive-read traces, and all 400 output-phase cases constrain the final
  model together. The archived live log retains those earlier local failures.
- Independent-board input checks: **276/276 passed**.
- Final VCD versus Saleae: **368/368** framed pin comparisons at 100 MS/s and
  **552/552** at 50 MS/s, including initial levels and every interior edge.

`evidence/validation.json` records the counts, hashes, capture configuration and
provenance; compressed logs preserve the complete validation output.

## Checks and evidence

All twenty modes have independently measured literal checklist expectations
in `tests/windtunnel/state/smart-pin-*.propan`. Characterization bypasses local
execution; only after collecting silicon observations does `fixtures.py freeze`
replace placeholders. It checks source identity, full run coverage and duplicate
observation stability. Ordinary CI replays the frozen expectations without hardware.

| Fixture | Coverage |
| --- | --- |
| output + output-phases | Seven output modes, eight instruction offsets, both clocks; waveform, Z/C/IN, ACK and reset |
| counting | Thirteen input modes, event selections, continuous and windowed operation |
| synchronous + synchronous-boundary | Clocked GPIO selection and fine input/output sampling boundaries |
| frequency-boundary | Stable frequency bands, normal and clocked GPIO, nine instruction gaps |
| nco-boundary + nco-isolate + nco-trace + nco-reset | Phase writes, divider timing, carry publication, consecutive reads and retained results |
| period-state | All period edge selections and coincident boundaries |
| lifecycle | Active X/Y writes, same-mode WRPIN, held reset, re-enable, zero/maximum encodings |
| routing | A/B Boolean operations, inversion, all unanimous filter lengths feeding smart counters/timers |
| events + gpio-events | WAITSE completion, re-arming, repeated completion and INT1 timing |
| clocked-history | Safe DIR-low clocked output configuration and settling |
| smps-feedback | Every initial A/B combination and recovery from digital gate conditions |

`waveforms.propan` produces one bounded 200 MHz pass for simulation. The analyzer
program repeats 800 passes, then releases P0–P7. Marker pulse groups identify
modes 4–10. The retained three- and four-channel captures include all seven
output modes and SMPS feedback changes. `compare_waveforms.py` compares initial
levels, edge polarities/counts and edge times in each driven interior window
(15–950 microseconds after the last marker fall). It calibrates oscillator scale
from independent same-polarity GPIO marker intervals; tolerance is two analyzer
samples. This comparison deliberately excludes the floating configuration interval.

`external.py` loads `peer.propan` onto the secondary through Turboprop and
`external.propan` onto the primary through P2AAS. The peer drives P8–P15 weakly
and atomically. Primary P10 routes external P8/P9 as SmartA/SmartB, with its
output disabled. The checks exercise forward/reverse quadrature, gated and
up/down counts, A-minus-B rises, and missing-event alarms at both clocks. The
boards have independent clocks, so these checks assert counts and alarm state,
not a shared-clock phase. Both banks are floated in cleanup; the peer also has
an idle watchdog. `external.json` records all 276 passing observations and
`evidence/external-images.json` records the exact primary sources and image hashes.

`evidence/characterization.tar.gz` contains 1,341 raw native characterization
folders. Each run includes its source, original/uploaded images, generated oracle,
constants and observation offsets, hardware register snapshot, checksummed
reporter bytes, request and transport log. `characterization.json` indexes those
observations and hashes. Some captures are research rather than acceptance:

- `p2-smart-characterization`: initial synchronous/counting captures; its early
  output source used weak drive and was superseded by `p2-smart-output-fast`.
- `p2-smart-frequency-unloaded`: full sweep, including the approximate intermediate band.
- `p2-smart-clocked-history-normalized`: active mux switching transients, superseded
  for acceptance by `p2-smart-clocked-safe`.
- `p2-smart-final-pad`: SMPS acceptance plus an earlier active-switching research source.

The captured source and constants, not the filename alone, define a measurement.
The analyzer `.sal`, CSV, comparison JSON, compact physical-pin VCD and capture
manifest are retained alongside the characterization archive. The first capture's
original uploaded image is retained; its reconstructed idle-loop source reproduces
that image byte for byte. The later capture used the retained source with an
explicit settling wait and COGSTOP after pin release; its image is reassembled
from that source. The driven mode interiors are identical. VCD timestamps
are system clocks; convert using 200 MHz, regardless of the synthetic `1ns` declaration.

## Reproduce

Use `zig-0.16.0`, as required by `build.zig.zon`:

```sh
zig-0.16.0 build install test -Dcoverage=false -Doptimize=ReleaseFast
zig-0.16.0 build test-windtunnel-unit test-windtunnel-debugger -Dcoverage=false
```

The fixture runner uses the built-in assembler. With the bench connected, compare
any fixture directly:

```sh
P2AAS_ENDPOINT=ws://localhost:12880/ zig-out/bin/windtunnel-tests --oracle tests/windtunnel/state/smart-pin-nco-reset.propan
```

For new research, keep characterization separate from freezing:

```sh
P2AAS_ENDPOINT=ws://localhost:12880/ zig-out/bin/windtunnel-tests --characterize --artifact-dir /tmp/new-pin-measurements tests/windtunnel/state/smart-pin-nco-reset.propan
python3 data/timing-data/digital-smart-pins/fixtures.py freeze tests/windtunnel/state/smart-pin-nco-reset.propan /tmp/new-pin-measurements
```

The optional external-board runner uses Python's standard library plus the
already-installed `websockets` package; it does not require pyserial. Run it only
with the stated wiring and no other owner of the primary P2AAS connection:

```sh
.venv/bin/python data/timing-data/digital-smart-pins/external.py
```

## Practical limits

The digital engine and architectural timing checks are strict at RCFAST and
200 MHz. The pad model remains digital: it does not reproduce drive-strength
voltage, Schmitt hysteresis, analog jitter, or the exact RNG tap assignment.
The later 1 MHz-reference PLL sweep crossed into the longer GPIO path already
at 166/167 MHz, unlike the earlier sweep. The simulator retains a frequency-only
170 MHz threshold, so intermediate PLL/load-dependent pad behavior is approximate.

Active WRPIN switching of a clocked output mux can expose physical retained-latch
transients whose exact phase is not modeled. Safe DIR-low configuration and
settled clocked operation have their own passing regressions. These research
observations are retained, rather than promoted to false exact expectations.

Synthetic unit checks exercise saturation, counter rollover, full 2^32 zero
encodings, absolute CT rollover, and `fast_io` isolation. They do not claim that
a 2^32-clock hardware trace was collected. Existing serial endpoint modes retain
their previous functional model.


The optional first-completion probe expired its requested one-second runtime
allowance; the recorded close does not establish that a shared quota was exhausted.
P2AAS allows 60 seconds per run. The original close and request are retained in
`evidence/quota-limit.json`. The additional ten-repeat campaign was not collected. `nco-first-completion.propan` is unmeasured
research with placeholder assertions and is not registered in CI.
`repeat.py` is ready to perform 329 hardware runs per iteration, comparing every
captured register with the frozen source value without rewriting expectations:

```sh
python3 data/timing-data/digital-smart-pins/repeat.py --count 10 --artifact-dir /tmp/p2-smart-repeats
```

No successful repetition results are claimed for that blocked campaign.
