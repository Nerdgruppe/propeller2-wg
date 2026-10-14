# COGID, hub stacks, and digital input measurements

Captured on the attached P2AAS board on 2026-10-13 using
`ws://localhost:12880/` and the existing Windtunnel characterization runner.
The ten registered fixtures contain **131 runs**, all also checked against both
the simulator and the hardware oracle. Zig binary: `zig-0.16.0`.

- `measurements.json` lists each run, assembly constants, named register
  observations with their register indices, flags/Q, and program stdout.
- `evidence.tar.gz` retains the exact fixture and generated oracle sources,
  original/uploaded binaries, request metadata, complete register snapshots, and
  raw hardware output frames. Entries are grouped by fixture and run name.
- SHA-256 identifies the captured fixture source; register indices come from
  the runner's observation metadata, rather than guessing from binary layout.

The retained fixtures cover:

| Fixture | Observation |
| --- | --- |
| `cogid-forms` | Executing IDs 0–7, immediate no-result form, D/flags, augmentation, conditional cancellation, and status IDs 0–15 |
| `cogid-timing` | All eight command phases for register ID, immediate status, and immediate no-result forms, checked against the measured CT phase |
| `debug-stack-protection` | All four CALLA/B target forms inside INT0 and application code |
| `adjacent-feedback` | Logic/Schmitt feedback, both pin parities, synchronous input, inversion, and relative selectors |
| `input-logic` | Complete Boolean A/B truth tables |
| `input-filter-history` | WRPIN/HUBSET/DIR history retention, bypass, switching settings, and frozen initialization |
| `input-filter-length` | Shortening a frozen partial history changes the output without another sample |
| `input-filter-threshold` | Exact OUT-selector and filt0 visibility thresholds after OUTH |
| `input-filter-timing` | Rising/falling traces for every length, taps 0/1/2/5, and two instruction phases, normalized for WAITCT1 resume variation |
| `input-filter-shared` | All four global settings and lengths, shared by two GPIO inputs with DIR low |

Rebuild and replay (the endpoint defaults to the value above):

```sh
zig-0.16.0 build install
zig-out/bin/windtunnel-tests --oracle \
  tests/windtunnel/state/cogid-forms.propan \
  tests/windtunnel/state/cogid-timing.propan \
  tests/windtunnel/state/adjacent-feedback.propan \
  tests/windtunnel/state/input-logic.propan \
  tests/windtunnel/state/input-filter-history.propan \
  tests/windtunnel/state/input-filter-length.propan \
  tests/windtunnel/state/input-filter-threshold.propan \
  tests/windtunnel/state/input-filter-timing.propan \
  tests/windtunnel/state/input-filter-shared.propan \
  tests/windtunnel/program/debug-stack-protection.propan
```

Replace `--oracle` with `--characterize --artifact-dir /tmp/digital-input-capture`
to retain fresh raw observations without checking simulator behavior. A capture
alone is not a passing regression check.

Synthetic Zig tests cover X/Z sampling, rejected short pulses, independent A/B
histories and smart-mode input delivery, output-disabled feedback with external
drivers, CT31/rollover, and write-origin retention across stalls. Highest-period
CT taps and the existing endpoint-based smart engines are not claimed to have
complete waveform-level silicon validation. FIFO privilege is outside this work.

Final repository verification:

```sh
zig-0.16.0 build test-windtunnel-debugger test-windtunnel-unit test-libp2 -Dcoverage=false
zig-0.16.0 build install test -Dcoverage=false -Doptimize=ReleaseFast
```

The Debug headless suites passed **215/215** tests. The complete optimized build
passed **173/173 fixtures and 2,564/2,564 fixture runs**, with **306 tests passed
and two skipped**. The optimized run completed the long clock-stepped startup
waits; the redundant unoptimized fixture run was stopped.
