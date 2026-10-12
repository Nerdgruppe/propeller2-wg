# Local PC overflow hardware parity

Recovered and implemented on 2026-10-12 using the P2AAS endpoint
`ws://localhost:12880/`. The eleven `tests/windtunnel/state/local-pc-*.propan`
fixtures contain **207 cases**. Two independent retained captures agree on every
observed register/flag; a third independent `--oracle` run compares every
observation with both the assertions and simulator. `measurements.json` retains
those observations and SHA-256 digests of both full reporter frames. It also
includes seven existing boundary cases with two retained matching captures and
a third successful oracle run. The summary records the 214/214 boundary and
157/157 existing pipeline, FIFO transition, interrupt, skip and branch comparisons.
Local `oracle.log` and `regressions.log` files are ignored, reproducible run output.
The full
`zig-0.16.0 build install test -Dcoverage=false -Doptimize=ReleaseFast --summary all`
run passes 23/23 build steps, 95 Windtunnel harness tests and 2,423 fixture runs
across 162 fixtures; Propan passes 102 tests with two skipped.

| Fixture | Cases | Coverage |
| --- | ---: | --- |
| `local-pc-fetch` | 7 | Distinct S-selected CALLD destinations and complete twenty-bit rollover |
| `local-pc-modifiers` | 12 | Immediate/register S, ALTS/ALTD, AUGS and cancellation |
| `local-pc-loc` | 18 | Positive/negative LOC offsets before, at and after overflow |
| `local-pc-repeat` | 60 | LOC in REP lengths 0..4/counts 1..3, two dispatch paths, cogs 0/7 |
| `local-pc-branches` | 12 | Absolute/relative CALLD, explicit/implicit returns |
| `local-pc-interrupts` | 20 | CT1 phases, full saved PC and LOC/ADD observations |
| `local-pc-carries` | 12 | Higher carries, alternating LUT windows, PC `$FFFFE` |
| `local-pc-resume` | 4 | RETI1/RESI1 selecting hub execution for saved PC `$404` |
| `local-pc-skip` | 36 | SKIP/SKIPF/EXECF crossing, patterns and clock counts |
| `local-pc-special` | 18 | Architectural special registers versus underlying RAM/debug ROM |
| `local-pc-writes` | 8 | Old RAM data on a simultaneous write and overflow fetch |

`tests/windtunnel/local-pc-lut.propan` is the shared explicitly initialized,
executable all-NOP LUT image used for higher carries and rollover.

```sh
zig-0.16.0 build install -Dcoverage=false -Doptimize=ReleaseFast
P2AAS_ENDPOINT=ws://localhost:12880/ zig-out/bin/windtunnel-tests --oracle tests/windtunnel/state/local-pc-*.propan
P2AAS_ENDPOINT=ws://localhost:12880/ zig-out/bin/windtunnel-tests --characterize --artifact-dir /tmp/local-pc-capture tests/windtunnel/state/local-pc-*.propan
```

Characterization artifacts contain source, assembled/uploaded images, request,
complete binary reporter frames, decoded register snapshots and transport logs.
Absolute setup/timer values in the full frames can differ; the regression
observations are register signatures and modulo-32-bit GETCT differences.

The original `pc-stamps.propan`, `pc-stamps.json`, `rep-cross-wrap.propan` and
`rep-cross-wrap-frame.bin` are retained as historical evidence. The crossing REP
fixture is now registered and passes with the original hardware expectation,
**one addition**. Its ADD at `$3FF` selects cog register 1, containing the exit
branch; the full logical PC is retained. The older whole-PC wrap model produced
six additions. Existing local-boundary fixtures now install explicit exits in
that S-selected register instead of depending on incidental setup instructions.

See [accumulated knowledge](../../../docs/accumulated.md#local-pc-overflow-and-loc-2026-10-12)
for the measured fetch, timing and LOC rules and their coverage limits. The LOC
fixtures retain raw encodings from the measurements; Propan's relative-LOC label
encoding is now fixed and checked by `tests/propan/sema/loc-addresses.propan`.

The separate `tests/windtunnel/state/local-pc-iret.propan` fixture adds eight
simulation regressions: a no-interrupt control and unmodified RETI1/RETI2/RETI3
returns into hub `$404`, on cogs 0 and 7, including C/Z restoration and an intact
IRETx. It is not included in the 207 measured cases above. Live oracle validation
of these new cases was unavailable due to a transport error; the four original
RETI1/RESI1 captures in `measurements.json` remain the hardware evidence for the
domain transition.
