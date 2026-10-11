# CORDIC hardware measurements

These are the **1,080 physical Propeller 2 measurements** used by
[cordic-arithmetic.propan](../../tests/windtunnel/state/cordic-arithmetic.propan).
They were captured through the local P2AAS hardware service at
`ws://localhost:12880/` on **2026-10-10**, in 56 batches on cog 0. The final fixture
was subsequently validated against hardware on cogs 0, 3 and 7 for all eight
operations; [validation.log](validation.log) retains that run, including other
CORDIC regressions and larger random validation runs.

| Operation | Captured rows |
| --- | ---: |
| QMUL | 35 |
| QDIV | 35 |
| QFRAC | 35 |
| QSQRT | 35 |
| QROTATE | 235 |
| QVECTOR | 235 |
| QLOG | 235 |
| QEXP | 235 |

The first 35 inputs for each operation comprise ten explicit boundary cases and
25 random triples from Python's `random.Random(724)`, with one shared generator
advanced in the operation order above. Four operations received another 200
inputs during investigation. [cases.json](cases.json) freezes every input and its
original batch order, so reproduction does not depend on reconstructing the
later random generator state.

## Files and data format

- [vectors.json](vectors.json) contains flattened records with `op`, `d`, `s`,
  `q`, `x` and `y`. All numeric values are unsigned 32-bit words; signed coordinates
  use two's-complement representation. `x` and `y` are hardware `GETQX`/`GETQY`
  results. Inputs unused by an operation remain in the record.
- [cases.json](cases.json) names each batch and lists its input triples in
  **D, S, Q** order. The regression fixture stores **Q, D, S, X, Y**, grouped by
  operation; within each operation, row order matches this archive.
- [captures/](captures/) preserves the original artifacts without modification,
  in directories named after their probes. Each contains `fixture.propan`, the
  instrumented `oracle.propan`, `original.bin`, `uploaded.bin`, `request.json`,
  `hardware-output.bin`, `hardware.json`, `simulation.json` and `transport.txt`.
  The binary UART frame includes the hardware register snapshot and CRC.
- [capture.py](capture.py) uploads those source probes through the repository's
  native characterization client, or extracts the archived frames offline.

The historical filename `simulation.json` contains observation metadata used
to locate the named result registers. Extraction reads result words from
`hardware-output.bin`, validates its header, length and CRC, and cross-checks
`hardware.json`. The native client's `--characterize` mode captures hardware
without executing or checking Windtunnel. Placeholder zero assertions in the
probe sources identify registers to observe; they are not expected results.

The original sources and images remain archived byte for byte. Fresh captures
assemble those sources using the current compiler, so source formatting, image
bytes, register addresses and capture directory names can differ while the
extracted measurement rows remain identical.

## Reproduce from the saved hardware frames

From the repository root, using Python 3.9 or newer and its standard library:

```sh
python3 data/cordic/capture.py --extract-only \
  --artifacts data/cordic/captures \
  --output /tmp/cordic-replayed.json
cmp data/cordic/vectors.json /tmp/cordic-replayed.json
```

This validates all 56 frames, reproduces all 1,080 rows, and checks that the
regression fixture contains precisely the captured tables. No board, simulator
execution, Python packages or compiled tools are needed for offline extraction.

## Recapture on physical hardware

Build the native tools with the version required by `build.zig.zon`, currently
`zig-0.16.0`:

```sh
zig-0.16.0 build install -Doptimize=ReleaseFast -Dcoverage=false
P2AAS_ENDPOINT=ws://localhost:12880/ python3 data/cordic/capture.py \
  --artifacts .zig-cache/cordic-recapture \
  --output /tmp/cordic-recaptured.json
cmp data/cordic/vectors.json /tmp/cordic-recaptured.json
```

Use a running P2AAS service connected to the physical Propeller 2. The script
defaults to `ws://localhost:12880/`; `P2AAS_ENDPOINT` selects another service.
Each batch uploads a program and resets the board. The native client handles
assembly, instrumentation, upload, UART framing and capture. Python needs only
its standard library and invokes `zig-out/bin/windtunnel-tests`.

Each invocation creates a fresh `capture-*` directory beneath `--artifacts`,
retaining per-probe evidence and `capture.log`. The script writes the extracted
JSON, compares it with the committed corpus and checks the fixture tables.
A measurement difference produces a nonzero exit status while preserving the
new data for inspection. It does not overwrite the committed captures.

To repeat the fixture's hardware comparison across cogs 0, 3 and 7:

```sh
P2AAS_ENDPOINT=ws://localhost:12880/ zig-out/bin/windtunnel-tests --oracle \
  --artifact-dir .zig-cache/cordic-validation \
  tests/windtunnel/state/cordic-arithmetic.propan
```

The inferred arithmetic and pipeline behavior are described in
[Accumulated Propeller 2 Knowledge](../../docs/accumulated.md#cordic-computer).
