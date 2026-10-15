# Propeller 2 timing assertions

This directory contains YAML timing cases and captured results, the shared
`fixture.propan`, and `run_tests.py`. Exploratory measurement campaigns and generated
captures are not part of this project. Simulator regression fixtures live in
`tests/`; hardware findings are recorded in `docs/accumulated.md`.

Build Propan first, then run from the repository root:

```sh
zig-0.16.0 build install
python data/timing-data/run_tests.py
```

For smart-pin command, routing and GPIO timing assertions:

```sh
python data/timing-data/run_tests.py \
  --cases data/timing-data/smart-pin-cases.yaml \
  --results data/timing-data/smart-pin-results.yaml
```

The runner checks eight instruction phases per case and writes the observed results
to the selected YAML file. Use `|` literal blocks for assembly in YAML.

Hardware defaults to P2AAS at `ws://localhost:12880/`; `P2AAS_ENDPOINT` overrides it.
An empty endpoint selects direct serial with `turboprop`. The fixture retains
RCFAST and the serial ROM's calibrated UART dividers at the 2 Mbaud upload rate.
Python requires PyYAML and, for P2AAS, websockets.
