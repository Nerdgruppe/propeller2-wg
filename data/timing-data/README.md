# Propeller 2 Instruction Timing Data

This folder contains a test fixture + test runner to collect real-world data from
a Propeller 2 how long instructions take to be executed.

The hardware endpoint defaults to `ws://localhost:12880/`. Set `P2AAS_ENDPOINT`
to select another endpoint, or set it to an empty string for the serial workflow.
The fixture keeps RCFAST and the serial ROM's calibrated UART dividers. Both
transports use the 2 Mbaud upload rate, with no initial clock/UART settling wait.
Collect the smart-pin command and GPIO probes with:

```sh
python data/timing-data/run_tests.py \
  --cases data/timing-data/smart-pin-cases.yaml \
  --results data/timing-data/smart-pin-results.yaml
```

The committed results are hardware captures for eight instruction phases per
probe at RCFAST. GPIO's earliest changed TESTP/MOV samples arrive one clock sooner
than the earlier 200 MHz captures; the two boundary expectations reflect this.
The [frequency sweep](serial-boot/gpio-frequency.json) retains the stable bands
and marginal samples. Assembly in YAML uses `|` literal multiline strings. These probes cover
repository readback/reset/acknowledgement, simultaneous cog command writes,
digital routing, GPIO readback latency and per-cog DIR gating of OUT.
`tests/windtunnel/state/smart-pin-hardware.propan` checks the same observations
locally and with `windtunnel-tests --oracle`.

