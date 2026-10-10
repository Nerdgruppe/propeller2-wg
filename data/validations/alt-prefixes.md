# ALTx hardware measurements

Measured on 2026-10-10 through the physical P2AAS service at
`ws://localhost:12880/`, using the existing compiled `propan` and
`windtunnel-tests` binaries. No Zig command was needed. The service does not
report the board or silicon revision.

## ALTD before MODCZ

`ALTD` substitutes MODCZ's D literal, including its C/Z truth tables.
It does not supply the contents of the register named by that literal.

```text
MOV     selector, 0xff
ALTD    selector, 0
MODCZ   _CLR, _CLR :wcz
// Observed C=1, Z=1.
```

[altd-modcz.propan](altd-modcz.propan) checks all 256 pairs of truth tables
against all four initial C/Z states, using shifts to compute the expected
flags. All eight cogs reported `checked=1024`, `failures=0`: 8192 cases total.
An additional control substitutes D=`0xf0` while register 240 contains `0x0f`;
every cog produced C=1,Z=0, establishing that the field bits supply the tables.

The current simulator fails this probe with 768 mismatches per cog. In
`src/windtunnel/sim/execute.zig`, `execute_alu` takes MODCZ's D literal from
`state.instr` and ignores `state.alt_d`. The probe lives outside the default
simulator fixture suite so that it can retain the hardware expectation without
changing the simulator as part of this measurement task.

## Prefix chains

[alt-chain.propan](alt-chain.propan) measures all nine ordered pairs on cog 0:

| First prefix | Following ALTD | Following ALTS | Following ALTR |
| --- | --- | --- | --- |
| ALTD | D substituted | D substituted | D substituted |
| ALTS | S substituted | S substituted | S substituted |
| ALTR | Pointer writeback redirected | Pointer writeback redirected | Pointer writeback redirected |

Each chain contains three adjacent instructions: prefix, prefix, MOV. Distinct
target data distinguish the final register selection; signed increments
distinguish the second prefix's writeback. ALTR changes where that prefix's
updated D value is stored while its next-instruction selection still uses its
original D and S inputs. ALTD changes both the D read and its writeback address.

All nine pairs matched the expected register snapshots. Three additional cases
confirm that ALTS can replace an immediate S field of ALTD, ALTS, and ALTR.
All twelve cases also pass in the current simulator.

## Reproduction and retained evidence

```sh
P2AAS_ENDPOINT=ws://localhost:12880/ \
  zig-out/bin/windtunnel-tests --characterize \
  --artifact-dir=.zig-cache/alt-prefix-hardware \
  data/validations/altd-modcz.propan data/validations/alt-chain.propan
```

`--characterize` captures hardware without checking the simulator or asserting
postconditions. For this measurement, all 108 requested register observations
in the 20 captured `hardware.json` files were separately compared with the
fixture postconditions and matched. Evidence is retained locally under
`.zig-cache/alt-prefix-hardware/`, including the uploaded images and raw UART
frames. For MODCZ, check `failures=0`, `checked=1024`, and `literal_control=2`.
