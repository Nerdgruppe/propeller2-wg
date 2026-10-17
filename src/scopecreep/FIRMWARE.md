# STO1104C firmware verification — 2026-10-16

## Release gate

The intended server must return reliable, correctly scaled waveforms. That gate has not
passed. No waveform decoder, capture storage, typed configuration, or typed measurements
should be presented as working based solely on the supplied command manual. The later
integration below implements independent screen/file handling and individually tested controls.

The later approved integration exposes visual control and read-only file retrieval independently
of this raw-waveform gate. Typed USB controls are admitted individually by firmware verification;
they do not imply correctly scaled sample retrieval.

Device identity: `Micsig,STO1104C,16298,5\r\n`. The final field is the instrument's reported
software value; it is insufficient to establish compatibility with newer STO models.
Transport: Linux `/dev/usbtmc0`. Channel names: CH1–CH4. CH1 has the stated 1 kHz, 0–2 V
reference signal; CH2 is connected to P2 P22.

## Observations

The initial query-only probes and subsequent production C# diagnostics found:

| Query | Raw reply or outcome | Interpretation |
|---|---|---|
| `*IDN?` | `Micsig,STO1104C,16298,5\r\n` | Identity reads work. |
| `:MEASure:FREQ? CH1` | `1000.0\r\n` | Agrees with the stated reference frequency. |
| `:MEASure:PKPK? CH1` | `0.0\r\n` | Does not validate the stated 2 V peak-to-peak signal. |
| `:CHANnel1:DISPlay?` | `1\|ON\r\n` | Boolean format differs from the manual's example. |
| `:CHANnel1:PROBe?` | `0.001\r\n` | Physical probe correspondence unverified. |
| `:CHANnel1:SCALe?` | `1.0\r\n` | Effective units unverified. |
| `:TIMebase:EXTent?` | `0.0\r\n` | Cannot establish a usable sample interval. |
| `:TRIGger:EDGE:LEVel?` | `230.0\r\n` | Units unverified; do not assume volts. |
| `:ACQuire:SRATe?` / `:ACQuire:DEPTh?` | `ErrorEnum:SCPI Command!` | Unterminated firmware error reply. |
| `:WAVeform:FORMat?` | `ErrorEnum:SCPI Command!` | Documented format query not verified. |
| `:WAVeform:SOURce?` / `:WAVeform:PREamble?` / `:WAVeform:DATA?` | Read timeout | No waveform payload or scaling metadata obtained. |

Abbreviated and uppercase waveform variants were also attempted. Some failed during
recovery before the requested command was transmitted; these are not evidence that those
particular spellings are unsupported.

We attempted the documented source/mode/format/start/stop setup using WORD and ASCII.
Data queries still timed out. However, reliable STOP could not be established, so these
attempts do **not** validate or conclusively reject stopped raw-memory transfer.

Initial run/stop behavior was unresolved. Repeated status reads after `:MENU:RUN` remained STOP;
`:MENU:STOP` was followed by RUN in another sequence. A final guarded probe required RUN,
sent STOP, and still read RUN after one second; it stopped before sending waveform setup
commands. At that point, these observations did not justify a toggle workaround. The screenshot-assisted
follow-up below establishes a reversed command mapping instead.

USBTMC clear returned `EPIPE`. Closing/reopening and checking identity recovered several
failed transactions, but other recovery attempts also timed out. Known status queries
occasionally timed out too. This could involve firmware processing or USB transfer state;
the exact cause has not been established.

## Reproduction and next step

Run `diagnostics/identity.json` first, then `diagnostics/waveform-queries.json` with the
compiled diagnostic client documented in the README. The recorded query-only transcript
is `diagnostics/observed-2026-10-16.jsonl`; it contains exact replies, elapsed time, and
recovery failures, including whether the requested command was withheld during recovery.

Before implementing the remaining approved plan:

1. Establish model-specific SCPI support and the actual scope application/firmware version.
   Compare the physical acquisition display with query replies when someone is available.
2. Verify a reliable run/stop/single-acquisition sequence and channel/probe settings.
3. Demonstrate a CH1 waveform with independently checked time/voltage scaling. Record
   the wire format, byte order, signedness, preamble layout, and supported transfer sizes.
4. Only then implement typed control, scalar units, arm/collect ownership, aligned channel
   capture, preview/pagination, temporary retention, and explicit saving from the approved plan.
5. Validate P22 with nominal 20 MHz RCFAST through Cockpit. Validate all four channels
   once physically connected.

No SCPI reset, calibration, firmware update, or P2 program upload was performed.
The later libmtp investigation attempted USB reset recovery, as recorded below.
The supplied 2024 manual is retained as the primary candidate reference. An additional
vendor STO SCPI manual (2022-11-04) documents the same waveform command family, but does
not establish support on this device:
[Vendor STO SCPI manual](https://www.micsig.com.cn/uploads/STOSCPI%E6%8C%87%E4%BB%A4%E6%9C%80%E6%96%B0_1667553489.pdf)

## MTP fallback investigation

USB descriptors confirm two independent interfaces:

| Interface | Function | Endpoints |
|---|---|---|
| 0 | Vendor-class MTP (`iInterface: MTP`) | bulk OUT `0x01`, bulk IN `0x81`, interrupt IN `0x82` |
| 1 | USBTMC/USB488 | bulk OUT `0x02`, bulk IN `0x83` |

The installed libmtp 1.1.23 successfully opened one MTP session and listed the root of
storage `65537`, including a `smart` directory (object ID 16 in that session). Object IDs
are discovery results, not stable configuration values. The USB descriptor's serial string
also differs from the SCPI identity serial; do not equate those identifiers.

A second libmtp session failed to open. The library attempted USB reset recovery and then
failed; no additional reset was requested. A bounded direct USBFS OpenSession attempt on
interface 0 subsequently timed out, as did a read checking for a pending response. Thus,
MTP existence and one root listing are proven; reliable repeated access is not.

The documented storage queries `:STORage:SAVE:SOURce?`, `:STORage:SAVE:LOCAtion?`,
`:STORage:SAVE:TYPE?`, and `:STORage:SAVE:FILename?` all returned
`ErrorEnum:SCPI Command!`. No save command was issued, because a unique destination
filename could not be verified. No device file was created, overwritten, or deleted.

The candidate save sequence is:

```text
:STORage:SAVE:SOURce CH1
:STORage:SAVE:LOCAtion LOCal
:STORage:SAVE:TYPE CSV
:STORage:SAVE:FILename "scopecreep-<unique-id>"
:STORage:SAVE:START
```

This sequence is documented, **not verified**. MTP could retrieve the resulting file if
saving and repeated MTP access can be established. It does not by itself resolve coherent
multi-channel acquisition or trigger completion. The user manual describes CSV scaling
metadata but also warns of decimation and loss of narrow extrema; verify resolution on
this four-channel model before using CSV to characterize P2 timing. Prefer a verified
full-resolution BIN/WAV path when CSV cannot retain the needed detail.

Do not embed libmtp's default recovery in the MCP server: it can reset the composite USB
device, and its native stdout diagnostics would corrupt stdio MCP. A future implementation
must avoid unsolicited resets, constrain access to this scope, and preserve protocol stdout.


## Screenshot-assisted follow-up — 2026-10-17

The user supplies an optional, continuously refreshed `/tmp/micsig.png` from the port-8888
H.264 stream. It has several seconds of latency and may stop updating while the user
reverses the control protocol. Check modification times and allow delayed observations;
do not treat an unchanged frame as evidence of a current instrument state. Scopecreep
originally had no runtime dependency on this external screenshot producer. The integrated
MCP now connects to the persistent local relay, reconstructs video in C#, and supervises an
explicitly approved FFmpeg subprocess for native MCP image responses.

With the scope application visibly running, open CH1 measurement queries now returned
full-precision values agreeing with the display: approximately 999.98 Hz, 1.000 ms period,
2.015 V maximum, 0 V minimum, 0.984 V mean, and 3.2 us rise time. CH2 frequency returned
zero. The earlier rounded/zero readings are historical observations, not permanent
measurement limitations.

Opening `PKPK,CH1` produced approximately 1.999 V and displayed that measurement.
Its numeric query remained nonzero after removal: measurements can retain cached values.
A later frame omitted PKPK after `:MEASURE:CLOSE PKPK,CH1`, but it subsequently
reappeared without another OPEN command. CLOSE therefore did not establish reliable
removal, and casing was not shown to be causal. Never infer freshness or availability
from zero versus nonzero values alone.

The documented per-slot cleanup `:MEASURE:CLEAR ITEM7` cleared **all** displayed
measurements. Reopening the original six CH1 items restored FREQ, RISETIME, MAX, MIN,
and MEAN, confirmed in a fresh screenshot. PERIOD did not reappear using PERIOD, the
manual's alternate PEROID spelling, the exact mixed-case/spaced example, or abbreviated
PER. Acquisition is back in RUN, but the original period display item still needs restoration
through another control path. Do not use CLEAR as a per-item operation on this firmware,
and do not silently open/close measurements around reads in production until UI-state
preservation is resolved.

The screenshot showed probe factors of 10X, 500 us/div, 500 MSa/s, and 3.5M depth.
Both mixed-case and uppercase SCPI probe/timebase queries still returned `0.001` and
`0.0`, respectively. Sample-rate and depth queries still returned firmware errors.
These settings must not be exposed as verified effective values.

### Verified run/stop mapping on this attached instrument

| SCPI command | Observed effect |
|---|---|
| `:MENU:RUN` | Stops acquisition. RUN -> STOP; repeated calls from STOP leave it stopped. |
| `:MENU:STOP` | Starts acquisition. STOP -> RUN; calls from RUN leave it running. |

Status queries and delayed screenshots corroborated these observations. This is the
opposite of the supplied manual, rather than a toggle workaround. Scope status must be
verified after writes; do not assume the same quirk applies to other models or software.

### Confirmed stopped-memory transfer test

Using that mapping, a new probe verified STOP before sending the full uppercase source
CH1, RAW mode, WORD format, start 1, stop 1000 sequence. DATA and PREAMBLE queries
still timed out. Identity recovered, status remained STOP, and cleanup sent `:MENU:STOP`
then verified RUN twice. The trace is `diagnostics/stopped-raw-2026-10-17.jsonl`.

Thus the stopped-transfer test is now valid with respect to acquisition state, although
individual waveform setters still lack effective-setting verification. No waveform payload
or scaling preamble has been obtained. The waveform release gate remains unmet.


## Integrated MCP controls and file retrieval — 2026-10-17

See [the integration verification report](diagnostics/integration-2026-10-17/README.md) for
verified USB setters, GUI actions, the completed on-device export, retained screen evidence,
and the unresolved native MTP first-response timeout. The implemented 13-tool MCP API
keeps visual/file control independent of the unpassed calibrated-waveform release gate.


### MTP resolved after physical USB reconnect — 2026-10-17

The user reconnected USB. The same running MCP detected the new enumeration and listed
internal storage without restarting or resetting USB. It retrieved the previously saved
`smart/csvwave/2610170001.csv` (1,947,572 bytes). After a clean MCP restart, a second
MTP session retrieved identical bytes; stale references were rejected, temporary files were
cleaned up, and the explicitly saved copy survived. Video kept its original relay connection.

The CSV contains 87,500 CH1 points at 80 ns spacing over 7 ms. Its measured frequency
is 1,000 Hz, with extrema −0.02 and 1.9799999 V, agreeing with the reference. The file
contains 40 times fewer points than the displayed 3.5M record and differs from the displayed
500 MSa/s acquisition rate. This establishes single-channel CSV retrieval and known-signal
agreement, without establishing raw transfer, narrow-extrema preservation or multichannel
alignment. See the integration report for hashes, data analysis and retained plots.
