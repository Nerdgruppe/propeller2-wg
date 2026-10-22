# Scopecreep

A C#/.NET 10 MCP server for the Micsig STO1104C, with stdio and streamable HTTP.
It combines Linux USBTMC configuration, live screen observation and GUI input through
[the persistent TCP relay](relay/README.md), and a read-only MTP file client.

Video, left-button input and MTP capture retrieval are hardware verified. USB support is
firmware-specific; only verified mappings are enabled. MTP listing, repeated session opening,
streaming download and explicit retention were checked on the attached scope; see
[FIRMWARE.md](FIRMWARE.md). Raw waveform access
and physical sample scaling remain unverified; screenshots and retrieved files do not
claim to be calibrated waveform objects.

## Build and run

```sh
dotnet build src/scopecreep/scopecreep.csproj
src/scopecreep/bin/Debug/net10.0/linux-x64/scopecreep
```

Use `--http` to serve `http://127.0.0.1:3001/mcp`. Both modes expose the same tools.
For a self-contained single executable:

```sh
dotnet publish src/scopecreep/scopecreep.csproj -c Release -r linux-x64
```

Supported build targets are Linux x64, arm64, and musl x64; hardware verification is
on Linux x64. No Zig build is required.

**FFmpeg is an explicitly approved runtime dependency for screen decoding.** The C# server
starts it directly with redirected pipes, without a shell or Python. USBTMC and MTP do not
require FFmpeg, libmtp, libusb, VISA, or an installed .NET runtime after publishing.
A missing decoder or relay does not prevent USB tools from starting.

Start the relay separately, approve its scope connection if prompted, then start MCP.
The relay accepts only one local client: stop `utility/scope-capture.sh` or other viewers
before MCP takes the slot. Restarting MCP preserves the relay's scope connection.

Use `--config scopecreep.json` to change defaults:

```json
{
  "devicePath": "/dev/usbtmc0",
  "timeoutMs": 5000,
  "httpUrl": "http://127.0.0.1:3001",
  "remoteEnabled": true,
  "remoteHost": "127.0.0.1",
  "remotePort": 8888,
  "ffmpegPath": "ffmpeg",
  "screenSettleMs": 5000,
  "captureDirectory": "captures/scopecreep",
  "maxFileBytes": 536870912,
  "maxTemporaryBytes": 1073741824
}
```

Paths resolve relative to the config file, or the working directory without a config.
A bare `ffmpegPath` uses PATH. Set `remoteEnabled: false` for USB-only operation.
The same remote client can connect directly to the scope by changing `remoteHost`, but
that connection then needs the scope's approval and does not retain the relay's restart protection.
HTTP binds loopback by default and provides no authentication for a wider binding.

## Agent workflow

1. Call `scope_status` to inspect independent USB, video/decoder, and MTP health.
2. Prefer `scope_configure` for verified USB settings. A `needsGui` result suggests
   a menu/preset; the agent chooses the fallback after inspecting the screen.
3. Call `scope_screen` and visually inspect the current layout before GUI operations.
4. Execute `scope_press`, `scope_drag`, or a verified `scope_action`. These return after
   transmission/gesture completion, without waiting for video.
5. Call `scope_screen(afterActionId: "<receipt ID>")`. It waits through the default
   five-second settling interval and then obtains a newer host-observed frame.
6. Confirm the intended state visually. Request another image if it is not visible.
   If `superseded` is true, another action intervened; inspect that state before continuing.
7. For exports, select the source, local destination and format in the Save menu, inspect
   the generated filename, and explicitly save. Do not confirm an overwrite. Wait for
   completion, then list MTP files to identify the new object. Large exports can take much
   longer than five seconds: poll the screen until the busy indicator clears and the
   generated filename advances. For frozen-memory captures, keep acquisition stopped
   until saving finishes. Recheck the Save source after changing channel visibility.
8. Download with `scope_file_get`; inspect chunks with `scope_file_read`. Use
   `scope_file_save` only for selected files that should persist.

There is no one-call acquisition/export engine, OCR, automatic GUI fallback, or automatic
UI-state restoration. Named actions require the stated screen context; the agent checks it.
CH1–CH4 are one-based instrument names. GUI presets advertise only verified argument choices.

## Tools

| Tool | Purpose |
|---|---|
| `scope_status` | Identity/acquisition readback and independent connection health, capabilities, latest frame and last action. Does not open MTP as a side effect. |
| `scpi_query` | Raw query, text or lossless base64; 1–16 MiB response bound. Firmware errors are explicit. No physical units are inferred. |
| `scpi_command` | One serialized raw write; action receipt retains uncertainty and is never automatically replayed. |
| `scope_screen` | Native MCP `image/png` plus JSON frame metadata. Optional `afterActionId`, `settleMs`, and `timeoutMs` (default 15 seconds). |
| `scope_press` | Left press at `(x,y)`, duration default 100 ms; use 1000 ms for long press. Allowed duration 10–10000 ms. |
| `scope_drag` | Held-left movement from one coordinate to another, default 500 ms; approximately 100 ms steps, then release. |
| `scope_actions` | Lists available presets, argument choices, transport and required view; unavailable candidates are separate. |
| `scope_action` | Executes one verified named action with optional string `argument` and `frameId`. |
| `scope_configure` | Validates all requested fields first; returns per-field verified/transmitted/coerced/needsGui/unknown/notAttempted results. |
| `scope_files` | Omit `parentRef` for storages, then list a returned storage/folder reference. Shallow pages: `offset`, `count` default 100, maximum 500. |
| `scope_file_get` | Stream an MTP object into temporary storage; return download ID, filename, byte count, SHA-256 and server-local path. |
| `scope_file_read` | Bounded byte ranges for stdio/HTTP clients. Default 64 KiB, maximum 1 MiB, format `base64` or strict UTF-8 `text`. |
| `scope_file_save` | Copy a selected download into `captureDirectory` using a plain filename, without overwriting existing files. |

Coordinates are native 800×600 pixels: x=0..799, y=0..599. Optional GUI `frameId` rejects
unknown, expired (>30 seconds), and old-connection references. Hover movement is not
supported by the recovered client protocol. Right-button and wheel input are not exposed.

Typed settings include `runState`, `single`, `channels` (channel/enabled/probeFactor/coupling/
voltsPerDiv/offsetVolts), `timebase` (secondsPerDiv/positionSeconds), `trigger` (source/slope/
levelVolts/mode), and `acquisition` (mode/depth). `scope_status` identifies enabled USB mappings;
unverified fields report `needsGui` rather than attempting a speculative write. The timebase
setter requires visual review because its getter is broken. Physical voltage scaling remains
gated because the setter uses pre-probe units and the probe getter is unreliable.

## Observation and failure semantics

Frame timestamps are **host receive/decode times**, not instrument capture times. The
video encoder and decoder can delay observation by several seconds. A newer timestamp
alone never proves the intended state. Static pictures still produce new frame IDs.
First-image startup can also wait several seconds for an IDR/keyframe.

A single coordinator serializes gestures, USB mutations and configuration batches across
HTTP callers. The video receiver remains independent. Action receipts last for the current
MCP process (256 recent receipts); frames are invalidated on disconnect. Unknown writes
are never replaced by GUI actions or retried. A gesture interrupted by disconnect is not
resumed; only pending button release is sent on the replacement connection.
Startup also sends a button release before accepting input, to recover a previous process's
interrupted press. It does not repeat that process's action.

The known instrument `Micsig,STO1104C,16298,5` has reversed run/stop commands. Typed
controls verify this identity before using that profile. Raw SCPI is available for diagnostics
on other instruments but carries no model compatibility promise.

USBTMC recovery closes/reopens and checks identity twice; firmware rejects USBTMC clear,
so it is omitted. Cancellation awaits native I/O before releasing transaction ownership.
MTP separately claims interface zero and uses discovered bulk endpoints; it never detaches
or resets USBTMC or invokes libmtp reset recovery. A complete protocol rejection leaves
its session synchronized; an incomplete transaction disables further MTP requests until a
physical USB reconnection or server restart. A changed USB device number invalidates old
MTP object references. File IDs/paths are not stable device configuration.

MTP needs read/write permission on the underlying `/dev/bus/usb/...` node even for reading
files, because commands must be sent. USBTMC permission alone does not grant MTP access.
Device symlinks do not make hidden device nodes accessible inside a sandbox.

Downloads are unchanged scope files, without CSV/BIN/WAV interpretation. The default limits
are 512 MiB per file and 1 GiB total temporary storage. Failed downloads are discarded;
completed temporary downloads are removed on orderly server shutdown. Explicit saves survive.
The scope's originals are never deleted. Crash leftovers may remain under the private
`/tmp/scopecreep/` process directory; the server never broadly deletes another process's files.

## Validation and diagnostics

```sh
dotnet run --project src/scopecreep/tests/checks.csproj
python3 src/scopecreep/tests/smoke.py
python3 src/scopecreep/tests/remote.py
```

The checks use byte transcripts and do not touch hardware. Smoke tests exercise both MCP
transports with a missing device and video disabled. The remote test uses a private fake
scope, synthetic H.264 from FFmpeg's libx264 encoder, native image replies, input timing,
delayed observations, and disconnect/reconnect cleanup. These tests need loopback access.

Hardware verification and retained screen evidence are recorded in
[the integration report](diagnostics/integration-2026-10-17/README.md). A physical USB
reconnection resolved the previously stuck MTP endpoint. The already-running MCP recovered
automatically and retrieved a CH1 CSV; a clean MCP restart reopened MTP and downloaded an
identical file. The CSV agrees with the known 1 kHz, approximately 0–2 V reference, but its
80 ns exported spacing differs from the scope's displayed 500 MSa/s acquisition rate.
The [P2 P22 capture example](diagnostics/p2-stimulus-2026-10-17/README.md) records a complete
stimulus/upload, frozen CH2 export, MTP retrieval and measured pulse timing/duty workflow.

The standalone USB diagnostic mode remains available:

```sh
src/scopecreep/bin/Debug/net10.0/linux-x64/scopecreep --diagnose src/scopecreep/diagnostics/identity.json
```

Diagnostic scripts contain explicit commands/queries and optional `expectedText` guards.
They are prevalidated before I/O, retain exact reply bytes in JSONL, and never connect the
remote display. See [firmware findings](FIRMWARE.md) and the
[relay/video/control evidence](diagnostics/remote-relay-2026-10-17.md).
