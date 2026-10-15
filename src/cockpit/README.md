# Cockpit

Cockpit is an MCP server for Propeller 2 work. It assembles Propan source or PASM2 DAT source with FlexSpin, runs the result through P2AAS, and looks up instruction metadata. Propan uses stdin and stdout. FlexSpin uses temporary source and binary files, which are removed after each assembly, including failed assemblies.

## Run

Build with `dotnet build src/cockpit/cockpit.csproj`, then start from the repository root (replace `<runtime-id>` with the built target, such as `linux-x64` or `win-x64`):

```sh
dotnet src/cockpit/bin/Debug/net10.0/<runtime-id>/cockpit.dll
```

This starts stdio MCP. For streamable HTTP MCP at `http://127.0.0.1:3000/mcp`:

```sh
dotnet src/cockpit/bin/Debug/net10.0/<runtime-id>/cockpit.dll --http
```

Use `--config path/to/cockpit.json` with either mode. Example:

```json
{
  "p2aasEndpoint": "ws://127.0.0.1:12880/",
  "flexspinPath": "flexspin",
  "propanPath": "propan",
  "instructionsTsvPath": "data/encoding/instructions.tsv",
  "p2instructionsJsonPath": "data/encoding/p2instructions.json",
  "httpUrl": "http://127.0.0.1:3000"
}
```

Relative paths in a config file resolve from that file's directory. Without a config file, instruction data is found by searching upward from the working directory or executable location; the packaged copy is used outside the repository. Omit either instruction path from a config file to use its packaged copy. Bare executable names use `PATH`.

Start with `--development` to expose `mcp-reboot`. The tool acknowledges the call and then exits cleanly, allowing an external control loop to restart Cockpit. It is absent without that flag.

## Tools

- `assemble`: `language` is `propan` or `pasm2`. Propan supports `hex`, `listing`, and `none` output formats; PASM2 supports `hex` and `none`. PASM2 input is a complete FlexSpin source with a `DAT` section. A failed assembly returns diagnostics and `success: false`.
- `run`: assembles, uploads, and runs through P2AAS. It accepts UTF-8 `stdin` or binary `stdinBase64`. It returns output as `hex`, UTF-8 `text`, or arrays of `u8`, `i8`, `u16`, `i16`, `u32`, or `i32`. Multi-byte values are little-endian. Set `baudrate` and `timeout_ms` per call (defaults: 115200 and 5000 ms). Set `scaffold: true` to run a short Propan or PASM2 snippet with the startup and UART helpers below. A timeout or dropped connection after upload returns the captured output as a successful run. A failed WebSocket handshake returns the HTTP status and P2AAS error header.
- `lookup_instruction`: returns every JSON encoding option and TSV reference row for a mnemonic.
- `search_instructions`: searches descriptions and mnemonics, returning all variants for each matching mnemonic.

### Scaffolded runs

The embedded [Propan](RunScaffold.propan) and [PASM2](RunScaffold.spin2) templates set the clock to 200 MHz, configure UART TX on pin 62 and RX on pin 63 at the call's `baudrate`, wait for the clock to settle, execute `code`, then stop the current cog. When `stdin` is supplied, Cockpit waits briefly for the UART setup before sending it. Use `timeout_ms: 300` or longer for the 100 ms startup wait and serial output.

`write_byte` transmits the byte in PA and waits for transmission to finish. `read_byte` is nonblocking: it sets Z when no byte is ready; otherwise it puts the received byte in PA and clears Z. Propan snippets call them like this:

```propan
CALLPA 'X', write_byte
read_loop:
    CALL read_byte
if(Z) JMP read_loop
    CALL write_byte
```

PASM2 uses `CALLPA #"X", #write_byte` and `CALL #read_byte`. Its character literal and immediate syntax differ from Propan. The snippet should contain instructions and labels, without its own startup or `DAT` section.

The default HTTP listener accepts local connections only. The configured P2AAS endpoint must be a WebSocket URL. FlexSpin's full Spin2 compilation is outside this PASM2 DAT tool.

After building, run `python3 src/cockpit/tests/smoke.py` to exercise both transports, assembly, lookup, and a mock P2AAS session. The test needs Python's `websockets` package and permission to bind loopback ports.
