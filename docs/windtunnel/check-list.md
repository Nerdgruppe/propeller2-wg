# Windtunnel fixtures

A `.propan` fixture starts with `//? WINDTUNNEL CHECK LIST`, followed by contiguous
`//?` lines. The first ordinary source line ends the checklist. Run the suite with
`zig-0.16.0 build install test-windtunnel`, or select files with
`zig-out/bin/windtunnel-tests FILE.propan ...`.

## Runs and initial values

Conditions before the first `run:` apply to every run. Each `run:` starts a new
section containing additional `pre:` and `post:` conditions. Runs may also
select a `cog:` and supply `const:` assembler parameters. Names are optional
quoted strings; explicit names must be nonempty and unique within the fixture.
A fixture without `run:` has one implicit run. The common section does not create
an extra run when explicit sections exist.

```cpp
//? WINDTUNNEL CHECK LIST
//? timeout-ms: 1000
//? pre: sym[input] = u32 [ 10 ]
//? post: cog[0].c == false
//? run: "common input"
//? post: cog[0].reg[result] == 11
//? run:
//? pre: sym[input] = u32 [ 20 ]
//? post: cog[0].reg[result] == 21
//? run: "common input restored"
//? post: cog[0].reg[result] == 11

    MOV result, input
    ADD result, 1
    JMP nrel(_end)
var input: LONG 0
var result: LONG 0
```

A matrix can share its source and assertions while varying assembly-time values:

```cpp
//? WINDTUNNEL CHECK LIST
//? const: size = 4
//? post: cog[*].reg[failures] == 0
//? run: "cog 0, long"
//? cog: 0
//? run: "cog 7, byte"
//? cog: 7
//? const: size = 1
```

`cog[*]` means the selected cog for that run. Explicit `cog[N]` targets must
still match the selected cog. Run-local cog selection defaults to the shared
`cog:` value; it does not carry into the next run.

`const: name = INTEGER` supplies a Propan constant before assembling the fixture
for either backend. Integers accept the same bases and separators as other
checklist numbers. Common constants apply to all runs, with run-local values
overriding matching names. A name may appear only once in each section; names
reserved for the oracle (`_wt_...`) are rejected. Constants cannot duplicate
symbols declared in the fixture. Use constants for layout or instruction choices
and `pre: sym[...]` for initial memory values. Each named matrix case is a normal
run with its own fresh assembly, oracle upload, result, and failure evidence.

`pre: sym[name] = BLOCK` patches the assembled image at the symbol's hub byte
offset before startup. Cog variables are initialized by loading this patched
image, so the same syntax seeds hub memory and cog registers. Symbols must exist,
be unambiguous, and have an emitted hub address. Patches must be nonempty and fit
entirely in emitted fixture memory; they cannot touch oracle instrumentation.
They may cover multiple values or only part of a value.

Every run starts with a fresh assembly and simulator. Common patches apply first,
then local patches in source order; later patches overwrite overlapping bytes.
Unpatched bytes always retain their original assembled values. Neither patches
nor mutations during execution carry into subsequent runs. The hardware oracle
uses the same patches, resolved against its own assembly.

Existing flag and Q seeds remain available:

```cpp
//? pre: cog[0].c = true
//? pre: cog[0].z = false
//? pre: cog[0].q = 0x12345678
```

A local seed for C, Z, or Q replaces the common seed for that field. Repeated
seeds for a field within the effective section are errors. Common and local
postconditions are all checked. `sym[...]` is only a precondition target;
`cog[N].reg[...]` and `hub[...]` are postcondition targets.

## Values and observations

Blocks support `u8 [ ... ]`, `u16 [ ... ]`, `u32 [ ... ]`, and `hex [ ... ]`.
Integer elements are emitted in little-endian order. Decimal, hexadecimal, binary,
underscores, and negative values that fit the element width are accepted. Commas
are optional, and blocks can span multiple checklist lines. Hex blocks contain
pairs of hexadecimal digits, for example `hex [ 00 80 ff ]`.

```cpp
//? post: cog[0].reg[result] == 0x12345678
//? post: hub[buffer] == u16 [ 0x1234, -1 ]
//? post: cog[0].c == true
//? post: cog[0].z == false
//? post: cog[0].q == 3
```

Register addresses may be symbols or numeric register indices. Hub addresses may
be symbols or numeric byte offsets. State targets select the configured cog; `cog[*]` follows run-local selection.
Register 0 and registers 506..511 belong to the scaffold or special registers;
hub observations must refer to emitted fixture memory.

## Shared configuration

Configuration directives precede all run sections and apply to every run.
`cog:` and `const:` may also occur inside a run to override their shared values:

| Directive | Default and meaning |
| --- | --- |
| `profile: cog` or `program` | `cog`: imported isolated fixture with state snapshots; `program`: complete standalone program |
| `cog: N` | `0`; target cog, 0..7, for the cog profile |
| `entry: symbol` | Start of imported code; optional alternative entry for the cog profile |
| `stop: symbol` | Optional checkpoint for a program fixture; cog fixtures always exit through `_end` |
| `max-cycles: N` | `1000000`; positive simulator cycle limit per run |
| `timeout-ms: N` | `5000`; hardware timeout, 100..10000 ms |
| `baudrate: N` | `115200`; UART baud rate |
| `oracle: required` | Default; hardware checks run when enabled |
| `oracle: off "reason"` | Skip hardware checks for the stated nonempty reason |
| `stdin: STREAM` | Empty; program UART input |
| `stdin-after: STREAM` | Empty; output prefix that signals input readiness |
| `stdout: STREAM` | Empty; exact expected program UART output |

Streams accept blocks or quoted strings with `\n`, `\r`, `\t`, `\xNN`, `\"`,
and `\\` escapes. Nonempty stdin requires `stdin-after`, which must be a prefix of
the expected stdout. Program fixtures support symbol patches and serial checks;
cog fixtures support state checks and reserve serial I/O for the reporter.

The runner reports each run separately and continues after a failed run. Its exit
code is nonzero if any fixture or run fails. Failure evidence and
`--prepare-oracle` artifacts record the run name and patched image in separate
directories. Enable live hardware checks with `--oracle` and `P2AAS_ENDPOINT`.

See [core-pixels.propan](../../tests/windtunnel/state/core-pixels.propan) for a
fixture using named and unnamed runs, shared seeds, overrides, and partial patches.
