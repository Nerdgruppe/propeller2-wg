# Propeller 2 Working Group

This repository contains several projects related to the Parallax Propeller 2.

## Engineering Philosophy

Optimize for architectural coherence, not minimal diffs.

This codebase is actively evolving. Internal APIs, abstractions,
and data structures may be changed or removed when doing so
improves the overall design.

Do not preserve backward compatibility by default.

## Refactoring Policy

When changing an interface:

- Find and inspect every call site.
- Reconsider the semantics at each call site.
- Update callers explicitly instead of introducing defaults,
  overloads, wrappers, or compatibility shims.
- Remove obsolete abstractions and dead code.

Prefer a coherent system-wide refactor over a localized
workaround.

Do not treat existing code as inherently correct.

## Decision Ownership

Follow the user's architectural intent.

You may challenge assumptions and identify contradictions,
but do not silently substitute your own architecture.

When implementation reveals a design conflict, explain
the conflict instead of hiding it behind a workaround.

## Validation

Verify global invariants, not just local test success.

A compiling implementation is necessary, not sufficient.

## Propeller 2

Read more on the Propeller 2 here: docs/p2.md

## Projects

The following projects are managed in this repository:

- [P2DB](data/p2db/) - A formalized semantic instruction database
- [Timing Data](data/timing-data/) - A project to extract reproducible timing data from the chip
- [Propan](src/propan/) - A new, modern assembler for the Propeller 2
- [Turboprop](src/turboprop/) - A tiny application loader that allows launching programs on the Propeller 2 over serial port
- [Windtunnel](src/windtunnel/) - A clock cycle-exact simulator for the Propeller 2
- [VsCode Extension](vscode-extension/) - A Visual Studio Code extension meant to support the other projects

## Propeller 2 Documentation

The following official documentation is shipped inside the repository:

- [Datasheet](docs/p2/P2X8C4M64P_datasheet.pdf)
- [Instruction Set Summary](docs/p2/Instructions.xlsx)
- [Hardware Manual](docs/p2/Hardware_Manual.docx)
- [PASM2 Manual](docs/p2/PASM2_Manual.docx)
- [Silicon Documentation](docs/p2/Silicon_Documentation.docx)
- [SPIN2 Documentation](docs/p2/Spin2_Documentation.docx)
- [SPIN2 Manual](docs/p2/Spin2_Manual.docx)
- [Questions & Answers](docs/p2/Questions_and_Answers.xlsx)

The following community documentation is available as well:

- [p2docs](docs/p2docs.github.io/)
- [Smart Pins Documentation](docs/community/Smart_Pins.docx)

## Working in the Repository

### Persistent Memory

- If `.memory/environment.md` exists, read it for available hardware wiring, setup, and access information.
- Keep active task progress, decisions, discoveries, blockers, and next steps in `.memory/task.md`.
- Write information to `.memory` immediately when it comes up, before continuing; unwritten context can be lost.
- Clear `.memory/task.md` when the task is complete.
- Keep necessary project information missing from `AGENTS.md` and existing documentation in `.memory/project.md`.
- Never clear `.memory/project.md`.
- Use terse, compressed notes in both files; omit prose, preserve information.

### General Rules

Primary directive: Do not change code when only a question was asked. Questions are never an implicit request for changes.

There's a `justfile` in the workspace root which has these recipies:

- `just validate`: Executes all validation tasks.
- `just build`: Builds the Zig based software
- `just validate-p2db`: Validates the P2DB schema and database
- `just update-windtunnel`: Regenerates
  - `src/windtunnel/sim/encoding.zig` with `utility/gen_windtunnel.py encoding ...`.
  - `src/windtunnel/sim/decode.zig` with `utility/gen_windtunnel.py decoder ...`.
- `regenerate-windtunnel-executor`: Regenerates the contents of `src/windtunnel/sim/execute.zig` with `utility/gen_windtunnel.py executor ...`. Use with care!
- `just setup-venv`: Sets up a virtual environment in the repository. Do not use unless explicitly asked to!
- Do not depend on external tools unless it's optional. This is done to keep the system maintainable. This means:
  - dotnet projects just use their compiled binary, do not invoke external tools
  - zig projects just use their compiled binary, do not invoke external tools
  - python projects just use their compiled binary, do not invoke external tools
- `P2AAS_ENDPOINT` is always `ws://localhost:12880/` unless stated otherwise.
- When writing YAML files, always use `|`-based multiline strings instead of `'`-based ones.
- Functions always need a document comment on what they do and, if necessary, why they do it

### Propan Assembly Sources

- Follow the official Propan coding style and use `propan --pretty-print` to format your code files.
- Assembly typically requires good code documentation, so for each semantic block, add at least a comment
  that explains what this block is supposed to do.

### Propeller-2 Knowledge

When you find knowledge about the Propeller 2, write your gained knowledge to [Accumulated Propeller 2 Knowledge](docs/accumulated.md).

This includes behavioral properties, semantics, edge cases, ...

### Zig-based Projects

Propan, Turboprop and Windtunnel are implemented in Zig (see `build.zig.zon` for required version).

Typical workflows (started from the workspace root):

- `${zig} build install test`: Build the projects and run their test suites.
- `${zig} build install -Dno-emit-bin`: Compile the projects, but don't write the output. This is a quick syntax check.

- The GUI linker warning can be safely ignored and does not need fixing.

- Instead of using `@tagName(v)` or `@errorName(v)` + `{s}` formatting, use `v` directly and `{t}` formatting.
- Instead of `writer: anytype` use `writer: *std.Io.Writer`
- Instead of `reader: anytype` use `reader: *std.Io.Reader`
- Do not use `@import("...").field`. Always introduce a meaningful import alias in the file.
- Do not use bare integers for things that are non-arithmetic.
  - Example: `operation: u3` is not a case where an integer is appropriate, it sounds like `operation` is something one can add 3 to. Better: `operation: OperationKind` + `OperationKind = enum(u3) { ... }`.
  - Use fully open enums for IDs that are not arithmetic values or indices.
- Do not use bare integers for bitfields.
  - Always prefer `packed struct(Int)` over `Int` unless you need dynamic indexing into the bitfield.
  - When a value can have multiple layouts, use `packed union(Int)` with the different layouts as variants.
  - Do: `if (command.control & (1 << 23) != 0)`
  - Don't: `if (command.control.write_fifo)`
  - If you need to use bit masking still, don't just bare bit masks like (`1<<23`) but use a named constant instead.
    - Do: `value & NAMED_MASK`, `const NAMED_MASK = (1 << 17);`
    - Don't: `value & (1<< 17)`
- Never compare enumerations by their numeric value unless that number has an arithmetic semantics.
- Do not write long if-else chains when a switch is usable.
- Do not use `else` in a switch on an enumeration unless the switch only selects a small portion of the
  enumeration. Prefer explicit listing of enumerators.
- For enumerations where some members share the same property or category, introduce query functions
  - Do: `if (mode.is_schmitt())`
  - Don't: `if (mode == .schmitt or mode == .schmitt_feedback or mode == .schmitt_adjacent_feedback)`
- Code Style
  - These formatting rules apply to all control-flow constructs, including `if`, `switch`,
    `while`, `for`, and similar constructs, whether used as expressions or statements.
    Never put an entire control-flow construct on one line or cram nested constructs onto one line.
    When computing a value, use a multiline expression and keep the result `const`; do not replace
    it with an `undefined` temporary assigned in branches. Use labeled blocks when an expression
    branch needs multiple statements. The `if` examples below illustrate the general rule.

    ```zig
    // Forbidden:
    const x = if (cond) value else other;

    // Forbidden:
    var x: Type = undefined;
    if (cond) {
        x = value;
    } else {
        x = other;
    }

    // Allowed:
    const x = if (cond)
        value
    else
        other;

    // Allowed:
    const x = if (cond) blk: {
        // More code here.
        break :blk value;
    } else blk: {
        // More code here.
        break :blk other;
    };
    ```

  - When a control-flow construct does not produce a value, use statement blocks for its bodies
    and branches. A single `return`, `break`, or `continue` may omit braces where the syntax allows,
    but must be on the next line. Function calls require braces, even when the call is on the next line.

    ```zig
    // Forbidden:
    if (cond) return 10;
    if (cond) break;
    if (cond) continue;
    if (cond) call();
    if (cond)
        call();

    // Allowed:
    if (cond)
        return 10;
    if (cond)
        break;
    if (cond)
        continue;
    if (cond) {
        call();
    }
    ```

    ```zig
    // Forbidden:
    const x = switch (kind) { .first => value, else => other };
    for (items) |item| call(item);
    while (cond)
        call();
    if (cond) for (items) |item| call(item);

    // Allowed:
    const x = switch (kind) {
        .first => value,
        else => other,
    };
    for (items) |item| {
        call(item);
    }
    while (cond) {
        call();
    }
    if (cond) {
        for (items) |item| {
            call(item);
        }
    }
    ```

  - Try to keep the lines under 120 characters.
    - This is not a hard limit, but shorter lines are better than cramping everything into one line.
  - Try to keep lines free of multiple field assignments.
    - Multiple assignments are ok when the assignments are simple `key = value` assignments with `value` being a variable or a short literal.
    - Bad style: `.direction = .{ .write = .{ .immediate = if (!block or (immediate and !cog.q2)) value else null, .masked = masked, .origin = if (cog.debug.state == .active) .debug else .application } },`
  - Do not cramp multiple independent expressions (e.g. arguments) into a single line.
    - Bad style: `if (!cog.return_debug(args.s, args.s_imm, args.c_mod == .write and args.z_mod == .write, target))`
      - Better assign the return value to a constant, and put each argument onto its own line for clarity.
  - Do not use inline structs in function signatureds.
    - Bad style: `pub fn start_cog(hub: *Hub, index: u3, options: struct { hub_address: u32 = 0, ptra: u32 = 0, load_image: bool = true, clocked: bool = true, command_clock: ?u64 = null }) !void {`
      - Better: `const StartCogOptions = struct { hub_address: u32 = 0, ptra: u32 = 0, load_image: bool = true, clocked: bool = true, command_clock: ?u64 = null, };  pub fn start_cog(hub: *Hub, index: u3, options: StartCogOptions) !void {` which will format into readable code.
  - Do not care for "legacy compatibility".
    - General rule: Introducing a breaking change to a function or type signature should require an update of all relevant code locations to ensure there's not a potentially missed semantic effect.
    - Do not introduce new fields with defaults to keep the change count low. Introduce them with defaults if there's never a reason to initialize them otherwise (except testing).
      - Bad style: `pub const Write = struct { origin: Hub.WriteOrigin = .application, ... }` which would then mask all potential code sites using the `Write` struct which are now affected by new semantics.
    - Do not introduce redirection functions to keep old function call locations valid.
  - Do not cramp code together without empty lines. Empty lines communicate intent and logical blocks 
    - Insert an empty line afte a `defer` to keep it clearly separated from the following code
- Always format your code with `zig fmt`

#### Propan

Do not modify `tests/propan/format/expected.propan` or
`tests/propan/format/layout-expected.propan`. Treat these files as fixed formatter
specifications and fix the formatter to match them.

When working on Propan, always prefer writing a `.propan` file in `tests/propan` over unit tests when applicable.
End-to-end tests are beneficial as they prevent potential future regressions also in other parts of the codebase,
and they are way easier to review.

Use the `docs/propan/check-list.md` feature to annotate your expectations.

### P2DB

The P2DB is written in CUE and located in `data/p2db`.

Typical workflows in that folder:

- `cue vet isa.cue`: Check the syntax of the schema for errors.
- `cue vet -c isa.cue examples.yml`: Check the example file for errors.

### Hardware Test Clock

- Use nominal 20 MHz RCFAST for hardware measurements and oracle tests.
- Use another clock rate only when technically necessary, such as testing high-speed behavior.
- Smart-pin behavior tests use 20 MHz: slower operation makes signals easier to observe.

### Timing Data

The timing data project is meant to collect timing data from a system-attached Propeller 2-Board.

Typical workflows in that folder:

- `python data/timing-data/run_tests.py`
