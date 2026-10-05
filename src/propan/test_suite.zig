const std = @import("std");
const propan = @import("propan.zig");
const cases = @import("test-cases");
const test_options = @import("test-options");

test "Flat output must use the configured byte for all padding." {
    const expected: []const u8 = &.{ 0x7E, 0x7E, 0x7E, 0x7E, 0xAA, 0x7E, 0x7E, 0x7E, 0xBB, 0x7E, 0x7E, 0x7E, 0x44, 0x33, 0x22, 0x11 };

    var run: Run = .{};
    defer run.deinit();
    run.options.format = .flat;
    run.options.@"fill-byte" = 126;
    run.options.output = "-";
    run.inputs = &.{"tests/propan/regressions/fill-byte.propan"};

    run.expected_stdout = expected;
    try run.check();
}

test "Differences in incomplete final words must remain visible in the diff." {
    const reference: []const u8 = &.{2};
    var run = Run.init(.compare, "tests/propan/regressions/compare-partial-word.propan", reference);
    defer run.deinit();
    run.expectExitCode(1);
    run.expectStdOutMatch("@00000: expected: 0x00000002");
    run.expectStdOutMatch("actual: 0x00000001");
    try run.check();
}

test "Multi-file input is rejected before either file is analyzed." {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .flat;
    run.options.output = "-";
    run.inputs = &.{ "tests/propan/regressions/multi-file-first.propan", "tests/propan/regressions/multi-file-diagnostic.propan" };
    run.expectExitCode(1);
    run.expectStdOutEqual("");
    run.expectStdErrEqual("error: multiple input files are not supported\n");
    try run.check();
}

test "Imports search the containing file first, then CLI include paths in order." {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .none;
    run.options.@"test-mode" = .sema;
    run.include_paths = &.{ "tests/propan/sema/fixtures/include-a", "tests/propan/sema/fixtures/include-b" };

    run.inputs = &.{"tests/propan/sema/fixtures/include-case/main.propan"};
    run.expectStdErrEqual("");
    try run.check();
}

test "Import-once identity is independent of relative, absolute, and include paths." {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .none;
    run.options.@"test-mode" = .sema;
    run.include_paths = &.{"tests/propan/sema/fixtures"};
    const absolute_path = try std.Io.Dir.cwd().realPathFileAlloc(std.testing.io, "tests/propan/sema/fixtures/import-once-alias-leaf.propan", std.testing.allocator);
    defer std.testing.allocator.free(absolute_path);
    const source = try std.fmt.allocPrint(
        std.testing.allocator,
        "//? PROPAN CHECK LIST\n//? mem: 0 == u8 [9]\n" ++
            ".import \"tests/propan/sema/fixtures/import-once-alias-leaf.propan\"\n" ++
            ".import \"{s}\"\n" ++
            ".import \"import-once-alias-leaf.propan\"\n",
        .{absolute_path},
    );
    defer std.testing.allocator.free(source);
    run.source = source;
    run.inputs = &.{"-"};
    run.expectStdErrEqual("");
    try run.check();
}

test "Diagnostics in an imported file retain its path and source excerpt." {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .none;
    run.inputs = &.{"tests/propan/sema/import-diagnostic-source.propan"};
    run.expectExitCode(1);
    run.expectStdErrMatch("fixtures/import-bad.propan:1:1: error");
    run.expectStdErrMatch("UNKNOWN_MNEMONIC");
    try run.check();
}
test "imported local labels in listings" {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .none;
    run.options.@"list-file" = "-";
    run.inputs = &.{"tests/propan/sema/import-local-scope.propan"};
    run.expectStdOutMatch("00004 | 001 | second:local");
    try run.check();
}
test "import source paths in JSON" {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .json;
    run.options.output = "-";
    run.inputs = &.{"tests/propan/sema/import-basic.propan"};
    run.expectStdOutMatch("fixtures/import-repeat.propan");
    try run.check();
}

test "Data labels have a hub address but no jump PC in JSON metadata." {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .json;
    run.options.output = "-";
    run.inputs = &.{"tests/propan/sema/data-mode.propan"};
    run.expectStdOutMatch("\"mode\": \"data\"");
    run.expectStdOutMatch("\"none\": {}");
    try run.check();
}

test "LUT jumps expose a 9-bit index within LUT memory." {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .json;
    run.options.output = "-";
    run.inputs = &.{"tests/propan/sema/lut-mode.propan"};
    run.expectStdOutMatch("\"lut\": 4");
    try run.check();
}

test "Scoped and literal dotted labels have distinct debug names and addresses." {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .json;
    run.options.output = "-";
    run.inputs = &.{"tests/propan/sema/local-labels.propan"};
    run.expectStdOutMatch("\"name\": \"foo:loop\",\n      \"segment_id\": 0,\n      \"offset\": 8");
    run.expectStdOutMatch("\"name\": \"foo.loop\",\n      \"segment_id\": 0,\n      \"offset\": 12");
    try run.check();
}
test "scoped and dotted labels in listings" {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .none;
    run.options.@"list-file" = "-";
    run.inputs = &.{"tests/propan/sema/local-labels.propan"};
    run.expectStdOutMatch("00008 | 002 | foo:loop");
    run.expectStdOutMatch("0000C | 003 | foo.loop");
    try run.check();
}
test "local labels retain segment metadata" {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .json;
    run.options.output = "-";
    run.inputs = &.{"tests/propan/sema/local-label-segments.propan"};
    run.expectStdOutMatch("\"name\": \".loop\",\n      \"segment_id\": 2,\n      \"offset\": 272");
    try run.check();
}

test "Cases that require expected failures or a comparison reference." {
    const nop_reference = "\x00\x00\x00\xF0" ++ "\x00" ** 32;
    var nop_disasm = Run.init(.compare, "tests/propan/sema/nop-encoding.propan", nop_reference);
    defer nop_disasm.deinit();
    nop_disasm.expectExitCode(1);
    nop_disasm.expectStdErrEqual("");
    nop_disasm.expectStdOutMatch("expected:             ROR 0x0, 0x0");
    nop_disasm.expectStdOutMatch("actual:               NOP");
    nop_disasm.expectStdOutMatch("actual:               ROR 0x0, 0x0");
    nop_disasm.expectStdOutMatch("actual:   if(C)       ROR 0x0, 0x0");
    const reference: []const u8 = &.{ 1, 0, 0, 0 };
    const warning_reference: []const u8 = &.{1};
    const all_modes: []const propan.TestMode = &.{ .parser, .sema, .compare };
    const build_modes: []const propan.TestMode = &.{ .sema, .compare };
    const failure_cases = [_]PropanTestCase{
        .{
            .path = "tests/propan/regressions/hexadecimal-string-tail.propan",
            .modes = &.{.sema},
            .result = .{ .failure = &.{"assertion failed: AZB!"} },
        },
        .{
            .path = "tests/propan/sema/diagnostics/fit-message.propan",
            .modes = &.{.sema},
            .result = .{ .failure = &.{"assertion failed: code exceeds size"} },
        },
        .{
            .path = "tests/propan/regressions/check-list-mismatch.propan",
            .modes = build_modes,
            .reference = reference,
            .result = .{ .failure = &.{"checklist memory mismatch"} },
        },
        .{
            .path = "tests/propan/regressions/check-list-missing-error.propan",
            .modes = all_modes,
            .result = .{ .failure = &.{"checklist diagnostic err_assertion_failed: expected 1, got 0"} },
        },
        .{
            .path = "tests/propan/regressions/check-list-unexpected-error.propan",
            .modes = build_modes,
            .result = .{ .failure = &.{
                "checklist diagnostic err_assertion_failed: expected 1, got 0",
                "checklist diagnostic err_unknown_mnemonic: expected 0, got 1",
            } },
        },
        .{
            .path = "tests/propan/regressions/check-list-parser-unexpected-error.propan",
            .modes = &.{.parser},
            .result = .{ .failure = &.{"checklist diagnostic err_empty_character_literal_not_allowed: expected 0, got 1"} },
        },
        .{
            .path = "tests/propan/regressions/check-list-parser-unexpected-warning.propan",
            .modes = &.{.parser},
            .result = .{ .failure = &.{"checklist diagnostic warn_invalid_escape_sequence: expected 0, got 1"} },
        },
        .{
            .path = "tests/propan/regressions/check-list-missing-warning.propan",
            .modes = all_modes,
            .reference = warning_reference,
            .result = .{ .failure = &.{"checklist diagnostic warn_symbol_has_no_references: expected 1, got 0"} },
        },
        .{ .path = "tests/propan/regressions/check-list-warning.propan", .modes = &.{.compare}, .reference = warning_reference, .result = .silent },
        .{
            .path = "tests/propan/regressions/check-list-unexpected-warning.propan",
            .modes = build_modes,
            .reference = warning_reference,
            .result = .{ .failure = &.{"checklist diagnostic warn_symbol_has_no_references: expected 0, got 1"} },
        },
        .{
            .path = "tests/propan/parser/diagnostics/malformed-checklist.propan",
            .modes = &.{.parser},
            .result = .{ .failure = &.{
                "checklist sym requires",
                "checklist mem requires",
                "invalid checklist memory address",
                "invalid checklist memory comparison",
                "invalid checklist memory format",
                "invalid checklist hex byte",
                "text after checklist memory block",
                "unexpected '[' in checklist memory block",
            } },
        },
    };
    for (failure_cases) |case| try checkCase(case);
    try nop_disasm.check();
}

test "missing input is rejected" {
    var no_input: Run = .{};
    defer no_input.deinit();
    no_input.options.format = .none;
    no_input.expectExitCode(1);
    no_input.expectStdErrMatch("missing input files");
    try no_input.check();
}
test "warning suppression" {
    const source = "tests/propan/sema/pack-values.propan";
    var normal: Run = .{};
    defer normal.deinit();
    normal.options.format = .none;
    normal.inputs = &.{source};
    normal.expectStdErrMatch("warning:");
    try normal.check();

    var quiet: Run = .{};
    defer quiet.deinit();
    quiet.options.format = .none;
    quiet.options.@"no-warnings" = true;
    quiet.inputs = &.{source};
    quiet.expectStdErrEqual("");
    try quiet.check();
}

test "Preserve trailing comments after strings and character literals in Spin2." {
    var run: Run = .{};
    defer run.deinit();
    run.options.format = .spin2;
    run.inputs = &.{"tests/propan/sema/spin2-quoted-comments.propan"};
    run.expectStdOutMatch("' keep quote character comment\n");
    run.expectStdOutMatch("' keep escaped quote comment\n");
    run.expectStdOutMatch("' keep apostrophe comment\n");
    run.expectStdOutMatch("' keep backslash comment\n");
    run.expectStdOutMatch("' keep string comment\n");
    run.expectStdErrEqual("");
    try run.check();
}

test "Render the stdlib documentation for testing" {
    var run: Run = .{};
    defer run.deinit();
    run.options.@"render-stdlib-docs" = "-";
    run.expectStdOutMatch("<!doctype html>");
    run.expectStdOutMatch(">P_DAC_DITHER_PWM<");
    run.expectStdOutMatch(">popcnt<");
    try run.check();
}

test "formatter golden files and idempotence" {
    for (cases.formatter_tests) |fixture| {
        var run: Run = .{};
        defer run.deinit();
        run.options.@"pretty-print" = fixture.input;
        run.expectStdOutEqual(fixture.expected);
        run.expectStdErrEqual("");
        try run.check();

        var again: Run = .{};
        defer again.deinit();
        again.options.@"pretty-print" = "-";

        again.source = fixture.expected;
        again.expectStdOutEqual(fixture.expected);
        again.expectStdErrEqual("");
        try again.check();
    }
}
test "pretty printer rejects malformed input" {
    var run: Run = .{};
    defer run.deinit();
    run.options.@"pretty-print" = "tests/propan/parser/diagnostics/missing-parenthesis.propan";
    run.expectExitCode(1);
    run.expectStdOutEqual("");
    run.expectStdErrMatch("error:");
    try run.check();
}

const Result = struct {
    status: u8,
    stdout: std.Io.Writer.Allocating,
    stderr: std.Io.Writer.Allocating,

    fn deinit(self: *Result) void {
        self.stdout.deinit();
        self.stderr.deinit();
    }
};

/// Calls the same application function as main, with no argument parsing or subprocess.
fn execute(options: propan.Options, inputs: []const []const u8, include_paths: []const []const u8, source: []const u8, source_path: ?[]const u8, comparison: ?[]const u8) !Result {
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();
    var stdin: std.Io.Reader = .fixed(source);
    var result: Result = .{
        .status = undefined,
        .stdout = .init(std.testing.allocator),
        .stderr = .init(std.testing.allocator),
    };
    errdefer result.deinit();
    errdefer std.debug.print("\nPropan input: {s}\n", .{source_path orelse if (inputs.len > 0) inputs[0] else options.@"pretty-print"});
    result.status = try propan.run(.{
        .io = std.testing.io,
        .gpa = std.testing.allocator,
        .arena = &arena,
        .stdin = &stdin,
        .stdout = &result.stdout.writer,
        .stderr = &result.stderr.writer,
        .include_paths = include_paths,
        .source_path = source_path,
        .comparison = comparison,
    }, options, inputs);
    return result;
}

const Run = struct {
    options: propan.Options = .{},
    inputs: []const []const u8 = &.{},
    path: ?[]const u8 = null,
    include_paths: []const []const u8 = &.{},
    source: []const u8 = "",
    comparison: ?[]const u8 = null,
    expected_status: u8 = 0,
    expected_stdout: ?[]const u8 = null,
    expected_stderr: ?[]const u8 = null,
    stdout_matches: std.ArrayList([]const u8) = .empty,
    stderr_matches: std.ArrayList([]const u8) = .empty,

    fn init(mode: propan.TestMode, path: []const u8, comparison: ?[]const u8) Run {
        return .{
            .options = .{ .format = .none, .@"test-mode" = mode, .@"compare-to" = "unused-reference.bin" },
            .path = path,
            .comparison = comparison,
        };
    }

    fn deinit(self: *Run) void {
        self.stdout_matches.deinit(std.testing.allocator);
        self.stderr_matches.deinit(std.testing.allocator);
    }

    fn expectExitCode(self: *Run, status: u8) void {
        self.expected_status = status;
    }
    fn expectStdOutEqual(self: *Run, bytes: []const u8) void {
        self.expected_stdout = bytes;
    }
    fn expectStdErrEqual(self: *Run, bytes: []const u8) void {
        self.expected_stderr = bytes;
    }
    fn expectStdOutMatch(self: *Run, bytes: []const u8) void {
        self.stdout_matches.append(std.testing.allocator, bytes) catch @panic("oom");
    }
    fn expectStdErrMatch(self: *Run, bytes: []const u8) void {
        self.stderr_matches.append(std.testing.allocator, bytes) catch @panic("oom");
    }

    fn check(self: Run) !void {
        const single_input = [_][]const u8{self.path orelse ""};
        const inputs = if (self.path != null) &single_input else self.inputs;
        var result = try execute(self.options, inputs, self.include_paths, self.source, null, self.comparison);
        defer result.deinit();
        errdefer std.debug.print("\ninputs: {s}\nstdout:\n{s}\nstderr:\n{s}\n", .{ if (inputs.len > 0) inputs[0] else "<none>", result.stdout.written(), result.stderr.written() });
        try std.testing.expectEqual(self.expected_status, result.status);
        if (self.expected_stdout) |expected| try std.testing.expectEqualSlices(u8, expected, result.stdout.written());
        if (self.expected_stderr) |expected| try std.testing.expectEqualStrings(expected, result.stderr.written());
        for (self.stdout_matches.items) |expected| try expectContains(result.stdout.written(), expected);
        for (self.stderr_matches.items) |expected| try expectContains(result.stderr.written(), expected);
    }
};

fn expectContains(actual: []const u8, expected: []const u8) !void {
    if (std.mem.indexOf(u8, actual, expected) == null) {
        std.debug.print("missing output: {s}\n", .{expected});
        return error.TestExpectedEqual;
    }
}

const PropanTestCase = struct {
    path: []const u8,
    modes: []const propan.TestMode,
    reference: ?[]const u8 = null,
    result: union(enum) { silent, failure: []const []const u8 },
};

fn checkCase(case: PropanTestCase) !void {
    for (case.modes) |mode| {
        var run = Run.init(mode, case.path, case.reference);
        defer run.deinit();
        switch (case.result) {
            .silent => run.expectStdErrEqual(""),
            .failure => |messages| {
                run.expectExitCode(1);
                for (messages) |message| run.expectStdErrMatch(message);
            },
        }
        try run.check();
    }
}

fn checkFixtures(mode: propan.TestMode, paths: []const []const u8) !void {
    for (paths) |path| {
        var run = Run.init(mode, path, null);
        defer run.deinit();
        run.expectStdErrEqual("");
        try run.check();
    }
}

test "parser acceptance fixtures" {
    try checkFixtures(.parser, cases.parser_accept_tests);
}
test "semantic acceptance fixtures" {
    try checkFixtures(.sema, cases.sema_accept_tests);
}
test "parser diagnostic fixtures" {
    try checkFixtures(.parser, cases.parser_diagnostic_tests);
}
test "semantic diagnostic fixtures" {
    try checkFixtures(.sema, cases.sema_diagnostic_tests);
}
test "comparison diagnostic fixtures" {
    try checkFixtures(.compare, cases.compare_diagnostic_tests);
}

fn fuzz_arbitrary_assembly(_: void, smith: *std.testing.Smith) !void {
    var buffer: [8192]u8 = undefined;
    const source = buffer[0..smith.slice(&buffer)];
    errdefer std.debug.print("\nassembly fuzz source:\n{s}\n", .{source});
    var result = try execute(.{ .format = .flat, .output = "-", .@"no-warnings" = true }, &.{"-"}, &.{}, source, null, null);
    defer result.deinit();
    try std.testing.expect(result.status <= 1);
    if (result.status == 1) {
        // Invalid programs must report a diagnostic without emitting a partial binary.
        try std.testing.expect(result.stderr.written().len > 0);
        try std.testing.expectEqual(@as(usize, 0), result.stdout.written().len);
    }
}

test "fuzz arbitrary source through full assembly" {
    const sources = @import("fuzz-corpus").files ++ &[_][]const u8{
        "",
        "MOV (\n",
        "UNKNOWN_MNEMONIC\n",
        "const A = 1 / 0\nLONG A\n",
        ".assert 0, \"expected failure\"\n",
        "BYTE \"\\x\"\n",
    };
    var corpus: std.ArrayList([]const u8) = .empty;
    defer {
        for (corpus.items) |input| std.testing.allocator.free(input);
        corpus.deinit(std.testing.allocator);
    }
    for (sources) |source| {
        const len = @min(source.len, 8192);
        const input = try std.testing.allocator.alloc(u8, 4 + len);
        errdefer std.testing.allocator.free(input);
        // Smith.slice replays a little-endian u32 length followed by the source bytes.
        std.mem.writeInt(u32, input[0..4], @intCast(len), .little);
        @memcpy(input[4..], source[0..len]);
        try corpus.append(std.testing.allocator, input);
    }
    try std.testing.fuzz({}, fuzz_arbitrary_assembly, .{ .corpus = corpus.items });
}

const GeneratedOperation = enum { immediate, register, augment, pointer, branch, expression, local_label, data, alignment };

fn generate_program(smith: *std.testing.Smith, writer: *std.Io.Writer) !void {
    const mode = smith.value(enum { cog, lut, hub });
    const count = smith.valueRangeAtMost(u8, 1, 16);
    const fill = smith.value(u8);
    const base = smith.value(u16);
    try writer.print(".{t}exec\nconst BASE = {d}\n", .{ mode, base });
    const mnemonics = [_][]const u8{ "MOV", "ADD", "SUB", "AND", "OR", "XOR" };
    const conditions = [_][]const u8{ "", "if(C) ", "if(!Z) ", "if(C == Z) " };
    const effects = [_][]const u8{ "", " :wc", " :wz", " :wcz" };
    for (0..count) |index| {
        const operation = smith.value(GeneratedOperation);
        const dst = smith.value(u9);
        const src = smith.value(u9);
        const value = smith.valueWeighted(u32, &.{
            .rangeAtMost(u32, 0, std.math.maxInt(u32), 1),
            .value(u32, 0, 8),
            .value(u32, 1, 8),
            .value(u32, 255, 8),
            .value(u32, 256, 8),
            .value(u32, 511, 8),
            .value(u32, 512, 8),
            .value(u32, std.math.maxInt(u32), 8),
        });
        const condition = conditions[smith.index(conditions.len)];
        const effect = effects[smith.index(effects.len)];
        const mnemonic = mnemonics[smith.index(mnemonics.len)];
        try writer.print("block{d}:\n", .{index});
        switch (operation) {
            .immediate => try writer.print("{s}{s} register({d}), {d}{s}\n", .{ condition, mnemonic, dst, value & 511, effect }),
            .register => try writer.print("{s}{s} register({d}), register({d}){s}\n", .{ condition, mnemonic, dst, src, effect }),
            .augment => try writer.print("{s}{s} register({d}), aug(0x{X}){s}\n", .{ condition, mnemonic, dst, value, effect }),
            .pointer => try writer.print("RDLONG register({d}), {s}[{d}]\n", .{ dst, if (src & 1 == 0) "PTRA" else "PTRB", value & 15 }),
            .branch => try writer.print("JMP after{d}\nNOP\nafter{d}:\nJMP block{d}\n", .{ index, index, index }),
            .expression => {
                const expected = (@as(u32, dst) + src) ^ value;
                try writer.print("const value{d} = ({d} + {d}) ^ 0x{X}\nLONG value{d}\n.assert value{d} == 0x{X}\n", .{ index, dst, src, value, index, index, expected });
            },
            .local_label => try writer.writeAll("JMP .done\n.loop:\nNOP\n.done:\nJMP .loop\n"),
            .data => try writer.print("BYTE {d}, \"A\\x00Z\"\nWORD {d}\nLONG 0x{X}\n.align 4\n", .{ @as(u8, @truncate(value)), @as(u16, @truncate(value)), value }),
            .alignment => try writer.print(".align {d}\nNOP\n", .{@as(u32, 4) << @as(u5, @intCast(value % 3))}),
        }
    }
    // A separate data segment exercises flattening alongside the generated code.
    try writer.print(".data\nBYTE {d}, \"end\"\nWORD BASE\nLONG BASE + 1\n", .{fill});
}

fn fuzz_generated_assembly(_: void, smith: *std.testing.Smith) !void {
    var source: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer source.deinit();
    try generate_program(smith, &source.writer);
    errdefer std.debug.print("\ngenerated assembly:\n{s}\n", .{source.written()});
    var result = try execute(.{ .format = .flat, .output = "-", .@"no-warnings" = true }, &.{"-"}, &.{}, source.written(), null, null);
    defer result.deinit();
    errdefer std.debug.print("\ndiagnostics:\n{s}\n", .{result.stderr.written()});
    try std.testing.expectEqual(@as(u8, 0), result.status);
    try std.testing.expectEqualStrings("", result.stderr.written());
    try std.testing.expect(result.stdout.written().len > 0);
    // At most 16 bounded blocks, with alignment no greater than 16 cog longs.
    try std.testing.expect(result.stdout.written().len <= 4096);
}

test "fuzz generated valid programs through full assembly" {
    const operation_count = std.meta.fields(GeneratedOperation).len;
    // Four initial choices and seven choices per block; Smith integers replay as u64.
    var seeds: [3][(4 + 7 * operation_count) * 8]u8 = undefined;
    for (&seeds, 0..) |*seed, mode| {
        var writer: std.Io.Writer = .fixed(seed);
        for ([_]u64{ mode, operation_count, 0x7E, 42 }) |value| try writer.writeInt(u64, value, .little);
        for (0..operation_count) |operation| {
            const values = [_]u32{ 0, 1, 255, 256, 511, 512, 0xFFFF, 0xFFFFFFFF };
            for ([_]u64{ operation, 511, 255, values[(operation + mode) % values.len], operation % 4, (operation / 2) % 4, operation % 6 }) |value|
                try writer.writeInt(u64, value, .little);
        }
    }
    // Ordinary test runs exercise every operation in each execution mode, plus EOF defaults.
    try std.testing.fuzz({}, fuzz_generated_assembly, .{ .corpus = &.{ "", &seeds[0], &seeds[1], &seeds[2] } });
}

fn assemble(path: []const u8, format: @FieldType(propan.Options, "format"), source: ?[]const u8) !Result {
    var result = try execute(.{ .format = format, .output = "-", .@"no-warnings" = true }, &.{if (source != null) "-" else path}, &.{}, source orelse "", if (source != null) path else null, null);
    errdefer result.deinit();
    errdefer std.debug.print("\nassembly failed: {s}\n{s}\n", .{ path, result.stderr.written() });
    try std.testing.expectEqual(@as(u8, 0), result.status);
    return result;
}

test "formatter preserves assembled bytes for all semantic fixtures" {
    for (cases.sema_accept_tests) |path| {
        var original = try assemble(path, .flat, null);
        defer original.deinit();
        var formatted = try execute(.{ .@"pretty-print" = path }, &.{}, &.{}, "", null, null);
        defer formatted.deinit();
        try std.testing.expectEqual(@as(u8, 0), formatted.status);
        var rebuilt = try assemble(path, .flat, formatted.stdout.written());
        defer rebuilt.deinit();
        errdefer std.debug.print("\nformatter round trip: {s}\n", .{path});
        try std.testing.expectEqualSlices(u8, original.stdout.written(), rebuilt.stdout.written());
    }
}

/// FlexSpin remains an optional external oracle; Propan always runs in this process.
fn flexspinAssemble(path: []const u8, generated: ?[]const u8) ![]const u8 {
    const flexspin: ?[]const u8 = test_options.flexspin;
    const exe = flexspin orelse return error.SkipZigTest;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const directory = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(directory);
    const output = try std.fs.path.join(std.testing.allocator, &.{ directory, "rebuilt.bin" });
    defer std.testing.allocator.free(output);
    const input = if (generated) |source| blk: {
        try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "generated.spin2", .data = source });
        break :blk try std.fs.path.join(std.testing.allocator, &.{ directory, "generated.spin2" });
    } else try std.testing.allocator.dupe(u8, path);
    defer std.testing.allocator.free(input);
    const child = try std.process.run(std.testing.allocator, std.testing.io, .{ .argv = &.{ exe, "-2", "-q", "-o", output, input } });
    defer std.testing.allocator.free(child.stdout);
    defer std.testing.allocator.free(child.stderr);
    errdefer std.debug.print("\nFlexSpin input: {s}\n{s}\n{s}\n", .{ path, child.stdout, child.stderr });
    try std.testing.expectEqual(std.process.Child.Term{ .exited = 0 }, child.term);
    return tmp.dir.readFileAlloc(std.testing.io, "rebuilt.bin", std.testing.allocator, .limited(1 << 20));
}

test "Spin2 export preserves assembled bytes for all semantic fixtures" {
    const flexspin: ?[]const u8 = test_options.flexspin;
    if (flexspin == null) return error.SkipZigTest;
    for (cases.sema_accept_tests) |path| {
        var original = try assemble(path, .flat, null);
        defer original.deinit();
        var exported = try assemble(path, .spin2, null);
        defer exported.deinit();
        const rebuilt = try flexspinAssemble(path, exported.stdout.written());
        defer std.testing.allocator.free(rebuilt);
        errdefer std.debug.print("\nSpin2 round trip: {s}\n", .{path});
        try std.testing.expectEqualSlices(u8, original.stdout.written(), rebuilt);
    }
}

test "assembler equivalence with independent Spin2 fixtures" {
    const flexspin: ?[]const u8 = test_options.flexspin;
    if (flexspin == null) return error.SkipZigTest;
    for (cases.emit_compare_tests) |path| {
        const spin2 = try std.fmt.allocPrint(std.testing.allocator, "{s}.spin2", .{path[0 .. path.len - std.fs.path.extension(path).len]});
        defer std.testing.allocator.free(spin2);
        const reference = try flexspinAssemble(spin2, null);
        defer std.testing.allocator.free(reference);
        var run = Run.init(.compare, path, reference);
        defer run.deinit();
        try run.check();
    }
}

test "Spin2 export readability" {
    var readable: Run = .{ .options = .{ .format = .spin2, .output = "-" }, .inputs = &.{"tests/propan/sema/spin2-readable.propan"} };
    defer readable.deinit();
    readable.expectStdOutMatch("MOV dst, #VALUE");
    readable.expectStdOutMatch("MOV dst, #$3");
    readable.expectStdOutMatch("MOV 10, 32");
    readable.expectStdOutMatch("ADD 12, #$A");
    try readable.check();
    var sumloop: Run = .{ .options = .{ .format = .spin2, .output = "-" }, .inputs = &.{"examples/sumloop.propan"} };
    defer sumloop.deinit();
    sumloop.expectStdOutMatch("' swiftly sums buf_a and buf_b into buf_c");
    sumloop.expectStdOutMatch(".loop\n  REP @.end, #8");
    sumloop.expectStdOutMatch("  ADD 0-0, #0");
    sumloop.expectStdOutMatch(".end\n  RET wcz");
    sumloop.expectStdOutMatch("BYTE 0[8] ' .align  8");
    try sumloop.check();
}
