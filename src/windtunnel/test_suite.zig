const std = @import("std");
const args = @import("args");
const propan = @import("propan");
const checklist = @import("check_list.zig");
const Cog = @import("sim/Cog.zig");
const Hub = @import("sim/Hub.zig");
const smart_pin = @import("sim/smart_pin.zig");
const disassembly = @import("sim/disassemble.zig");
comptime {
    _ = disassembly;
}
const decode = @import("sim/decode.zig");
const encoding = @import("sim/encoding.zig");
const oracle = @import("oracle.zig");
const p2aas = @import("p2aas.zig");
const p2aas_server = @import("p2aas_server.zig");
const serial_boot = @import("serial_boot.zig");

pub const std_options: std.Options = .{ .log_level = .err };

const Options = struct {
    help: bool = false,
    oracle: bool = false,
    characterize: bool = false,
    jobs: usize = 0,
    include: ?[]const u8 = null,
    exclude: ?[]const u8 = null,
    @"timing-report": ?[]const u8 = null,
    @"trace-pipeline": bool = false,
    @"fast-io": bool = false,
    @"prepare-oracle": bool = false,
    @"artifact-dir": []const u8 = ".zig-cache/windtunnel-artifacts",
    pub const shorthands = .{ .h = "help", .j = "jobs" };
    pub const meta = .{ .usage_summary = "[FILE.propan ...]", .option_docs = .{
        .help = "Show usage",
        .oracle = "Run the hardware oracle through P2AAS_ENDPOINT",
        .characterize = "Capture hardware measurements without executing or checking the simulator",
        .jobs = "Number of parallel simulator workers (0 uses all cores); hardware runs use one worker",
        .include = "Run files matching any comma-separated path substring (case-sensitive)",
        .exclude = "Skip files matching any comma-separated path substring; exclusions override includes",
        .@"timing-report" = "Write file totals, min/average/max run times and individual run times to a Markdown file",
        .@"trace-pipeline" = "Retain up to 10000 pipeline events with prepared images and failure evidence",
        .@"fast-io" = "Transfer UART/USART endpoint words at single-clock throughput unless a fixture requires timing",
        .@"prepare-oracle" = "Generate oracle images and requests without uploading them",
        .@"artifact-dir" = "Directory for prepared images and failure evidence",
    } };
};

const Context = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    errors: *std.Io.Writer,
    options: Options = .{},
    endpoint: []const u8 = "",
    retain_failures: bool = true,
};
const Kind = oracle.Kind;
const Observation = oracle.Observation;
const Resolved = oracle.Resolved;

/// Match one of the validated, comma-separated substrings against a complete fixture path.
fn matchesFileFilter(path: []const u8, filter: []const u8) bool {
    var patterns = std.mem.splitScalar(u8, filter, ',');
    while (patterns.next()) |raw| {
        const pattern = std.mem.trim(u8, raw, " \t\r\n");
        if (std.mem.find(u8, path, pattern) != null)
            return true;
    }
    return false;
}

/// Compact the file list before parsing or assembling fixtures; reject empty filter patterns.
fn filterFiles(paths: *std.ArrayList([]const u8), options: Options) !void {
    for ([_]?[]const u8{ options.include, options.exclude }) |filter| {
        if (filter) |value| {
            var patterns = std.mem.splitScalar(u8, value, ',');
            while (patterns.next()) |pattern| {
                if (std.mem.trim(u8, pattern, " \t\r\n").len == 0)
                    return error.EmptyFileFilter;
            }
        }
    }
    var selected: usize = 0;
    for (paths.items) |path| {
        if (options.include) |filter| {
            if (!matchesFileFilter(path, filter))
                continue;
        }
        if (options.exclude) |filter| {
            if (matchesFileFilter(path, filter))
                continue;
        }
        paths.items[selected] = path;
        selected += 1;
    }
    paths.shrinkRetainingCapacity(selected);
}

test "file filters combine includes, prioritize excludes and reject empty patterns" {
    const input = [_][]const u8{
        "tests/windtunnel/state/fifo-write-visibility.propan",
        "tests/windtunnel/state/fifo-startup.propan",
        "tests/windtunnel/state/cogid.propan",
        "tests/windtunnel/program/hello-world.propan",
    };
    var paths: std.ArrayList([]const u8) = .empty;
    defer paths.deinit(std.testing.allocator);

    try paths.appendSlice(std.testing.allocator, &input);
    try filterFiles(&paths, .{});
    try std.testing.expectEqual(input.len, paths.items.len);
    try filterFiles(&paths, .{ .include = "state/, hello-world", .exclude = "fifo-write-visibility.propan,cogid.propan" });
    try std.testing.expectEqual(@as(usize, 2), paths.items.len);
    try std.testing.expectEqualStrings(input[1], paths.items[0]);
    try std.testing.expectEqualStrings(input[3], paths.items[1]);
    try filterFiles(&paths, .{ .include = "DOES-NOT-MATCH" });
    try std.testing.expectEqual(@as(usize, 0), paths.items.len);
    for ([_][]const u8{ "", " ", ",fifo", "fifo,", "fifo,,cogid" }) |filter| {
        try std.testing.expectError(error.EmptyFileFilter, filterFiles(&paths, .{ .include = filter }));
        try std.testing.expectError(error.EmptyFileFilter, filterFiles(&paths, .{ .exclude = filter }));
    }
    try paths.appendSlice(std.testing.allocator, &input);
    try filterFiles(&paths, .{ .exclude = ".propan" });
    try std.testing.expectEqual(@as(usize, 0), paths.items.len);
}

/// Adapt attached -jN values to the argument parser's separate-value shorthand syntax.
const Arguments = struct {
    items: []const [:0]const u8,
    index: usize = 0,
    pending: ?[:0]const u8 = null,
    positional: bool = false,

    /// Yield arguments without rewriting positional filenames after --.
    pub fn next(iterator: *Arguments) ?[:0]const u8 {
        if (iterator.pending) |value| {
            iterator.pending = null;
            return value;
        }
        if (iterator.index == iterator.items.len)
            return null;
        const value = iterator.items[iterator.index];
        iterator.index += 1;
        if (std.mem.eql(u8, value, "--")) {
            iterator.positional = true;
        }
        if (!iterator.positional and value.len > 2 and std.mem.startsWith(u8, value, "-j")) {
            iterator.pending = value[2..];
            return "-j";
        }
        return value;
    }
};

/// One independently assembled and simulated case, with diagnostics owned by its worker.
const Job = struct {
    path: []const u8,
    source: []const u8,
    list: checklist.List,
    name: []const u8,
    fixture: usize,
    progress: std.Progress.Node,
    diagnostics: std.Io.Writer.Allocating,
    status: ?oracle.Status = null,
    failure: ?anyerror = null,
    elapsed_ns: u64 = 0,
};

/// Measure elapsed wall time, including assembly, simulation, assertions and optional hardware access.
fn elapsedSince(io: std.Io, started: std.Io.Timestamp) u64 {
    return @intCast(std.Io.Timestamp.now(io, .awake).toNanoseconds() - started.toNanoseconds());
}

/// Keep literal run names and paths inside one Markdown table cell.
fn timingCell(writer: *std.Io.Writer, value: []const u8) !void {
    for (value) |byte| {
        switch (byte) {
            '\\', '|', '`' => {
                try writer.writeByte('\\');
                try writer.writeByte(byte);
            },
            '\r', '\n' => try writer.writeByte(' '),
            else => try writer.writeByte(byte),
        }
    }
}

/// Summarize elapsed run durations for one file; preparation contributes only to its total.
const FileTiming = struct {
    path: []const u8,
    count: usize = 0,
    total_ns: u64,
    run_ns: u64 = 0,
    min_ns: u64 = std.math.maxInt(u64),
    max_ns: u64 = 0,

    /// Rank the most expensive files first, breaking ties by path.
    fn longestFirst(_: void, a: FileTiming, b: FileTiming) bool {
        if (a.total_ns != b.total_ns)
            return a.total_ns > b.total_ns;
        return std.mem.lessThan(u8, a.path, b.path);
    }
};

/// Write a complete, longest-first overview after every worker has finished, including failed cases.
fn writeTimingReport(
    allocator: std.mem.Allocator,
    writer: *std.Io.Writer,
    paths: []const []const u8,
    preparation_ns: []const u64,
    jobs: []const Job,
    workers: usize,
    suite_ns: u64,
) !void {
    const files = try allocator.alloc(FileTiming, paths.len);
    defer allocator.free(files);

    for (files, paths, preparation_ns) |*file, path, preparation| {
        file.* = .{ .path = path, .total_ns = preparation };
    }
    const runs = try allocator.alloc(*const Job, jobs.len);
    defer allocator.free(runs);

    for (jobs, runs) |*job, *run| {
        run.* = job;
        const file = &files[job.fixture];
        file.count += 1;
        file.total_ns += job.elapsed_ns;
        file.run_ns += job.elapsed_ns;
        file.min_ns = @min(file.min_ns, job.elapsed_ns);
        file.max_ns = @max(file.max_ns, job.elapsed_ns);
    }
    std.mem.sort(FileTiming, files, {}, FileTiming.longestFirst);
    std.mem.sort(*const Job, runs, {}, struct {
        /// Rank runs by elapsed duration, then by file path and case name.
        fn longestFirst(_: void, a: *const Job, b: *const Job) bool {
            if (a.elapsed_ns != b.elapsed_ns)
                return a.elapsed_ns > b.elapsed_ns;
            if (!std.mem.eql(u8, a.path, b.path))
                return std.mem.lessThan(u8, a.path, b.path);
            return std.mem.lessThan(u8, a.name, b.name);
        }
    }.longestFirst);
    const seconds: f64 = std.time.ns_per_s;
    try writer.print(
        "# Windtunnel timing report\n\nWorkers: {d}. Suite elapsed: {d:.6} s. Files: {d}. Runs: {d}.\n\n" ++
            "All times are elapsed wall time in seconds. File totals include checklist preparation and " ++
            "the sum of their run times. Each run includes assembly, simulation, assertions and any hardware access; " ++
            "queue waiting and report generation are excluded. Parallel run times overlap and include CPU contention; " ++
            "use `-j1` for comparisons.\n\n" ++
            "## Files (longest first)\n\n" ++
            "| File | Runs | Total (s) | Min/run (s) | Avg/run (s) | Max/run (s) |\n" ++
            "| --- | ---: | ---: | ---: | ---: | ---: |\n",
        .{ workers, @as(f64, @floatFromInt(suite_ns)) / seconds, paths.len, jobs.len },
    );
    for (files) |file| {
        try writer.writeAll("| ");
        try timingCell(writer, file.path);
        try writer.print(" | {d} | {d:.6} |", .{ file.count, @as(f64, @floatFromInt(file.total_ns)) / seconds });
        if (file.count == 0) {
            try writer.writeAll(" — | — | — |\n");
        } else {
            const average = @as(f64, @floatFromInt(file.run_ns)) / @as(f64, @floatFromInt(file.count));
            try writer.print(" {d:.6} | {d:.6} | {d:.6} |\n", .{
                @as(f64, @floatFromInt(file.min_ns)) / seconds,
                average / seconds,
                @as(f64, @floatFromInt(file.max_ns)) / seconds,
            });
        }
    }
    try writer.writeAll(
        "\n## Individual runs (longest first)\n\n" ++
            "| File | Run | Time (s) | Result |\n| --- | --- | ---: | --- |\n",
    );
    for (runs) |job| {
        try writer.writeAll("| ");
        try timingCell(writer, job.path);
        try writer.writeAll(" | ");
        try timingCell(writer, job.name);
        try writer.print(" | {d:.6} | ", .{@as(f64, @floatFromInt(job.elapsed_ns)) / seconds});
        if (job.failure) |err| {
            try writer.print("FAIL ({t})", .{err});
        } else {
            try writer.print("{s} (oracle {t})", .{
                if (job.status == .captured) "CAPTURE" else "PASS",
                job.status.?,
            });
        }
        try writer.writeAll(" |\n");
    }
}

/// A bounded group of workers dynamically claims cases to balance uneven fixture matrices.
const WorkGroup = struct {
    context: Context,
    jobs: []Job,
    next: std.atomic.Value(usize) = .init(0),

    /// Run each claimed case in isolation; only the owner writes its result and diagnostics.
    fn worker(work: *WorkGroup) void {
        while (true) {
            const index = work.next.fetchAdd(1, .monotonic);
            if (index >= work.jobs.len)
                return;
            const job = &work.jobs[index];
            const fixture_progress = job.progress.start(job.path, 1);
            defer fixture_progress.end();

            const progress = fixture_progress.start(job.name, 0);
            defer progress.end();

            var context = work.context;
            context.errors = &job.diagnostics.writer;
            const started = if (context.options.@"timing-report" != null)
                std.Io.Timestamp.now(context.io, .awake)
            else
                null;
            defer {
                if (started) |timestamp| {
                    job.elapsed_ns = elapsedSince(context.io, timestamp);
                }
            }

            job.status = runOne(context, job.path, job.source, job.list, job.name) catch |err| {
                job.failure = err;
                continue;
            };
        }
    }

    /// Use the caller as one worker and await all additional standard-library pool tasks.
    fn run(work: *WorkGroup, workers: usize) !void {
        std.debug.assert(workers > 0);
        var group: std.Io.Group = .init;
        defer group.cancel(work.context.io);

        const count = @max(1, @min(workers, work.jobs.len));
        work.context.options.jobs = count;
        for (1..count) |_| {
            try group.concurrent(work.context.io, worker, .{work});
        }
        worker(work);
        try group.await(work.context.io);
    }
};

/// Resolve automatic parallelism while keeping a shared hardware board exclusively owned.
fn workerCount(options: Options, cpu_count: usize) usize {
    if ((options.oracle or options.characterize) and !options.@"prepare-oracle")
        return 1;
    return @max(1, if (options.jobs == 0) cpu_count else options.jobs);
}

test "worker options accept separate and attached counts and serialize hardware access" {
    for ([_][]const [:0]const u8{
        &.{ "-j", "4", "fixture.propan" },
        &.{ "-j4", "fixture.propan" },
        &.{ "--jobs", "4", "fixture.propan" },
    }) |items| {
        var iterator: Arguments = .{ .items = items };
        var parsed = try args.parse(Options, &iterator, std.testing.allocator, .print);
        defer parsed.deinit();

        try std.testing.expectEqual(@as(usize, 4), parsed.options.jobs);
        try std.testing.expectEqualStrings("fixture.propan", parsed.positionals[0]);
    }
    var iterator: Arguments = .{ .items = &.{
        "-j0",                          "--timing-report", "timings.md",
        "--include",                    "state/,program/", "--exclude",
        "fifo-write-visibility.propan", "--",              "-j1",
    } };
    var parsed = try args.parse(Options, &iterator, std.testing.allocator, .print);
    defer parsed.deinit();

    try std.testing.expectEqual(@as(usize, 0), parsed.options.jobs);
    try std.testing.expectEqualStrings("timings.md", parsed.options.@"timing-report".?);
    try std.testing.expectEqualStrings("state/,program/", parsed.options.include.?);
    try std.testing.expectEqualStrings("fifo-write-visibility.propan", parsed.options.exclude.?);
    try std.testing.expectEqualStrings("-j1", parsed.positionals[0]);
    try std.testing.expectEqual(@as(usize, 8), workerCount(.{}, 8));
    try std.testing.expectEqual(@as(usize, 1), workerCount(.{ .jobs = 1 }, 8));
    try std.testing.expectEqual(@as(usize, 4), workerCount(.{ .jobs = 4 }, 8));
    try std.testing.expectEqual(@as(usize, 1), workerCount(.{ .oracle = true }, 8));
    try std.testing.expectEqual(@as(usize, 1), workerCount(.{ .characterize = true, .jobs = 4 }, 8));
    try std.testing.expectEqual(@as(usize, 8), workerCount(.{ .oracle = true, .@"prepare-oracle" = true }, 8));
}

test "workers isolate cases, retain failures and finish every queued job" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const source =
        "//? WINDTUNNEL CHECK LIST\n" ++
        "//? post: cog[0].reg[result] == 7\n" ++
        "//? run: \"first\"\n//? pre: sym[value] = u32 [7]\n" ++
        "//? run: \"failure\"\n//? pre: sym[value] = u32 [8]\n" ++
        "//? run: \"last\"\n//? pre: sym[value] = u32 [7]\n" ++
        "\nMOV result, value\nJMP nrel(_end)\nvar result: LONG 0\nvar value: LONG 0\n";
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "fixture.propan", .data = source });
    var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    const path = try tmp.dir.realPathFileAlloc(std.testing.io, "fixture.propan", allocator);
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const list = try checklist.parse(allocator, path, source, &errors.writer);
    for ([_]usize{ 1, 4 }) |workers| {
        var jobs: [3]Job = undefined;
        for (&jobs, list.runs) |*job, run| {
            job.* = .{
                .path = path,
                .source = source,
                .list = try list.forRun(allocator, run),
                .name = run.name.?,
                .fixture = 0,
                .progress = .none,
                .diagnostics = .init(std.testing.allocator),
            };
        }
        defer {
            for (&jobs) |*job| {
                job.diagnostics.deinit();
            }
        }

        var work: WorkGroup = .{
            .context = .{
                .allocator = std.testing.allocator,
                .io = std.testing.io,
                .errors = &errors.writer,
                .options = .{ .@"timing-report" = if (workers > 1) "unused.md" else null },
                .retain_failures = false,
            },
            .jobs = &jobs,
        };
        try work.run(workers);
        try std.testing.expectEqual(oracle.Status.skipped, jobs[0].status.?);
        try std.testing.expectEqual(error.AssertionFailed, jobs[1].failure.?);
        try std.testing.expect(jobs[1].diagnostics.written().len != 0);
        try std.testing.expectEqual(oracle.Status.skipped, jobs[2].status.?);
        try std.testing.expect(jobs[0].failure == null and jobs[2].failure == null);
        for (jobs) |job| {
            try std.testing.expectEqual(workers > 1, job.elapsed_ns > 0);
        }
    }
    var empty: WorkGroup = .{
        .context = .{ .allocator = std.testing.allocator, .io = std.testing.io, .errors = &errors.writer },
        .jobs = &.{},
    };
    try empty.run(4);
}

test "timing reports aggregate and rank files and runs, including failures and empty files" {
    var jobs = [_]Job{
        .{
            .path = "fast.propan",
            .source = "",
            .list = .{},
            .name = "fast",
            .fixture = 0,
            .progress = .none,
            .diagnostics = .init(std.testing.allocator),
            .status = .skipped,
            .elapsed_ns = 2 * std.time.ns_per_s,
        },
        .{
            .path = "slow.propan",
            .source = "",
            .list = .{},
            .name = "failed | case",
            .fixture = 1,
            .progress = .none,
            .diagnostics = .init(std.testing.allocator),
            .failure = error.AssertionFailed,
            .elapsed_ns = std.time.ns_per_s,
        },
        .{
            .path = "slow.propan",
            .source = "",
            .list = .{},
            .name = "slow",
            .fixture = 1,
            .progress = .none,
            .diagnostics = .init(std.testing.allocator),
            .status = .skipped,
            .elapsed_ns = 3 * std.time.ns_per_s,
        },
    };
    defer {
        for (&jobs) |*job| {
            job.diagnostics.deinit();
        }
    }

    var report: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer report.deinit();

    try writeTimingReport(
        std.testing.allocator,
        &report.writer,
        &.{ "fast.propan", "slow.propan", "empty.propan" },
        &.{ std.time.ns_per_s, std.time.ns_per_s / 2, std.time.ns_per_s / 5 },
        &jobs,
        2,
        7 * std.time.ns_per_s,
    );
    const text = report.written();
    try std.testing.expect(std.mem.find(u8, text, "Workers: 2. Suite elapsed: 7.000000 s. Files: 3. Runs: 3.") != null);
    try std.testing.expect(std.mem.find(
        u8,
        text,
        "| slow.propan | 2 | 4.500000 | 1.000000 | 2.000000 | 3.000000 |\n" ++
            "| fast.propan | 1 | 3.000000 | 2.000000 | 2.000000 | 2.000000 |\n" ++
            "| empty.propan | 0 | 0.200000 | — | — | — |\n",
    ) != null);
    try std.testing.expect(std.mem.find(
        u8,
        text,
        "| slow.propan | slow | 3.000000 | PASS (oracle skipped) |\n" ++
            "| fast.propan | fast | 2.000000 | PASS (oracle skipped) |\n" ++
            "| slow.propan | failed \\| case | 1.000000 | FAIL (AssertionFailed) |\n",
    ) != null);
}

/// Resolve a numeric or symbolic address in the requested address space.
fn address(module: propan.Module, value: checklist.Address, kind: Kind) !u32 {
    return switch (value) {
        .number => |n| n,
        .symbol => |name| blk: {
            var found: ?u32 = null;
            for (module.symbols) |symbol| {
                if (!std.mem.eql(u8, symbol.name, name))
                    continue;
                if (found != null)
                    return error.AmbiguousSymbol;
                found = if (kind == .hub)
                    symbol.label.hub_address orelse return error.InvalidSymbolKind
                else switch (symbol.label.local) {
                    .cog, .regspace => |n| n,
                    else => return error.InvalidSymbolKind,
                };
            }
            break :blk found orelse return error.UnknownSymbol;
        },
    };
}

/// Resolve an executable symbol to its cog, LUT, or hub PC.
fn codeAddress(module: propan.Module, name: []const u8) !u20 {
    var found: ?u20 = null;
    for (module.symbols) |symbol| {
        if (!std.mem.eql(u8, symbol.name, name))
            continue;
        if (found != null)
            return error.AmbiguousSymbol;
        if (symbol.type != .code)
            return error.InvalidSymbolKind;
        found = switch (symbol.label.local) {
            .cog => |n| n,
            .lut => |n| @as(u20, n) + 0x200,
            .hub => @intCast(symbol.label.hub_address orelse return error.InvalidSymbolKind),
            else => return error.InvalidSymbolKind,
        };
    }
    return found orelse error.UnknownSymbol;
}

/// Convert a checklist assignment into a validated observation.
fn resolve(module: propan.Module, assignment: checklist.Assignment) !Resolved {
    const kind: Kind = switch (assignment.target) {
        .sym => return error.InvalidPrecondition,
        .reg => .reg,
        .hub => .hub,
        .c => .c,
        .z => .z,
        .q => .q,
        .dacs => .dacs,
    };
    const offset = switch (assignment.target) {
        .reg, .hub => |a| try address(module, a, kind),
        .c, .z, .q, .dacs => 0,
        .sym => unreachable,
    };
    const len: u32 = @intCast(assignment.bytes.len);
    if (len == 0)
        return error.EmptyObservation;
    if (kind == .reg and (offset == 0 or offset >= 506))
        return error.ReservedRegister;
    if (kind == .hub and oracle.reservedMemory(module, offset, len))
        return error.ReservedMemory;
    return .{ .observation = .{ .kind = kind, .offset = offset, .len = len, .line = assignment.line }, .bytes = assignment.bytes };
}

/// Check whether an assembled source range contains executable code.
fn isCode(module: propan.Module, offset: u32, len: u32) bool {
    for (module.line_data) |line| {
        if (line.kind == .code and
            offset < @as(u64, line.offset) + line.length and
            line.offset < @as(u64, offset) + len)
            return true;
    }
    return false;
}

/// Reject fixture code that overlaps the oracle scaffold or reserved space.
fn validateLayout(module: propan.Module, image: []const u8, stop: u20) !void {
    for (module.symbols) |symbol| {
        if (std.mem.startsWith(u8, symbol.name, "_wt_") and
            !oracle.isScaffold(if (symbol.source_location) |loc|
                loc.source
            else
                null))
            return error.ReservedSymbol;
    }
    for (module.constants) |constant| {
        if (std.mem.startsWith(u8, constant.name, "_wt_") and
            !oracle.isScaffold(constant.location.source))
            return error.ReservedSymbol;
    }
    const origin = try address(module, .{ .symbol = "_wt_start" }, .hub);
    var exit = false;
    for (module.line_data) |line| {
        if (oracle.isScaffold(line.location.source) or line.length == 0)
            continue;
        const end = @as(u64, line.offset) + line.length;
        if (oracle.reservedMemory(module, line.offset, line.length))
            return error.ReservedMemory;
        if (line.pc) |pc| {
            if (pc < 0x200) {
                // Separate images may be loaded into other cogs by the DUT. They
                // must lie entirely outside its scaffold-loaded register image.
                var other_image = false;
                for (module.segments) |segment| {
                    if (segment.exec_mode == .cog and
                        segment.hub_offset >= origin + 504 * 4 and line.offset >= segment.hub_offset and
                        end <= segment.hub_offset + segment.data.len)
                    {
                        other_image = true;
                    }
                }
                if (!other_image and
                    (pc == 0 or
                        pc >= 504 or
                        end > origin + 504 * 4 or
                        line.offset != origin + pc * 4))
                    return error.InvalidCogLayout;
            } else if (pc < 0x400) {
                // Cog code still has the loader mapping; LUT code is loaded by the DUT.
                var in_lut = false;
                for (module.segments) |segment| {
                    if (line.offset >= segment.hub_offset and
                        end <= segment.hub_offset + segment.data.len and
                        segment.exec_mode == .lut)
                    {
                        in_lut = true;
                    }
                }
                if (!in_lut and
                    (pc >= 504 or
                        end > origin + 504 * 4 or
                        line.offset != origin + pc * 4))
                    return error.InvalidCogLayout;
            }
        }
        if (line.kind == .code and line.length >= 4) {
            const instr: encoding.Instruction = .{ .raw = std.mem.readInt(u32, image[line.offset..][0..4], .little) };
            if (decode.decode(instr.raw) == .jmp_a and
                !instr.abs_pointer.relative and
                instr.abs_pointer.cond == .IF_ALWAYS and
                instr.abs_pointer.address == stop)
            {
                exit = true;
            }
        }
    }
    if (!exit)
        return error.MissingExitJump;
}

/// Ensure the dispatched instruction belongs to assembled executable code.
fn validateDispatch(module: propan.Module, origin: u32, instruction: Cog.PipelineState) !void {
    if (instruction.interrupt != null)
        return; // Internal CALLD entries do not belong to the assembled image.
    const fetch_pc = instruction.local_address orelse instruction.pc;
    if (fetch_pc == 0)
        return; // The one-word trampoline belongs to the scaffold.
    if (instruction.debug_rom)
        return error.ReservedExecution;
    if (fetch_pc < 504 and isCode(module, origin + @as(u32, fetch_pc) * 4, 4))
        return;
    // Loaded cog/LUT images can supply code at an operand-selected physical address.
    for (module.line_data) |line| {
        const pc = line.pc orelse continue;
        if (line.kind != .code or pc > fetch_pc or fetch_pc >= pc + line.length / 4)
            continue;
        for (module.segments) |segment| {
            const matching_mode = if (fetch_pc >= 0x200)
                segment.exec_mode == .lut
            else
                segment.exec_mode == .cog;
            if (matching_mode and
                line.offset >= segment.hub_offset and
                line.offset < segment.hub_offset + segment.data.len)
                return;
        }
    }
    // Special-register operands can lie outside the assembler's executable cog range. Require their fetched
    // value to be an instruction emitted as code elsewhere in the fixture.
    if (instruction.pc >= 0x400 and fetch_pc >= 504 and fetch_pc < 512) {
        for (module.line_data) |line| {
            if (line.kind != .code)
                continue;
            for (module.segments) |segment| {
                if (line.offset < segment.hub_offset or
                    line.offset + line.length > segment.hub_offset + segment.data.len)
                    continue;
                var offset = line.offset - segment.hub_offset;
                const end = offset + line.length;
                while (offset + 4 <= end) : (offset += 4) {
                    if (std.mem.readInt(u32, segment.data[offset..][0..4], .little) == instruction.instr)
                        return;
                }
            }
        }
    }
    return error.ExecutingData;
}

/// Check that every byte of an observation was emitted by a segment.
fn definedMemory(module: propan.Module, observation: Observation) bool {
    for (observation.offset..observation.offset + observation.len) |index| {
        var defined = false;
        for (module.segments) |segment| {
            if (index >= segment.hub_offset and index < segment.hub_offset + segment.data.len) {
                defined = true;
                break;
            }
        }
        if (!defined)
            return false;
    }
    return true;
}

/// Serialize an observed simulator value into the result stream.
fn read(hub: *Hub, cog: u3, observation: Observation, out: *std.Io.Writer) !void {
    switch (observation.kind) {
        .reg => try out.writeInt(u32, hub.cogs[cog].registers.get(@enumFromInt(observation.offset)), .little),
        .q => try out.writeInt(u32, hub.cogs[cog].q, .little),
        .dacs => try out.writeAll(&hub.cogs[cog].dacs),
        .hub => try out.writeAll(hub.memory[observation.offset..][0..observation.len]),
        .c => try out.writeByte(@intFromBool(hub.cogs[cog].c)),
        .z => try out.writeByte(@intFromBool(hub.cogs[cog].z)),
    }
}

/// Parse a fixture and run each named checklist case.
fn runSource(ctx: Context, path: []const u8, source: []const u8) !oracle.Status {
    var arena: std.heap.ArenaAllocator = .init(ctx.allocator);
    defer arena.deinit();

    const list = try checklist.parse(arena.allocator(), path, source, ctx.errors);
    var status: oracle.Status = .skipped;
    for (list.runs, 1..) |run, index| {
        const name = run.name orelse try std.fmt.allocPrint(arena.allocator(), "run {d}", .{index});
        status = try runOne(ctx, path, source, try list.forRun(arena.allocator(), run), name);
    }
    return status;
}

/// Assemble and simulate one fixture case, then check its observations.
fn runOne(
    ctx: Context,
    path: []const u8,
    source: []const u8,
    list: checklist.List,
    run_name: []const u8,
) !oracle.Status {
    var arena: std.heap.ArenaAllocator = .init(ctx.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    if (list.profile == .cog and list.baudrate != serial_boot.baudrate)
        return error.OracleRequiresUploadBaud;
    const oracle_ctx: oracle.Context = .{ .allocator = ctx.allocator, .io = ctx.io, .errors = ctx.errors };
    var program_source: std.Io.Writer.Allocating = .init(allocator);
    defer program_source.deinit();

    if (list.profile == .program) {
        for (list.constants) |constant| {
            try program_source.writer.print("const {s} = {d}\n", .{ constant.name, constant.value });
        }
        try program_source.writer.writeAll(source);
    }
    var compiled = if (list.profile == .cog)
        try oracle.assemble(oracle_ctx, path, list, &.{}, &.{}, 1, false)
    else
        try propan.assemble(ctx.allocator, ctx.io, path, program_source.written(), ctx.errors);
    defer compiled.deinit();

    if (compiled.image.len == 0 or compiled.image.len > 512 * 1024)
        return error.InvalidImageSize;
    const dut_origin = if (list.profile == .cog)
        try address(compiled.module, .{ .symbol = "_wt_start" }, .hub)
    else
        0;
    const entry: u20 = if (list.profile == .cog)
        try codeAddress(compiled.module, list.entry orelse "_wt_testcode")
    else
        0;
    var stop: ?u20 = if (list.profile == .cog)
        try codeAddress(compiled.module, "_wt_stop")
    else if (list.stop) |name|
        try codeAddress(compiled.module, name)
    else
        null;
    if (list.profile == .cog) {
        if (list.stop != null)
            return error.CogStopIsFixed;
        try validateLayout(compiled.module, compiled.image, stop.?);
        if (entry == 0 or entry >= 504)
            return error.ReservedExecution;
    }
    var pre: std.ArrayList(Resolved) = .empty;
    var post: std.ArrayList(Resolved) = .empty;
    var observations: std.ArrayList(Observation) = .empty;
    if (list.profile == .cog) {
        try observations.appendSlice(allocator, &.{
            .{ .kind = .c, .len = 1, .line = 1 },
            .{ .kind = .z, .len = 1, .line = 1 },
            .{ .kind = .q, .len = 4, .line = 1 },
        });
        if (list.pre.len == 0 and list.post.len == 0 and !ctx.options.characterize)
            return error.MissingObservation;
    }
    for ([_][]const checklist.Assignment{ list.pre, list.post }, 0..) |assignments, group| {
        for (assignments) |assignment| {
            if (assignment.target == .sym)
                continue;
            const resolved = resolve(compiled.module, assignment) catch |err| {
                try ctx.errors.print("{s}:{d}: {t}\n", .{ path, assignment.line, err });
                return err;
            };
            const o = resolved.observation;
            if (group == 0) {
                for (pre.items) |old| {
                    if (o.kind == old.observation.kind)
                        return error.DuplicatePrecondition;
                }
                try pre.append(allocator, resolved);
            } else {
                try post.append(allocator, resolved);
            }
            var duplicate = false;
            for (observations.items) |old| {
                if (old.kind == o.kind and old.offset == o.offset and old.len == o.len) {
                    duplicate = true;
                    break;
                }
            }
            if (!duplicate) {
                try observations.append(allocator, o);
            }
        }
    }
    for (post.items) |assertion| {
        if (assertion.observation.kind == .hub and
            !definedMemory(compiled.module, assertion.observation))
            return error.UndefinedMemory;
    }

    if (list.profile == .cog) {
        const initialized = try oracle.assemble(oracle_ctx, path, list, pre.items, observations.items, entry, false);
        compiled.deinit();
        compiled = initialized;
        stop = try codeAddress(compiled.module, "_wt_stop");
    }
    try oracle.patchImage(compiled.module, compiled.image, list.pre, path, ctx.errors);
    if (list.profile == .cog) {
        try validateLayout(compiled.module, compiled.image, stop.?);
    }
    if (ctx.options.characterize)
        return oracle.run(.{
            .allocator = allocator,
            .io = ctx.io,
            .errors = ctx.errors,
            .endpoint = ctx.endpoint,
            .artifacts = ctx.options.@"artifact-dir",
            .prepare_only = ctx.options.@"prepare-oracle",
            .characterize = true,
        }, .{
            .path = path,
            .run_name = run_name,
            .source = source,
            .list = list,
            .entry = entry,
            .seeds = pre.items,
            .assertions = &.{},
            .observations = observations.items,
            .simulated = &.{},
            .stdout = "",
            .original_image = compiled.image,
            .cycles = 0,
            .stop_reason = "hardware characterization; simulator not executed",
        });
    const hub = try allocator.create(Hub);
    try hub.init(.ideal);
    serial_boot.prepare(hub);
    const virtual_terminal = ctx.options.@"fast-io" and list.fast_io_off == null;
    @memcpy(hub.memory[0..compiled.image.len], compiled.image);
    var stdout: std.Io.Writer.Allocating = .init(allocator);
    defer stdout.deinit();

    var terminal_sink: smart_pin.DataSink = .{ .writer = &stdout.writer };
    hub.io.pins[62].smart.tx_mode = if (virtual_terminal)
        .{ .virtual = &terminal_sink }
    else
        .{ .emulated = &terminal_sink };
    var stdin = std.Io.Reader.fixed(list.stdin);
    var terminal_source: smart_pin.DataSource = .{ .reader = &stdin };
    var trace: std.Io.Writer.Allocating = .init(allocator);
    defer trace.deinit();

    if (ctx.options.@"trace-pipeline") {
        hub.trace_writer = &trace.writer;
    }
    try hub.start_cog(0, .{});
    var captured: std.Io.Writer.Allocating = .init(allocator);
    defer captured.deinit();

    errdefer |err| {
        if (ctx.retain_failures and
            err != error.HardwareTransportFailed and
            err != error.OracleMismatch and
            err != error.InvalidOracleLength)
        {
            var snapshot: std.Io.Writer.Allocating = .init(allocator);
            defer snapshot.deinit();

            if (captured.written().len != 0) {
                snapshot.writer.writeAll(captured.written()) catch {};
            } else {
                for (observations.items) |o| {
                    read(hub, list.cog, o, &snapshot.writer) catch {};
                }
            }
            oracle.retainLocalFailure(
                .{ .allocator = allocator, .io = ctx.io, .errors = ctx.errors, .artifacts = ctx.options.@"artifact-dir" },
                path,
                run_name,
                source,
                compiled.image,
                pre.items,
                observations.items,
                snapshot.written(),
                stdout.written(),
                hub.counter,
                trace.written(),
                err,
            ) catch {};
        }
    }
    var reached = false;
    var dut_started = false;
    var supplied_stdin = list.stdin.len == 0;
    // Parallel workers use CPU time so scheduler contention cannot consume a case's execution budget.
    const simulation_clock: std.Io.Clock = if (ctx.options.jobs > 1) .cpu_thread else .awake;
    const started = std.Io.Timestamp.now(ctx.io, simulation_clock).toNanoseconds();
    // Complete program fixtures can intentionally run longer than isolated cog checks.
    const execution_limit_ns: i96 = (if (list.profile == .program)
        @as(i96, 600)
    else
        30) * std.time.ns_per_s;
    while (hub.is_any_cog_active() and hub.counter < list.max_cycles) {
        if (hub.counter & 1023 == 0 and
            std.Io.Timestamp.now(ctx.io, simulation_clock).toNanoseconds() - started > execution_limit_ns)
            return error.SimulatorTimeout;
        if (stop) |pc| {
            if (hub.cogs[list.cog].pipeline[3]) |instruction| {
                if (!reached and instruction.interrupt == null and instruction.pc == pc and
                    (pc < 0x400 or instruction.local_address == null))
                {
                    reached = true;
                    for (observations.items) |o| {
                        try read(hub, list.cog, o, &captured.writer);
                    }
                    if (list.profile == .program) {
                        break;
                    }
                }
            }
        }
        if (list.profile == .cog and hub.cogs[list.cog].registers.get(.PTRB) == dut_origin) {
            dut_started = true;
        }
        if (list.profile == .cog and dut_started and !reached and hub.cogs[list.cog].exec_mode == .cog) {
            if (hub.cogs[list.cog].pipeline[3]) |instruction| {
                try validateDispatch(compiled.module, dut_origin, instruction);
            }
        }
        switch (hub.step()) {
            .normal => {},
            .trap => {
                const fault = hub.fault orelse return error.ExecutionTrap;
                try ctx.errors.print("{s}: cog {d}, pc 0x{x}, {t}: {s}\n", .{
                    path,
                    fault.cog,
                    fault.pc,
                    decode.decode(fault.instruction),
                    fault.reason(),
                });
                return switch (fault.result) {
                    .not_implemented => error.NotImplementedInstruction,
                    .illegal => error.IllegalInstruction,
                    .trap => error.ExecutionTrap,
                    else => unreachable,
                };
            },
            .reset => break,
            .breakpoint => return error.UnexpectedBreakpoint,
        }
        if (!supplied_stdin and stdout.written().len >= list.stdin_after.len) {
            if (!std.mem.startsWith(u8, stdout.written(), list.stdin_after))
                return error.ReadinessMismatch;
            const receiver = &hub.io.pins[63].smart;
            const timing = smart_pin.SerialTiming.init(receiver.registers.x);
            if (receiver.logic != .uart_rx or
                !receiver.registers.enabled or
                timing.bits != 8 or
                (timing.period == 0 and !virtual_terminal))
                return error.UartRxNotReady;
            receiver.rx_mode = if (virtual_terminal)
                .{ .virtual = &terminal_source }
            else
                .{ .emulated = &terminal_source };
            supplied_stdin = true;
        }

        if (stdout.written().len > 1 << 20)
            return error.OutputLimit;
    }
    if (stop != null and !reached)
        return if (hub.is_any_cog_active())
            error.CycleLimit
        else
            error.StopNotReached;
    if ((stop == null or list.profile == .cog) and hub.is_any_cog_active())
        return error.CycleLimit;
    if (!supplied_stdin)
        return error.ReadinessNotReached;
    var ok = true;
    for (post.items) |assertion| {
        var actual: std.Io.Writer.Allocating = .init(allocator);
        defer actual.deinit();

        if (list.profile == .cog) {
            var offset: usize = 0;
            for (observations.items) |o| {
                if (o.kind == assertion.observation.kind and
                    o.offset == assertion.observation.offset and
                    o.len == assertion.observation.len)
                {
                    try actual.writer.writeAll(captured.written()[offset..][0..o.len]);
                    break;
                }
                offset += o.len;
            }
        } else {
            try read(hub, list.cog, assertion.observation, &actual.writer);
        }
        if (!std.mem.eql(u8, assertion.bytes, actual.written())) {
            try ctx.errors.print("{s}:{d}: postcondition {t}[0x{x}]: expected {x}, simulator {x}\n", .{
                path,
                assertion.observation.line,
                assertion.observation.kind,
                assertion.observation.offset,
                assertion.bytes,
                actual.written(),
            });
            ok = false;
        }
    }
    if (!std.mem.eql(u8, list.stdout, stdout.written())) {
        try ctx.errors.print("{s}: stdout: expected {x}, simulator {x}\n", .{ path, list.stdout, stdout.written() });
        ok = false;
    }
    if (!ok)
        return error.AssertionFailed;
    var simulated: std.Io.Writer.Allocating = .init(allocator);
    defer simulated.deinit();

    if (list.profile == .cog) {
        try simulated.writer.writeAll(captured.written());
    } else {
        for (observations.items) |o| {
            try read(hub, list.cog, o, &simulated.writer);
        }
    }

    return oracle.run(.{
        .allocator = allocator,
        .io = ctx.io,
        .errors = ctx.errors,
        .endpoint = if (ctx.options.oracle)
            ctx.endpoint
        else
            "",
        .artifacts = ctx.options.@"artifact-dir",
        .prepare_only = ctx.options.@"prepare-oracle",
    }, .{
        .path = path,
        .run_name = run_name,
        .source = source,
        .list = list,
        .entry = entry,
        .seeds = pre.items,
        .assertions = post.items,
        .observations = observations.items,
        .simulated = simulated.written(),
        .stdout = stdout.written(),
        .original_image = compiled.image,
        .cycles = hub.counter,
        .stop_reason = if (reached)
            "checkpoint"
        else
            "all cogs stopped",
        .trace = trace.written(),
    });
}

/// Collect fixtures and report the test suite result.
pub fn main(init: std.process.Init) !u8 {
    const arguments = try init.minimal.args.toSlice(init.arena.allocator());
    var iterator: Arguments = .{ .items = arguments };
    const executable_name = iterator.next() orelse "windtunnel-tests";
    var cli = args.parse(Options, &iterator, init.arena.allocator(), .print) catch return 1;
    defer cli.deinit();

    var stderr_buffer: [4096]u8 = undefined;
    var stderr = std.Io.File.stderr().writer(init.io, &stderr_buffer);
    defer stderr.interface.flush() catch {};

    if (cli.options.help) {
        try args.printHelp(Options, executable_name, &stderr.interface);
        return 0;
    }
    const endpoint = init.environ_map.get("P2AAS_ENDPOINT") orelse "ws://localhost:12880/";
    if ((cli.options.oracle or cli.options.characterize) and !cli.options.@"prepare-oracle") {
        if (endpoint.len == 0) {
            try stderr.interface.writeAll("P2AAS_ENDPOINT is required for --oracle or --characterize\n");
            return 1;
        }
    }
    var paths: std.ArrayList([]const u8) = .empty;
    const allocator = init.arena.allocator();
    if (cli.positionals.len != 0) {
        try paths.appendSlice(allocator, cli.positionals);
    } else {
        var dir = try std.Io.Dir.cwd().openDir(init.io, "tests/windtunnel", .{ .iterate = true });
        defer dir.close(init.io);

        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next(init.io)) |file| {
            if (file.kind == .file and std.mem.endsWith(u8, file.path, ".propan")) {
                const path = try std.fs.path.join(allocator, &.{ "tests/windtunnel", file.path });
                const source = try std.Io.Dir.cwd().readFileAlloc(init.io, path, allocator, .limited(1 << 20));
                if (checklist.hasHeader(source)) {
                    try paths.append(allocator, path);
                }
            }
        }
    }
    if (paths.items.len == 0)
        return error.EmptySuite;
    const available_files = paths.items.len;
    filterFiles(&paths, cli.options) catch |err| {
        try stderr.interface.print("Invalid file filter: {t}; provide nonempty comma-separated path substrings\n", .{err});
        return 1;
    };
    if (paths.items.len == 0) {
        try stderr.interface.writeAll("No fixture files matched the include/exclude filters\n");
        return 1;
    }
    if (cli.options.include != null or cli.options.exclude != null) {
        try stderr.interface.print("File filters: selected {d} of {d} fixtures\n", .{ paths.items.len, available_files });
    }
    std.mem.sort([]const u8, paths.items, {}, struct {
        /// Sort fixture paths into a stable execution order.
        fn less(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.less);
    const progress = std.Progress.start(init.io, .{
        .root_name = "Windtunnel fixtures",
        .estimated_total_items = paths.items.len,
    });
    defer progress.end();

    // The build runner treats captured stderr as diagnostics.
    const report_success = !init.environ_map.contains("ZIG_PROGRESS");
    const fixture_failed = try allocator.alloc(bool, paths.items.len);
    @memset(fixture_failed, false);
    const suite_started = if (cli.options.@"timing-report" != null)
        std.Io.Timestamp.now(init.io, .awake)
    else
        null;
    const preparation_ns = if (suite_started != null) try allocator.alloc(u64, paths.items.len) else null;

    var jobs: std.ArrayList(Job) = .empty;
    defer {
        for (jobs.items) |*job| {
            job.diagnostics.deinit();
        }
    }

    var failed: usize = 0;
    var failed_runs: usize = 0;
    for (paths.items, fixture_failed, 0..) |path, *failed_fixture, fixture_index| {
        const started = if (suite_started != null) std.Io.Timestamp.now(init.io, .awake) else null;
        defer {
            if (started) |timestamp| {
                preparation_ns.?[fixture_index] = elapsedSince(init.io, timestamp);
            }
        }

        const source = try std.Io.Dir.cwd().readFileAlloc(init.io, path, allocator, .limited(1 << 20));
        const list = checklist.parse(allocator, path, source, &stderr.interface) catch |err| {
            try stderr.interface.print("FAIL {s}: {t}\n", .{ path, err });
            failed_fixture.* = true;
            continue;
        };
        for (list.runs, 1..) |run, index| {
            try jobs.append(allocator, .{
                .path = path,
                .source = source,
                .list = try list.forRun(allocator, run),
                .name = run.name orelse try std.fmt.allocPrint(allocator, "run {d}", .{index}),
                .fixture = fixture_index,
                .progress = progress,
                .diagnostics = .init(init.gpa),
            });
        }
    }
    progress.setEstimatedTotalItems(jobs.items.len);
    try stderr.interface.flush();
    var work: WorkGroup = .{
        .context = .{
            .allocator = init.gpa,
            .io = init.io,
            .errors = &stderr.interface,
            .options = cli.options,
            .endpoint = endpoint,
        },
        .jobs = jobs.items,
    };
    const cpus = if (cli.options.jobs == 0) try std.Thread.getCpuCount() else 1;
    try work.run(workerCount(cli.options, cpus));
    const suite_ns = if (suite_started) |timestamp| elapsedSince(init.io, timestamp) else 0;
    for (jobs.items) |*job| {
        try stderr.interface.writeAll(job.diagnostics.written());
        if (job.failure) |err| {
            try stderr.interface.print("FAIL {s} [{s}]: {t}\n", .{ job.path, job.name, err });
            fixture_failed[job.fixture] = true;
            failed_runs += 1;
        } else if (report_success) {
            try stderr.interface.print("{s} {s} [{s}] (oracle {t})\n", .{
                if (cli.options.characterize) "CAPTURE" else "PASS",
                job.path,
                job.name,
                job.status.?,
            });
        }
    }
    for (fixture_failed) |failed_fixture| {
        if (failed_fixture) {
            failed += 1;
        }
    }
    const total_runs = jobs.items.len;
    if (report_success or failed != 0) {
        try stderr.interface.print("{d} fixtures: {d} {s}, {d} failed; {d} runs: {d} {s}, {d} failed\n", .{
            paths.items.len,
            paths.items.len - failed,
            if (cli.options.characterize)
                "captured"
            else
                "passed",
            failed,
            total_runs,
            total_runs - failed_runs,
            if (cli.options.characterize)
                "captured"
            else
                "passed",
            failed_runs,
        });
    }
    if (cli.options.@"timing-report") |path| {
        const file = try std.Io.Dir.cwd().createFile(init.io, path, .{});
        defer file.close(init.io);

        var buffer: [4096]u8 = undefined;
        var writer = file.writer(init.io, &buffer);
        try writeTimingReport(
            init.gpa,
            &writer.interface,
            paths.items,
            preparation_ns.?,
            jobs.items,
            work.context.options.jobs,
            suite_ns,
        );
        try writer.interface.flush();
        try stderr.interface.print("Timing report: {s}\n", .{path});
    }
    return if (failed == 0)
        0
    else
        1;
}

/// Run an in-memory fixture through the test harness.
fn runTemporary(ctx: Context, source: []const u8) !oracle.Status {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(ctx.io, .{ .sub_path = "fixture.propan", .data = source });
    const path = try tmp.dir.realPathFileAlloc(ctx.io, "fixture.propan", ctx.allocator);
    defer ctx.allocator.free(path);

    return runSource(ctx, path, source);
}

test "bounded imported fixtures reject wrong assertions, invalid state and missing exit" {
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const ctx: Context = .{ .allocator = std.testing.allocator, .io = std.testing.io, .errors = &errors.writer, .retain_failures = false };
    try std.testing.expect(std.mem.find(u8, @embedFile("oracle-template"), "WAITX") == null);
    try std.testing.expectError(error.OracleRequiresUploadBaud, runTemporary(
        ctx,
        "//? WINDTUNNEL CHECK LIST\n//? baudrate: 115200\n//? post: cog[0].c == false\n\nJMP nrel(_end)\n",
    ));
    const code = "\nMOV result, 7\nJMP nrel(_end)\nvar result: LONG 0\n";
    _ = try runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].reg[result] == 7\n" ++ code);
    try std.testing.expectError(error.AssertionFailed, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].reg[result] == 8\n" ++ code));
    try std.testing.expectError(error.CycleLimit, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? max-cycles: 1\n//? post: cog[0].reg[result] == 7\n" ++ code));
    try std.testing.expectError(error.CycleLimit, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? max-cycles: 100\n//? post: cog[0].reg[value] == 0\n\nSETQ aug(0xffffffff)\nRDLONG value, PTRA\nJMP nrel(_end)\nvar value: LONG 0\n"));
    try std.testing.expectError(error.DuplicatePrecondition, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? pre: cog[0].c = true\n//? pre: cog[0].c = false\n" ++ code));
    try std.testing.expectError(error.UnknownSymbol, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].reg[missing] == 7\n" ++ code));
    try std.testing.expectError(error.InvalidChecklist, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? pre: cog[0].reg[1] = 0\n" ++ code));
    try std.testing.expectError(error.ReservedRegister, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].reg[0] == 7\n" ++ code));
    try std.testing.expectError(error.ReservedMemory, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: hub[_wt_reg_snapshot] == hex [00]\n" ++ code));
    try std.testing.expectError(error.MissingExitJump, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].c == false\n\nCOGSTOP 0\n"));
    try std.testing.expectError(error.MissingExitJump, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].c == false\n\nif(C) JMP nrel(_end)\n"));
    try std.testing.expectError(error.StopNotReached, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].c == false\n\nCOGSTOP 0\nJMP nrel(_end)\n"));
    try std.testing.expectEqual(oracle.Status.skipped, try runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].reg[20] == 0\n\nGETSCP register(20)\nJMP nrel(_end)\n"));
    try std.testing.expectEqual(oracle.Status.skipped, try runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nGETSCP register(20)\nCOGSTOP 0\n"));
    try std.testing.expectError(error.NotImplementedInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nWRPIN P_USB_PAIR, 1\nCOGSTOP 0\n"));
    try std.testing.expectError(error.ReadinessNotReached, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n//? stdin: \"x\"\n//? stdin-after: \"READY\"\n//? stdout: \"READYx\"\n\n.cogexec\nCOGSTOP 0\n"));
}

test "illegal FIFO use reports its reason and respects conditional cancellation" {
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const ctx: Context = .{ .allocator = std.testing.allocator, .io = std.testing.io, .errors = &errors.writer, .retain_failures = false };
    const header = "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nJMP entry\n.hubexec 0x1000\nentry:\n";
    for ([_][]const u8{ "RFBYTE register(20)\n", "RDFAST 0, 0\n" }) |instruction| {
        errors.clearRetainingCapacity();
        const source = try std.mem.concat(ctx.allocator, u8, &.{ header, instruction, "COGSTOP 0\n" });
        defer ctx.allocator.free(source);

        try std.testing.expectError(error.IllegalInstruction, runTemporary(ctx, source));
        try std.testing.expect(std.mem.indexOf(u8, errors.written(), "hub execution uses the FIFO for instruction fetching") != null);
    }
    errors.clearRetainingCapacity();
    try std.testing.expectError(error.IllegalInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nRFBYTE register(20)\nCOGSTOP 0\n"));
    try std.testing.expect(std.mem.indexOf(u8, errors.written(), "FIFO read requires RDFAST") != null);
    _ = try runTemporary(ctx, header ++ "if(C) RFBYTE register(20)\nif(C) RDFAST 0, 0\nCOGSTOP 0\n");
}

test "runs patch cog and hub initialization independently" {
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const ctx: Context = .{ .allocator = std.testing.allocator, .io = std.testing.io, .errors = &errors.writer, .retain_failures = false };
    _ = try runTemporary(ctx,
        \\//? WINDTUNNEL CHECK LIST
        \\//? pre: sym[value] = u32 [1]
        \\//? post: cog[0].reg[value] == 2
        \\//? post: hub[buffer] == u32 [2]
        \\//? run: "zero"
        \\//? pre: sym[buffer] = u8 [0, 0, 0, 0]
        \\//? pre: cog[0].c = true
        \\//? post: cog[0].reg[loaded] == 0
        \\//? post: cog[0].c == true
        \\//? run:
        \\//? post: cog[0].reg[loaded] == 0x01020304
        \\//? post: cog[0].c == false
        \\//? run: "partial"
        \\//? pre: sym[buffer] = hex [ff]
        \\//? post: cog[0].reg[loaded] == 0x010203ff
        \\//? post: cog[0].c == false
        \\//? run: "restored"
        \\//? post: cog[0].reg[loaded] == 0x01020304
        \\//? post: cog[0].c == false
        \\
        \\RDLONG loaded, aug(hubaddr(buffer))
        \\ADD value, 1
        \\WRLONG value, aug(hubaddr(buffer))
        \\JMP nrel(_end)
        \\var value: LONG 0xffffffff
        \\var loaded: LONG 0
        \\.data
        \\buffer: LONG 0x01020304
    );
    const code = "\nJMP nrel(_end)\nvar value: LONG 0\n";
    try std.testing.expectError(error.UnknownSymbol, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? pre: sym[missing] = u32 [1]\n" ++ code));
    try std.testing.expectError(error.EmptyPatch, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? pre: sym[value] = u32 []\n" ++ code));
    try std.testing.expectError(error.ReservedMemory, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? pre: sym[_wt_reg_snapshot] = u32 [1]\n" ++ code));
    try std.testing.expectError(error.UndefinedMemory, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? pre: sym[value] = u32 [1, 2]\n" ++ code));
    // Program fixtures can patch initialization without the cog reporter.
    _ = try runTemporary(ctx,
        \\//? WINDTUNNEL CHECK LIST
        \\//? profile: program
        \\//? pre: sym[stopper] = u32 [0]
        \\
        \\.cogexec
        \\COGSTOP stopper
        \\var stopper: LONG 7
    );
}

test "oracle preparation applies patches to every run and records run identity" {
    var tmp = std.testing.tmpDir(.{ .iterate = true });
    defer tmp.cleanup();

    const artifacts = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(artifacts);

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const ctx: Context = .{ .allocator = std.testing.allocator, .io = std.testing.io, .errors = &errors.writer, .options = .{ .@"prepare-oracle" = true, .@"artifact-dir" = artifacts }, .retain_failures = false };
    try std.testing.expectEqual(oracle.Status.prepared, try runTemporary(ctx,
        \\//? WINDTUNNEL CHECK LIST
        \\//? post: cog[0].reg[value] == 0x76543210
        \\//? post: cog[0].reg[next] == 0xfedcba98
        \\//? pre: sym[value] = u32 [0x76543210, 0xfedcba98]
        \\//? run: "first"
        \\//? run: "second"
        \\
        \\JMP nrel(_end)
        \\var value: LONG 0x01234567
        \\var next: LONG 0x89abcdef
    ));
    var walker = try tmp.dir.walk(std.testing.allocator);
    defer walker.deinit();

    var images: usize = 0;
    var metadata: usize = 0;
    var names: [2]bool = .{ false, false };
    while (try walker.next(std.testing.io)) |file| {
        if (file.kind != .file)
            continue;
        if (std.mem.eql(u8, file.basename, "original.bin") or std.mem.eql(u8, file.basename, "uploaded.bin")) {
            const image = try tmp.dir.readFileAlloc(std.testing.io, file.path, std.testing.allocator, .limited(1 << 20));
            defer std.testing.allocator.free(image);

            try std.testing.expect(std.mem.indexOf(u8, image, &.{ 0x10, 0x32, 0x54, 0x76, 0x98, 0xba, 0xdc, 0xfe }) != null);
            images += 1;
        } else if (std.mem.eql(u8, file.basename, "simulation.json")) {
            const json = try tmp.dir.readFileAlloc(std.testing.io, file.path, std.testing.allocator, .limited(1 << 20));
            defer std.testing.allocator.free(json);

            var parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, json, .{});
            defer parsed.deinit();

            const name = parsed.value.object.get("run").?.string;
            const index: usize = if (std.mem.eql(u8, name, "first"))
                0
            else if (std.mem.eql(u8, name, "second"))
                1
            else
                return error.UnexpectedRunName;
            try std.testing.expect(!names[index]);
            names[index] = true;
            metadata += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 4), images);
    try std.testing.expectEqual(@as(usize, 2), metadata);
}

test "characterization preparation does not require a supported local instruction or correct postcondition" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const artifacts = try tmp.dir.realPathFileAlloc(std.testing.io, ".", std.testing.allocator);
    defer std.testing.allocator.free(artifacts);

    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const ctx: Context = .{ .allocator = std.testing.allocator, .io = std.testing.io, .errors = &errors.writer, .options = .{ .characterize = true, .@"prepare-oracle" = true, .@"artifact-dir" = artifacts }, .retain_failures = false };
    try std.testing.expectEqual(oracle.Status.prepared, try runTemporary(ctx,
        \\//? WINDTUNNEL CHECK LIST
        \\//? post: cog[0].reg[value] == 99
        \\
        \\WAITX 0 :wc
        \\MOV value, 7
        \\JMP nrel(_end)
        \\var value: LONG 0
    ));
}

test "matrix runs assemble constants and select fresh cogs" {
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const ctx: Context = .{ .allocator = std.testing.allocator, .io = std.testing.io, .errors = &errors.writer, .retain_failures = false };
    _ = try runTemporary(
        ctx,
        "//? WINDTUNNEL CHECK LIST\n" ++
            "//? const: value = 3\n" ++
            "//? const: adjustment = 1\n" ++
            "//? run: \"cog zero\"\n" ++
            "//? cog: 0\n" ++
            "//? post: cog[*].reg[result] == 4\n" ++
            "//? run: \"cog seven\"\n" ++
            "//? cog: 7\n" ++
            "//? const: value = 8\n" ++
            "//? post: cog[*].reg[result] == 9\n" ++
            "MOV result, value\nADD result, adjustment\nJMP nrel(_end)\nvar result: LONG 0\n",
    );
}

test "fast I/O supports program input and output while timing fixtures retain clocked UART" {
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const ctx: Context = .{
        .allocator = std.testing.allocator,
        .io = std.testing.io,
        .errors = &errors.writer,
        .options = .{ .@"fast-io" = true },
        .retain_failures = false,
    };
    _ = try runTemporary(ctx,
        \\//? WINDTUNNEL CHECK LIST
        \\//? profile: program
        \\//? stdin: hex [0080ff]
        \\//? stdin-after: "!"
        \\//? stdout: "!\x00\x80\xff"
        \\//? max-cycles: 2000
        \\.cogexec
        \\WRPIN P_ASYNC_RX, 63
        \\WXPIN 7, 63
        \\DIRH 63
        \\WRPIN P_TT_01 | P_ASYNC_TX, 62
        \\WXPIN 7, 62
        \\DIRH 62
        \\WYPIN '!', 62
        \\MOV count, 3
        \\read_byte:
        \\TESTP 63 :wc
        \\if(!C) JMP read_byte
        \\RDPIN PA, 63
        \\SHR PA, 24
        \\WYPIN PA, 62
        \\DJNZ count, read_byte
        \\WAITX 8
        \\COGSTOP 0
        \\var count: LONG 0
    );
    const path = "tests/windtunnel/program/uart-checkpoint.propan";
    const source = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, path, std.testing.allocator, .limited(1 << 20));
    defer std.testing.allocator.free(source);

    _ = try runSource(ctx, path, source);
}

/// Serve a bounded number of actual TCP connections so session cleanup is checked without a subprocess.
fn serveTestConnections(listener: *std.Io.net.Server, count: usize, configuration: Hub.SystemConfiguration) !void {
    for (0..count) |_| {
        const stream = try listener.accept(std.testing.io);
        defer stream.close(std.testing.io);

        try p2aas_server.handleConnection(std.testing.allocator, std.testing.io, stream, .{
            .trace = false,
            .fast_io = false,
            .system_configuration = configuration,
        });
    }
}

test "RCFAST hello world preserves inherited ROM baud timing and drains UART without WAITX" {
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const path = "examples/hello-world-no-pll.propan";
    const source = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, path, std.testing.allocator, .limited(1 << 20));
    defer std.testing.allocator.free(source);

    try std.testing.expect(std.mem.find(u8, source, "WAITX") == null);
    var compiled = try propan.assemble(std.testing.allocator, std.testing.io, path, source, &errors.writer);
    defer compiled.deinit();

    const halt = try codeAddress(compiled.module, ".halt");
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    // Model serial ROM handoff at three RCFAST rates, including a fractional divider.
    for ([_]u32{ 0x000a_0007, 0x000b_8007, 0x000f_0007 }) |divider| {
        try hub.init(.ideal);
        serial_boot.prepare(hub);
        try std.testing.expectEqual(@as(u32, 0x000a_0007), hub.io.pins[63].smart.registers.x);
        @memcpy(hub.memory[0..compiled.image.len], compiled.image);
        var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
        defer output.deinit();

        var sink: smart_pin.DataSink = .{ .writer = &output.writer };
        const tx = &hub.io.pins[62].smart;
        tx.registers.x = divider;
        tx.tx_mode = .{ .emulated = &sink };
        try hub.start_cog(0, .{});

        while (hub.counter < 10_000) {
            if (hub.cogs[0].pipeline[3]) |instruction| {
                if (instruction.pc == halt)
                    break;
            }
            _ = hub.step();
        }
        try std.testing.expect(hub.counter < 10_000);
        try std.testing.expectEqualStrings("Hello world!\r\n", output.written());
        try std.testing.expectEqual(divider, tx.registers.x);
        try std.testing.expectEqual(@as(u32, 0xf0), hub.io.clock_mode);
        try std.testing.expect(!hub.io.pins[62].outputs.flag);
    }
}

test "native P2AAS client uses the simulator server and subsequent sessions start fresh" {
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    errdefer std.debug.print("{s}\n", .{errors.written()});

    const path = "tests/windtunnel/program/server-terminal.propan";
    const source = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, path, std.testing.allocator, .limited(1 << 20));
    defer std.testing.allocator.free(source);

    var compiled = try propan.assemble(std.testing.allocator, std.testing.io, path, source, &errors.writer);
    defer compiled.deinit();

    for ([_]Hub.SystemConfiguration{ .ideal, .realistic }) |configuration| {
        const ip = try std.Io.net.IpAddress.parse("127.0.0.1", 0);
        var listener = try ip.listen(std.testing.io, .{});
        defer listener.deinit(std.testing.io);

        const endpoint = try std.fmt.allocPrint(std.testing.allocator, "ws://127.0.0.1:{d}/", .{listener.socket.address.getPort()});
        defer std.testing.allocator.free(endpoint);

        var server = try std.Io.concurrent(std.testing.io, serveTestConnections, .{ &listener, 4, configuration });
        defer server.cancel(std.testing.io) catch {};

        for ([_][]const u8{ "\x00\x80\xff", "ABC" }) |packet| {
            var output: std.ArrayList(u8) = .empty;
            defer output.deinit(std.testing.allocator);

            try p2aas.run(std.testing.allocator, std.testing.io, endpoint, .{
                .image = compiled.image,
                .stdin = packet,
                .ready = "!",
                .baudrate = serial_boot.baudrate,
                // Debug builds share CPU with the fixture suite; allow clocked I/O to finish.
                .timeout_ms = 3000,
            }, &output, &errors.writer);
            try std.testing.expectEqual(@as(usize, 4), output.items.len);
            try std.testing.expectEqualSlices(u8, packet, output.items[1..]);
        }
        var tmp = std.testing.tmpDir(.{});
        defer tmp.cleanup();

        // HUBSET reset must close normally instead of waiting for the runtime quota.
        {
            const reset_source = ".cogexec\nHUBSET aug(0x10000000)\n";
            try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "reset.propan", .data = reset_source });
            const reset_path = try tmp.dir.realPathFileAlloc(std.testing.io, "reset.propan", std.testing.allocator);
            defer std.testing.allocator.free(reset_path);

            var reset = try propan.assemble(std.testing.allocator, std.testing.io, reset_path, reset_source, &errors.writer);
            defer reset.deinit();

            var reset_output: std.ArrayList(u8) = .empty;
            defer reset_output.deinit(std.testing.allocator);

            const before_reset = errors.written().len;
            try p2aas.run(std.testing.allocator, std.testing.io, endpoint, .{
                .image = reset.image,
                .stdin = "",
                .ready = "",
                .baudrate = serial_boot.baudrate,
                .timeout_ms = 1000,
            }, &reset_output, &errors.writer);
            try std.testing.expectEqual(@as(usize, 0), reset_output.items.len);
            try std.testing.expect(std.mem.find(u8, errors.written()[before_reset..], "P2AAS close 1000:") != null);
        }
        // A simulator fault must finish the close handshake and release the board just like a transport failure.

        const illegal_source = ".cogexec\nJMP hub_entry\n.hubexec 0x1000\nhub_entry:\nRFBYTE PA\n";
        try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "illegal.propan", .data = illegal_source });
        const illegal_path = try tmp.dir.realPathFileAlloc(std.testing.io, "illegal.propan", std.testing.allocator);
        defer std.testing.allocator.free(illegal_path);

        var illegal = try propan.assemble(std.testing.allocator, std.testing.io, illegal_path, illegal_source, &errors.writer);
        defer illegal.deinit();

        var output: std.ArrayList(u8) = .empty;
        defer output.deinit(std.testing.allocator);

        try std.testing.expectError(error.HardwareClosed, p2aas.run(std.testing.allocator, std.testing.io, endpoint, .{
            .image = illegal.image,
            .stdin = "",
            .ready = "",
            .baudrate = 115200,
            .timeout_ms = 1000,
        }, &output, &errors.writer));
        try std.testing.expect(std.mem.find(u8, errors.written(), "P2AAS close 1011:") != null);
        try server.await(std.testing.io);
    }
}

test "DAC checklist observations are local-only, bounded and reject preconditions" {
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const ctx: Context = .{ .allocator = std.testing.allocator, .io = std.testing.io, .errors = &errors.writer, .retain_failures = false };
    const header = "//? WINDTUNNEL CHECK LIST\n//? oracle: off \"no digital DAC readback\"\n";
    const code = "\nSETDACS aug(0x78563412)\nJMP nrel(_end)\n";
    try std.testing.expectEqual(oracle.Status.off, try runTemporary(ctx, header ++ "//? post: cog[*].dacs == u8 [0x12 0x34 0x56 0x78]\n" ++ code));
    try std.testing.expectError(error.AssertionFailed, runTemporary(ctx, header ++ "//? post: cog[*].dacs == u8 [0 0 0 0]\n" ++ code));
    try std.testing.expectError(error.InvalidChecklist, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[*].dacs == u8 [0 0 0 0]\n" ++ code));
    try std.testing.expectError(error.InvalidChecklist, runTemporary(ctx, header ++ "//? post: cog[*].dacs == u8 [0 0 0]\n" ++ code));
    try std.testing.expectError(error.InvalidChecklist, runTemporary(ctx, header ++ "//? post: cog[*].dacs == u32 [0]\n" ++ code));
    try std.testing.expectError(error.InvalidChecklist, runTemporary(ctx, header ++ "//? pre: cog[*].dacs = u8 [0 0 0 0]\n" ++ code));
}

test "streamer excluded modes and FIFO ownership fail explicitly, while cancellation is inert" {
    var errors: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer errors.deinit();

    const ctx: Context = .{ .allocator = std.testing.allocator, .io = std.testing.io, .errors = &errors.writer, .retain_failures = false };
    const header = "//? WINDTUNNEL CHECK LIST\n//? post: cog[*].c == false\n\n";
    for ([_][]const u8{
        "XINIT aug(X_1ADC8_0P_1DAC8_WFBYTE | 1), 0\n",
    }) |instruction| {
        const source = try std.mem.concat(ctx.allocator, u8, &.{ header, instruction, "JMP nrel(_end)\n" });
        defer ctx.allocator.free(source);

        try std.testing.expectEqual(oracle.Status.skipped, try runTemporary(ctx, source));
    }
    try std.testing.expectError(error.IllegalInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nXINIT aug(X_RFLONG_32P_4DAC8 | 1), 0\nCOGSTOP 0\n"));
    try std.testing.expectError(error.IllegalInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nXINIT aug(X_32P_4DAC8_WFLONG | X_WRITE_ON | 1), 0\nCOGSTOP 0\n"));
    try std.testing.expectEqual(oracle.Status.skipped, try runTemporary(ctx, header ++ "if(C) SETCMOD 0x120\nif(C) XINIT aug(X_DDS_GOERTZEL_SINC1 | 1), 0\nJMP nrel(_end)\n"));
}
