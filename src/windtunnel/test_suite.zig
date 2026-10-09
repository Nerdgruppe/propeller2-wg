const std = @import("std");
const args = @import("args");
const propan = @import("propan");
const checklist = @import("check_list.zig");
const Hub = @import("sim/Hub.zig");
const decode = @import("sim/decode.zig");
const encoding = @import("sim/encoding.zig");
const oracle = @import("oracle.zig");

pub const std_options: std.Options = .{ .log_level = .err };

const Options = struct {
    help: bool = false,
    oracle: bool = false,
    characterize: bool = false,
    @"trace-pipeline": bool = false,
    @"prepare-oracle": bool = false,
    @"artifact-dir": []const u8 = ".zig-cache/windtunnel-artifacts",
    pub const shorthands = .{ .h = "help" };
    pub const meta = .{ .usage_summary = "[FILE.propan ...]", .option_docs = .{
        .help = "Show usage",
        .oracle = "Run the hardware oracle through P2AAS_ENDPOINT",
        .characterize = "Capture hardware measurements without executing or checking the simulator",
        .@"trace-pipeline" = "Retain up to 10000 pipeline events with prepared images and failure evidence",
        .@"prepare-oracle" = "Generate oracle images and requests without uploading them",
        .@"artifact-dir" = "Directory for prepared images and failure evidence",
    } };
};

const Context = struct { allocator: std.mem.Allocator, io: std.Io, errors: *std.Io.Writer, options: Options = .{}, endpoint: []const u8 = "", retain_failures: bool = true };
const Kind = oracle.Kind;
const Observation = oracle.Observation;
const Resolved = oracle.Resolved;

fn address(module: propan.Module, value: checklist.Address, kind: Kind) !u32 {
    return switch (value) {
        .number => |n| n,
        .symbol => |name| blk: {
            var found: ?u32 = null;
            for (module.symbols) |symbol| {
                if (!std.mem.eql(u8, symbol.name, name)) continue;
                if (found != null) return error.AmbiguousSymbol;
                found = if (kind == .hub) symbol.label.hub_address orelse return error.InvalidSymbolKind else switch (symbol.label.local) {
                    .cog, .regspace => |n| n,
                    else => return error.InvalidSymbolKind,
                };
            }
            break :blk found orelse return error.UnknownSymbol;
        },
    };
}

fn codeAddress(module: propan.Module, name: []const u8) !u20 {
    var found: ?u20 = null;
    for (module.symbols) |symbol| {
        if (!std.mem.eql(u8, symbol.name, name)) continue;
        if (found != null) return error.AmbiguousSymbol;
        if (symbol.type != .code) return error.InvalidSymbolKind;
        found = switch (symbol.label.local) {
            .cog => |n| n,
            .lut => |n| @as(u20, n) + 0x200,
            .hub => @intCast(symbol.label.hub_address orelse return error.InvalidSymbolKind),
            else => return error.InvalidSymbolKind,
        };
    }
    return found orelse error.UnknownSymbol;
}

fn resolve(module: propan.Module, assignment: checklist.Assignment) !Resolved {
    const kind: Kind = switch (assignment.target) {
        .sym => return error.InvalidPrecondition,
        .reg => .reg,
        .hub => .hub,
        .c => .c,
        .z => .z,
        .q => .q,
    };
    const offset = switch (assignment.target) {
        .reg, .hub => |a| try address(module, a, kind),
        .c, .z, .q => 0,
        .sym => unreachable,
    };
    const len: u32 = @intCast(assignment.bytes.len);
    if (len == 0) return error.EmptyObservation;
    if (kind == .reg and (offset == 0 or offset >= 506)) return error.ReservedRegister;
    if (kind == .hub and oracle.reservedMemory(module, offset, len)) return error.ReservedMemory;
    return .{ .observation = .{ .kind = kind, .offset = offset, .len = len, .line = assignment.line }, .bytes = assignment.bytes };
}

fn isCode(module: propan.Module, offset: u32, len: u32) bool {
    for (module.line_data) |line| {
        if (line.kind == .code and offset < @as(u64, line.offset) + line.length and line.offset < @as(u64, offset) + len) return true;
    }
    return false;
}

fn validateLayout(module: propan.Module, image: []const u8, stop: u20) !void {
    for (module.symbols) |symbol| {
        if (std.mem.startsWith(u8, symbol.name, "_wt_") and !oracle.isScaffold(if (symbol.source_location) |loc| loc.source else null)) return error.ReservedSymbol;
    }
    for (module.constants) |constant| if (std.mem.startsWith(u8, constant.name, "_wt_") and !oracle.isScaffold(constant.location.source)) return error.ReservedSymbol;
    const origin = try address(module, .{ .symbol = "_wt_start" }, .hub);
    var exit = false;
    for (module.line_data) |line| {
        if (oracle.isScaffold(line.location.source) or line.length == 0) continue;
        const end = @as(u64, line.offset) + line.length;
        if (oracle.reservedMemory(module, line.offset, line.length)) return error.ReservedMemory;
        if (line.pc) |pc| if (pc < 0x200) {
            // Separate images may be loaded into other cogs by the DUT. They
            // must lie entirely outside its scaffold-loaded register image.
            var other_image = false;
            for (module.segments) |segment| if (segment.exec_mode == .cog and
                segment.hub_offset >= origin + 504 * 4 and line.offset >= segment.hub_offset and
                end <= segment.hub_offset + segment.data.len)
            {
                other_image = true;
            };
            if (!other_image and (pc == 0 or pc >= 504 or end > origin + 504 * 4 or line.offset != origin + pc * 4)) return error.InvalidCogLayout;
        } else if (pc < 0x400) {
            // Cog code still has the loader mapping; LUT code is loaded by the DUT.
            var in_lut = false;
            for (module.segments) |segment| if (line.offset >= segment.hub_offset and end <= segment.hub_offset + segment.data.len and segment.exec_mode == .lut) {
                in_lut = true;
            };
            if (!in_lut and (pc >= 504 or end > origin + 504 * 4 or line.offset != origin + pc * 4)) return error.InvalidCogLayout;
        };
        if (line.kind == .code and line.length >= 4) {
            const instr: encoding.Instruction = .{ .raw = std.mem.readInt(u32, image[line.offset..][0..4], .little) };
            if (decode.decode(instr.raw) == .jmp_a and !instr.abs_pointer.relative and instr.abs_pointer.cond == .IF_ALWAYS and instr.abs_pointer.address == stop) exit = true;
        }
    }
    if (!exit) return error.MissingExitJump;
}

fn validateDispatch(module: propan.Module, origin: u32, instruction: @import("sim/Cog.zig").PipelineState) !void {
    if (instruction.pc == 0) return; // The one-word trampoline belongs to the scaffold.
    if (instruction.pc >= 0x200 and instruction.pc < 0x400) {
        for (module.line_data) |line| {
            const pc = line.pc orelse continue;
            if (line.kind != .code or pc < 0x200 or pc > instruction.pc or instruction.pc >= pc + line.length / 4) continue;
            for (module.segments) |segment| if (segment.exec_mode == .lut and line.offset >= segment.hub_offset and line.offset < segment.hub_offset + segment.data.len) return;
        }
        return error.ExecutingData;
    }
    if (instruction.pc >= 504) return error.ReservedExecution;
    if (!isCode(module, origin + @as(u32, instruction.pc) * 4, 4)) return error.ExecutingData;
}

fn definedMemory(module: propan.Module, observation: Observation) bool {
    for (observation.offset..observation.offset + observation.len) |index| {
        var defined = false;
        for (module.segments) |segment| {
            if (index >= segment.hub_offset and index < segment.hub_offset + segment.data.len) {
                defined = true;
                break;
            }
        }
        if (!defined) return false;
    }
    return true;
}

fn read(hub: *Hub, cog: u3, observation: Observation, out: *std.Io.Writer) !void {
    switch (observation.kind) {
        .reg => try out.writeInt(u32, hub.cogs[cog].registers.get(@enumFromInt(observation.offset)), .little),
        .q => try out.writeInt(u32, hub.cogs[cog].q, .little),
        .hub => try out.writeAll(hub.memory[observation.offset..][0..observation.len]),
        .c => try out.writeByte(@intFromBool(hub.cogs[cog].c)),
        .z => try out.writeByte(@intFromBool(hub.cogs[cog].z)),
    }
}

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

fn runOne(ctx: Context, path: []const u8, source: []const u8, list: checklist.List, run_name: []const u8) !oracle.Status {
    var arena: std.heap.ArenaAllocator = .init(ctx.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const oracle_ctx: oracle.Context = .{ .allocator = ctx.allocator, .io = ctx.io, .errors = ctx.errors };
    var program_source: std.Io.Writer.Allocating = .init(allocator);
    defer program_source.deinit();
    if (list.profile == .program) {
        for (list.constants) |constant| try program_source.writer.print("const {s} = {d}\n", .{ constant.name, constant.value });
        try program_source.writer.writeAll(source);
    }
    var compiled = if (list.profile == .cog) try oracle.assemble(oracle_ctx, path, list, &.{}, &.{}, 1, false) else try propan.assemble(ctx.allocator, ctx.io, path, program_source.written(), ctx.errors);
    defer compiled.deinit();
    if (compiled.image.len == 0 or compiled.image.len > 512 * 1024) return error.InvalidImageSize;
    const dut_origin = if (list.profile == .cog) try address(compiled.module, .{ .symbol = "_wt_start" }, .hub) else 0;
    const entry: u20 = if (list.profile == .cog) try codeAddress(compiled.module, list.entry orelse "_wt_testcode") else 0;
    var stop: ?u20 = if (list.profile == .cog) try codeAddress(compiled.module, "_wt_stop") else if (list.stop) |name| try codeAddress(compiled.module, name) else null;
    if (list.profile == .cog) {
        if (list.stop != null) return error.CogStopIsFixed;
        try validateLayout(compiled.module, compiled.image, stop.?);
        if (entry == 0 or entry >= 504) return error.ReservedExecution;
    }
    var pre: std.ArrayList(Resolved) = .empty;
    var post: std.ArrayList(Resolved) = .empty;
    var observations: std.ArrayList(Observation) = .empty;
    if (list.profile == .cog) {
        try observations.appendSlice(allocator, &.{ .{ .kind = .c, .len = 1, .line = 1 }, .{ .kind = .z, .len = 1, .line = 1 }, .{ .kind = .q, .len = 4, .line = 1 } });
        if (list.pre.len == 0 and list.post.len == 0 and !ctx.options.characterize) return error.MissingObservation;
    }
    for ([_][]const checklist.Assignment{ list.pre, list.post }, 0..) |assignments, group| {
        for (assignments) |assignment| {
            if (assignment.target == .sym) continue;
            const resolved = resolve(compiled.module, assignment) catch |err| {
                try ctx.errors.print("{s}:{d}: {t}\n", .{ path, assignment.line, err });
                return err;
            };
            const o = resolved.observation;
            if (group == 0) {
                for (pre.items) |old| if (o.kind == old.observation.kind) return error.DuplicatePrecondition;
                try pre.append(allocator, resolved);
            } else try post.append(allocator, resolved);
            var duplicate = false;
            for (observations.items) |old| if (old.kind == o.kind and old.offset == o.offset and old.len == o.len) {
                duplicate = true;
                break;
            };
            if (!duplicate) try observations.append(allocator, o);
        }
    }
    for (post.items) |assertion| if (assertion.observation.kind == .hub and !definedMemory(compiled.module, assertion.observation)) return error.UndefinedMemory;

    if (list.profile == .cog) {
        const initialized = try oracle.assemble(oracle_ctx, path, list, pre.items, observations.items, entry, false);
        compiled.deinit();
        compiled = initialized;
        stop = try codeAddress(compiled.module, "_wt_stop");
    }
    try oracle.patchImage(compiled.module, compiled.image, list.pre, path, ctx.errors);
    if (list.profile == .cog) try validateLayout(compiled.module, compiled.image, stop.?);
    if (ctx.options.characterize) return oracle.run(.{
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
    hub.init();
    @memcpy(hub.memory[0..compiled.image.len], compiled.image);
    var stdout: std.Io.Writer.Allocating = .init(allocator);
    defer stdout.deinit();
    hub.output_writer = &stdout.writer;
    var trace: std.Io.Writer.Allocating = .init(allocator);
    defer trace.deinit();
    if (ctx.options.@"trace-pipeline") hub.trace_writer = &trace.writer;
    try hub.start_cog(0, .{});
    var captured: std.Io.Writer.Allocating = .init(allocator);
    defer captured.deinit();
    errdefer |err| {
        if (ctx.retain_failures and err != error.HardwareTransportFailed and err != error.OracleMismatch and err != error.InvalidOracleLength) {
            var snapshot: std.Io.Writer.Allocating = .init(allocator);
            defer snapshot.deinit();
            if (captured.written().len != 0) snapshot.writer.writeAll(captured.written()) catch {} else for (observations.items) |o| read(hub, list.cog, o, &snapshot.writer) catch {};
            oracle.retainLocalFailure(.{ .allocator = allocator, .io = ctx.io, .errors = ctx.errors, .artifacts = ctx.options.@"artifact-dir" }, path, run_name, source, compiled.image, pre.items, observations.items, snapshot.written(), stdout.written(), hub.counter, trace.written(), err) catch {};
        }
    }
    var reached = false;
    var dut_started = false;
    var supplied_stdin = list.stdin.len == 0;
    const started = std.Io.Timestamp.now(ctx.io, .awake).toNanoseconds();
    while (hub.is_any_cog_active() and hub.counter < list.max_cycles) {
        if (hub.counter & 1023 == 0 and std.Io.Timestamp.now(ctx.io, .awake).toNanoseconds() - started > 30 * std.time.ns_per_s) return error.SimulatorTimeout;
        if (stop) |pc| {
            if (hub.cogs[list.cog].pipeline[3]) |instruction| {
                if (!reached and instruction.pc == pc) {
                    reached = true;
                    for (observations.items) |o| try read(hub, list.cog, o, &captured.writer);
                    if (list.profile == .program) break;
                }
            }
        }
        if (list.profile == .cog and hub.cogs[list.cog].registers.get(.PTRB) == dut_origin) dut_started = true;
        if (list.profile == .cog and dut_started and !reached and hub.cogs[list.cog].exec_mode == .cog) {
            if (hub.cogs[list.cog].pipeline[3]) |instruction| try validateDispatch(compiled.module, dut_origin, instruction);
        }
        hub.step();
        if (hub.next_idle_clock()) |next| hub.counter = @min(next, list.max_cycles);
        if (!supplied_stdin and stdout.written().len >= list.stdin_after.len) {
            if (!std.mem.startsWith(u8, stdout.written(), list.stdin_after)) return error.ReadinessMismatch;
            try hub.io.supplyInput(list.stdin, hub.counter);
            supplied_stdin = true;
        }
        if (hub.fault) |fault| {
            try ctx.errors.print("{s}: cog {d}, pc 0x{x}, {t}: {s}\n", .{ path, fault.cog, fault.pc, decode.decode(fault.instruction), fault.reason() });
            return switch (fault.result) {
                .not_implemented => error.NotImplementedInstruction,
                .illegal => error.IllegalInstruction,
                .trap => error.ExecutionTrap,
                else => unreachable,
            };
        }
        if (stdout.written().len > 1 << 20) return error.OutputLimit;
    }
    if (stop != null and !reached) return if (hub.is_any_cog_active()) error.CycleLimit else error.StopNotReached;
    if ((stop == null or list.profile == .cog) and hub.is_any_cog_active()) return error.CycleLimit;
    if (!supplied_stdin) return error.ReadinessNotReached;
    // A checkpoint freezes the cog; its last queued UART frames still finish.
    while (hub.io.txBusy() and hub.counter < list.max_cycles) {
        hub.io.step(hub);
        hub.counter += 1;
    }
    if (hub.io.txBusy()) return error.CycleLimit;
    if (stdout.written().len > 1 << 20) return error.OutputLimit;
    var ok = true;
    for (post.items) |assertion| {
        var actual: std.Io.Writer.Allocating = .init(allocator);
        defer actual.deinit();
        if (list.profile == .cog) {
            var offset: usize = 0;
            for (observations.items) |o| {
                if (o.kind == assertion.observation.kind and o.offset == assertion.observation.offset and o.len == assertion.observation.len) {
                    try actual.writer.writeAll(captured.written()[offset..][0..o.len]);
                    break;
                }
                offset += o.len;
            }
        } else try read(hub, list.cog, assertion.observation, &actual.writer);
        if (!std.mem.eql(u8, assertion.bytes, actual.written())) {
            try ctx.errors.print("{s}:{d}: postcondition {t}[0x{x}]: expected {x}, simulator {x}\n", .{ path, assertion.observation.line, assertion.observation.kind, assertion.observation.offset, assertion.bytes, actual.written() });
            ok = false;
        }
    }
    if (!std.mem.eql(u8, list.stdout, stdout.written())) {
        try ctx.errors.print("{s}: stdout: expected {x}, simulator {x}\n", .{ path, list.stdout, stdout.written() });
        ok = false;
    }
    if (!ok) return error.AssertionFailed;
    var simulated: std.Io.Writer.Allocating = .init(allocator);
    defer simulated.deinit();
    if (list.profile == .cog) try simulated.writer.writeAll(captured.written()) else for (observations.items) |o| try read(hub, list.cog, o, &simulated.writer);

    return oracle.run(.{
        .allocator = allocator,
        .io = ctx.io,
        .errors = ctx.errors,
        .endpoint = if (ctx.options.oracle) ctx.endpoint else "",
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
        .stop_reason = if (reached) "checkpoint" else "all cogs stopped",
        .trace = trace.written(),
    });
}

pub fn main(init: std.process.Init) !u8 {
    var cli = args.parseForCurrentProcess(Options, init, .print) catch return 1;
    defer cli.deinit();
    var stderr_buffer: [4096]u8 = undefined;
    var stderr = std.Io.File.stderr().writer(init.io, &stderr_buffer);
    defer stderr.interface.flush() catch {};
    if (cli.options.help) {
        try args.printHelp(Options, cli.executable_name orelse "windtunnel-tests", &stderr.interface);
        return 0;
    }
    const endpoint = init.environ_map.get("P2AAS_ENDPOINT") orelse "";
    if ((cli.options.oracle or cli.options.characterize) and !cli.options.@"prepare-oracle") {
        if (endpoint.len == 0) {
            try stderr.interface.writeAll("P2AAS_ENDPOINT is required for --oracle or --characterize\n");
            return 1;
        }
    }
    var paths: std.ArrayList([]const u8) = .empty;
    const allocator = init.arena.allocator();
    if (cli.positionals.len != 0) try paths.appendSlice(allocator, cli.positionals) else {
        var dir = try std.Io.Dir.cwd().openDir(init.io, "tests/windtunnel", .{ .iterate = true });
        defer dir.close(init.io);
        var walker = try dir.walk(allocator);
        defer walker.deinit();
        while (try walker.next(init.io)) |file| if (file.kind == .file and std.mem.endsWith(u8, file.path, ".propan")) {
            try paths.append(allocator, try std.fs.path.join(allocator, &.{ "tests/windtunnel", file.path }));
        };
    }
    if (paths.items.len == 0) return error.EmptySuite;
    std.mem.sort([]const u8, paths.items, {}, struct {
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
    var failed: usize = 0;
    var total_runs: usize = 0;
    var failed_runs: usize = 0;
    for (paths.items) |path| {
        const fixture_progress = progress.start(path, 0);
        defer fixture_progress.end();
        const source = try std.Io.Dir.cwd().readFileAlloc(init.io, path, allocator, .limited(1 << 20));
        var arena: std.heap.ArenaAllocator = .init(init.gpa);
        defer arena.deinit();
        const list = checklist.parse(arena.allocator(), path, source, &stderr.interface) catch |err| {
            try stderr.interface.print("FAIL {s}: {t}\n", .{ path, err });
            failed += 1;
            continue;
        };
        var fixture_failed = false;
        for (list.runs, 1..) |run, index| {
            total_runs += 1;
            const name = run.name orelse try std.fmt.allocPrint(arena.allocator(), "run {d}", .{index});
            const status = runOne(.{ .allocator = init.gpa, .io = init.io, .errors = &stderr.interface, .options = cli.options, .endpoint = endpoint }, path, source, try list.forRun(arena.allocator(), run), name) catch |err| {
                try stderr.interface.print("FAIL {s} [{s}]: {t}\n", .{ path, name, err });
                fixture_failed = true;
                failed_runs += 1;
                continue;
            };
            if (report_success) try stderr.interface.print("{s} {s} [{s}] (oracle {t})\n", .{ if (cli.options.characterize) "CAPTURE" else "PASS", path, name, status });
            try stderr.interface.flush();
        }
        if (fixture_failed) failed += 1;
    }
    if (report_success or failed != 0) try stderr.interface.print("{d} fixtures: {d} {s}, {d} failed; {d} runs: {d} {s}, {d} failed\n", .{ paths.items.len, paths.items.len - failed, if (cli.options.characterize) "captured" else "passed", failed, total_runs, total_runs - failed_runs, if (cli.options.characterize) "captured" else "passed", failed_runs });
    return if (failed == 0) 0 else 1;
}

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
    try std.testing.expectError(error.NotImplementedInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].c == false\n\nBITRND value, 1\nJMP nrel(_end)\nvar value: LONG 0\n"));
    try std.testing.expectError(error.NotImplementedInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nWXPIN 'x', 1\nCOGSTOP 0\n"));
    try std.testing.expectError(error.NotImplementedInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nWYPIN 1, 1\nCOGSTOP 0\n"));
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
    try std.testing.expectError(error.IllegalInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].c == false\n\nRFBYTE value\nJMP nrel(_end)\nvar value: LONG 0\n"));
    try std.testing.expect(std.mem.indexOf(u8, errors.written(), "RFBYTE requires RDFAST") != null);
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
        if (file.kind != .file) continue;
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
            const index: usize = if (std.mem.eql(u8, name, "first")) 0 else if (std.mem.eql(u8, name, "second")) 1 else return error.UnexpectedRunName;
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

/// Serve a bounded number of actual TCP connections so session cleanup is checked without a subprocess.
fn serveTestConnections(listener: *std.Io.net.Server, count: usize) !void {
    for (0..count) |_| {
        const stream = try listener.accept(std.testing.io);
        defer stream.close(std.testing.io);
        try @import("p2aas_server.zig").handleConnection(std.testing.allocator, std.testing.io, stream, false);
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
    const ip = try std.Io.net.IpAddress.parse("127.0.0.1", 0);
    var listener = try ip.listen(std.testing.io, .{});
    defer listener.deinit(std.testing.io);
    const endpoint = try std.fmt.allocPrint(std.testing.allocator, "ws://127.0.0.1:{d}/", .{listener.socket.address.getPort()});
    defer std.testing.allocator.free(endpoint);
    var server = try std.Io.concurrent(std.testing.io, serveTestConnections, .{ &listener, 3 });
    defer server.cancel(std.testing.io) catch {};
    for ([_][]const u8{ "\x00\x80\xff", "ABC" }) |packet| {
        var output: std.ArrayList(u8) = .empty;
        defer output.deinit(std.testing.allocator);
        try @import("p2aas.zig").run(std.testing.allocator, std.testing.io, endpoint, .{
            .image = compiled.image,
            .stdin = packet,
            .ready = "!",
            .baudrate = 115200,
            .timeout_ms = 1000,
        }, &output, &errors.writer);
        try std.testing.expectEqual(@as(usize, 4), output.items.len);
        try std.testing.expectEqualSlices(u8, packet, output.items[1..]);
    }
    // A simulator fault must finish the close handshake and release the board just like a transport failure.
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const illegal_source = ".cogexec\nJMP hub_entry\n.hubexec 0x1000\nhub_entry:\nRFBYTE PA\n";
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "illegal.propan", .data = illegal_source });
    const illegal_path = try tmp.dir.realPathFileAlloc(std.testing.io, "illegal.propan", std.testing.allocator);
    defer std.testing.allocator.free(illegal_path);
    var illegal = try propan.assemble(std.testing.allocator, std.testing.io, illegal_path, illegal_source, &errors.writer);
    defer illegal.deinit();
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);
    try std.testing.expectError(error.HardwareClosed, @import("p2aas.zig").run(std.testing.allocator, std.testing.io, endpoint, .{
        .image = illegal.image,
        .stdin = "",
        .ready = "",
        .baudrate = 115200,
        .timeout_ms = 1000,
    }, &output, &errors.writer));
    try std.testing.expect(std.mem.find(u8, errors.written(), "P2AAS close 1011:") != null);
    try server.await(std.testing.io);
}
