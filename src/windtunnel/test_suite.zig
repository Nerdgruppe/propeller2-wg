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
    @"prepare-oracle": bool = false,
    @"artifact-dir": []const u8 = ".zig-cache/windtunnel-artifacts",
    pub const shorthands = .{ .h = "help" };
    pub const meta = .{ .usage_summary = "[FILE.propan ...]", .option_docs = .{
        .help = "Show usage",
        .oracle = "Run the hardware oracle through P2AAS_ENDPOINT",
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
            .hub => @intCast(symbol.label.hub_address orelse return error.InvalidSymbolKind),
            else => return error.InvalidSymbolKind,
        };
    }
    return found orelse error.UnknownSymbol;
}

fn resolve(module: propan.Module, assignment: checklist.Assignment) !Resolved {
    const kind: Kind = switch (assignment.target) {
        .reg => .reg,
        .hub => .hub,
        .c => .c,
        .z => .z,
        .q => .q,
    };
    const offset = switch (assignment.target) {
        .reg, .hub => |a| try address(module, a, kind),
        .c, .z, .q => 0,
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
        if (line.pc) |pc| if (pc < 0x400) {
            if (pc == 0 or pc >= 504 or end > origin + 504 * 4 or line.offset != origin + pc * 4) return error.InvalidCogLayout;
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
    const allocator = arena.allocator();
    const list = try checklist.parse(allocator, path, source, ctx.errors);
    const oracle_ctx: oracle.Context = .{ .allocator = ctx.allocator, .io = ctx.io, .errors = ctx.errors };
    var compiled = if (list.profile == .cog) try oracle.assemble(oracle_ctx, path, list, &.{}, &.{}, 1, false) else try propan.assemble(ctx.allocator, ctx.io, path, source, ctx.errors);
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
        if (list.pre.len == 0 and list.post.len == 0) return error.MissingObservation;
    }
    for ([_][]const checklist.Assignment{ list.pre, list.post }, 0..) |assignments, group| {
        for (assignments) |assignment| {
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
    const hub = try allocator.create(Hub);
    hub.init();
    @memcpy(hub.memory[0..compiled.image.len], compiled.image);
    var stdout: std.Io.Writer.Allocating = .init(allocator);
    defer stdout.deinit();
    hub.output_writer = &stdout.writer;
    try hub.start_cog(0, .{});
    var captured: std.Io.Writer.Allocating = .init(allocator);
    defer captured.deinit();
    errdefer |err| {
        if (ctx.retain_failures and err != error.HardwareTransportFailed and err != error.OracleMismatch and err != error.InvalidOracleLength) {
            var snapshot: std.Io.Writer.Allocating = .init(allocator);
            defer snapshot.deinit();
            if (captured.written().len != 0) snapshot.writer.writeAll(captured.written()) catch {} else for (observations.items) |o| read(hub, list.cog, o, &snapshot.writer) catch {};
            oracle.retainLocalFailure(.{ .allocator = allocator, .io = ctx.io, .errors = ctx.errors, .artifacts = ctx.options.@"artifact-dir" }, path, source, compiled.image, pre.items, observations.items, snapshot.written(), stdout.written(), hub.counter, err) catch {};
        }
    }
    var reached = false;
    var dut_started = false;
    var supplied_stdin = list.stdin.len == 0;
    const started = std.Io.Timestamp.now(ctx.io, .awake).toNanoseconds();
    while (hub.is_any_cog_active() and hub.counter < list.max_cycles) {
        if (hub.counter & 1023 == 0 and std.Io.Timestamp.now(ctx.io, .awake).toNanoseconds() - started > 30 * std.time.ns_per_s) return error.SimulatorTimeout;
        if (stop) |pc| {
            if (hub.cogs[list.cog].current_instruction orelse hub.cogs[list.cog].next_instruction) |instruction| {
                if (!reached and instruction.pc == pc) {
                    reached = true;
                    for (observations.items) |o| try read(hub, list.cog, o, &captured.writer);
                    if (list.profile == .program) break;
                }
            }
        }
        if (list.profile == .cog and hub.cogs[list.cog].registers.get(.PTRB) == dut_origin) dut_started = true;
        if (list.profile == .cog and dut_started and !reached and hub.cogs[list.cog].exec_mode == .cog) {
            if (hub.cogs[list.cog].current_instruction orelse hub.cogs[list.cog].next_instruction) |instruction| try validateDispatch(compiled.module, dut_origin, instruction);
        }
        hub.step();
        // WAITX cannot make progress before its deadline. With no other cog or
        // pending UART event, advancing the counter preserves observable state.
        if (hub.cogs[0].wait_until) |deadline| {
            var alone = true;
            for (hub.cogs[1..]) |cog| if (cog.exec_mode != .stopped) {
                alone = false;
                break;
            };
            if (alone and !hub.io.txBusy() and hub.io.input_index == hub.io.input.len) hub.counter = @min(deadline, list.max_cycles);
        }
        if (!supplied_stdin and stdout.written().len >= list.stdin_after.len) {
            if (!std.mem.startsWith(u8, stdout.written(), list.stdin_after)) return error.ReadinessMismatch;
            try hub.io.supplyInput(list.stdin, hub.counter);
            supplied_stdin = true;
        }
        if (hub.fault) |fault| {
            try ctx.errors.print("{s}: cog {d}, pc 0x{x}, {t}: {s}\n", .{ path, fault.cog, fault.pc, decode.decode(fault.instruction), if (fault.unsupported) "unsupported instruction" else "execution trap" });
            return if (fault.unsupported) error.UnsupportedInstruction else error.ExecutionTrap;
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
    if (cli.options.oracle and !cli.options.@"prepare-oracle") {
        if (endpoint.len == 0) {
            try stderr.interface.writeAll("P2AAS_ENDPOINT is required when --oracle is enabled\n");
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
    for (paths.items) |path| {
        const fixture_progress = progress.start(path, 0);
        defer fixture_progress.end();
        const source = try std.Io.Dir.cwd().readFileAlloc(init.io, path, allocator, .limited(1 << 20));
        const status = runSource(.{ .allocator = init.gpa, .io = init.io, .errors = &stderr.interface, .options = cli.options, .endpoint = endpoint }, path, source) catch |err| {
            try stderr.interface.print("FAIL {s}: {t}\n", .{ path, err });
            failed += 1;
            continue;
        };
        if (report_success) try stderr.interface.print("PASS {s} (oracle {t})\n", .{ path, status });
    }
    if (report_success or failed != 0) try stderr.interface.print("{d} fixtures: {d} passed, {d} failed\n", .{ paths.items.len, paths.items.len - failed, failed });
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
    try std.testing.expectError(error.DuplicatePrecondition, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? pre: cog[0].c = true\n//? pre: cog[0].c = false\n" ++ code));
    try std.testing.expectError(error.UnknownSymbol, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].reg[missing] == 7\n" ++ code));
    try std.testing.expectError(error.InvalidChecklist, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? pre: cog[0].reg[1] = 0\n" ++ code));
    try std.testing.expectError(error.ReservedRegister, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].reg[0] == 7\n" ++ code));
    try std.testing.expectError(error.ReservedMemory, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: hub[_wt_reg_snapshot] == hex [00]\n" ++ code));
    try std.testing.expectError(error.MissingExitJump, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].c == false\n\nCOGSTOP 0\n"));
    try std.testing.expectError(error.MissingExitJump, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].c == false\n\nif(C) JMP nrel(_end)\n"));
    try std.testing.expectError(error.StopNotReached, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].c == false\n\nCOGSTOP 0\nJMP nrel(_end)\n"));
    try std.testing.expectError(error.UnsupportedInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? post: cog[0].c == false\n\nBITRND value, 1\nJMP nrel(_end)\nvar value: LONG 0\n"));
    try std.testing.expectError(error.UnsupportedInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nWXPIN 'x', 1\nCOGSTOP 0\n"));
    try std.testing.expectError(error.UnsupportedInstruction, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n\n.cogexec\nWYPIN 1, 1\nCOGSTOP 0\n"));
    try std.testing.expectError(error.ReadinessNotReached, runTemporary(ctx, "//? WINDTUNNEL CHECK LIST\n//? profile: program\n//? stdin: \"x\"\n//? stdin-after: \"READY\"\n//? stdout: \"READYx\"\n\n.cogexec\nCOGSTOP 0\n"));
}
