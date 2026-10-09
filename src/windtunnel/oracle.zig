const std = @import("std");
const propan = @import("propan");
const checklist = @import("check_list.zig");
const p2aas = @import("p2aas.zig");

pub const Kind = enum { reg, hub, c, z, q };
pub const Observation = struct { kind: Kind, offset: u32 = 0, len: u32, line: u32 };
pub const Resolved = struct { observation: Observation, bytes: []const u8 };
pub const Status = enum { off, skipped, prepared, captured, passed };
pub const Context = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    errors: *std.Io.Writer,
    endpoint: []const u8 = "",
    artifacts: []const u8 = ".zig-cache/windtunnel-artifacts",
    prepare_only: bool = false,
    characterize: bool = false,
};
pub const Case = struct {
    path: []const u8,
    run_name: []const u8,
    source: []const u8,
    list: checklist.List,
    entry: u20,
    seeds: []const Resolved,
    assertions: []const Resolved,
    observations: []const Observation,
    simulated: []const u8,
    stdout: []const u8,
    original_image: []const u8,
    cycles: u64,
    stop_reason: []const u8,
    trace: []const u8 = "",
};

fn evidencePath(ctx: Context, path: []const u8, run_name: []const u8) ![]const u8 {
    const stamp = std.Io.Timestamp.now(ctx.io, .real).toNanoseconds();
    return std.fmt.allocPrint(ctx.allocator, "{s}/{s}-{x}-{x}-{d}", .{ ctx.artifacts, std.fs.path.basename(path), std.hash.Wyhash.hash(0, path), std.hash.Wyhash.hash(0, run_name), stamp });
}

pub fn retainLocalFailure(ctx: Context, path: []const u8, run_name: []const u8, source: []const u8, image: []const u8, seeds: []const Resolved, observations: []const Observation, simulated: []const u8, stdout: []const u8, cycles: u64, trace: []const u8, reason: anyerror) !void {
    const directory = try evidencePath(ctx, path, run_name);
    const dir = try std.Io.Dir.cwd().createDirPathOpen(ctx.io, directory, .{});
    defer dir.close(ctx.io);
    try dir.writeFile(ctx.io, .{ .sub_path = "fixture.propan", .data = source });
    try dir.writeFile(ctx.io, .{ .sub_path = "original.bin", .data = image });
    if (trace.len != 0) try dir.writeFile(ctx.io, .{ .sub_path = "pipeline.txt", .data = trace });
    var metadata: std.Io.Writer.Allocating = .init(ctx.allocator);
    defer metadata.deinit();
    try std.json.Stringify.value(.{ .run = run_name, .seeds = seeds, .observations = observations, .simulated = simulated, .stdout = stdout, .cycles = cycles, .stop_reason = reason }, .{}, &metadata.writer);
    try dir.writeFile(ctx.io, .{ .sub_path = "simulation.json", .data = metadata.written() });
    try ctx.errors.print("{s} [{s}]: simulator evidence retained in {s}\n", .{ path, run_name, directory });
}

/// Patch emitted bytes only, using this assembly's symbol addresses.
pub fn patchImage(module: propan.Module, image: []u8, assignments: []const checklist.Assignment, path: []const u8, errors: *std.Io.Writer) !void {
    for (assignments) |assignment| {
        if (assignment.target != .sym) continue;
        patchSymbol(module, image, assignment.target.sym, assignment.bytes) catch |err| {
            try errors.print("{s}:{d}: patch sym[{s}]: {t}\n", .{ path, assignment.line, assignment.target.sym, err });
            return err;
        };
    }
}

fn patchSymbol(module: propan.Module, image: []u8, name: []const u8, bytes: []const u8) !void {
    var found: ?u32 = null;
    for (module.symbols) |symbol| {
        if (!std.mem.eql(u8, symbol.name, name)) continue;
        if (found != null) return error.AmbiguousSymbol;
        found = symbol.label.hub_address orelse return error.InvalidSymbolKind;
    }
    const offset = found orelse return error.UnknownSymbol;
    if (bytes.len == 0) return error.EmptyPatch;
    if (reservedMemory(module, offset, bytes.len)) return error.ReservedMemory;
    if (offset > image.len or bytes.len > image.len - offset) return error.PatchOutsideImage;
    for (offset..offset + bytes.len) |index| {
        var defined = false;
        for (module.segments) |segment| {
            if (index >= segment.hub_offset and index < segment.hub_offset + segment.data.len) {
                defined = true;
                break;
            }
        }
        if (!defined) return error.UndefinedMemory;
    }
    @memcpy(image[offset..][0..bytes.len], bytes);
}

pub const snapshot_length = 506 * 4 + 4 + 2 + 2;
pub const snapshot_count = 506 + 3;

pub fn payloadLength(observations: []const Observation) usize {
    var length: usize = snapshot_length;
    for (observations) |o| if (o.kind == .hub) {
        length += o.len;
    };
    return length;
}

pub fn observationCount(observations: []const Observation) usize {
    var count: usize = snapshot_count;
    for (observations) |o| if (o.kind == .hub) {
        count += 1;
    };
    return count;
}

pub fn isScaffold(source: ?[]const u8) bool {
    return if (source) |path| std.mem.endsWith(u8, path, "/oracle.propan.in") or std.mem.eql(u8, path, "tests/windtunnel/oracle.propan.in") else false;
}

/// Instrumentation occupies its emitted ranges, rather than fixed RAM windows.
pub fn reservedMemory(module: propan.Module, offset: u32, len: usize) bool {
    if (@as(u64, offset) + len > 512 * 1024) return true;
    for (module.symbols) |symbol| if (std.mem.eql(u8, symbol.name, "_wt_start")) {
        if (offset < symbol.label.hub_address.?) return true;
        break;
    };
    for (module.line_data) |line| {
        if (isScaffold(line.location.source) and offset < @as(u64, line.offset) + line.length and line.offset < @as(u64, offset) + len) return true;
    }
    return false;
}

/// Both backends assemble the same template and import the fixture normally.
pub fn assemble(ctx: Context, path: []const u8, list: checklist.List, seeds: []const Resolved, observations: []const Observation, entry: u20, hardware: bool) !propan.AssemblyResult {
    const allocator = ctx.allocator;
    const payload_length = payloadLength(observations);
    if (payload_length + 20 > 1 << 20) return error.ReporterCapacity;
    var source: std.Io.Writer.Allocating = .init(allocator);
    defer source.deinit();
    var memory: std.Io.Writer.Allocating = .init(allocator);
    defer memory.deinit();
    var initial_c = false;
    var initial_z = false;
    var initial_q: u32 = 0;
    for (seeds) |seed| switch (seed.observation.kind) {
        .c => initial_c = seed.bytes[0] != 0,
        .z => initial_z = seed.bytes[0] != 0,
        .q => initial_q = std.mem.readInt(u32, seed.bytes[0..4], .little),
        .reg, .hub => return error.InvalidPrecondition,
    };
    for (observations) |o| if (o.kind == .hub) try memory.writer.print("    LONG {d}, {d}\n", .{ o.offset, o.len });
    for (list.constants) |constant| try source.writer.print("const {s} = {d}\n", .{ constant.name, constant.value });
    try source.writer.print("const _wt_baudrate = {d}\nconst _wt_target_cog = {d}\nconst _wt_is_oracle = {d}\nconst _wt_test_entry = {d}\n", .{ list.baudrate, list.cog, @intFromBool(hardware), entry });
    try source.writer.print("const _wt_observation_count = {d}\nconst _wt_payload_length = {d}\nconst _wt_extra_length = {d}\nconst _wt_memory_count = {d}\n", .{ observationCount(observations), payload_length, payload_length - snapshot_length, observationCount(observations) - snapshot_count });
    try source.writer.print("const _wt_initial_c = #{s}\nconst _wt_initial_z = #{s}\nconst _wt_initial_q = 0x{x}\n", .{ if (initial_c) "SET" else "CLR", if (initial_z) "SET" else "CLR", initial_q });
    const absolute_path = try std.Io.Dir.cwd().realPathFileAlloc(ctx.io, path, allocator);
    defer allocator.free(absolute_path);
    var import_path: std.Io.Writer.Allocating = .init(allocator);
    defer import_path.deinit();
    try import_path.writer.print("{f}", .{std.zig.fmtString(absolute_path)});
    const imported = try std.mem.replaceOwned(u8, allocator, @embedFile("oracle-template"), "{{ TEST_FILE }}", import_path.written());
    defer allocator.free(imported);
    const template = try std.mem.replaceOwned(u8, allocator, imported, "// {{MEMORY}}", memory.written());
    defer allocator.free(template);
    try source.writer.writeAll(template);
    var result = try propan.assemble(allocator, ctx.io, "tests/windtunnel/oracle.propan.in", source.written(), ctx.errors);
    errdefer result.deinit();
    for (observations) |o| if (o.kind == .hub and reservedMemory(result.module, o.offset, o.len)) return error.ReservedMemory;
    return result;
}

fn same(a: Observation, b: Observation) bool {
    return a.kind == b.kind and a.offset == b.offset and a.len == b.len;
}

pub fn run(ctx: Context, case: Case) !Status {
    if (case.list.oracle_off != null) return .off;
    if (ctx.endpoint.len == 0 and !ctx.prepare_only) return .skipped;
    var prepared: ?propan.AssemblyResult = if (case.list.profile == .cog) try assemble(ctx, case.path, case.list, case.seeds, case.observations, case.entry, true) else null;
    defer if (prepared) |*image| image.deinit();
    if (prepared) |*p| try patchImage(p.module, p.image, case.list.pre, case.path, ctx.errors);
    const image = if (prepared) |p| p.image else case.original_image;
    const allocator = ctx.allocator;
    const directory = try evidencePath(ctx, case.path, case.run_name);
    const dir = try std.Io.Dir.cwd().createDirPathOpen(ctx.io, directory, .{});
    defer dir.close(ctx.io);
    try dir.writeFile(ctx.io, .{ .sub_path = "fixture.propan", .data = case.source });
    try dir.writeFile(ctx.io, .{ .sub_path = "original.bin", .data = case.original_image });
    if (case.trace.len != 0) try dir.writeFile(ctx.io, .{ .sub_path = "pipeline.txt", .data = case.trace });
    try dir.writeFile(ctx.io, .{ .sub_path = "uploaded.bin", .data = image });
    if (prepared) |p| if (p.module.sources.len > 0) try dir.writeFile(ctx.io, .{ .sub_path = "oracle.propan", .data = p.module.sources[0].text });
    var metadata: std.Io.Writer.Allocating = .init(allocator);
    defer metadata.deinit();
    try std.json.Stringify.value(.{ .run = case.run_name, .cog = case.list.cog, .constants = case.list.constants, .patches = case.list.pre, .seeds = case.seeds, .observations = case.observations, .simulated = case.simulated, .stdout = case.stdout, .cycles = case.cycles, .stop_reason = case.stop_reason }, .{}, &metadata.writer);
    try dir.writeFile(ctx.io, .{ .sub_path = "simulation.json", .data = metadata.written() });
    const request: p2aas.Request = .{
        .image = image,
        .stdin = case.list.stdin,
        .ready = case.list.stdin_after,
        .baudrate = case.list.baudrate,
        .timeout_ms = case.list.timeout_ms,
        .observation_count = if (case.list.profile == .cog) observationCount(case.observations) else 0,
        .payload_length = if (case.list.profile == .cog) payloadLength(case.observations) else 0,
    };
    var request_json: std.Io.Writer.Allocating = .init(allocator);
    defer request_json.deinit();
    try std.json.Stringify.value(.{
        .image_length = image.len,
        .stdin = request.stdin,
        .ready = request.ready,
        .profile = case.list.profile,
        .baudrate = request.baudrate,
        .timeout_ms = request.timeout_ms,
        .observation_count = request.observation_count,
        .payload_length = request.payload_length,
    }, .{}, &request_json.writer);
    try dir.writeFile(ctx.io, .{ .sub_path = "request.json", .data = request_json.written() });
    if (ctx.prepare_only) {
        try ctx.errors.print("{s} [{s}]: oracle image prepared in {s}\n", .{ case.path, case.run_name, directory });
        return .prepared;
    }
    errdefer ctx.errors.print("{s} [{s}]: oracle evidence retained in {s}\n", .{ case.path, case.run_name, directory }) catch {};
    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);
    var transport: std.Io.Writer.Allocating = .init(allocator);
    defer transport.deinit();
    errdefer {
        dir.writeFile(ctx.io, .{ .sub_path = "hardware-output.bin", .data = output.items }) catch {};
        dir.writeFile(ctx.io, .{ .sub_path = "transport.txt", .data = transport.written() }) catch {};
    }
    p2aas.run(allocator, ctx.io, ctx.endpoint, request, &output, &transport.writer) catch |err| {
        try transport.writer.print("Transport failed: {t}\n", .{err});
        try ctx.errors.print("{s}: {s}", .{ case.path, transport.written() });
        return err;
    };
    const actual_stdout = output.items;
    if (ctx.characterize) {
        // Validate the complete device frame before retaining measured state.
        var measured: std.Io.Writer.Allocating = .init(allocator);
        defer measured.deinit();
        if (case.list.profile == .cog) {
            const actual = try p2aas.observations(actual_stdout, request);
            var registers: [506]u32 = undefined;
            for (&registers, 0..) |*value, i| value.* = std.mem.readInt(u32, actual[i * 4 ..][0..4], .little);
            try std.json.Stringify.value(.{
                .run = case.run_name,
                .cog = case.list.cog,
                .registers = registers,
                .q = std.mem.readInt(u32, actual[506 * 4 ..][0..4], .little),
                .c = actual[506 * 4 + 4] != 0,
                .z = actual[506 * 4 + 6] != 0,
                .extra = actual[snapshot_length..],
            }, .{}, &measured.writer);
        } else try std.json.Stringify.value(.{ .run = case.run_name, .stdout = actual_stdout }, .{}, &measured.writer);
        try dir.writeFile(ctx.io, .{ .sub_path = "hardware.json", .data = measured.written() });
        try dir.writeFile(ctx.io, .{ .sub_path = "hardware-output.bin", .data = output.items });
        try dir.writeFile(ctx.io, .{ .sub_path = "transport.txt", .data = transport.written() });
        try ctx.errors.print("{s} [{s}]: hardware measurements captured in {s}\n", .{ case.path, case.run_name, directory });
        return .captured;
    }
    if (case.list.profile == .program) {
        if (!std.mem.eql(u8, actual_stdout, case.list.stdout) or !std.mem.eql(u8, actual_stdout, case.stdout)) {
            try ctx.errors.print("{s}: hardware stdout mismatch: expected {x}, simulator {x}, hardware {x}\n", .{ case.path, case.list.stdout, case.stdout, actual_stdout });
            return error.OracleMismatch;
        }
    } else {
        const actual = try p2aas.observations(actual_stdout, request);
        if (actual.len != payloadLength(case.observations)) return error.InvalidOracleLength;
        var offset: usize = 0;
        var memory_offset: usize = snapshot_length;
        var ok = true;
        for (case.observations) |o| {
            const hardware = switch (o.kind) {
                .reg => actual[o.offset * 4 ..][0..4],
                .q => actual[506 * 4 ..][0..4],
                .c => actual[506 * 4 + 4 ..][0..1],
                .z => actual[506 * 4 + 6 ..][0..1],
                .hub => blk: {
                    const bytes = actual[memory_offset..][0..o.len];
                    memory_offset += o.len;
                    break :blk bytes;
                },
            };
            const simulated = case.simulated[offset..][0..o.len];
            for (case.assertions) |assertion| if (same(o, assertion.observation) and !std.mem.eql(u8, hardware, assertion.bytes)) {
                try ctx.errors.print("{s}:{d}: hardware postcondition {t}[0x{x}]: expected {x}, simulator {x}, hardware {x}\n", .{ case.path, assertion.observation.line, o.kind, o.offset, assertion.bytes, simulated, hardware });
                ok = false;
            };
            if (!std.mem.eql(u8, hardware, simulated)) {
                try ctx.errors.print("{s}:{d}: oracle {t}[0x{x}]: simulator {x}, hardware {x}\n", .{ case.path, o.line, o.kind, o.offset, simulated, hardware });
                ok = false;
            }
            offset += o.len;
        }
        if (!ok) return error.OracleMismatch;
    }
    try std.Io.Dir.cwd().deleteTree(ctx.io, directory);
    return .passed;
}
