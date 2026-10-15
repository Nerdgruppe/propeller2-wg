//! Reproducible electrical-kernel and complete Hub.step timings, with setup excluded.
const std = @import("std");
const Hub = @import("sim/Hub.zig");
const Pad = @import("sim/Pad.zig");
const Pads = @import("sim/Pads.zig");
const Reference = @import("sim/pad_reference.zig");
const p2 = @import("p2");
const signal = @import("sim/signal.zig");
const Workload = enum { idle, one, many, eight, rc, limited, mixed };
const Kernel = enum { hub, scalar, scalar_small_sort, simd, simd_uniform };

/// Copy the public source into the independent reference's descriptor type.
fn referenceSource(source: Pad.Source) Reference.Source {
    return .{
        .voltage = source.voltage,
        .resistance = source.resistance,
        .max_current = source.max_current,
        .drive_enabled = source.drive_enabled,
        .capacitance = source.capacitance,
    };
}

/// Select a deterministic drive pattern, with no dependence on simulated inputs.
fn driver(configuration: *const Hub.SystemConfiguration, workload: Workload, index: usize, clock: usize) Pad.Driver {
    if (workload == .idle or (workload == .one and index != 0))
        return .float;
    const mode: p2.types.PinConfiguration.Drive = switch (workload) {
        .rc => .resistor_15k,
        .limited => .current_100ua,
        .mixed => switch (index % 4) {
            0 => .fast,
            1 => .resistor_15k,
            2 => .current_100ua,
            else => .float,
        },
        else => .fast,
    };
    const level: signal.Signal = if ((clock / 8) % 2 == 0)
        .high
    else
        .low;
    return Pad.internalDriver(configuration, .{ .high_drive = mode, .low_drive = mode }, level, true);
}

/// Prepare external loading without charging or redistributing the initial voltage.
fn externalLoad(workload: Workload) Pad.Source {
    var result: Pad.Source = .float;
    if (workload == .rc or workload == .limited or workload == .mixed) {
        result.capacitance = 95e-12;
    }
    return result;
}

/// Run just the electrical pass, including internal-driver collection, for one kernel variant.
fn electrical(io: std.Io, configuration: *const Hub.SystemConfiguration, workload: Workload, steps: usize, comptime kernel: Kernel) !i96 {
    const allocator = std.heap.page_allocator;
    const bank = try allocator.create(Pads);
    defer allocator.destroy(bank);
    const scalar = try allocator.create([64]Reference);
    defer allocator.destroy(scalar);

    bank.init(configuration);
    scalar.* = @splat(.{});
    const external = externalLoad(workload);
    for (0..64) |index| {
        bank.setExternal(index, external);
    }
    var started: i96 = 0;
    for (0..steps + 5000) |clock| {
        if (clock == 5000) {
            std.mem.doNotOptimizeAway(bank);
            std.mem.doNotOptimizeAway(scalar);
            started = std.Io.Timestamp.now(io, .awake).toNanoseconds();
        }
        for (0..64) |index| {
            const internal = driver(configuration, workload, index, clock);
            if (kernel == .scalar or kernel == .scalar_small_sort) {
                const converted: Reference.Driver = switch (internal) {
                    .fast => |level| .{ .fast = level },
                    .linear => |linear| .{ .linear = referenceSource(linear) },
                };
                _ = scalar[index].advance(configuration, converted, referenceSource(external), 50e-9, kernel == .scalar_small_sort);
            } else {
                bank.setInternal(index, internal);
            }
        }
        if (kernel == .simd or kernel == .simd_uniform) {
            bank.advance(configuration, 50e-9, kernel == .simd);
        }
    }
    std.mem.doNotOptimizeAway(bank);
    std.mem.doNotOptimizeAway(scalar);
    return std.Io.Timestamp.now(io, .awake).toNanoseconds() - started;
}

/// Run real GPIO instruction loops and time completed Hub clocks only.
fn hubSteps(io: std.Io, configuration: Hub.SystemConfiguration, workload: Workload, steps: usize) !i96 {
    const hub = try std.heap.page_allocator.create(Hub);
    defer std.heap.page_allocator.destroy(hub);

    try hub.init(configuration);
    if (workload != .idle) {
        // DIRH P0; loop: DRVNOT P0; JMP loop.
        const one = [_]u32{ 0xfd640041, 0xfd64005f, 0xfd9ffff8 };
        // NOT DIRA; NOT DIRB; loop: NOT OUTA; NOT OUTB; JMP loop.
        const many = [_]u32{ 0xf623f5fa, 0xf623f7fb, 0xf623f9fc, 0xf623fbfd, 0xfd9ffff4 };
        const image: []const u32 = if (workload == .one)
            &one
        else
            &many;
        for (image, 0..) |word, index| {
            std.mem.writeInt(u32, hub.memory[index * 4 ..][0..4], word, .little);
        }
        const count: usize = if (workload == .eight)
            8
        else
            1;
        for (0..count) |index| {
            try hub.start_cog(@intCast(index), .{ .clocked = false });
        }
        for (0..64) |index| {
            var pin = hub.io.pins[index].configuration;
            const mode: p2.types.PinConfiguration.Drive = switch (workload) {
                .rc => .resistor_15k,
                .limited => .current_100ua,
                .mixed => switch (index % 4) {
                    0 => .fast,
                    1 => .resistor_15k,
                    2 => .current_100ua,
                    else => .float,
                },
                else => .fast,
            };
            pin.high_drive = mode;
            pin.low_drive = mode;
            hub.io.editConfiguration(@enumFromInt(index), @bitCast(pin));
            try hub.io.setExternal(@enumFromInt(index), externalLoad(workload));
        }
    }
    for (0..5000) |_| {
        if (hub.step() != .normal)
            return error.SimulationFailed;
    }
    std.mem.doNotOptimizeAway(hub);
    const start = std.Io.Timestamp.now(io, .awake).toNanoseconds();
    for (0..steps) |_| {
        if (hub.step() != .normal)
            return error.SimulationFailed;
    }
    std.mem.doNotOptimizeAway(hub);
    return std.Io.Timestamp.now(io, .awake).toNanoseconds() - start;
}

/// Print machine-readable timing; repeat randomized paired invocations for meaningful medians.
pub fn main(init: std.process.Init) !void {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 5)
        return error.ExpectedProfileWorkloadKernelSteps;
    const configuration: Hub.SystemConfiguration = if (std.mem.eql(u8, args[1], "ideal"))
        .ideal
    else if (std.mem.eql(u8, args[1], "realistic"))
        .realistic
    else
        return error.InvalidProfile;
    const workload = std.meta.stringToEnum(Workload, args[2]) orelse return error.InvalidWorkload;
    const kernel = std.meta.stringToEnum(Kernel, args[3]) orelse return error.InvalidKernel;
    const steps = try std.fmt.parseInt(usize, args[4], 10);
    if (steps == 0)
        return error.InvalidSteps;
    const elapsed = switch (kernel) {
        .hub => try hubSteps(init.io, configuration, workload, steps),
        .scalar => try electrical(init.io, &configuration, workload, steps, .scalar),
        .scalar_small_sort => try electrical(init.io, &configuration, workload, steps, .scalar_small_sort),
        .simd => try electrical(init.io, &configuration, workload, steps, .simd),
        .simd_uniform => try electrical(init.io, &configuration, workload, steps, .simd_uniform),
    };
    var buffer: [512]u8 = undefined;
    var output = std.Io.File.stdout().writer(init.io, &buffer);
    try output.interface.print("{s},{t},{t},{d},{d},{d:.3}\n", .{
        args[1], workload, kernel, steps, elapsed, @as(f64, @floatFromInt(elapsed)) / @as(f64, @floatFromInt(steps)),
    });
    try output.interface.flush();
}
