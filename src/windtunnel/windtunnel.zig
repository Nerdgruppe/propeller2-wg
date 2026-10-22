//! Reusable cycle-exact Propeller 2 simulator, debugger and capture types.

const std = @import("std");
const windtunnel = @This();
const p2 = @import("p2");
pub const types = p2.types;

const disassembly = @import("sim/disassemble.zig");
pub const disassemble = disassembly.disassemble;
pub const Frame = @import("sim/Frame.zig");
pub const Capture = @import("sim/Capture.zig");
pub const CaptureTime = @import("CaptureTime.zig");
pub const inspect = @import("sim/inspect.zig");
pub const Debugger = @import("sim/Debugger.zig");
pub const access = @import("sim/access.zig");
pub const Cordic = @import("sim/Cordic.zig");
pub const Streamer = @import("sim/Streamer.zig");
pub const ColorConverter = @import("sim/ColorConverter.zig");

pub const HubRam = @import("sim/HubRam.zig");
pub const Hub = @import("sim/Hub.zig");
pub const Cog = @import("sim/Cog.zig");
pub const IO = @import("sim/IO.zig");
pub const Pad = @import("sim/Pad.zig");
pub const Pads = @import("sim/Pads.zig");
pub const SystemConfiguration = @import("sim/SystemConfiguration.zig");
pub const ClockConfiguration = @import("sim/ClockConfiguration.zig");

pub const SmartPin = smart_pin.SmartPin;

pub const smart_pin = @import("sim/smart_pin.zig");

pub const serial_boot = @import("serial_boot.zig");

test "simulator module accepts explicit configuration and serializes a capture in memory" {
    inline for (.{ "main", "std_options", "SimulationProfile", "ClockOptions", "configureClocks", "saveCapture" }) |name| {
        try std.testing.expect(!@hasDecl(windtunnel, name));
    }

    const hub = try std.testing.allocator.create(windtunnel.Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration: windtunnel.SystemConfiguration = .ideal;
    configuration.clocks.seed = 42;
    configuration.clocks.rcfast = .{ .frequency = 20e6, .below = 0, .above = 0 };
    try hub.init(configuration);

    var capture = try windtunnel.Capture.init(
        std.testing.allocator,
        windtunnel.Capture.minimum_memory,
        .stop,
        hub.io.captureState(hub.io.pads),
    );
    defer capture.deinit();

    hub.attachCapture(&capture);
    defer hub.capture = null;

    try std.testing.expectEqual(windtunnel.Hub.StepResult.normal, hub.step());
    try std.testing.expectEqual(@as(u64, 1), hub.counter);
    try std.testing.expectEqual(@as(u64, 42), hub.system_configuration.clocks.seed);
    try std.testing.expect(capture.metadataConst().end > capture.metadataConst().start);

    var bytes: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer bytes.deinit();

    try capture.writeNative(&bytes.writer);
    var reader: std.Io.Reader = .fixed(bytes.written());
    var restored = try windtunnel.Capture.readNative(std.testing.allocator, &reader, bytes.written().len);
    defer restored.deinit();

    try std.testing.expectEqualDeep(capture.metadataConst().*, restored.metadataConst().*);
}
