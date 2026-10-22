//! Raw host terminal and independent physical UART input for the CLI.
const std = @import("std");
const windtunnel = @import("windtunnel");
const Hub = windtunnel.Hub;
const Issue = Hub.Issue;
const Capture = windtunnel.Capture;
const smart_pin = windtunnel.smart_pin;
const serial_boot = windtunnel.serial_boot;
const Terminal = @This();

io: std.Io,
allocator: std.mem.Allocator,
output: *std.Io.Writer,
sink: *const smart_pin.DataSink,
capture: ?*const Capture,
input: std.ArrayList(u8) = .empty,
input_position: usize = 0,
input_mutex: std.Io.Mutex = .init,

/// Release buffered keystrokes after both terminal tasks have stopped.
pub fn deinit(terminal: *Terminal) void {
    terminal.input.deinit(terminal.allocator);
}

/// Buffer input without blocking the reader behind an upload, keeping menu commands readable.
fn put(terminal: *Terminal, byte: u8) !void {
    try terminal.input_mutex.lock(terminal.io);
    defer terminal.input_mutex.unlock(terminal.io);

    try terminal.input.append(terminal.allocator, byte);
}

/// Take one queued byte, occasionally compacting consumed input during long pastes.
fn take(terminal: *Terminal) !?u8 {
    try terminal.input_mutex.lock(terminal.io);
    defer terminal.input_mutex.unlock(terminal.io);

    if (terminal.input_position == terminal.input.items.len) {
        terminal.input.clearRetainingCapacity();
        terminal.input_position = 0;
        return null;
    }
    const byte = terminal.input.items[terminal.input_position];
    terminal.input_position += 1;
    if (terminal.input_position >= 65536 and terminal.input_position >= terminal.input.items.len / 2) {
        const remaining = terminal.input.items[terminal.input_position..];
        std.mem.copyForwards(u8, terminal.input.items, remaining);
        terminal.input.items.len = remaining.len;
        terminal.input_position = 0;
    }
    return byte;
}

pub const help =
    "\r\nWindtunnel terminal commands:\r\n" ++
    "  Ctrl-A, Ctrl-A  Send 0x01 to the P2\r\n" ++
    "  Ctrl-A, Ctrl-H  Show this menu\r\n" ++
    "  Ctrl-A, Ctrl-Q  Quit\r\n" ++
    "ROM session: Esc opens TAQOZ; Ctrl-D opens the monitor.\r\n";

pub const InputEnd = enum { quit, eof };

pub const Parser = struct {
    menu: bool = false,

    pub const Action = union(enum) { wait, send: u8, help, quit };

    /// Consume only menu commands; all other bytes, including control characters, reach the P2.
    pub fn accept(parser: *Parser, byte: u8) Action {
        if (parser.menu) {
            parser.menu = false;
            return switch (byte) {
                0x08 => .help,
                0x11 => .quit,
                else => .{ .send = byte },
            };
        }
        if (byte == 0x01) {
            parser.menu = true;
            return .wait;
        }
        return .{ .send = byte };
    }
};

/// POSIX terminal attributes belong to the TTY, so restoring stdin restores its output flags too.
pub const Raw = struct {
    original: ?std.posix.termios,

    /// Disable line editing, echo, signals, CR/LF conversion and software flow control on a TTY.
    pub fn init() !Raw {
        const original = std.posix.tcgetattr(std.Io.File.stdin().handle) catch |err| switch (err) {
            error.NotATerminal => return .{ .original = null },
            else => return err,
        };
        var raw = original;
        raw.iflag.BRKINT = false;
        raw.iflag.IGNBRK = false;
        raw.iflag.PARMRK = false;
        raw.iflag.ISTRIP = false;
        raw.iflag.INLCR = false;
        raw.iflag.IGNCR = false;
        raw.iflag.ICRNL = false;
        raw.iflag.IXON = false;
        raw.iflag.IXOFF = false;
        raw.oflag.OPOST = false;
        raw.lflag.ECHO = false;
        raw.lflag.ECHONL = false;
        raw.lflag.ICANON = false;
        raw.lflag.ISIG = false;
        raw.lflag.IEXTEN = false;
        raw.cflag.PARENB = false;
        raw.cflag.CSIZE = .CS8;
        raw.cc[@intFromEnum(std.posix.V.MIN)] = 1;
        raw.cc[@intFromEnum(std.posix.V.TIME)] = 0;
        try std.posix.tcsetattr(std.Io.File.stdin().handle, .NOW, raw);
        return .{ .original = original };
    }

    /// Restore the exact saved terminal state on normal exit, faults and cooperative cancellation.
    pub fn deinit(raw: Raw) void {
        if (raw.original) |original| {
            std.posix.tcsetattr(std.Io.File.stdin().handle, .NOW, original) catch {};
        }
    }
};

/// Read uncooked stdin concurrently so menu commands remain responsive during ROM downloads.
pub fn read(terminal: *Terminal) anyerror!InputEnd {
    var parser: Parser = .{};
    var buffer: [256]u8 = undefined;
    var menu_buffer: [512]u8 = undefined;
    var menu = std.Io.File.stderr().writer(terminal.io, &menu_buffer);
    while (true) {
        const count = try std.Io.File.stdin().readStreaming(terminal.io, &.{&buffer});
        if (count == 0)
            return .eof;
        for (buffer[0..count]) |byte| {
            switch (parser.accept(byte)) {
                .wait => {},
                .send => |value| try terminal.put(value),
                .help => {
                    try menu.interface.writeAll(help);
                    try menu.interface.flush();
                },
                .quit => return .quit,
            }
        }
    }
}

/// Print short warnings in the same stream and ordering as the simulated UART output.
fn warning(writer: *std.Io.Writer, issue: Issue) !void {
    try writer.writeByte('[');
    switch (issue.kind) {
        .pc_domain => |value| {
            try writer.print("PC outside execution domain ${X}", .{value.pc});
        },
        .cog_fetch_write => |value| {
            try writer.print("cog fetch/write at ${X}", .{value.reader.address});
        },
        .lut_read_write => |value| {
            try writer.print("LUT read/write at ${X}", .{value.reader.address});
        },
        .reg_write_conflict => |value| {
            try writer.print("register write conflict at ${X}", .{value.first.address});
        },
        .lut_write_conflict => |value| {
            try writer.print("LUT write conflict at ${X}", .{value.first.address});
        },
        .pin_command_collision => |value| {
            try writer.print("pin {d} command collision", .{value.pin});
        },
        .pin_buffer_overwrite => |value| {
            try writer.print("pin {d} transmit overwrite", .{value.pin});
        },
        .usb_receive_error => |value| {
            try writer.print("pin {d} USB receive error", .{value.pin});
        },
        .smart_dac_clipped => |value| {
            try writer.print("pin {d} DAC saturation", .{value.pin});
        },
        .streamer_starvation => {
            try writer.writeAll("streamer FIFO empty");
        },
        .streamer_capture_dropped => {
            try writer.writeAll("streamer capture dropped");
        },
        .streamer_capture_displaced => {
            try writer.writeAll("streamer capture displaced");
        },
        .fifo_configuration_lost => {
            try writer.writeAll("pending FIFO configuration replaced");
        },
        .cog_start_replaced => |value| {
            try writer.print("pending cog {d} start replaced", .{value.target});
        },
        .memory_read_race => |value| {
            try writer.print("split read race at ${x}", .{value.address});
        },
        .memory_hole_read => |value| {
            try writer.print("hub hole read at ${X}", .{value.address});
        },
        .memory_hole_write => |value| {
            try writer.print("hub hole write at ${X}", .{value.address});
        },
        .pad_undervoltage => |value| {
            try writer.print("pin {d} undervoltage", .{value.pin});
        },
        .pad_overvoltage => |value| {
            try writer.print("pin {d} overvoltage", .{value.pin});
        },
        .pad_overcurrent => |value| {
            try writer.print("pin {d} overcurrent", .{value.pin});
        },
        .unsupported_hubset => |value| {
            try writer.print("unsupported HUBSET ${X:0>8}", .{value});
        },
        .uncharacterized_pll => {
            try writer.writeAll("uncharacterized PLL configuration");
        },
        .unsafe_postdivider => {
            try writer.writeAll("unsafe PLL postdivider change");
        },
        .invalid_encoding,
        .hub_fifo_conflict,
        .fifo_read_unconfigured,
        .fifo_write_unconfigured,
        .streamer_fifo_mismatch,
        .xbyte_fifo_unconfigured,
        .hub_skipf,
        .streamer_mode,
        .streamer_fifo_lost,
        .memory_protected_read,
        .memory_protected_write,
        => unreachable, // This renderer is only used for warning-severity issues.
    }
    try writer.writeByte(']');
}

/// Apply the CLI's policy after every edge, including edges advanced by the serial host.
/// The CLI checks owned adapters and capture capacity; traps take precedence over a full capture.
pub fn afterStep(terminal: *Terminal, hub: *const Hub) !void {
    if (terminal.sink.failure) |err|
        return err;
    var trapped = false;
    for (0..hub.issues.len) |issue_index| {
        const issue = hub.issues.at(issue_index);
        switch (issue.severity()) {
            .warning => {
                try warning(terminal.output, issue);
            },
            .trap => {
                try terminal.output.print("\r\n{f}\r\n", .{issue});
                trapped = true;
            },
        }
    }
    if (hub.issues.len != 0) {
        try terminal.output.flush();
    }
    if (trapped)
        return error.SimulationTrap;
    if (terminal.capture) |capture| {
        if (capture.metadataConst().status == .full)
            return error.CaptureFull;
    }
}

pub const Session = struct {
    boot: bool,
    image: ?[]const u8,
    rate: u32,
};

/// Run the chosen startup path and serialize queued terminal bytes onto physical P63.
pub fn simulate(terminal: *Terminal, hub: *Hub, session: Session) anyerror!u8 {
    var host: serial_boot.Host = .{ .io = terminal.io, .rate = session.rate };
    try hub.io.setExternal(@enumFromInt(63), .{
        .voltage = hub.system_configuration.vio_voltage,
        .resistance = 1,
    });
    if (session.boot) {
        if (session.image) |image| {
            try serial_boot.load(terminal.io, hub, image, session.rate, terminal);
        } else {
            try serial_boot.start(hub);
            // Give the ROM the same reset allowance as an upload, then calibrate its UART.
            const settle_clocks: u64 = @intFromFloat(hub.io.clock.frequency() / 50);
            for (0..settle_clocks) |clock| {
                if (clock % 256 == 0) {
                    try terminal.io.checkCancel();
                }
                const notification = hub.step();
                try terminal.afterStep(hub);
                if (notification != .normal)
                    return error.BootInterrupted;
            }
            try host.write(hub, "> ", terminal);
        }
    } else {
        // Settle the attached host's idle mark before firmware can enable its receiver.
        _ = hub.step();
        try terminal.afterStep(hub);
        hub.start_cog(0, .{});
    }

    // One final edge aggregates stopped cogs' DIR/OUT and aborts their smart transfers.
    var final_edge = false;
    while (hub.is_any_cog_active() or !final_edge) {
        try terminal.io.checkCancel();
        if (try terminal.take()) |byte| {
            try host.write(hub, &.{byte}, terminal);
        }
        const inactive = !hub.is_any_cog_active();
        for (0..256) |_| {
            const notification = hub.step();
            try terminal.afterStep(hub);
            switch (notification) {
                .normal => {},
                .breakpoint, .reset => return 0,
            }
            if (!hub.is_any_cog_active())
                break;
        }
        final_edge = inactive;
    }
    return 0;
}

test "terminal control prefix leaves P2 input bytes uncooked" {
    var parser: Parser = .{};
    for ([_]u8{ 0x00, 0x03, 0x04, 0x08, 0x0a, 0x0d, 0x11, 0x13, 0x1b, 0x7f, 0xff }) |byte| {
        try std.testing.expectEqualDeep(Parser.Action{ .send = byte }, parser.accept(byte));
    }
    try std.testing.expectEqualDeep(Parser.Action.wait, parser.accept(0x01));
    try std.testing.expectEqualDeep(Parser.Action.help, parser.accept(0x08));
    try std.testing.expectEqualDeep(Parser.Action.wait, parser.accept(0x01));
    try std.testing.expectEqualDeep(Parser.Action{ .send = 0x01 }, parser.accept(0x01));
    try std.testing.expectEqualDeep(Parser.Action.wait, parser.accept(0x01));
    try std.testing.expectEqualDeep(Parser.Action.quit, parser.accept(0x11));
    try std.testing.expectEqualDeep(Parser.Action.wait, parser.accept(0x01));
    try std.testing.expectEqualDeep(Parser.Action{ .send = 'x' }, parser.accept('x'));
}

test "terminal settling warnings remain inline and the application runs to completion" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    try hub.io.setExternal(@enumFromInt(0), .{ .voltage = 5, .resistance = 1000 });
    var buffer: [65536]u8 = undefined;
    var output = std.Io.Writer.fixed(&buffer);
    var sink: smart_pin.DataSink = .{ .endpoint = .{ .writer = &output } };
    var terminal: Terminal = .{
        .io = std.testing.io,
        .allocator = std.testing.allocator,
        .output = &output,
        .sink = &sink,
        .capture = null,
    };
    defer terminal.deinit();

    hub.write_memory(0, 0xfd64_0003, 4, false); // COGSTOP #0.
    const result = try terminal.simulate(hub, .{ .boot = false, .image = null, .rate = 2_000_000 });
    try std.testing.expectEqual(@as(u8, 0), result);
    try std.testing.expect(hub.counter > 500);
    try std.testing.expect(std.mem.startsWith(u8, output.buffered(), "[pin 0 overvoltage]"));
    try std.testing.expect(std.mem.indexOfScalar(u8, output.buffered(), '\n') == null);
    try std.testing.expectEqual(@as(usize, 1), hub.issues.len);
    try std.testing.expectEqual(@as(u6, 0), hub.issues.at(0).kind.pad_overvoltage.pin);
    try std.testing.expect(hub.cogs[0].exec_mode == .stopped);
}

test "terminal warnings are inline traps start a line and only selected adapters are checked" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var buffer: [4096]u8 = undefined;
    var output = std.Io.Writer.fixed(&buffer);
    var sink: smart_pin.DataSink = .{ .endpoint = .{ .writer = &output } };
    var ignored: smart_pin.DataSink = .{ .endpoint = .discard, .failure = error.WriteFailed };
    hub.io.pins[0].smart.tx_mode = .{ .virtual = &ignored };
    var terminal: Terminal = .{
        .io = std.testing.io,
        .allocator = std.testing.allocator,
        .output = &output,
        .sink = &sink,
        .capture = null,
    };
    defer terminal.deinit();

    hub.report(.{ .unsupported_hubset = 0x3000_0000 }, .{});
    try terminal.afterStep(hub);
    try std.testing.expectEqualStrings("[unsupported HUBSET $30000000]", output.buffered());
    hub.issues.clear();
    var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, hub.io.captureState(hub.io.pads));
    defer capture.deinit();

    capture.metadata().status = .full;
    terminal.capture = &capture;
    hub.report(.invalid_encoding, .{ .cog = 0, .pc = 42, .instruction = 123 });
    try std.testing.expectError(error.SimulationTrap, terminal.afterStep(hub));
    try std.testing.expect(std.mem.indexOf(u8, output.buffered(), "]\r\nclock ") != null);
    try std.testing.expect(std.mem.endsWith(u8, output.buffered(), "\r\n"));
    hub.issues.clear();
    try std.testing.expectError(error.CaptureFull, terminal.afterStep(hub));
    sink.failure = error.WriteFailed;
    try std.testing.expectError(error.WriteFailed, terminal.afterStep(hub));
}
