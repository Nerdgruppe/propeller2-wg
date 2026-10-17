//! Raw host terminal and independent physical UART input for the CLI.
const std = @import("std");
const Hub = @import("sim/Hub.zig");
const serial_boot = @import("serial_boot.zig");
const Terminal = @This();

io: std.Io,
allocator: std.mem.Allocator,
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
            try serial_boot.load(terminal.io, hub, image, session.rate);
        } else {
            try serial_boot.start(hub);
            // Give the ROM the same reset allowance as an upload, then calibrate its UART.
            for (0..hub.io.clock_frequency / 50) |clock| {
                if (clock % 256 == 0) {
                    try terminal.io.checkCancel();
                }
                if (hub.step() != .normal)
                    return error.BootInterrupted;
            }
            try host.write(hub, "> ");
        }
    } else {
        // Settle the attached host's idle mark before firmware can enable its receiver.
        _ = hub.step();
        try hub.start_cog(0, .{});
    }

    // One final edge aggregates stopped cogs' DIR/OUT and aborts their smart transfers.
    var final_edge = false;
    while (hub.is_any_cog_active() or !final_edge) {
        try terminal.io.checkCancel();
        if (try terminal.take()) |byte| {
            try host.write(hub, &.{byte});
        }
        const inactive = !hub.is_any_cog_active();
        for (0..256) |_| {
            switch (hub.step()) {
                .normal => {},
                .trap => {
                    if (hub.fault) |fault| {
                        std.log.err("cog {d}, pc 0x{x}, instruction 0x{x:0>8}: {s}", .{
                            fault.cog, fault.pc, fault.instruction, fault.reason(),
                        });
                    }
                    return 1;
                },
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
