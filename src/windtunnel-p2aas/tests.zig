//! Protocol and real-loopback regressions for the standalone P2AAS server.
const std = @import("std");
const windtunnel = @import("windtunnel");
const Hub = windtunnel.Hub;
const serial_boot = windtunnel.serial_boot;
const p2aas_server = @import("server.zig");
const p2aas = @import("p2aas-client");
const propan = @import("propan");

pub const std_options: std.Options = .{ .log_level = .err };

comptime {
    _ = p2aas_server;
}

/// Serve a bounded number of actual TCP connections so session cleanup is checked without a subprocess.
fn serveTestConnections(listener: *std.Io.net.Server, count: usize, configuration: Hub.SystemConfiguration) !void {
    for (0..count) |_| {
        const stream = try listener.accept(std.testing.io);
        defer stream.close(std.testing.io);

        try p2aas_server.handleConnection(std.testing.allocator, std.testing.io, stream, .{
            .fast_io = false,
            .system_configuration = configuration,
        });
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
