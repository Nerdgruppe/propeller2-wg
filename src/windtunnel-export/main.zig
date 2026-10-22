//! Inspect native Scope block images and convert their retained traces to VCD.
const std = @import("std");
const args = @import("args");
const windtunnel = @import("windtunnel");
const Capture = windtunnel.Capture;
const CaptureTime = windtunnel.CaptureTime;
const Vcd = @import("Vcd.zig");

test {
    std.testing.refAllDecls(Vcd);
}

const Options = struct {
    help: bool = false,
    inspect: bool = false,
    vcd: ?[]const u8 = null,

    pub const shorthands = .{ .h = "help", .i = "inspect" };
    pub const meta = .{
        .usage_summary = "[--inspect] [--vcd PATH] DUMP.wtscope",
        .full_text =
        \\Inspect a Windtunnel Scope v1 block image or export its retained traces.
        \\Inspection is the default when --vcd is omitted. Use both options to do both.
        \\Use --vcd - for VCD on stdout; accompanying inspection goes to stderr.
        ,
        .option_docs = .{
            .help = "Print this help text",
            .inspect = "Show retained times, sample/clipping counts and blocks for recorded channels",
            .vcd = "Write retained digital, voltage, clipping and reset signals as VCD",
        },
    };
};

/// Parse the command line and report file or image errors without a runtime stack trace.
pub fn main(init: std.process.Init) !u8 {
    var cli = args.parseForCurrentProcess(Options, init, .print) catch return 1;
    defer cli.deinit();

    if (cli.options.help or cli.positionals.len != 1) {
        const file = if (cli.options.help)
            std.Io.File.stdout()
        else
            std.Io.File.stderr();
        var buffer: [4096]u8 = undefined;
        var output = file.writer(init.io, &buffer);
        try args.printHelp(Options, cli.executable_name orelse "windtunnel-export", &output.interface);
        try output.interface.flush();
        return if (cli.options.help)
            0
        else
            1;
    }
    run(init, cli.positionals[0], cli.options) catch |err| {
        std.log.err("{s}: {t}", .{ cli.positionals[0], err });
        return 1;
    };
    return 0;
}

/// Load and validate the complete image before creating any output, then perform the requested operations.
fn run(init: std.process.Init, path: []const u8, options: Options) !void {
    const file = try std.Io.Dir.cwd().openFile(init.io, path, .{});
    defer file.close(init.io);

    const stat = try file.stat(init.io);
    if (stat.kind != .file)
        return error.ExpectedRegularCaptureFile;
    var buffer: [4096]u8 = undefined;
    var reader = file.reader(init.io, &buffer);
    var capture = try Capture.readNative(init.gpa, &reader.interface, stat.size);
    defer capture.deinit();

    const vcd_stdout = if (options.vcd) |destination|
        std.mem.eql(u8, destination, "-")
    else
        false;
    if (options.inspect or options.vcd == null) {
        const report_file = if (vcd_stdout)
            std.Io.File.stderr()
        else
            std.Io.File.stdout();
        var output = report_file.writer(init.io, &buffer);
        try writeInspection(&capture, &output.interface);
        try output.interface.flush();
    }
    if (options.vcd) |destination| {
        if (vcd_stdout) {
            var output = std.Io.File.stdout().writer(init.io, &buffer);
            try Vcd.write(&capture, &output.interface);
        } else {
            try exportVcd(&capture, init, path, destination);
        }
    }
}

/// Replace a completed output atomically, preserving existing files on failed exports and guarding the input path.
fn exportVcd(capture: *const Capture, init: std.process.Init, source: []const u8, destination: []const u8) !void {
    const cwd = std.Io.Dir.cwd();
    const source_path = try cwd.realPathFileAlloc(init.io, source, init.gpa);
    defer init.gpa.free(source_path);

    const destination_path = cwd.realPathFileAlloc(init.io, destination, init.gpa) catch |err| switch (err) {
        error.FileNotFound => null,
        else => return err,
    };
    defer if (destination_path) |resolved| {
        init.gpa.free(resolved);
    };

    if (destination_path) |resolved| {
        if (std.mem.eql(u8, source_path, resolved))
            return error.OutputWouldReplaceCapture;
    }
    var output_file = try cwd.createFileAtomic(init.io, destination, .{ .replace = true });
    defer output_file.deinit(init.io);

    var buffer: [64 * 1024]u8 = undefined;
    var output = output_file.file.writer(init.io, &buffer);
    try Vcd.write(capture, &output.interface);
    try output_file.replace(init.io);
}

const InspectionRow = struct {
    pin: ?u6,
    kind: []const u8,
    stats: Capture.ChannelStatistics,

    /// Format a physical pin name or identify the reset-event stream.
    fn channelName(row: InspectionRow, buffer: *[5]u8) []const u8 {
        return if (row.pin) |pin|
            std.fmt.bufPrint(buffer, "P{d}", .{pin}) catch unreachable
        else
            "Reset";
    }
};

/// Print retained times, stored sample/clipping totals, and only channels with sample blocks.
fn writeInspection(capture: *const Capture, writer: *std.Io.Writer) !void {
    var rows: [129]InspectionRow = undefined;
    var count: usize = 0;
    for (0..64) |pin| {
        inline for (.{ Capture.DigitalSample, Capture.AnalogSample }) |Sample| {
            const channel = if (Sample == Capture.DigitalSample)
                capture.digitalChannelsConst()[pin]
            else
                capture.analogChannelsConst()[pin];
            const stats = capture.channelStatistics(Sample, channel);
            if (stats.blocks != 0) {
                rows[count] = .{
                    .pin = @intCast(pin),
                    .kind = if (Sample == Capture.DigitalSample)
                        "digital"
                    else
                        "analog",
                    .stats = stats,
                };
                count += 1;
            }
        }
    }
    const resets = capture.channelStatistics(Capture.ResetSample, capture.resetChannelConst().*);
    if (resets.blocks != 0) {
        rows[count] = .{ .pin = null, .kind = "events", .stats = resets };
        count += 1;
    }
    const header = capture.metadataConst();
    var time_buffers: [3][64]u8 = undefined;
    var times: [3][]const u8 = undefined;
    var time_width: usize = 0;
    for ([_]u128{ header.start, header.end, header.end - header.start }, &time_buffers, &times) |value, *buffer, *time| {
        time.* = try std.fmt.bufPrint(buffer, "{f}", .{CaptureTime{ .femtoseconds = value }});
        time_width = @max(time_width, time.len);
    }
    for ([_][]const u8{ "Start:", "End:", "Duration:" }, times) |label, time| {
        try writer.alignBuffer(label, 10, .left, ' ');
        try writer.alignBuffer(time, time_width, .right, ' ');
        try writer.writeByte('\n');
    }
    var samples: usize = 0;
    var clipped: usize = 0;
    for (rows[0..count]) |row| {
        samples += row.stats.samples;
        clipped += row.stats.clipped;
    }
    const count_width = @max(2, @max(std.fmt.count("{d}", .{samples}), std.fmt.count("{d}", .{clipped})));
    for ([_][]const u8{ "Samples:", "Clipped:" }, [_]usize{ samples, clipped }) |label, value| {
        try writer.alignBuffer(label, 10, .left, ' ');
        try writer.printInt(value, 10, .lower, .{ .width = count_width, .alignment = .right });
        try writer.writeByte('\n');
    }
    try writer.writeAll("Channels:\n  64 digital\n  64 analog\n   1 reset-event stream\n\n");
    try writeInspectionTable(writer, rows[0..count]);
}

/// Measure every column before rendering, keeping names left-aligned and numeric cells right-aligned.
fn writeInspectionTable(writer: *std.Io.Writer, rows: []const InspectionRow) !void {
    const headings = [_][]const u8{ "Channel", "Kind", "Samples", "Clipped", "Blocks" };
    var widths: [headings.len]usize = undefined;
    for (headings, &widths) |heading, *width| {
        width.* = heading.len;
    }
    for (rows) |row| {
        var name_buffer: [5]u8 = undefined;
        widths[0] = @max(widths[0], row.channelName(&name_buffer).len);
        widths[1] = @max(widths[1], row.kind.len);
        for ([_]usize{ row.stats.samples, row.stats.clipped, row.stats.blocks }, widths[2..]) |value, *width| {
            width.* = @max(width.*, std.fmt.count("{d}", .{value}));
        }
    }
    for (headings, widths, 0..) |heading, width, column| {
        if (column != 0) {
            try writer.writeByte(' ');
        }
        const alignment: std.fmt.Alignment = if (column < 2)
            .left
        else
            .right;
        try writer.alignBuffer(heading, width, alignment, ' ');
    }
    try writer.writeByte('\n');
    for (rows) |row| {
        var name_buffer: [5]u8 = undefined;
        try writer.alignBuffer(row.channelName(&name_buffer), widths[0], .left, ' ');
        try writer.writeByte(' ');
        try writer.alignBuffer(row.kind, widths[1], .left, ' ');
        for ([_]usize{ row.stats.samples, row.stats.clipped, row.stats.blocks }, widths[2..], 0..) |value, width, column| {
            try writer.writeByte(' ');
            if (column == 1 and std.mem.eql(u8, row.kind, "digital")) {
                try writer.splatByteAll(' ', width);
            } else {
                try writer.printInt(value, 10, .lower, .{ .width = width, .alignment = .right });
            }
        }
        try writer.writeByte('\n');
    }
}

test "inspection reports stored samples and hides blockless channels" {
    var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .rolling, .{
        .digital = @splat(.low),
        .analog = @splat(Capture.voltage(0)),
    });
    defer capture.deinit();

    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 10, .high));
    capture.metadata().start = 5;
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();

    try writeInspection(&capture, &output.writer);
    try std.testing.expectEqualStrings(
        "Start:    0.000005 ns\nEnd:      0.000010 ns\nDuration: 0.000005 ns\n" ++
            "Samples:   2\nClipped:   0\nChannels:\n  64 digital\n  64 analog\n   1 reset-event stream\n\n" ++
            "Channel Kind    Samples Clipped Blocks\nP0      digital       2              1\n",
        output.written(),
    );
}

test "inspection counts clipped analog records and actual reset events" {
    var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, .{
        .digital = @splat(.high),
        .analog = @splat(0),
    });
    defer capture.deinit();

    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 1, .low));
    try std.testing.expect(capture.recordAnalog(&capture.analogChannels()[0], 1, 65535));
    try std.testing.expect(capture.recordAnalog(&capture.analogChannels()[0], 2, Capture.voltage(0)));
    capture.reset(.firmware);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();

    try writeInspection(&capture, &output.writer);
    try std.testing.expect(std.mem.indexOf(u8, output.written(), "Samples:   6\nClipped:   2\n") != null);
    try std.testing.expect(std.mem.endsWith(u8, output.written(), "P0      digital       2              1\nP0      analog        3       2      1\nReset   events        1       0      1\n"));
}

test "inspection table widens numeric and text columns without truncation" {
    const rows = [_]InspectionRow{
        .{ .pin = 63, .kind = "digital", .stats = .{ .samples = 1_234_567_890, .clipped = 0, .blocks = 1 } },
        .{ .pin = null, .kind = "long-event-kind", .stats = .{ .samples = 2, .clipped = 123_456_789, .blocks = 123_456 } },
    };
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();

    try writeInspectionTable(&output.writer, &rows);
    try std.testing.expectEqualStrings(
        "Channel Kind               Samples   Clipped Blocks\n" ++
            "P63     digital         1234567890                1\n" ++
            "Reset   long-event-kind          2 123456789 123456\n",
        output.written(),
    );
}
