const std = @import("std");

const args_parser = @import("args");
const serial_utils = @import("serial");

const p2_ram = 512 * 1024;
const p2_version = "Prop_Ver G";

pub fn main(init: std.process.Init) !u8 {
    var cli = args_parser.parseForCurrentProcess(CliArgs, init, .print) catch return 1;
    defer cli.deinit();

    // Validate CLI args:

    if (cli.options.help) {
        var buffer: [4096]u8 = undefined;
        var writer = std.Io.File.stdout().writer(init.io, &buffer);
        try args_parser.printHelp(CliArgs, cli.executable_name orelse "turboprop", &writer.interface);
        try writer.interface.flush();
        return 0;
    }

    const verbose = cli.options.verbose;

    if (cli.options.exec == .default and cli.positionals.len == 0) {
        std.log.err("A path to a binary file is required!", .{});
        return 1;
    }

    const maybe_load_file_path: ?[]const u8, const chainloader_argv: []const []const u8 = if (cli.options.exec == .default)
        .{ cli.positionals[0], cli.positionals[1..] }
    else
        .{ null, cli.positionals };

    if (cli.options.monitor and chainloader_argv.len > 0) {
        std.log.err("monitor and sub-command execution contradict each other!", .{});
        return 1;
    }

    // Load an application file if possible

    var load_file_buffer: [p2_ram + 4]u8 = undefined;
    const checksummed_file: []const u8 = if (maybe_load_file_path) |load_file_path| cs_file: {
        const load_file = if (std.mem.eql(u8, load_file_path, "-")) blk: {
            var reader = std.Io.File.stdin().readerStreaming(init.io, &.{});
            const len = try reader.interface.readSliceShort(&load_file_buffer);
            break :blk load_file_buffer[0..len];
        } else try std.Io.Dir.cwd().readFile(init.io, load_file_path, &load_file_buffer);
        std.debug.assert(load_file.len <= p2_ram);

        if ((load_file.len % 4) != 0) {
            std.log.err("\"{f}\" is not loadable: length not divisible by 4!", .{
                std.zig.fmtString(load_file_path),
            });
            return 1;
        }

        if (load_file.len == 0) {
            std.log.warn("\"{f}\" is empty!", .{
                std.zig.fmtString(load_file_path),
            });
        }

        const checksum: u32 = blk: {
            var cs: u32 = 0x706F7250; // "Prop"
            for (std.mem.bytesAsSlice(u32, load_file)) |item| {
                const word = std.mem.littleToNative(u32, item);
                cs -%= word;
            }
            break :blk cs;
        };
        if (verbose) std.log.debug("file checksum is 0x{X:0>8}", .{checksum});

        // append checksum to buffer:
        const checksummed_file: []const u8 = blk: {
            var data = load_file;
            const end = data.len;
            data.len += 4;
            std.mem.writeInt(u32, data[end..][0..4], checksum, .little);
            break :blk data;
        };

        break :cs_file checksummed_file;
    } else "";

    // Open and configure serial port:

    var port = try std.Io.Dir.cwd().openFile(init.io, cli.options.port, .{ .mode = .read_write });
    defer port.close(init.io);

    try serial_utils.configureSerialPort(port, .{
        .baud_rate = cli.options.baudrate,
        .parity = .none,
        .stop_bits = .one,
        .word_size = .eight,
    });

    // Reset device if necessary:

    if (cli.options.reset != .none) {
        if (verbose) std.debug.print("resetting...\n", .{});
        try serial_utils.changeControlPins(port, .{
            .dtr = optional_value(cli.options.reset == .dtr, true),
            .rts = optional_value(cli.options.reset == .rts, true),
        });
        try init.io.sleep(.fromMilliseconds(5), .awake);
        try serial_utils.changeControlPins(port, .{
            .dtr = optional_value(cli.options.reset == .dtr, false),
            .rts = optional_value(cli.options.reset == .rts, false),
        });
        try init.io.sleep(.fromMilliseconds(20), .awake);
    }

    {
        var fileWriter = port.writer(init.io, &.{});
        const writer = &fileWriter.interface;

        // perform auto-baud configuration:
        try writer.writeAll("> ");
        try writer.flush();

        // version check:
        if (!cli.options.@"no-version-check") {
            if (verbose) std.log.info("check version...", .{});
            var magic_buf: [256]u8 = undefined;

            try writer.writeAll("Prop_Chk 0 0 0 0\r");
            try writer.flush();

            // response will be [ CR, LF, "Prop_Ver G", CR, LF]
            var buffer: [256]u8 = undefined;
            var reader = port.readerStreaming(init.io, &buffer);

            _ = try reader.interface.discardDelimiterInclusive('\n');

            var fbs: std.Io.Writer = .fixed(&magic_buf);

            _ = try reader.interface.streamDelimiter(&fbs, '\n');

            const magic = std.mem.trim(u8, fbs.buffered(), " \r\n");

            if (!std.mem.eql(u8, magic, p2_version)) {
                std.log.warn("Device identifies as \"{f}\", but epxected \"{f}\"", .{
                    std.zig.fmtString(magic),
                    std.zig.fmtString(p2_version),
                });
            }
        }

        switch (cli.options.exec) {
            .default => {
                if (verbose) std.log.info("load code...", .{});

                var b64_buffer: [std.base64.standard.Encoder.calcSize(load_file_buffer.len)]u8 = undefined;
                const b64_data = std.base64.standard.Encoder.encode(&b64_buffer, checksummed_file);

                const chunk_size: usize = cli.options.@"chunk-size";

                try writer.writeAll("Prop_Txt 0 0 0 0 ");
                try writer.flush();
                {
                    var rest: []const u8 = b64_data;
                    while (rest.len > 0) {
                        const chunk = rest[0..@min(rest.len, chunk_size)];

                        try writer.writeAll(chunk);
                        try writer.flush();

                        rest = rest[chunk.len..];

                        if (rest.len > 0) {
                            // Resynchronize after each transferred chunk:
                            try writer.writeAll("\r> ");
                            try writer.flush();
                        }
                    }
                }

                try writer.writeAll(" ?\r");
                try writer.flush();

                var response_buf: [1]u8 = undefined;
                var reader = port.readerStreaming(init.io, &response_buf);

                const response = try reader.interface.takeByte();
                switch (response) {
                    '.' => {},
                    '!' => {
                        std.log.err("invalid checksum!", .{});
                        return 1;
                    },
                    else => {
                        std.log.err("unexpected response from Prop_Txt: 0x{X:0>8}!", .{response});
                        return 1;
                    },
                }

                if (verbose) std.log.info("code fully loaded.", .{});
            },

            .monitor => {
                if (verbose) std.log.info("starting monitor...", .{});
                try writer.writeByte(std.ascii.control_code.eot); // "Ctrl-D" starts the monitor
                try writer.flush();
            },

            .taqoz => {
                if (verbose) std.log.info("starting taqoz...", .{});
                try writer.writeByte(std.ascii.control_code.esc); // "ESC" starts the monitor
                try writer.flush();
            },
        }
    }

    if (chainloader_argv.len > 0) {
        @panic("subcommand execution is not supported yet!");
    }

    if (cli.options.monitor) {
        var writer_buf: [256]u8 = undefined;
        var fwriter = std.Io.File.stdout().writer(init.io, &writer_buf);

        const writer = &fwriter.interface;

        switch (cli.options.@"monitor-format") {
            .escaped => {
                while (true) {
                    var buffer: [64]u8 = undefined;
                    const len = try port.readStreaming(init.io, &.{&buffer});

                    try writer.print("{f}\n", .{
                        std.zig.fmtString(buffer[0..len]),
                    });
                    try writer.flush();
                }
            },
            .raw => {
                while (true) {
                    var buffer: [64]u8 = undefined;
                    const len = try port.readStreaming(init.io, &.{&buffer});

                    try writer.writeAll(buffer[0..len]);
                    try writer.flush();
                }
            },
        }
    }

    return 0;
}

const CliArgs = struct {
    help: bool = false,

    monitor: bool = false,
    @"monitor-format": enum { escaped, raw } = .escaped,

    port: []const u8 = "/dev/ttyUSB0",
    baudrate: u32 = 115200,

    reset: ResetKind = .dtr,
    exec: ExecMode = .default,
    @"chunk-size": usize = 32,

    @"no-version-check": bool = false,

    verbose: bool = false,

    pub const shorthands = .{
        .h = "help",
        .P = "port",
        .m = "monitor",
        .b = "baudrate",
        .v = "verbose",
    };

    pub const meta = .{
        .usage_summary = "summary",

        .full_text = "full",

        .option_docs = .{
            .help = "Prints this help",
            .port = "Selects which serial port is used.",
            .monitor = "If enabled, will start a simple monitor which prints what is sent over the serial port.",
            .@"monitor-format" = "The format that the monitor will use for printing. 'raw' outputs data as received, 'escaped' prints zig-style escapes for values.",
            .@"chunk-size" = "Defines the size of ",
            .reset = "Selects how to reset the device before starting",
            .baudrate = "Selects the baud rate used for loading",
            .@"no-version-check" = "Disables checking if the device is actually the expected Propeller 2",
            .exec = "Sets what should be started by turboprop as a chainloader.",
            .verbose = "If given, prints more debug logs",
        },
    };
};

const ResetKind = enum {
    none,
    dtr,
    rts,
};

const ExecMode = enum {
    default,
    monitor,
    taqoz,
};

fn optional_value(enabled: bool, value: anytype) ?@TypeOf(value) {
    return if (enabled)
        value
    else
        null;
}
