const std = @import("std");
const builtin = @import("builtin");

const frontend = @import("frontend/parser.zig");

const args_parser = @import("args");

const CliArgs = struct {
    help: bool = false,
    output: []const u8 = "",

    pub const shorthands = .{
        .h = "help",
        .o = "output",
    };

    pub const meta = .{
        .usage_summary = "[-h] [-o <output>] <sources...>",

        .full_text =
        \\Propan is an assembler for the Propeller 2 architecture.
        ,

        .option_docs = .{
            .help = "Prints this help text",
            .output = "Sets the path of the output file.",
        },
    };
};

pub fn main() !u8 {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

    const allocator = if (builtin.mode == .Debug)
        debug_allocator.allocator()
    else
        std.heap.smp_allocator;

    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();

    var cli = args_parser.parseForCurrentProcess(CliArgs, allocator, .print) catch return 1;
    defer cli.deinit();

    if (cli.options.help) {
        try args_parser.printHelp(
            CliArgs,
            cli.executable_name orelse "propan",
            std.io.getStdOut().writer(),
        );
        return 0;
    }

    const source_files = try arena.allocator().alloc([]const u8, cli.positionals.len);
    for (source_files, cli.positionals) |*buffer, input_path| {
        std.log.debug("loading {s}...", .{input_path});
        buffer.* = try std.fs.cwd().readFileAlloc(arena.allocator(), input_path, 1 << 20);
    }

    for (cli.positionals, source_files) |input_path, source_code| {
        std.log.debug("parsing {s}...", .{input_path});

        // var tokenizer: frontend.Tokenizer = .init(source_code, input_path);

        // while (try tokenizer.next()) |item| {
        //     std.debug.print("{s: >10} | '{}'\n", .{ @tagName(item.type), std.zig.fmtEscapes(item.text) });
        // }

        var parser: frontend.Parser = .init(source_code, input_path);

        var parsed_file = try parser.parse(allocator);
        defer parsed_file.deinit();
    }

    return 0;
}

fn usage_mistake(comptime fmt: []const u8, args: anytype) !noreturn {
    try std.io.getStdErr().print("usage error: " ++ fmt ++ "\n", args);
    std.process.exit(1);
}

test {
    _ = frontend;
}
