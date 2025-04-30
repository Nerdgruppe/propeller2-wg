const std = @import("std");
const builtin = @import("builtin");

const frontend = @import("frontend.zig");
const sema = @import("sema.zig");

const args_parser = @import("args");

pub const std_options: std.Options = .{
    .log_scope_levels = &.{
        // .{ .scope = .parser, .level = .info },
    },
};

const TestMode = enum {
    parser,
};

const CliArgs = struct {
    help: bool = false,
    output: []const u8 = "",
    @"test-mode": ?TestMode = null,

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
            .@"test-mode" = "<internal use only>",
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

    const loaded_files = try arena.allocator().alloc(frontend.ParsedFile, cli.positionals.len);
    for (cli.positionals, loaded_files, source_files, 0..) |input_path, *parsed_file, source_code, i| {
        errdefer for (loaded_files[0..i]) |*file|
            file.deinit();
        std.log.debug("parsing {s}...", .{input_path});

        var parser: frontend.Parser = .init(source_code, input_path);

        parsed_file.* = try parser.parse(allocator);
    }
    defer for (loaded_files) |*file|
        file.deinit();

    // Stop after having each file parsed successfully:
    if (cli.options.@"test-mode" == .parser)
        return 0;

    // try frontend.render.pretty_print(
    //     std.io.getStdOut().writer(),
    //     parsed_file.file,
    // );
    // try frontend.dump_ast(
    //     std.io.getStdOut().writer(),
    //     parsed_file.file,
    // );

    for (cli.positionals, loaded_files) |input_path, parsed_file| {
        std.log.debug("analyzing {s}...", .{input_path});

        var module = try sema.analyze(allocator, parsed_file.file);
        defer module.deinit();
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
