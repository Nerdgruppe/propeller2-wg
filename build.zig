const std = @import("std");
const cases = @import("tests/propan/cases.zig");

const FastBuild = struct {
    b: *std.Build,
    no_emit_bin: bool,

    fn installArtifact(fb: FastBuild, step: *std.Build.Step.Compile) void {
        if (fb.no_emit_bin) {
            fb.b.getInstallStep().dependOn(&step.step);
        } else {
            fb.b.installArtifact(step);
        }
    }
};

pub fn build(b: *std.Build) !void {
    // Steps:
    const run_step = b.step("run", "Runs propan");
    const test_step = b.step("test", "Runs the test suite");

    // Options:
    const with_flexspin = b.option(bool, "with-flexspin", "Includes FlexSpin for Spin2 round-trip and assembler equivalence tests.") orelse false;
    const no_emit_bin = b.option(bool, "no-emit-bin", "Does not emit a binary, just compiles the applications") orelse false;

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Dependencies:
    const serial_dep = b.dependency("serial", .{});

    const ptk_dep = b.dependency("ptk", .{});
    const args_dep = b.dependency("args", .{});

    const serial_mod = serial_dep.module("serial");
    const ptk_mod = ptk_dep.module("parser-toolkit");
    const args_mod = args_dep.module("args");
    const lsp_mod = b.dependency("lsp_kit", .{ .target = target, .optimize = optimize }).module("lsp");

    const fb: FastBuild = .{
        .b = b,
        .no_emit_bin = no_emit_bin,
    };

    // Build:

    const turboprop_mod = b.addModule("turboprop", .{
        .root_source_file = b.path("src/turboprop/turboprop.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "args", .module = args_mod },
            .{ .name = "serial", .module = serial_mod },
        },
    });

    const propan_mod = b.addModule("propan", .{
        .root_source_file = b.path("src/propan/propan.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "ptk", .module = ptk_mod },
            .{ .name = "args", .module = args_mod },
        },
    });

    const windtunnel_mod = b.addModule("windtunnel", .{
        .root_source_file = b.path("src/windtunnel/windtunnel.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "args", .module = args_mod },
        },
    });

    const propan_lsp_mod = b.createModule(.{
        .root_source_file = b.path("src/propan-lsp/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "propan", .module = propan_mod },
            .{ .name = "lsp", .module = lsp_mod },
        },
    });
    fb.installArtifact(b.addExecutable(.{ .name = "propan-lsp", .root_module = propan_lsp_mod }));

    {
        const exe = b.addExecutable(.{
            .name = "turboprop",
            .root_module = turboprop_mod,
        });
        fb.installArtifact(exe);
    }

    const propan_exe = blk: {
        const exe = b.addExecutable(.{
            .name = "propan",
            .root_module = propan_mod,
            .use_llvm = true,
        });

        fb.installArtifact(exe);

        break :blk exe;
    };

    const windtunnel_exe = blk: {
        const exe = b.addExecutable(.{
            .name = "windtunnel",
            .root_module = windtunnel_mod,
        });

        fb.installArtifact(exe);

        break :blk exe;
    };

    _ = windtunnel_exe;

    // "zig build run"
    {
        const run_cmd = b.addRunArtifact(propan_exe);

        run_cmd.step.dependOn(b.getInstallStep());

        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        run_step.dependOn(&run_cmd.step);
    }

    // One process runs unit tests and all fixture categories.
    {
        const fuzz_corpus_files = b.addWriteFiles();

        var fuzz_corpus_index: std.Io.Writer.Allocating = .init(b.allocator);
        defer fuzz_corpus_index.deinit();

        fuzz_corpus_index.writer.writeAll(
            \\pub const files: []const []const u8 = &.{
            \\
        ) catch @panic("oom");

        for (cases.parser_accept_tests) |path| {
            const filename = std.fs.path.basename(path);

            _ = fuzz_corpus_files.addCopyFile(b.path(path), filename);

            fuzz_corpus_index.writer.print(
                \\    @embedFile("{f}"),
                \\
            ,
                .{std.zig.fmtString(filename)},
            ) catch @panic("oom");
        }

        fuzz_corpus_index.writer.writeAll(
            \\};
            \\
        ) catch @panic("oom");

        const fuzz_corpus_file = fuzz_corpus_files.add("corpus.zig", fuzz_corpus_index.written());

        const fuzz_corpus_mod = b.createModule(.{ .root_source_file = fuzz_corpus_file });

        propan_mod.addImport("fuzz-corpus", fuzz_corpus_mod);

        propan_mod.addImport("test-cases", b.createModule(.{ .root_source_file = b.path("tests/propan/cases.zig") }));
        const options = b.addOptions();
        if (with_flexspin) {
            const dep = b.lazyDependency("p2devsuite", .{}) orelse return;
            const flexspin = dep.artifact("flexspin");
            fb.installArtifact(flexspin);
            fb.installArtifact(dep.artifact("loadp2"));
            options.addOptionPath("flexspin", flexspin.getEmittedBin());
        } else {
            options.addOption(?[]const u8, "flexspin", null);
        }
        propan_mod.addOptions("test-options", options);
        const tests = b.addTest(.{
            .name = "propan-tests",
            .root_module = propan_mod,
            .use_llvm = true,
        });
        const install = b.addInstallArtifact(tests, .{});
        test_step.dependOn(&install.step);
        const kcov_path = b.findProgram(&.{"kcov"}, &.{}) catch null;
        if (kcov_path) |kcov| {
            tests.setExecCmd(&.{
                kcov,
                "--clean",
                b.fmt("--include-path={s}", .{b.pathFromRoot("src")}),
                ".coverage",
                null, // addRunArtifact inserts the test executable here.
            });
        } else {
            std.log.warn("could not find kcov, not generating coverage", .{});
        }
        // The native runner reports fuzz-test discovery through Zig's server protocol.
        const run = b.addRunArtifact(tests);
        run.step.name = "run Propan test suite";
        // Coverage writes a report outside Zig's cache.
        run.has_side_effects = kcov_path != null;
        // Include imported sources, FILE payloads, and Spin2 references read at runtime.
        var fixture_paths: std.ArrayList([]const u8) = .empty;
        for ([_][]const u8{ "tests/propan", "examples" }) |path| {
            var dir = try b.build_root.handle.openDir(b.graph.io, path, .{ .iterate = true });
            defer dir.close(b.graph.io);
            var walker = try dir.walk(b.allocator);
            defer walker.deinit();
            while (try walker.next(b.graph.io)) |entry| {
                if (entry.kind == .file)
                    try fixture_paths.append(b.allocator, b.fmt("{s}/{s}", .{ path, entry.path }));
            }
        }
        std.mem.sort([]const u8, fixture_paths.items, {}, struct {
            fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                return std.mem.lessThan(u8, lhs, rhs);
            }
        }.lessThan);
        for (fixture_paths.items) |path| run.addFileInput(b.path(path));
        run.setCwd(b.path("."));
        test_step.dependOn(&run.step);
    }
}
