const std = @import("std");
const cases = @import("tests/propan/cases.zig");

const windtunnel_fixtures = [_][]const u8{
    "tests/windtunnel/behaviour/augs.propan",
    "tests/windtunnel/behaviour/cogstop.propan",
    "tests/windtunnel/program/echo.propan",
    "tests/windtunnel/program/hello-world.propan",
    "tests/windtunnel/state/augd.propan",
    "tests/windtunnel/state/augmentation.propan",
    "tests/windtunnel/state/augs.propan",
    "tests/windtunnel/state/cogid-7.propan",
    "tests/windtunnel/state/cogid.propan",
    "tests/windtunnel/state/conditions-00.propan",
    "tests/windtunnel/state/conditions-01.propan",
    "tests/windtunnel/state/conditions-10.propan",
    "tests/windtunnel/state/conditions-11.propan",
    "tests/windtunnel/state/full-cog.propan",
    "tests/windtunnel/state/mov-00-keep.propan",
    "tests/windtunnel/state/mov-00-wc.propan",
    "tests/windtunnel/state/mov-00-wcz.propan",
    "tests/windtunnel/state/mov-00-wz.propan",
    "tests/windtunnel/state/mov-01-keep.propan",
    "tests/windtunnel/state/mov-01-wc.propan",
    "tests/windtunnel/state/mov-01-wcz.propan",
    "tests/windtunnel/state/mov-01-wz.propan",
    "tests/windtunnel/state/mov-10-keep.propan",
    "tests/windtunnel/state/mov-10-wc.propan",
    "tests/windtunnel/state/mov-10-wcz.propan",
    "tests/windtunnel/state/mov-10-wz.propan",
    "tests/windtunnel/state/mov-11-keep.propan",
    "tests/windtunnel/state/mov-11-wc.propan",
    "tests/windtunnel/state/mov-11-wcz.propan",
    "tests/windtunnel/state/mov-11-wz.propan",
    "tests/windtunnel/state/mov-zero.propan",
    "tests/windtunnel/state/q.propan",
    "tests/windtunnel/state/reporter-block.propan",
    "tests/windtunnel/state/reporter-byte.propan",
    "tests/windtunnel/state/reporter.propan",
    "tests/windtunnel/state/return.propan",
    "tests/windtunnel/state/snapshot-registers.propan",
};

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
    const windtunnel_test_step = b.step("test-windtunnel", "Runs Windtunnel checklist fixtures and harness tests");

    // Options:
    const with_flexspin = b.option(bool, "with-flexspin", "Includes FlexSpin for Spin2 round-trip and assembler equivalence tests.") orelse false;
    const no_emit_bin = b.option(bool, "no-emit-bin", "Does not emit a binary, just compiles the applications") orelse false;
    const coverage = b.option(bool, "coverage", "Collect Propan coverage with kcov when available") orelse true;
    const with_p2aas = b.option(bool, "with-p2aas", "Run Windtunnel hardware oracle tests through P2AAS_ENDPOINT") orelse false;

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Dependencies:
    const serial_dep = b.dependency("serial", .{});

    const ptk_dep = b.dependency("ptk", .{});
    const args_dep = b.dependency("args", .{});

    const serial_mod = serial_dep.module("serial");
    const ptk_mod = ptk_dep.module("parser-toolkit");
    const args_mod = args_dep.module("args");

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

    const windtunnel_suite = b.createModule(.{
        .root_source_file = b.path("src/windtunnel/test_suite.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "args", .module = args_mod },
            .{ .name = "propan", .module = propan_mod },
            .{ .name = "oracle-template", .module = b.createModule(.{ .root_source_file = b.path("tests/windtunnel/oracle.propan.in") }) },
        },
    });
    const windtunnel_tests = b.addExecutable(.{ .name = "windtunnel-tests", .root_module = windtunnel_suite });
    fb.installArtifact(windtunnel_tests);
    const harness_tests = b.addTest(.{ .name = "windtunnel-harness-tests", .root_module = windtunnel_suite });
    const harness_run = b.addRunArtifact(harness_tests);
    harness_run.setCwd(b.path("."));
    windtunnel_test_step.dependOn(&harness_run.step);
    const fixture_run = b.addRunArtifact(windtunnel_tests);
    fixture_run.setCwd(b.path("."));
    fixture_run.expectExitCode(0); // Capture stdio so Zig forwards progress updates.
    if (with_p2aas) {
        fixture_run.addArg("--oracle");
        fixture_run.has_side_effects = true;
    }
    fixture_run.addFileInput(b.path("examples/hello-world.propan"));
    fixture_run.addFileInput(b.path("tests/windtunnel/oracle.propan.in"));
    fixture_run.addArgs(&windtunnel_fixtures);
    for (windtunnel_fixtures) |path| fixture_run.addFileInput(b.path(path));
    windtunnel_test_step.dependOn(&fixture_run.step);
    test_step.dependOn(windtunnel_test_step);

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
        const kcov_path = if (coverage) b.findProgram(&.{"kcov"}, &.{}) catch null else null;
        if (kcov_path) |kcov| {
            tests.setExecCmd(&.{
                kcov,
                "--clean",
                b.fmt("--include-path={s}", .{b.pathFromRoot("src")}),
                ".coverage",
                null, // addRunArtifact inserts the test executable here.
            });
        } else if (coverage) {
            std.log.warn("could not find kcov, not generating coverage", .{});
        }
        // The native runner reports fuzz-test discovery through Zig's server protocol.
        const run = b.addRunArtifact(tests);
        run.step.name = "run Propan test suite";
        // Coverage writes a report outside Zig's cache.
        run.has_side_effects = kcov_path != null;
        // Include imported sources, FILE payloads, and Spin2 references read at runtime.
        for ([_][]const []const u8{
            cases.parser_accept_tests,
            cases.sema_accept_tests,
            cases.parser_diagnostic_tests,
            cases.sema_diagnostic_tests,
            cases.compare_diagnostic_tests,
            cases.support_files,
        }) |paths| for (paths) |path| run.addFileInput(b.path(path));
        for (cases.formatter_tests) |fixture| run.addFileInput(b.path(fixture.input));
        run.setCwd(b.path("."));
        test_step.dependOn(&run.step);
    }
}
