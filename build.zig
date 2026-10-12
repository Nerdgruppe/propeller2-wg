const std = @import("std");
const cases = @import("tests/propan/cases.zig");

const windtunnel_fixtures = [_][]const u8{
    "tests/windtunnel/state/smart-pin-hardware.propan",
    "tests/windtunnel/state/smart-pin-fields.propan",
    "tests/windtunnel/state/pin-families.propan",
    "tests/windtunnel/state/test-pin-flags.propan",
    "tests/windtunnel/behaviour/augs.propan",
    "tests/windtunnel/behaviour/cogstop.propan",
    "tests/windtunnel/behaviour/core-execution.propan",
    "tests/windtunnel/behaviour/stack-flags.propan",
    "tests/windtunnel/behaviour/stack.propan",
    "tests/windtunnel/program/echo.propan",
    "tests/windtunnel/program/server-terminal.propan",
    "tests/windtunnel/program/server-loader.propan",
    "tests/windtunnel/program/hello-world.propan",
    "tests/windtunnel/program/dir-register-uart.propan",
    "tests/windtunnel/program/uart-checkpoint.propan",
    "tests/windtunnel/program/debug-basic.propan",
    "tests/windtunnel/program/debug-context.propan",
    "tests/windtunnel/program/debug-protection.propan",
    "tests/windtunnel/program/debug-startup.propan",
    "tests/windtunnel/program/debug-triggers.propan",
    "tests/windtunnel/state/core-branches.propan",
    "tests/windtunnel/state/core-calls.propan",
    "tests/windtunnel/state/branch-a-low-bits.propan",
    "tests/windtunnel/state/local-pc-branches.propan",
    "tests/windtunnel/state/local-pc-carries.propan",
    "tests/windtunnel/state/local-pc-fetch.propan",
    "tests/windtunnel/state/local-pc-interrupts.propan",
    "tests/windtunnel/state/local-pc-iret.propan",
    "tests/windtunnel/state/local-pc-loc.propan",
    "tests/windtunnel/state/local-pc-modifiers.propan",
    "tests/windtunnel/state/local-pc-repeat.propan",
    "tests/windtunnel/state/local-pc-resume.propan",
    "tests/windtunnel/state/local-pc-skip.propan",
    "tests/windtunnel/state/local-pc-special.propan",
    "tests/windtunnel/state/local-pc-writes.propan",
    "data/timing-data/local-pc-overflow/rep-cross-wrap.propan",
    "tests/windtunnel/state/repeat-skip-slots.propan",
    "tests/windtunnel/state/repeat-branches.propan",
    "tests/windtunnel/state/skip-branches.propan",
    "tests/windtunnel/state/skip-calls.propan",
    "tests/windtunnel/state/repeat-skip-mixed.propan",
    "tests/windtunnel/state/skip-status.propan",
    "tests/windtunnel/state/skip-return.propan",
    "tests/windtunnel/state/xbyte-dispatch.propan",
    "tests/windtunnel/state/xbyte-lookup.propan",
    "tests/windtunnel/state/xbyte-modes.propan",
    "tests/windtunnel/state/xbyte-fifo.propan",
    "tests/windtunnel/state/xbyte-control.propan",
    "tests/windtunnel/state/xbyte-interrupts.propan",
    "tests/windtunnel/state/repeat-hub.propan",
    "tests/windtunnel/state/repeat-skip-modes.propan",
    "tests/windtunnel/state/repeat-skip-wrap.propan",
    "tests/windtunnel/state/repeat-skip-interrupts.propan",
    "tests/windtunnel/state/skip-stack.propan",
    "tests/windtunnel/state/repeat-state.propan",
    "tests/windtunnel/state/skip-cancellation.propan",
    "tests/windtunnel/state/core-memory.propan",
    "tests/windtunnel/state/core-memory-prefixes.propan",
    "tests/windtunnel/state/core-indirection.propan",
    "tests/windtunnel/state/core-pixels.propan",
    "tests/windtunnel/state/core-events.propan",
    "tests/windtunnel/state/pattern-events.propan",
    "tests/windtunnel/state/fifo-data.propan",
    "tests/windtunnel/state/fifo-blocks.propan",
    "tests/windtunnel/state/fifo-timing.propan",
    "tests/windtunnel/state/fifo-startup.propan",
    "tests/windtunnel/state/fifo-write-visibility.propan",
    "tests/windtunnel/state/fifo-fbw-timing.propan",
    "tests/windtunnel/state/fifo-handover.propan",
    "tests/windtunnel/state/fifo-map.propan",
    "tests/windtunnel/state/fifo-reconfigure.propan",
    "tests/windtunnel/state/selectable-pin-events.propan",
    "tests/windtunnel/state/selectable-pin-arming.propan",
    "tests/windtunnel/state/interrupt-block.propan",
    "tests/windtunnel/state/interrupt-boundary.propan",
    "tests/windtunnel/state/interrupt-controls.propan",
    "tests/windtunnel/state/interrupt-ct.propan",
    "tests/windtunnel/state/interrupt-entry.propan",
    "tests/windtunnel/state/interrupt-modes.propan",
    "tests/windtunnel/state/interrupt-nesting.propan",
    "tests/windtunnel/state/interrupt-returns.propan",
    "tests/windtunnel/state/interrupt-shielding.propan",
    "tests/windtunnel/state/interrupt-sources.propan",
    "tests/windtunnel/state/interrupt-status.propan",
    "tests/windtunnel/state/interrupt-waits.propan",
    "tests/windtunnel/state/cordic-arithmetic.propan",
    "tests/windtunnel/state/cordic-state.propan",
    "tests/windtunnel/state/cordic-timing.propan",
    "tests/windtunnel/state/cordic-pipeline.propan",
    "tests/windtunnel/state/cordic-minimum.propan",
    "tests/windtunnel/state/cordic-reset.propan",
    "tests/windtunnel/state/cordic-prefix.propan",
    "tests/windtunnel/state/cordic-cog-isolation.propan",
    "tests/windtunnel/state/cordic-shared.propan",
    "tests/windtunnel/state/setq2-bit-span.propan",
    "tests/windtunnel/state/setq2-write-source.propan",
    "tests/windtunnel/state/setq-canceled-prefix.propan",
    "tests/windtunnel/state/setq2-coginit.propan",
    "tests/windtunnel/state/mul-zero.propan",
    "tests/windtunnel/state/pipeline-cog.propan",
    "tests/windtunnel/state/pipeline-lut-source-events.propan",
    "tests/windtunnel/state/pipeline-hub-wrap.propan",
    "tests/windtunnel/state/pipeline-pair-startup.propan",
    "tests/windtunnel/state/pipeline-restart.propan",
    "tests/windtunnel/state/pipeline-lut.propan",
    "tests/windtunnel/state/pipeline-block-visibility.propan",
    "tests/windtunnel/state/pipeline-fifo-transition.propan",
    "tests/windtunnel/state/pipeline-software-prefetch.propan",
    "tests/windtunnel/state/pipeline-lut-dual-write.propan",

    "tests/windtunnel/state/pipeline-software-fifo.propan",

    "tests/windtunnel/state/pipeline-lut-prefetch.propan",
    "tests/windtunnel/state/pipeline-local-wrap.propan",
    "tests/windtunnel/state/pipeline-shared-events.propan",

    "tests/windtunnel/state/pipeline-branches.propan",
    "tests/windtunnel/state/pipeline-cancellation.propan",
    "tests/windtunnel/state/pipeline-q-clear.propan",
    "tests/windtunnel/state/pipeline-q-lut-return.propan",
    "tests/windtunnel/state/pipeline-fifo-contention.propan",
    "tests/windtunnel/state/pipeline-hub-block.propan",
    "tests/windtunnel/state/pipeline-hub-entry.propan",

    "tests/windtunnel/state/pipeline-startup.propan",

    "tests/windtunnel/state/pipeline-hub-commands.propan",

    "tests/windtunnel/state/pipeline-hub-fifo.propan",

    "tests/windtunnel/state/pipeline-hub-stack.propan",

    "tests/windtunnel/state/pipeline-prefetch.propan",
    "tests/windtunnel/state/pipeline-hub-matrix.propan",
    "tests/windtunnel/state/augd.propan",
    "tests/windtunnel/state/alu-arithmetic.propan",
    "tests/windtunnel/state/alu-lanes.propan",
    "tests/windtunnel/state/alu-crc.propan",
    "tests/windtunnel/state/alu-source.propan",
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

/// Runs a Windtunnel artifact with optional kcov collection in the named report directory.
fn addRunArtifactWithCoverage(b: *std.Build, kcov_path: ?[]const u8, artifact: *std.Build.Step.Compile, name: []const u8) *std.Build.Step.Run {
    const run = if (kcov_path) |kcov| blk: {
        const run = b.addSystemCommand(&.{
            kcov,
            "--clean",
            b.fmt("--include-path={s},{s}", .{ b.pathFromRoot("src/windtunnel/sim"), b.pathFromRoot("src/libp2/alu.zig") }),
            b.fmt(".coverage/{s}", .{name}),
        });
        run.addArtifactArg(artifact);
        break :blk run;
    } else b.addRunArtifact(artifact);
    run.has_side_effects = kcov_path != null;
    return run;
}

pub fn build(b: *std.Build) !void {
    // Steps:
    const run_step = b.step("run", "Runs propan");
    const test_step = b.step("test", "Runs the test suite");
    const windtunnel_test_step = b.step("test-windtunnel", "Runs Windtunnel checklist fixtures and harness tests");
    const libp2_test_step = b.step("test-libp2", "Runs the libp2 test suite.");

    // Options:
    const with_flexspin = b.option(bool, "with-flexspin", "Includes FlexSpin for Spin2 round-trip and assembler equivalence tests.") orelse false;
    const no_emit_bin = b.option(bool, "no-emit-bin", "Does not emit a binary, just compiles the applications") orelse false;
    const coverage = b.option(bool, "coverage", "Collect Propan and Windtunnel coverage with kcov when available") orelse true;
    const with_p2aas = b.option(bool, "with-p2aas", "Run Windtunnel hardware oracle tests through P2AAS_ENDPOINT") orelse false;
    const fast_io = b.option(bool, "fast-io", "Use single-clock serial endpoint throughput in Windtunnel fixtures") orelse false;

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const kcov_path = if (coverage) b.findProgram(&.{"kcov"}, &.{}) catch null else null;
    if (coverage and kcov_path == null) std.log.warn("could not find kcov, not generating coverage", .{});

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

    const mod_p2 = b.createModule(.{
        .root_source_file = b.path("src/libp2/propeller2.zig"),
        .target = target,
        .optimize = optimize,
    });

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
            .{ .name = "p2", .module = mod_p2 },
            .{ .name = "ptk", .module = ptk_mod },
            .{ .name = "args", .module = args_mod },
        },
    });

    const windtunnel_mod = b.addModule("windtunnel", .{
        .root_source_file = b.path("src/windtunnel/windtunnel.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "p2", .module = mod_p2 },
            .{ .name = "args", .module = args_mod },
        },
    });

    const boot_rom = b.createModule(.{ .root_source_file = b.path("docs/p2/bootrom/device-rom.bin") });
    windtunnel_mod.addImport("boot-rom", boot_rom);

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

    {
        const zglfw = b.dependency("zglfw", .{ .target = target, .optimize = optimize });
        const zopengl = b.dependency("zopengl", .{ .target = target });
        const zgui = b.dependency("zgui", .{
            .target = target,
            .optimize = optimize,
            .backend = .glfw_opengl3,
        });
        const gui_mod = b.createModule(.{
            .root_source_file = b.path("src/windtunnel-gui/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zglfw", .module = zglfw.module("root") },
                .{ .name = "zopengl", .module = zopengl.module("root") },
                .{ .name = "zgui", .module = zgui.module("root") },
                .{ .name = "windtunnel", .module = windtunnel_mod },
            },
        });
        gui_mod.linkLibrary(zglfw.artifact("glfw"));
        gui_mod.linkLibrary(zgui.artifact("imgui"));

        const exe = b.addExecutable(.{
            .name = "windtunnel-gui",
            .root_module = gui_mod,
            .use_llvm = true,
        });
        fb.installArtifact(exe);

        const gui_step = b.step("build-windtunnel-gui", "Builds Windtunnel's graphical interface");
        if (no_emit_bin) {
            gui_step.dependOn(&exe.step);
        } else {
            gui_step.dependOn(&b.addInstallArtifact(exe, .{}).step);
        }

        const run_gui = b.addRunArtifact(exe);
        const run_gui_step = b.step("run-windtunnel-gui", "Runs Windtunnel's graphical interface");
        run_gui_step.dependOn(&run_gui.step);
    }

    const windtunnel_suite = b.createModule(.{
        .root_source_file = b.path("src/windtunnel/test_suite.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "p2", .module = mod_p2 },
            .{ .name = "args", .module = args_mod },
            .{ .name = "propan", .module = propan_mod },
            .{ .name = "oracle-template", .module = b.createModule(.{ .root_source_file = b.path("tests/windtunnel/oracle.propan.in") }) },
        },
    });
    windtunnel_suite.addImport("boot-rom", boot_rom);
    const windtunnel_tests = b.addExecutable(.{
        .name = "windtunnel-tests",
        .root_module = windtunnel_suite,
        .use_llvm = if (kcov_path != null) true else null,
    });
    fb.installArtifact(windtunnel_tests);
    const harness_tests = b.addTest(.{
        .name = "windtunnel-harness-tests",
        .root_module = windtunnel_suite,
        .use_llvm = if (kcov_path != null) true else null,
    });
    if (kcov_path) |kcov| {
        harness_tests.setExecCmd(&.{
            kcov,
            "--clean",
            b.fmt("--include-path={s},{s}", .{ b.pathFromRoot("src/windtunnel/sim"), b.pathFromRoot("src/libp2/alu.zig") }),
            ".coverage/windtunnel-unit",
            null,
        });
    }
    const harness_run = b.addRunArtifact(harness_tests);
    harness_run.setCwd(b.path("."));
    harness_run.has_side_effects = kcov_path != null;
    windtunnel_test_step.dependOn(&harness_run.step);
    const windtunnel_unit_step = b.step("test-windtunnel-unit", "Run Windtunnel harness and simulator unit tests");
    windtunnel_unit_step.dependOn(&harness_run.step);
    const fixture_run = addRunArtifactWithCoverage(b, kcov_path, windtunnel_tests, "windtunnel-fixtures");
    fixture_run.setCwd(b.path("."));
    fixture_run.expectExitCode(0); // Capture stdio so Zig forwards progress updates.
    if (fast_io) {
        fixture_run.addArg("--fast-io");
    }
    if (with_p2aas) {
        fixture_run.addArg("--oracle");
        fixture_run.has_side_effects = true;
    }
    fixture_run.addFileInput(b.path("examples/hello-world.propan"));
    fixture_run.addFileInput(b.path("tests/windtunnel/oracle.propan.in"));
    fixture_run.addArgs(&windtunnel_fixtures);
    for (windtunnel_fixtures) |path| fixture_run.addFileInput(b.path(path));
    windtunnel_test_step.dependOn(&fixture_run.step);
    if (kcov_path) |kcov| {
        const merge = b.addSystemCommand(&.{ kcov, "--merge", ".coverage/windtunnel", ".coverage/windtunnel-fixtures", ".coverage/windtunnel-unit" });
        merge.setCwd(b.path("."));
        merge.step.dependOn(&fixture_run.step);
        merge.step.dependOn(&harness_run.step);
        windtunnel_test_step.dependOn(&merge.step);
    }
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

    {
        const libp2_tests = b.addTest(.{
            .root_module = mod_p2,
        });

        const run_cmd = b.addRunArtifact(libp2_tests);

        libp2_test_step.dependOn(&run_cmd.step);
        run_step.dependOn(libp2_test_step);
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
        if (kcov_path) |kcov| {
            tests.setExecCmd(&.{
                kcov,
                "--clean",
                b.fmt("--include-path={s}", .{b.pathFromRoot("src")}),
                ".coverage",
                null, // addRunArtifact inserts the test executable here.
            });
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
