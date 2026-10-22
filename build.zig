const std = @import("std");
const cases = @import("src/propan/cases.zig");

const windtunnel_cases = @import("src/windtunnel/cases.zig");

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
    const no_gui = b.option(bool, "no-gui", "Skip the Windtunnel graphical interface and its tests") orelse false;
    const coverage = b.option(bool, "coverage", "Collect Propan and Windtunnel coverage with kcov when available") orelse true;
    const with_p2aas = b.option(bool, "with-p2aas", "Run Windtunnel hardware oracle tests through P2AAS_ENDPOINT") orelse false;
    const fast_io = b.option(bool, "fast-io", "Use single-clock serial endpoint throughput in Windtunnel fixtures") orelse false;
    const windtunnel_jobs = b.option(usize, "windtunnel-jobs", "Windtunnel fixture workers (0 uses all cores)") orelse 0;
    const windtunnel_timing_report = b.option([]const u8, "windtunnel-timing-report", "Write Windtunnel timings to a Markdown file");
    const windtunnel_include = b.option([]const u8, "windtunnel-include", "Include Windtunnel files by comma-separated path substrings");
    const windtunnel_exclude = b.option([]const u8, "windtunnel-exclude", "Exclude Windtunnel files by comma-separated path substrings");

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

    const windtunnel_cli_mod = b.createModule(.{
        .root_source_file = b.path("src/windtunnel-cli/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "windtunnel", .module = windtunnel_mod },
            .{ .name = "args", .module = args_mod },
        },
    });

    const windtunnel_p2aas_mod = b.createModule(.{
        .root_source_file = b.path("src/windtunnel-p2aas/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "windtunnel", .module = windtunnel_mod },
            .{ .name = "args", .module = args_mod },
        },
    });
    const windtunnel_p2aas = b.addExecutable(.{
        .name = "windtunnel-p2aas",
        .root_module = windtunnel_p2aas_mod,
        .use_llvm = true,
    });
    fb.installArtifact(windtunnel_p2aas);
    const build_p2aas = b.step("build-windtunnel-p2aas", "Build and install Windtunnel's P2AAS server");
    if (no_emit_bin) {
        build_p2aas.dependOn(&windtunnel_p2aas.step);
    } else {
        build_p2aas.dependOn(&b.addInstallArtifact(windtunnel_p2aas, .{}).step);
    }

    const windtunnel_exe = blk: {
        const exe = b.addExecutable(.{
            .name = "windtunnel",
            .root_module = windtunnel_cli_mod,
            .use_llvm = true,
        });

        fb.installArtifact(exe);

        break :blk exe;
    };

    const windtunnel_export_mod = b.createModule(.{
        .root_source_file = b.path("src/windtunnel-export/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "windtunnel", .module = windtunnel_mod },
            .{ .name = "args", .module = args_mod },
        },
    });
    const windtunnel_export = b.addExecutable(.{
        .name = "windtunnel-export",
        .root_module = windtunnel_export_mod,
        .use_llvm = true,
    });
    fb.installArtifact(windtunnel_export);
    const export_build = b.step("build-windtunnel-export", "Build and install the Scope inspection and VCD conversion tool");
    if (no_emit_bin) {
        export_build.dependOn(&windtunnel_export.step);
    } else {
        export_build.dependOn(&b.addInstallArtifact(windtunnel_export, .{}).step);
    }
    const export_tests = b.addTest(.{ .name = "windtunnel-export-tests", .root_module = windtunnel_export_mod, .use_llvm = true });
    const export_test_run = b.addRunArtifact(export_tests);
    const export_test_step = b.step("test-windtunnel-export", "Test Scope metadata inspection");
    export_test_step.dependOn(&export_test_run.step);
    windtunnel_test_step.dependOn(&export_test_run.step);

    if (!no_gui) {
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
                .{ .name = "args", .module = args_mod },
            },
        });
        gui_mod.linkLibrary(zglfw.artifact("glfw"));
        gui_mod.linkLibrary(zgui.artifact("imgui"));
        gui_mod.addIncludePath(zgui.path("libs/imgui"));
        gui_mod.addCSourceFile(.{ .file = b.path("src/windtunnel-gui/imgui.cpp"), .flags = &.{} });

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

        const gui_tests = b.addTest(.{ .name = "windtunnel-gui-tests", .root_module = gui_mod, .use_llvm = true });
        const gui_test_run = b.addRunArtifact(gui_tests);
        const gui_test_step = b.step("test-windtunnel-gui", "Render all Windtunnel inspectors headlessly");
        gui_test_step.dependOn(&gui_test_run.step);
        windtunnel_test_step.dependOn(&gui_test_run.step);

        const run_gui = b.addRunArtifact(exe);
        if (b.args) |args| {
            run_gui.addArgs(args);
        }
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
            .{ .name = "oracle-template", .module = b.createModule(.{ .root_source_file = b.path(windtunnel_cases.oracle_template) }) },
        },
    });
    windtunnel_suite.addImport("boot-rom", boot_rom);
    const windtunnel_tests = b.addExecutable(.{
        .name = "windtunnel-tests",
        .root_module = windtunnel_suite,
        .use_llvm = true,
    });
    fb.installArtifact(windtunnel_tests);
    const build_windtunnel_tests = b.step("build-windtunnel-tests", "Build and install the Windtunnel fixture runner");
    if (no_emit_bin) {
        build_windtunnel_tests.dependOn(&windtunnel_tests.step);
    } else {
        build_windtunnel_tests.dependOn(&b.addInstallArtifact(windtunnel_tests, .{}).step);
    }
    const build_windtunnel = b.step("build-windtunnel", "Build and install the Windtunnel simulator");
    if (no_emit_bin) {
        build_windtunnel.dependOn(&windtunnel_exe.step);
    } else {
        build_windtunnel.dependOn(&b.addInstallArtifact(windtunnel_exe, .{}).step);
    }
    const pads_benchmark_module = b.createModule(.{
        .root_source_file = b.path("src/windtunnel/pads_benchmark.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{.{ .name = "p2", .module = mod_p2 }},
    });
    pads_benchmark_module.addImport("boot-rom", boot_rom);
    const pads_benchmark = b.addExecutable(.{
        .name = "windtunnel-pads-benchmark",
        .root_module = pads_benchmark_module,
        .use_llvm = true,
    });
    const benchmark_step = b.step("bench-windtunnel-pads", "Benchmark electrical kernels or complete Hub clocks");
    const benchmark_run = b.addRunArtifact(pads_benchmark);
    if (b.args) |args| {
        benchmark_run.addArgs(args);
    }
    benchmark_step.dependOn(&benchmark_run.step);
    const build_benchmark_step = b.step("build-windtunnel-pads-benchmark", "Build the standalone pad benchmark");
    build_benchmark_step.dependOn(&b.addInstallArtifact(pads_benchmark, .{}).step);

    const debugger_module = b.createModule(.{
        .root_source_file = b.path("src/windtunnel/debugger_tests.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{.{ .name = "p2", .module = mod_p2 }},
    });
    debugger_module.addImport("boot-rom", boot_rom);

    // Hardware capture goldens are test data; named embeds keep them outside src.
    const usb_vectors = b.createModule(.{ .root_source_file = b.path("tests/windtunnel/usb-vectors.json") });
    const clock_vectors = b.createModule(.{ .root_source_file = b.path("tests/windtunnel/clock-vectors.json") });
    const usb_transmit_vectors = b.createModule(.{
        .root_source_file = b.path("tests/windtunnel/usb-transmit-vectors.json"),
    });
    for ([_]*std.Build.Module{ windtunnel_mod, windtunnel_suite, debugger_module }) |module| {
        module.addImport("usb-vectors", usb_vectors);
        module.addImport("clock-vectors", clock_vectors);
        module.addImport("usb-transmit-vectors", usb_transmit_vectors);
    }

    const debugger_tests = b.addTest(.{ .name = "windtunnel-debugger-tests", .root_module = debugger_module, .use_llvm = true });
    const debugger_run = b.addRunArtifact(debugger_tests);
    const debugger_step = b.step("test-windtunnel-debugger", "Run headless debugger and simulator regressions");
    debugger_step.dependOn(&debugger_run.step);
    windtunnel_test_step.dependOn(&debugger_run.step);

    const harness_tests = b.addTest(.{
        .name = "windtunnel-harness-tests",
        .root_module = windtunnel_suite,
        .use_llvm = true,
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
    const cli_tests = b.addTest(.{
        .name = "windtunnel-cli-tests",
        .root_module = windtunnel_cli_mod,
        .use_llvm = true,
    });
    const cli_test_run = b.addRunArtifact(cli_tests);
    const cli_test_step = b.step("test-windtunnel-cli", "Run terminal adapter regressions");
    cli_test_step.dependOn(&cli_test_run.step);
    windtunnel_unit_step.dependOn(cli_test_step);
    windtunnel_test_step.dependOn(cli_test_step);

    const server_test_module = b.createModule(.{
        .root_source_file = b.path("src/windtunnel-p2aas/tests.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "windtunnel", .module = windtunnel_mod },
            .{ .name = "propan", .module = propan_mod },
            .{ .name = "p2aas-client", .module = b.createModule(.{
                .root_source_file = b.path("src/windtunnel/p2aas.zig"),
                .target = target,
                .optimize = optimize,
            }) },
        },
    });
    const server_tests = b.addTest(.{
        .name = "windtunnel-p2aas-tests",
        .root_module = server_test_module,
        .use_llvm = true,
    });
    const server_test_run = b.addRunArtifact(server_tests);
    server_test_run.setCwd(b.path("."));
    const server_test_step = b.step("test-windtunnel-p2aas", "Run P2AAS protocol and loopback regressions");
    server_test_step.dependOn(&server_test_run.step);
    windtunnel_unit_step.dependOn(server_test_step);
    windtunnel_test_step.dependOn(server_test_step);

    const module_tests = b.addTest(.{
        .name = "windtunnel-module-tests",
        .root_module = windtunnel_mod,
        .use_llvm = true,
    });
    const module_test_run = b.addRunArtifact(module_tests);
    windtunnel_unit_step.dependOn(&module_test_run.step);
    windtunnel_test_step.dependOn(&module_test_run.step);

    const fixture_run = addRunArtifactWithCoverage(b, kcov_path, windtunnel_tests, "windtunnel-fixtures");
    fixture_run.setCwd(b.path("."));
    fixture_run.expectExitCode(0); // Capture stdio so Zig forwards progress updates.
    fixture_run.addArgs(&.{ "-j", b.fmt("{d}", .{windtunnel_jobs}) });
    if (windtunnel_include) |filter| {
        fixture_run.addArgs(&.{ "--include", filter });
    }
    if (windtunnel_exclude) |filter| {
        fixture_run.addArgs(&.{ "--exclude", filter });
    }
    if (windtunnel_timing_report) |path| {
        fixture_run.addArgs(&.{ "--timing-report", path });
        fixture_run.has_side_effects = true;
    }
    if (fast_io) {
        fixture_run.addArg("--fast-io");
    }
    if (with_p2aas) {
        fixture_run.addArg("--oracle");
        fixture_run.has_side_effects = true;
    }
    fixture_run.addFileInput(b.path(windtunnel_cases.oracle_template));
    fixture_run.addArgs(&windtunnel_cases.fixtures);
    for (windtunnel_cases.fixtures) |path| fixture_run.addFileInput(b.path(path));
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
        for (cases.formatter_tests) |fixture| {
            run.addFileInput(b.path(fixture.input));
            run.addFileInput(b.path(fixture.expected_path));
        }
        run.setCwd(b.path("."));
        test_step.dependOn(&run.step);
    }
}
