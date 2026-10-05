const std = @import("std");

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

pub fn build(b: *std.Build) void {
    // Steps:
    const run_step = b.step("run", "Runs propan");
    const test_step = b.step("test", "Runs the test suite");

    // Options:
    const with_flexspin = b.option(bool, "with-flexspin", "Includes the flexspin dependency required for running the testsuite.") orelse false;
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

    var coverage_stash: CoverageDirectoryStash = .{
        .b = b,
        .kcov_path = b.findProgram(&.{"kcov"}, &.{}) catch blk: {
            std.log.warn("could not find kcov, not generating coverage", .{});
            break :blk null;
        },
    };
    defer if (coverage_stash.kcov_path) |kcov| {
        // when everything else is done, create a step which merges all test results:

        const merge_run = b.addSystemCommand(&.{
            kcov,
            "--merge",
            "--clean",
        });

        // merge_run.addArg("--merge"); // Merge output from multiple source dirs
        // merge_run.addArg("--clean"); // don't keep previous runs

        merge_run.addArg(".coverage");

        for (coverage_stash.list.items) |input_dir| {
            merge_run.addDirectoryArg(input_dir);
        }

        test_step.dependOn(&merge_run.step);
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

    // Propan Unit Tests
    {
        const fuzz_corpus_files = b.addWriteFiles();

        var fuzz_corpus_index: std.Io.Writer.Allocating = .init(b.allocator);
        defer fuzz_corpus_index.deinit();

        fuzz_corpus_index.writer.writeAll(
            \\pub const files: []const []const u8 = &.{
            \\
        ) catch @panic("oom");

        for (parser_accept_tests) |path| {
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

        const propan_tests = b.addTest(.{
            .root_module = propan_mod,
            .use_llvm = true,
        });

        const install_tests = b.addInstallArtifact(propan_tests, .{});
        test_step.dependOn(&install_tests.step);

        const run_tests_step = coverage_stash.create_test_run(propan_tests);

        test_step.dependOn(&run_tests_step.step);
    }

    const flat_checker = b.addExecutable(.{
        .name = "check-flat-output",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tests/propan/regressions/check-flat-output.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Flat output must use the configured byte for all padding.
    {
        const expected_files = b.addWriteFiles();
        const expected = expected_files.add("fill-byte.bin", &.{ 0x7E, 0x7E, 0x7E, 0x7E, 0xAA, 0x7E, 0x7E, 0x7E, 0xBB, 0x7E, 0x7E, 0x7E, 0x44, 0x33, 0x22, 0x11 });

        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=flat");
        run.addArg("--fill-byte=126");
        const actual = run.addPrefixedOutputFileArg("--output=", "fill-byte.bin");
        run.addFileArg(b.path("tests/propan/regressions/fill-byte.propan"));

        const compare = b.addRunArtifact(flat_checker);
        compare.addFileArg(expected);
        compare.addFileArg(actual);
        test_step.dependOn(&compare.step);
    }

    // Differences in incomplete final words must remain visible in the diff.
    {
        const references = b.addWriteFiles();
        const reference = references.add("partial-word.bin", &.{2});
        const run = create_propan_test_run(&coverage_stash, propan_exe, .compare, "tests/propan/regressions/compare-partial-word.propan", reference);
        run.expectExitCode(1);
        run.expectStdOutMatch("@00000: expected: 0x00000002");
        run.expectStdOutMatch("actual: 0x00000001");
        test_step.dependOn(&run.step);
    }

    // Multi-file input is rejected before either file is analyzed.
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=flat");
        run.addArg("--output=-");
        run.addFileArg(b.path("tests/propan/regressions/multi-file-first.propan"));
        run.addFileArg(b.path("tests/propan/regressions/multi-file-diagnostic.propan"));
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrEqual("error: multiple input files are not supported\n");
        test_step.dependOn(&run.step);
    }

    // Imports search the containing file first, then CLI include paths in order.
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=none");
        run.addArg("--test-mode=sema");
        run.addPrefixedDirectoryArg("--include-path=", b.path("tests/propan/sema/fixtures/include-a"));
        run.addArg("-I");
        run.addDirectoryArg(b.path("tests/propan/sema/fixtures/include-b"));
        run.addFileArg(b.path("tests/propan/sema/fixtures/include-case/main.propan"));
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }

    // Diagnostics in an imported file retain its path and source excerpt.
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=none");
        run.addFileArg(b.path("tests/propan/sema/import-diagnostic-source.propan"));
        run.expectExitCode(1);
        run.expectStdErrMatch("fixtures/import-bad.propan:1:1: error");
        run.expectStdErrMatch("UNKNOWN_MNEMONIC");
        test_step.dependOn(&run.step);
    }
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=none");
        run.addArg("--list-file=-");
        run.addFileArg(b.path("tests/propan/sema/import-local-scope.propan"));
        run.expectStdOutMatch("00004 | 001 | second:local");
        test_step.dependOn(&run.step);
    }
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=json");
        run.addArg("--output=-");
        run.addFileArg(b.path("tests/propan/sema/import-basic.propan"));
        run.expectStdOutMatch("fixtures/import-repeat.propan");
        test_step.dependOn(&run.step);
    }

    // Data labels have a hub address but no jump PC in JSON metadata.
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=json");
        run.addArg("--output=-");
        run.addFileArg(b.path("tests/propan/sema/data-mode.propan"));
        run.expectStdOutMatch("\"mode\": \"data\"");
        run.expectStdOutMatch("\"none\": {}");
        test_step.dependOn(&run.step);
    }

    // LUT jumps expose a 9-bit index within LUT memory.
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=json");
        run.addArg("--output=-");
        run.addFileArg(b.path("tests/propan/sema/lut-mode.propan"));
        run.expectStdOutMatch("\"lut\": 4");
        test_step.dependOn(&run.step);
    }

    // Scoped and literal dotted labels have distinct debug names and addresses.
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=json");
        run.addArg("--output=-");
        run.addFileArg(b.path("tests/propan/sema/local-labels.propan"));
        run.expectStdOutMatch("\"name\": \"foo:loop\",\n      \"segment_id\": 0,\n      \"offset\": 8");
        run.expectStdOutMatch("\"name\": \"foo.loop\",\n      \"segment_id\": 0,\n      \"offset\": 12");
        test_step.dependOn(&run.step);
    }
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=none");
        run.addArg("--list-file=-");
        run.addFileArg(b.path("tests/propan/sema/local-labels.propan"));
        run.expectStdOutMatch("00008 | 002 | foo:loop");
        run.expectStdOutMatch("0000C | 003 | foo.loop");
        test_step.dependOn(&run.step);
    }
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=json");
        run.addArg("--output=-");
        run.addFileArg(b.path("tests/propan/sema/local-label-segments.propan"));
        run.expectStdOutMatch("\"name\": \".loop\",\n      \"segment_id\": 2,\n      \"offset\": 272");
        test_step.dependOn(&run.step);
    }

    // Cases that require expected failures or a comparison reference.
    {
        const expected_files = b.addWriteFiles();
        const reference = expected_files.add("check-list-reference.bin", &.{ 1, 0, 0, 0 });
        const warning_reference = expected_files.add("check-list-warning.bin", &.{1});
        const all_modes: []const TestMode = &.{ .parser, .sema, .compare };
        const build_modes: []const TestMode = &.{ .sema, .compare };
        const cases = [_]PropanTestCase{
            .{
                .path = "tests/propan/regressions/hexadecimal-string-tail.propan",
                .modes = &.{.sema},
                .result = .{ .failure = &.{"assertion failed: AZB!"} },
            },
            .{
                .path = "tests/propan/sema/diagnostics/fit-message.propan",
                .modes = &.{.sema},
                .result = .{ .failure = &.{"assertion failed: code exceeds size"} },
            },
            .{
                .path = "tests/propan/regressions/check-list-mismatch.propan",
                .modes = build_modes,
                .reference = reference,
                .result = .{ .failure = &.{"checklist memory mismatch"} },
            },
            .{
                .path = "tests/propan/regressions/check-list-missing-error.propan",
                .modes = all_modes,
                .result = .{ .failure = &.{"checklist diagnostic err_assertion_failed: expected 1, got 0"} },
            },
            .{
                .path = "tests/propan/regressions/check-list-unexpected-error.propan",
                .modes = build_modes,
                .result = .{ .failure = &.{
                    "checklist diagnostic err_assertion_failed: expected 1, got 0",
                    "checklist diagnostic err_unknown_mnemonic: expected 0, got 1",
                } },
            },
            .{
                .path = "tests/propan/regressions/check-list-parser-unexpected-error.propan",
                .modes = &.{.parser},
                .result = .{ .failure = &.{"checklist diagnostic err_empty_character_literal_not_allowed: expected 0, got 1"} },
            },
            .{
                .path = "tests/propan/regressions/check-list-parser-unexpected-warning.propan",
                .modes = &.{.parser},
                .result = .{ .failure = &.{"checklist diagnostic warn_invalid_escape_sequence: expected 0, got 1"} },
            },
            .{
                .path = "tests/propan/regressions/check-list-missing-warning.propan",
                .modes = all_modes,
                .reference = warning_reference,
                .result = .{ .failure = &.{"checklist diagnostic warn_symbol_has_no_references: expected 1, got 0"} },
            },
            .{ .path = "tests/propan/regressions/check-list-warning.propan", .modes = &.{.compare}, .reference = warning_reference, .result = .silent },
            .{
                .path = "tests/propan/regressions/check-list-unexpected-warning.propan",
                .modes = build_modes,
                .reference = warning_reference,
                .result = .{ .failure = &.{"checklist diagnostic warn_symbol_has_no_references: expected 0, got 1"} },
            },
            .{
                .path = "tests/propan/parser/diagnostics/malformed-checklist.propan",
                .modes = &.{.parser},
                .result = .{ .failure = &.{
                    "checklist sym requires",
                    "checklist mem requires",
                    "invalid checklist memory address",
                    "invalid checklist memory comparison",
                    "invalid checklist memory format",
                    "invalid checklist hex byte",
                    "text after checklist memory block",
                    "unexpected '[' in checklist memory block",
                } },
            },
        };
        for (cases) |case| add_propan_test_case(&coverage_stash, propan_exe, test_step, case);
    }

    for (parser_diagnostic_tests) |path| {
        const run = create_propan_test_run(&coverage_stash, propan_exe, .parser, path, null);
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for (sema_diagnostic_tests) |path| {
        const run = create_propan_test_run(&coverage_stash, propan_exe, .sema, path, null);
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    for (compare_diagnostic_tests) |path| {
        const run = create_propan_test_run(&coverage_stash, propan_exe, .compare, path, null);
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    {
        const no_input = coverage_stash.create_test_run(propan_exe);
        no_input.addArg("--format=none");
        no_input.expectExitCode(1);
        no_input.expectStdErrMatch("missing input files");
        test_step.dependOn(&no_input.step);
    }
    {
        const source = b.path("tests/propan/sema/pack-values.propan");
        const normal = coverage_stash.create_test_run(propan_exe);
        normal.addArg("--format=none");
        normal.addFileArg(source);
        normal.expectStdErrMatch("warning:");
        test_step.dependOn(&normal.step);

        const quiet = coverage_stash.create_test_run(propan_exe);
        quiet.addArgs(&.{ "--format=none", "--no-warnings" });
        quiet.addFileArg(source);
        quiet.expectStdErrEqual("");
        test_step.dependOn(&quiet.step);
    }

    // Render the stdlib documentation for testing
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--render-stdlib-docs=-");
        run.expectStdOutMatch("<!doctype html>");
        run.expectStdOutMatch(">P_DAC_DITHER_PWM<");
        run.expectStdOutMatch(">popcnt<");
        test_step.dependOn(&run.step);
    }

    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--pretty-print");
        run.addFileArg(b.path("tests/propan/format/input.propan"));
        run.expectStdOutEqual(@embedFile("tests/propan/format/expected.propan"));
        run.expectStdErrEqual("");
        test_step.dependOn(&run.step);
    }
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--pretty-print");
        run.addFileArg(b.path("tests/propan/parser/diagnostics/missing-parenthesis.propan"));
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch("error:");
        test_step.dependOn(&run.step);
    }

    // Exports:

    if (with_flexspin) blk: {
        const p2dev_dep = b.lazyDependency("p2devsuite", .{}) orelse break :blk;

        const flexspin = p2dev_dep.artifact("flexspin");
        fb.installArtifact(flexspin);

        const loadp2_exe = p2dev_dep.artifact("loadp2");
        fb.installArtifact(loadp2_exe);

        // Propan Behaviour Tests
        {
            const parser_tests = make_sequencing_step(b, "parser tests");
            test_step.dependOn(parser_tests);

            for (parser_accept_tests) |accept_file| {
                const run = create_propan_test_run(&coverage_stash, propan_exe, .parser, accept_file, null);
                run.has_side_effects = true;
                parser_tests.dependOn(&run.step);
            }

            const sema_tests = make_sequencing_step(b, "semantic tests");
            sema_tests.dependOn(parser_tests);
            test_step.dependOn(sema_tests);

            for (sema_accept_tests) |accept_file| {
                const run = create_propan_test_run(&coverage_stash, propan_exe, .sema, accept_file, null);
                run.has_side_effects = true;
                sema_tests.dependOn(&run.step);
            }

            const formatter_tests = make_sequencing_step(b, "formatter round-trip tests");
            formatter_tests.dependOn(sema_tests);
            test_step.dependOn(formatter_tests);

            for (sema_accept_tests) |accept_file| {
                const original = coverage_stash.create_test_run(propan_exe);
                original.addArgs(&.{ "--format=flat", "--no-warnings" });
                const reference = original.addPrefixedOutputFileArg("--output=", "original.bin");
                original.addFileArg(b.path(accept_file));
                original.expectExitCode(0);

                const format = coverage_stash.create_test_run(propan_exe);
                format.addArgs(&.{ "--no-warnings", "--pretty-print" });
                format.addFileArg(b.path(accept_file));
                const formatted = format.captureStdOut(.{ .basename = "formatted.propan" });
                format.expectExitCode(0);

                const reassemble = coverage_stash.create_test_run(propan_exe);
                reassemble.setCwd(b.path(std.fs.path.dirname(accept_file) orelse "."));
                reassemble.setStdIn(.{ .lazy_path = formatted });
                reassemble.addArgs(&.{ "--format=flat", "--no-warnings" });
                const rebuilt = reassemble.addPrefixedOutputFileArg("--output=", "formatted.bin");
                reassemble.addArg("-");
                reassemble.expectExitCode(0);

                const check = b.addRunArtifact(flat_checker);
                check.addFileArg(reference);
                check.addFileArg(rebuilt);
                formatter_tests.dependOn(&check.step);
            }

            const spin2_tests = make_sequencing_step(b, "Spin2 round-trip tests");
            spin2_tests.dependOn(sema_tests);
            test_step.dependOn(spin2_tests);

            // sema_accept_tests already includes emit_compare_tests.
            for (sema_accept_tests) |accept_file| {
                const flat = b.addRunArtifact(propan_exe);
                flat.addArg("--format=flat");
                const reference = flat.addPrefixedOutputFileArg("--output=", "reference.bin");
                flat.addFileArg(b.path(accept_file));
                flat.expectExitCode(0);

                const spin2 = b.addRunArtifact(propan_exe);
                spin2.addArg("--format=spin2");
                const generated = spin2.addPrefixedOutputFileArg("--output=", b.fmt("{s}.spin2", .{std.fs.path.stem(accept_file)}));
                spin2.addFileArg(b.path(accept_file));
                spin2.expectExitCode(0);

                const convert = b.addRunArtifact(flexspin);
                convert.addArgs(&.{ "-2", "-q", "-o" });
                const rebuilt = convert.addOutputFileArg("rebuilt.bin");
                convert.addFileArg(generated);

                const check = b.addRunArtifact(flat_checker);
                check.addFileArg(reference);
                check.addFileArg(rebuilt);
                spin2_tests.dependOn(&check.step);
            }

            const readable = b.addRunArtifact(propan_exe);
            readable.addArgs(&.{ "--format=spin2", "--output=-" });
            readable.addFileArg(b.path("tests/propan/sema/spin2-readable.propan"));
            readable.expectStdOutMatch("MOV dst, #VALUE");
            readable.expectStdOutMatch("MOV dst, #$3");
            readable.expectStdOutMatch("MOV 10, 32");
            readable.expectStdOutMatch("ADD 12, #$A");
            spin2_tests.dependOn(&readable.step);

            const sumloop = b.addRunArtifact(propan_exe);
            sumloop.addArgs(&.{ "--format=spin2", "--output=-" });
            sumloop.addFileArg(b.path("examples/sumloop.propan"));
            sumloop.expectStdOutMatch("' swiftly sums buf_a and buf_b into buf_c");
            sumloop.expectStdOutMatch(".loop\n  REP @.end, #8");
            sumloop.expectStdOutMatch("  ADD 0-0, #0");
            sumloop.expectStdOutMatch(".end\n  RET wcz");
            sumloop.expectStdOutMatch("BYTE 0[8] ' .align 8");
            spin2_tests.dependOn(&sumloop.step);

            const equivalence_tests = make_sequencing_step(b, "equivalence tests");
            equivalence_tests.dependOn(sema_tests);
            test_step.dependOn(equivalence_tests);

            for (emit_compare_tests) |accept_file| {
                const suffix = std.fs.path.extension(accept_file);

                const spin2_file = b.fmt("{s}.spin2", .{accept_file[0 .. accept_file.len - suffix.len]});

                const convert = b.addRunArtifact(flexspin);
                convert.addArg("-2");
                convert.addArg("-o");

                const ref_file = convert.addOutputFileArg(b.fmt("{s}.bin", .{
                    std.fs.path.basename(accept_file),
                }));

                convert.addFileArg(b.path(spin2_file));

                const run = create_propan_test_run(&coverage_stash, propan_exe, .compare, accept_file, ref_file);
                run.has_side_effects = true;
                equivalence_tests.dependOn(&run.step);
            }
        }
    } else {
        const fail_step = b.addFail("Cannot run test suite without flexspin! Use -Dwith-flexspin to enable it.");
        test_step.dependOn(&fail_step.step);
    }

    // // Windtunnel behaviour tests
    // {
    //     for (windtunnel_behaviour_tests) |test_file| {
    //         const assemble = b.addRunArtifact(propan_exe);
    //         assemble.addArg("--format=flat");
    //         assemble.addFileArg(b.path(test_file));
    //         const bin_file = assemble.addPrefixedOutputFileArg("--output=", "app.bin");

    //         const run = b.addRunArtifact(windtunnel_exe);
    //         run.addPrefixedFileArg("--image=", bin_file);
    //         run.addPrefixedFileArg("--tests=", b.path(test_file));
    //         run.has_side_effects = true;
    //         test_step.dependOn(&run.step);
    //     }
    // }
}

const TestMode = enum { parser, sema, compare };

const PropanTestCase = struct {
    path: []const u8,
    modes: []const TestMode,
    reference: ?std.Build.LazyPath = null,
    result: union(enum) {
        silent,
        failure: []const []const u8,
    },
};

fn create_propan_test_run(
    coverage_stash: *CoverageDirectoryStash,
    exe: *std.Build.Step.Compile,
    mode: TestMode,
    path: []const u8,
    reference: ?std.Build.LazyPath,
) *std.Build.Step.Run {
    const run = coverage_stash.create_test_run(exe);
    run.addArg("--format=none");
    run.addArg(coverage_stash.b.fmt("--test-mode={s}", .{@tagName(mode)}));
    if (mode == .compare) {
        if (reference) |file| {
            run.addPrefixedFileArg("--compare-to=", file);
        } else {
            // Expected compilation errors should stop before opening this path.
            run.addArg("--compare-to=unused-reference.bin");
        }
    }
    run.addFileArg(coverage_stash.b.path(path));
    return run;
}

fn add_propan_test_case(
    coverage_stash: *CoverageDirectoryStash,
    exe: *std.Build.Step.Compile,
    test_step: *std.Build.Step,
    case: PropanTestCase,
) void {
    for (case.modes) |mode| {
        const run = create_propan_test_run(coverage_stash, exe, mode, case.path, case.reference);
        switch (case.result) {
            .silent => run.expectStdErrEqual(""),
            .failure => |messages| {
                run.expectExitCode(1);
                for (messages) |message| run.expectStdErrMatch(message);
            },
        }
        test_step.dependOn(&run.step);
    }
}

fn make_sequencing_step(b: *std.Build, name: []const u8) *std.Build.Step {
    const step = b.allocator.create(std.Build.Step) catch @panic("OOM");
    step.* = .init(.{
        .id = .custom,
        .name = b.fmt("{s}", .{name}),
        .owner = b,
    });
    return step;
}

const examples: []const []const u8 = &[_][]const u8{
    "examples/propio-client.propan",
    "examples/sumloop.propan",
};

const parser_accept_tests: []const []const u8 = common_accept_tests ++ &[_][]const u8{
    "./tests/propan/parser/labels.propan",
    "./tests/propan/parser/conditions.propan",
    "./tests/propan/parser/effects.propan",
    "./tests/propan/parser/values.propan",
    "./tests/propan/parser/escape_sequences.propan",
    "./tests/propan/parser/basic_instruction_layout.propan",
    "./tests/propan/parser/directives.propan",
    "./tests/propan/parser/fncalls.propan",
    "./tests/propan/parser/expressions.propan",
    "./tests/propan/parser/comments.propan",
    "./tests/propan/parser/amiguity.propan",
};

const regression_tests: []const []const u8 = &[_][]const u8{
    // TODO: Implement "failing tests" "tests/propan/regressions/null-in-assert.propan",
};

const parser_diagnostic_tests: []const []const u8 = &.{
    "tests/propan/parser/diagnostics/hexadecimal-escape-tail.propan",
    "tests/propan/parser/diagnostics/incomplete-binary.propan",
    "tests/propan/parser/diagnostics/incomplete-unary.propan",
    "tests/propan/parser/diagnostics/incomplete-constant.propan",
    "tests/propan/parser/diagnostics/inactive-conditional-syntax.propan",
    "tests/propan/parser/diagnostics/invalid-condition.propan",
    "tests/propan/parser/diagnostics/missing-parenthesis.propan",
    "tests/propan/parser/diagnostics/missing-function-argument.propan",
    "tests/propan/parser/diagnostics/long-character.propan",
    "tests/propan/parser/diagnostics/lone-string-quote.propan",
    "tests/propan/parser/diagnostics/lone-character-quote.propan",
    "tests/propan/regressions/check-list-mismatch.propan",
    "tests/propan/regressions/check-list-parser-errors.propan",
    "tests/propan/parser/diagnostics/incomplete-escapes.propan",
    "tests/propan/parser/diagnostics/empty-character.propan",
    "tests/propan/parser/diagnostics/invalid-escape-warning.propan",
    "tests/propan/parser/diagnostics/del-character.propan",
};

const sema_diagnostic_tests: []const []const u8 = &.{
    "tests/propan/sema/diagnostics/conditional-structure.propan",
    "tests/propan/sema/diagnostics/conditional-values.propan",
    "tests/propan/sema/diagnostics/conditional-arity.propan",
    "tests/propan/sema/diagnostics/array-zero.propan",
    "tests/propan/sema/diagnostics/array-negative.propan",
    "tests/propan/sema/diagnostics/constant-cycle.propan",
    "tests/propan/sema/diagnostics/layout-constant-label.propan",
    "tests/propan/sema/diagnostics/layout-constant-known-label.propan",
    "tests/propan/sema/diagnostics/array-constant-label.propan",
    "tests/propan/sema/diagnostics/array-invalid-utf8.propan",
    "tests/propan/sema/diagnostics/array-too-large.propan",
    "tests/propan/sema/diagnostics/pic-invalid.propan",
    "tests/propan/sema/diagnostics/pic-force-absolute.propan",
    "tests/propan/sema/diagnostics/pack-invalid.propan",
    "tests/propan/sema/diagnostics/pack-mode-count.propan",
    "tests/propan/sema/diagnostics/pack-offset-address-space.propan",
    "tests/propan/sema/diagnostics/pack-offset-needs-address.propan",
    "tests/propan/sema/diagnostics/pack-unaligned-cog.propan",
    "tests/propan/sema/diagnostics/pack-unaligned-lut.propan",
    "tests/propan/sema/diagnostics/invalid-origins.propan",
    "tests/propan/sema/diagnostics/invalid-local-start.propan",
    "tests/propan/sema/diagnostics/fit-invalid-arguments.propan",
    "tests/propan/sema/diagnostics/fit-incompatible-label.propan",
    "tests/propan/sema/diagnostics/local-start-incompatible-label.propan",
    "tests/propan/sema/diagnostics/fit-over-limit.propan",
    "tests/propan/sema/diagnostics/assert-message-with-true-condition.propan",
    "tests/propan/sema/diagnostics/assert-relative-comparison.propan",
    "tests/propan/sema/diagnostics/invalid-data-types.propan",
    "tests/propan/sema/diagnostics/pointer-constant-crash.propan",
    "tests/propan/sema/diagnostics/constant-address-crash.propan",
    "tests/propan/sema/diagnostics/unsupported-binary-types.propan",
    "tests/propan/sema/diagnostics/expression-evaluation-failure.propan",
    "tests/propan/sema/diagnostics/division-overflow.propan",
    "tests/propan/sema/diagnostics/ticks-overflow.propan",
    "tests/propan/sema/diagnostics/invalid-constants.propan",
    "tests/propan/regressions/check-list-parser-errors.propan",
    "tests/propan/regressions/check-list-sema-error.propan",
    "tests/propan/regressions/check-list-warning.propan",
    "tests/propan/sema/diagnostics/whole-memory-length-mismatch.propan",
    "tests/propan/sema/diagnostics/ambiguous-selection.propan",
    "tests/propan/sema/diagnostics/file-requires-path.propan",
    "tests/propan/sema/diagnostics/align-exceeds-address-space.propan",
    "tests/propan/sema/diagnostics/org-requires-argument.propan",
    "tests/propan/sema/diagnostics/reserve-requires-count.propan",
    "tests/propan/sema/diagnostics/reserve-exceeds-cog.propan",
    "tests/propan/sema/diagnostics/assert-requires-operand.propan",
    "tests/propan/sema/diagnostics/aug-must-be-root.propan",
    "tests/propan/sema/diagnostics/aug-pointer-index-out-of-range.propan",
    "tests/propan/sema/diagnostics/nrel-must-be-root.propan",
    "tests/propan/sema/diagnostics/lutaddr-register.propan",
    "tests/propan/sema/diagnostics/address-without-execution-pc.propan",
    "tests/propan/sema/diagnostics/at-outside-scope.propan",
    "tests/propan/sema/diagnostics/current-pc-regspace.propan",
    "tests/propan/sema/diagnostics/current-pc-constant.propan",
    "tests/propan/sema/diagnostics/at-target-without-hub.propan",
    "tests/propan/sema/diagnostics/at-unaligned-delta.propan",
    "tests/propan/sema/diagnostics/at-current-without-hub.propan",
    "tests/propan/sema/diagnostics/positional-after-named.propan",
    "tests/propan/sema/diagnostics/waitx-short-delay.propan",
    "tests/propan/sema/diagnostics/align-forward-reference.propan",
    "tests/propan/sema/diagnostics/augment-address-operand.propan",
    "tests/propan/sema/diagnostics/org-invalid-in-data.propan",
    "tests/propan/sema/diagnostics/org-target-exceeds-space.propan",
    "tests/propan/sema/diagnostics/org-cannot-move-backward.propan",
    "tests/propan/sema/diagnostics/data-in-regspace.propan",
    "tests/propan/sema/diagnostics/code-in-data.propan",
    "tests/propan/sema/diagnostics/branch-into-data.propan",
    "tests/propan/sema/diagnostics/hubaddr-without-hub.propan",
    "tests/propan/sema/diagnostics/duplicate-label.propan",
    "tests/propan/sema/diagnostics/duplicate-local-label.propan",
    "tests/propan/sema/diagnostics/local-label-after-global.propan",
    "tests/propan/sema/diagnostics/local-label-after-var.propan",
    "tests/propan/sema/diagnostics/local-label-after-segment.propan",
    "tests/propan/sema/diagnostics/duplicate-constant.propan",
    "tests/propan/sema/diagnostics/unknown-function.propan",
    "tests/propan/sema/diagnostics/layout-needs-integer.propan",
    "tests/propan/sema/diagnostics/layout-integer-out-of-range.propan",
    "tests/propan/sema/diagnostics/instruction-operand-count.propan",
    "tests/propan/sema/diagnostics/assert-too-many-operands.propan",
    "tests/propan/sema/diagnostics/assert-needs-integer.propan",
    "tests/propan/sema/diagnostics/assert-needs-string-message.propan",
    "tests/propan/sema/diagnostics/invalid-enumerator.propan",
    "tests/propan/sema/diagnostics/pointer-immediate-out-of-range.propan",
    "tests/propan/sema/diagnostics/operand-out-of-range.propan",
    "tests/propan/sema/diagnostics/branch-too-far-bytes.propan",
    "tests/propan/sema/diagnostics/branch-too-far-instructions.propan",
    "tests/propan/sema/diagnostics/augmented-branch-too-far.propan",
    "tests/propan/sema/diagnostics/negative-integer-truncated.propan",
    "tests/propan/sema/diagnostics/positive-integer-truncated.propan",
    "tests/propan/sema/diagnostics/pointer-index-out-of-range.propan",
    "tests/propan/sema/diagnostics/incrementing-pointer-index-out-of-range.propan",
    "tests/propan/sema/diagnostics/pointer-index-already-set.propan",
    "tests/propan/sema/diagnostics/increment-needs-pointer.propan",
    "tests/propan/sema/diagnostics/unary-operator-on-register.propan",
    "tests/propan/sema/diagnostics/unary-operator-on-enumerator.propan",
    "tests/propan/sema/diagnostics/bang-needs-integer.propan",
    "tests/propan/sema/diagnostics/tilde-needs-integer.propan",
    "tests/propan/sema/diagnostics/plus-needs-integer.propan",
    "tests/propan/sema/diagnostics/minus-needs-integer.propan",
    "tests/propan/sema/diagnostics/at-needs-address.propan",
    "tests/propan/sema/diagnostics/dereference-needs-address.propan",
    "tests/propan/sema/diagnostics/address-of-needs-address.propan",
    "tests/propan/sema/diagnostics/index-needs-register-and-integer.propan",
    "tests/propan/sema/diagnostics/binary-mismatched-types.propan",
    "tests/propan/sema/diagnostics/binary-operator-on-registers.propan",
    "tests/propan/sema/diagnostics/binary-operator-on-enumerators.propan",
    "tests/propan/sema/diagnostics/binary-operator-on-pointer.propan",
    "tests/propan/sema/diagnostics/address-function-needs-address.propan",
    "tests/propan/sema/diagnostics/address-function-expected-offset.propan",
    "tests/propan/sema/diagnostics/address-function-wrong-mode.propan",
    "tests/propan/sema/diagnostics/hubaddr-expected-offset.propan",
    "tests/propan/sema/diagnostics/hubaddr-needs-address.propan",
    "tests/propan/sema/diagnostics/pointer-expression-needs-ptra-ptrb.propan",
    "tests/propan/sema/diagnostics/function-argument-count.propan",
    "tests/propan/sema/diagnostics/function-unknown-parameter.propan",
    "tests/propan/sema/diagnostics/function-parameter-passed-twice.propan",
    "tests/propan/sema/diagnostics/function-missing-parameter.propan",
    "tests/propan/sema/diagnostics/builtin-functions.propan",
    "tests/propan/sema/diagnostics/import-invalid.propan",
    "tests/propan/sema/diagnostics/import-missing.propan",
    "tests/propan/sema/diagnostics/import-cycle.propan",
    "tests/propan/sema/diagnostics/register-function-invalid.propan",
    "tests/propan/sema/diagnostics/alti-config-invalid.propan",
    "tests/propan/sema/diagnostics/alti-state-invalid.propan",
    "tests/propan/sema/diagnostics/alti-state-segments.propan",
    "tests/propan/sema/diagnostics/pin-range-wraps.propan",
    "tests/propan/sema/diagnostics/delay-exceeds-u32.propan",
};

const compare_diagnostic_tests: []const []const u8 = &.{
    "tests/propan/regressions/check-list-parser-errors.propan",
    "tests/propan/regressions/check-list-sema-error.propan",
    "tests/propan/sema/diagnostics/whole-memory-length-mismatch.propan",
};

const sema_accept_tests: []const []const u8 = common_accept_tests ++ &[_][]const u8{
    "tests/propan/sema/conditional-compilation.propan",
    "tests/propan/sema/pack-groups.propan",
    "tests/propan/sema/pack-hub-code.propan",
    "tests/propan/sema/pack-offsets.propan",
    "tests/propan/sema/pack-values.propan",
    "tests/propan/sema/pack.propan",
};

const common_accept_tests: []const []const u8 = examples ++ emit_compare_tests ++ regression_tests ++ &[_][]const u8{
    "tests/propan/sema/array-emission.propan",
    "tests/propan/sema/array-constants.propan",
    "tests/propan/sema/lazy-constants.propan",
    "tests/propan/sema/pic-modes.propan",
    "tests/propan/sema/basic-constants.propan",
    "tests/propan/sema/basic-instruction-selection.propan",
    "tests/propan/sema/addressing-modes.propan",
    "tests/propan/sema/ambigious-selection.propan",
    "tests/propan/sema/basic-label-addressing.propan",
    "tests/propan/sema/local-labels.propan",
    "tests/propan/sema/local-label-segments.propan",
    "tests/propan/sema/explicit-local-start.propan",
    "tests/propan/sema/fit-overlays.propan",
    "tests/propan/sema/fit-modes.propan",
    "tests/propan/sema/fit-address-labels.propan",
    "tests/propan/sema/local-label-non-boundaries.propan",
    "tests/propan/sema/operators.propan",
    "tests/propan/sema/builtin-functions.propan",
    "tests/propan/sema/alti-config-s-mode.propan",
    "tests/propan/sema/alti-config-d-mode.propan",
    "tests/propan/sema/alti-config-r-mode.propan",
    "tests/propan/sema/alti-config-s-ring.propan",
    "tests/propan/sema/alti-config-d-ring.propan",
    "tests/propan/sema/alti-config-r-ring.propan",
    "tests/propan/sema/alti-state-s.propan",
    "tests/propan/sema/alti-state-d.propan",
    "tests/propan/sema/alti-state-r.propan",
    "tests/propan/sema/unary-plus.propan",
    "tests/propan/sema/operator-associativity.propan",
    "tests/propan/sema/value-hint-converter.propan",
    "tests/propan/sema/stdlib.propan",
    "tests/propan/sema/mixed-function-arguments.propan",
    "tests/propan/sema/register-offset-wrap.propan",
    "tests/propan/sema/import-basic.propan",
    "tests/propan/sema/import-file-relative.propan",
    "tests/propan/sema/import-once-self.propan",
    "tests/propan/sema/import-local-scope.propan",
    "tests/propan/sema/register-function.propan",
    "tests/propan/sema/ticks-large-duration.propan",
    "tests/propan/sema/integer-extremes.propan",
    "tests/propan/sema/hexadecimal-escapes.propan",
    "tests/propan/sema/pointer-variant-order.propan",
    "tests/propan/sema/render-roundtrip.propan",
    "tests/propan/sema/current-pc.propan",
    "tests/propan/sema/char-literals.propan",
    "tests/propan/sema/aug-pointer-update.propan",
    "tests/propan/sema/align.propan",
    "tests/propan/sema/data-mode.propan",
    "tests/propan/sema/check-list-regspace.propan",
    "tests/propan/sema/file_source.propan",
    "tests/propan/sema/lut-mode.propan",
    "tests/propan/sema/string-emission.propan",
    "tests/propan/sema/spin2-readable.propan",
};

const emit_compare_tests: []const []const u8 = &[_][]const u8{
    "tests/propan/equivalence/address-byte-offsets.propan",
    "tests/propan/equivalence/absrel_sample.propan",
    "tests/propan/equivalence/ambigious.propan",
    "tests/propan/equivalence/argless.propan",
    "tests/propan/equivalence/arithmetic1.propan",
    "tests/propan/equivalence/arithmetic2.propan",
    "tests/propan/equivalence/aug.propan",
    "tests/propan/equivalence/auxilary.propan",
    "tests/propan/equivalence/branching.propan",
    "tests/propan/equivalence/cordic.propan",
    "tests/propan/equivalence/cursed.propan",
    "tests/propan/equivalence/flags.propan",
    "tests/propan/equivalence/io.propan",
    "tests/propan/equivalence/memory-ptr.propan",
    "tests/propan/equivalence/memory-ptr-aug.propan",
    "tests/propan/equivalence/memory.propan",
    "tests/propan/equivalence/metaprogramming.propan",
    "tests/propan/equivalence/rdlong-selection-bug.propan",
    "tests/propan/equivalence/same-source-aliases-alternating.propan",
    "tests/propan/equivalence/same-source-aliases.propan",
    "tests/propan/equivalence/special_effects.propan",
    "tests/propan/equivalence/three_ops.propan",
    "tests/propan/equivalence/hubset.propan",
    "tests/propan/equivalence/implicit-field-aliases.propan",
};

const windtunnel_behaviour_tests: []const []const u8 = &[_][]const u8{
    "tests/windtunnel/behaviour/cogstop.propan",
    "tests/windtunnel/behaviour/output.propan",
    "tests/windtunnel/behaviour/augs.propan",
};

const CoverageDirectoryStash = struct {
    b: *std.Build,

    list: std.ArrayList(std.Build.LazyPath) = .empty,
    kcov_path: ?[]const u8,

    fn create_test_run(cds: *CoverageDirectoryStash, exe: *std.Build.Step.Compile) *std.Build.Step.Run {
        const run_step = std.Build.Step.Run.create(cds.b, "run propan");

        if (cds.kcov_path) |path| {
            run_step.addArg(path);

            // Only report for files in the `src` directory:
            run_step.addPrefixedDirectoryArg("--include-path=", cds.b.path("src"));

            // Only collect the data, we're not interested in rendering yet.
            run_step.addArg("--collect-only");

            // Collect data into a new directory
            const cov_dir = run_step.addOutputDirectoryArg("coverage");

            cds.list.append(cds.b.allocator, cov_dir) catch @panic("out of memory");
        }

        // Then add the propan executable:
        run_step.addArtifactArg(exe);
        return run_step;
    }
};
