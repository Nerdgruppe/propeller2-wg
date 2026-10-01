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

    var coverage_stash: CoverageDirectoryStash = .{ .b = b };
    defer {
        // when everything else is done, create a step which merges all test results:

        const merge_run = b.addSystemCommand(&.{"kcov"});

        merge_run.addArg("--merge"); // Merge output from multiple source dirs
        // merge_run.addArg("--clean"); // don't keep previous runs

        merge_run.addArg(".coverage");

        for (coverage_stash.list.items) |input_dir| {
            merge_run.addDirectoryArg(input_dir);
        }

        test_step.dependOn(&merge_run.step);
    }

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

    // Flat output must preserve the configured byte in segment gaps.
    {
        const expected_files = b.addWriteFiles();
        const expected = expected_files.add("fill-byte.bin", &.{ 0x7E, 0x7E, 0x7E, 0x7E, 0xAA });

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

    // Multi-file input is analyzed fully, then rejected without output.
    {
        const run = coverage_stash.create_test_run(propan_exe);
        run.addArg("--format=flat");
        run.addArg("--output=-");
        run.addFileArg(b.path("tests/propan/regressions/multi-file-first.propan"));
        run.addFileArg(b.path("tests/propan/regressions/multi-file-diagnostic.propan"));
        run.expectExitCode(1);
        run.expectStdOutEqual("");
        run.expectStdErrMatch("second file analyzed");
        run.expectStdErrMatch("multiple input files are not supported yet");
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

    // The embedded checklist runs in semantic and comparison test modes.
    {
        const expected_files = b.addWriteFiles();
        const reference = expected_files.add("check-list-reference.bin", &.{ 1, 0, 0, 0 });
        const parser_run = coverage_stash.create_test_run(propan_exe);
        parser_run.addArg("--format=none");
        parser_run.addArg("--test-mode=parser");
        parser_run.addFileArg(b.path("tests/propan/regressions/check-list-mismatch.propan"));
        test_step.dependOn(&parser_run.step);
        inline for (.{ "sema", "compare" }) |mode| {
            const run = coverage_stash.create_test_run(propan_exe);
            run.addArg("--format=none");
            run.addArg("--test-mode=" ++ mode);
            if (std.mem.eql(u8, mode, "compare")) run.addPrefixedFileArg("--compare-to=", reference);
            run.addFileArg(b.path("tests/propan/regressions/check-list-mismatch.propan"));
            run.expectExitCode(1);
            run.expectStdErrMatch("checklist memory mismatch");
            test_step.dependOn(&run.step);
        }
    }

    // Diagnostic checklists pass silently when emitted kinds and counts match.
    {
        inline for (.{ "parser", "sema", "compare" }) |mode| {
            const run = coverage_stash.create_test_run(propan_exe);
            run.addArg("--format=none");
            run.addArg("--test-mode=" ++ mode);
            if (std.mem.eql(u8, mode, "compare")) run.addArg("--compare-to=unused-reference.bin");
            run.addFileArg(b.path("tests/propan/regressions/check-list-parser-errors.propan"));
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
        inline for (.{ "sema", "compare" }) |mode| {
            const run = coverage_stash.create_test_run(propan_exe);
            run.addArg("--format=none");
            run.addArg("--test-mode=" ++ mode);
            if (std.mem.eql(u8, mode, "compare")) run.addArg("--compare-to=unused-reference.bin");
            run.addFileArg(b.path("tests/propan/regressions/check-list-sema-error.propan"));
            run.expectStdErrEqual("");
            test_step.dependOn(&run.step);
        }
        inline for (.{ "parser", "sema", "compare" }) |mode| {
            const missing = coverage_stash.create_test_run(propan_exe);
            missing.addArg("--format=none");
            missing.addArg("--test-mode=" ++ mode);
            if (std.mem.eql(u8, mode, "compare")) missing.addArg("--compare-to=unused-reference.bin");
            missing.addFileArg(b.path("tests/propan/regressions/check-list-missing-error.propan"));
            missing.expectExitCode(1);
            missing.expectStdErrMatch("checklist diagnostic err_assertion_failed: expected 1, got 0");
            test_step.dependOn(&missing.step);
        }
        inline for (.{ "sema", "compare" }) |mode| {
            const unexpected = coverage_stash.create_test_run(propan_exe);
            unexpected.addArg("--format=none");
            unexpected.addArg("--test-mode=" ++ mode);
            if (std.mem.eql(u8, mode, "compare")) unexpected.addArg("--compare-to=unused-reference.bin");
            unexpected.addFileArg(b.path("tests/propan/regressions/check-list-unexpected-error.propan"));
            unexpected.expectExitCode(1);
            unexpected.expectStdErrMatch("checklist diagnostic err_assertion_failed: expected 1, got 0");
            unexpected.expectStdErrMatch("checklist diagnostic err_unknown_mnemonic: expected 0, got 1");
            test_step.dependOn(&unexpected.step);
        }
        const parser_unexpected = coverage_stash.create_test_run(propan_exe);
        parser_unexpected.addArg("--format=none");
        parser_unexpected.addArg("--test-mode=parser");
        parser_unexpected.addFileArg(b.path("tests/propan/regressions/check-list-parser-unexpected-error.propan"));
        parser_unexpected.expectExitCode(1);
        parser_unexpected.expectStdErrMatch("checklist diagnostic err_empty_character_literal_not_allowed: expected 0, got 1");
        test_step.dependOn(&parser_unexpected.step);

        const warning_reference = b.addWriteFiles().add("check-list-warning.bin", &.{1});
        inline for (.{ "sema", "compare" }) |mode| {
            const warning = coverage_stash.create_test_run(propan_exe);
            warning.addArg("--format=none");
            warning.addArg("--test-mode=" ++ mode);
            if (std.mem.eql(u8, mode, "compare")) warning.addPrefixedFileArg("--compare-to=", warning_reference);
            warning.addFileArg(b.path("tests/propan/regressions/check-list-warning.propan"));
            warning.expectStdErrEqual("");
            test_step.dependOn(&warning.step);

            const unexpected_warning = coverage_stash.create_test_run(propan_exe);
            unexpected_warning.addArg("--format=none");
            unexpected_warning.addArg("--test-mode=" ++ mode);
            if (std.mem.eql(u8, mode, "compare")) unexpected_warning.addPrefixedFileArg("--compare-to=", warning_reference);
            unexpected_warning.addFileArg(b.path("tests/propan/regressions/check-list-unexpected-warning.propan"));
            unexpected_warning.expectExitCode(1);
            unexpected_warning.expectStdErrMatch("checklist diagnostic warn_symbol_has_no_references: expected 0, got 1");
            test_step.dependOn(&unexpected_warning.step);
        }
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
                const run = coverage_stash.create_test_run(propan_exe);
                run.addArg("--format=none");
                run.addArg("--test-mode=parser");
                run.addFileArg(b.path(accept_file));
                run.has_side_effects = true;
                parser_tests.dependOn(&run.step);
            }

            const sema_tests = make_sequencing_step(b, "parser tests");
            sema_tests.dependOn(parser_tests);
            test_step.dependOn(sema_tests);

            for (sema_accept_tests) |accept_file| {
                const run = coverage_stash.create_test_run(propan_exe);
                run.addArg("--format=none");
                run.addArg("--test-mode=sema");
                run.addFileArg(b.path(accept_file));
                run.has_side_effects = true;
                sema_tests.dependOn(&run.step);
            }

            const equivalence_tests = make_sequencing_step(b, "parser tests");
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

                const run = coverage_stash.create_test_run(propan_exe);
                run.addArg("--format=none");
                run.addArg("--test-mode=compare");
                run.addPrefixedFileArg("--compare-to=", ref_file);
                run.addFileArg(b.path(accept_file));
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
    // "examples/sumloop.propan",
};

const parser_accept_tests: []const []const u8 = sema_accept_tests ++ &[_][]const u8{
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

const sema_accept_tests: []const []const u8 = examples ++ emit_compare_tests ++ regression_tests ++ &[_][]const u8{
    "tests/propan/sema/basic-constants.propan",
    "tests/propan/sema/basic-instruction-selection.propan",
    "tests/propan/sema/addressing-modes.propan",
    "tests/propan/sema/ambigious-selection.propan",
    "tests/propan/sema/basic-label-addressing.propan",
    "tests/propan/sema/operators.propan",
    "tests/propan/sema/unary-plus.propan",
    "tests/propan/sema/operator-associativity.propan",
    "tests/propan/sema/value-hint-converter.propan",
    "tests/propan/sema/stdlib.propan",
    "tests/propan/sema/char-literals.propan",
    "tests/propan/sema/align.propan",
    "tests/propan/sema/data-mode.propan",
    "tests/propan/sema/check-list-regspace.propan",
    "tests/propan/sema/file_source.propan",
    "tests/propan/sema/lut-mode.propan",
};

const emit_compare_tests: []const []const u8 = &[_][]const u8{
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
    "tests/propan/equivalence/memory.propan",
    "tests/propan/equivalence/metaprogramming.propan",
    "tests/propan/equivalence/rdlong-selection-bug.propan",
    "tests/propan/equivalence/special_effects.propan",
    "tests/propan/equivalence/three_ops.propan",
    "tests/propan/equivalence/hubset.propan",
};

const windtunnel_behaviour_tests: []const []const u8 = &[_][]const u8{
    "tests/windtunnel/behaviour/cogstop.propan",
    "tests/windtunnel/behaviour/output.propan",
    "tests/windtunnel/behaviour/augs.propan",
};

const CoverageDirectoryStash = struct {
    b: *std.Build,

    list: std.ArrayList(std.Build.LazyPath) = .empty,

    fn create_test_run(cds: *CoverageDirectoryStash, exe: *std.Build.Step.Compile) *std.Build.Step.Run {
        const run_step = std.Build.Step.Run.create(cds.b, "run propan");

        run_step.addArg("kcov");

        // Only report for files in the `src` directory:
        run_step.addPrefixedDirectoryArg("--include-path=", cds.b.path("src"));

        // Only collect the data, we're not interested in rendering yet.
        run_step.addArg("--collect-only");

        // Collect data into a new directory
        const cov_dir = run_step.addOutputDirectoryArg("coverage");

        cds.list.append(cds.b.allocator, cov_dir) catch @panic("out of memory");

        // Then add the propan executable:
        run_step.addArtifactArg(exe);
        return run_step;
    }
};
