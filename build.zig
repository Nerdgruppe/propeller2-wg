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
        });

        const run_tests_step = b.addRunArtifact(propan_tests);

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

        const run = b.addRunArtifact(propan_exe);
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
        const run = b.addRunArtifact(propan_exe);
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
        const run = b.addRunArtifact(propan_exe);
        run.addArg("--format=json");
        run.addArg("--output=-");
        run.addFileArg(b.path("tests/propan/sema/data-mode.propan"));
        run.expectStdOutMatch("\"mode\": \"data\"");
        run.expectStdOutMatch("\"none\": {}");
        test_step.dependOn(&run.step);
    }

    // LUT jumps expose a 9-bit index within LUT memory.
    {
        const run = b.addRunArtifact(propan_exe);
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
        const parser_run = b.addRunArtifact(propan_exe);
        parser_run.addArg("--format=none");
        parser_run.addArg("--test-mode=parser");
        parser_run.addFileArg(b.path("tests/propan/regressions/check-list-mismatch.propan"));
        test_step.dependOn(&parser_run.step);
        inline for (.{ "sema", "compare" }) |mode| {
            const run = b.addRunArtifact(propan_exe);
            run.addArg("--format=none");
            run.addArg("--test-mode=" ++ mode);
            if (std.mem.eql(u8, mode, "compare")) run.addPrefixedFileArg("--compare-to=", reference);
            run.addFileArg(b.path("tests/propan/regressions/check-list-mismatch.propan"));
            run.expectExitCode(1);
            run.expectStdErrMatch("checklist memory mismatch");
            test_step.dependOn(&run.step);
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
                const run = b.addRunArtifact(propan_exe);
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
                const run = b.addRunArtifact(propan_exe);
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

                const run = b.addRunArtifact(propan_exe);
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
