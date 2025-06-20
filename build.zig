const std = @import("std");

pub fn build(b: *std.Build) void {
    // Steps:
    const run_step = b.step("run", "Runs propan");
    const test_step = b.step("test", "Runs the test suite");

    // Options:

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Dependencies:
    const serial_dep = b.dependency("serial", .{});

    const p2dev_dep = b.dependency("p2devsuite", .{});

    const ptk_dep = b.dependency("ptk", .{});
    const args_dep = b.dependency("args", .{});

    const serial_mod = serial_dep.module("serial");
    const ptk_mod = ptk_dep.module("parser-toolkit");
    const args_mod = args_dep.module("args");

    // Exports:

    const flexspin = p2dev_dep.artifact("flexspin");
    b.installArtifact(flexspin);

    const loadp2_exe = p2dev_dep.artifact("loadp2");
    b.installArtifact(loadp2_exe);

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
        b.installArtifact(exe);
    }

    const propan_exe = blk: {
        const exe = b.addExecutable(.{
            .name = "propan",
            .root_module = propan_mod,
        });

        b.installArtifact(exe);

        break :blk exe;
    };

    const windtunnel_exe = blk: {
        const exe = b.addExecutable(.{
            .name = "windtunnel",
            .root_module = windtunnel_mod,
        });

        b.installArtifact(exe);

        break :blk exe;
    };

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

        var fuzz_corpus_index: std.ArrayList(u8) = .init(b.allocator);
        defer fuzz_corpus_index.deinit();

        fuzz_corpus_index.writer().writeAll(
            \\pub const files: []const []const u8 = &.{
            \\
        ) catch @panic("oom");

        for (parser_accept_tests) |path| {
            const filename = std.fs.path.basename(path);

            _ = fuzz_corpus_files.addCopyFile(b.path(path), filename);

            fuzz_corpus_index.writer().print(
                \\    @embedFile("{}"),
                \\
            ,
                .{std.zig.fmtEscapes(filename)},
            ) catch @panic("oom");
        }

        fuzz_corpus_index.writer().writeAll(
            \\};
            \\
        ) catch @panic("oom");

        const fuzz_corpus_file = fuzz_corpus_files.add("corpus.zig", fuzz_corpus_index.items);

        const fuzz_corpus_mod = b.createModule(.{ .root_source_file = fuzz_corpus_file });

        propan_mod.addImport("fuzz-corpus", fuzz_corpus_mod);

        const propan_tests = b.addTest(.{
            .root_module = propan_mod,
        });

        const run_tests_step = b.addRunArtifact(propan_tests);

        test_step.dependOn(&run_tests_step.step);
    }

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

    // Windtunnel behaviour tests
    {
        for (windtunnel_behaviour_tests) |test_file| {
            const assemble = b.addRunArtifact(propan_exe);
            assemble.addArg("--format=flat");
            assemble.addFileArg(b.path(test_file));
            const bin_file = assemble.addPrefixedOutputFileArg("--output=", "app.bin");

            const run = b.addRunArtifact(windtunnel_exe);
            run.addPrefixedFileArg("--image=", bin_file);
            run.addPrefixedFileArg("--tests=", b.path(test_file));
            run.has_side_effects = true;
            test_step.dependOn(&run.step);
        }
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

const sema_accept_tests: []const []const u8 = examples ++ emit_compare_tests ++ &[_][]const u8{
    "tests/propan/sema/basic-constants.propan",
    "tests/propan/sema/basic-instruction-selection.propan",
    "tests/propan/sema/addressing-modes.propan",
    "tests/propan/sema/ambigious-selection.propan",
    "tests/propan/sema/basic-label-addressing.propan",
    "tests/propan/sema/operators.propan",
    "tests/propan/sema/value-hint-converter.propan",
    "tests/propan/sema/stdlib.propan",
    "tests/propan/sema/char-literals.propan",
};

const emit_compare_tests: []const []const u8 = &[_][]const u8{
    "tests/propan/equivalence/arithmetic1.propan",
    "tests/propan/equivalence/arithmetic2.propan",
    "tests/propan/equivalence/special_effects.propan",
    "tests/propan/equivalence/argless.propan",
    "tests/propan/equivalence/three_ops.propan",
    "tests/propan/equivalence/metaprogramming.propan",
    "tests/propan/equivalence/io.propan",
    "tests/propan/equivalence/branching.propan",
    "tests/propan/equivalence/memory.propan",
    "tests/propan/equivalence/cordic.propan",
    "tests/propan/equivalence/aux.propan",
    "tests/propan/equivalence/flags.propan",
    "tests/propan/equivalence/memory-ptr.propan",
    "tests/propan/equivalence/cursed.propan",
    "tests/propan/equivalence/ambigious.propan",
    "tests/propan/equivalence/aug.propan",
};

const windtunnel_behaviour_tests: []const []const u8 = &[_][]const u8{
    "tests/windtunnel/behaviour/cogstop.propan",
    "tests/windtunnel/behaviour/output.propan",
    "tests/windtunnel/behaviour/augs.propan",
};
