const std = @import("std");

pub fn build(b: *std.Build) void {
    // Steps:
    const run_step = b.step("run", "Runs propan");
    const test_step = b.step("test", "Runs the test suite");

    // Options:

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Dependencies:

    const ptk_dep = b.dependency("ptk", .{});
    const args_dep = b.dependency("args", .{});

    const ptk_mod = ptk_dep.module("parser-toolkit");
    const args_mod = args_dep.module("args");

    // Build:

    const propan_mod = b.addModule("propan", .{
        .root_source_file = b.path("src/propan/propan.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "ptk", .module = ptk_mod },
            .{ .name = "args", .module = args_mod },
        },
    });

    const propan_exe = blk: {
        const exe = b.addExecutable(.{
            .name = "propan",
            .root_module = propan_mod,
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
        const propan_tests = b.addTest(.{
            .root_module = propan_mod,
        });

        const run_tests_step = b.addRunArtifact(propan_tests);

        test_step.dependOn(&run_tests_step.step);
    }

    // Propan Behaviour Tests
    {
        for (parser_accept_tests) |parser_accept_file| {
            const run = b.addRunArtifact(propan_exe);
            run.addArg("--test-mode=parser");
            run.addFileInput(b.path(parser_accept_file));
            run.has_side_effects = true;
            test_step.dependOn(&run.step);
        }
    }
}

const parser_accept_tests: []const []const u8 = &.{
    "./tests/propan/parser/labels.propan",
    "./tests/propan/parser/conditions.propan",
    "./tests/propan/parser/effects.propan",
    "./tests/propan/parser/values.propan",
    "./tests/propan/parser/escape_sequences.propan",
    "./tests/propan/parser/basic_instruction_layout.propan",
    "./tests/propan/parser/directives.propan",
    "./tests/propan/parser/fncalls.propan",
};
