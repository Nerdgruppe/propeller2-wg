const std = @import("std");

const ast = @import("frontend.zig").ast;

pub fn analyze(allocator: std.mem.Allocator, file: ast.File) !Module {
    //
    _ = allocator;
    _ = file;
    @panic("not implemented yet");
}

pub const Module = struct {};
