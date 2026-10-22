//! Test-only assembly of a scaffold importing a materialized fixture under its original identity.
const std = @import("std");
const propan = @import("propan");

/// Seed the existing import cache with expanded source; all other imports keep normal resolution.
pub fn assemble(
    allocator: std.mem.Allocator,
    io: std.Io,
    path: []const u8,
    text: []const u8,
    fixture_path: []const u8,
    fixture_text: []const u8,
    errors: *std.Io.Writer,
) !propan.AssemblyResult {
    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();

    var diagnostics: propan.diagnostics.Collection = .init(allocator);
    defer diagnostics.deinit();
    defer diagnostics.render(errors, .{ .include_warnings = false, .include_infos = false }) catch {};

    var root = try propan.SourceFile.init(arena.allocator(), path, text);
    root.relative_path = std.fs.path.basename(path);
    try diagnostics.register_source_file(&root);
    var dir = try std.Io.Dir.cwd().openDir(io, std.fs.path.dirname(path) orelse ".", .{});
    defer dir.close(io);

    var dirs = [_]std.Io.Dir{dir};
    var expander = propan.frontend.imports.Expander.init(arena.allocator(), io, &diagnostics, &dirs, &.{});
    defer expander.deinit();

    const identity = try std.Io.Dir.cwd().realPathFileAlloc(io, fixture_path, arena.allocator());
    var fixture = try propan.SourceFile.init(arena.allocator(), identity, fixture_text);
    fixture.relative_path = identity;
    try diagnostics.register_source_file(&fixture);
    try expander.files.put(arena.allocator(), identity, &fixture);
    const ast = expander.expand(&root) catch return error.AssemblyFailed;
    if (diagnostics.has_errors())
        return error.AssemblyFailed;
    var module = propan.sema.analyze(allocator, ast, .{
        .io = io,
        .blank_pointer_expr = .as_ptr_epxr,
        .rebind_scopes = expander.did_import,
    }, &diagnostics) catch |err| switch (err) {
        error.SemanticErrors => return error.AssemblyFailed,
        else => return err,
    };
    errdefer module.deinit();

    if (diagnostics.has_errors())
        return error.AssemblyFailed;
    var size: usize = 0;
    for (module.segments) |segment| {
        size = @max(size, segment.hub_offset + segment.data.len);
    }
    const image = try allocator.alloc(u8, size);
    @memset(image, 0);
    for (module.segments) |segment| {
        @memcpy(image[segment.hub_offset..][0..segment.data.len], segment.data);
    }
    return .{ .module = module, .image = image, .allocator = allocator };
}
