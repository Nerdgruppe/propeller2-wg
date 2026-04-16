const std = @import("std");

const ast = @import("frontend/ast.zig");

pub const Collection = @This();

arena: std.heap.ArenaAllocator,
diagnostics: std.ArrayListUnmanaged(Diagnostic) = .empty,
sources: std.StringArrayHashMapUnmanaged(Source) = .empty,

pub const Diagnostic = struct {
    level: Level,
    location: ?ast.Location,
    message: []const u8,
};

pub const Level = enum {
    @"error",
    warning,
    info,
};

pub const Source = struct {
    text: []const u8,
};

pub const RenderOptions = struct {
    include_warnings: bool = true,
    include_infos: bool = true,
};

pub fn init(allocator: std.mem.Allocator) Collection {
    return .{
        .arena = .init(allocator),
    };
}

pub fn deinit(self: *Collection) void {
    const allocator = self.arena.allocator();
    self.diagnostics.deinit(allocator);
    self.sources.deinit(allocator);
    self.arena.deinit();
    self.* = undefined;
}

pub fn register_source(self: *Collection, path: []const u8, source: []const u8) !void {
    const allocator = self.arena.allocator();
    const gop = try self.sources.getOrPut(allocator, path);

    if (!gop.found_existing)
        gop.key_ptr.* = try allocator.dupe(u8, path);

    gop.value_ptr.* = .{
        .text = source,
    };
}

pub fn emit_error(self: *Collection, location: ?ast.Location, comptime fmt: []const u8, args: anytype) !void {
    try self.emit(.@"error", location, fmt, args);
}

pub fn emit_warning(self: *Collection, location: ?ast.Location, comptime fmt: []const u8, args: anytype) !void {
    try self.emit(.warning, location, fmt, args);
}

pub fn emit(self: *Collection, level: Level, location: ?ast.Location, comptime fmt: []const u8, args: anytype) !void {
    const allocator = self.arena.allocator();
    var owned_location = location;
    if (owned_location) |*loc| {
        if (loc.source) |source|
            loc.source = try allocator.dupe(u8, source);
    }

    try self.diagnostics.append(allocator, .{
        .level = level,
        .location = owned_location,
        .message = try std.fmt.allocPrint(allocator, fmt, args),
    });
}

pub fn has_errors(self: Collection) bool {
    for (self.diagnostics.items) |diagnostic| {
        if (diagnostic.level == .@"error")
            return true;
    }
    return false;
}

pub fn has_warnings(self: Collection) bool {
    for (self.diagnostics.items) |diagnostic| {
        if (diagnostic.level == .warning)
            return true;
    }
    return false;
}

pub fn render(self: Collection, writer: *std.Io.Writer, options: RenderOptions) !void {
    for (self.diagnostics.items) |item| {
        if (!should_render(item, options))
            continue;

        if (item.location) |location| {
            try render_location(self, writer, item, location);
        } else {
            try writer.print("{s}: {s}\n", .{
                @tagName(item.level),
                item.message,
            });
        }
    }
}

fn should_render(item: Diagnostic, options: RenderOptions) bool {
    return switch (item.level) {
        .@"error" => true,
        .warning => options.include_warnings,
        .info => options.include_infos,
    };
}

fn render_location(self: Collection, writer: *std.Io.Writer, item: Diagnostic, location: ast.Location) !void {
    if (location.source) |path| {
        try writer.print("{s}:{d}:{d}: {s}: {s}\n", .{
            path,
            location.line,
            location.column,
            @tagName(item.level),
            item.message,
        });

        if (self.sources.get(path)) |source| {
            if (source_line(source.text, location.line)) |line| {
                const line_width = @max(@as(usize, 4), decimal_width(location.line));
                try writer.splatByteAll(' ', line_width - decimal_width(location.line));
                try writer.print("{d} | {s}\n", .{ location.line, line });
                try writer.splatByteAll(' ', line_width);
                try writer.writeAll(" | ");
                if (location.column > 1)
                    try writer.splatByteAll(' ', location.column - 1);
                try writer.writeAll("^\n");
            }
        }
    } else {
        try writer.print("{d}:{d}: {s}: {s}\n", .{
            location.line,
            location.column,
            @tagName(item.level),
            item.message,
        });
    }
}

fn source_line(source: []const u8, one_based_line: u32) ?[]const u8 {
    if (one_based_line == 0)
        return null;

    var start: usize = 0;
    var current_line: u32 = 1;

    while (current_line < one_based_line) : (current_line += 1) {
        const newline = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse return null;
        start = newline + 1;
    }

    var end = std.mem.indexOfScalarPos(u8, source, start, '\n') orelse source.len;
    if (end > start and source[end - 1] == '\r')
        end -= 1;

    return source[start..end];
}

fn decimal_width(value: u32) usize {
    var digits: usize = 1;
    var rest = value;
    while (rest >= 10) : (digits += 1)
        rest /= 10;
    return digits;
}

test "collects diagnostics" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();

    try collection.emit_error(null, "failed with {}", .{1});
    try collection.emit_warning(null, "warned", .{});

    try std.testing.expect(collection.has_errors());
    try std.testing.expect(collection.has_warnings());
}

test "renders source excerpts" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();

    try collection.register_source("test.propan", "first\n    BAD\n");
    try collection.emit_error(.{ .source = "test.propan", .line = 2, .column = 5 }, "unknown mnemonic BAD", .{});

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try collection.render(&output.writer, .{});

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualStrings(
        \\test.propan:2:5: error: unknown mnemonic BAD
        \\   2 |     BAD
        \\     |     ^
        \\
    , actual);
}

test "renders missing location fallback" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();

    try collection.emit_error(null, "missing input files.", .{});

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try collection.render(&output.writer, .{});

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualStrings("error: missing input files.\n", actual);
}

test "can suppress warnings" {
    var collection: Collection = .init(std.testing.allocator);
    defer collection.deinit();

    try collection.emit_warning(null, "unused", .{});
    try collection.emit_error(null, "bad", .{});

    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    try collection.render(&output.writer, .{ .include_warnings = false });

    const actual = try output.toOwnedSlice();
    defer std.testing.allocator.free(actual);

    try std.testing.expectEqualStrings("error: bad\n", actual);
}
