const std = @import("std");
const ast = @import("ast.zig");
const parser = @import("parser.zig");
const diagnostics = @import("../diagnostics.zig");
const SourceFile = @import("../SourceFile.zig");

/// Parses imports recursively and keeps every parsed AST alive until analysis finishes.
pub const Expander = struct {
    allocator: std.mem.Allocator,
    io: std.Io,
    diagnostics: *diagnostics.Collection,
    dirs: []std.Io.Dir,
    include_paths: []const []const u8,
    files: std.StringHashMapUnmanaged(*SourceFile) = .empty,
    active: std.StringHashMapUnmanaged(void) = .empty,
    once_seen: std.StringHashMapUnmanaged(void) = .empty,
    parsed: std.ArrayListUnmanaged(parser.ParsedFile) = .empty,
    did_import: bool = false,

    pub fn init(allocator: std.mem.Allocator, io: std.Io, collection: *diagnostics.Collection, dirs: []std.Io.Dir, include_paths: []const []const u8) Expander {
        std.debug.assert(dirs.len == include_paths.len + 1);
        return .{ .allocator = allocator, .io = io, .diagnostics = collection, .dirs = dirs, .include_paths = include_paths };
    }

    pub fn deinit(self: *Expander) void {
        for (self.parsed.items) |*item| item.deinit();
        self.parsed.deinit(self.allocator);
        self.files.deinit(self.allocator);
        self.active.deinit(self.allocator);
        self.once_seen.deinit(self.allocator);
    }

    pub fn expand(self: *Expander, root: *SourceFile) !ast.File {
        try self.files.put(self.allocator, root.identity, root);
        var lines: std.ArrayListUnmanaged(ast.Line) = .empty;
        errdefer lines.deinit(self.allocator);
        try self.expand_file(root, null, &lines);
        return .{ .span = self.parsed.items[0].file.span, .sequence = try lines.toOwnedSlice(self.allocator), .source = root };
    }

    fn expand_file(self: *Expander, source: *SourceFile, from: ?ast.Location, lines: *std.ArrayListUnmanaged(ast.Line)) !void {
        if (self.once_seen.contains(source.identity)) return;
        if (self.active.contains(source.identity)) {
            try self.diagnostics.emit_diag(from, .{ .err_import_cycle = .{ .path = source.path } });
            return error.ImportFailed;
        }

        try self.active.put(self.allocator, source.identity, {});
        defer _ = self.active.remove(source.identity);

        var subparser: parser.Parser = .init(source, self.diagnostics);
        const parsed_file = try subparser.parse(self.allocator);
        try self.parsed.append(self.allocator, parsed_file);

        // Find the declaration before descending, so an import cycle through an
        // import-once file stops at its first visit.
        for (parsed_file.file.sequence) |line| {
            if (line != .instruction or !is_import(line.instruction)) continue;
            const instruction = line.instruction;
            if (is_once(instruction)) {
                try self.once_seen.put(self.allocator, source.identity, {});
            }
        }

        for (parsed_file.file.sequence) |line| {
            if (line != .instruction or !is_import(line.instruction)) {
                try lines.append(self.allocator, line);
                continue;
            }
            const instruction = line.instruction;
            if (is_once(instruction)) continue;
            if (instruction.condition != null or instruction.effect != null or
                instruction.arguments.len != 1 or instruction.arguments[0] != .string)
            {
                try self.diagnostics.emit_diag(instruction.location(), .err_import_requires_path_or_once);
                return error.ImportFailed;
            }

            const path = instruction.arguments[0].string.value;
            self.did_import = true;
            const dir = std.fs.path.dirname(source.path) orelse ".";
            const local_path = try std.fs.path.resolve(self.allocator, &.{ dir, path });
            const absolute = std.fs.path.isAbsolute(path);
            const search_count = if (absolute) 1 else self.include_paths.len + 1;
            const imported = search: {
                for (0..search_count) |index| {
                    const dir_index = if (absolute) 0 else if (index == 0) source.dir_index else index;
                    const relative_path = if (index == 0)
                        try std.fs.path.resolve(self.allocator, &.{ std.fs.path.dirname(source.relative_path) orelse ".", path })
                    else
                        try std.fs.path.resolve(self.allocator, &.{path});
                    const display_path = if (index == 0)
                        local_path
                    else
                        try std.fs.path.resolve(self.allocator, &.{ self.include_paths[index - 1], path });
                    const identity = try std.fmt.allocPrint(self.allocator, "{}:{s}", .{ dir_index, relative_path });
                    if (self.files.get(identity)) |file| break :search file;

                    const search_dir = if (std.fs.path.isAbsolute(relative_path)) std.Io.Dir.cwd() else self.dirs[dir_index];
                    const text = search_dir.readFileAlloc(self.io, relative_path, self.allocator, .limited(1 << 20)) catch |err| switch (err) {
                        error.FileNotFound, error.NotDir => continue,
                        else => {
                            try self.diagnostics.emit_diag(instruction.location(), .{ .err_cannot_read_import = .{ .path = display_path, .reason = err } });
                            return error.ImportFailed;
                        },
                    };
                    const file = try self.allocator.create(SourceFile);
                    file.* = try .init(self.allocator, display_path, text);
                    file.identity = identity;
                    file.dir_index = dir_index;
                    file.relative_path = relative_path;
                    try self.files.put(self.allocator, identity, file);
                    try self.diagnostics.register_source_file(file);
                    break :search file;
                }
                try self.diagnostics.emit_diag(instruction.location(), .{ .err_cannot_read_import = .{ .path = local_path, .reason = error.FileNotFound } });
                return error.ImportFailed;
            };
            try self.expand_file(imported, instruction.location(), lines);
        }
    }

    fn is_import(instruction: ast.Instruction) bool {
        return std.ascii.eqlIgnoreCase(instruction.mnemonic, ".import");
    }

    fn is_once(instruction: ast.Instruction) bool {
        return instruction.condition == null and instruction.effect == null and
            instruction.arguments.len == 1 and instruction.arguments[0] == .symbol and
            std.ascii.eqlIgnoreCase(instruction.arguments[0].symbol.symbol_name, "once");
    }
};
