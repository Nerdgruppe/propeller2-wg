/// A source buffer with a stable path for AST locations and diagnostics.
pub const SourceFile = @This();

path: []const u8,
identity: []const u8,
text: []const u8,
