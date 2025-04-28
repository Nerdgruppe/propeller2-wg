pub const parser = @import("frontend/parser.zig");
pub const ast = @import("frontend/ast.zig");
pub const render = @import("frontend/render.zig");

pub const Parser = parser.Parser;
pub const ParsedFile = parser.ParsedFile;

test {
    _ = parser;
    _ = ast;
    _ = render;
}
