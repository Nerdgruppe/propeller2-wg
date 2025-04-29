const dump = @import("frontend/dump.zig");

pub const parser = @import("frontend/parser.zig");
pub const ast = @import("frontend/ast.zig");
pub const render = @import("frontend/render.zig");

pub const Parser = parser.Parser;
pub const ParsedFile = parser.ParsedFile;

pub const dump_ast = dump.dump_ast;

test {
    _ = parser;
    _ = ast;
    _ = render;
    _ = dump;
}
