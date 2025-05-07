const std = @import("std");
const eval = @import("../eval.zig");

/// contains all pre-defined symbols available in the assembler for Propeller 1.
///
/// These can be found here:
///     https://github.com/totalspectrum/spin2cpp/blob/996a5745ef9c04dbdc7f0e23fc58a9c48852679b/frontends/lexer.c#L2988
///
pub const common_constants: std.StaticStringMap(eval.Value) = .initComptime([_]struct { []const u8, eval.Value }{
    .{ "TRUE", .int(0xffffffff) },
    .{ "FALSE", .int(0) },
    .{ "POSX", .int(0x7fffffff) },
    .{ "NEGX", .int(-0x80000000) },
    .{ "PI", .int(0x40490fdb) },
});
