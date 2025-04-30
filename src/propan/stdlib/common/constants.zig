const std = @import("std");

/// contains all pre-defined symbols available in the assembler for Propeller 1.
///
/// These can be found here:
///     https://github.com/totalspectrum/spin2cpp/blob/996a5745ef9c04dbdc7f0e23fc58a9c48852679b/frontends/lexer.c#L2988
///
pub const common_constants: std.StaticStringMap(i64) = .initComptime(.{
    .{ "TRUE", -1 },
    .{ "FALSE", 0 },
    .{ "POSX", 0x7fffffff },
    .{ "NEGX", -0x80000000 },
    .{ "PI", 0x40490fdb },
});
