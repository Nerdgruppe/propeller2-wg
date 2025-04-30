const std = @import("std");

/// contains all pre-defined symbols available in the assembler for Propeller 1.
///
/// These can be found here:
///     https://github.com/totalspectrum/spin2cpp/blob/996a5745ef9c04dbdc7f0e23fc58a9c48852679b/frontends/lexer.c#L2988
///
pub const p1_constants: std.StaticStringMap(i64) = .initComptime(.{
    .{ "CHIPVER", 1 },

    .{ "RCFAST", 0x00000001 },
    .{ "RCSLOW", 0x00000002 },
    .{ "XINPUT", 0x00000004 },
    .{ "XTAL1", 0x00000008 },
    .{ "XTAL2", 0x00000010 },
    .{ "XTAL3", 0x00000020 },
    .{ "PLL1X", 0x00000040 },
    .{ "PLL2X", 0x00000080 },
    .{ "PLL4X", 0x00000100 },
    .{ "PLL8X", 0x00000200 },
    .{ "PLL16X", 0x00000400 },
});
