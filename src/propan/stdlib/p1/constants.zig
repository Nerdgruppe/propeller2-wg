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

    // Registers:

    .{ "PAR", 0x1f0 },
    .{ "CNT", 0x1f1 },
    .{ "INA", 0x1f2 },
    .{ "INB", 0x1f3 },
    .{ "OUTA", 0x1f4 },
    .{ "OUTB", 0x1f5 },
    .{ "DIRA", 0x1f6 },
    .{ "DIRB", 0x1f7 },
    .{ "CTRA", 0x1f8 },
    .{ "CTRB", 0x1f9 },

    .{ "FRQA", 0x1fa },
    .{ "FRQB", 0x1fb },
    .{ "PHSA", 0x1fc },
    .{ "PHSB", 0x1fd },

    .{ "VCFG", 0x1fe },
    .{ "VSCL", 0x1ff },
});
