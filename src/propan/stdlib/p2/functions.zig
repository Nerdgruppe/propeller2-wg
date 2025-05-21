const std = @import("std");
const eval = @import("../eval.zig");
const define = @import("../define.zig");

pub const DebugConfig = packed struct(u32) {
    debug_enable: u16,
    write_protect: bool,
    locked: bool,
    _padding: u10 = 0,
    tag: u4 = 0b0010,
};

pub const FilterName = enum(u2) {
    filt0 = 0,
    filt1 = 1,
    filt2 = 2,
    filt3 = 3,
};

pub const FilterConfig = packed struct(u32) {
    // %0100_xxxx_xxxx_xxxx_xxxx_xxxR_RLLT_TTTT

    tap: u5,
    length: u2,
    filter: FilterName,
    _padding: u19 = 0,
    tag: u4 = 0b0100,
};
pub const CrystalMode = enum(u2) {
    float = 0,
    nocap = 1,
    @"15pF" = 2,
    @"30pF" = 3,
};

pub const ClockSource = enum(u2) {
    rcfast = 0b00,
    rcslow = 0b01,
    xi = 0b10,
    pll = 0b11,
};

pub const ClockMode = packed struct(u32) {
    // %0000_000E_DDDD_DDMM_MMMM_MMMM_PPPP_CCSS

    clock_src: ClockSource,
    crystal: CrystalMode,
    vco_div: u4,
    vco_mul: u10,
    xi_div: u6,
    pll_on: bool,
    _padding: u3 = 0,
    tag: u4 = 0b0000,
};

pub const functions = define.namespace(.{
    .Hub = define.namespace(.{
        .reboot = define.function(struct {
            pub const docs =
                \\Hard reset, reboots chip
            ;

            pub const params = .{};

            pub fn invoke() u32 {

                // %0001_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx
                return 0x1000_0000;
            }
        }),

        .seedRng = define.function(struct {
            pub const docs =
                \\Seed Xoroshiro128 PRNG with `seed`
            ;

            pub const params = .{
                .seed = .{ .docs = "The seed to use for the RNG" },
            };

            pub fn invoke(seed: u31) u32 {
                return @as(u32, seed) | 0x8000_0000;
            }
        }),

        .debugConfig = define.function(struct {
            pub const docs =
                \\Change the debug configuration of hub and cogs.
            ;

            pub const params = .{
                .debug_enable = .{ .docs = "Debug interrupt enables for cogs 15..0, respectively" },
                .write_protect = .{
                    .docs = "Write-protect last 16KB of hub RAM",
                    .default = false,
                },
                .lock = .{
                    .docs = "Lock W and `write_protect` bit settings until next reset",
                    .default = false,
                },
            };

            pub fn invoke(debug_enable: u16, write_protect: bool, lock: bool) u32 {
                return @bitCast(DebugConfig{
                    .debug_enable = debug_enable,
                    .write_protect = write_protect,
                    .locked = lock,
                });
            }
        }),

        .setFilter = define.function(struct {
            pub const docs =
                \\Set `filter` to fifo count `length` (2,3,5 or 8) and `tap` bit (0..31).
            ;

            pub const params = .{
                .filter = .{ .docs = "The filter to be changed" },
                .length = .{ .docs = "Length of the shift register used for filtering" },
                .tap = .{ .docs = "The bit of CNT that is used to advance the shift register." },
            };

            pub fn invoke(filter: FilterName, length: i64, tap: u5) !u32 {
                const enc_len: u2 = switch (length) {
                    2 => 0,
                    3 => 1,
                    5 => 2,
                    8 => 3,
                    else => return error.InvalidArg,
                };
                return @bitCast(FilterConfig{
                    .filter = filter,
                    .length = enc_len,
                    .tap = tap,
                });
            }
        }),

        .clockMode = define.function(struct {
            pub const docs = "Computes a hub clock configuration";

            pub const params = .{
                .pll = .{ .docs = "If true, the PLL will be enabled" },
                .in_div = .{ .docs = "Input divider of the crystal frequency", .min = 1, .max = 64 },
                .mul = .{ .docs = "Multiplier of the crystal frequency", .min = 1, .max = 1024 },
                .out_div = .{ .docs = "Divider of the PLL frequency" },
                .xi = .{ .docs = "Selects the crystal pin mode" },
                .sysclk = .{ .docs = "Selects which clock drives the system" },
            };

            pub fn invoke(
                pll: bool,
                in_div: std.math.IntFittingRange(1, 64),
                mul: std.math.IntFittingRange(1, 1024),
                out_div: i64,
                xi: CrystalMode,
                sysclk: ClockSource,
            ) !u32 {
                const vco_div: u4 = switch (out_div) {
                    2 => 0,
                    4 => 1,
                    6 => 2,
                    8 => 3,
                    10 => 4,
                    12 => 5,
                    14 => 6,
                    16 => 7,
                    18 => 8,
                    20 => 9,
                    22 => 10,
                    24 => 11,
                    26 => 12,
                    28 => 13,
                    3 => 14,
                    1 => 15,
                    else => return error.InvalidArg,
                };

                return @bitCast(ClockMode{
                    .pll_on = pll,
                    .crystal = xi,
                    .xi_div = @intCast(in_div - 1),
                    .vco_mul = @intCast(mul - 1),
                    .vco_div = vco_div,
                    .clock_src = sysclk,
                });
            }
        }),
    }),

    .SmartPin = define.namespace(.{
        .UartTx = define.namespace(.{
            //
        }),

        .UartRx = define.namespace(.{
            //
        }),
    }),
});
