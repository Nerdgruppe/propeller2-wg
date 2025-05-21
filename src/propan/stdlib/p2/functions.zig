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

            pub fn invoke(filter: FilterName, length: u32, tap: u5) !u32 {
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
                out_div: u32,
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
            .config = config_uart_rx_tx,
        }),

        .UartRx = define.namespace(.{
            .config = config_uart_rx_tx,
        }),
    }),
});

const config_uart_rx_tx = define.function(struct {
    pub const docs = "";

    pub const params = .{
        .baud = .{ .docs = "The baud rate in symbols/s" },
        .clk = .{ .docs = "The cpu clock in Hz" },
        .bits = .{ .docs = "Number of bits", .min = 1, .max = 32, .default = 8 },
    };

    pub fn invoke(baud: u64, clk: u64, bits: u6) !u32 {
        std.debug.assert(bits >= 1 and bits <= 32);

        // X[31:16] establishes the number of clocks in a bit period, and in case X[31:26] is zero, X[15:10]
        // establishes the number of fractional clocks in a bit period. The X bit period value can be simply computed
        // as: (clocks * $1_0000) & $FFFFFC00. For example, 7.5 clocks would be $00078000, and 33.33 clocks
        // would be $00215400.

        // Use float here to support fractional divisions:
        const clk_f: f64 = @floatFromInt(clk);
        const baud_f: f64 = @floatFromInt(baud);

        const clocks_f = clk_f / baud_f;

        const fract_clocks_f = clocks_f * 0x1_0000;
        if (fract_clocks_f < 0 or fract_clocks_f > std.math.maxInt(u32))
            return error.Overflow;

        const fract_clocks: u32 = @intFromFloat(fract_clocks_f);

        // Cast back after multiplying with the hex value:
        return (fract_clocks & 0xFFFFFC00) | (bits - 1);
    }
});
