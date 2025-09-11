const std = @import("std");
const eval = @import("../eval.zig");
const define = @import("../define.zig");
const EvalContext = define.EvalContext;

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

pub const BitField = packed struct(u10) {
    // Make bitfield, (x & $1F) | (y & $1F) << 5
    base: u5,
    extra_bits: u5,
};

pub const PinField = packed struct(u11) {
    // x ADDPINS y: Make pinfield, (x & $3F) | (y & $1F) << 6
    base: u6,
    extra_pins: u5,
};

pub const OptionalBoolean = enum {
    unset,
    true,
    false,
    yes,
    no,
    on,
    off,
    @"1",
    @"0",

    pub fn as_bool(trool: OptionalBoolean) ?bool {
        return switch (trool) {
            .unset => null,
            .@"0", .false, .no, .off => false,
            .@"1", .true, .yes, .on => true,
        };
    }
};

pub const functions = define.namespace(.{
    .bitrange = define.function(struct {
        pub const docs = "Computes a bit range including low and high.";

        pub const params = .{
            .low = .{ .docs = "The index of the lowest bit in the range." },
            .high = .{ .docs = "The index of the highest bit in the range." },
        };

        pub fn invoke(low: u5, high: u5) !u10 {
            if (high < low)
                return error.InvalidArg; // TODO: Diagnostic
            return @bitCast(BitField{
                .base = low,
                .extra_bits = high - low,
            });
        }
    }),

    .pinrange = define.function(struct {
        pub const docs = "Computes a bit range including low and high.";

        pub const params = .{
            .start = .{ .docs = "The index of the first pin of the range." },
            .end = .{ .docs = "The index of the last pin of the range." },
            .wrap = .{ .docs = "If not set, will emit an error diagnostic if the pins wrap at an unexpected location", .default = .unset },
        };

        pub fn invoke(ctx: EvalContext, start: u6, end: u6, wrap: OptionalBoolean) !u11 {
            if (start == end)
                return start;

            const start_grp = start / 32;
            const end_grp = end / 32;

            if (start_grp != end_grp) {
                return ctx.fatal_error("Pins {} and {} are not in the same pin group", .{ start, end });
            }

            if (start > end) {
                if (wrap.as_bool() == null) {
                    try ctx.emit_warning("The pin range from {} to {} wraps inside its register. Add wrap=#on to mute this, or wrap=#off to make it an error.", .{ start, end });
                } else if (wrap.as_bool() == false) {
                    try ctx.emit_error("The pin range from {} to {} wraps inside its register.", .{ start, end });
                }
            }

            const extra_count: u5 = @intCast(if (start > end)
                (@as(u7, 32) + end) - start
            else
                end - start);

            return @bitCast(PinField{
                .base = start,
                .extra_pins = extra_count,
            });
        }
    }),

    .ticks = define.function(struct {
        pub const RoundingMode = enum {
            floor,
            ceil,
            nearest,
        };

        pub const docs = "Computes the number of clock periods required to delay a certain amount of time based on a 'clk' frequency.";

        pub const params = .{
            .s = .{ .docs = "The integer number of seconds to wait.", .default = 0 },
            .ms = .{ .docs = "The integer number of milli seconds to wait.", .default = 0 },
            .us = .{ .docs = "The integer number of micro seconds to wait.", .default = 0 },
            .ns = .{ .docs = "The integer number of nano seconds to wait.", .default = 0 },
            .clk = .{ .docs = "The frequency used to calculated the number of clock periods.", .default = 0 },
            .waitx = .{ .docs = "If set `true` will subtract the required 2 clocks", .default = false },
            .round = .{ .docs = "Selects how the result will be rounded.", .default = .nearest },
        };

        pub fn invoke(ctx: EvalContext, s: u64, ms: u64, us: u64, ns: u64, clk: u32, waitx: bool, rounding: RoundingMode) !u32 {
            const delay_ns = std.time.ns_per_s * s +
                std.time.ns_per_ms * ms +
                std.time.ns_per_us * us +
                ns;

            const rounding_offset: u64 = switch (rounding) {
                .floor => 0,
                .nearest => std.time.ns_per_s / 2,
                .ceil => std.time.ns_per_s - 1,
            };

            var clocks_u64: u64 = (delay_ns * clk + rounding_offset) / std.time.ns_per_s;

            if (waitx) {
                if (clocks_u64 < 2) {
                    try ctx.emit_warning("Requested delay time is less than 2 periods. It's recommended to remove the WAITX in question.", .{});
                }
                clocks_u64 -|= 2;
            }

            return std.math.cast(u32, clocks_u64) orelse {
                try ctx.emit_error("A delay of {} periods ({:.3}) cannot be represented with 32 bits.", .{
                    clocks_u64,
                    std.fmt.fmtDuration(delay_ns),
                });
                return std.math.maxInt(u32);
            };
        }
    }),

    .Hub = define.namespace(.{
        .reboot = define.function(struct {
            pub const docs = "Hard reset, reboots chip";

            pub const params = .{};

            pub fn invoke() u32 {

                // %0001_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx_xxxx
                return 0x1000_0000;
            }
        }),

        .seedRng = define.function(struct {
            pub const docs = "Seed Xoroshiro128 PRNG with `seed`";

            pub const params = .{
                .seed = .{ .docs = "The seed to use for the RNG" },
            };

            pub fn invoke(seed: u31) u32 {
                return @as(u32, seed) | 0x8000_0000;
            }
        }),

        .debugConfig = define.function(struct {
            pub const docs = "Change the debug configuration of hub and cogs.";

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
                    else => return error.InvalidArg, // TODO: Diagnostic
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
                    else => return error.InvalidArg, // TODO: Diagnostic
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
    pub const docs = "Computes a UART smart mode configuration for register X";

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
