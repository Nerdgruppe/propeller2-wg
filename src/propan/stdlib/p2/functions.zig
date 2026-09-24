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

pub const AdcMode = enum(u2) {
    sample = 0,
    sinc2 = 1,
    sinc3 = 2,
    bits = 3,
};

pub const ScopeFilter = enum(u2) {
    tukey68 = 0,
    tukey45 = 1,
    hann28 = 2,
};

pub const MeasureSensitivity = enum(u2) {
    high = 0,
    rise = 1,
    edge = 2,
};

pub const PinEvent = enum(u3) {
    rise = 1,
    fall = 2,
    change = 3,
    low = 4,
    high = 6,
};

pub const LockEvent = enum(u2) {
    rise = 1,
    fall = 2,
    change = 3,
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
    .regoffset = define.function(struct {
        pub const docs = "Computes a bit range including low and high.";

        pub const params = .{
            .reg = .{ .docs = "The base register to be used" },
            .offset = .{ .docs = "Relative offset between -512 and +511" },
        };

        pub fn invoke(reg: eval.Register, offset: i10) !eval.Register {
            const new: u9 = @intCast(
                @mod(@as(i10, @intFromEnum(reg)) +% offset, std.math.maxInt(u9)),
            );

            return @enumFromInt(new);
        }
    }),

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
            .clk = .{ .docs = "The frequency used to calculated the number of clock periods." },
            .s = .{ .docs = "The integer number of seconds to wait.", .default = 0 },
            .ms = .{ .docs = "The integer number of milli seconds to wait.", .default = 0 },
            .us = .{ .docs = "The integer number of micro seconds to wait.", .default = 0 },
            .ns = .{ .docs = "The integer number of nano seconds to wait.", .default = 0 },
            .waitx = .{ .docs = "If set `true` will subtract the required 2 clocks", .default = false },
            .round = .{ .docs = "Selects how the result will be rounded.", .default = .nearest },
        };

        pub fn invoke(ctx: EvalContext, clk: u32, s: u64, ms: u64, us: u64, ns: u64, waitx: bool, rounding: RoundingMode) !u32 {
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
                try ctx.emit_error("A delay of {} periods ({f}) cannot be represented with 32 bits.", .{
                    clocks_u64,
                    std.Io.Duration.fromNanoseconds(delay_ns),
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
            pub const docs = "Compute a HUBSET clock word. Enable a crystal/PLL while still using RCFAST, allow it to stabilize, then select XI or PLL as the source.";

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
                if ((pll or sysclk == .xi) and xi == .float)
                    return error.InvalidArg;
                if (sysclk == .pll and !pll)
                    return error.InvalidArg;

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
                    30 => 14,
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

        .fifoConfig = define.function(struct {
            pub const docs = "Pack RDFAST/WRFAST D: 64-byte wrap blocks in bits 13:0 and optional no-wait in bit 31. Zero blocks disables wrapping. With no-wait, allow setup time before using the FIFO.";
            pub const params = .{
                .blocks = .{ .docs = "Number of 64-byte blocks before wrapping; zero disables wrapping." },
                .no_wait = .{ .docs = "Return without waiting for FIFO setup.", .default = false },
            };

            pub fn invoke(blocks: u14, no_wait: bool) u32 {
                return @as(u32, blocks) | (@as(u32, @intFromBool(no_wait)) << 31);
            }
        }),
    }),

    .SmartPin = define.namespace(.{
        .Pulse = define.namespace(.{
            .config = define.function(struct {
                pub const docs = "Pack pulse-mode X: base period in bits 15:0 and high-time threshold in bits 31:16.";
                pub const params = .{
                    .base = .{ .docs = "Base period in clocks." },
                    .threshold = .{ .docs = "High-time comparison threshold." },
                };
                pub fn invoke(base: u16, threshold: u16) u32 {
                    return pack_x_halves(base, threshold);
                }
            }),
        }),

        .Pwm = define.namespace(.{
            .config = define.function(struct {
                pub const docs = "Pack PWM triangle, sawtooth, or SMPS X: base period and frame count.";
                pub const params = .{
                    .base = .{ .docs = "Clocks per base period." },
                    .frame = .{ .docs = "Base periods per PWM frame." },
                };
                pub fn invoke(base: u16, frame: u16) u32 {
                    return pack_x_halves(base, frame);
                }
            }),
        }),

        .Nco = define.namespace(.{
            .config = define.function(struct {
                pub const docs = "Pack NCO frequency or duty X: base period and initial phase.";
                pub const params = .{
                    .base = .{ .docs = "Clocks per base period." },
                    .phase = .{ .docs = "Initial upper 16 phase bits." },
                };
                pub fn invoke(base: u16, phase: u16) u32 {
                    return pack_x_halves(base, phase);
                }
            }),
        }),

        .SyncTx = define.namespace(.{
            .config = define.function(struct {
                pub const docs = "Pack synchronous TX X: word length and optional start-stop mode.";
                pub const params = .{
                    .bits = .{ .docs = "Bits per word, 1 through 32.", .min = 1, .max = 32 },
                    .start_stop = .{ .docs = "Use start-stop instead of continuous mode.", .default = false },
                };
                pub fn invoke(bits: u6, start_stop: bool) u32 {
                    return (bits - 1) | (@as(u32, @intFromBool(start_stop)) << 5);
                }
            }),
        }),

        .SyncRx = define.namespace(.{
            .config = define.function(struct {
                pub const docs = "Pack synchronous RX X: word length and input sample position.";
                pub const params = .{
                    .bits = .{ .docs = "Bits per word, 1 through 32.", .min = 1, .max = 32 },
                    .on_edge = .{ .docs = "Sample on the registered B edge instead of just before it.", .default = false },
                };
                pub fn invoke(bits: u6, on_edge: bool) u32 {
                    return (bits - 1) | (@as(u32, @intFromBool(on_edge)) << 5);
                }
            }),
        }),

        .Adc = define.namespace(.{
            .config = define.function(struct {
                pub const docs = "Pack ADC X: acquisition mode in bits 5:4 and log2 sample period in bits 3:0.";
                pub const params = .{
                    .mode = .{ .docs = "Sampling, SINC2, SINC3, or raw bit capture." },
                    .period_exp = .{ .docs = "Log2 of the initial sample period in clocks." },
                };
                pub fn invoke(mode: AdcMode, period_exp: u4) !u32 {
                    const max_exp: u4 = switch (mode) {
                        .sample, .sinc2 => 13,
                        .sinc3 => 9,
                        .bits => 5,
                    };
                    if (period_exp > max_exp) return error.InvalidArg;
                    return (@as(u32, @intFromEnum(mode)) << 4) | period_exp;
                }
            }),
        }),

        .Scope = define.namespace(.{
            .config = define.function(struct {
                pub const docs = "Pack ADC scope X: B trigger in bits 15:10, A trigger in bits 7:2, and filter in bits 1:0.";
                pub const params = .{
                    .b = .{ .docs = "B trigger level, 0 through 63." },
                    .a = .{ .docs = "A trigger level, 0 through 63." },
                    .filter = .{ .docs = "Scope sample filter." },
                };
                pub fn invoke(b: u6, a: u6, filter: ScopeFilter) u32 {
                    return (@as(u32, b) << 10) | (@as(u32, a) << 2) | @intFromEnum(filter);
                }
            }),

            .pipeConfig = define.function(struct {
                pub const docs = "Pack SETSCP D for a four-pin-aligned scope block.";
                pub const params = .{
                    .base_pin = .{ .docs = "First pin of the four-pin block, 0, 4, ..., 60." },
                    .enabled = .{ .docs = "Enable the scope data pipe.", .default = true },
                };
                pub fn invoke(base_pin: u6, enabled: bool) !u32 {
                    if (base_pin % 4 != 0) return error.InvalidArg;
                    return @as(u32, base_pin) | (@as(u32, @intFromBool(enabled)) << 6);
                }
            }),
        }),

        .Usb = define.namespace(.{
            .config = define.function(struct {
                pub const docs = "Pack USB pair X for the lower even pin: host/full-speed flags and a 16-bit clock fraction.";
                pub const params = .{
                    .baud = .{ .docs = "USB symbol rate in symbols per second." },
                    .clk = .{ .docs = "System clock in Hz." },
                    .host = .{ .docs = "Select host mode.", .default = false },
                    .full_speed = .{ .docs = "Select full-speed instead of low-speed.", .default = false },
                };
                pub fn invoke(baud: u32, clk: u32, host: bool, full_speed: bool) !u32 {
                    if (baud == 0 or clk == 0 or @as(u64, baud) * 4 >= clk)
                        return error.InvalidArg;
                    const fraction = (@as(u64, baud) << 16) / clk;
                    if (fraction == 0) return error.InvalidArg;
                    return @as(u32, @intCast(fraction)) |
                        (@as(u32, @intFromBool(full_speed)) << 14) |
                        (@as(u32, @intFromBool(host)) << 15);
                }
            }),
        }),

        .Measure = define.namespace(.{
            .eventY = define.function(struct {
                pub const docs = "Pack P_EVENTS_TICKS Y: A-input sensitivity and optional timeout mode.";
                pub const params = .{
                    .sensitivity = .{ .docs = "Count A highs, rises, or edges." },
                    .timeout = .{ .docs = "Raise IN after X clocks without an event.", .default = false },
                };
                pub fn invoke(sensitivity: MeasureSensitivity, timeout: bool) u32 {
                    return @as(u32, @intFromEnum(sensitivity)) | (@as(u32, @intFromBool(timeout)) << 2);
                }
            }),

            .periodY = define.function(struct {
                pub const docs = "Pack measurement Y for P_PERIODS_* and P_COUNTER_*: select rises or either edge for A and B.";
                pub const params = .{
                    .a_edge = .{ .docs = "Trigger A on either edge instead of rises.", .default = false },
                    .b_edge = .{ .docs = "Trigger B on either edge instead of rises.", .default = false },
                };
                pub fn invoke(a_edge: bool, b_edge: bool) u32 {
                    return (@as(u32, @intFromBool(a_edge)) << 1) | @intFromBool(b_edge);
                }
            }),
        }),

        .UartTx = define.namespace(.{
            .config = config_uart_rx_tx,
        }),

        .UartRx = define.namespace(.{
            .config = config_uart_rx_tx,
        }),
    }),

    .Event = define.namespace(.{
        .pin = define.function(struct {
            pub const docs = "Pack a pin event selector for SETSE1 through SETSE4.";
            pub const params = .{
                .pin = .{ .docs = "Pin number, 0 through 63." },
                .kind = .{ .docs = "Rise, fall, change, low, or high." },
            };
            pub fn invoke(pin: u6, kind: PinEvent) u32 {
                return (@as(u32, @intFromEnum(kind)) << 6) | pin;
            }
        }),

        .lock = define.function(struct {
            pub const docs = "Pack a hub-lock event selector for SETSE1 through SETSE4.";
            pub const params = .{
                .lock = .{ .docs = "Hub lock number, 0 through 15." },
                .kind = .{ .docs = "Rise, fall, or change." },
            };
            pub fn invoke(lock: u4, kind: LockEvent) u32 {
                return (@as(u32, @intFromEnum(kind)) << 4) | lock;
            }
        }),

        .lut = define.function(struct {
            pub const docs = "Pack a LUT read/write event selector for addresses $1FC through $1FF.";
            pub const params = .{
                .address = .{ .docs = "LUT address $1FC through $1FF.", .min = 0x1FC, .max = 0x1FF },
                .write = .{ .docs = "Select writes instead of reads.", .default = false },
                .companion = .{ .docs = "Observe the odd/even companion cog instead of this cog.", .default = false },
            };
            pub fn invoke(address: u9, write: bool, companion: bool) u32 {
                return @as(u32, address & 3) |
                    (@as(u32, @intFromBool(write)) << 2) |
                    (@as(u32, @intFromBool(companion)) << 3);
            }
        }),
    }),
});

fn pack_x_halves(low: u16, high: u16) u32 {
    return @as(u32, low) | (@as(u32, high) << 16);
}

const config_uart_rx_tx = define.function(struct {
    pub const docs = "Computes a UART smart mode configuration for register X";

    pub const params = .{
        .baud = .{ .docs = "The baud rate in symbols/s" },
        .clk = .{ .docs = "The cpu clock in Hz" },
        .bits = .{ .docs = "Number of bits", .min = 1, .max = 32, .default = 8 },
    };

    pub fn invoke(baud: u64, clk: u64, bits: u6) !u32 {
        if (baud == 0 or clk == 0)
            return error.InvalidArg;

        // X[31:16] is the whole clocks per bit; X[15:10] supplies fractional clocks.
        const scaled = (@as(u128, clk) << 16) / baud;
        if (scaled == 0 or scaled > std.math.maxInt(u32))
            return error.Overflow;
        return (@as(u32, @intCast(scaled)) & 0xFFFF_FC00) | (bits - 1);
    }
});

test "invalid P2 configuration words are rejected" {
    try expect_invalid_config("Hub.clockMode", &.{
        .int(0), .int(1), .int(1), .int(3), .enumerator("float"), .enumerator("rcfast"),
    });
    try expect_invalid_config("Hub.clockMode", &.{
        .int(0), .int(1), .int(1), .int(1), .enumerator("float"), .enumerator("pll"),
    });
    try expect_invalid_config("SmartPin.UartTx.config", &.{ .int(0), .int(80_000_000), .int(8) });
    try expect_invalid_config("SmartPin.Usb.config", &.{ .int(20_000_000), .int(80_000_000), .int(1), .int(1) });
    try expect_invalid_config("SmartPin.Adc.config", &.{ .enumerator("bits"), .int(6) });
    try expect_invalid_config("SmartPin.Scope.pipeConfig", &.{ .int(3), .int(1) });
    try expect_invalid_config("Event.lut", &.{ .int(0x1FB), .int(0), .int(0) });
    try expect_invalid_config("SmartPin.SyncTx.config", &.{ .int(0), .int(0) });

    const uart = functions.get("SmartPin.UartTx.config").?;
    try std.testing.expectError(error.Overflow, uart.invoke(undefined, &.{ .int(1), .int(100_000_000), .int(8) }));
}

fn expect_invalid_config(name: []const u8, args: []const eval.Value) !void {
    const function = functions.get(name).?;
    try std.testing.expectError(error.InvalidArg, function.invoke(undefined, args));
}
