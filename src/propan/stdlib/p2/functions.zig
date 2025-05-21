const std = @import("std");
const eval = @import("../eval.zig");
const define = @import("../define.zig");

pub const DebugConfig = packed struct(u32) {
    debug_enable: u16,
    write_protect: bool,
    locked: bool,
    _pad: u10 = 0,
    tag: u4 = 0b0010,
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
                .seed = .{
                    .docs = "The seed to use for the RNG",
                },
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
                .debug_enable = .{
                    .docs = "Debug interrupt enables for cogs 15..0, respectively",
                },
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
