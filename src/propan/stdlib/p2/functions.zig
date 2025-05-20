const std = @import("std");
const eval = @import("../eval.zig");
const define = @import("../define.zig");

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
