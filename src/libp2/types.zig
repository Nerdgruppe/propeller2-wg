//!
//! This file implements the basic types used by the Propeller 2.
//!
const std = @import("std");

/// HUBSET CC: XI/XO drive and crystal loading capacitors.
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

/// HUBSET clock configuration: %0000_000E_DDDD_DDMM_MMMM_MMMM_PPPP_CCSS.
/// XI division and VCO multiplication encode their factors minus one.
/// VCO division is 2*(vco_div+1), except 15 selects division by one.
pub const ClockMode = packed struct(u32) {
    clock_src: ClockSource,
    crystal: CrystalMode,
    vco_div: u4,
    vco_mul: u10,
    xi_div: u6,
    pll_on: bool,
    _padding: u3 = 0,
    tag: u4 = 0b0000,

    /// Return the nominal system frequency in Hz; RCFAST/RCSLOW use 20 MHz/20 kHz.
    /// Reject non-clock HUBSET operations, disabled clock sources, and unrepresentable frequencies.
    /// Crystal/PLL settling and RC oscillator drift are not modeled.
    pub fn get_frequency(mode: ClockMode, crystal_freq: u64) error{InvalidClockMode}!u64 {
        if (mode.tag != 0 or mode._padding != 0) return error.InvalidClockMode;
        if ((mode.clock_src == .xi or mode.clock_src == .pll) and mode.crystal == .float) return error.InvalidClockMode;
        if (mode.clock_src == .pll and !mode.pll_on) return error.InvalidClockMode;
        const frequency: u64 = switch (mode.clock_src) {
            .rcfast => 20_000_000,
            .rcslow => 20_000,
            .xi => crystal_freq,
            .pll => blk: {
                const divider = @as(u64, mode.xi_div) + 1;
                const multiplier = @as(u64, mode.vco_mul) + 1;
                const post_divider: u64 = if (mode.vco_div == 15) 1 else 2 * (@as(u64, mode.vco_div) + 1);
                break :blk std.math.cast(u64, @as(u128, crystal_freq) * multiplier / (divider * post_divider)) orelse return error.InvalidClockMode;
            },
        };
        if (frequency == 0) return error.InvalidClockMode;
        return frequency;
    }
};

test "clock mode frequencies follow HUBSET encodings" {
    inline for (.{
        .{ 0xf0, 20_000_000 },
        .{ 0xf1, 20_000 },
        .{ 0xfe, 20_000_000 },
        .{ 0x0100_09fb, 200_000_000 },
        // Hardware manual's 20 MHz / 40 * 297 example, with post-divider bypass and /2.
        .{ 0x019d_28fb, 148_500_000 },
        .{ 0x019d_280b, 74_250_000 },
    }) |case| {
        const mode: ClockMode = @bitCast(@as(u32, case[0]));
        try std.testing.expectEqual(@as(u64, case[1]), try mode.get_frequency(20_000_000));
    }
    const pll: ClockMode = @bitCast(@as(u32, 0x0100_09fb));
    try std.testing.expectEqual(@as(u64, 120_000_000), try pll.get_frequency(12_000_000));
    try std.testing.expectError(error.InvalidClockMode, pll.get_frequency(0));
    try std.testing.expectError(error.InvalidClockMode, pll.get_frequency(std.math.maxInt(u64)));
    for ([_]u32{ 0x1000_00f0, 0x0200_00f0, 2, 3, 0xfb }) |raw| {
        const mode: ClockMode = @bitCast(raw);
        try std.testing.expectError(error.InvalidClockMode, mode.get_frequency(20_000_000));
    }
}

/// WRPIN D[5:1] smart-engine selection (%SSSSS).
/// Encodings 1..3 are long repositories unless pin configuration enables DAC_MODE.
pub const SmartPinMode = enum(u5) {
    /// Normal I/O; the smart engine is bypassed.
    gpio = 0b00000,
    /// Long repository, or DAC noise when DAC_MODE is enabled.
    repository = 0b00001,
    /// 16-bit DAC with random dither; long repository without DAC_MODE.
    dac_dither_rnd = 0b00010,
    /// 16-bit DAC with PWM dither; long repository without DAC_MODE.
    dac_dither_pwm = 0b00011,
    /// Pulse/cycle output.
    pulse = 0b00100,
    /// Transition output.
    transition = 0b00101,
    /// NCO frequency output.
    nco_frequency = 0b00110,
    /// NCO duty output.
    nco_duty = 0b00111,
    /// Triangle PWM output.
    pwm_triangle = 0b01000,
    /// Sawtooth PWM output.
    pwm_sawtooth = 0b01001,
    /// SMPS PWM with voltage and current feedback.
    pwm_smps = 0b01010,
    /// A/B quadrature encoder.
    quadrature = 0b01011,
    /// Count A rises when B is high.
    reg_up = 0b01100,
    /// Count A rises with B selecting increment or decrement.
    reg_up_down = 0b01101,
    /// Count A rises; Y[0] optionally selects A-rise minus B-rise counting.
    count_rises = 0b01110,
    /// Count A highs; Y[0] optionally selects A-high minus B-high counting.
    count_highs = 0b01111,
    /// Measure the duration of each A state.
    state_ticks = 0b10000,
    /// Measure the duration of each A high state.
    high_ticks = 0b10001,
    /// Time X A highs/rises/edges, or timeout on missing events (Y[2]).
    events_ticks = 0b10010,
    /// Count clock cycles across X A-to-B periods.
    periods_ticks = 0b10011,
    /// Count A high states across X A-to-B periods.
    periods_highs = 0b10100,
    /// Count clock cycles across complete periods spanning at least X clocks.
    counter_ticks = 0b10101,
    /// Count A high states across complete periods spanning at least X clocks.
    counter_highs = 0b10110,
    /// Count complete A-to-B periods spanning at least X clocks.
    counter_periods = 0b10111,
    /// ADC sample/filter/capture with internal clocking.
    adc = 0b11000,
    /// ADC sample/filter/capture with external clocking.
    adc_ext = 0b11001,
    /// ADC scope with trigger.
    adc_scope = 0b11010,
    /// USB host/device on an odd/even pin pair.
    usb_pair = 0b11011,
    /// Synchronous serial transmit.
    sync_tx = 0b11100,
    /// Synchronous serial receive.
    sync_rx = 0b11101,
    /// Asynchronous serial transmit.
    uart_tx = 0b11110,
    /// Asynchronous serial receive.
    uart_rx = 0b11111,

    pub const dac_noise: SmartPinMode = .repository;
};

/// Index of a smart engine and its associated cog register bit.
pub const PinIndex = enum(u6) { _ };
/// Index of a physical pad in the I/O ring.
pub const PadIndex = enum(u6) { _ };

/// WRPIN configuration, in hardware bit order: %AAAA_BBBB_FFF_MMMMMMMMMMMMM_TT_SSSSS_0.
/// The low bit selects acknowledgement instead of configuration on the command bus.
pub const PinConfiguration = packed struct(u32) {
    acknowledge: bool = false,
    mode: SmartPinMode = .gpio,
    output_enable: bool = false,
    output_source: enum(u1) { out, other } = .out,
    low_drive: Drive = .fast,
    high_drive: Drive = .fast,
    invert_output: bool = false,
    invert_input: bool = false,
    synchronous: bool = false,
    pad_mode: PadMode = .logic,
    input_logic: InputLogic = .a,
    b: Selector = .{},
    a: Selector = .{},

    pub const Drive = enum(u3) { fast, resistor_1k5, resistor_15k, resistor_150k, current_1ma, current_100ua, current_10ua, float };
    /// The upper four bits of the physical configuration. ADC/DAC modes reinterpret lower bits.
    pub const PadMode = enum(u4) {
        logic,
        logic_feedback,
        logic_adjacent_feedback,
        schmitt,
        schmitt_feedback,
        schmitt_adjacent_feedback,
        compare,
        compare_feedback,
        adc_gio_vio_float_1x,
        adc_3x_10x_30x_100x,
        dac_990r_600r,
        dac_124r_75r,
        level,
        level_negative_feedback,
        level_adjacent_positive_feedback,
        level_adjacent_negative_feedback,
    };
    pub const InputLogic = enum(u3) { a, a_and_b, a_or_b, a_xor_b, filter0, filter1, filter2, filter3 };
    pub const Selector = packed struct(u4) {
        source: enum(u3) { local, plus1, plus2, plus3, out, minus3, minus2, minus1 } = .local,
        invert: bool = false,
    };
};

/// Hardware event IDs used by polling, waiting, branching, and interrupt selection.
pub const EventId = enum(u4) {
    /// Interrupt 1, 2, or 3 occurred; debug interrupts are excluded.
    /// When used with SETINT1/SETINT2/SETINT3, ID 0 disables the interrupt source.
    INT = 0,
    /// The low 32 bits of the global counter reached or passed the target set by ADDCT1.
    CT1 = 1,
    /// The low 32 bits of the global counter reached or passed the target set by ADDCT2.
    CT2 = 2,
    /// The low 32 bits of the global counter reached or passed the target set by ADDCT3.
    CT3 = 3,
    /// The pin, LUT access, or hub lock event selected by SETSE1 occurred.
    SE1 = 4,
    /// The pin, LUT access, or hub lock event selected by SETSE2 occurred.
    SE2 = 5,
    /// The pin, LUT access, or hub lock event selected by SETSE3 occurred.
    SE3 = 6,
    /// The pin, LUT access, or hub lock event selected by SETSE4 occurred.
    SE4 = 7,
    /// INA or INB matched or mismatched the masked pin pattern configured by SETPAT.
    PAT = 8,
    /// The hub RAM FIFO exhausted its block count and reloaded its start address and block count.
    FBW = 9,
    /// The streamer's command buffer is empty and ready to accept another command.
    XMT = 10,
    /// The streamer finished executing its commands and became idle.
    XFI = 11,
    /// The streamer's numerically controlled oscillator (NCO) rolled over.
    XRO = 12,
    /// The streamer read LUT (lookup RAM) address 0x1FF.
    XRL = 13,
    /// This cog received an attention request issued by COGATN.
    ATN = 14,
    /// GETQX or GETQY executed with no CORDIC results available or operations in progress.
    QMT = 15,

    pub fn mask(event: EventId) u16 {
        return @as(u16, 1) << @intFromEnum(event);
    }
};

pub const Flag = enum(u1) {
    unset = 0,
    set = 1,

    pub fn from_bool(value: bool) Flag {
        return @enumFromInt(@intFromBool(value));
    }

    pub fn from_lsb(value: u32) Flag {
        return @enumFromInt(Decomposition.from(value).view.lsb);
    }

    pub fn from_msb(value: u32) Flag {
        return @enumFromInt(Decomposition.from(value).view.msb);
    }

    pub fn from_zero(value: u32) Flag {
        return from_bool(value == 0);
    }

    pub fn from_parity(value: u32) Flag {
        return @enumFromInt(@popCount(value) & 1);
    }

    pub fn as_int(flag: Flag, comptime T: type) T {
        return @intFromEnum(flag);
    }

    pub fn as_mask(flag: Flag, comptime T: type) T {
        return switch (flag) {
            .unset => 0,
            .set => ~@as(T, 0),
        };
    }

    pub fn not(flag: Flag) Flag {
        return switch (flag) {
            .unset => .set,
            .set => .unset,
        };
    }
};

/// A 32-bit wide array of `T` elements.
pub fn BitArray(comptime T: type) type {
    return packed struct(u32) {
        const Array = @This();

        pub const Index = std.math.IntFittingRange(0, len - 1);

        pub const len = @divExact(32, @bitSizeOf(T));
        pub const zero: Array = .{ .raw = 0 };

        raw: u32,

        pub fn from_array(arr: [len]T) Array {
            var out: Array = .zero;
            for (arr, 0..) |v, i| {
                out.set(@intCast(i), v);
            }
            return out;
        }

        pub fn to_array(arr: Array) [len]T {
            var out: [len]T = @splat(0);
            for (&out, 0..) |*d, i| {
                d.* = arr.get(@intCast(i));
            }
            return out;
        }

        pub fn set(arr: *Array, index: Index, value: T) void {
            const shift = @bitSizeOf(T) * @as(u5, index);

            const mask = @as(u32, ~@as(T, 0)) << shift;

            arr.raw &= ~mask;
            arr.raw |= @as(u32, value) << shift;
        }

        pub fn get(arr: Array, index: Index) T {
            const shift = @bitSizeOf(T) * @as(u5, index);
            return @truncate(arr.raw >> shift);
        }
    };
}

/// A struct that allows viewing a `u32` as multiple different types.
pub const Decomposition = packed union(u32) {
    u32: u32,
    i32: i32,
    u16: BitArray(u16),
    u8: BitArray(u8),
    u4: BitArray(u4),
    u2: BitArray(u2),
    u1: BitArray(u1),
    view: packed struct(u32) {
        lsb: u1,
        center: u30,
        msb: u1,
    },

    pub fn from(value: u32) Decomposition {
        return .{ .u32 = value };
    }
};

/// RGBSQZ input / RGBEXP output: {R[7:0], G[7:0], B[7:0], A[7:0]}.
/// ADDPIX/MULPIX/BLNPIX/MIXPIX operate on all four byte channels.
pub const RGBA8888 = packed struct(u32) {
    a: u8,
    b: u8,
    g: u8,
    r: u8,
};

/// RGBSQZ output / RGBEXP input in D[15:0]: {R[4:0], G[5:0], B[4:0]}.
pub const RGB565 = packed struct(u16) {
    b: u5,
    g: u6,
    r: u5,
};

/// Kept for callers using the original name; RGB565 has no alpha channel.
pub const RGBA565 = RGB565;

/// S operand of BITL/BITH/BITC/BITNC/BITZ/BITNZ/BITRND/BITNOT:
/// D[S[9:5]+S[4:0]:S[4:0]]. Count is the number of *additional* bits (0..31).
/// Prior SETQ replaces count with Q[4:0].
/// TESTB/TESTBN (including AND/OR/XOR effects), ROR/ROL/SHR/SHL/RCR/RCL/SAR/SAL,
/// ZEROX/SIGNX and DECOD/BMASK use only index and ignore the remaining bits.
pub const BitIndexAndCount = packed struct(u32) {
    index: u5,
    count: u5,
    unused: u22,
};

/// DIRx/OUTx/FLTx/DRVx D operand and WRPIN/WXPIN/WYPIN/AKPIN S operand:
/// pins [index + count .. index], wrapping within the same 32-pin port.
/// Count is additional pins; prior SETQ replaces it with Q[4:0].
/// TESTP/TESTPN and RDPIN/RQPIN use only index.
pub const PinIndexAndCount = packed struct(u32) {
    index: u6,
    count: u5,
    unused: u21,
};

/// Nine-bit cog RAM address, with names for the architectural special registers.
pub const Register = enum(u9) {
    /// INT3 call address
    IJMP3 = 0x1F0,

    /// INT3 return address
    IRET3 = 0x1F1,

    /// INT2 call address
    IJMP2 = 0x1F2,

    /// INT2 return address
    IRET2 = 0x1F3,

    /// INT1 call address
    IJMP1 = 0x1F4,

    /// INT1 return address
    IRET1 = 0x1F5,

    /// Used with CALLPA, CALLD and LOC
    PA = 0x1F6,

    /// Used with CALLPB, CALLD and LOC
    PB = 0x1F7,

    /// Pointer A register
    PTRA = 0x1F8,

    /// Pointer B register
    PTRB = 0x1F9,

    /// I/O port A direction register
    DIRA = 0x1FA,

    /// I/O port B direction register
    DIRB = 0x1FB,

    /// I/O port A output register
    OUTA = 0x1FC,

    /// I/O port B output register
    OUTB = 0x1FD,

    /// I/O port A input register
    INA = 0x1FE,

    /// I/O port B input register
    INB = 0x1FF,
    _,
};

/// Instruction WC/WZ bit: preserve the flag or write the instruction result.
pub const FlagModifier = enum(u1) {
    keep = 0,
    write = 1,
};

/// MODC/MODZ/MODCZ truth table selecting a new flag value from C and Z.
pub const FlagExpression = enum(u4) {
    /// C/Z = 0
    clr = 0b0000,

    /// C/Z = !C AND !Z
    nc_and_nz = 0b0001,

    /// C/Z = !C AND Z
    nc_and_z = 0b0010,

    /// C/Z = !C
    nc = 0b0011,

    /// C/Z = C AND !Z
    c_and_nz = 0b0100,

    /// C/Z = !Z
    nz = 0b0101,

    /// C/Z = C NOT_EQUAL_TO Z
    c_ne_z = 0b0110,

    /// C/Z = !C OR !Z
    nc_or_nz = 0b0111,

    /// C/Z = C AND Z
    c_and_z = 0b1000,

    /// C/Z = C EQUAL_TO Z
    c_eq_z = 0b1001,

    /// C/Z = Z
    z = 0b1010,

    /// C/Z = !C OR Z
    nc_or_z = 0b1011,

    /// C/Z = C
    c = 0b1100,

    /// C/Z = C OR !Z
    c_or_nz = 0b1101,

    /// C/Z = C OR Z
    c_or_z = 0b1110,

    /// C/Z = 1
    set = 0b1111,
};

/// Instruction condition EEEE. Zero executes and returns; the all-zero word is NOP.
pub const Condition = enum(u4) {
    _RET_ = 0b0000, //  _RET_       Always execute and return (More Info)
    IF_NC_AND_NZ = 0b0001, //  IF_NC_AND_NZ IF_NZ_AND_NC IF_GT IF_00 Execute if C=0 AND Z=0
    IF_NC_AND_Z = 0b0010, //  IF_NC_AND_Z IF_Z_AND_NC   IF_01 Execute if C=0 AND Z=1
    IF_NC = 0b0011, //  IF_NC   IF_GE IF_0X Execute if C=0
    IF_C_AND_NZ = 0b0100, //  IF_C_AND_NZ IF_NZ_AND_C   IF_10 Execute if C=1 AND Z=0
    IF_NZ = 0b0101, //  IF_NZ   IF_NE IF_X0 Execute if Z=0
    IF_C_NE_Z = 0b0110, //  IF_C_NE_Z IF_Z_NE_C     Execute if C!=Z
    IF_NC_OR_NZ = 0b0111, //  IF_NC_OR_NZ IF_NZ_OR_NC     Execute if C=0 OR Z=0
    IF_C_AND_Z = 0b1000, //  IF_C_AND_Z IF_Z_AND_C   IF_11 Execute if C=1 AND Z=1
    IF_C_EQ_Z = 0b1001, //  IF_C_EQ_Z IF_Z_EQ_C     Execute if C=Z
    IF_Z = 0b1010, //  IF_Z   IF_E IF_X1 Execute if Z=1
    IF_NC_OR_Z = 0b1011, //  IF_NC_OR_Z IF_Z_OR_NC     Execute if C=0 OR Z=1
    IF_C = 0b1100, //  IF_C   IF_LT IF_1X Execute if C=1
    IF_C_OR_NZ = 0b1101, //  IF_C_OR_NZ IF_NZ_OR_C     Execute if C=1 OR Z=0
    IF_C_OR_Z = 0b1110, //  IF_C_OR_Z IF_Z_OR_C     Execute if C=1 OR Z=1
    IF_ALWAYS = 0b1111, // (empty) IF_ALWAYS     Always execute
};

/// CALLD/LOC's two-bit register selector WW.
pub const PointerReg = enum(u2) {
    PA = 0,
    PB = 1,
    PTRA = 2,
    PTRB = 3,
};

/// Overlapping instruction fields used by SETS/SETD/SETR and ALTI's D operand.
/// S is [8:0], D is [17:9], I is [18], R is [27:19], condition is [31:28].
pub const InstructionFields = packed struct(u32) {
    s: u9,
    d: u9,
    i: bool,
    r: u9,
    condition: Condition,
};

/// MOVBYTS S operand: each selector chooses the input byte for an output byte.
/// Bits [7:6], [5:4], [3:2], [1:0] select output bytes 3, 2, 1, 0.
pub const BytePermutation = packed struct(u32) {
    byte0: u2,
    byte1: u2,
    byte2: u2,
    byte3: u2,
    unused: u24,
};

/// ALTSN/ALTGN D operand: D[2:0] selects a nibble, D[11:3] a cog register.
pub const NibbleRegisterIndex = packed struct(u32) {
    index: u3,
    register: u9,
    unused: u20,
};

/// ALTSB/ALTGB D operand: D[1:0] selects a byte, D[10:2] a cog register.
pub const ByteRegisterIndex = packed struct(u32) {
    index: u2,
    register: u9,
    unused: u21,
};

/// ALTSW/ALTGW D operand: D[0] selects a word, D[9:1] a cog register.
pub const WordRegisterIndex = packed struct(u32) {
    index: u1,
    register: u9,
    unused: u22,
};

/// ALTB D operand: D[4:0] selects a bit, D[13:5] a cog register.
pub const BitRegisterIndex = packed struct(u32) {
    index: u5,
    register: u9,
    unused: u18,
};

/// S operand of ALTSN/ALTGN/ALTSB/ALTGB/ALTSW/ALTGW/ALTR/ALTD/ALTS/ALTB.
/// S[8:0] is the register base; sign-extended S[17:9] is added to D.
pub const RegisterBaseAndDelta = packed struct(u32) {
    base: u9,
    delta: i9,
    unused: u14,
};

/// MODCZ/MODC/MODZ modifier: bit [{C,Z}] supplies the new flag value.
pub const FlagCondition = packed struct(u4) {
    nc_nz: Flag,
    nc_z: Flag,
    c_nz: Flag,
    c_z: Flag,

    pub fn evaluate(condition: FlagCondition, c: Flag, z: Flag) Flag {
        return switch (c) {
            .unset => if (z == .unset) condition.nc_nz else condition.nc_z,
            .set => if (z == .unset) condition.c_nz else condition.c_z,
        };
    }
};

/// MODCZ's encoded D literal: [7:4] is the C modifier, [3:0] the Z modifier.
/// MODC fixes Z's modifier to zero; MODZ fixes C's modifier to zero.
pub const FlagConditions = packed struct(u9) {
    z: FlagCondition,
    c: FlagCondition,
    unused: u1,
};

/// CALLD/CALLPA/CALLPB/CALL/CALLA/CALLB save {C, Z, 10'b0, PC[19:0]}.
/// JMP/CALL/CALLA/CALLB register targets and RET/RETA/RETB restore these fields.
pub const ReturnAddress = packed struct(u32) {
    address: u20,
    unused: u10,
    z: Flag,
    c: Flag,
};

/// RDFAST/WRFAST/FBLOCK S and COGINIT S use address[19:0]. LOC outputs this.
/// GETPTR returns the FIFO hub pointer. PTRx operands use PointerExpr instead.
pub const HubAddress = packed struct(u32) {
    address: u20,
    unused: u12,
};

/// Immediate S operand of RDLUT/RDBYTE/RDWORD/RDLONG/WMLONG and
/// WRLUT/WRBYTE/WRWORD/WRLONG, including POPA/POPB/PUSHA/PUSHB aliases.
/// When is_ptr_expr is false, value.index is an unsigned eight-bit literal.
/// Layout follows src/propan/sema.zig's encode_ptr_expr: 1PUxxxxxx.
pub const PointerExpr = packed struct(u9) {
    pub const Pointer = enum(u1) {
        PTRA = 0,
        PTRB = 1,
    };

    value: packed union(u8) {
        index: u8,
        expr: packed struct(u8) {
            index: packed union(u6) {
                /// Signed offset without pointer updates, in access-size units.
                offset: i6,
                update: packed struct(u6) {
                    /// Signed update amount; zero encodes +16, -16 encodes -16.
                    offset: i5,
                    post: bool,
                },
            },
            update: bool,
            pointer: Pointer,
        },
    },
    is_ptr_expr: bool,
};

/// AUGS-extended PointerExpr, as emitted by Propan's encode_ptr_expr.
/// Bits [19:0] hold the index, [20] post-update, [21] update, [22] PTRB,
/// and [23] distinguishes a pointer expression from a direct literal.
/// Direct literals use index.unsigned with all other fields zero.
pub const AugmentedPointerExpr = packed struct(u32) {
    index: packed union(u20) {
        /// Without updates, Propan accepts indices from -0x80000 to 0xFFFFF.
        unsigned: u20,
        /// Pointer updates use signed indices; there is no special zero encoding.
        signed: i20,
    },
    post: bool,
    update: bool,
    pointer: PointerExpr.Pointer,
    is_ptr_expr: bool,
    unused: u8,
};

/// RDFAST/WRFAST D: block size is in 64-byte units; zero means maximum size.
/// FBLOCK uses block_size but ignores no_wait.
pub const FifoBlockConfig = packed struct(u32) {
    block_size: u14,
    unused: u17,
    no_wait: bool,
};

/// EXECF D: jump to cog/LUT address D[9:0], then apply D[31:10] as SKIPF pattern.
pub const ExecuteFastConfig = packed struct(u32) {
    address: u10,
    skip_pattern: u22,
};

/// QLOG result (retrieved by GETQX) and QEXP input: unsigned 5.27 logarithm.
pub const CordicLogarithm = packed struct(u32) {
    fractional_exponent: u27,
    whole_exponent: u5,
};

/// SETSCP D: enable [6], input pin base [5:2]; other bits are unused.
/// GETSCP samples, like SETDACS channel values, use BitArray(u8) byte order.
pub const OscilloscopeConfig = packed struct(u32) {
    unused_low: u2,
    pin_base: u4,
    enabled: bool,
    unused_high: u25,
};

/// General instruction word: EEEE ooooooo CZI DDDDDDDDD SSSSSSSSS.
/// Used by arithmetic, logic, bit operations, RDLUT/RDBYTE/RDWORD/RDLONG and
/// CALLD. Also covers fixed-field forms: MUL/MULS/SCA/SCAS (fixed C),
/// RDPIN/RQPIN (fixed Z), ALTx/SETS/SETD/SETR/CRCx/MUXQ/MOVBYTS/PIXx,
/// ADDCTx, DJx/IJx/TJx, J-event jumps, single-register and no-operand forms.
/// Fixed bits, including alias operands, must retain the TSV's opcode values.
/// C/Z mean write enables or effect selection depending on the instruction.
pub const InstructionEncoding = packed struct(u32) {
    s: u9,
    d: u9,
    s_immediate: bool,
    z: bool,
    c: bool,
    opcode: u7,
    condition: Condition,
};

/// EEEE ooooooo CLI DDDDDDDDD SSSSSSSSS: COGINIT; C is carry write enable.
/// CALLPA/CALLPB/SETPAT/WRPIN/WXPIN/WYPIN/WRLUT/WRBYTE/WRWORD/WRLONG,
/// RDFAST/WRFAST/FBLOCK/XINIT/XZERO/XCONT/REP and QMUL/QDIV/QFRAC/QSQRT/
/// QROTATE/QVECTOR fix C to an opcode bit. L selects a literal D operand.
/// PUSHA/PUSHB also use L at [19], while fixing S and I to a PTRx expression.
/// Other D-only literal forms (HUBSET, SETQ, SETSE/SETINT, pin instructions,
/// etc.) use L at [18], represented by InstructionEncoding.s_immediate.
pub const DualImmediateInstructionEncoding = packed struct(u32) {
    s: u9,
    d: u9,
    s_immediate: bool,
    d_immediate: bool,
    c: bool,
    opcode: u7,
    condition: Condition,
};

/// SETNIB/GETNIB/ROLNIB: EEEE ooooooN NNI DDDDDDDDD SSSSSSSSS.
pub const NibbleInstructionEncoding = packed struct(u32) {
    s: u9,
    d: u9,
    s_immediate: bool,
    n: u3,
    opcode: u6,
    condition: Condition,
};

/// SETBYTE/GETBYTE/ROLBYTE: EEEE ooooooo NNI DDDDDDDDD SSSSSSSSS.
pub const ByteInstructionEncoding = packed struct(u32) {
    s: u9,
    d: u9,
    s_immediate: bool,
    n: u2,
    opcode: u7,
    condition: Condition,
};

/// SETWORD/GETWORD/ROLWORD: EEEE oooooooo NI DDDDDDDDD SSSSSSSSS.
pub const WordInstructionEncoding = packed struct(u32) {
    s: u9,
    d: u9,
    s_immediate: bool,
    n: u1,
    opcode: u8,
    condition: Condition,
};

/// Immediate JMP/CALL/CALLA/CALLB: EEEE ooooooo RAA AAAAAAAAA AAAAAAAAA.
/// Relative addresses are signed 20-bit displacements; absolute ones unsigned.
pub const BranchInstructionEncoding = packed struct(u32) {
    address: u20,
    relative: bool,
    opcode: u7,
    condition: Condition,
};

/// Immediate CALLD/LOC: EEEE oooooWW RAA AAAAAAAAA AAAAAAAAA.
/// Register selector 0..3 designates PA, PB, PTRA, PTRB, respectively.
pub const PointerBranchInstructionEncoding = packed struct(u32) {
    address: u20,
    relative: bool,
    register: PointerReg,
    opcode: u5,
    condition: Condition,
};

/// AUGS/AUGD: EEEE oooooNN NNN NNNNNNNNN NNNNNNNNN.
/// Payload is the upper 23 bits of the next immediate operand, not an address.
pub const AugmentInstructionEncoding = packed struct(u32) {
    upper: u23,
    opcode: u5,
    condition: Condition,
};

/// MODCZ/MODC/MODZ: EEEE 1101011 CZ1 0cccczzzz 001101111.
pub const ModifyFlagsInstructionEncoding = packed struct(u32) {
    subopcode: u9,
    z_condition: FlagCondition,
    c_condition: FlagCondition,
    reserved: u1,
    immediate: bool,
    z: bool,
    c: bool,
    opcode: u7,
    condition: Condition,
};

test "instruction layouts match the variable fields in instructions.tsv" {
    // One representative for each of the TSV's 28 patterns, ignoring fixed opcode bits.
    inline for (.{
        .{ InstructionEncoding, "00000000000000000000000000000000" }, // NOP
        .{ InstructionEncoding, "EEEE0000000CZIDDDDDDDDDSSSSSSSSS" }, // ROR
        .{ InstructionEncoding, "EEEE0110001CZ0DDDDDDDDDDDDDDDDDD" }, // NOT
        .{ NibbleInstructionEncoding, "EEEE100000NNNIDDDDDDDDDSSSSSSSSS" }, // SETNIB
        .{ InstructionEncoding, "EEEE100000000I000000000SSSSSSSSS" }, // SETNIB
        .{ InstructionEncoding, "EEEE1000010000DDDDDDDDD000000000" }, // GETNIB
        .{ ByteInstructionEncoding, "EEEE1000110NNIDDDDDDDDDSSSSSSSSS" }, // SETBYTE
        .{ WordInstructionEncoding, "EEEE10010010NIDDDDDDDDDSSSSSSSSS" }, // SETWORD
        .{ InstructionEncoding, "EEEE100101010IDDDDDDDDDSSSSSSSSS" }, // ALTSN
        .{ InstructionEncoding, "EEEE1001110000DDDDDDDDDDDDDDDDDD" }, // DECOD
        .{ InstructionEncoding, "EEEE10100000ZIDDDDDDDDDSSSSSSSSS" }, // MUL
        .{ InstructionEncoding, "EEEE1010100C0IDDDDDDDDDSSSSSSSSS" }, // RQPIN
        .{ InstructionEncoding, "EEEE1011000CZ1DDDDDDDDD101011111" }, // POPA
        .{ InstructionEncoding, "EEEE1011001110111110000111110001" }, // RESI3
        .{ DualImmediateInstructionEncoding, "EEEE10110100LIDDDDDDDDDSSSSSSSSS" }, // CALLPA
        .{ DualImmediateInstructionEncoding, "EEEE11000110L1DDDDDDDDD101100001" }, // PUSHA
        .{ DualImmediateInstructionEncoding, "EEEE1100111CLIDDDDDDDDDSSSSSSSSS" }, // COGINIT
        .{ InstructionEncoding, "EEEE110101100LDDDDDDDDD000000000" }, // HUBSET
        .{ InstructionEncoding, "EEEE1101011C0LDDDDDDDDD000000001" }, // COGID
        .{ InstructionEncoding, "EEEE1101011C00DDDDDDDDD000000100" }, // LOCKNEW
        .{ InstructionEncoding, "EEEE1101011CZ1000000000000011011" }, // GETRND
        .{ InstructionEncoding, "EEEE1101011CZLDDDDDDDDD000011111" }, // WAITX
        .{ ModifyFlagsInstructionEncoding, "EEEE1101011CZ10cccczzzz001101111" }, // MODCZ
        .{ ModifyFlagsInstructionEncoding, "EEEE1101011C010cccc0000001101111" }, // MODC
        .{ ModifyFlagsInstructionEncoding, "EEEE11010110Z100000zzzz001101111" }, // MODZ
        .{ BranchInstructionEncoding, "EEEE1101100RAAAAAAAAAAAAAAAAAAAA" }, // JMP
        .{ PointerBranchInstructionEncoding, "EEEE11100WWRAAAAAAAAAAAAAAAAAAAA" }, // CALLD
        .{ AugmentInstructionEncoding, "EEEE11110nnnnnnnnnnnnnnnnnnnnnnn" }, // AUGS
    }) |case| {
        const T = case[0];
        const pattern = case[1];
        try std.testing.expectEqual(@as(usize, 32), @bitSizeOf(T));
        inline for (pattern, 0..) |marker, position| {
            const bit = 31 - position;
            const name: ?[]const u8 = comptime switch (marker) {
                '0', '1' => null,
                'E' => "condition",
                'S' => "s",
                'D' => if (bit < 9) "s" else "d", // D,D aliases repeat the D operand in S.
                'I' => "s_immediate",
                'L' => if (T == DualImmediateInstructionEncoding) "d_immediate" else "s_immediate",
                'C' => "c",
                'Z' => "z",
                'N' => "n",
                'A' => "address",
                'R' => "relative",
                'W' => "register",
                'n' => "upper",
                'c' => "c_condition",
                'z' => "z_condition",
                else => @compileError("Unknown encoding marker"),
            };
            if (comptime name) |field| {
                const offset = @bitOffsetOf(T, field);
                const width = @bitSizeOf(@FieldType(T, field));
                try std.testing.expect(bit >= offset and bit < offset + width);
            }
        }
    }
}

test "operand layouts match TSV bit positions and preserve neighboring fields" {
    const pixel: RGBA8888 = @bitCast(@as(u32, 0x1234_5678));
    try std.testing.expectEqual(@as(u8, 0x12), pixel.r);
    try std.testing.expectEqual(@as(u8, 0x34), pixel.g);
    try std.testing.expectEqual(@as(u8, 0x56), pixel.b);
    try std.testing.expectEqual(@as(u8, 0x78), pixel.a);
    const rgb: RGB565 = @bitCast(@as(u16, 0xF800));
    try std.testing.expectEqual(@as(u5, 31), rgb.r);
    try std.testing.expectEqual(@as(u6, 0), rgb.g);
    try std.testing.expectEqual(@as(u5, 0), rgb.b);

    const fields: InstructionFields = @bitCast(@as(u32, 0xF004_0000));
    try std.testing.expect(fields.i);
    try std.testing.expectEqual(@as(u9, 0), fields.r);
    try std.testing.expectEqual(Condition.IF_ALWAYS, fields.condition);
    var modified = fields;
    modified.r = 0x1FF;
    try std.testing.expectEqual(@as(u32, 0xFFFC_0000), @as(u32, @bitCast(modified)));

    const bit_range: BitIndexAndCount = @bitCast(@as(u32, 0xFFFF_FFE1));
    try std.testing.expectEqual(@as(u5, 1), bit_range.index);
    try std.testing.expectEqual(@as(u5, 31), bit_range.count);
    const pin_range: PinIndexAndCount = @bitCast(@as(u32, 0xFFFF_FFE1));
    try std.testing.expectEqual(@as(u6, 33), pin_range.index);
    try std.testing.expectEqual(@as(u5, 31), pin_range.count);

    const bytes: BytePermutation = @bitCast(@as(u32, 0xFFFF_FF1B));
    try std.testing.expectEqual(@as(u2, 3), bytes.byte0);
    try std.testing.expectEqual(@as(u2, 2), bytes.byte1);
    try std.testing.expectEqual(@as(u2, 1), bytes.byte2);
    try std.testing.expectEqual(@as(u2, 0), bytes.byte3);
    const base: RegisterBaseAndDelta = @bitCast(@as(u32, 0x0003_FFFF));
    try std.testing.expectEqual(@as(u9, 511), base.base);
    try std.testing.expectEqual(@as(i9, -1), base.delta);

    const saved: ReturnAddress = @bitCast(@as(u32, 0xC00A_BCDE));
    try std.testing.expectEqual(@as(u20, 0xABCDE), saved.address);
    try std.testing.expectEqual(Flag.set, saved.c);
    try std.testing.expectEqual(Flag.set, saved.z);
    const block: FifoBlockConfig = @bitCast(@as(u32, 0x8000_3FFF));
    try std.testing.expect(block.no_wait);
    try std.testing.expectEqual(@as(u14, 0x3FFF), block.block_size);
    const fast: ExecuteFastConfig = @bitCast(@as(u32, 0xFFFF_FC01));
    try std.testing.expectEqual(@as(u10, 1), fast.address);
    try std.testing.expectEqual(@as(u22, 0x3F_FFFF), fast.skip_pattern);
    const logarithm: CordicLogarithm = @bitCast(@as(u32, 0xF800_0001));
    try std.testing.expectEqual(@as(u5, 31), logarithm.whole_exponent);
    try std.testing.expectEqual(@as(u27, 1), logarithm.fractional_exponent);
    const scope: OscilloscopeConfig = @bitCast(@as(u32, 0x7C));
    try std.testing.expect(scope.enabled);
    try std.testing.expectEqual(@as(u4, 15), scope.pin_base);

    inline for (.{
        .{ NibbleRegisterIndex, 3, 0xFFF },
        .{ ByteRegisterIndex, 2, 0x7FF },
        .{ WordRegisterIndex, 1, 0x3FF },
        .{ BitRegisterIndex, 5, 0x3FFF },
    }) |case| {
        const value: case[0] = @bitCast(@as(u32, case[2]));
        try std.testing.expectEqual(@as(u9, 511), value.register);
        try std.testing.expectEqual(case[1], @bitOffsetOf(case[0], "register"));
    }
    const hub: HubAddress = @bitCast(@as(u32, 0xFFFF_FFFF));
    try std.testing.expectEqual(@as(u20, 0xFFFFF), hub.address);
    const modifiers: FlagConditions = @bitCast(@as(u9, 0xA5));
    try std.testing.expectEqual(@as(u4, 10), @as(u4, @bitCast(modifiers.c)));
    try std.testing.expectEqual(@as(u4, 5), @as(u4, @bitCast(modifiers.z)));
}

test "pointer expressions match Propan encodings" {
    const literal: PointerExpr = .{ .value = .{ .index = 255 }, .is_ptr_expr = false };
    try std.testing.expectEqual(@as(u9, 0x0FF), @as(u9, @bitCast(literal)));
    const offset: PointerExpr = .{
        .value = .{ .expr = .{ .index = .{ .offset = -32 }, .update = false, .pointer = .PTRA } },
        .is_ptr_expr = true,
    };
    try std.testing.expectEqual(@as(u9, 0x120), @as(u9, @bitCast(offset)));
    const increment: PointerExpr = .{
        .value = .{ .expr = .{ .index = .{ .update = .{ .offset = 0, .post = true } }, .update = true, .pointer = .PTRB } },
        .is_ptr_expr = true,
    };
    try std.testing.expectEqual(@as(u9, 0x1E0), @as(u9, @bitCast(increment))); // PTRB++[16]
    var decrement = increment;
    decrement.value.expr.index.update.offset = -16;
    decrement.value.expr.index.update.post = false;
    try std.testing.expectEqual(@as(u9, 0x1D0), @as(u9, @bitCast(decrement))); // --PTRB[16]

    // Golden AUGS + RDLONG pairs from tests/propan/sema/aug-pointer-update.propan.
    inline for (.{
        .{ 0xFF005800, 0xFB07ED00, PointerExpr.Pointer.PTRA, true, 0x100 },
        .{ 0xFF005FFF, 0xFB07ED00, PointerExpr.Pointer.PTRA, true, -0x100 },
        .{ 0xFF007091, 0xFB07ED45, PointerExpr.Pointer.PTRB, false, 0x12345 },
        .{ 0xFF00776E, 0xFB07ECBB, PointerExpr.Pointer.PTRB, false, -0x12345 },
    }) |case| {
        const encoded = ((@as(u32, case[0]) & 0x7F_FFFF) << 9) | (case[1] & 0x1FF);
        const expression: AugmentedPointerExpr = @bitCast(encoded);
        try std.testing.expect(expression.is_ptr_expr);
        try std.testing.expect(expression.update);
        try std.testing.expectEqual(case[2], expression.pointer);
        try std.testing.expectEqual(case[3], expression.post);
        try std.testing.expectEqual(@as(i20, case[4]), expression.index.signed);
        try std.testing.expectEqual(@as(u8, 0), expression.unused);
    }
    const augmented_literal: AugmentedPointerExpr = @bitCast(@as(u32, 0xFFFFF));
    try std.testing.expect(!augmented_literal.is_ptr_expr);
    try std.testing.expectEqual(@as(u20, 0xFFFFF), augmented_literal.index.unsigned);
    const augmented_offset: AugmentedPointerExpr = @bitCast(@as(u32, 0xCF_FFFF));
    try std.testing.expect(augmented_offset.is_ptr_expr);
    try std.testing.expect(!augmented_offset.update);
    try std.testing.expectEqual(PointerExpr.Pointer.PTRB, augmented_offset.pointer);
    try std.testing.expectEqual(@as(u20, 0xFFFFF), augmented_offset.index.unsigned);

    const branch: PointerBranchInstructionEncoding = @bitCast(@as(u32, 0xF060_0000));
    try std.testing.expectEqual(PointerReg.PTRB, branch.register);
    try std.testing.expectEqual(Condition.IF_ALWAYS, branch.condition);
}
