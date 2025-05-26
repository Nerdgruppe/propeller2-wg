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

pub const FlagModifier = enum(u1) {
    keep = 0,
    write = 1,
};

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

    // C/Z = 1
    set = 0b1111,
};

pub const PointerReg = enum(u2) {
    PA = 0,
    PB = 1,
    PTRA = 2,
    PTRB = 3,
};
