//! Shared vocabulary for observed memory accesses and their issuing instruction.

pub const Provenance = struct {
    /// Cog that issued the operation; absent for external electrical activity.
    cog: ?u3 = null,
    /// Issuing instruction address in the cog's execution domain.
    pc: ?u20 = null,
    /// Captured instruction bits, independent of later pipeline or memory changes.
    instruction: ?u32 = null,
};
pub const Space = enum { registers, lut, hub };
pub const Storage = enum { architectural, ram, execute_rom };
pub const Access = enum { read, write };
/// Physical reads include preloads; consumed reads use previously latched data.
pub const ReadClass = enum { data, physical, consumed };
