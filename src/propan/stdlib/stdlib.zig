const std = @import("std");

pub const p1 = struct {
    pub const constants = @import("p1/constants.zig").p1_constants;

    pub const functions = @import("p1/functions.zig");
};

pub const p2 = struct {
    pub const constants = @import("p2/constants.zig").p2_constants;
    pub const instructions = @import("p2/instructions.zig").p2_instructions;

    pub const functions = @import("p2/functions.zig").functions;
};

pub const common = struct {
    pub const constants = @import("common/constants.zig").common_constants;

    pub const functions = @import("common/functions.zig").functions;
};
