//! Binary digital levels shared by pad routing and smart engines; drive enable is separate.
pub const InputLogic = enum(u2) {
    a = 0,
    a_and_b = 1,
    a_or_b = 2,
    a_xor_b = 3,
};

pub const Signal = enum {
    low,
    high,

    /// Convert a Boolean level to a driven digital signal.
    pub fn from(value: bool) Signal {
        return if (value)
            .high
        else
            .low;
    }
    /// Read a driven high signal as true.
    pub fn sample(value: Signal) bool {
        return value == .high;
    }
    /// Invert a binary digital level.
    pub fn invert(value: Signal) Signal {
        return switch (value) {
            .low => .high,
            .high => .low,
        };
    }
    /// Apply the selected two-input digital logic mode.
    pub fn logic(a: Signal, b: Signal, operation: InputLogic) Signal {
        return switch (operation) {
            .a => a,
            .a_and_b => from(a.sample() and b.sample()),
            .a_or_b => from(a.sample() or b.sample()),
            .a_xor_b => from(a != b),
        };
    }
};
