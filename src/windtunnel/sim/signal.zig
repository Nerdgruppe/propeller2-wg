//! Four-state digital levels shared by pad routing and smart engines.
pub const InputLogic = enum(u2) {
    a = 0,
    a_and_b = 1,
    a_or_b = 2,
    a_xor_b = 3,
};

pub const Signal = enum {
    zero,
    one,
    x,
    z,

    pub fn from(value: bool) Signal {
        return if (value) .one else .zero;
    }
    pub fn sample(value: Signal) bool {
        return value == .one;
    }
    pub fn invert(value: Signal) Signal {
        return switch (value) {
            .zero => .one,
            .one => .zero,
            .x, .z => .x,
        };
    }
    pub fn resolve(a: Signal, b: Signal) Signal {
        if (a == .z) return b;
        if (b == .z) return a;
        return if (a == b) a else .x;
    }
    pub fn logic(a: Signal, b: Signal, operation: InputLogic) Signal {
        return switch (operation) {
            .a => a,
            .a_and_b => if (a == .zero or b == .zero) .zero else if (a == .one and b == .one) .one else .x,
            .a_or_b => if (a == .one or b == .one) .one else if (a == .zero and b == .zero) .zero else .x,
            .a_xor_b => if ((a == .zero or a == .one) and (b == .zero or b == .one)) from(a != b) else .x,
        };
    }
};
