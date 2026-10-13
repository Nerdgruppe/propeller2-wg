//! Single-threaded simulation advancement with an independently supplied monotonic clock.
const Hub = @import("Hub.zig");
const Frame = @This();
pub const Mode = enum { paused, run, step };
pub const Clock = struct {
    context: *anyopaque,
    now: *const fn (*anyopaque) i96,
};
clocks: u64 = 0,
nanoseconds: i96 = 0,
running: bool = false,
pub const budget_ns = 14_000_000;

/// Advance whole hub edges until the deadline, a stop, or a fault; never catch up skipped host time.
pub fn advance(hub: *Hub, mode: Mode, clock: Clock) Frame {
    var result: Frame = .{ .running = mode == .run };
    if (mode == .paused)
        return result;
    const start = clock.now(clock.context);
    while (true) {
        if (hub.fault != null) {
            if (hub.debugger) |debugger| {
                debugger.stop = .fault;
            }
            result.running = false;
            break;
        }
        if (hub.debugger != null and hub.debugger.?.stop != .none) {
            result.running = false;
            break;
        }
        if (mode == .run and clock.now(clock.context) - start >= budget_ns)
            break;
        const before = hub.counter;
        hub.step();
        if (hub.counter != before) {
            result.clocks += 1;
        }
        if (hub.counter == before or hub.fault != null or
            (hub.debugger != null and hub.debugger.?.stop != .none))
        {
            result.running = false;
            break;
        }
        if (mode == .step)
            break;
    }
    result.nanoseconds = clock.now(clock.context) - start;
    return result;
}
