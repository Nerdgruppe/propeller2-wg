//! Host debugger. Stops never use the emulated BRK/COGBRK controllers.
const std = @import("std");
const Hub = @import("Hub.zig");
const Cog = @import("Cog.zig");
const Debugger = @This();
const access_types = @import("access.zig");

pub const Domain = enum { cog, lut, hub };
pub const AccessMode = enum { read, write, both };
pub const Scope = enum { data, physical };
pub const Execution = struct { cog: u3, domain: Domain, pc: u20 };
pub const Watchpoint = struct {
    space: access_types.Space,
    cog: ?u3,
    /// Half-open range in the selected domain (bytes for hub RAM, longs for local RAM).
    start: u32,
    end: u32,
    access: AccessMode,
    scope: Scope = .data,
    issuer: ?u3 = null,
};
pub const Hit = struct {
    watchpoint: usize,
    clock: u64,
    provenance: access_types.Provenance,
    space: access_types.Space,
    cog: ?u3,
    address: u32,
    access: access_types.Access,
    value: u32,
    class: access_types.ReadClass,
    storage: access_types.Storage,
};
pub const Stop = enum { none, execution, data };

allocator: std.mem.Allocator,
executions: std.ArrayList(Execution) = .empty,
watches: std.ArrayList(Watchpoint) = .empty,
hits: std.ArrayList(Hit) = .empty,
encounters: struct {
    storage: [8]Execution = undefined,
    items: []Execution = &.{},
} = .{},
stop: Stop = .none,
observed_accesses: usize = 0,

/// Release debugger-owned lists; detach from the hub before destroying it.
pub fn deinit(debugger: *Debugger) void {
    debugger.executions.deinit(debugger.allocator);
    debugger.watches.deinit(debugger.allocator);
    debugger.hits.deinit(debugger.allocator);
}

/// Resolve the execution domain from the actual fetch source, including local PC carry.
pub fn location(cog: *const Cog, state: Cog.PipelineState) Execution {
    return .{
        .cog = cog.id,
        .domain = if (state.local_address) |address|
            (if (address < 0x200) .cog else .lut)
        else if (cog.exec_mode == .hub) .hub else if (state.pc & 0x200 == 0) .cog else .lut,
        .pc = state.pc,
    };
}

/// Test an exact cog/domain/PC execution address.
pub fn hasExecution(debugger: *const Debugger, execution: Execution) bool {
    for (debugger.executions.items) |item| {
        if (std.meta.eql(item, execution))
            return true;
    }
    return false;
}

/// Toggle one exact address without disturbing any other breakpoint.
pub fn toggleExecution(debugger: *Debugger, execution: Execution) !void {
    for (debugger.executions.items, 0..) |item, index| {
        if (std.meta.eql(item, execution)) {
            _ = debugger.executions.orderedRemove(index);
            return;
        }
    }
    try debugger.executions.append(debugger.allocator, execution);
}

/// Continue/Step acknowledges only the instruction encounters that caused the current stop.
pub fn acknowledge(debugger: *Debugger, hub: *Hub) void {
    for (debugger.encounters.items) |encounter| {
        const cog = &hub.cogs[encounter.cog];
        if (cog.pipeline[3]) |*state| {
            if (std.meta.eql(location(cog, state.*), encounter)) {
                state.debugger_attempted = true;
            }
        }
    }
    debugger.stop = .none;
    debugger.hits.clearRetainingCapacity();
    debugger.encounters.items = debugger.encounters.storage[0..0];
}

/// Collect every first-attempt execution stop before any work on the hub edge.
pub fn beforeClock(debugger: *Debugger, hub: *Hub) bool {
    if (debugger.stop != .none)
        return false;
    debugger.observed_accesses = 0;
    if (debugger.executions.items.len == 0)
        return true;
    for (&hub.cogs) |*cog| {
        if (cog.exec_mode == .stopped or cog.startup != null)
            continue;
        if (cog.pipeline[3]) |state| {
            if (state.cancelled or state.debugger_attempted)
                continue;
            const execution = location(cog, state);
            if (debugger.hasExecution(execution)) {
                const count = debugger.encounters.items.len;
                debugger.encounters.storage[count] = execution;
                debugger.encounters.items = debugger.encounters.storage[0 .. count + 1];
            }
        }
    }
    if (debugger.encounters.items.len != 0) {
        debugger.stop = .execution;
        return false;
    }
    return true;
}

/// Match a committed access. Callers provide backing addresses and accepted byte lanes only.
pub fn observe(debugger: *Debugger, hit: Hit) void {
    debugger.observed_accesses += 1;
    if (debugger.observed_accesses > max_accesses_per_edge)
        @panic("debugger access emission bound exceeded");
    for (debugger.watches.items, 0..) |watch, index| {
        if (watch.space != hit.space or hit.address < watch.start or hit.address >= watch.end)
            continue;
        if (watch.cog != null and watch.cog != hit.cog)
            continue;
        if (watch.issuer != null and watch.issuer != hit.provenance.cog)
            continue;
        const access_matches = switch (watch.access) {
            .both => true,
            .read => hit.access == .read,
            .write => hit.access == .write,
        };
        if (!access_matches)
            continue;
        if (hit.access == .read and ((watch.scope == .data and hit.class == .physical) or
            (watch.scope == .physical and hit.class == .consumed)))
            continue;
        var matched = hit;
        matched.watchpoint = index;
        if (debugger.hits.items.len == debugger.hits.capacity)
            @panic("watchpoint storage was not reserved outside stepping");
        debugger.hits.appendAssumeCapacity(matched);
    }
}

/// Data hits pause only after the complete edge; preserve every match even if a fault also occurred.
pub fn afterClock(debugger: *Debugger, hub: *const Hub) void {
    _ = hub;
    debugger.stop = if (debugger.hits.items.len != 0)
        .data
    else
        .none;
}

/// Maximum observed byte lanes and local ports per edge: eight cogs times 128 accesses.
/// Startup, writeback, block transfer, FIFO, streamer, operands and fetch fit within 128 per cog.
pub const max_accesses_per_edge = 8 * 128;

/// Reserve hit storage before publishing a new watchpoint; stepping never allocates.
pub fn addWatch(debugger: *Debugger, watch: Watchpoint) !void {
    const count = try std.math.add(usize, debugger.watches.items.len, 1);
    const hits = try std.math.mul(usize, count, max_accesses_per_edge);
    try debugger.hits.ensureTotalCapacity(debugger.allocator, hits);
    try debugger.watches.append(debugger.allocator, watch);
}

/// Remove one watch while retaining the already reserved hit capacity.
pub fn removeWatch(debugger: *Debugger, index: usize) void {
    _ = debugger.watches.orderedRemove(index);
}
