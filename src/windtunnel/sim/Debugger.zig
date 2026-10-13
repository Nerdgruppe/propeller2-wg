//! Host debugger. Stops never use the emulated BRK/COGBRK controllers.
const std = @import("std");
const Hub = @import("Hub.zig");
const Cog = @import("Cog.zig");
const Debugger = @This();

pub const Domain = enum { cog, lut, hub };
pub const Space = enum { registers, lut, hub };
pub const Storage = enum { architectural, ram, execute_rom };
pub const Access = enum { read, write };
pub const AccessMode = enum { read, write, both };
pub const Scope = enum { data, physical };
/// Consumed latched operands are logical reads; their physical read was the earlier preload.
pub const ReadClass = enum { data, physical, consumed };
pub const Provenance = struct {
    cog: ?u3 = null,
    pc: ?u20 = null,
    instruction: ?u32 = null,
};
pub const Execution = struct { cog: u3, domain: Domain, pc: u20 };
pub const Watchpoint = struct {
    space: Space,
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
    provenance: Provenance,
    space: Space,
    cog: ?u3,
    address: u32,
    access: Access,
    value: u32,
    class: ReadClass,
    storage: Storage,
};
pub const Stop = enum { none, execution, data, fault, allocation_failure };

allocator: std.mem.Allocator,
executions: std.ArrayList(Execution) = .empty,
watches: std.ArrayList(Watchpoint) = .empty,
hits: std.ArrayList(Hit) = .empty,
encounters: std.ArrayList(Execution) = .empty,
stop: Stop = .none,

/// Release debugger-owned lists; detach from the hub before destroying it.
pub fn deinit(debugger: *Debugger) void {
    debugger.executions.deinit(debugger.allocator);
    debugger.watches.deinit(debugger.allocator);
    debugger.hits.deinit(debugger.allocator);
    debugger.encounters.deinit(debugger.allocator);
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
    debugger.encounters.clearRetainingCapacity();
}

/// Collect every first-attempt execution stop before any work on the hub edge.
pub fn beforeClock(debugger: *Debugger, hub: *Hub) bool {
    if (debugger.stop != .none)
        return false;
    if (hub.fault != null) {
        debugger.stop = .fault;
        return false;
    }
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
                debugger.encounters.append(debugger.allocator, execution) catch {
                    debugger.stop = .allocation_failure;
                    return false;
                };
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
    for (debugger.watches.items, 0..) |watch, index| {
        if (watch.space != hit.space or hit.address < watch.start or hit.address >= watch.end)
            continue;
        if (watch.cog != null and watch.cog != hit.cog)
            continue;
        if (watch.issuer != null and watch.issuer != hit.provenance.cog)
            continue;
        if (watch.access != .both and @intFromEnum(watch.access) != @intFromEnum(hit.access))
            continue;
        if (hit.access == .read and ((watch.scope == .data and hit.class == .physical) or
            (watch.scope == .physical and hit.class == .consumed)))
            continue;
        var matched = hit;
        matched.watchpoint = index;
        debugger.hits.append(debugger.allocator, matched) catch {
            debugger.stop = .allocation_failure;
            return;
        };
    }
}

/// Data hits pause only after the complete edge; preserve every match even if a fault also occurred.
pub fn afterClock(debugger: *Debugger, hub: *const Hub) void {
    if (debugger.stop == .allocation_failure)
        return;
    debugger.stop = if (hub.fault != null) .fault else if (debugger.hits.items.len != 0) .data else .none;
}
