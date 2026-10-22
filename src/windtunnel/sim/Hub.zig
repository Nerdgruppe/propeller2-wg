//! Shared hub memory, clocks, locks and cross-cog edge ordering.
//! A step commits simultaneous writes before any cog samples the new edge.

const std = @import("std");

const access_types = @import("access.zig");
pub const HubRam = @import("HubRam.zig");
const reads = @import("read.zig");
const Cog = @import("Cog.zig");
const IO = @import("IO.zig");
pub const SystemConfiguration = @import("SystemConfiguration.zig");
const Cordic = @import("Cordic.zig");
const Random = @import("Random.zig");
const Capture = @import("Capture.zig");
const Hub = @This();
pub const Debugger = @import("Debugger.zig");
pub const Issue = @import("Issue.zig");
const p2 = @import("p2");
const EventId = p2.types.EventId;

pub const boot_rom = @embedFile("boot-rom");
comptime {
    @setEvalBranchQuota(1_000_000);
    if (boot_rom.len != 16_384) {
        @compileError("invalid recovered device ROM length");
    }
    var digest: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(boot_rom, &digest, .{});
    if (!std.mem.eql(
        u8,
        &std.fmt.bytesToHex(digest, .lower),
        "b7a33635d05ebfe62a1efba88a6599be3b62ca36c756eb459691a05cf2b27583",
    ))
        @compileError("recovered device ROM identity changed");
}

/// Captured at the issuing instruction, never inferred from a later cog state.
pub const WriteOrigin = enum { application, debug };

/// Notify the host when execution continues, stops at a breakpoint, or resets; issues are inspected separately.
pub const StepResult = enum { normal, breakpoint, reset };

debugger: ?*Debugger = null,
access_context: access_types.Provenance = .{},
read_class: access_types.ReadClass = .data,

ram: HubRam = .{},
cogs: [8]Cog,
counter: u64 = 0,
io: IO,
/// Host configuration is fixed for this initialized simulation, including across firmware resets.
system_configuration: SystemConfiguration,
cordic: Cordic = .{},
random: Random = .{},
debug: struct {
    enables: u8 = 0,
    protected: bool = false,
    locked: bool = false,
} = .{},

/// Caller-owned electrical recording survives chip resets.
capture: ?*Capture = null,
issues: Issue.Collection = .{},
/// Actual committed writes for same-edge writer conflict detection.
edge_writes: [8 * max_cog_edge_writes]Issue.Participant = undefined,
edge_write_count: usize = 0,
/// One single-port operation per physical slice on the current edge.
ram_ports: [8]?HubTransfer = @splat(null),
/// Accepted RAM byte writes on this edge, reconciled after every physical producer.
ram_writes: [8 * 4]reads.CommittedWrite = undefined,
ram_write_count: usize = 0,
/// The shared command bus accepts one cog's command per edge.
command_issuer: ?u3 = null,
locks: [16]Lock = undefined,
/// True only while step() processes a clock edge; direct semantic calls apply shared effects immediately.
clocking: bool = false,
/// HUBSET reset is consumed at the next step boundary or immediately after its issuing cog.
reset_requested: bool = false,
pending_starts: [8]?struct { provenance: access_types.Provenance, at: u64, address: u32, ptra: u32, load: bool, command_clock: u64 } = @splat(null),

comptime {
    std.debug.assert(@sizeOf(Cog) <= 32 * 1024);
    std.debug.assert(@sizeOf(Hub) <= 1280 * 1024);
}

pub const Lock = struct {
    allocated: bool = false,
    taken: bool = false,
    owner: u3 = 0,
    id: u4,
};

/// Shared observation budget for writes from one cog on a completed edge.
pub const max_cog_edge_writes = 32;

/// Distinguish physical producers; consecutive beats of one memory instruction may share write lanes.
pub const TransferOrigin = union(enum) {
    startup: u3,
    fifo: u3,
    memory: access_types.Provenance,
};
const HubTransfer = struct {
    origin: TransferOrigin,
    access: access_types.Access,
    address: u18,
    lanes: std.StaticBitSet(4),
    accepted: HubRam.LaneMask,
};

/// Scheduled delays are below half the counter period. Compare modulo 2^64
/// so a deadline after rollover remains in the future until its clock arrives.
pub fn clock_reached(clock: u64, deadline: u64) bool {
    return clock -% deadline < (@as(u64, 1) << 63);
}

/// Initialize all shared resources and stopped cogs, binding cogs and I/O ports to this hub.
pub fn init(hub: *Hub, configuration: SystemConfiguration) error{InvalidConfiguration}!void {
    try configuration.validate();
    hub.* = .{
        .cogs = .{
            .init(hub, 0),
            .init(hub, 1),
            .init(hub, 2),
            .init(hub, 3),
            .init(hub, 4),
            .init(hub, 5),
            .init(hub, 6),
            .init(hub, 7),
        },
        .io = .{ .clock = .init(configuration.clocks) },
        .system_configuration = configuration,
    };
    hub.io.connect();
    hub.io.analog_pads.init(&hub.system_configuration);
    @memcpy(hub.ram.bytes()[0x7c000..], boot_rom);
    for (&hub.locks, 0..) |*lock, i| {
        lock.* = .{ .id = @intCast(i) };
    }
}

/// Apply shared clock, random, debug, or input-filter configuration, or request a full reset.
/// Unsupported configurations leave the current hub state intact.
pub fn configure(hub: *Hub, mode: u32) void {
    if (mode & 0x8000_0000 != 0) {
        hub.random.seed(mode);
    } else if (mode & 0xf000_0000 == 0x1000_0000) {
        hub.reset_requested = true;
    } else if (mode & 0xf000_0000 == 0x2000_0000) {
        if (!hub.debug.locked)
            hub.debug = .{
                .enables = @truncate(mode),
                .protected = mode & (1 << 16) != 0,
                .locked = mode & (1 << 17) != 0,
            };
    } else if (mode & 0xf000_0000 == 0x4000_0000) {
        hub.io.configureFilter(mode);
    } else {
        const previous_coverage = hub.io.clock.coverage;
        const previous_unsafe = hub.io.clock.unsafe_postdivider_up;
        if (!hub.io.setClock(mode)) {
            hub.report(.{ .unsupported_hubset = mode }, hub.access_context);
        } else if (previous_coverage != hub.io.clock.coverage and hub.io.clock.coverage == .uncharacterized) {
            hub.report(.{ .uncharacterized_pll = mode }, hub.access_context);
        }
        if (!previous_unsafe and hub.io.clock.unsafe_postdivider_up) {
            hub.report(.{ .unsafe_postdivider = mode }, hub.access_context);
        }
    }
}

/// Attach a caller-owned capture and observe current levels, preserving history when reattaching after reset.
pub fn attachCapture(hub: *Hub, capture: *Capture) void {
    hub.capture = capture;
    hub.io.observeCapture(capture, hub.io.pads);
}

/// Release ownership while retaining allocation and last owner, and signal lock-release events.
pub fn release_lock(hub: *Hub, lock: *Lock) void {
    lock.taken = false;
    hub.signal_lock(lock.id, false);
}

/// Notify selectable lock sensors of a taken/released edge and the corresponding change.
pub fn signal_lock(hub: *Hub, id: u4, taken: bool) void {
    for (&hub.cogs) |*cog| {
        cog.signal_selectable((if (taken)
            @as(u6, 0x10)
        else
            0x20) | id);
        cog.signal_selectable(0x30 | @as(u6, id));
    }
}

/// Strobe attention for every cog selected by the low eight bits of the operand.
pub fn send_attention(hub: *Hub, mask: u32) void {
    for (&hub.cogs) |*cog| {
        if (mask & (@as(u32, 1) << cog.id) != 0) {
            cog.signal_event(.ATN);
        }
    }
}

/// Reserve the rotating single-port slice; merge only disjoint pieces of one write beat.
fn claim_hub_access(hub: *Hub, origin: TransferOrigin, access: access_types.Access, address: u32, size: u3) void {
    if (!hub.clocking)
        return;
    const decoded = HubRam.Address.init(address);
    std.debug.assert(size > 0 and @as(u3, decoded.lane) + size <= 4);
    const requester = switch (origin) {
        .startup, .fifo => |id| id,
        .memory => |provenance| provenance.cog orelse @panic("RAM beat has no transport requester"),
    };
    std.debug.assert(decoded.slice == (hub.counter +% requester) % 8);
    const backing: u20 = decoded.backing() orelse decoded.value();
    const long_address: u18 = @intCast(backing / 4);
    var lanes = HubRam.LaneMask.initEmpty();
    lanes.setRangeValue(.{ .start = decoded.lane, .end = @as(usize, decoded.lane) + size }, true);
    const port = &hub.ram_ports[decoded.slice];
    if (port.*) |*claimed| {
        std.debug.assert((origin == .memory or origin == .fifo) and access == .write and
            claimed.access == .write and claimed.address == long_address and
            std.meta.eql(origin, claimed.origin) and lanes.intersectWith(claimed.lanes).count() == 0);
        claimed.lanes.setUnion(lanes);
    } else {
        port.* = .{ .origin = origin, .access = access, .address = long_address, .lanes = lanes, .accepted = .initEmpty() };
    }
}

pub const ReadBeat = struct { origin: TransferOrigin, address: u32, size: u3 };
pub const ReadResult = struct { value: u32, sample: reads.Sample };
pub const WriteBeat = struct {
    origin: TransferOrigin,
    address: u32,
    value: u32,
    size: u3,
    masked: bool,
    authorization: WriteOrigin,
};

/// Issue one selected physical long; callers retain the returned sampling metadata.
pub fn readHubBeat(hub: *Hub, request: ReadBeat) ReadResult {
    const address = HubRam.Address.init(request.address);
    std.debug.assert(@as(u3, address.lane) + request.size <= 4);
    hub.claim_hub_access(request.origin, .read, request.address, request.size);
    var failures: ReadFailures = .{};
    return hub.readLong(address, request.size, &failures);
}

/// Commit one physical write with captured authorization and explicit byte enables.
pub fn writeHubBeat(hub: *Hub, request: WriteBeat) void {
    const address = HubRam.Address.init(request.address);
    std.debug.assert(@as(u3, address.lane) + request.size <= 4);
    hub.claim_hub_access(request.origin, .write, request.address, request.size);
    hub.write_memory_from(request.address, request.value, request.size, request.masked, request.authorization);
}

const ReadFailures = struct { hole: bool = false, protected: bool = false };

/// Sample selected lanes after one address/protection decode, coalescing logical read failures.
fn readLong(hub: *Hub, address: HubRam.Address, size: u3, failures: *ReadFailures) ReadResult {
    const resolution = HubRam.resolve(address, hub.debug.protected, .read, false);
    var result: ReadResult = .{
        .value = 0,
        .sample = .{ .address = address.aligned().value(), .clock = hub.counter, .readable = .initEmpty() },
    };
    switch (resolution) {
        .ram => |physical| {
            result.sample.readable = .initFull();
            const word = hub.ram.read(physical);
            for (0..size) |i| {
                const byte = word[address.lane + i];
                result.value |= @as(u32, byte) << @intCast(i * 8);
                hub.observe(.hub, null, physical.backing().? + @as(u19, @intCast(i)), .read, byte, hub.read_class, .ram);
            }
        },
        .hole, .protected => {
            const reported = switch (resolution) {
                .hole => &failures.hole,
                .protected => &failures.protected,
                .ram => unreachable,
            };
            if (hub.clocking and !reported.*) {
                reported.* = true;
                const read: Issue.MemoryRead = .{ .address = address.value() };
                hub.report(switch (resolution) {
                    .hole => .{ .memory_hole_read = read },
                    .protected => .{ .memory_protected_read = read },
                    .ram => unreachable,
                }, hub.access_context);
            }
        },
    }
    return result;
}

/// Compose an untimed logical span from decoded physical longs, wrapping the twenty address lines.
pub fn read_memory(hub: *Hub, address: u32, size: u3) u32 {
    std.debug.assert(size <= 4);
    var value: u32 = 0;
    var failures: ReadFailures = .{};
    var offset: u3 = 0;
    while (offset < size) {
        const decoded = HubRam.Address.init(address +% offset);
        const count: u3 = @min(size - offset, 4 - @as(u3, decoded.lane));
        value |= hub.readLong(decoded, count, &failures).value << @intCast(@as(u5, offset) * 8);
        offset += count;
    }
    return value;
}

/// Write an application span through the decoded long and byte-enable interface.
pub fn write_memory(hub: *Hub, address: u32, value: u32, size: u3, masked: bool) void {
    hub.write_memory_from(address, value, size, masked, .application);
}

/// Apply captured authorization once per long and zero-byte masking per selected lane.
pub fn write_memory_from(hub: *Hub, address: u32, value: u32, size: u3, masked: bool, origin: WriteOrigin) void {
    std.debug.assert(size <= 4);
    var offset: u3 = 0;
    while (offset < size) {
        const decoded = HubRam.Address.init(address +% offset);
        const count: u3 = @min(size - offset, 4 - @as(u3, decoded.lane));
        const resolution = HubRam.resolve(decoded, hub.debug.protected, .write, origin == .debug);
        var lanes = HubRam.LaneMask.initEmpty();
        var word: HubRam.Long = @splat(0);
        for (0..count) |i| {
            const byte: u8 = @truncate(value >> @intCast((offset + i) * 8));
            if (masked and byte == 0)
                continue;
            switch (resolution) {
                .ram => |physical| {
                    if (hub.clocking) {
                        std.debug.assert(hub.ram_write_count < hub.ram_writes.len);
                        hub.ram_writes[hub.ram_write_count] = .{
                            .address = physical.backing().? + @as(u19, @intCast(i)),
                            .value = byte,
                            .provenance = hub.access_context,
                        };
                        hub.ram_write_count += 1;
                        if (hub.ram_ports[decoded.slice]) |*port| {
                            std.debug.assert(port.access == .write);
                            port.accepted.set(decoded.lane + i);
                        }
                    }
                    lanes.set(decoded.lane + i);
                    word[decoded.lane + i] = byte;
                    hub.observe(.hub, null, physical.backing().? + @as(u19, @intCast(i)), .write, byte, .data, .ram);
                },
                .hole, .protected => {
                    if (hub.clocking) {
                        const write: Issue.MemoryWrite = .{
                            .address = decoded.value() + @as(u20, @intCast(i)),
                            .value = byte,
                        };
                        hub.report(switch (resolution) {
                            .hole => .{ .memory_hole_write = write },
                            .protected => .{ .memory_protected_write = write },
                            .ram => unreachable,
                        }, hub.access_context);
                    }
                },
            }
        }
        if (resolution == .ram) {
            hub.ram.write(resolution.ram, word, lanes);
        }
        offset += count;
    }
}

/// Report simulator traffic only. Host inspection and edits run outside clocking.
pub fn observe(hub: *Hub, space: access_types.Space, cog: ?u3, address: u32, access: access_types.Access, value: u32, class: access_types.ReadClass, storage: access_types.Storage) void {
    if (!hub.clocking)
        return;
    if (access == .write and space != .hub) {
        const participant: Issue.Participant = .{
            .provenance = hub.access_context,
            .space = space,
            .cog = cog,
            .address = address,
            .access = access,
            .value = value,
            .class = class,
            .storage = storage,
        };
        for (hub.edge_writes[0..hub.edge_write_count]) |previous| {
            if (previous.space == space and previous.cog == cog and previous.address == address and
                !std.meta.eql(previous.provenance, participant.provenance))
            {
                const writers: Issue.Writers = .{ .first = previous, .second = participant };
                const kind: Issue.Kind = switch (space) {
                    .registers => .{ .reg_write_conflict = writers },
                    .lut => .{ .lut_write_conflict = writers },
                    .hub => unreachable,
                };
                hub.report(kind, participant.provenance);
            }
        }
        if (hub.edge_write_count == hub.edge_writes.len)
            @panic("simulation physical write bound exceeded");
        hub.edge_writes[hub.edge_write_count] = participant;
        hub.edge_write_count += 1;
    }
    if (hub.debugger) |debugger| {
        debugger.observe(.{
            .watchpoint = 0,
            .clock = hub.counter,
            .provenance = hub.access_context,
            .space = space,
            .cog = cog,
            .address = address,
            .access = access,
            .value = value,
            .class = class,
            .storage = storage,
        });
    }
}

/// Commit old writes, advance I/O once, execute cogs, then capture traces and advance CT.
/// All cogs observe previous-edge writes before any cog executes on this edge.
/// Reset returns only after init() has restored the entire hub and stopped every cog.
pub fn step(hub: *Hub) StepResult {
    hub.issues.clear();
    hub.edge_write_count = 0;
    hub.ram_ports = @splat(null);
    hub.ram_write_count = 0;
    hub.command_issuer = null;
    if (hub.reset_requested) {
        hub.firmwareReset();
        return .reset;
    }
    if (hub.debugger) |debugger| {
        if (!debugger.beforeClock(hub))
            return .breakpoint;
    }
    const interval = hub.io.clock.step();
    if (interval.quiet != 0) {
        hub.io.advanceQuiet(hub, interval.quiet);
    }
    if (interval.period == 0) {
        return .normal;
    }
    hub.clocking = true;
    defer {
        hub.clocking = false;
        hub.access_context = .{};
        hub.read_class = .data;
    }

    var lut_writes: [8]@TypeOf(hub.cogs[0].writeback.lut) = undefined;
    var lut_provenance: [8]access_types.Provenance = undefined;
    for (&hub.cogs, &lut_writes) |*cog, *write| {
        cog.streamer.lut_write = null;
        cog.streamer.displaced_capture = null;
        write.* = cog.writeback.lut;
        lut_provenance[cog.id] = cog.writeback.provenance;
    }
    for (&hub.cogs) |*cog| {
        hub.access_context = cog.writeback.provenance;
        cog.commit_results();
    }
    for (&hub.cogs) |*cog| {
        cog.clock_events();
    }
    // Both RAM ports complete together. Distinct-address writes both become
    // visible. Same-address collisions vary by physical RAM instance; use a
    // deterministic local-write convention and expose the collision.
    for (&hub.cogs, lut_writes) |*cog, write| {
        if (write) |value| {
            const other = cog.other();
            if (other.lut_sharing) {
                other.streamer.lut_write = .{
                    .provenance = lut_provenance[cog.id],
                    .space = .lut,
                    .cog = other.id,
                    .address = value.address,
                    .access = .write,
                    .value = value.value,
                    .class = .data,
                    .storage = .ram,
                };
            }
            const own = lut_writes[other.id];
            if (other.lut_sharing and own != null and own.?.address == value.address) {
                hub.report(.{ .lut_write_conflict = .{
                    .first = .{
                        .provenance = lut_provenance[cog.id],
                        .space = .lut,
                        .cog = other.id,
                        .address = value.address,
                        .access = .write,
                        .value = value.value,
                        .class = .data,
                        .storage = .ram,
                    },
                    .second = .{
                        .provenance = lut_provenance[other.id],
                        .space = .lut,
                        .cog = other.id,
                        .address = own.?.address,
                        .access = .write,
                        .value = own.?.value,
                        .class = .data,
                        .storage = .ram,
                    },
                } }, lut_provenance[cog.id]);
            }
            if (other.lut_sharing and (own == null or own.?.address != value.address)) {
                other.lut[value.address] = value.value;
                hub.access_context = lut_provenance[cog.id];
                hub.observe(.lut, other.id, value.address, .write, value.value, .data, .ram);
            }
        }
    }
    // All previous-edge writes are visible before any cog samples this edge.
    for (&hub.cogs) |*cog| {
        if (cog.hub_writeback) |write| {
            hub.access_context = write.provenance;
            hub.writeHubBeat(.{
                .origin = .{ .memory = write.provenance },
                .address = write.address,
                .value = write.value,
                .size = write.size,
                .masked = write.masked,
                .authorization = write.origin,
            });
            cog.hub_writeback = null;
        }
    }
    for (&hub.pending_starts, 0..) |*pending, id| {
        if (pending.*) |start| {
            std.debug.assert(clock_reached(start.at, hub.counter));
            if (start.at == hub.counter) {
                pending.* = null;
                hub.access_context = start.provenance;
                hub.start_cog(@intCast(id), .{
                    .hub_address = start.address,
                    .ptra = start.ptra,
                    .load_image = start.load,
                    .command_clock = start.command_clock,
                });
            }
        }
    }
    for (&hub.cogs) |*cog| {
        hub.access_context = cog.provenance();
        hub.read_class = .data;
        cog.clock_startup();
    }
    for (&hub.cogs) |*cog| {
        hub.access_context = cog.provenance();
        hub.read_class = .data;
        cog.clock_fifo();
    }
    for (&hub.cogs) |*cog| {
        hub.access_context = cog.provenance();
        hub.read_class = .data;
        cog.clock_memory();
    }
    for (&hub.cogs) |*cog| {
        cog.finish_reads();
    }
    if (hub.cordic.step()) |delivery| {
        hub.cogs[@intFromEnum(delivery.cog)].cordic_result.accept(delivery);
    }
    // Issue shared hub commands before advancing any cog's instruction pipeline.
    // Command side effects belong to this edge; each cog completes from its saved result.
    for (&hub.cogs) |*cog| {
        hub.access_context = cog.provenance();
        hub.read_class = .data;
        cog.clock_command();
        if (hub.reset_requested) {
            hub.firmwareReset();
            return .reset;
        }
    }
    for (&hub.cogs) |*cog| {
        hub.access_context = cog.provenance();
        cog.streamer.step(cog);
        cog.dacs = cog.converter.step(cog.raw_dacs);
    }
    hub.io.step(hub);
    {
        for (&hub.cogs) |*cog| {
            hub.access_context = cog.provenance();
            hub.read_class = .data;
            cog.step();
            if (hub.reset_requested) {
                hub.firmwareReset();
                return .reset;
            }
        }
    }
    for (&hub.cogs) |*cog| {
        cog.advance_fifo_write_ready();
        cog.advance_read_bus();
        cog.assert_read_ownership();
    }
    hub.io.advance_delays();

    hub.random.step();
    if (std.debug.runtime_safety) {
        const pending = hub.cordic.pending_counts();
        for (&hub.cogs, pending) |*cog, count| {
            std.debug.assert(cog.cordic_result.pending == count);
        }
    }
    hub.counter +%= 1;
    if (hub.debugger) |debugger| {
        debugger.afterClock(hub);
        if (debugger.stop != .none)
            return .breakpoint;
    }
    return .normal;
}

/// Append an architectural issue without changing execution or allocating.
pub fn report(hub: *Hub, kind: Issue.Kind, provenance: access_types.Provenance) void {
    hub.issues.append(.{ .clock = hub.counter, .kind = kind, .provenance = provenance });
}

/// Reset chip state while retaining reports already detected on this edge.
fn firmwareReset(hub: *Hub) void {
    const issues = hub.issues;
    const capture = hub.capture;
    hub.init(hub.system_configuration) catch unreachable;
    hub.issues = issues;
    if (capture) |recording| {
        recording.reset(.firmware);
        hub.attachCapture(recording);
    }
}

/// Return whether any cog is executing or waiting for startup to complete.
pub fn is_any_cog_active(hub: *Hub) bool {
    for (hub.cogs) |cog| {
        if (cog.exec_mode != .stopped)
            return true;
    }
    return false;
}

pub const StartCogOptions = struct {
    hub_address: u32 = 0,
    ptra: u32 = 0,
    load_image: bool = true,
    clocked: bool = true,
    command_clock: ?u64 = null,
};

/// Only the low 20 source bits select memory/PC; PTRB receives all 32 bits.
pub fn start_cog(hub: *Hub, index: u3, options: StartCogOptions) void {
    if (hub.pending_starts[index]) |previous| {
        hub.report(.{ .cog_start_replaced = .{
            .previous = previous.provenance,
            .replacement = hub.access_context,
            .target = index,
        } }, hub.access_context);
    }
    const cog = &hub.cogs[index];
    cog.reset();
    const command_clock = options.command_clock orelse hub.counter;
    // Loaded cog images retain byte alignment; the ROM's TAQOZ image starts at $FED42.
    const base = options.hub_address & 0xfffff;
    if (options.load_image) {
        if (options.clocked) {
            cog.startup = .{
                .provenance = if (hub.access_context.cog == null) .{ .cog = index } else hub.access_context,
                .address = base,
                .grant_at = command_clock +% 17 +% (((@as(u64, base) >> 2) -% (command_clock +% 17 +% index)) & 7),
                .remaining = 504,
            };
        } else {
            for (0..504) |i| {
                cog.registers.values[i] = hub.read_memory(base + @as(u32, @intCast(i * 4)), 4);
            }
        }
        cog.exec_mode = .cog;
    } else {
        cog.pc = @truncate(options.hub_address);
        cog.exec_mode = if (cog.pc < 0x400)
            .cog
        else
            .hub;
        if (options.clocked) {
            cog.startup = .{
                .provenance = if (hub.access_context.cog == null) .{ .cog = index } else hub.access_context,
                .address = base,
                .grant_at = 0,
                .remaining = 0,
                .release_at = command_clock +% @as(u64, if (cog.exec_mode == .hub)
                    17
                else
                    18),
            };
            if (cog.exec_mode == .hub)
                cog.fifo_state.engine = .{
                    .direction = .{ .read = .{} },
                    .byte_offset = @truncate(cog.pc),
                    .address = cog.pc & 0xffffc,
                    .ready_at = command_clock +% 22,
                };
        }
    }
    cog.registers.set(.PTRA, options.ptra);
    cog.registers.set(.PTRB, options.hub_address);
    cog.started_no_load = !options.load_image;
    cog.ct_targets = .{ 0, 0, 0 };
    cog.request_debug(true);
}

test "UART DIR changes once per edge across register writes, reset and simultaneous handoff" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .configure, .value = 0x7c, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .write_x, .value = (100 << 16) | 7, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    hub.cogs[0].write_reg(.DIRB, 1 << 30);
    try std.testing.expect(!hub.io.pins[62].smart.registers.enabled);
    _ = hub.step();
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .write_y, .value = 'x', .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    try std.testing.expect(hub.io.pins[62].outputs.scheduled);
    hub.cogs[0].write_reg(.DIRB, 0);
    hub.cogs[1].write_reg(.DIRB, 1 << 30);
    hub.cogs[0].reset();
    try std.testing.expect(hub.io.pins[62].smart.registers.enabled);
    try std.testing.expect(hub.io.pins[62].outputs.scheduled);

    hub.cogs[0].write_reg(.DIRB, 1 << 30);
    hub.cogs[1].reset();
    // On one edge, cog 0 releases the pin and cog 1 takes over. The OR stays
    // high; a sequential transient must not reset the smart pin.
    hub.cogs[1].writeback.count = 1;
    hub.cogs[1].writeback.writes[0] = .{ .provenance = .{ .cog = 1 }, .reg = .DIRB, .value = 1 << 30 };
    hub.cogs[0].writeback.count = 1;
    hub.cogs[0].writeback.writes[0] = .{ .provenance = .{ .cog = 0 }, .reg = .DIRB, .value = 0 };
    _ = hub.step();
    try std.testing.expect(hub.io.pins[62].outputs.scheduled);
    hub.cogs[1].write_reg(.DIRB, 0);
    try std.testing.expect(hub.io.pins[62].smart.registers.enabled);
    try std.testing.expect(hub.io.pins[62].outputs.scheduled);
    _ = hub.step();
    try std.testing.expect(!hub.io.pins[62].smart.registers.enabled);
    try std.testing.expect(!hub.io.pins[62].outputs.scheduled);
    try std.testing.expect(!hub.io.pins[62].smart.registers.ready);
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .write_y, .value = 'y', .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    try std.testing.expect(hub.io.pins[62].smart.logic.uart_tx.buffer == null);

    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 63, .kind = .configure, .value = 0x3e, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 63, .kind = .write_x, .value = (100 << 16) | 7, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    hub.cogs[0].write_reg(.DIRB, 1 << 31);
    _ = hub.step();
    var reader = std.Io.Reader.fixed("z");
    var source: IO.DataSource = .{ .endpoint = .{ .reader = &reader } };
    hub.io.pins[63].smart.rx_mode = .{ .emulated = &source };
    _ = hub.step();
    try std.testing.expectEqual(@as(usize, 0), reader.bufferedLen());
    hub.cogs[0].write_reg(.DIRB, 0);
    hub.counter +%= 1000;
    _ = hub.step();
    try std.testing.expect(!hub.io.pins[63].smart.registers.ready);
    try std.testing.expectEqual(@as(u32, 0), hub.io.pins[63].smart.registers.result);
}

test "COGSTOP aborts a UART frame before its completion edge" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var sink: IO.DataSink = .{ .endpoint = .{ .writer = &output.writer } };
    const pin = &hub.io.pins[62];
    pin.smart.tx_mode = .{ .emulated = &sink };
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .configure, .value = 0x7c, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .write_x, .value = (100 << 16) | 7, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    hub.cogs[0].write_reg(.DIRB, 1 << 30);
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .write_y, .value = 'x', .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    _ = hub.step();
    const completion = hub.counter + 1000;
    try std.testing.expect(pin.smart.logic.uart_tx.shifter != null);

    // Execute a real self-stop while the frame is underway, then service its completion edge.
    hub.cogs[0].write_reg(@enumFromInt(0), 0xFD64_0003); // COGSTOP #0.
    hub.cogs[0].exec_mode = .cog;
    while (hub.is_any_cog_active() and hub.counter < completion) {
        _ = hub.step();
    }
    try std.testing.expect(!hub.is_any_cog_active());
    try std.testing.expect(hub.counter < completion);
    hub.counter = completion;
    _ = hub.step();
    try std.testing.expect(!pin.smart.registers.enabled);
    try std.testing.expect(pin.smart.logic.uart_tx.shifter == null);
    try std.testing.expect(pin.smart.logic.uart_tx.buffer == null);
    try std.testing.expect(!pin.outputs.scheduled);
    try std.testing.expectEqualStrings("", output.written());
}

test "event delivery coalesces without crossing cogs and reset cancels queued latches" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const events = std.enums.values(EventId);
    for (events) |event| {
        hub.cogs[0].signal_event(event);
    }
    try std.testing.expect(hub.cogs[0].events.eql(.full));
    hub.cogs[0].reset();
    hub.clocking = true;
    for (events) |event| {
        hub.cogs[0].signal_event(event);
        hub.cogs[0].signal_event(event);
        hub.cogs[1].signal_event(event);
        hub.cogs[2].signal_event(event);
    }
    hub.clocking = false;
    hub.cogs[0].events = .full;
    hub.cogs[0].reset();
    try std.testing.expectEqual(@as(usize, 0), hub.cogs[0].events.count());
    for (0..3) |_| {
        _ = hub.step();
        try std.testing.expectEqual(@as(usize, 0), hub.cogs[0].events.count());
        for (hub.cogs[3..]) |cog| {
            try std.testing.expectEqual(@as(usize, 0), cog.events.count());
        }
    }
    try std.testing.expectEqual(@as(usize, 0), hub.cogs[0].events.count());
    try std.testing.expect(hub.cogs[1].events.eql(.full));
    try std.testing.expect(hub.cogs[2].events.eql(.full));
    for (hub.cogs[3..]) |cog| {
        try std.testing.expectEqual(@as(usize, 0), cog.events.count());
    }
    for (hub.cogs) |cog| {
        for (cog.pending_events) |pending| {
            try std.testing.expectEqual(@as(usize, 0), pending.flags.count());
            try std.testing.expectEqual(@as(usize, 0), pending.requests.count());
        }
    }
}

test "pending events arrive during a timed wait" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (std.enums.values(EventId)) |event| {
        try hub.init(.ideal);
        const cog = &hub.cogs[0];
        cog.exec_mode = .cog;
        cog.pipeline[3] = .{ .pc = 0, .instr = 0xFD64_161F }; // WAITX #11.
        _ = hub.step();
        hub.clocking = true;
        cog.signal_event(event);
        hub.clocking = false;
        for (0..3) |_| {
            _ = hub.step();
        }
        try std.testing.expect(cog.events.contains(event));
        // WAITX ignores unrelated event latches after delivery has settled.
    }
}

test "pending events keep their two-clock delay across counter rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.counter = std.math.maxInt(u64) - 1;
    hub.clocking = true;
    hub.cogs[0].signal_event(.QMT);
    hub.clocking = false;
    for (0..2) |_| {
        _ = hub.step();
        try std.testing.expectEqual(@as(usize, 0), hub.cogs[0].events.count());
    }
    _ = hub.step();
    try std.testing.expect(hub.cogs[0].events.eql(.initOne(.QMT)));
}

test "FIFO and hub reads keep five-clock responses across counter rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (0..8) |offset| {
        try hub.init(.ideal);
        hub.counter = std.math.maxInt(u64) - offset;
        const fifo_address: u32 = @intCast((hub.counter & 7) * 4);
        const read_address: u32 = @intCast(((hub.counter +% 1) & 7) * 4);
        hub.write_memory(fifo_address, 0x1122_3344, 4, false);
        hub.write_memory(read_address, 0x5566_7788, 4, false);
        hub.cogs[0].fifo_state.engine = .{ .direction = .{ .read = .{} }, .address = @intCast(fifo_address), .byte_offset = 0, .ready_at = hub.counter };
        hub.cogs[1].memory_transfer = .{
            .provenance = .{ .cog = 1 },
            .address = read_address,
            .remaining = 1,
            .reg = 20,
            .lut = false,
            .timed = true,
            .size = 4,
            .grant_at = hub.counter,
            .direction = .{ .read = .{ .grant_address = read_address, .grants_left = 1 } },
        };
        for (0..5) |_| {
            _ = hub.step();
            try std.testing.expectEqual(@as(usize, 0), hub.cogs[0].fifo_state.engine.?.count);
            try std.testing.expect(hub.cogs[1].memory_transfer.?.direction.read.value == null);
        }
        _ = hub.step();
        try std.testing.expectEqual(@as(usize, 1), hub.cogs[0].fifo_state.engine.?.count);
        try std.testing.expectEqual(@as(u32, 0x1122_3344), hub.cogs[0].peek_fifo(0).? | (@as(u32, hub.cogs[0].peek_fifo(1).?) << 8) | (@as(u32, hub.cogs[0].peek_fifo(2).?) << 16) | (@as(u32, hub.cogs[0].peek_fifo(3).?) << 24));
        try std.testing.expectEqual(@as(?u32, 0x5566_7788), hub.cogs[1].memory_transfer.?.direction.read.value);
    }
}

test "wait deadlines remain future across counter rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.counter = std.math.maxInt(u64) - 4;
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.pipeline[3] = .{ .pc = 0, .instr = 0xFD64_161F }; // WAITX #11.
    _ = hub.step();
    try std.testing.expectEqual(@as(?u64, 6), cog.wait_until);
    for (1..11) |_| {
        _ = hub.step();
        try std.testing.expect(cog.wait_until != null);
    }
    _ = hub.step();
    try std.testing.expect(cog.wait_until == null);
}

test "UART and image startup finish at their scheduled clocks across rollover" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.counter = std.math.maxInt(u64) - 10;
    var output: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer output.deinit();

    var terminal_sink: IO.DataSink = .{ .endpoint = .{ .writer = &output.writer } };
    hub.io.pins[62].smart.tx_mode = .{ .emulated = &terminal_sink };
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .configure, .value = 0x7c, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .write_x, .value = (8 << 16) | 7, .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 1) |_| {
        _ = hub.step();
    }
    hub.cogs[0].write_reg(.DIRB, 1 << 30);
    try std.testing.expect(!hub.io.pins[62].smart.registers.enabled);
    _ = hub.step();
    hub.io.enqueue(hub, .{ .mask = @as(u64, 1) << 62, .kind = .write_y, .value = 'x', .cog = 0, .pc = 0, .instruction = 0 });
    for (0..IO.command_delay + 81) |_| {
        _ = hub.step();
        try std.testing.expectEqual(@as(usize, 0), output.written().len);
    }
    _ = hub.step();
    _ = hub.step();
    try std.testing.expectEqualStrings("x", output.written());

    try hub.init(.ideal);
    hub.counter = std.math.maxInt(u64) - 4;
    // The first loaded instruction stops cog 1; the other words are data.
    hub.write_memory(0x1000, 0xFD64_0203, 4, false);
    for (1..504) |i| {
        hub.write_memory(@intCast(0x1000 + i * 4), @intCast(i), 4, false);
    }
    hub.start_cog(1, .{ .hub_address = 0x1000 });
    for (0..550) |_| {
        _ = hub.step();
    }
    try std.testing.expect(hub.cogs[1].startup == null);
    try std.testing.expectEqual(Cog.ExecMode.stopped, hub.cogs[1].exec_mode);
    try std.testing.expectEqual(@as(u32, 0xFD64_0203), hub.cogs[1].registers.values[0]);
    for (1..504) |i| {
        try std.testing.expectEqual(@as(u32, @intCast(i)), hub.cogs[1].registers.values[i]);
    }
}

test "pattern conditions are sampled while a cog waits" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.pipeline[3] = .{ .pc = 0, .instr = 0xFD64_C81F }; // WAITX #100.
    cog.z = true;
    cog.configure_pattern(1, 1);
    _ = hub.step(); // Stable zero input does not match.
    // Model a new architectural snapshot, independently of I/O propagation timing.
    hub.io.samples.input_registers = 1;
    cog.step();
    try std.testing.expect(cog.events.contains(.PAT));
    cog.events.remove(.PAT);
    hub.io.samples.input_registers = 0;
    cog.step();
}

test "selectable pin samples retain delayed notifications during waits" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    const cog = &hub.cogs[0];
    cog.exec_mode = .cog;
    cog.pipeline[3] = .{ .pc = 0, .instr = 0xFD64_C81F }; // WAITX #100.
    cog.configure_selectable(.SE1, 0x100); // Held low input.
    cog.configure_interrupt(.int1, @intFromEnum(EventId.SE1));
    cog.interrupts.stalled = true;
    _ = hub.step();
    try std.testing.expect(!cog.events.contains(.SE1));
    // Reconfiguration clears the latch, but cannot retract the sample already in flight.
    cog.configure_selectable(.SE1, 0x40);
    _ = hub.step();
    try std.testing.expect(!cog.events.contains(.SE1));
    _ = hub.step();
    try std.testing.expect(!cog.events.contains(.SE1));
    try std.testing.expectEqual(Cog.InterruptState.pending, cog.interrupts.levels.get(.int1).state);
    _ = hub.step();
    try std.testing.expect(!cog.events.contains(.SE1));
    _ = hub.step();
    try std.testing.expect(cog.events.contains(.SE1));
    cog.cancel_interrupt(.int1);
    cog.events.remove(.SE1);
    // Only the sampled event/architectural input buses change; no pad propagation is involved.
    hub.io.samples.input_registers = 1;
    hub.io.samples.inputs = @splat(1);
    cog.step();
    try std.testing.expect(cog.events.contains(.SE1));
    cog.reset();
    for (cog.pending_events) |pending| {
        try std.testing.expect(!pending.flags.contains(.SE1));
    }
    for (cog.selectable_events) |selection| {
        try std.testing.expect(selection == null);
    }
}

test "cog restart clears pending interrupts and controller state" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (std.enums.values(Cog.InterruptLevel)) |level| {
        try hub.init(.ideal);
        hub.start_cog(3, .{ .hub_address = 0x4000, .load_image = false, .clocked = false });
        const cog = &hub.cogs[3];
        try std.testing.expect(cog.started_no_load);
        try std.testing.expectEqual(@as(u32, 1), cog.interrupt_status());
        cog.exec_mode = .cog;
        cog.pipeline[3] = .{ .pc = 0, .instr = 0xFD64_C81F }; // WAITX #100.
        _ = hub.step();
        cog.interrupts.stalled = true;
        cog.trigger_interrupt(level);
        hub.start_cog(3, .{ .clocked = false });
        try std.testing.expect(!cog.started_no_load);
        try std.testing.expect(!cog.interrupts.stalled);
        try std.testing.expectEqual(Cog.InterruptState.idle, cog.interrupts.levels.get(level).state);
        try std.testing.expectEqual(@as(?u32, 0), cog.ct_targets[0]);
    }
}

test "one shared RNG advances through stopped clocks, rollover and slotted HUBSET seeds" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    var expected = hub.random;
    hub.counter = std.math.maxInt(u64) - 3;
    for (0..16) |_| {
        const before = hub.random;
        for (0..8) |id| {
            const word = hub.random.word(@intCast(id));
            try std.testing.expectEqual(word, hub.random.word(@intCast(id)));
        }
        try std.testing.expectEqualDeep(before, hub.random);
        try std.testing.expect(!hub.is_any_cog_active());
        _ = hub.step();
        expected.step();
        try std.testing.expectEqualDeep(expected, hub.random);
        // Pad noise borrows the same output sampled by the cogs on this edge.
        try std.testing.expectEqual(before.output, hub.io.random_bits);
    }
    try std.testing.expectEqual(@as(u64, 12), hub.counter);

    for (0..8) |id| {
        for (0..8) |phase| {
            try hub.init(.ideal);
            for (0..16) |_| {
                _ = hub.step();
            }
            hub.counter = phase;
            const cog = &hub.cogs[id];
            cog.exec_mode = .cog;
            cog.write_reg(@enumFromInt(24), 0x80001234);
            cog.pipeline[3] = .{ .pc = 0, .instr = 0xfd603000 };
            expected = hub.random;
            const grant = phase + ((id -% phase) & 7);
            for (0..10) |_| {
                if (hub.counter == grant) {
                    expected.seed(0x80001234);
                }
                expected.step();
                _ = hub.step();
                try std.testing.expectEqualDeep(expected, hub.random);
                try std.testing.expectEqual(@as(u32, 0), hub.io.clock_mode);
            }
        }
    }
}

test "ROM cache aliases, byte protection, masked wrapping and configuration lock" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    try std.testing.expectEqualSlices(u8, boot_rom, hub.ram.bytes()[0x7c000..]);
    try std.testing.expectEqual(hub.read_memory(0x7c000, 4), hub.read_memory(0xfc000, 4));
    hub.write_memory(0x7ffff, 0x1122_3344, 4, false);
    try std.testing.expectEqual(@as(u32, 0x44), hub.read_memory(0xfffff, 1));
    hub.configure(0x2003_00ff);
    try std.testing.expectEqual(@as(u32, 0), hub.read_memory(0x7ffff, 4));
    hub.write_memory_from(0x7ffff, 0xdd, 1, false, .debug);
    hub.write_memory(0xfffff, 0xaabb_ccdd, 4, false);
    try std.testing.expectEqual(@as(u32, 0xaabb_cc44), hub.read_memory(0xfffff, 4));
    hub.write_memory_from(0xfffff, 0x0055_0066, 4, true, .debug);
    try std.testing.expectEqual(@as(u32, 0xaa55_cc66), hub.read_memory(0xfffff, 4));
    hub.configure(0x2000_0000);
    try std.testing.expect(hub.debug.protected and hub.debug.locked);
    try std.testing.expectEqual(@as(u8, 255), hub.debug.enables);
    hub.start_cog(0, .{ .clocked = false });
    try std.testing.expect(hub.debug.locked);
    try std.testing.expectEqual(Cog.InterruptState.pending, hub.cogs[0].debug.state);
    try hub.init(.ideal);
    try std.testing.expect(!hub.debug.locked);
    try std.testing.expectEqualSlices(u8, boot_rom, hub.ram.bytes()[0x7c000..]);
}

test "debug ROM saves and restores sixteen registers for all eight cogs" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    for (0..8) |id| {
        try hub.init(.ideal);
        hub.configure(0x2000_00ff);
        const cog = &hub.cogs[id];
        const base: u32 = 0xff800 + ((15 ^ @as(u32, @intCast(id))) << 7);
        hub.write_memory(base + 0x40, 0xFD64_2036, 4, false); // BRK #16.
        hub.write_memory(base + 0x44, 0xFD80_01fd, 4, false); // JMP #$1FD.
        hub.start_cog(@intCast(id), .{ .load_image = false, .clocked = false, .ptra = 0x1234 });
        cog.registers.values[0] = 0xF604_2807; // MOV r20,#7.
        for (1..16) |reg| {
            cog.registers.values[reg] = 0;
        }
        for (0..200) |_| {
            _ = hub.step();
            if (cog.registers.values[20] == 7) {
                break;
            }
        }
        try std.testing.expect(hub.issues.len == 0);
        try std.testing.expectEqual(Cog.InterruptState.idle, cog.debug.state);
        try std.testing.expectEqual(@as(u32, 7), cog.registers.values[20]);
        try std.testing.expectEqual(@as(u32, 0xF604_2807), hub.read_memory(base, 4));
        try std.testing.expectEqual(@as(u32, 0x1234), cog.read_reg(.PTRA));
        try std.testing.expectEqual(@as(u32, 0), cog.read_reg(.PTRB));
        try std.testing.expect(!cog.events.contains(.INT));
    }
}

test "deferred write authorization survives a debug state transition" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    hub.configure(0x2001_0001);
    const before = hub.read_memory(0xfc000, 4);
    const cog = &hub.cogs[0];
    cog.debug.state = .active;
    cog.hub_writeback = .{ .provenance = cog.provenance(), .address = 0xfc000, .value = 0xdead_beef, .size = 4, .masked = false, .origin = .application };
    hub.counter = cog.memory_grant_after(0xfc000, 0);
    _ = hub.step();
    try std.testing.expectEqual(before, hub.read_memory(0xfc000, 4));
    cog.debug.state = .idle;
    cog.hub_writeback = .{ .provenance = cog.provenance(), .address = 0xfc000, .value = 0x1234_5678, .size = 4, .masked = false, .origin = .debug };
    hub.counter = cog.memory_grant_after(0xfc000, 0);
    _ = hub.step();
    try std.testing.expectEqual(@as(u32, 0x1234_5678), hub.read_memory(0xfc000, 4));
}

test "HUBSET reset clears the entire Hub even while a debugger stop is pending" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    try hub.init(.ideal);
    @memset(hub.ram.bytes(), 0xaa);
    hub.counter = 123;
    hub.debug = .{ .enables = 0xff, .protected = true, .locked = true };
    hub.random.seed(0x87654321);
    try hub.io.setExternal(@enumFromInt(0), .high);
    hub.io.pins[0].smart.registers.x = 99;
    hub.locks[0].allocated = true;
    hub.locks[0].taken = true;
    for (&hub.cogs) |*cog| {
        cog.exec_mode = .cog;
        cog.registers.set(.OUTA, 1);
        cog.lut[0] = 42;
        cog.begin_fifo(.write, 0, 0x1000);
        try std.testing.expect(cog.write_fifo(0xaabbccdd, 4));
    }
    var debugger: Debugger = .{ .allocator = std.testing.allocator, .stop = .execution };
    defer debugger.deinit();

    hub.debugger = &debugger;
    hub.configure(0x1fff_ffff);
    try std.testing.expectEqual(StepResult.reset, hub.step());
    try std.testing.expectEqual(@as(u64, 0), hub.counter);
    try std.testing.expectEqual(@as(f64, 24_000_000), hub.io.clock.frequency());
    try std.testing.expectEqual(@as(u8, 0), hub.debug.enables);
    try std.testing.expect(!hub.debug.protected and !hub.debug.locked);
    try std.testing.expectEqualDeep(Random{}, hub.random);
    try std.testing.expectEqualDeep(IO.Pad.Source.float, hub.io.external[0]);
    try std.testing.expect(hub.io.getExternal(@enumFromInt(0)) == null);
    try std.testing.expectEqual(@as(u32, 0), hub.io.pins[0].smart.registers.x);
    try std.testing.expect(!hub.locks[0].allocated and !hub.locks[0].taken);
    try std.testing.expect(hub.debugger == null);
    try std.testing.expect(hub.issues.len == 0 and !hub.reset_requested and !hub.clocking);
    try std.testing.expect(std.mem.allEqual(u8, hub.ram.bytes()[0..0x7c000], 0));
    try std.testing.expectEqualSlices(u8, boot_rom, hub.ram.bytes()[0x7c000..]);
    for (&hub.cogs, 0..) |*cog, id| {
        try std.testing.expectEqual(Cog.ExecMode.stopped, cog.exec_mode);
        try std.testing.expect(cog.hub == hub and cog.id == id);
        try std.testing.expectEqual(@as(u32, 0), cog.registers.get(.OUTA));
        try std.testing.expectEqual(@as(u32, 0), cog.lut[0]);
        try std.testing.expect(cog.fifo_state.engine == null);
    }
    try std.testing.expectEqual(StepResult.normal, hub.step());
    try std.testing.expectEqual(@as(u64, 1), hub.counter);
}

test "converter clocks independently of execution and cog reset clears only its own DAC state" {
    var hub: Hub = undefined;
    try hub.init(.ideal);
    const cog = &hub.cogs[0];
    cog.streamer.apply(cog, .{ .dacs = 0x7856_3412 });
    cog.converter.apply(.{ .cy = 0x8000_0001 });
    cog.converter.apply(.{ .ci = 0x0080_0002 });
    cog.converter.apply(.{ .cq = 0x0000_8003 });
    cog.converter.apply(.{ .control = 0x120 });
    for (0..6) |_| {
        _ = hub.step();
    }
    try std.testing.expectEqual([4]u8{ 18, 52, 86, 120 }, cog.raw_dacs);
    try std.testing.expectEqual([4]u8{ 0, 55, 88, 121 }, cog.dacs);
    try std.testing.expectEqual(@as(u32, 1 << 22), cog.interrupt_status());
    try std.testing.expectEqual([4]u8{ 0, 0, 0, 0 }, hub.cogs[1].dacs);
    const adjacent = hub.cogs[1].converter;
    cog.reset();
    try std.testing.expectEqual([4]u8{ 0, 0, 0, 0 }, cog.dacs);
    try std.testing.expectEqual([4]u8{ 0, 0, 0, 0 }, cog.raw_dacs);
    try std.testing.expect(!cog.converter.active());
    try std.testing.expectEqualDeep(adjacent, hub.cogs[1].converter);
}

test "system configuration initializes atomically and survives firmware reset" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);

    var configuration: SystemConfiguration = .realistic;
    configuration.oversample = 8;
    configuration.vio_voltage = 2.5;
    configuration.threshold_ratio = 0.25;
    try hub.init(configuration);
    hub.ram.bytes()[0] = 0xaa;
    hub.counter = 7;
    try hub.io.setExternal(@enumFromInt(10), .{ .voltage = 1, .resistance = 1 });
    try std.testing.expectEqual(StepResult.normal, hub.step());
    try std.testing.expectEqual(IO.Signal.high, hub.io.pads[10]);
    const state = hub.io.getExternal(@enumFromInt(10)).?;
    var invalid = configuration;
    invalid.pad_capacitance = 0;
    try std.testing.expectError(error.InvalidConfiguration, hub.init(invalid));
    try std.testing.expectEqualDeep(configuration, hub.system_configuration);
    try std.testing.expectEqual(@as(u8, 0xaa), hub.ram.bytes()[0]);
    try std.testing.expectEqual(@as(u64, 8), hub.counter);
    try std.testing.expectEqualDeep(state, hub.io.getExternal(@enumFromInt(10)).?);
    hub.configure(0x1000_0000);
    try std.testing.expectEqual(StepResult.reset, hub.step());
    try std.testing.expectEqualDeep(configuration, hub.system_configuration);
    try std.testing.expectEqual(@as(u64, 0), hub.counter);
    try std.testing.expectEqual(@as(u8, 0), hub.ram.bytes()[0]);
    try std.testing.expect(hub.io.getExternal(@enumFromInt(10)) == null);
    try std.testing.expectEqualDeep(IO.Pad.Source.float, hub.io.external[10]);
    try hub.init(.ideal);
    try std.testing.expectEqualDeep(SystemConfiguration.ideal, hub.system_configuration);
}

test "an unready empirical PLL advances shared physical time and pads without CT or synchronous work" {
    const hub = try std.testing.allocator.create(Hub);
    defer std.testing.allocator.destroy(hub);
    var configuration: SystemConfiguration = .realistic;
    configuration.clocks.rcfast = IO.Clock.Configuration.ideal.rcfast;
    try hub.init(configuration);
    try hub.io.setExternal(@enumFromInt(2), .{ .voltage = 3.3, .resistance = 1000, .capacitance = 1e-8 });
    var capture = try Capture.init(std.testing.allocator, Capture.minimum_memory, .stop, hub.io.captureState(hub.io.pads));
    defer capture.deinit();

    hub.attachCapture(&capture);
    hub.configure(0x010007fb);
    const random_before = hub.random;
    for (0..100) |_| {
        try std.testing.expectEqual(StepResult.normal, hub.step());
        try std.testing.expectEqual(@as(u64, 0), hub.counter);
        try std.testing.expectEqualDeep(random_before, hub.random);
    }
    try std.testing.expect(hub.io.getExternal(@enumFromInt(2)).?.voltage > 1);
    try std.testing.expect(hub.io.clock.elapsed.femtoseconds > 9_000_000_000);
    try std.testing.expectEqual(hub.io.clock.elapsed.femtoseconds, capture.metadataConst().end);
    var iterations: usize = 100;
    while (hub.counter == 0 and iterations < 10000) : (iterations += 1) {
        _ = hub.step();
    }
    try std.testing.expectEqual(@as(u64, 1), hub.counter);
    try std.testing.expectEqual(hub.io.clock.elapsed.femtoseconds, capture.metadataConst().end);
    try std.testing.expectEqual(hub.io.clock.elapsed.fraction, capture.metadataConst().time.fraction);
}
