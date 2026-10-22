//! Bounded, allocation-free recording of timestamped electrical samples.
const std = @import("std");
const Clock = @import("Clock.zig");
const builtin = @import("builtin");
const Capture = @This();

pub const Level = enum(u1) { low, high };
pub const DigitalSample = packed struct(u128) { timestamp: u127, value: Level };
pub const AnalogSample = packed struct(u128) { timestamp: u112, value: u16 };
pub const ResetKind = enum(u16) { firmware, reinitialize };
pub const ResetSample = packed struct(u128) { timestamp: u112, value: ResetKind };
pub const BlockId = enum(u32) { none = 0xffffffff, _ };
pub const Overflow = enum(u8) { rolling, stop };
pub const Status = enum(u8) { recording, frozen, full };
pub const InitialState = struct {
    digital: [64]Level,
    analog: [64]u16,
};
pub const block_size = 4096;
pub const samples_per_block = (block_size - 16) / @sizeOf(u128);
pub const default_memory = 256 * 1024 * 1024;
pub const minimum_memory = 2 * 1024 * 1024;

pub const CntOrBlockId = packed struct(u32) {
    payload: u31,
    is_next: bool,

    /// Read a successor only when the current block is implicitly full.
    pub fn next(control: CntOrBlockId) BlockId {
        return if (control.is_next)
            @enumFromInt(control.payload)
        else
            .none;
    }

    /// Linked blocks are full; tails carry their explicit count.
    pub fn count(control: CntOrBlockId) usize {
        return if (control.is_next)
            samples_per_block
        else
            control.payload;
    }
};

/// Cache values in descriptor records so unchanged observations never read sample blocks.
pub fn Channel(comptime Value: type) type {
    return packed struct(u128) {
        first_block: BlockId = .none,
        latest_block: BlockId = .none,
        last_value: Value = std.mem.zeroes(Value),
        reserved: std.meta.Int(.unsigned, 64 - @bitSizeOf(Value)) = 0,
    };
}

/// Descriptor and sample blocks share one page-aligned layout with an explicit 16-byte prefix.
pub fn Block(comptime Sample: type) type {
    return extern struct {
        control: CntOrBlockId align(block_size),
        reserved: [12]u8,
        samples: [samples_per_block]Sample,
    };
}

const Storage = extern union {
    digital: Block(DigitalSample),
    analog: Block(AnalogSample),
    reset: Block(ResetSample),
    descriptor: Block(u128),
};

/// Explicitly laid-out physical time stored in the image, including its fractional carry.
pub const Time = extern struct {
    femtoseconds: u128 = 0,
    fraction: u64 = 0,
    reserved: u64 = 0,

    /// Reuse the clock's Q64 arithmetic without persisting its compiler-defined runtime layout.
    pub fn add(time: *Time, interval: u128) void {
        var physical: Clock.Time = .{ .femtoseconds = time.femtoseconds, .fraction = time.fraction };
        physical.add(interval);
        time.femtoseconds = physical.femtoseconds;
        time.fraction = physical.fraction;
    }
};

/// Native image header fused into the first descriptor block payload; every byte, including reserved space, has a defined layout.
pub const Header = extern struct {
    magic: [8]u8 = "WTSCOPE\x00".*,
    version: u32 = 1,
    block_size: u32 = Capture.block_size,
    block_count: u32,
    descriptor_blocks: u32 = Capture.descriptor_blocks,
    descriptor_bytes: u32 = Capture.descriptor_records * @sizeOf(u128),
    channel_count: u32 = 129,
    free_head: BlockId,
    free_count: u32,
    overflow: Overflow,
    status: Status = .recording,
    reserved_flags: [6]u8 = @splat(0),
    time: Time = .{},
    start: u128 = 0,
    end: u128 = 0,
    minimum_substep: u128 = 0,
    reserved: [128]u8 = @splat(0),
};

const header_records = @sizeOf(Header) / @sizeOf(u128);
const channel_count = 129;
const descriptor_records = header_records + channel_count;
pub const descriptor_blocks = (descriptor_records + samples_per_block - 1) / samples_per_block;

comptime {
    std.debug.assert(@sizeOf(DigitalSample) == 16 and @alignOf(DigitalSample) == 16);
    std.debug.assert(@sizeOf(AnalogSample) == 16 and @alignOf(AnalogSample) == 16);
    std.debug.assert(@sizeOf(Storage) == block_size and @alignOf(Storage) == block_size);
    std.debug.assert(@offsetOf(Block(DigitalSample), "samples") == 16);
    std.debug.assert(@offsetOf(Block(AnalogSample), "samples") == 16);
    std.debug.assert(@sizeOf(Time) == 32 and @offsetOf(Time, "fraction") == 16);
    std.debug.assert(@sizeOf(Header) == 256);
    std.debug.assert(@offsetOf(Header, "version") == 8);
    std.debug.assert(@offsetOf(Header, "block_count") == 16);
    std.debug.assert(@offsetOf(Header, "free_head") == 32);
    std.debug.assert(@offsetOf(Header, "overflow") == 40);
    std.debug.assert(@offsetOf(Header, "time") == 48);
    std.debug.assert(@offsetOf(Header, "start") == 80);
    std.debug.assert(@offsetOf(Header, "end") == 96);
    std.debug.assert(@offsetOf(Header, "minimum_substep") == 112);
    std.debug.assert(@offsetOf(Header, "reserved") == 128);
    std.debug.assert(@sizeOf(Channel(Level)) == 16 and @sizeOf(Channel(u16)) == 16);
    std.debug.assert(@sizeOf(Channel(ResetKind)) == 16);
    std.debug.assert(@sizeOf(Header) % @sizeOf(u128) == 0);
    std.debug.assert(@sizeOf(Block(u128)) == block_size);
    std.debug.assert(@alignOf(Block(u128)) == block_size);
}

allocator: std.mem.Allocator,
blocks: []Storage,

/// Access the image header within the first block's ordinary payload.
pub fn metadata(capture: *Capture) *Header {
    return @ptrCast(&capture.blocks[0].descriptor.samples);
}

/// Read the first descriptor block without permitting mutation through the handle.
pub fn metadataConst(capture: *const Capture) *const Header {
    return @ptrCast(&capture.blocks[0].descriptor.samples);
}

/// Locate typed descriptor records in the same indexed blocks used for samples and free slots.
fn descriptorRange(capture: *const Capture, comptime Value: type, comptime first: usize, comptime count: usize) *[count]Value {
    comptime {
        std.debug.assert(@sizeOf(Value) == @sizeOf(u128));
        std.debug.assert(first / samples_per_block == (first + count - 1) / samples_per_block);
        std.debug.assert(first + count <= descriptor_records);
    }
    return @ptrCast(&capture.blocks[first / samples_per_block].descriptor.samples[first % samples_per_block]);
}

/// Access digital channel records, retaining the cached-value fast path in descriptor storage.
pub fn digitalChannels(capture: *Capture) *[64]Channel(Level) {
    return capture.descriptorRange(Channel(Level), header_records, 64);
}

/// Read digital channel records directly from the image.
pub fn digitalChannelsConst(capture: *const Capture) *const [64]Channel(Level) {
    return capture.descriptorRange(Channel(Level), header_records, 64);
}

/// Access analog channel records in the descriptor payload.
pub fn analogChannels(capture: *Capture) *[64]Channel(u16) {
    return capture.descriptorRange(Channel(u16), header_records + 64, 64);
}

/// Read analog channel records directly from the image.
pub fn analogChannelsConst(capture: *const Capture) *const [64]Channel(u16) {
    return capture.descriptorRange(Channel(u16), header_records + 64, 64);
}

/// Access the reset-event chain descriptor in the same reserved record sequence.
pub fn resetChannel(capture: *Capture) *Channel(ResetKind) {
    return &capture.descriptorRange(Channel(ResetKind), header_records + 128, 1)[0];
}

/// Read the reset-event chain descriptor without exposing mutable records.
pub fn resetChannelConst(capture: *const Capture) *const Channel(ResetKind) {
    return &capture.descriptorRange(Channel(ResetKind), header_records + 128, 1)[0];
}

/// Allocate and initialize the complete image; metadata blocks never enter the free list.
pub fn init(allocator: std.mem.Allocator, memory: usize, overflow: Overflow, initial: InitialState) !Capture {
    if (builtin.cpu.arch.endian() != .little)
        return error.UnsupportedCaptureEndianness;
    if (memory < minimum_memory)
        return error.CaptureMemoryTooSmall;
    const count = memory / @sizeOf(Storage);
    if (count > std.math.maxInt(u31))
        return error.CaptureMemoryTooLarge;
    const blocks = try allocator.alloc(Storage, count);
    @memset(std.mem.sliceAsBytes(blocks), 0);
    var capture: Capture = .{ .allocator = allocator, .blocks = blocks };
    // Bootstrap the free list before borrowing descriptor blocks through the ordinary allocator.
    for (blocks, 0..) |*slot, index| {
        slot.descriptor.control = if (index + 1 < count)
            .{ .payload = @intCast(index + 1), .is_next = true }
        else
            .{ .payload = 0, .is_next = false };
    }
    capture.metadata().* = .{
        .block_count = @intCast(count),
        .free_head = @enumFromInt(0),
        .free_count = @intCast(count),
        .overflow = overflow,
    };
    var remaining: usize = descriptor_records;
    var previous: BlockId = .none;
    for (0..descriptor_blocks) |index| {
        const id = capture.allocate();
        std.debug.assert(@intFromEnum(id) == index);
        if (previous != .none) {
            const preceding = &blocks[@intFromEnum(previous)].descriptor;
            std.debug.assert(preceding.control.count() == samples_per_block);
            preceding.control = .{ .payload = @intCast(@intFromEnum(id)), .is_next = true };
        }
        const records = @min(remaining, samples_per_block);
        blocks[@intFromEnum(id)].descriptor.control = .{ .payload = @intCast(records), .is_next = false };
        remaining -= records;
        previous = id;
    }
    std.debug.assert(remaining == 0);
    for (capture.digitalChannels(), initial.digital) |*channel, value| {
        channel.* = .{ .last_value = value };
    }
    for (capture.analogChannels(), initial.analog) |*channel, value| {
        channel.* = .{ .last_value = value };
    }
    capture.resetChannel().* = .{};
    return capture;
}

/// Release the single backing allocation after detaching from the simulator.
pub fn deinit(capture: *Capture) void {
    capture.allocator.free(capture.blocks);
    capture.* = undefined;
}

/// Report occupied image blocks, including the descriptor prefix.
pub fn memoryUsed(capture: *const Capture) usize {
    return (capture.blocks.len - capture.metadataConst().free_count) * @sizeOf(Storage);
}

/// Report the entire block image, rounded down from the requested budget.
pub fn memoryCapacity(capture: *const Capture) usize {
    return capture.blocks.len * @sizeOf(Storage);
}

/// Quantize in-range voltages, reserving both integer endpoints for explicit clipping.
pub fn voltage(value: f64) u16 {
    std.debug.assert(std.math.isFinite(value));
    if (value < -2)
        return 0;
    if (value > 4.5)
        return std.math.maxInt(u16);
    return 1 + @as(u16, @intFromFloat(@round((value + 2) * 10_000)));
}

/// Decode a sample for plotting; clipping codes draw on the corresponding range boundary.
pub fn volts(value: u16) f64 {
    return switch (value) {
        0 => -2,
        std.math.maxInt(u16) => 4.5,
        else => @as(f64, @floatFromInt(value - 1)) / 10_000 - 2,
    };
}

/// Access the typed view belonging to a channel; block IDs carry no arithmetic semantics.
fn block(capture: *const Capture, comptime Sample: type, id: BlockId) *Block(Sample) {
    std.debug.assert(@intFromEnum(id) >= descriptor_blocks and @intFromEnum(id) < capture.blocks.len);
    return @ptrCast(&capture.blocks[@intFromEnum(id)]);
}

/// Determine whether a changed value needs another slot, without traversing the chain.
fn needsBlock(capture: *const Capture, channel: anytype) bool {
    const tail = channel.latest_block;
    if (tail == .none)
        return true;
    return capture.block(DigitalSample, tail).control.count() == samples_per_block;
}

/// Borrow any free slot, including descriptor slots during bootstrap, through the shared allocator.
fn allocate(capture: *Capture) BlockId {
    const id = capture.metadata().free_head;
    std.debug.assert(id != .none);
    const slot = &capture.blocks[@intFromEnum(id)].descriptor;
    capture.metadata().free_head = slot.control.next();
    capture.metadata().free_count -= 1;
    slot.control = .{ .payload = 0, .is_next = false };
    return id;
}

/// Return an unlinked block to the common free list.
fn release(capture: *Capture, id: BlockId) void {
    const head = capture.metadata().free_head;
    capture.block(DigitalSample, id).control = if (head != .none)
        .{ .payload = @intCast(@intFromEnum(head)), .is_next = true }
    else
        .{ .payload = 0, .is_next = false };
    capture.metadata().free_head = id;
    capture.metadata().free_count += 1;
}

/// Find the earliest successor time among reclaimable heads; its level anchors retained history.
fn evictionTime(capture: *const Capture, comptime Sample: type, channel: anytype) ?u128 {
    const head = channel.first_block;
    if (head == .none)
        return null;
    const next = capture.block(Sample, head).control.next();
    if (next == .none)
        return null;
    return capture.block(Sample, next).samples[0].timestamp;
}

/// Reclaim heads while preserving a sample at or before the shared retained start.
fn trim(capture: *Capture, comptime Sample: type, channel: anytype, cutoff: u128) void {
    while (capture.evictionTime(Sample, channel.*)) |time| {
        if (time > cutoff)
            break;
        const head = channel.first_block;
        channel.first_block = capture.block(Sample, head).control.next();
        capture.release(head);
    }
}

/// Reserve all new blocks before writing any channel in one simultaneous substep.
fn reserve(capture: *Capture, needed: usize) bool {
    if (capture.metadata().status != .recording)
        return false;
    if (capture.metadata().overflow == .stop and capture.metadata().free_count < needed) {
        capture.metadata().status = .full;
        return false;
    }
    while (capture.metadata().free_count < needed) {
        var cutoff: u128 = std.math.maxInt(u128);
        for (capture.digitalChannels().*) |channel| {
            cutoff = @min(cutoff, capture.evictionTime(DigitalSample, channel) orelse cutoff);
        }
        for (capture.analogChannels().*) |channel| {
            cutoff = @min(cutoff, capture.evictionTime(AnalogSample, channel) orelse cutoff);
        }
        cutoff = @min(cutoff, capture.evictionTime(ResetSample, capture.resetChannel().*) orelse cutoff);
        // The minimum budget has more slots than all tails plus one whole simultaneous sample.
        std.debug.assert(cutoff != std.math.maxInt(u128));
        capture.metadata().start = @max(capture.metadata().start, cutoff);
        for (capture.digitalChannels()) |*channel| {
            capture.trim(DigitalSample, channel, capture.metadata().start);
        }
        for (capture.analogChannels()) |*channel| {
            capture.trim(AnalogSample, channel, capture.metadata().start);
        }
        capture.trim(ResetSample, capture.resetChannel(), capture.metadata().start);
    }
    return true;
}

/// Materialize a pin's baseline on its first transition; publish successors only after writing their first sample.
fn append(capture: *Capture, comptime Sample: type, channel: anytype, sample: Sample) void {
    if (capture.needsBlock(channel.*)) {
        const id = capture.allocate();
        const fresh = capture.block(Sample, id);
        const previous = channel.latest_block;
        const baseline = Sample != ResetSample and previous == .none;
        if (baseline) {
            fresh.samples[0] = .{ .timestamp = 0, .value = channel.last_value };
        }
        fresh.samples[@intFromBool(baseline)] = sample;
        fresh.control = .{ .payload = 1 + @as(u31, @intFromBool(baseline)), .is_next = false };
        if (previous != .none) {
            capture.block(Sample, previous).control = .{ .payload = @intCast(@intFromEnum(id)), .is_next = true };
        } else {
            channel.first_block = id;
        }
        channel.latest_block = id;
    } else {
        const tail = capture.block(Sample, channel.latest_block);
        const count = tail.control.count();
        tail.samples[count] = sample;
        tail.control = .{ .payload = @intCast(count + 1), .is_next = false };
    }
    channel.last_value = sample.value;
}

/// Record a digital source in a pool-owned channel, independently of electrical pad sampling.
pub fn recordDigital(capture: *Capture, channel: *Channel(Level), timestamp: u127, value: Level) bool {
    if (capture.metadata().status != .recording)
        return false;
    if (channel.last_value == value) {
        capture.extend(timestamp);
        return true;
    }
    if (!capture.reserve(@intFromBool(capture.needsBlock(channel.*))))
        return false;
    capture.append(DigitalSample, channel, .{ .timestamp = timestamp, .value = value });
    capture.extend(timestamp);
    return true;
}

/// Record an analog source in a pool-owned channel; callers supply the quantized level.
pub fn recordAnalog(capture: *Capture, channel: *Channel(u16), timestamp: u112, value: u16) bool {
    if (capture.metadata().status != .recording)
        return false;
    if (channel.last_value == value) {
        capture.extend(timestamp);
        return true;
    }
    if (!capture.reserve(@intFromBool(capture.needsBlock(channel.*))))
        return false;
    capture.append(AnalogSample, channel, .{ .timestamp = timestamp, .value = value });
    capture.extend(timestamp);
    return true;
}

/// Hold unchanged sources through observation time without reading sample blocks or allocating.
fn extend(capture: *Capture, timestamp: u128) void {
    capture.metadata().end = @max(capture.metadata().end, timestamp);
    if (timestamp > capture.metadata().time.femtoseconds) {
        capture.metadata().time = .{ .femtoseconds = timestamp };
    }
}

/// Retain the smallest positive electrical substep in Q64 femtoseconds across the entire recording.
pub fn noteSubstep(capture: *Capture, interval: u128) void {
    const header = capture.metadata();
    if (header.status != .recording or interval == 0)
        return;
    if (header.minimum_substep == 0 or interval < header.minimum_substep) {
        header.minimum_substep = interval;
    }
}

test "minimum substep survives reset and freeze without following later slower intervals" {
    var capture = try Capture.init(std.testing.allocator, minimum_memory, .rolling, test_initial);
    defer capture.deinit();

    try std.testing.expectEqual(@as(u128, 0), capture.metadataConst().minimum_substep);
    capture.noteSubstep(0);
    capture.noteSubstep(@as(u128, 10) << 64);
    const smallest = (@as(u128, 3) << 64) + 7;
    capture.noteSubstep(smallest);
    capture.noteSubstep(@as(u128, 20) << 64);
    capture.reset(.reinitialize);
    try std.testing.expectEqual(smallest, capture.metadataConst().minimum_substep);
    capture.metadata().status = .frozen;
    capture.noteSubstep(1);
    try std.testing.expectEqual(smallest, capture.metadataConst().minimum_substep);
}

/// Commit all pin observations atomically; unchanged levels skip sample-block reads.
pub fn observe(capture: *Capture, digital: [64]Level, analog: [64]u16) void {
    if (capture.metadata().status != .recording)
        return;
    var needed: usize = 0;
    for (capture.digitalChannels().*, digital) |channel, value| {
        if (channel.last_value != value) {
            needed += @intFromBool(capture.needsBlock(channel));
        }
    }
    for (capture.analogChannels().*, analog) |channel, value| {
        if (channel.last_value != value) {
            needed += @intFromBool(capture.needsBlock(channel));
        }
    }
    if (!capture.reserve(needed))
        return;
    const timestamp = capture.metadata().time.femtoseconds;
    for (capture.digitalChannels(), digital) |*channel, value| {
        if (channel.last_value != value) {
            capture.append(DigitalSample, channel, .{ .timestamp = @intCast(timestamp), .value = value });
        }
    }
    for (capture.analogChannels(), analog) |*channel, value| {
        if (channel.last_value != value) {
            capture.append(AnalogSample, channel, .{ .timestamp = @intCast(timestamp), .value = value });
        }
    }
    capture.metadata().end = timestamp;
}

/// Mark a reset on the persistent physical timeline without clearing the pin chains.
pub fn reset(capture: *Capture, kind: ResetKind) void {
    if (!capture.reserve(@intFromBool(capture.needsBlock(capture.resetChannel().*))))
        return;
    capture.append(ResetSample, capture.resetChannel(), .{ .timestamp = @intCast(capture.metadata().time.femtoseconds), .value = kind });
    capture.metadata().end = capture.metadata().time.femtoseconds;
}

/// Iterate a typed chain without ownership or allocation; channels must remain attached to this pool.
pub fn Iterator(comptime Sample: type) type {
    return struct {
        capture: *const Capture,
        current: BlockId,
        implicit: ?Sample,
        index: usize = 0,

        /// Yield a blockless pin's implicit baseline once, otherwise follow full-block links.
        pub fn next(cursor: *@This()) ?Sample {
            if (cursor.implicit) |sample| {
                cursor.implicit = null;
                return sample;
            }
            const id = cursor.current;
            if (id == .none)
                return null;
            const slot = cursor.capture.block(Sample, id);
            if (cursor.index == slot.control.count())
                return null;
            const result = slot.samples[cursor.index];
            cursor.index += 1;
            if (cursor.index == samples_per_block) {
                cursor.current = slot.control.next();
                cursor.index = 0;
            }
            return result;
        }

        /// Skip whole blocks while retaining the sample that determines the value at the viewport start.
        pub fn seek(cursor: *@This(), timestamp: u128) void {
            while (cursor.current != .none) {
                const id = cursor.current;
                const following = cursor.capture.block(Sample, id).control.next();
                if (following == .none)
                    break;
                if (cursor.capture.block(Sample, following).samples[0].timestamp >= timestamp)
                    break;
                cursor.current = following;
                cursor.index = 0;
            }
        }
    };
}

/// Obtain a read-only iterator for a channel, including its retained baseline sample.
pub fn iterator(capture: *const Capture, comptime Sample: type, channel: anytype) Iterator(Sample) {
    return .{
        .capture = capture,
        .current = channel.first_block,
        .implicit = if (Sample != ResetSample and channel.first_block == .none)
            .{ .timestamp = 0, .value = channel.last_value }
        else
            null,
    };
}

/// Save the self-contained native image verbatim, preserving every block ID and free-list link.
pub fn writeNative(capture: *const Capture, writer: *std.Io.Writer) !void {
    try writer.writeAll(std.mem.sliceAsBytes(capture.blocks));
    try writer.flush();
}

/// Read an exact-size native image into aligned storage and validate it before exposing typed samples.
pub fn readNative(allocator: std.mem.Allocator, reader: *std.Io.Reader, byte_count: u64) !Capture {
    if (builtin.cpu.arch.endian() != .little)
        return error.UnsupportedCaptureEndianness;
    if (byte_count < minimum_memory or byte_count % block_size != 0 or
        byte_count / block_size > std.math.maxInt(u31) or byte_count > std.math.maxInt(usize))
    {
        return error.InvalidCaptureSize;
    }
    var first: Storage = undefined;
    try reader.readSliceAll(std.mem.asBytes(&first));
    try validateHeader(&first, byte_count / block_size);
    var capture: Capture = .{
        .allocator = allocator,
        .blocks = try allocator.alloc(Storage, @intCast(byte_count / block_size)),
    };
    errdefer capture.deinit();

    capture.blocks[0] = first;
    try reader.readSliceAll(std.mem.sliceAsBytes(capture.blocks[1..]));
    try capture.validateImage();
    return capture;
}

/// Check raw enum representations before reading the header through its typed view.
fn validateHeader(first: *const Storage, count: u64) !void {
    const bytes = std.mem.asBytes(&first.descriptor.samples);
    if (!std.mem.eql(u8, bytes[0..8], "WTSCOPE\x00"))
        return error.InvalidCaptureMagic;
    if (std.mem.readInt(u32, bytes[8..12], .little) != 1)
        return error.UnsupportedCaptureVersion;
    if (bytes[@offsetOf(Header, "overflow")] > @intFromEnum(Overflow.stop) or
        bytes[@offsetOf(Header, "status")] > @intFromEnum(Status.full))
    {
        return error.InvalidCaptureHeader;
    }
    const header: *const Header = @ptrCast(&first.descriptor.samples);
    if (header.block_size != block_size or header.block_count != count or
        header.descriptor_blocks != descriptor_blocks or header.descriptor_bytes != descriptor_records * 16 or
        header.channel_count != channel_count or header.free_count > count - descriptor_blocks or
        header.start > header.end or header.end > header.time.femtoseconds or
        header.time.reserved != 0 or !std.mem.allEqual(u8, &header.reserved_flags, 0) or
        !std.mem.allEqual(u8, &header.reserved, 0))
    {
        return error.InvalidCaptureHeader;
    }
}

/// Claim each block once across descriptors, channels and the free list, rejecting cycles and aliases.
fn claimBlock(capture: *const Capture, seen: *std.DynamicBitSet, id: BlockId) !*const Block(u128) {
    const index = @intFromEnum(id);
    if (index >= capture.blocks.len or seen.isSet(index))
        return error.InvalidCaptureBlockChain;
    seen.set(index);
    const slot = &capture.blocks[index].descriptor;
    if (!std.mem.allEqual(u8, &slot.reserved, 0))
        return error.InvalidCaptureReservedBytes;
    return slot;
}

/// Validate the complete ownership graph, including unused capacity, without following unchecked indices.
fn validateImage(capture: *const Capture) !void {
    var seen = try std.DynamicBitSet.initEmpty(capture.allocator, capture.blocks.len);
    defer seen.deinit();

    for (0..descriptor_blocks) |index| {
        const slot = try capture.claimBlock(&seen, @enumFromInt(index));
        const remaining = descriptor_records - index * samples_per_block;
        const expected: CntOrBlockId = if (index + 1 < descriptor_blocks)
            .{ .payload = @intCast(index + 1), .is_next = true }
        else
            .{ .payload = @intCast(remaining), .is_next = false };
        if (@as(u32, @bitCast(slot.control)) != @as(u32, @bitCast(expected)))
            return error.InvalidCaptureDescriptor;
        const used = @min(remaining, samples_per_block);
        if (!std.mem.allEqual(u128, slot.samples[used..], 0))
            return error.InvalidCaptureReservedBytes;
    }
    // Read descriptors as raw records until reset enums and reserved bits have been checked.
    for (0..channel_count) |index| {
        const record = header_records + index;
        const raw = capture.blocks[record / samples_per_block].descriptor.samples[record % samples_per_block];
        if (index < 64) {
            try capture.validateChannel(DigitalSample, raw, &seen);
        } else if (index < 128) {
            try capture.validateChannel(AnalogSample, raw, &seen);
        } else {
            try capture.validateChannel(ResetSample, raw, &seen);
        }
    }
    var free = capture.metadataConst().free_head;
    var free_count: usize = 0;
    while (free != .none) {
        const slot = try capture.claimBlock(&seen, free);
        if (!slot.control.is_next and slot.control.payload != 0)
            return error.InvalidCaptureFreeList;
        free_count += 1;
        free = slot.control.next();
    }
    if (free_count != capture.metadataConst().free_count or seen.count() != capture.blocks.len)
        return error.InvalidCaptureFreeList;
}

/// Accept only the voltage codes and reset kinds emitted by the recorder.
fn validSampleValue(comptime Sample: type, value: u64) bool {
    return switch (Sample) {
        DigitalSample => value <= 1,
        AnalogSample => value <= 65001 or value == 65535,
        ResetSample => value <= @intFromEnum(ResetKind.reinitialize),
        else => @compileError("Unsupported capture sample type"),
    };
}

/// Check a raw channel descriptor and its samples before any potentially invalid enum is interpreted.
fn validateChannel(capture: *const Capture, comptime Sample: type, raw: u128, seen: *std.DynamicBitSet) !void {
    const value_bits = @bitSizeOf(@FieldType(Sample, "value"));
    const Timestamp = std.meta.Int(.unsigned, 128 - value_bits);
    const first: BlockId = @enumFromInt(@as(u32, @truncate(raw)));
    const latest: BlockId = @enumFromInt(@as(u32, @truncate(raw >> 32)));
    const cached: u64 = @intCast(raw >> 64);
    if (!validSampleValue(Sample, cached) or
        (first == .none and (latest != .none or (Sample == ResetSample and cached != 0))))
        return error.InvalidCaptureChannel;
    var current = first;
    var last: BlockId = .none;
    var timestamp: u128 = 0;
    while (current != .none) {
        const slot = try capture.claimBlock(seen, current);
        const count = slot.control.count();
        if (count == 0 or count > samples_per_block)
            return error.InvalidCaptureSampleCount;
        for (slot.samples[0..count]) |sample| {
            const time: Timestamp = @truncate(sample);
            const value: u64 = @intCast(sample >> @bitSizeOf(Timestamp));
            if (!validSampleValue(Sample, value))
                return error.InvalidCaptureSampleValue;
            if (time < timestamp or time > capture.metadataConst().end)
                return error.InvalidCaptureTimestamp;
            timestamp = time;
        }
        if (slot.control.next() == .none and cached != slot.samples[count - 1] >> @bitSizeOf(Timestamp))
            return error.InvalidCaptureChannel;
        last = current;
        current = slot.control.next();
    }
    if (last != latest)
        return error.InvalidCaptureChannel;
}

pub const ChannelStatistics = struct {
    blocks: usize = 0,
    samples: usize = 0,
    clipped: usize = 0,
};

/// Count stored samples, owned blocks, and analog samples with explicit low/high clipping codes.
pub fn channelStatistics(capture: *const Capture, comptime Sample: type, channel: anytype) ChannelStatistics {
    var result: ChannelStatistics = .{};
    var current = channel.first_block;
    while (current != .none) {
        const slot = capture.block(Sample, current);
        result.blocks += 1;
        result.samples += slot.control.count();
        if (Sample == AnalogSample) {
            for (slot.samples[0..slot.control.count()]) |sample| {
                result.clipped += @intFromBool(sample.value == 0 or sample.value == std.math.maxInt(u16));
            }
        }
        current = slot.control.next();
    }
    return result;
}

const test_initial: InitialState = .{ .digital = @splat(.low), .analog = @splat(20001) };

test "quiet descriptors retain nonzero baselines and first transitions materialize exactly two samples" {
    var initial: InitialState = .{ .digital = @splat(.high), .analog = @splat(voltage(1.2345)) };
    initial.analog[31] = 0;
    initial.analog[32] = 65535;
    var capture = try Capture.init(std.testing.allocator, minimum_memory, .stop, initial);
    defer capture.deinit();

    capture.observe(initial.digital, initial.analog);
    capture.metadata().time.femtoseconds = 10;
    capture.observe(initial.digital, initial.analog);
    try std.testing.expectEqual(descriptor_blocks * block_size, capture.memoryUsed());
    for (capture.digitalChannelsConst().*, capture.analogChannelsConst().*) |digital, analog| {
        try std.testing.expectEqual(BlockId.none, digital.first_block);
        try std.testing.expectEqual(BlockId.none, analog.first_block);
    }
    var quiet = capture.iterator(DigitalSample, capture.digitalChannelsConst()[1]);
    quiet.seek(10);
    try std.testing.expectEqual(DigitalSample{ .timestamp = 0, .value = .high }, quiet.next().?);
    try std.testing.expect(quiet.next() == null);
    try std.testing.expectEqual(@as(usize, 0), capture.channelStatistics(AnalogSample, capture.analogChannelsConst()[1]).samples);
    var resets = capture.iterator(ResetSample, capture.resetChannelConst().*);
    try std.testing.expect(resets.next() == null);

    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 30, .low));
    try std.testing.expect(capture.recordAnalog(&capture.analogChannels()[0], 30, voltage(3.3)));
    try std.testing.expectEqual((descriptor_blocks + 2) * block_size, capture.memoryUsed());
    try std.testing.expectEqual(@as(u32, 509), capture.metadataConst().free_count);
    const digital = capture.block(DigitalSample, capture.digitalChannelsConst()[0].first_block);
    try std.testing.expectEqual(@as(usize, 2), digital.control.count());
    try std.testing.expectEqual(DigitalSample{ .timestamp = 0, .value = .high }, digital.samples[0]);
    try std.testing.expectEqual(DigitalSample{ .timestamp = 30, .value = .low }, digital.samples[1]);
    const analog = capture.block(AnalogSample, capture.analogChannelsConst()[0].first_block);
    try std.testing.expectEqual(@as(usize, 2), analog.control.count());
    try std.testing.expectEqual(AnalogSample{ .timestamp = 0, .value = voltage(1.2345) }, analog.samples[0]);
    try std.testing.expectEqual(AnalogSample{ .timestamp = 30, .value = voltage(3.3) }, analog.samples[1]);
    try expectConsistent(&capture);

    var reader = std.Io.Reader.fixed(std.mem.sliceAsBytes(capture.blocks));
    var loaded = try readNative(std.testing.allocator, &reader, capture.memoryCapacity());
    defer loaded.deinit();
    var quiet_digital = loaded.iterator(DigitalSample, loaded.digitalChannelsConst()[1]);
    try std.testing.expectEqual(Level.high, quiet_digital.next().?.value);
    var quiet_analog = loaded.iterator(AnalogSample, loaded.analogChannelsConst()[1]);
    try std.testing.expectEqual(voltage(1.2345), quiet_analog.next().?.value);
    var low_clip = loaded.iterator(AnalogSample, loaded.analogChannelsConst()[31]);
    var high_clip = loaded.iterator(AnalogSample, loaded.analogChannelsConst()[32]);
    try std.testing.expectEqual(@as(u16, 0), low_clip.next().?.value);
    try std.testing.expectEqual(@as(u16, 65535), high_clip.next().?.value);
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(capture.blocks), std.mem.sliceAsBytes(loaded.blocks));
}

test "packed block chains, independent sources and native encoding" {
    var capture = try Capture.init(std.testing.allocator, minimum_memory, .stop, test_initial);
    defer capture.deinit();

    try std.testing.expectEqual(@as(u16, 1), voltage(-2));
    try std.testing.expectEqual(@as(u16, 65001), voltage(4.5));
    try std.testing.expectEqual(@as(u16, 0), voltage(-2.0001));
    try std.testing.expectEqual(@as(u16, 65535), voltage(4.5001));
    try std.testing.expectEqual(@as(u16, 20001), voltage(0));
    for (0..samples_per_block + 1) |i| {
        const level: Level = if (i % 2 == 0)
            .low
        else
            .high;
        try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], i, level));
    }
    const head = capture.block(DigitalSample, capture.digitalChannels()[0].first_block);
    try std.testing.expect(head.control.is_next);
    try std.testing.expectEqual(@as(usize, 255), head.control.count());
    const tail = capture.block(DigitalSample, capture.digitalChannels()[0].latest_block);
    try std.testing.expectEqual(@as(usize, 1), tail.control.count());
    var it = capture.iterator(DigitalSample, capture.digitalChannels()[0]);
    for (0..256) |i| {
        const sample = it.next().?;
        try std.testing.expectEqual(@as(u127, @intCast(i)), sample.timestamp);
        const expected: Level = if (i % 2 == 0)
            .low
        else
            .high;
        try std.testing.expectEqual(expected, sample.value);
    }
    try std.testing.expect(it.next() == null);
    var output = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output.deinit();

    try capture.writeNative(&output.writer);
    const bytes = output.written();
    try std.testing.expectEqualStrings("WTSCOPE\x00", bytes[16..24]);
    try std.testing.expectEqual(@as(u32, 1), std.mem.readInt(u32, bytes[24..28], .little));
    try std.testing.expectEqual(capture.memoryCapacity(), bytes.len);
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(capture.blocks), bytes);
    const linked = std.mem.readInt(u32, bytes[4096..4100], .little);
    try std.testing.expectEqual(@as(u32, 0x80000002), linked);
}

test "whole-substep full admission, rolling baselines and reclaimed blocks" {
    inline for (.{ Overflow.stop, Overflow.rolling }) |mode| {
        var capture = try Capture.init(std.testing.allocator, minimum_memory, mode, test_initial);
        defer capture.deinit();

        var admitted: usize = 0;
        for (0..1500) |step| {
            capture.metadata().time.femtoseconds = step;
            const digital: Level = if (step % 2 == 0)
                .low
            else
                .high;
            capture.observe(@splat(digital), @splat(@as(u16, @intCast(20_001 + step))));
            if (capture.metadata().status == .full)
                break;
            admitted += 1;
        }
        try expectConsistent(&capture);
        try std.testing.expect(capture.memoryUsed() <= minimum_memory);
        switch (mode) {
            .stop => {
                try std.testing.expectEqual(Status.full, capture.metadata().status);
                try std.testing.expect(admitted > 255 and admitted < 1500);
                try std.testing.expectEqual(@as(u128, admitted - 1), capture.metadata().end);
                for (capture.analogChannels().*, capture.digitalChannels().*) |analog, digital| {
                    try std.testing.expectEqual(@as(u16, @intCast(20_000 + admitted)), analog.last_value);
                    const expected: Level = if ((admitted - 1) % 2 == 0)
                        .low
                    else
                        .high;
                    try std.testing.expectEqual(expected, digital.last_value);
                }
            },
            .rolling => {
                try std.testing.expectEqual(Status.recording, capture.metadata().status);
                try std.testing.expectEqual(@as(u128, 1499), capture.metadata().end);
                try std.testing.expect(capture.metadata().start > 0);
                for (capture.digitalChannels().*, capture.analogChannels().*) |digital, analog| {
                    var di = capture.iterator(DigitalSample, digital);
                    var ai = capture.iterator(AnalogSample, analog);
                    try std.testing.expect(di.next().?.timestamp <= capture.metadata().start);
                    try std.testing.expect(ai.next().?.timestamp <= capture.metadata().start);
                    var last: ?u127 = null;
                    while (di.next()) |sample| {
                        if (last) |time| {
                            try std.testing.expect(sample.timestamp > time);
                        }
                        last = sample.timestamp;
                    }
                    try std.testing.expectEqual(@as(u127, 1499), last.?);
                }
            },
        }
    }
}

test "constant channels keep their baseline while a busy channel rolls" {
    var capture = try Capture.init(std.testing.allocator, minimum_memory, .rolling, test_initial);
    defer capture.deinit();

    capture.observe(@splat(.low), @splat(voltage(0)));
    for (0..capture.blocks.len * samples_per_block * 2) |step| {
        capture.metadata().time.femtoseconds = step + 1;
        var levels: [64]Level = @splat(.low);
        levels[0] = if (step % 2 == 0)
            .high
        else
            .low;
        capture.observe(levels, @splat(voltage(0)));
    }
    try std.testing.expect(capture.metadata().start > 0);
    try std.testing.expectEqual(Status.recording, capture.metadata().status);
    var idle = capture.iterator(DigitalSample, capture.digitalChannels()[63]);
    try std.testing.expectEqual(@as(u127, 0), idle.next().?.timestamp);
    try std.testing.expect(idle.next() == null);
    try std.testing.expectEqual(BlockId.none, capture.digitalChannelsConst()[63].first_block);
    try std.testing.expectEqual(BlockId.none, capture.analogChannelsConst()[63].first_block);
    try std.testing.expect(capture.digitalChannels()[0].first_block != capture.digitalChannels()[0].latest_block);
    const event_time = capture.metadataConst().end + 1;
    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[63], @intCast(event_time), .high));
    var late = capture.iterator(DigitalSample, capture.digitalChannelsConst()[63]);
    try std.testing.expectEqual(DigitalSample{ .timestamp = 0, .value = .low }, late.next().?);
    try std.testing.expectEqual(DigitalSample{ .timestamp = @intCast(event_time), .value = .high }, late.next().?);
    try std.testing.expect(late.next() == null);
    try expectConsistent(&capture);
}

/// Audit every owned slot across all chains and the free list; used only by regression tests.
fn expectConsistent(capture: *const Capture) !void {
    var seen = try std.DynamicBitSet.initEmpty(std.testing.allocator, capture.blocks.len);
    defer seen.deinit();

    var descriptor_id: BlockId = @enumFromInt(0);
    var records: usize = 0;
    var prefix_blocks: usize = 0;
    while (descriptor_id != .none) {
        const index = @intFromEnum(descriptor_id);
        try std.testing.expect(index < descriptor_blocks);
        try std.testing.expect(!seen.isSet(index));
        seen.set(index);
        const slot = &capture.blocks[index].descriptor;
        const count = slot.control.count();
        try std.testing.expect(count > 0 and count <= samples_per_block);
        records += count;
        prefix_blocks += 1;
        descriptor_id = slot.control.next();
    }
    try std.testing.expectEqual(descriptor_records, records);
    try std.testing.expectEqual(descriptor_blocks, prefix_blocks);
    try std.testing.expectEqual(@as(u32, @intCast(capture.blocks.len)), capture.metadataConst().block_count);
    try std.testing.expectEqual(@as(u32, descriptor_blocks), capture.metadataConst().descriptor_blocks);
    inline for (.{ true, false }) |digital| {
        const Sample = if (digital)
            DigitalSample
        else
            AnalogSample;
        const channels = if (digital)
            capture.digitalChannelsConst().*
        else
            capture.analogChannelsConst().*;
        for (channels) |channel| {
            var current = channel.first_block;
            var last: BlockId = .none;
            var time: u128 = 0;
            while (current != .none) {
                const id = current;
                const index = @intFromEnum(id);
                try std.testing.expect(!seen.isSet(index));
                seen.set(index);
                const slot = capture.block(Sample, id);
                const count = slot.control.count();
                try std.testing.expect(count > 0 and count <= samples_per_block);
                for (slot.samples[0..count]) |sample| {
                    try std.testing.expect(sample.timestamp >= time);
                    time = sample.timestamp;
                }
                if (slot.control.next() == .none) {
                    try std.testing.expectEqual(channel.last_value, slot.samples[count - 1].value);
                }
                last = id;
                current = slot.control.next();
            }
            try std.testing.expectEqual(channel.latest_block, last);
        }
    }
    var resets = capture.resetChannelConst().first_block;
    while (resets != .none) {
        const id = resets;
        try std.testing.expect(!seen.isSet(@intFromEnum(id)));
        seen.set(@intFromEnum(id));
        resets = capture.block(ResetSample, id).control.next();
    }
    var free = capture.metadataConst().free_head;
    var free_count: usize = 0;
    while (free != .none) {
        const id = free;
        try std.testing.expect(!seen.isSet(@intFromEnum(id)));
        seen.set(@intFromEnum(id));
        free_count += 1;
        free = capture.block(DigitalSample, id).control.next();
    }
    try std.testing.expectEqual(capture.metadataConst().free_count, free_count);
    try std.testing.expectEqual(capture.blocks.len, seen.count());
}

test "packed field maxima and reset-marker rolling obey the same pool invariants" {
    const digital: DigitalSample = .{ .timestamp = std.math.maxInt(u127), .value = .high };
    const analog: AnalogSample = .{ .timestamp = std.math.maxInt(u112), .value = 65535 };
    try std.testing.expectEqual(std.math.maxInt(u128), @as(u128, @bitCast(digital)));
    try std.testing.expectEqual(std.math.maxInt(u128), @as(u128, @bitCast(analog)));
    var capture = try Capture.init(std.testing.allocator, minimum_memory, .rolling, test_initial);
    defer capture.deinit();

    capture.observe(@splat(.low), @splat(voltage(0)));
    for (0..capture.blocks.len * samples_per_block * 2) |step| {
        capture.metadata().time.femtoseconds = step;
        capture.reset(.firmware);
    }
    try expectConsistent(&capture);
    try std.testing.expect(capture.metadata().start > 0);
}

test "unchanged independent observations extend time and preserve coincident changes at the origin" {
    var capture = try Capture.init(std.testing.allocator, minimum_memory, .stop, test_initial);
    defer capture.deinit();

    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 0, .low));
    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 0, .high));
    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 0, .low));
    const before = capture.memoryUsed();
    try std.testing.expect(capture.recordDigital(&capture.digitalChannels()[0], 100, .low));
    try std.testing.expectEqual(before, capture.memoryUsed());
    try std.testing.expectEqual(@as(u128, 100), capture.metadata().end);
    try std.testing.expectEqual(@as(u128, 100), capture.metadata().time.femtoseconds);
    var samples = capture.iterator(DigitalSample, capture.digitalChannelsConst()[0]);
    for ([_]Level{ .low, .high, .low }) |level| {
        const sample = samples.next().?;
        try std.testing.expectEqual(@as(u127, 0), sample.timestamp);
        try std.testing.expectEqual(level, sample.value);
    }
    try std.testing.expect(samples.next() == null);
}

test "initialized image has fixed layout, deterministic padding and descriptor-only usage" {
    var first = try Capture.init(std.testing.allocator, minimum_memory + 17, .stop, test_initial);
    defer first.deinit();
    var second = try Capture.init(std.testing.allocator, minimum_memory, .stop, test_initial);
    defer second.deinit();

    try std.testing.expect(first.blocks.ptr != second.blocks.ptr);
    try std.testing.expectEqual(@as(usize, 0), @intFromPtr(first.blocks.ptr) % block_size);
    try std.testing.expectEqual(@as(usize, 0), @intFromPtr(second.blocks.ptr) % block_size);
    try std.testing.expectEqual(@as(usize, 2), @typeInfo(Capture).@"struct".fields.len);
    for (first.blocks) |*slot| {
        try std.testing.expectEqual(@as(usize, 0), @intFromPtr(slot) % block_size);
    }
    try std.testing.expectEqual(descriptor_records, first.blocks[0].descriptor.control.count());
    try std.testing.expectEqual(BlockId.none, first.blocks[0].descriptor.control.next());
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(first.blocks), std.mem.sliceAsBytes(second.blocks));
    try std.testing.expectEqual(@as(usize, minimum_memory), first.memoryCapacity());
    try std.testing.expectEqual(descriptor_blocks * 4096, first.memoryUsed());
    const descriptor = first.metadataConst();
    try std.testing.expectEqual(@as(u32, 512), descriptor.block_count);
    try std.testing.expectEqual(@as(u32, 1), descriptor.descriptor_blocks);
    try std.testing.expectEqual(@as(u32, 2320), descriptor.descriptor_bytes);
    try std.testing.expectEqual(@as(u32, 129), descriptor.channel_count);
    try std.testing.expectEqual(@as(u32, 511), descriptor.free_count);
    try std.testing.expectEqual(BlockId.none, first.digitalChannelsConst()[0].first_block);
    try std.testing.expectEqual(Level.low, first.digitalChannelsConst()[0].last_value);
    try std.testing.expectEqual(voltage(0), first.analogChannelsConst()[0].last_value);
    try std.testing.expectEqual(ResetKind.firmware, first.resetChannelConst().last_value);
    const bytes = std.mem.sliceAsBytes(first.blocks);
    try std.testing.expect(std.mem.allEqual(u8, bytes[16 + descriptor_records * 16 .. block_size], 0));
    for (first.blocks[descriptor_blocks..]) |slot| {
        try std.testing.expect(std.mem.allEqual(u8, &slot.digital.reserved, 0));
        try std.testing.expect(std.mem.allEqual(u8, std.mem.asBytes(&slot.digital.samples), 0));
    }
    try expectConsistent(&first);
}

test "relocated raw block image preserves traces, fractional time and future rolling allocation" {
    var original = try Capture.init(std.testing.allocator, minimum_memory, .rolling, test_initial);
    defer original.deinit();

    original.noteSubstep((@as(u128, 25) << 64) + 7);
    original.metadata().time.fraction = std.math.maxInt(u64) - 10;
    for (0..1500) |step| {
        original.metadata().time.femtoseconds = step * 1000;
        const digital: Level = if (step % 2 == 0)
            .low
        else
            .high;
        original.observe(@splat(digital), @splat(@intCast(20_001 + step)));
    }
    original.metadata().time.add(30);
    original.reset(.firmware);
    try std.testing.expect(original.metadataConst().start > 0);
    try std.testing.expectEqual(@as(u64, 19), original.metadataConst().time.fraction);
    var reader = std.Io.Reader.fixed(std.mem.sliceAsBytes(original.blocks));
    var relocated = try readNative(std.testing.allocator, &reader, original.memoryCapacity());
    defer relocated.deinit();

    try std.testing.expectEqual(original.metadataConst().minimum_substep, relocated.metadataConst().minimum_substep);

    try std.testing.expect(original.blocks.ptr != relocated.blocks.ptr);
    try std.testing.expect(original.metadata() != relocated.metadata());
    try std.testing.expectEqualDeep(original.metadataConst().*, relocated.metadataConst().*);
    try expectConsistent(&relocated);
    for (1500..2000) |step| {
        const value: Level = if (step % 2 == 0)
            .low
        else
            .high;
        original.metadata().time.femtoseconds = step * 1000;
        relocated.metadata().time.femtoseconds = step * 1000;
        original.observe(@splat(value), @splat(@intCast(20_001 + step)));
        relocated.observe(@splat(value), @splat(@intCast(20_001 + step)));
        if (step % 37 == 0) {
            original.reset(.reinitialize);
            relocated.reset(.reinitialize);
        }
    }
    try expectConsistent(&original);
    try expectConsistent(&relocated);
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(original.blocks), std.mem.sliceAsBytes(relocated.blocks));
    var native = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer native.deinit();

    try relocated.writeNative(&native.writer);
    try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(original.blocks), native.written());
}

test "native loading preserves empty and coincident clipped/reset traces" {
    var original = try Capture.init(std.testing.allocator, minimum_memory, .stop, test_initial);
    defer original.deinit();

    for (0..2) |populated| {
        if (populated != 0) {
            original.observe(@splat(.low), @splat(voltage(0)));
            try std.testing.expect(original.recordDigital(&original.digitalChannels()[0], 0, .high));
            try std.testing.expect(original.recordDigital(&original.digitalChannels()[0], 0, .low));
            try std.testing.expect(original.recordAnalog(&original.analogChannels()[0], 0, 0));
            try std.testing.expect(original.recordAnalog(&original.analogChannels()[0], 0, 65535));
            original.reset(.firmware);
            original.reset(.reinitialize);
            original.metadata().time.fraction = 123;
            original.metadata().status = .frozen;
        }
        var reader = std.Io.Reader.fixed(std.mem.sliceAsBytes(original.blocks));
        var loaded = try readNative(std.testing.allocator, &reader, original.memoryCapacity());
        defer loaded.deinit();

        try std.testing.expectEqual(@as(usize, 0), @intFromPtr(loaded.blocks.ptr) % block_size);
        try std.testing.expectEqualSlices(u8, std.mem.sliceAsBytes(original.blocks), std.mem.sliceAsBytes(loaded.blocks));
        var digital = loaded.iterator(DigitalSample, loaded.digitalChannelsConst()[0]);
        try std.testing.expectEqual(DigitalSample{ .timestamp = 0, .value = .low }, digital.next().?);
        if (populated == 0) {
            try std.testing.expect(digital.next() == null);
        } else {
            try std.testing.expectEqual(DigitalSample{ .timestamp = 0, .value = .high }, digital.next().?);
            try std.testing.expectEqual(DigitalSample{ .timestamp = 0, .value = .low }, digital.next().?);
            try std.testing.expect(digital.next() == null);
        }
    }
}

test "native loader rejects corrupt headers, ownership graphs, descriptors and samples" {
    var original = try Capture.init(std.testing.allocator, minimum_memory, .stop, test_initial);
    defer original.deinit();

    original.metadata().time.femtoseconds = 5;
    original.observe(@splat(.low), @splat(voltage(0)));
    try std.testing.expect(original.recordDigital(&original.digitalChannels()[0], 10, .high));
    try std.testing.expect(original.recordAnalog(&original.analogChannels()[0], 10, 65535));
    original.reset(.firmware);
    const digital = @intFromEnum(original.digitalChannels()[0].first_block) * block_size;
    const analog = @intFromEnum(original.analogChannels()[0].first_block) * block_size;
    const reset_block = @intFromEnum(original.resetChannel().first_block) * block_size;
    const free = @intFromEnum(original.metadata().free_head) * block_size;
    const digital_descriptor = 16 + header_records * 16;
    const analog_descriptor = digital_descriptor + 64 * 16;
    const reset_descriptor = analog_descriptor + 64 * 16;
    const Corruption = struct { offset: usize, size: usize, value: u128, expected: anyerror };
    const cases = [_]Corruption{
        .{ .offset = 16, .size = 1, .value = 0, .expected = error.InvalidCaptureMagic },
        .{ .offset = 24, .size = 4, .value = 2, .expected = error.UnsupportedCaptureVersion },
        .{ .offset = 28, .size = 4, .value = 2048, .expected = error.InvalidCaptureHeader },
        .{ .offset = 32, .size = 4, .value = 511, .expected = error.InvalidCaptureHeader },
        .{ .offset = 36, .size = 4, .value = 2, .expected = error.InvalidCaptureHeader },
        .{ .offset = 40, .size = 4, .value = 1, .expected = error.InvalidCaptureHeader },
        .{ .offset = 44, .size = 4, .value = 128, .expected = error.InvalidCaptureHeader },
        .{ .offset = 56, .size = 1, .value = 255, .expected = error.InvalidCaptureHeader },
        .{ .offset = 57, .size = 1, .value = 255, .expected = error.InvalidCaptureHeader },
        .{ .offset = 58, .size = 1, .value = 1, .expected = error.InvalidCaptureHeader },
        .{ .offset = 88, .size = 1, .value = 1, .expected = error.InvalidCaptureHeader },
        .{ .offset = 96, .size = 16, .value = 11, .expected = error.InvalidCaptureHeader },
        .{ .offset = 112, .size = 16, .value = 11, .expected = error.InvalidCaptureHeader },
        .{ .offset = 144, .size = 1, .value = 1, .expected = error.InvalidCaptureHeader },
        .{ .offset = 0, .size = 4, .value = 0x80000000, .expected = error.InvalidCaptureDescriptor },
        .{ .offset = 4, .size = 1, .value = 1, .expected = error.InvalidCaptureReservedBytes },
        .{ .offset = 16 + descriptor_records * 16, .size = 1, .value = 1, .expected = error.InvalidCaptureReservedBytes },
        .{ .offset = digital_descriptor, .size = 4, .value = 512, .expected = error.InvalidCaptureBlockChain },
        .{ .offset = digital_descriptor, .size = 4, .value = 0, .expected = error.InvalidCaptureBlockChain },
        .{ .offset = digital_descriptor + 4, .size = 4, .value = 0xffffffff, .expected = error.InvalidCaptureChannel },
        .{ .offset = digital_descriptor + 8, .size = 1, .value = 0, .expected = error.InvalidCaptureChannel },
        .{ .offset = digital_descriptor + 9, .size = 1, .value = 1, .expected = error.InvalidCaptureChannel },
        .{ .offset = digital_descriptor + 16, .size = 4, .value = digital / block_size, .expected = error.InvalidCaptureBlockChain },
        .{ .offset = analog_descriptor + 8, .size = 2, .value = 65002, .expected = error.InvalidCaptureChannel },
        .{ .offset = reset_descriptor + 8, .size = 2, .value = 2, .expected = error.InvalidCaptureChannel },
        .{ .offset = digital, .size = 4, .value = 256, .expected = error.InvalidCaptureSampleCount },
        .{ .offset = digital, .size = 4, .value = 0, .expected = error.InvalidCaptureSampleCount },
        .{ .offset = digital + 4, .size = 1, .value = 1, .expected = error.InvalidCaptureReservedBytes },
        .{ .offset = digital + 16, .size = 16, .value = 11, .expected = error.InvalidCaptureTimestamp },
        .{ .offset = digital + 32, .size = 16, .value = 0, .expected = error.InvalidCaptureChannel },
        .{ .offset = digital + 32, .size = 16, .value = (@as(u128, 1) << 127) | 11, .expected = error.InvalidCaptureTimestamp },
        .{ .offset = analog + 16, .size = 16, .value = @as(u128, 65002) << 112, .expected = error.InvalidCaptureSampleValue },
        .{ .offset = reset_block + 16, .size = 16, .value = @as(u128, 2) << 112, .expected = error.InvalidCaptureSampleValue },
        .{ .offset = 48, .size = 4, .value = 0xffffffff, .expected = error.InvalidCaptureFreeList },
        .{ .offset = free, .size = 4, .value = 0x80000000 | (free / block_size), .expected = error.InvalidCaptureBlockChain },
        .{ .offset = free, .size = 4, .value = 0, .expected = error.InvalidCaptureFreeList },
        .{ .offset = free, .size = 4, .value = 1, .expected = error.InvalidCaptureFreeList },
    };
    const bytes = try std.testing.allocator.dupe(u8, std.mem.sliceAsBytes(original.blocks));
    defer std.testing.allocator.free(bytes);

    for (cases) |case| {
        @memcpy(bytes, std.mem.sliceAsBytes(original.blocks));
        const encoded = std.mem.toBytes(std.mem.nativeToLittle(u128, case.value));
        @memcpy(bytes[case.offset..][0..case.size], encoded[0..case.size]);
        var reader = std.Io.Reader.fixed(bytes);
        try std.testing.expectError(case.expected, readNative(std.testing.allocator, &reader, bytes.len));
    }
    @memcpy(bytes, std.mem.sliceAsBytes(original.blocks));
    std.mem.writeInt(u128, bytes[digital + 16 ..][0..16], 5, .little);
    std.mem.writeInt(u128, bytes[digital + 32 ..][0..16], @as(u128, 1) << 127, .little);
    var backwards = std.Io.Reader.fixed(bytes);
    try std.testing.expectError(error.InvalidCaptureTimestamp, readNative(std.testing.allocator, &backwards, bytes.len));
    for ([_]u64{ 0, 1, block_size - 1, block_size, bytes.len - 1, (@as(u64, 1) << 31) * block_size }) |size| {
        var reader = std.Io.Reader.fixed(bytes);
        try std.testing.expectError(error.InvalidCaptureSize, readNative(std.testing.allocator, &reader, size));
    }
    var truncated = std.Io.Reader.fixed(bytes[0 .. bytes.len - 1]);
    try std.testing.expectError(error.EndOfStream, readNative(std.testing.allocator, &truncated, bytes.len));
}
