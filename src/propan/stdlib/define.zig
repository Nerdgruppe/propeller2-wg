const std = @import("std");
const eval = @import("eval.zig");
const sema = @import("../sema.zig");

const Value = eval.Value;
const Parameter = sema.Function.Parameter;

const PTRA: eval.Register = @enumFromInt(0x1F8);
const PTRB: eval.Register = @enumFromInt(0x1F9);

pub const EvalError = sema.FunctionCallError;

pub const EvalContext = sema.FunctionCallContext;

pub const Function = sema.UserFunction;

pub fn function(comptime T: type) Function {
    std.debug.assert(@hasDecl(T, "docs"));
    std.debug.assert(@hasDecl(T, "params"));
    std.debug.assert(@hasDecl(T, "invoke"));

    var params: []const Parameter = &.{};

    const pdecls = @typeInfo(@TypeOf(T.params)).@"struct".fields;

    const InvokeType = @TypeOf(T.invoke);
    const invoke_info = @typeInfo(InvokeType).@"fn";

    const has_ctx = if (invoke_info.params.len > 0)
        invoke_info.params[0].type == EvalContext
    else
        false;

    const invoke_params = if (has_ctx)
        invoke_info.params[1..]
    else
        invoke_info.params;

    if (pdecls.len != invoke_params.len)
        @compileError("parameter mismatch!");

    for (pdecls, 0..) |fld, i| {
        const name = fld.name;
        const value = @field(T.params, name);
        const Param = @TypeOf(value);

        const ptype = invoke_params[i];

        var default: ?Value = null;
        if (@hasField(Param, "default")) {
            default = convert_to_value(value.default) catch @compileError("unsupported default value");
        }

        const param: sema.Function.Parameter = .{
            .name = name,
            .type = derive_value_type(ptype.type.?),
            .docs = value.docs,
            .default_value = default,
        };

        params = params ++ &[_]sema.Function.Parameter{param};
    }

    const Wrap = struct {
        fn ArgsTuple(comptime Func: type) type {
            const info = @typeInfo(Func);
            if (info != .@"fn")
                @compileError("ArgsTuple expects a function type");

            const function_info = info.@"fn";
            if (function_info.is_var_args)
                @compileError("Cannot create ArgsTuple for variadic function");

            const delta = if (has_ctx) 1 else 0;

            var argument_field_list: [function_info.params.len - delta]type = undefined;
            inline for (function_info.params[delta..], 0..) |arg, i| {
                const Arg = arg.type orelse @compileError("cannot create ArgsTuple for function with an 'anytype' parameter");
                argument_field_list[i] = Arg;
            }

            return std.meta.Tuple(&argument_field_list);
        }

        fn invoke(ctx: EvalContext, args: []const eval.Value) EvalError!eval.Value {
            if (args.len != invoke_params.len)
                return error.InvalidArgCount;
            const args_fixed = args[0..invoke_params.len];

            var mapped_args: ArgsTuple(InvokeType) = undefined;

            inline for (&mapped_args, args_fixed, 0..) |*dst, arg, index| {
                const param = @field(T.params, pdecls[index].name);
                const Param = @TypeOf(param);

                dst.* = try convert_to_type(@TypeOf(dst.*), arg);

                if (@hasField(Param, "min")) {
                    if (dst.* < param.min)
                        return error.InvalidArg;
                }
                if (@hasField(Param, "max")) {
                    if (dst.* > param.max)
                        return error.InvalidArg;
                }
            }

            const wrapped_result = if (has_ctx)
                @call(.auto, T.invoke, .{ctx} ++ mapped_args)
            else
                @call(.auto, T.invoke, mapped_args);

            const info = @typeInfo(@TypeOf(wrapped_result));
            const result = if (info == .error_union)
                try wrapped_result
            else
                wrapped_result;
            return convert_to_value(result);
        }
    };

    return Function{
        .docs = T.docs,
        .params = params,
        .invoke = Wrap.invoke,
    };
}

pub const Namespace = std.StaticStringMap(Function);

pub fn namespace(comptime ns: anytype) Namespace {
    const Container = @TypeOf(ns);
    const info = @typeInfo(Container).@"struct";

    const KV = struct {
        []const u8,
        Function,
    };

    var decls: []const KV = &.{};
    for (info.fields) |fld| {
        const item = @field(ns, fld.name);
        switch (@TypeOf(item)) {
            Function => decls = decls ++ &[1]KV{
                .{ fld.name, item },
            },

            Namespace => for (item.keys(), item.values()) |k, v| {
                decls = decls ++ &[1]KV{
                    .{ fld.name ++ "." ++ k, v },
                };
            },

            else => @compileError(@typeName(@TypeOf(item))),
        }
    }

    return .initComptime(decls);
}

fn derive_value_type(comptime T: type) Parameter.Type {
    switch (T) {
        []const u8 => return .string,
        eval.TaggedAddress => return .address,
        eval.Register => return .register,
        eval.PointerExpression => return .pointer_expr,
        eval.Value => return .any,
        else => {},
    }

    const info = @typeInfo(T);
    switch (info) {
        .int, .bool => return .int,
        .@"enum" => return .enumerator,
        else => @compileError("unsupported parameter type " ++ @typeName(T)),
    }
}

fn convert_to_value(v: anytype) EvalError!Value {
    const T = @TypeOf(v);

    switch (T) {
        Value => return v,
        eval.Register => return .{
            .value = .{ .register = v },
            .flags = .{ .usage = .register },
        },

        else => {},
    }

    const info = @typeInfo(T);

    switch (info) {
        .comptime_int, .int => return .int(std.math.cast(i64, v) orelse return error.Overflow),
        .@"enum", .enum_literal => return .enumerator(@tagName(v)),
        .bool => return if (v) // TODO: Change this to #on, #off?
            Value.int(1)
        else
            Value.int(0),

        else => @compileError("Unsupported return type " ++ @typeName(T)),
    }
}

fn convert_to_type(comptime T: type, value: Value) EvalError!T {
    const info = @typeInfo(T);

    if (T == Value)
        return value;

    switch (T) {
        eval.TaggedAddress => switch (value.value) {
            .address => |addr| return addr,
            else => return error.TypeMismatch,
        },

        eval.PointerExpression => switch (value.value) {
            .pointer_expr => |expr| return expr,
            .register => |reg| switch (reg) {
                PTRA => .{ .pointer = .PTRA, .inremenet = .none, .index = null },
                PTRA => .{ .pointer = .PTRB, .inremenet = .none, .index = null },
                else => return error.InvalidArg,
            },
            else => return error.TypeMismatch,
        },

        eval.Register => switch (value.value) {
            .register => |reg| return reg,
            .address => |addr| switch (addr.local) {
                .cog => |reg| return @enumFromInt(reg),
                else => return error.TypeMismatch,
            },
            else => return error.TypeMismatch,
        },

        []const u8 => switch (value.value) {
            .string => |str| return str,
            else => return error.TypeMismatch,
        },

        else => {},
    }

    switch (info) {
        .int => switch (value.value) {
            .int => |val| return std.math.cast(T, val) orelse error.Overflow,
            else => return error.TypeMismatch,
        },

        .float => switch (value.value) {
            .int => |val| return @floatFromInt(val),
            else => return error.TypeMismatch,
        },

        .bool => switch (value.value) {
            .int => |val| return (val != 0),
            .enumerator => |en| return try enum_to_bool(en),
            else => return error.TypeMismatch,
        },

        .@"enum" => switch (value.value) {
            .enumerator => |en| return std.meta.stringToEnum(T, en) orelse return error.InvalidArg,
            else => return error.TypeMismatch,
        },

        .@"struct", // TODO: Should a int => packed struct cast be allowed?
        .void,
        .type,
        .noreturn,
        .pointer,
        .array,
        .null,
        .optional,
        .frame,
        .@"anyframe",
        .vector,
        .undefined,
        .@"union",
        .@"fn",
        .@"opaque",
        .enum_literal,
        .error_set,
        .error_union,
        .comptime_int,
        .comptime_float,
        => @compileError("Unsupported parameter type: " ++ @typeName(T)),
    }
}

const bool_lut: std.StaticStringMap(bool) = .initComptime(.{
    .{ "true", true }, .{ "false ", false },
    .{ "yes", true },  .{ "no", false },
    .{ "on", true },   .{ "off", false },
    .{ "1", true },    .{ "0", false },
});

fn enum_to_bool(str: []const u8) error{InvalidArg}!bool {
    return bool_lut.get(str) orelse return error.InvalidArg;
}
