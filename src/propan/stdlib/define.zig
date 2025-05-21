const std = @import("std");
const eval = @import("eval.zig");
const sema = @import("../sema.zig");

const Value = eval.Value;
const Parameter = sema.Function.Parameter;

pub const EvalError = sema.FunctionCallError;

pub const Context = sema.FunctionCallContext;

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
        invoke_info.params[0].type == Context
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
        fn invoke(ctx: Context, args: []const eval.Value) EvalError!eval.Value {
            if (args.len != invoke_params.len)
                return error.InvalidArgCount;
            const args_fixed = args[0..invoke_params.len];

            var mapped_args: std.meta.ArgsTuple(InvokeType) = undefined;

            inline for (&mapped_args, args_fixed) |*dst, arg| {
                dst.* = undefined;
                _ = arg;
            }

            const result = if (has_ctx)
                @call(.auto, T.invoke, &.{ctx} ++ mapped_args)
            else
                @call(.auto, T.invoke, mapped_args);

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

        else => {},
    }

    const info = @typeInfo(T);

    switch (info) {
        .int => return .int(std.math.cast(i64, v) orelse return error.Overflow),
        .@"enum" => return .enumerator(@tagName(v)),
        .bool => return if (v) // TODO: Change this to #on, #off?
            Value.int(1)
        else
            Value.int(0),

        else => @compileError("Unsupported return type " ++ @typeName(T)),
    }
}
