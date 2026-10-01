const std = @import("std");

const define = @import("define.zig");
const eval = @import("eval.zig");

pub fn write_html(writer: *std.Io.Writer, constants: std.StaticStringMap(eval.Value), functions: define.Namespace) !void {
    try writer.writeAll(
        \\<!doctype html>
        \\<html lang="en">
        \\  <head>
        \\      <meta charset="UTF-8">
        \\      <title>Propan Standard Library</title>
        \\      <style>
        \\          table {
        \\              border-collapse: collapse;
        \\          }
        \\          table tr, table td, table th {
        \\              border: 1px solid black;
        \\          }
        \\          table td, table th {
        \\              padding: 0.25em;
        \\          }
        \\
        \\          code.function   { font-weight: bold; }
        \\          code.parameter { font-weight: bold; }
        \\          code.type       { color: indigo; }
        \\          code.number     { color: blue; }
        \\          code.string     { color: green; }
        \\          code.enumerator { color: red; }
        \\          code.register   { color: darkslategray; }
        \\          code.lut_index  { color: darkslateblue; }
        \\      </style>
        \\  </head>
        \\  <body>
        \\      <h1>Propan Standard Library</h1>
        \\
    );

    try writer.writeAll(
        \\
        \\      <h2>Functions</h2>
    );
    for (functions.keys(), functions.values(), 0..) |name, item, fn_index| {
        if (fn_index > 0) {
            try writer.writeAll("<hr>\n");
        }
        try writer.writeAll("<pre><code>");

        try writer.print("<code class=\"function\">{s}</code>(", .{name});

        if (item.params.len > 0) {
            try writer.writeAll("\n");

            var name_len: usize = 0;
            var type_len: usize = 0;
            for (item.params) |param| {
                name_len = @max(name_len, std.fmt.count("{s}", .{param.name}));
                type_len = @max(type_len, std.fmt.count("{t}", .{param.type}));
            }

            const padding: [64]u8 = @splat(' ');

            for (item.params) |param| {
                try writer.print("    {s}:{s} <code class=\"type\">{[2]t}</code>", .{
                    param.name,
                    padding[0 .. name_len - param.name.len],
                    param.type,
                });
                if (param.default_value) |default| {
                    try writer.print("{s} = {f}", .{
                        padding[0 .. type_len - @tagName(param.type).len],
                        ValueFmt{ .value = default.value, .hex = false },
                    });
                }
                try writer.writeAll(",\n");
            }
        }

        try writer.writeAll(")");

        try writer.writeAll("</code></pre>\n");

        try writer.print("<p>{s}</p>\n", .{item.docs});

        if (item.params.len > 0) {
            try writer.writeAll("<ul>\n");

            for (item.params) |param| {
                try writer.print("<li><code class=\"parameter\">{s}</code>: {s}</li>\n", .{
                    param.name,
                    param.docs,
                });
            }

            try writer.writeAll("</ul>\n");
        }
    }

    try writer.writeAll(
        \\      <h2>Constants</h2>
        \\      <table>
        \\          <thead>
        \\              <tr>
        \\                  <th>Name</th>
        \\                  <th>Type</th>
        \\                  <th>Value</th>
        \\                  <th>Usage</th>
        \\                  <th>Addressing</th>
        \\                  <th>Augmented</th>
        \\              </tr>
        \\          </thead>
        \\          <tbody>
        \\
    );
    for (constants.keys(), constants.values()) |key, value| {
        try writer.print(
            \\              <tr>
            \\                  <td><code>{[0]s}</code></td>
            \\                  <td>{[1]t}</td>
            \\                  <td>{[2]f}</td>
            \\                  <td>{[3]t}</td>
            \\                  <td>{[4]t}</td>
            \\                  <td>{[5]s}</td>
            \\              </tr>
            \\
        , .{
            key,
            value.value,
            ValueFmt{ .value = value.value, .hex = true },
            value.flags.usage,
            value.flags.addressing,
            if (value.flags.augment) "yes" else "no",
        });
    }

    try writer.writeAll(
        \\          </tbody>
        \\      </table>
        \\
    );

    try writer.writeAll(
        \\  </body>
        \\</html>
        \\
    );
}

const ValueFmt = struct {
    hex: bool,

    value: eval.Value.Payload,

    pub fn format(fmt: ValueFmt, writer: *std.Io.Writer) !void {
        switch (fmt.value) {
            .int => |value| if (fmt.hex)
                try writer.print("<code class=\"number hex\">0x{X:0>8}</code>", .{to_u32(value)})
            else
                try writer.print("<code class=\"number dec\">{0}</code>", .{value}),
            .string => |value| try writer.print("<code class=\"string\">\"{f}\"</code>", .{std.zig.fmtString(value)}),
            .register => |value| try writer.print("<code class=\"register\">{f}</code>", .{value}),
            .enumerator => |value| try writer.print("<code class=\"enumerator\">#{s}</code>", .{value}),
            .pointer_expr => |value| try writer.print("<code>{f}</code>", .{value}),

            .address => |address| {
                try writer.print("hub: <code class=\"number hex\">0x{X:0>5}</code><br />", .{address.hub_address});
                try writer.print("segment: <code class=\"number dec\">{}</code><br />", .{address.segment_id});
                switch (address.local) {
                    .hub => {},
                    .cog => |reg| try writer.print("cog: <code class=\"register\">r{}</code>", .{reg}),
                    .lut => |idx| try writer.print("lut: <code class=\"lut_index\">l{}</code>", .{idx}),
                }
            },
        }
    }
};

fn to_u32(value: i64) u32 {
    if (value < 0) {
        const signed: i32 = @intCast(value);
        return @bitCast(signed);
    }
    return @intCast(value);
}
