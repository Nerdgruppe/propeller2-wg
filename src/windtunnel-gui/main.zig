//! Minimal desktop shell for Windtunnel, using zig-gamedev's GLFW/OpenGL ImGui backend.
const std = @import("std");
const glfw = @import("zglfw");
const zopengl = @import("zopengl");
const zgui = @import("zgui");
const gl = zopengl.bindings;

const Inputs = struct {
    image_path: [1024:0]u8 = @splat(0),
    cog: i32 = 0,
    cycles_per_step: i32 = 1,
    fast_io: bool = false,
};

/// Open the desktop window and render interactive controls until the user closes it.
pub fn main(init: std.process.Init) !void {
    try glfw.init();
    defer glfw.terminate();

    glfw.windowHint(.context_version_major, 3);
    glfw.windowHint(.context_version_minor, 3);
    glfw.windowHint(.opengl_profile, .opengl_core_profile);
    glfw.windowHint(.opengl_forward_compat, true);

    const window = try glfw.createWindow(960, 640, "Windtunnel", null, null);
    defer window.destroy();

    glfw.makeContextCurrent(window);
    glfw.swapInterval(1);
    try zopengl.loadCoreProfile(glfw.getProcAddress, 3, 3);

    zgui.init(init.gpa);
    defer zgui.deinit();

    zgui.io.setIniFilename(null);
    zgui.backend.initWithGlSlVersion(window, "#version 330 core");
    defer zgui.backend.deinit();

    var inputs: Inputs = .{};
    while (!window.shouldClose()) {
        glfw.pollEvents();
        if (window.shouldClose())
            break;

        const size = window.getFramebufferSize();
        if (size[0] == 0 or size[1] == 0) {
            glfw.waitEvents();
            continue;
        }

        zgui.backend.newFrame(@intCast(size[0]), @intCast(size[1]));
        drawControls(window, &inputs);

        gl.viewport(0, 0, size[0], size[1]);
        gl.clearColor(0.08, 0.09, 0.12, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT);
        zgui.backend.draw();
        window.swapBuffers();
    }
}

/// Render basic editable inputs; simulation integration can use these values later.
fn drawControls(window: *glfw.Window, inputs: *Inputs) void {
    zgui.setNextWindowPos(.{ .x = 24, .y = 24, .cond = .first_use_ever });
    zgui.setNextWindowSize(.{ .w = 520, .h = 300, .cond = .first_use_ever });
    const visible = zgui.begin("Windtunnel", .{});
    defer zgui.end();

    if (!visible)
        return;

    _ = zgui.inputText("Image path", .{ .buf = &inputs.image_path });
    _ = zgui.sliderInt("Cog", .{ .v = &inputs.cog, .min = 0, .max = 7 });
    if (zgui.inputInt("Cycles per step", .{ .v = &inputs.cycles_per_step })) {
        inputs.cycles_per_step = @max(1, inputs.cycles_per_step);
    }
    _ = zgui.checkbox("Fast I/O", .{ .v = &inputs.fast_io });

    zgui.separator();
    if (zgui.button("Reset inputs", .{})) {
        inputs.* = .{};
    }
    zgui.sameLine(.{});
    if (zgui.button("Quit", .{})) {
        window.setShouldClose(true);
    }
}
