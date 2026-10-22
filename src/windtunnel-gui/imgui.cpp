#include "imgui.h"

/// Read the current frame's wheel delta, which zgui does not expose.
extern "C" float windtunnelScopeMouseWheel() {
    return ImGui::GetIO().MouseWheel;
}
