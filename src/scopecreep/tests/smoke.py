"""Exercise both MCP transports without hardware; build Scopecreep first. Uses Python's standard library."""

import json
import platform
import signal
import socket
import subprocess
import tempfile
import time
import urllib.error
import urllib.request
from contextlib import contextmanager
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
ARCH = {"x86_64": "x64", "aarch64": "arm64"}[platform.machine()]
BINARY = ROOT / f"src/scopecreep/bin/Debug/net10.0/linux-{ARCH}/scopecreep"
INITIALIZE = {"protocolVersion": "2025-06-18", "capabilities": {},
              "clientInfo": {"name": "scopecreep-smoke", "version": "1"}}


@contextmanager
def running(*args, **kwargs):
    """Stop the test server even when an assertion fails."""
    with subprocess.Popen([str(BINARY), *args], **kwargs) as process:
        try:
            yield process
        finally:
            if process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait(timeout=5)


def verify_tools(call):
    """Verify diagnostic-only schemas and failures without requiring a connected scope."""
    tools = call("tools/list", {})["tools"]
    assert {tool["name"] for tool in tools} == {"scope_status", "scpi_command", "scpi_query", "scope_screen", "scope_press", "scope_drag", "scope_actions", "scope_action", "scope_configure", "scope_files", "scope_file_get", "scope_file_read", "scope_file_save"}, tools
    for name, arguments in [
        ("scpi_query", {"command": "*IDN?;*IDN?"}),
        ("scpi_query", {"command": "*IDN?", "maxBytes": 0}),
        ("scpi_query", {"command": "*IDN?", "format": "invalid"}),
        ("scpi_command", {"command": "*IDN?"}),
        ("scpi_query", {"command": "*IDN?"}),
    ]:
        result = call("tools/call", {"name": name, "arguments": arguments})
        assert result.get("isError"), result
        assert result.get("content"), result


def main():
    """Run stdio, HTTP, and fail-before-I/O diagnostic-script checks."""
    signal.alarm(45)
    with tempfile.TemporaryDirectory() as temporary:
        config = Path(temporary) / "scopecreep.json"
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            port = listener.getsockname()[1]
        config.write_text(json.dumps({"devicePath": "missing-device", "httpUrl": f"http://127.0.0.1:{port}", "remoteEnabled": False}))
        with running("--config", str(config), stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=subprocess.DEVNULL, text=True) as process:
            identifier = 0

            def stdio_call(method, params):
                """Read only JSON-RPC stdout messages; any log contamination fails JSON parsing."""
                nonlocal identifier
                identifier += 1
                process.stdin.write(json.dumps({"jsonrpc": "2.0", "id": identifier,
                                                "method": method, "params": params}) + "\n")
                process.stdin.flush()
                while True:
                    line = process.stdout.readline()
                    assert line, "stdio server exited"
                    response = json.loads(line)
                    if response.get("id") == identifier:
                        assert "error" not in response, response
                        return response["result"]

            assert "serverInfo" in stdio_call("initialize", INITIALIZE)
            process.stdin.write('{"jsonrpc":"2.0","method":"notifications/initialized"}\n')
            process.stdin.flush()
            verify_tools(stdio_call)

        with running("--http", "--config", str(config), stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL):
            identifier = 0

            def http_call(method, params):
                """Read a JSON or SSE response from the stateless HTTP endpoint."""
                nonlocal identifier
                identifier += 1
                body = json.dumps({"jsonrpc": "2.0", "id": identifier, "method": method, "params": params}).encode()
                request = urllib.request.Request(f"http://127.0.0.1:{port}/mcp", body,
                    {"Content-Type": "application/json", "Accept": "application/json, text/event-stream"})
                with urllib.request.urlopen(request, timeout=3) as response:
                    payload = response.read().decode()
                    if response.headers.get_content_type() == "text/event-stream":
                        payload = next(line[6:] for line in payload.splitlines() if line.startswith("data: "))
                    result = json.loads(payload)
                    assert "error" not in result, result
                    return result["result"]

            for _ in range(50):
                try:
                    assert "serverInfo" in http_call("initialize", INITIALIZE)
                    break
                except urllib.error.URLError:
                    time.sleep(0.1)
            else:
                raise AssertionError("HTTP server did not start")
            verify_tools(http_call)

        script = Path(temporary) / "invalid.json"
        script.write_text(json.dumps([{"command": "*IDN?", "query": True},
                                      {"command": "*IDN?;*IDN?", "query": True}]))
        result = subprocess.run([str(BINARY), "--config", str(config), "--diagnose", str(script)],
                                capture_output=True, timeout=5)
        assert result.returncode != 0 and not result.stdout, result
    signal.alarm(0)
    print("PASS: stdio/HTTP MCP, schemas, validation, missing-device errors, diagnostic prevalidation")


if __name__ == "__main__":
    main()
