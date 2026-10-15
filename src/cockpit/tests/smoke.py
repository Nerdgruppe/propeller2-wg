"""Smoke test: build Cockpit first, then run this with Python and websockets installed."""

import asyncio
import json
import os
import platform
import socket
import struct
import subprocess
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from contextlib import contextmanager
from pathlib import Path

import websockets


ROOT = Path(__file__).resolve().parents[3]
SYSTEM = {"Windows": "win", "Darwin": "osx", "Linux": "linux"}[platform.system()]
ARCH = {"x86_64": "x64", "AMD64": "x64", "aarch64": "arm64", "arm64": "arm64"}[platform.machine()]
DLL = ROOT / "src/cockpit/bin/Debug/net10.0" / f"{SYSTEM}-{ARCH}" / "cockpit.dll"


@contextmanager
def running_process(*args, **kwargs):
    with subprocess.Popen(*args, **kwargs) as process:
        try:
            yield process
        finally:
            if process.poll() is None:
                process.terminate()
                process.wait(timeout=5)


class Mcp:
    def __init__(self, process):
        self.process = process
        self.id = 0

    def call(self, method, params=None):
        self.id += 1
        request = {"jsonrpc": "2.0", "id": self.id, "method": method, "params": params or {}}
        self.process.stdin.write(json.dumps(request) + "\n")
        self.process.stdin.flush()
        while True:
            line = self.process.stdout.readline()
            assert line, self.process.stderr.read()
            response = json.loads(line)
            if response.get("id") == self.id:
                assert "error" not in response, response
                return response["result"]

    def tool(self, name, **arguments):
        result = self.call("tools/call", {"name": name, "arguments": arguments})
        assert not result.get("isError"), result
        return json.loads(result["content"][0]["text"])


async def main():
    seen = []
    queries = []

    async def board(ws):
        queries.append(urllib.parse.parse_qs(urllib.parse.urlsplit(ws.request.path).query))
        upload = await ws.recv()
        seen.append((struct.unpack("<I", upload[:4])[0], upload[4:]))
        data = await ws.recv()
        seen.append(data)
        if data == b"deny":
            await ws.close(code=1008, reason="board rejected")
        elif data == b"long":
            await ws.send(b"a" * (1_048_576 + 1))
        elif data == b"txt":
            await ws.send(b'hello "P2"\n')
        elif data == b"drop":
            await ws.send(b"done")
            ws.transport.abort()
        else:
            await ws.send(bytes([0, 255, 1, 128]))

    def reject_bad_baud(ws, request):
        if urllib.parse.parse_qs(urllib.parse.urlsplit(request.path).query).get("baudrate") == ["57601"]:
            response = ws.respond(400, "Bad baud rate")
            response.headers["x-p2aas-error"] = "Serial port rejects baud 57601"
            return response

    async with websockets.serve(board, "127.0.0.1", 0, process_request=reject_bad_baud) as server:
        port = server.sockets[0].getsockname()[1]
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            http_port = listener.getsockname()[1]
        with tempfile.TemporaryDirectory() as config_directory:
            config = Path(config_directory) / "cockpit.json"
            config.write_text(json.dumps({
                "p2aasEndpoint": f"ws://127.0.0.1:{port}/",
                "httpUrl": f"http://127.0.0.1:{http_port}",
            }))

            with tempfile.TemporaryDirectory() as temporary, running_process(
                ["dotnet", str(DLL), "--config", str(config)],
                stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE, text=True,
                env={**os.environ, "TMPDIR": temporary},
            ) as process:
                mcp = Mcp(process)
                await asyncio.to_thread(mcp.call, "initialize", {
                    "protocolVersion": "2025-06-18", "capabilities": {},
                    "clientInfo": {"name": "smoke", "version": "1"},
                })
                process.stdin.write('{"jsonrpc":"2.0","method":"notifications/initialized"}\n')
                process.stdin.flush()
                tools = (await asyncio.to_thread(mcp.call, "tools/list"))["tools"]
                assert "mcp-reboot" not in {tool["name"] for tool in tools}
                assert (await asyncio.to_thread(mcp.tool, "assemble", language="propan", code="NOP\n"))["imageSize"] == 4
                assert (await asyncio.to_thread(mcp.tool, "assemble", language="pasm2", code="DAT\n org 0\n nop\n"))["success"]
                assert "Unknown symbol" in (await asyncio.to_thread(mcp.tool, "assemble", language="pasm2", code="DAT\n org 0\n mov x, y\n"))["diagnostics"]
                assert "unknown mnemonic" in (await asyncio.to_thread(mcp.tool, "assemble", language="propan", code="BOGUS\n"))["diagnostics"]
                assert "cogexec" in (await asyncio.to_thread(mcp.tool, "assemble", language="propan", code="NOP\n", format="listing"))["output"]
                assert len((await asyncio.to_thread(mcp.tool, "lookup_instruction", mnemonic="CALLD"))["options"]) > 1
                assert await asyncio.to_thread(mcp.tool, "search_instructions", query="rotate", limit=2)
                run = await asyncio.to_thread(mcp.tool, "run", language="propan", code="NOP\n", stdin="abc", format="i16", baudrate=57600, timeout_ms=7500)
                assert run["success"] and run["output"]["values"] == [-256, -32767], run
                assert queries[0] == {"baudrate": ["57600"], "timeout_ms": ["7500"]}, queries
                run = await asyncio.to_thread(mcp.tool, "run", language="propan", code="NOP\n", stdin="txt", format="text")
                assert run["success"] and run["output"] == 'hello "P2"\n', run
                assert queries[1] == {"baudrate": ["115200"], "timeout_ms": ["5000"]}, queries
                run = await asyncio.to_thread(mcp.tool, "run", language="propan", code="CALLPA 'X', write_byte\n",
                                              stdin="txt", scaffold=True, timeout_ms=300)
                assert run["success"] and seen[-2][0] > 50, run
                run = await asyncio.to_thread(mcp.tool, "run", language="pasm2", code='callpa #"X", #write_byte\n',
                                              stdin="txt", scaffold=True, timeout_ms=300)
                assert run["success"] and seen[-2][0] > 50, run
                run = await asyncio.to_thread(mcp.tool, "run", language="propan", code="NOP\n", stdin="drop", timeout_ms=300)
                assert run["success"] and run["output"] == "done" and run["closeStatus"] == "Disconnected", run
                run = await asyncio.to_thread(mcp.tool, "run", language="propan", code="NOP\n", baudrate=57601)
                assert not run["success"] and run["httpStatus"] == 400, run
                assert run["error"] == "Serial port rejects baud 57601", run
                run = await asyncio.to_thread(mcp.tool, "run", language="propan", code="NOP\n", stdin="deny")
                assert not run["success"] and run["closeStatus"] == "PolicyViolation", run
                run = await asyncio.to_thread(mcp.tool, "run", language="propan", code="NOP\n", stdinBase64="bG9uZw==")
                assert not run["success"] and run["truncated"] and not run["complete"]
                assert len(run["output"]) == 1_048_576
                assert seen[:4] == [
                    (4, b"\x00\x00\x00\x00"), b"abc",
                    (4, b"\x00\x00\x00\x00"), b"txt",
                ], seen
                assert seen[4][0] > 50 and seen[5] == b"txt", seen
                assert seen[6][0] > 50 and seen[7] == b"txt", seen
                assert seen[8:] == [
                    (4, b"\x00\x00\x00\x00"), b"drop",
                    (4, b"\x00\x00\x00\x00"), b"deny",
                    (4, b"\x00\x00\x00\x00"), b"long",
                ], seen
                process.terminate()
                process.wait(timeout=5)
                assert not list(Path(temporary).iterdir()), f"Temporary files left behind: {list(Path(temporary).iterdir())}"

            with running_process(["dotnet", str(DLL), "--development", "--config", str(config)],
                                 stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 text=True) as process:
                mcp = Mcp(process)
                await asyncio.to_thread(mcp.call, "initialize", {
                    "protocolVersion": "2025-06-18", "capabilities": {},
                    "clientInfo": {"name": "smoke", "version": "1"},
                })
                process.stdin.write('{"jsonrpc":"2.0","method":"notifications/initialized"}\n')
                process.stdin.flush()
                tools = (await asyncio.to_thread(mcp.call, "tools/list"))["tools"]
                assert "mcp-reboot" in {tool["name"] for tool in tools}
                reboot = await asyncio.to_thread(mcp.call, "tools/call", {
                    "name": "mcp-reboot", "arguments": {},
                })
                assert reboot["content"][0]["text"] == "Cockpit is stopping for restart.", reboot
                assert await asyncio.to_thread(process.wait, 5) == 0

            with running_process(["dotnet", str(DLL), "--http", "--config", str(config)],
                                  stdout=subprocess.PIPE, stderr=subprocess.PIPE) as process:
                def http_call(identifier, method, params):
                    body = json.dumps({"jsonrpc": "2.0", "id": identifier, "method": method, "params": params}).encode()
                    request = urllib.request.Request(f"http://127.0.0.1:{http_port}/mcp", body,
                        {"Content-Type": "application/json", "Accept": "application/json, text/event-stream"})
                    with urllib.request.urlopen(request, timeout=3) as response:
                        payload = response.read().decode()
                        if response.headers.get_content_type() == "text/event-stream":
                            payload = next(line[6:] for line in payload.splitlines() if line.startswith("data: "))
                        return json.loads(payload)

                for _ in range(30):
                    try:
                        result = await asyncio.to_thread(http_call, 1, "initialize", {
                            "protocolVersion": "2025-06-18", "capabilities": {},
                            "clientInfo": {"name": "smoke", "version": "1"},
                        })
                        break
                    except urllib.error.URLError:
                        await asyncio.sleep(0.1)
                else:
                    raise AssertionError("HTTP MCP did not start")
                assert "serverInfo" in result["result"], result
                result = await asyncio.to_thread(http_call, 2, "tools/call", {
                    "name": "lookup_instruction", "arguments": {"mnemonic": "CALLD"},
                })
                assert json.loads(result["result"]["content"][0]["text"])["referenceRows"], result
                process.terminate()
                process.wait(timeout=5)
    print("Cockpit smoke test passed")


if __name__ == "__main__":
    asyncio.run(main())
