"""Exercise native MCP images, input timing, and disconnect recovery against a fake video peer."""
import base64
import json
import platform
import socket
import struct
import subprocess
import tempfile
import threading
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
ARCH = {'x86_64': 'x64', 'aarch64': 'arm64'}[platform.machine()]
DLL = ROOT / f'src/scopecreep/bin/Debug/net10.0/linux-{ARCH}/scopecreep.dll'


class Peer:
    """Serve generated H.264 packets and record complete control events per connection."""

    def __init__(self, payload):
        """Bind a private loopback endpoint; no physical scope is contacted."""
        self.payload = payload
        self.listener = socket.socket()
        self.listener.bind(('127.0.0.1', 0))
        self.listener.listen()
        self.listener.settimeout(.2)
        self.port = self.listener.getsockname()[1]
        self.stop = threading.Event()
        self.events = []
        self.connections = []
        self.thread = threading.Thread(target=self.accept, daemon=True)
        self.thread.start()

    def accept(self):
        """Accept replacement clients, each with independent recorded input and framed video."""
        while not self.stop.is_set():
            try:
                connection, _ = self.listener.accept()
            except TimeoutError:
                continue
            except OSError:
                return
            self.connections.append(connection)
            index = len(self.connections)
            threading.Thread(target=self.read, args=(connection, index), daemon=True).start()
            threading.Thread(target=self.video, args=(connection,), daemon=True).start()

    def read(self, connection, index):
        """Record whole 12-byte events even if socket reads split or coalesce them."""
        pending = b''
        try:
            while data := connection.recv(4096):
                pending += data
                while len(pending) >= 12:
                    self.events.append((index, time.monotonic(), *struct.unpack('<III', pending[:12])))
                    pending = pending[12:]
        except OSError:
            pass

    def video(self, connection):
        """Send complete NAL payloads with deliberately fragmented scope headers."""
        header = b'\x03\x58\x55\xaa' + struct.pack('<I', len(self.payload))
        try:
            while not self.stop.is_set():
                connection.sendall(header[:3])
                connection.sendall(header[3:] + self.payload)
                time.sleep(.1)
        except OSError:
            pass

    def disconnect(self):
        """Simulate relay failure while retaining the ability to accept a replacement client."""
        connection = self.connections[-1]
        connection.shutdown(socket.SHUT_RDWR)
        connection.close()

    def close(self):
        """Stop this test endpoint and all accepted sockets."""
        self.stop.set()
        self.listener.close()
        for connection in self.connections:
            connection.close()
        self.thread.join(timeout=2)


def main():
    """Verify the production executable through stdio using only generated video and loopback sockets."""
    encoded = subprocess.run([
        'ffmpeg', '-v', 'error', '-f', 'lavfi', '-i', 'color=c=blue:s=800x600:r=10',
        '-frames:v', '2', '-c:v', 'libx264', '-preset', 'ultrafast', '-tune', 'zerolatency',
        '-x264-params', 'keyint=1:repeat-headers=1', '-f', 'h264', 'pipe:1'],
        capture_output=True, check=True, timeout=15).stdout
    peer = Peer(encoded)
    process = None
    try:
        with tempfile.TemporaryDirectory() as directory:
            config = Path(directory) / 'config.json'
            config.write_text(json.dumps({'devicePath': str(Path(directory) / 'missing-device'), 'remotePort': peer.port}))
            process = subprocess.Popen(['dotnet', str(DLL), '--config', str(config)],
                                       stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True)
            sequence = 0

            pending_responses = {}

            def read_response(identifier):
                """Keep out-of-order replies so concurrent requests can be observed safely."""
                while identifier not in pending_responses:
                    line = process.stdout.readline()
                    assert line, 'MCP exited'
                    response = json.loads(line)
                    if 'id' in response:
                        pending_responses[response['id']] = response
                response = pending_responses.pop(identifier)
                assert 'error' not in response, response
                return response['result']

            def rpc(method, params):
                """Read one JSON-RPC response, proving FFmpeg cannot contaminate protocol stdout."""
                nonlocal sequence
                sequence += 1
                process.stdin.write(json.dumps({'jsonrpc': '2.0', 'id': sequence, 'method': method, 'params': params}) + '\n')
                process.stdin.flush()
                return read_response(sequence)

            def call(name, arguments=None):
                """Return both protocol content and the JSON metadata block."""
                result = rpc('tools/call', {'name': name, 'arguments': arguments or {}})
                texts = [x['text'] for x in result.get('content', []) if x['type'] == 'text']
                metadata = json.loads(texts[0]) if texts and texts[0].startswith('{') else None
                return result, metadata

            rpc('initialize', {'protocolVersion': '2025-06-18', 'capabilities': {}, 'clientInfo': {'name': 'remote-checks', 'version': '1'}})
            process.stdin.write('{"jsonrpc":"2.0","method":"notifications/initialized"}\n')
            process.stdin.flush()
            result, screen = call('scope_screen', {'timeoutMs': 15000})
            assert not result.get('isError'), result
            png = base64.b64decode(next(x['data'] for x in result['content'] if x['type'] == 'image'))
            assert png.startswith(b'\x89PNG') and struct.unpack('>II', png[16:24]) == (800, 600)
            assert peer.events and peer.events[0][2:] == (1, 400, 300), 'startup did not release a previous process\'s held button'
            before = len(peer.events)
            result, press = call('scope_press', {'x': 120, 'y': 130, 'durationMs': 1000, 'frameId': screen['frameId']})
            assert not result.get('isError'), result
            deadline = time.monotonic() + 2
            while len(peer.events) < before + 2 and time.monotonic() < deadline:
                time.sleep(.01)
            events = peer.events[before:]
            assert [(x[2], x[3], x[4]) for x in events] == [(0x101, 120, 130), (1, 120, 130)], events
            assert events[1][1] - events[0][1] >= .9, events
            result, drag = call('scope_drag', {'fromX': 10, 'fromY': 20, 'toX': 40, 'toY': 50, 'durationMs': 300})
            assert not result.get('isError'), result
            result, observed = call('scope_screen', {'afterActionId': press['actionId'], 'settleMs': 200})
            assert not result.get('isError') and observed['superseded'] and observed['lastActionId'] == drag['actionId']
            before = len(peer.events)
            process.stdin.write(json.dumps({'jsonrpc': '2.0', 'id': 9000, 'method': 'tools/call',
                                           'params': {'name': 'scope_press', 'arguments': {'x': 15, 'y': 16, 'durationMs': 3000}}}) + '\n')
            process.stdin.flush()
            deadline = time.monotonic() + 5
            while len(peer.events) == before and time.monotonic() < deadline:
                time.sleep(.01)
            result, during = call('scope_screen', {'afterActionId': drag['actionId'], 'settleMs': 0})
            assert not result.get('isError') and during['superseded'], 'in-progress mutation was invisible to observers'
            status_result, status = call('scope_status')
            assert status['lastAction']['outcome'] == 'inProgress', status
            pending_press = read_response(9000)
            assert not pending_press.get('isError'), pending_press
            pending_metadata = json.loads(pending_press['content'][0]['text'])
            assert during['lastActionId'] == pending_metadata['actionId'], 'action ID changed on completion'
            before = len(peer.events)
            result, _ = call('scope_press', {'x': 800, 'y': 2})
            assert result.get('isError') and len(peer.events) == before, 'invalid coordinates reached peer'

            def break_during_press():
                """Drop the socket after the first held-button event reaches the peer."""
                deadline = time.monotonic() + 5
                while len(peer.events) == before and time.monotonic() < deadline:
                    time.sleep(.01)
                peer.disconnect()

            breaking = threading.Thread(target=break_during_press)
            breaking.start()
            result, _ = call('scope_press', {'x': 200, 'y': 210, 'durationMs': 3500})
            breaking.join(timeout=5)
            assert result.get('isError'), 'interrupted press reported success'
            result, new_screen = call('scope_screen', {'timeoutMs': 15000})
            assert not result.get('isError') and new_screen['generation'] > screen['generation'], result
            new_events = [x for x in peer.events if x[0] == len(peer.connections)]
            assert new_events and new_events[0][2] == 1 and all(x[2] != 0x101 for x in new_events), new_events
            before = len(peer.events)
            result, _ = call('scope_press', {'x': 10, 'y': 10, 'frameId': screen['frameId']})
            assert result.get('isError') and len(peer.events) == before, 'stale frame input reached peer'
            print('PASS: native MCP PNG, long-press timing, drag, action settling/supersession, reconnect release/no replay, stale-frame rejection')
    finally:
        if process is not None:
            process.terminate()
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=5)
        peer.close()


if __name__ == '__main__':
    main()
