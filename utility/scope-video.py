#!/usr/bin/env python3
"""Convert the local STO1104C relay's framed video to decodable Annex B H.264."""

import re
import socket
import struct
import sys


# Exact SPS/PPS from this scope's initial video packet, preserved in /tmp/micsig.log.
# Firmware-specific: replace these if the scope's encoder configuration changes.
SETUP = bytes.fromhex("000000016742401fa680c8137e540000000168ce3880")
MAX_PAYLOAD = 16 * 1024 * 1024
START_CODE = re.compile(b"\x00\x00(?:\x00)?\x01")


def read_exact(stream, size):
    """Read a complete field, distinguishing clean EOF from a truncated packet."""
    data = bytearray()
    while len(data) < size:
        chunk = stream.read(size - len(data))
        if not chunk:
            if not data:
                return None
            raise EOFError("Truncated video packet")
        data.extend(chunk)
    return bytes(data)


def reconstruct(stream, output):
    """Strip framing, prefeed known decoder parameters, and join at an IDR NAL."""
    output.write(SETUP)
    output.flush()
    waiting_for_keyframe = True
    while True:
        header = read_exact(stream, 8)
        if header is None:
            return
        if header[2:4] != b"\x55\xaa":
            raise ValueError("Invalid video packet magic")
        size = struct.unpack_from("<I", header, 4)[0]
        if size > MAX_PAYLOAD:
            raise ValueError("Video payload exceeds 16 MiB")
        payload = read_exact(stream, size)
        if payload is None:
            raise EOFError("Missing video payload")
        if waiting_for_keyframe:
            # Observed scope packets contain complete Annex B NAL units.
            for start in START_CODE.finditer(payload):
                if start.end() < len(payload) and payload[start.end()] & 31 == 5:
                    payload = payload[start.start():]
                    waiting_for_keyframe = False
                    break
            else:
                continue
        output.write(payload)
        output.flush()


def main():
    """Drain the existing local relay without sending any scope control events."""
    try:
        with socket.create_connection(("127.0.0.1", 8888), timeout=5) as connection:
            connection.settimeout(15)
            with connection.makefile("rb") as stream:
                reconstruct(stream, sys.stdout.buffer)
        print("Scope video connection closed", file=sys.stderr)
        return 1
    except BrokenPipeError:
        # The downstream decoder exited; suppress a second error during final flush.
        try:
            sys.stdout.close()
        except BrokenPipeError:
            pass
        return 0
    except (OSError, EOFError, ValueError) as error:
        print(f"Scope video failed: {error}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        return 130


if __name__ == "__main__":
    sys.exit(main())
