#!/usr/bin/env python3
"""Check fragmented framing, IDR joining, and malformed video without hardware."""

import importlib.util
import io
import struct
from pathlib import Path


spec = importlib.util.spec_from_file_location("scope_video", Path(__file__).with_name("scope-video.py"))
video = importlib.util.module_from_spec(spec)
spec.loader.exec_module(video)


class Fragmented(io.BytesIO):
    """Simulate stream reads split inside headers and payloads."""

    def read(self, size=-1):
        """Return at most three bytes per read."""
        return super().read(min(size, 3))


def packet(payload):
    """Wrap an Annex B payload in the scope's observed framing."""
    return b"\x03\x58\x55\xaa" + struct.pack("<I", len(payload)) + payload


def check():
    """Verify exact output and reject malformed or truncated packets."""
    inter = b"\x00\x00\x00\x01\x41\x12\x34"
    keyframe = b"\x00\x00\x01\x65\x56\x78"
    output = io.BytesIO()
    video.reconstruct(Fragmented(packet(inter) + packet(inter + keyframe) + packet(inter)), output)
    assert output.getvalue() == video.SETUP + keyframe + inter
    for malformed in (b"\x03", b"badmagic", packet(keyframe)[:-1], packet(b"x")[:8],
                      b"\x03\x58\x55\xaa" + struct.pack("<I", video.MAX_PAYLOAD + 1)):
        try:
            video.reconstruct(Fragmented(malformed), io.BytesIO())
        except (EOFError, ValueError):
            pass
        else:
            raise AssertionError("Malformed packet accepted")
    print("PASS: fragmented video, SPS/PPS prefeed, IDR joining, invalid/truncated packet rejection")


if __name__ == "__main__":
    check()
