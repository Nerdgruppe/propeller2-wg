"""Optional POSIX CLI integration check: python tests/windtunnel/terminal.py after building."""

import os
import pty
import select
import subprocess
import tempfile
import termios
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WINDTUNNEL = ROOT / "zig-out/bin/windtunnel"


class Session:
    """Run the compiled CLI on a real pseudoterminal and retain its original settings."""

    def __init__(self, *args):
        """Attach all console streams to one TTY, as in an ordinary terminal window."""
        self.master, self.slave = pty.openpty()
        self.original = termios.tcgetattr(self.slave)
        self.process = subprocess.Popen(
            [str(WINDTUNNEL), *args],
            stdin=self.slave,
            stdout=self.slave,
            stderr=self.slave,
        )
        self.output = bytearray()

    def __enter__(self):
        """Wait for raw mode and verify that signals, echo and software flow control are disabled."""
        deadline = time.monotonic() + 5
        while termios.tcgetattr(self.slave) == self.original:
            assert self.process.poll() is None, self.process.returncode
            assert time.monotonic() < deadline, "TTY did not enter raw mode"
            time.sleep(0.01)
        settings = termios.tcgetattr(self.slave)
        assert not settings[0] & (termios.ICRNL | termios.IXON | termios.IXOFF)
        assert not settings[3] & (termios.ICANON | termios.ECHO | termios.ISIG)
        return self

    def __exit__(self, *_):
        """Bound failed checks and restore the test TTY even if the child is killed."""
        if self.process.poll() is None:
            self.process.kill()
        self.process.wait(timeout=5)
        termios.tcsetattr(self.slave, termios.TCSANOW, self.original)
        os.close(self.master)
        os.close(self.slave)

    def write(self, data):
        """Send exact keyboard bytes without local line processing."""
        os.write(self.master, data)

    def until(self, expected, timeout=60):
        """Capture output until a marker arrives, reporting all captured bytes on failure."""
        deadline = time.monotonic() + timeout
        while expected not in self.output:
            assert self.process.poll() is None, bytes(self.output)
            assert time.monotonic() < deadline, bytes(self.output)
            if select.select([self.master], [], [], 0.1)[0]:
                self.output.extend(os.read(self.master, 65536))
        return bytes(self.output)

    def quit(self):
        """Require a prompt menu exit and exact restoration of every saved terminal attribute."""
        self.write(b"\x01\x11")
        self.process.wait(timeout=3)
        assert self.process.returncode == 0, self.process.returncode
        assert termios.tcgetattr(self.slave) == self.original


def main():
    """Exercise startup combinations, raw control bytes, TAQOZ and cancellation during upload."""
    with tempfile.TemporaryDirectory(prefix="windtunnel-terminal-") as directory:
        image = Path(directory) / "echo.bin"
        subprocess.run(
            [
                str(ROOT / "zig-out/bin/propan"),
                "-o",
                str(image),
                str(ROOT / "tests/windtunnel/program/server-terminal.propan"),
            ],
            check=True,
        )
        for options in [[], ["--boot"]]:
            with Session(*options) as terminal:
                terminal.write(b"\x01\x08")
                terminal.until(b"Ctrl-A, Ctrl-Q")
                terminal.write(b"Prop_Chk 0 0 0 0\r")
                terminal.until(b"Prop_Ver G")
                terminal.quit()

        for options in [[], ["--boot"], ["--boot", "--fast-io"]]:
            with Session(*options, "--image", str(image)) as terminal:
                terminal.until(b"!")
                terminal.output.clear()
                terminal.write(b"\x03\x01\x01\x13")
                assert terminal.until(b"\x03\x01\x13").startswith(b"\x03\x01\x13")
                terminal.quit()

        with Session("--boot") as terminal:
            terminal.write(b"\x1b")
            terminal.until(b"TAQOZ# ")
            terminal.output.clear()
            terminal.write(b"1 2 + .\r")
            assert b"3" in terminal.until(b"TAQOZ# ")
            terminal.quit()

        large_image = Path(directory) / "large.bin"
        large_image.write_bytes(image.read_bytes() + bytes(65536))
        with Session(
            "--boot", "--image", str(large_image), "--baudrate", "9600"
        ) as terminal:
            time.sleep(2)
            # A paste larger than a fixed queue must not hide the following quit command.
            terminal.write(b"A" * 131072)
            terminal.quit()

    print(
        "CLI startup, raw UART controls, TAQOZ, upload cancellation and TTY restoration passed"
    )


if __name__ == "__main__":
    main()
