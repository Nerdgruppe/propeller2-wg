import sys

from .parser import parse_file
from .render import render

def main():
    _patch_print()

    path = "examples/propio-client.propan"
    if len(sys.argv) > 1:
        path = sys.argv[1]

    program = parse_file(path)

    if program is not None:
        render(program=program, file=sys.stdout)
    else:
        sys.stderr.write("parse failure!\n")

def _patch_print():
    _original_print = __builtins__["print"]

    def _stderr_print(*args, **kwargs):
        kwargs.setdefault("file", sys.stderr)
        _original_print(*args, **kwargs)

    __builtins__["print"] = _stderr_print
