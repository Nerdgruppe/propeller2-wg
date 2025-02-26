import sys


from .parser import parse_file
from .render import render
from .sema import analyze, Scope


def main() -> int:
    _patch_print()

    path = "examples/propio-client.propan"
    if len(sys.argv) > 1:
        path = sys.argv[1]

    program = parse_file(path)
    if program is None:
        sys.stderr.write("parse failure!\n")
        return 1

    # render(program=program, file=sys.stdout)

    compile_unit = analyze(program=program)
    if not compile_unit:
        sys.stderr.write("analysis failure\n")
        return 1

    def _print_scope(scope: Scope, indent: str = "  ") -> None:
        for name, value in sorted(scope.symbols.items()):
            print(f"{indent}- {name} = {value}")
            _print_scope(value.locals, indent + "  ")

    print("symbol table:")
    _print_scope(compile_unit.globals)

    return 0


def _patch_print():
    _original_print = __builtins__["print"]

    def _stderr_print(*args, **kwargs):
        kwargs.setdefault("file", sys.stderr)
        _original_print(*args, **kwargs)

    __builtins__["print"] = _stderr_print
