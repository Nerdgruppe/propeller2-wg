
zig := "zig-0.14.0"

nothing:
    true

update-windtunnel:
    .venv/bin/python utility/gen_windtunnel.py encoding \
        | tee src/windtunnel/sim/encoding.zig
    .venv/bin/python utility/gen_windtunnel.py decoder \
        | tee src/windtunnel/sim/decode.zig

    {{zig}} fmt src/windtunnel/sim/encoding.zig
    {{zig}} fmt src/windtunnel/sim/decode.zig

regenerate-windtunnel-executor:
    .venv/bin/python utility/gen_windtunnel.py executor \
        | tee src/windtunnel/sim/execute.zig
    {{zig}} fmt src/windtunnel/sim/execute.zig

setup-venv:
    python -m venv .venv
    .venv/bin/python -m pip install -r tools/requirements.txt
    .venv/bin/python -m pip install -r tools/dev-requirements.txt
    realpath tools > .venv/lib/$(python -c 'import sys; print(f"python{sys.version_info.major}.{sys.version_info.minor}")')/site-packages/nerdgruppe.pth
