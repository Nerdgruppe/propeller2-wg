
test: test-propan

setup-venv:
    python -m venv .venv
    .venv/bin/python -m pip install -r tools/requirements.txt
    realpath tools > .venv/lib/$(python -c 'import sys; print(f"python{sys.version_info.major}.{sys.version_info.minor}")')/site-packages/nerdgruppe.pth

test-pasm-parser:
    .venv/bin/python -m nerdgruppe.p2.pasm

test-propan:
    .venv/bin/python -m nerdgruppe.p2.propan
