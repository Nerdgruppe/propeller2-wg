
demo-propan:
    .venv/bin/python -m nerdgruppe.p2.propan examples/propio-client.propan

test: test-unit test-propan  demo-propan

setup-venv:
    python -m venv .venv
    .venv/bin/python -m pip install -r tools/requirements.txt
    .venv/bin/python -m pip install -r tools/dev-requirements.txt
    realpath tools > .venv/lib/$(python -c 'import sys; print(f"python{sys.version_info.major}.{sys.version_info.minor}")')/site-packages/nerdgruppe.pth

test-pasm-parser:
    .venv/bin/python -m nerdgruppe.p2.pasm

test-propan:
    for file in $(find tests/propan/sema -name "*.propan"); do \
        echo $file ; \
        .venv/bin/python -m nerdgruppe.p2.propan "$file" --validate-output "${file%.propan}.json" ; \
    done

test-unit:
    .venv/bin/python -m pytest tests/python