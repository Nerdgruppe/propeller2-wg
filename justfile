
zig := "zig-0.15.2"

# Builds and validates all projects and
validate: build validate-p2db

# Builds the Zig based software
build:
    {{zig}} build \
        -freference-trace=10 \
        --prominent-compile-errors \
        install \
        test 

# Regenerates src/windtunnel/sim/{encoding,decode}.zig files
update-windtunnel:
    .venv/bin/python utility/gen_windtunnel.py encoding \
        | tee src/windtunnel/sim/encoding.zig
    .venv/bin/python utility/gen_windtunnel.py decoder \
        | tee src/windtunnel/sim/decode.zig

    {{zig}} fmt src/windtunnel/sim/encoding.zig
    {{zig}} fmt src/windtunnel/sim/decode.zig

# Regenerates src/windtunnel/sim/execute.zig, potentially destructive!
regenerate-windtunnel-executor:
    .venv/bin/python utility/gen_windtunnel.py executor src/windtunnel/sim/execute.zig
    {{zig}} fmt src/windtunnel/sim/execute.zig

# Validates the P2DB schema and database.
validate-p2db:
    cue vet data/p2db/p2db.cue
    cue vet -c data/p2db/p2db.cue data/p2db/db/p2db.yaml
    cue export \
        --out json \
        --outfile data/p2db/p2db.json \
        --force \
        data/p2db/p2db.cue \
        data/p2db/db/*.yaml
    @echo "P2DB ok."


# Sets up the virtual environment for Python work.
setup-venv:
    uv venv --allow-existing 
    uv pip install -r tools/requirements.txt
    uv pip install -r tools/dev-requirements.txt
    realpath tools > .venv/lib/$(python -c 'import sys; print(f"python{sys.version_info.major}.{sys.version_info.minor}")')/site-packages/nerdgruppe.pth
