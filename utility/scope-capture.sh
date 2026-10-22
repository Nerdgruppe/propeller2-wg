#!/usr/bin/env bash

set -euo pipefail

# Optional diagnostic utility: Python strips framing and restores codec parameters.
python3 "$(dirname -- "${BASH_SOURCE[0]}")/scope-video.py" | \
    ffmpeg -hide_banner -loglevel warning \
        -probesize 32768 -analyzeduration 0 -flags low_delay \
        -f h264 -i pipe:0 \
        -vf "fps=2" \
        -f image2 -update 1 -atomic_writing 1 \
        -y /tmp/micsig.png
