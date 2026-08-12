#!/usr/bin/env bash
set -euo pipefail

python -m compileall -q zara scripts
python -c 'import zara.wake; import zara.desktop.app; assert callable(zara.desktop.app.main)'
python -m zara --help | grep -q -- "--desktop"
