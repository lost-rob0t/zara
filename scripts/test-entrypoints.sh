#!/usr/bin/env bash
set -euo pipefail

python -m compileall -q zara scripts
python -c 'import zara.wake'
python -m zara --help
