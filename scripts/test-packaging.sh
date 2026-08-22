#!/usr/bin/env bash
set -euo pipefail

# Verify the Python packaging contract from ZARA-019:
#   - setup.py builds a wheel
#   - the wheel ships main.pl, kb/, and modules/ as data_files under share/zarathushtra/
#   - supported console scripts, including zara and zara-server, are declared
#   - the runtime dependencies cover the import surface
#
# Run with: nix develop -c scripts/test-packaging.sh

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

pytest -q "$repo_root/t/test_packaging.py"
