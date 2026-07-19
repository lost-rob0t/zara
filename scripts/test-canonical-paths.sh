#!/usr/bin/env bash
# Verifies that all wrappers and scripts delegate to canonical modules.
#
# Run with: nix develop -c scripts/test-canonical-paths.sh
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

pytest -q "$repo_root/t/test_canonical_paths.py"
