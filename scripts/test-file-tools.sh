#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"
export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"

pytest -q "$repo_root/t/test_file_tools.py" "$repo_root/t/test_config.py"
