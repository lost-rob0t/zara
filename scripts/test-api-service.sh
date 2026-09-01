#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"
mkdir -p "$XDG_CONFIG_HOME/zarathushtra"

swipl -q -g run_tests -t halt "$repo_root/t/api_service.pl"

python -m pytest -q \
  "$repo_root/t/test_api_service.py" \
  "$repo_root/t/test_host_api_service.py"
