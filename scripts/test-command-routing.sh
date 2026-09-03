#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"
export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"

swipl -q -g run_tests -t halt "$repo_root/t/commands.pl"
pytest -q "$repo_root/t/test_daemon_intent_router.py"
