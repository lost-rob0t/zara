#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"
export ZARA_DICTATION_PIDFILE="$test_root/dictation.pid"
export ZARA_DICTATION_LOGFILE="$test_root/dictation.log"

python -m py_compile "$repo_root/zara/dictate.py" "$repo_root/scripts/zara_dictate.py"
swipl -q -g run_tests -t halt "$repo_root/t/dictation_lifecycle.pl"
