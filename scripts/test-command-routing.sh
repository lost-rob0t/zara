#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"

swipl -q -g run_tests -t halt "$repo_root/t/commands.pl"
pytest -q "$repo_root/t/test_command_routing.py"
