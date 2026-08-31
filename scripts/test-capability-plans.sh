#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"

swipl -q -s "$repo_root/t/capability_plans.pl" -g run_tests -t halt
pytest -q \
    "$repo_root/t/test_capability_plans.py" \
    "$repo_root/t/test_plans.py" \
    "$repo_root/t/test_plan_executor.py"
