#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

swipl -q \
  -s t/philosophy_profiles.pl \
  -g "(run_tests(philosophy_profiles) -> halt(0) ; halt(1))" \
  -t "halt(1)"

python -m pytest -q t/test_agent_profiles.py
