#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

timeout 120s python -m pytest \
  -vv \
  -x \
  -o faulthandler_timeout=15 \
  t/test_zara1_tool_approvals.py
