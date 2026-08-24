#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

python -m pytest -q -o faulthandler_timeout=15 \
  t/test_runtime_tool_approval.py \
  t/test_config.py::test_default_config_is_valid_toml \
  t/test_config.py::test_tool_approval_policy_is_bounded_and_validated
