#!/usr/bin/env bash
set -euo pipefail

python -m pytest -q \
  t/test_cli_context_acquisition.py \
  t/test_context*.py \
  t/test_skill*.py \
  t/test_builtin_skill*.py \
  t/test_agent_context_integration.py \
  t/test_task_turn_context.py \
  t/test_mcp_agent_integration.py
