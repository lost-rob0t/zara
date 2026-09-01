#!/usr/bin/env bash
set -euo pipefail

python -m pytest -q \
  t/test_context_management.py \
  t/test_skill_registry.py \
  t/test_mcp_agent_integration.py
