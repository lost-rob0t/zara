#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

python -m pytest -q \
  t/test_principal_conversation_isolation.py \
  t/test_principal_memory_isolation.py \
  t/test_principal_runtime_isolation.py
