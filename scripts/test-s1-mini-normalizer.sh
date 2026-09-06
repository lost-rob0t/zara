#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

python -m pytest -q \
  t/test_s1_mini_normalizer.py \
  t/test_s1_mini_numeric_bounds.py
