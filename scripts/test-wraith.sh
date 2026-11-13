#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

swipl -q \
  -s "$repo_root/t/wraith.pl" \
  -g "(run_tests(wraith) -> halt(0) ; halt(1))" \
  -t "halt(1)"
