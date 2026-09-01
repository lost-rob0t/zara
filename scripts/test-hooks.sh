#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

swipl -q \
  -g "load_files('t/zara_hooks_registry.pl', [silent(true)]), run_tests(zara_hooks_registry), halt" \
  -t "halt(1)"
