#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

swipl -q \
  -g "load_files('t/org_browser_config.pl', [silent(true)]), load_files('t/org_roam_memory_projection.pl', [silent(true)]), run_tests([org_browser_config, org_roam_memory_projection]), halt" \
  -t "halt(1)"
