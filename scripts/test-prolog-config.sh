#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"
mkdir -p "$XDG_CONFIG_HOME/zarathushtra"

swipl -q \
  -s "$repo_root/t/prolog_config.pl" \
  -g "(run_tests(prolog_config) -> halt(0) ; halt(1))" \
  -t "halt(1)"

rm -rf "$XDG_CONFIG_HOME/zarathushtra"
mkdir -p "$XDG_CONFIG_HOME/zarathushtra"

# Hard semantic precondition for the recovery slice. This is intentionally
# independent of PlUnit reporting so a pre-implementation branch cannot
# false-green if the recovery unit is skipped or summarized incorrectly.
swipl -q \
  -s "$repo_root/kb/config.pl" \
  -g '(kb_config:search_engine("https://search.brave.com/search?q=~w") -> halt(0) ; halt(1))' \
  -t "halt(1)"

swipl -q \
  -s "$repo_root/t/prolog_config_recovery.pl" \
  -g "(run_tests(prolog_config_recovery) -> halt(0) ; halt(1))" \
  -t "halt(1)"

rm -rf "$XDG_CONFIG_HOME/zarathushtra"
mkdir -p "$XDG_CONFIG_HOME/zarathushtra"

# Trusted desktop executable configuration is intentionally a separate
# boundary from the fact-only server/provisioning configuration.
swipl -q \
  -s "$repo_root/t/exec_config.pl" \
  -g "(run_tests(exec_config) -> halt(0) ; halt(1))" \
  -t "halt(1)"

# Market-data provider normalization and user-defined multifile adapters are
# completely offline in this gate; CI never requires an API credential.
swipl -q \
  -s "$repo_root/t/market_data.pl" \
  -g "(run_tests(market_data) -> halt(0) ; halt(1))" \
  -t "halt(1)"
