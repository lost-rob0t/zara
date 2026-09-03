#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"
mkdir -p "$XDG_CONFIG_HOME/zarathushtra"

swipl -q \
  -s "$repo_root/t/prolog_config.pl" \
  -s "$repo_root/t/prolog_config_recovery.pl" \
  -g "( current_test_unit(prolog_config, _), current_test_unit(prolog_config_recovery, _), current_predicate(config_loader:user_local_config_path/1), run_tests([prolog_config, prolog_config_recovery], [summary(Summary)]), get_dict(total, Summary, Total), Total > 0, get_dict(failed, Summary, Failed), Failed =:= 0, get_dict(timeout, Summary, TimedOut), TimedOut =:= 0 -> halt(0) ; halt(1) )"
