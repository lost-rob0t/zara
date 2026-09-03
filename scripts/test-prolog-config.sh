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
  -g "( current_test_unit(prolog_config, _), current_test_unit(prolog_config_recovery, _), current_predicate(config_loader:user_local_config_path/1), run_tests(prolog_config, [summary(BaseSummary)]), get_dict(total, BaseSummary, BaseTotal), BaseTotal > 0, get_dict(failed, BaseSummary, BaseFailed), BaseFailed =:= 0, get_dict(timeout, BaseSummary, BaseTimedOut), BaseTimedOut =:= 0, run_tests(prolog_config_recovery, [summary(RecoverySummary)]), get_dict(total, RecoverySummary, RecoveryTotal), RecoveryTotal > 0, get_dict(failed, RecoverySummary, RecoveryFailed), RecoveryFailed =:= 0, get_dict(timeout, RecoverySummary, RecoveryTimedOut), RecoveryTimedOut =:= 0 -> halt(0) ; halt(1) )"
