#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"

swipl -q -g run_tests -t halt "$repo_root/t/intents.pl"
swipl -q -s "$repo_root/t/intents.pl" -g plunit_intents:emit_intent_corpus -t halt > "$test_root/intents.actual"
diff -u "$repo_root/t/fixtures/intents.expected" "$test_root/intents.actual"
pytest -q "$repo_root/t/test_intent_adapter.py"
