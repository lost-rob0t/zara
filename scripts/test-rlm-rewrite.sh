#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"
mkdir -p "$XDG_CONFIG_HOME/zarathushtra"

if [ -z "${ZARA_PROLOG_RLM_ROOT:-}" ]; then
    echo "ZARA_PROLOG_RLM_ROOT is not set; Prolog-RLM direct-mode rewrites cannot be tested" >&2
    exit 1
fi

if [ -z "${OPENROUTER_API_KEY:-}" ]; then
    export OPENROUTER_API_KEY="test-only-key"
fi

swipl -q -g run_tests -t halt "$repo_root/t/rlm_rewrite.pl"
