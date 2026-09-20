#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
for required in python swipl node; do
  command -v "$required" >/dev/null || { echo "ZARA-VERIFY/1 BLOCKED: missing $required" >&2; exit 2; }
done
python -m pytest -q -o faulthandler_timeout=15 \
  t/test_zara_verify_runner.py t/test_zara_verifier_expert.py t/test_zara_verifier_registry.py t/test_zara_verify_schema.py
swipl -q -f none -s t/zara_verify.pl -g '(run_tests -> halt(0); halt(1))' -t 'halt(1)'
node --test t/test_opencode_zara_verify.mjs
