#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
for required in python swipl node; do
  command -v "$required" >/dev/null || { echo "ZARA-VERIFY/1 BLOCKED: missing $required" >&2; exit 2; }
done

ARTIFACT_DIR="${ZARA_VERIFY_ARTIFACT_DIR:-.artifacts/zara-verify}"
case "$ARTIFACT_DIR" in
  ''|/|.) echo "ZARA-VERIFY/1 BLOCKED: unsafe artifact directory" >&2; exit 2 ;;
esac
rm -rf -- "$ARTIFACT_DIR"
mkdir -p -- "$ARTIFACT_DIR"

python -m pytest -q -o faulthandler_timeout=15 \
  --junit-xml="$ARTIFACT_DIR/python-junit.xml" \
  t/test_zara_verify_runner.py t/test_zara_verify_base_identity.py \
  t/test_zara_verify_mutation_epoch.py t/test_zara_verify_provider_accounting.py \
  t/test_zara_verifier_expert.py t/test_zara_verifier_registry.py \
  t/test_zara_verify_schema.py \
  2>&1 | tee "$ARTIFACT_DIR/python.log"
swipl -q -f none -s t/zara_verify.pl -g '(run_tests -> halt(0); halt(1))' -t 'halt(1)' \
  2>&1 | tee "$ARTIFACT_DIR/prolog.log"
node --test t/test_opencode_zara_verify.mjs \
  2>&1 | tee "$ARTIFACT_DIR/node.tap"
