#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

base_ref=""
case "$#" in
  0)
    ;;
  2)
    if [[ "$1" != "--base-ref" || -z "$2" ]]; then
      echo "usage: $0 [--base-ref <git-ref>]" >&2
      exit 2
    fi
    base_ref="$2"
    ;;
  *)
    echo "usage: $0 [--base-ref <git-ref>]" >&2
    exit 2
    ;;
esac

export ARTIFACT_DIR="${ARTIFACT_DIR:-$repo_root/artifacts/coverage}"
mkdir -p "$ARTIFACT_DIR"

fixture_dir="$repo_root/t/fixtures/audio"
trap 'rm -rf "$fixture_dir"' EXIT
rm -rf "$fixture_dir"
python "$repo_root/scripts/generate-audio-fixtures.py" "$fixture_dir" >/dev/null

python -m pytest \
  -q \
  -o faulthandler_timeout=15 \
  --junit-xml="$ARTIFACT_DIR/junit.xml" \
  --cov=zara \
  --cov-branch \
  --cov-config="$repo_root/.coveragerc" \
  --cov-report=term-missing \
  --cov-report="json:$ARTIFACT_DIR/coverage.json" \
  --cov-report="xml:$ARTIFACT_DIR/coverage.xml" \
  --cov-report="html:$ARTIFACT_DIR/html" \
  t/

gate_args=(
  --coverage "$ARTIFACT_DIR/coverage.json"
  --policy "$repo_root/coverage-baseline.json"
)

if [[ -n "$base_ref" ]]; then
  gate_args+=(--base-ref "$base_ref")
fi

python "$repo_root/scripts/check-coverage-ratchet.py" "${gate_args[@]}"
