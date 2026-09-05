#!/usr/bin/env bash
# Reproducible high-volume security fuzz + chaos soak.
#
# Usage:
#   nix develop -c bash scripts/security-fuzz-soak.sh
#   ZARA_SECURITY_FUZZ_SEEDS=1024 nix develop -c bash scripts/security-fuzz-soak.sh
#
# Every generated case is derived from its integer seed. If a soak fails, rerun
# the reported pytest parameter directly or rerun with the same seed corpus.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"
export ZARA_SECURITY_FUZZ_SEEDS="${ZARA_SECURITY_FUZZ_SEEDS:-256}"

case "$ZARA_SECURITY_FUZZ_SEEDS" in
  ''|*[!0-9]*)
    echo "ZARA_SECURITY_FUZZ_SEEDS must be an integer" >&2
    exit 2
    ;;
esac

if [ "$ZARA_SECURITY_FUZZ_SEEDS" -lt 1 ] || [ "$ZARA_SECURITY_FUZZ_SEEDS" -gt 4096 ]; then
  echo "ZARA_SECURITY_FUZZ_SEEDS must be between 1 and 4096" >&2
  exit 2
fi

python -m pytest -q -o faulthandler_timeout=15 \
  t/test_security_fuzz.py \
  t/test_security_chaos.py \
  t/test_production_secure_listener.py
