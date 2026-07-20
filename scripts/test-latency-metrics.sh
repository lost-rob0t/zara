#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
artifact_dir="${ARTIFACT_DIR:-$(mktemp -d)}"
mkdir -p "$artifact_dir"

pytest -q "$repo_root/t/test_latency_metrics.py"
python "$repo_root/scripts/benchmark-voice.py" \
  --jsonl "$artifact_dir/voice-latency.jsonl" \
  --report "$artifact_dir/voice-latency-report.json"
