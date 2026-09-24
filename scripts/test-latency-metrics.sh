#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
artifact_dir="${ARTIFACT_DIR:-$(mktemp -d)}"
mkdir -p "$artifact_dir"
source_sha="$(git -C "$repo_root" rev-parse HEAD)"

pytest -q "$repo_root/t/test_latency_metrics.py"
python "$repo_root/scripts/benchmark-voice.py" \
  --jsonl "$artifact_dir/voice-latency.jsonl" \
  --report "$artifact_dir/voice-latency-report.json"
python "$repo_root/scripts/benchmark-zmq-e2e.py" \
  --iterations 300 \
  --source-sha "$source_sha" \
  --gate \
  --output "$artifact_dir/zmq-e2e-report.json"
