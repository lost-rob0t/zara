#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

if [ -z "${OPENROUTER_API_KEY:-${ZARA_LLM_API_KEY:-}}" ]; then
    echo "Set OPENROUTER_API_KEY or ZARA_LLM_API_KEY for the live GLM benchmark" >&2
    exit 1
fi

model="${ZARA_BENCH_GLM_MODEL:-z-ai/glm-5.3-flash}"
python -m zara.bench \
    --provider openrouter \
    --model "$model" \
    --workers 10 \
    bench benchmarks/zara-glm-smoke.jsonl
