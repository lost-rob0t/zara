#!/usr/bin/env bash
set -euo pipefail

# Live OpenRouter smoke test for the Python LLM client.
#
# Requires a real credential; this is never part of the deterministic gate.
#
# Run with:
#   export OPENROUTER_API_KEY='sk-or-...'
#   nix develop -c bash scripts/test-openrouter-live.sh
#
# Optional environment:
#   ZARA_LLM_LIVE_MODEL   OpenRouter model id (default: openrouter/free)

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

if [ -z "${OPENROUTER_API_KEY:-}" ]; then
    echo "OPENROUTER_API_KEY is not set; export it before running this live smoke" >&2
    exit 1
fi

test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT
export XDG_CONFIG_HOME="$test_root/config"
mkdir -p "$XDG_CONFIG_HOME/zarathushtra"

python - "$repo_root" <<'PY'
import asyncio
import os
import sys

from zara.llm import LLMClient


async def main() -> None:
    model = os.getenv("ZARA_LLM_LIVE_MODEL") or None
    client = LLMClient(
        provider="openrouter",
        model=model,
        endpoint=None,
        connect_timeout=10.0,
        read_timeout=60.0,
        total_timeout=90.0,
        max_retries=2,
    )
    result = await client.query_async(
        "Reply with exactly one word: acknowledged",
        system_prompt="You are a smoke test. Obey the instruction exactly.",
        max_tokens=512,
    )
    await client.close()

    print(f"provider={result.provider} model={result.model} attempts={result.attempts}")
    if not result.success:
        print(f"FAIL: {result.error_type}: {result.error}", file=sys.stderr)
        raise SystemExit(1)
    print(f"response={result.text!r}")
    if not result.text.strip():
        print("FAIL: empty response", file=sys.stderr)
        raise SystemExit(1)


asyncio.run(main())
PY

echo "PASS: OpenRouter Python client live round trip"
