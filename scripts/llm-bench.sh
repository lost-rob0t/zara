#!/usr/bin/env bash
# Manual LLM bench matrix over Zara's OpenAI-compatible backend.
#
# Invoked by hand during reviews or performance work; never wired to push or
# cron triggers. One full run costs well under one US cent (request budget is
# enforced in scripts/llm_bench.py).
#
#   nix develop -c bash scripts/llm-bench.sh
#
# Credentials: NANOGPT_API_KEY in the environment, or the pi-agent key file
# at ~/.config/nanogpt/api-key (never committed).
#
# Output: artifacts/llm-bench/<timestamp>/{results.json,REPORT.md}

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

key_file="${NANOGPT_KEY_FILE:-$HOME/.config/nanogpt/api-key}"
if [ ! -r "$key_file" ]; then
    echo "llm-bench: no readable api key at $key_file (set NANOGPT_KEY_FILE or NANOGPT_API_KEY)" >&2
    exit 2
fi

python3 "$repo_root/scripts/llm_bench.py" --api-key-file "$key_file" "$@"
