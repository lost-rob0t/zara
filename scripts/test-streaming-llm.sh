#!/usr/bin/env bash
# ZARA-027: streaming LLM/agent output gate.
#
# Runs the sentence chunker suite and the streaming acceptance suite. All
# provider fixtures are local fake HTTP servers; no credentials or network
# access are required.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

pytest -q "$repo_root/t/test_sentence_chunker.py"
pytest -q "$repo_root/t/test_llm_streaming.py"
