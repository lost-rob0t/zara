#!/usr/bin/env bash
# ZARA-028: deterministic streaming TTS / persistent playback gate.
#
# Provider and audio fixtures are local/fake. No microphone, credentials,
# model download, or external network is required.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

pytest -q "$repo_root/t/test_daemon_tts_output.py"
pytest -q "$repo_root/t/test_tts_output_lifecycle.py"
pytest -q "$repo_root/t/test_wake_speaker.py"
