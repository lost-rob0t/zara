#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"
export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"

python - <<'PY'
import tomllib

from zara.config import ZaraConfig
from zara.tts.engine import TTSEngine

config = ZaraConfig()
with config.config_file.open("rb") as config_file:
    tomllib.load(config_file)

config.config_file.write_text('[tts]\nprovider = "qwen"\n\n[wake]\nthreshold = 0.75\n')
config.reload()
engine = TTSEngine(config.get("tts", "provider"), config._config)

assert config.get("wake", "threshold") == 0.75
assert config.get("tts", "provider") == "qwen3"
assert engine.provider == "qwen3"
PY

pytest -q "$repo_root/t/test_config.py"
