#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

mkdir -p "$test_root/bin"
fake_executable="$test_root/bin/fake-executable"

cat > "$fake_executable" <<'PY'
#!/usr/bin/env python3
import json
import os
import sys
import time

name = os.path.basename(sys.argv[0])
with open(os.environ["ZARA_PROCESS_LOG"], "a", encoding="utf-8") as log:
    json.dump([name, *sys.argv[1:]], log, ensure_ascii=False)
    log.write("\n")

if name == "safe-dictate":
    time.sleep(30)
if name == "fail-launch":
    raise SystemExit(23)
PY

chmod +x "$fake_executable"
for executable in xdg-open notify-send safe-launch safe-dictate fail-launch; do
    ln -s "$fake_executable" "$test_root/bin/$executable"
done

export PATH="$test_root/bin:$PATH"
export XDG_CONFIG_HOME="$test_root/config"
export ZARA_DICTATION_PIDFILE="$test_root/dictation.pid"
export ZARA_DICTATION_LOGFILE="$test_root/dictation.log"
export ZARA_PROCESS_LOG="$test_root/process.jsonl"
export ZARA_PROCESS_MARKER="$test_root/injected-marker"

swipl -q -g run_tests -t halt "$repo_root/t/process_safety.pl"
test ! -e "$ZARA_PROCESS_MARKER"
