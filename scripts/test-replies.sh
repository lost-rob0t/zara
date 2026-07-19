#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

mkdir -p "$test_root/bin"

cat > "$test_root/bin/reply-sink" <<'PY'
#!/usr/bin/env python3
import json
import os
import sys

with open(os.environ["ZARA_REPLY_LOG"], "a", encoding="utf-8") as log:
    json.dump([os.path.basename(sys.argv[0]), *sys.argv[1:]], log)
    log.write("\n")
PY

chmod +x "$test_root/bin/reply-sink"
for executable in notify-send fake-tts safe-launch; do
    ln -s "$test_root/bin/reply-sink" "$test_root/bin/$executable"
done

export PATH="$test_root/bin:$PATH"
export XDG_CONFIG_HOME="$test_root/config"
export ZARA_REPLY_LOG="$test_root/replies.jsonl"
export ZARA_REPLY_TTS_COMMAND="fake-tts"
export ZARA_RICH_REPLIES="0"

swipl -q -g run_tests -t halt "$repo_root/t/replies.pl"

swipl -q -g "
    use_module('$repo_root/modules/command_loop.pl'),
    use_module('$repo_root/kb/config.pl'),
    asserta(kb_config:app_mapping(reply_test_app, [\"safe-launch\"])),
    zara_hooks:acknowledge,
    command_loop:execute_resolved(open, [reply_test_app], Success),
    Success = command_result(success, open, [reply_test_app], none),
    command_loop:execute_resolved(no_such_intent, ['Ada Lovelace'], Failure),
    Failure = command_result(failure, no_such_intent, ['Ada Lovelace'], failed)
" -t halt

python3 - "$ZARA_REPLY_LOG" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as log:
    events = [json.loads(line) for line in log if line.strip()]

expected = [
    ["notify-send", "-u", "normal", "--", "Zara", "Okay."],
    ["fake-tts", "--", "Okay."],
    ["safe-launch"],
    ["notify-send", "-u", "normal", "--", "Zara", "Opened reply_test_app."],
    ["fake-tts", "--", "Opened reply_test_app."],
    ["notify-send", "-u", "normal", "--", "Zara", "I couldn't complete Ada Lovelace."],
    ["fake-tts", "--", "I couldn't complete Ada Lovelace."],
]

assert events == expected, (events, expected)
acknowledgement = events[0][-1].lower()
assert all(word not in acknowledgement for word in ("opened", "completed", "sent"))
PY
