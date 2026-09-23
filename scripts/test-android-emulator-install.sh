#!/usr/bin/env bash
set -euo pipefail

serial="${1:-emulator-5554}"
source_sha="${2:?Source SHA is required}"
repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

code_apk="android/code-editor/build/outputs/apk/debug/code-editor-debug.apk"
phone_apk="android/app/build/outputs/apk/debug/app-debug.apk"
phone_apk_sha256="$(sha256sum "$phone_apk" | awk '{print $1}')"
if [[ ! "$phone_apk_sha256" =~ ^[0-9a-f]{64}$ ]]; then
  echo "candidate phone APK did not produce a valid SHA-256" >&2
  exit 1
fi
trealla_library_root="$repo_root/android/app/build/trealla"
evidence_dir="android/app/build/reports/device"
instrumentation_log="$evidence_dir/connected-debug-android-test.log"

# Create the always-uploaded evidence directory before any emulator action so a
# failed acceptance run still leaves exact-head diagnostics instead of an empty
# artifact slot. This is local CI evidence only; it is not a runtime fallback.
mkdir -p "$evidence_dir"
printf 'source_sha=%s\nserial=%s\napk_sha256=%s\n' \
  "$source_sha" "$serial" "$phone_apk_sha256" > "$evidence_dir/run-context.txt"

copy_connected_test_diagnostics() {
  local diagnostics_dir="$evidence_dir/instrumentation"
  local reports="android/app/build/reports/androidTests/connected"
  local results="android/app/build/outputs/androidTest-results/connected"

  rm -rf "$diagnostics_dir"
  mkdir -p "$diagnostics_dir"
  if [[ -d "$reports" ]]; then
    cp -R "$reports" "$diagnostics_dir/reports"
  fi
  if [[ -d "$results" ]]; then
    cp -R "$results" "$diagnostics_dir/results"
  fi
}

adb -s "$serial" wait-for-device
test "$(adb -s "$serial" get-state)" = "device"

adb -s "$serial" install -r "$code_apk"
adb -s "$serial" shell cmd package path ai.zara.code.editor | grep -Fq "package:"
code_start="$(adb -s "$serial" shell am start -W -n ai.zara.code.editor/.MainActivity)"
grep -Fq "Status: ok" <<<"$code_start"
sleep 1
adb -s "$serial" shell pidof ai.zara.code.editor >/dev/null
adb -s "$serial" shell am force-stop ai.zara.code.editor

adb -s "$serial" install -r "$phone_apk"
# GitHub's hosted Pixel image can leave its launcher process in an ANR dialog over
# an otherwise healthy Zara activity. Quiesce only that OS-owned package before
# acceptance instead of hiding global error dialogs or masking Zara failures.
if adb -s "$serial" shell pm path com.google.android.apps.nexuslauncher >/dev/null 2>&1; then
  adb -s "$serial" shell am force-stop com.google.android.apps.nexuslauncher
fi

test -f "$trealla_library_root/arm64-v8a/libtrealla.a"
test -f "$trealla_library_root/x86_64/libtrealla.a"

# Exercise the real Android SQLiteOpenHelper migrations, persisted-type fences,
# legacy symbolic-owner claim, and restart cancellation fencing on the same
# emulator used for acceptance. The v2 fixture proves history plus a new
# zero-call projection survives migration/reopen. The v3 fixture proves
# fail-closed policy defaults can be replaced only by authoritative false/0
# policy and that REAL/TEXT counter corruption stays rejected after recreation.
# The legacy-owner fixture proves numeric-UID projection state follows canonical
# local history to local:owner without losing clarification or zero-call ledgers.
# The restart fixture proves a recovered streaming turn terminalizes both
# canonical history and its matching symbolic projection before any late
# completion/effect callback can land.
set +e
ANDROID_SERIAL="$serial" ZARA_SOURCE_SHA="$source_sha" \
  ZARA_TREALLA_LIBRARY_ROOT="$trealla_library_root" \
  nix develop ./android -c bash -lc \
  'cd android && gradle :app:connectedDebugAndroidTest --no-daemon \
    -Pandroid.testInstrumentationRunnerArguments.class=ai.zara.app.history.PortableConversationMigrationInstrumentedTest,ai.zara.app.history.PortableConversationV3MigrationInstrumentedTest,ai.zara.app.history.PortableConversationRestartFenceInstrumentedTest,ai.zara.app.history.PortableConversationLegacyPrincipalInstrumentedTest' \
  2>&1 | tee "$instrumentation_log"
instrumentation_status=${PIPESTATUS[0]}
set -e

if (( instrumentation_status != 0 )); then
  copy_connected_test_diagnostics
  {
    printf 'stage=connectedDebugAndroidTest\n'
    printf 'exit_code=%s\n' "$instrumentation_status"
    printf 'source_sha=%s\n' "$source_sha"
    printf 'serial=%s\n' "$serial"
  } > "$evidence_dir/instrumentation-failure.txt"
  exit "$instrumentation_status"
fi

# Gradle's connected-test lifecycle owns the target package installation and may
# leave it removed after the instrumentation runner exits. Reinstall the exact
# already-built candidate before UI acceptance, then prove PackageManager sees
# that package. This is deterministic test setup, not a runtime fallback.
adb -s "$serial" install -r "$phone_apk"
adb -s "$serial" shell cmd package path ai.zara.app | grep -Fq "package:"

nix develop "$repo_root/android" -c \
  python3 "$repo_root/android/integration/device_acceptance.py" \
  --serial "$serial" \
  --source-sha "$source_sha" \
  --apk-sha256 "$phone_apk_sha256" \
  --output android/app/build/reports/device

visual_manifest="$repo_root/android/app/build/reports/device/manifest.json"
python3 - "$visual_manifest" "$repo_root/android/app/build/reports/device" "$source_sha" <<'PY'
import hashlib
import json
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET

manifest_path = Path(sys.argv[1])
evidence_dir = Path(sys.argv[2])
expected_source_sha = sys.argv[3]
data = json.loads(manifest_path.read_text(encoding="utf-8"))
if data.get("passed") is not True:
    raise SystemExit("visual Android acceptance manifest did not report passed=true")
if data.get("source_sha") != expected_source_sha:
    raise SystemExit("visual Android acceptance source SHA does not match exact candidate")
checks = data.get("visual_checks")
if not isinstance(checks, list):
    raise SystemExit("visual Android acceptance omitted visual_checks")
receipt = next(
    (item for item in checks if item.get("state") == "drawer-conversation-overflow"),
    None,
)
if receipt is None:
    raise SystemExit("visual Android acceptance omitted drawer-conversation-overflow receipt")
if receipt.get("source_sha") != expected_source_sha:
    raise SystemExit("overflow visual receipt is not bound to the exact source SHA")
if receipt.get("device_api") != data.get("device", {}).get("api"):
    raise SystemExit("overflow visual receipt device API differs from manifest device state")
if receipt.get("profile") != "default":
    raise SystemExit("overflow visual receipt was not captured in the default display profile")


def plain_int(value):
    return isinstance(value, int) and not isinstance(value, bool)


viewport = receipt.get("viewport")
if (
    not isinstance(viewport, list)
    or len(viewport) != 2
    or not all(plain_int(value) and value > 0 for value in viewport)
):
    raise SystemExit("overflow visual receipt omitted valid viewport")
viewport_width, viewport_height = viewport


def valid_rect(value):
    if (
        not isinstance(value, list)
        or len(value) != 4
        or not all(plain_int(coordinate) for coordinate in value)
    ):
        return False
    left, top, right, bottom = value
    return (
        0 <= left < right <= viewport_width
        and 0 <= top < bottom <= viewport_height
    )


trigger_bounds = receipt.get("trigger_bounds")
if not valid_rect(trigger_bounds):
    raise SystemExit("overflow visual receipt omitted valid trigger_bounds")

declared_action_union = receipt.get("action_union")
if not valid_rect(declared_action_union):
    raise SystemExit("overflow visual receipt omitted valid action_union")

expected_actions = {"Pin", "Unpin", "Rename", "Move to project"}
actions = receipt.get("actions")
if not isinstance(actions, list):
    raise SystemExit("overflow visual receipt omitted action evidence")
labels = {item.get("label") for item in actions}
if not {"Rename", "Move to project"}.issubset(labels) or not ({"Pin", "Unpin"} & labels):
    raise SystemExit(f"overflow visual receipt action set is incomplete: {sorted(labels)}")
if not labels.issubset(expected_actions):
    raise SystemExit(f"overflow visual receipt contains unexpected actions: {sorted(labels)}")

for file_key, hash_key in (
    ("screenshot_file", "screenshot_sha256"),
    ("text_twin_file", "text_twin_sha256"),
):
    file_name = receipt.get(file_key)
    expected_hash = receipt.get(hash_key)
    if not isinstance(file_name, str) or not file_name:
        raise SystemExit(f"overflow visual receipt omitted {file_key}")
    if not isinstance(expected_hash, str) or len(expected_hash) != 64:
        raise SystemExit(f"overflow visual receipt omitted valid {hash_key}")
    path = evidence_dir / file_name
    if not path.is_file():
        raise SystemExit(f"overflow visual evidence file is missing: {path}")
    actual_hash = hashlib.sha256(path.read_bytes()).hexdigest()
    if actual_hash != expected_hash:
        raise SystemExit(
            f"overflow visual evidence hash mismatch for {file_name}: "
            f"expected={expected_hash} actual={actual_hash}"
        )


def parse_text_twin(path: Path) -> list[dict[str, str | None]]:
    text = path.read_text(encoding="utf-8")
    if text.lstrip().startswith("<"):
        try:
            root = ET.fromstring(text)
        except ET.ParseError as error:
            raise SystemExit(f"overflow visual text twin is malformed XML: {error}") from error
        return [
            {
                "text": node.get("text"),
                "content_desc": node.get("content-desc"),
                "bounds": node.get("bounds"),
            }
            for node in root.iter("node")
        ]

    json_string = r'"(?:\\.|[^"\\])*"'
    node_pattern = re.compile(
        rf'^class={json_string} text=(?P<text>{json_string}) '
        rf'content_desc=(?P<content_desc>{json_string}) '
        r'enabled=\S+ clickable=\S+ selected=\S+ focused=\S+ '
        r'bounds=(?P<bounds>\[\d+,\d+\]\[\d+,\d+\])$'
    )
    nodes: list[dict[str, str | None]] = []
    for line in text.splitlines():
        match = node_pattern.fullmatch(line)
        if match is None:
            continue
        try:
            label_text = json.loads(match.group("text"))
            content_desc = json.loads(match.group("content_desc"))
        except json.JSONDecodeError as error:
            raise SystemExit(
                f"overflow visual normalized text twin contains invalid JSON string: {error}"
            ) from error
        nodes.append(
            {
                "text": label_text,
                "content_desc": content_desc,
                "bounds": match.group("bounds"),
            }
        )
    if not nodes:
        raise SystemExit("overflow visual normalized text twin contains no UI nodes")
    return nodes


text_twin_path = evidence_dir / receipt["text_twin_file"]
text_twin_nodes = parse_text_twin(text_twin_path)
action_rects = []
for action in actions:
    label = action.get("label")
    bounds = action.get("bounds")
    if not isinstance(label, str) or not label:
        raise SystemExit("overflow visual receipt contains an action without a label")
    if not isinstance(bounds, str) or not bounds:
        raise SystemExit(f"overflow visual receipt action omitted bounds: {label}")
    match = re.fullmatch(r"\[(\d+),(\d+)\]\[(\d+),(\d+)\]", bounds)
    if match is None:
        raise SystemExit(f"overflow visual receipt action omitted valid bounds: {label}")
    action_rect = [int(value) for value in match.groups()]
    if not valid_rect(action_rect):
        raise SystemExit(f"overflow visual receipt action bounds exceed viewport: {label}")
    action_rects.append(action_rect)
    for field, minimum in (
        ("content_inset_px", 1),
        ("luma_span", 18),
        ("occupied_luma_bins", 4),
    ):
        value = action.get(field)
        if not plain_int(value) or value < minimum:
            raise SystemExit(
                f"overflow visual receipt action omitted valid {field}: {label}"
            )
    matching_label_nodes = [
        node
        for node in text_twin_nodes
        if label in (node.get("text"), node.get("content_desc"))
    ]
    if not matching_label_nodes:
        raise SystemExit(f"overflow visual text twin is missing action: {label}")
    if not any(node.get("bounds") == bounds for node in matching_label_nodes):
        raise SystemExit(
            "overflow visual text twin action bounds differ from receipt: "
            f"{label} expected={bounds}"
        )

computed_action_union = [
    min(rect[0] for rect in action_rects),
    min(rect[1] for rect in action_rects),
    max(rect[2] for rect in action_rects),
    max(rect[3] for rect in action_rects),
]
if declared_action_union != computed_action_union:
    raise SystemExit("overflow visual receipt action_union differs from action bounds")

screenshots = data.get("screenshots")
if not isinstance(screenshots, list):
    raise SystemExit("visual Android acceptance omitted screenshots")
screenshot_entry = next(
    (item for item in screenshots if item.get("state") == "drawer-conversation-overflow"),
    None,
)
if screenshot_entry is None:
    raise SystemExit("visual Android acceptance omitted drawer-conversation-overflow screenshot")
if screenshot_entry.get("file") != receipt.get("screenshot_file"):
    raise SystemExit("overflow screenshot manifest entry differs from visual receipt")
if screenshot_entry.get("sha256") != receipt.get("screenshot_sha256"):
    raise SystemExit("overflow screenshot hash differs between manifest entry and visual receipt")
PY

# The visual acceptance above is intentionally broad. This second gate proves
# the installed APK's real Android Keystore -> CURVE -> JeroMQ -> ZARA/1 path
# against the stock Python Zara server and completes an actual remote text turn.
interop_dir="$(mktemp -d)"
interop_fixture="$interop_dir/fixture.env"
interop_control="$interop_dir/control.fifo"
interop_log="$interop_dir/server.log"
interop_pid=""
reverse_port=""
mkfifo "$interop_control"
exec 9<>"$interop_control"

cleanup_remote_acceptance() {
  status=$?
  mkdir -p "$evidence_dir"
  if [[ -f "$interop_log" ]]; then
    cp "$interop_log" "$evidence_dir/remote-stock-server.log" || true
  fi
  if [[ -n "$reverse_port" ]]; then
    adb -s "$serial" reverse --remove "tcp:$reverse_port" >/dev/null 2>&1 || true
  fi
  if [[ -n "$interop_pid" ]] && kill -0 "$interop_pid" 2>/dev/null; then
    printf 'STOP\n' >&9 || true
    wait "$interop_pid" || true
  fi
  exec 9>&- || true
  exec 9<&- || true
  rm -rf "$interop_dir"
  exit "$status"
}
trap cleanup_remote_acceptance EXIT

nix develop "$repo_root" -c env \
  PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}" \
  python3 "$repo_root/android/integration/stock_zara_server_fixture.py" \
  --fixture-file "$interop_fixture" <&9 >"$interop_log" 2>&1 &
interop_pid=$!

for _ in $(seq 1 1200); do
  if [[ -f "$interop_fixture" ]] && grep -qx 'READY' "$interop_log"; then
    break
  fi
  if ! kill -0 "$interop_pid" 2>/dev/null; then
    cat "$interop_log" >&2
    echo "stock ZaraServer exited before installed-APK remote acceptance" >&2
    exit 1
  fi
  sleep 0.05
done
if [[ ! -f "$interop_fixture" ]] || ! grep -qx 'READY' "$interop_log"; then
  cat "$interop_log" >&2
  echo "stock ZaraServer did not become ready for installed-APK remote acceptance" >&2
  exit 1
fi

endpoint="$(sed -n 's/^endpoint=//p' "$interop_fixture")"
reverse_port="${endpoint##*:}"
if [[ ! "$reverse_port" =~ ^[0-9]+$ ]]; then
  cat "$interop_fixture" >&2
  echo "stock ZaraServer fixture did not publish a numeric TCP port" >&2
  exit 1
fi
adb -s "$serial" reverse "tcp:$reverse_port" "tcp:$reverse_port"

nix develop "$repo_root" -c env \
  PYTHONPATH="$repo_root/android/integration:$repo_root${PYTHONPATH:+:$PYTHONPATH}" \
  python3 "$repo_root/android/integration/device_remote_acceptance.py" \
  --serial "$serial" \
  --fixture-file "$interop_fixture" \
  --output "$repo_root/$evidence_dir"

# A successful UI path is not enough: the acceptance contract requires current
# process diagnostics and logcat to be readable and free of Zara crash/ANR
# markers. Fail closed if evidence collection itself broke so CI cannot silently
# report green without inspecting the exercised app logs.
remote_manifest="$repo_root/android/app/build/reports/device/remote-manifest.json"
python3 - "$remote_manifest" <<'PY'
import json
from pathlib import Path
import sys

path = Path(sys.argv[1])
data = json.loads(path.read_text(encoding="utf-8"))
if data.get("passed") is not True:
    raise SystemExit("remote Android acceptance manifest did not report passed=true")
if data.get("app_diagnostics_failure") or data.get("logcat_failure"):
    raise SystemExit("remote Android acceptance could not inspect required app diagnostics/logcat")
if not data.get("app_diagnostics") or not data.get("logcat"):
    raise SystemExit("remote Android acceptance omitted required app diagnostics/logcat evidence")
fatal_markers = data.get("fatal_log_markers")
if not isinstance(fatal_markers, list):
    raise SystemExit("remote Android acceptance omitted fatal_log_markers inspection result")
if fatal_markers:
    raise SystemExit(f"remote Android acceptance found crash/ANR markers: {fatal_markers}")
PY

printf 'STOP\n' >&9
wait "$interop_pid"
interop_pid=""
adb -s "$serial" reverse --remove "tcp:$reverse_port"
reverse_port=""
