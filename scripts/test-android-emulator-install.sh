#!/usr/bin/env bash
set -euo pipefail

serial="${1:-emulator-5554}"
source_sha="${2:?Source SHA is required}"
repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

code_apk="android/code-editor/build/outputs/apk/debug/code-editor-debug.apk"
phone_apk="android/app/build/outputs/apk/debug/app-debug.apk"
trealla_library_root="$repo_root/android/app/build/trealla"
evidence_dir="android/app/build/reports/device"
instrumentation_log="$evidence_dir/connected-debug-android-test.log"

# Create the always-uploaded evidence directory before any emulator action so a
# failed acceptance run still leaves exact-head diagnostics instead of an empty
# artifact slot. This is local CI evidence only; it is not a runtime fallback.
mkdir -p "$evidence_dir"
printf 'source_sha=%s\nserial=%s\n' "$source_sha" "$serial" > "$evidence_dir/run-context.txt"

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
  --output android/app/build/reports/device

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
