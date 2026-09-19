#!/usr/bin/env bash
set -euo pipefail

serial="${1:-emulator-5554}"
source_sha="${2:?Source SHA is required}"
repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

code_apk="android/code-editor/build/outputs/apk/debug/code-editor-debug.apk"
phone_apk="android/app/build/outputs/apk/debug/app-debug.apk"

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
python android/integration/device_acceptance.py \
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
  mkdir -p android/app/build/reports/device
  if [[ -f "$interop_log" ]]; then
    cp "$interop_log" android/app/build/reports/device/remote-stock-server.log || true
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
  --output "$repo_root/android/app/build/reports/device"

printf 'STOP\n' >&9
wait "$interop_pid"
interop_pid=""
adb -s "$serial" reverse --remove "tcp:$reverse_port"
reverse_port=""
