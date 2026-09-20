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

test -f "$trealla_library_root/arm64-v8a/libtrealla.a"
test -f "$trealla_library_root/x86_64/libtrealla.a"

# Exercise the real Android SQLiteOpenHelper migrations, persisted-type fences,
# and restart cancellation fencing on the same emulator used for acceptance.
# The v2 fixture proves history plus a new zero-call projection survives
# migration/reopen. The v3 fixture proves fail-closed policy defaults can be
# replaced only by authoritative false/0 policy and that REAL/TEXT counter
# corruption stays rejected after recreation. The restart fixture proves a
# recovered streaming turn terminalizes both canonical history and its matching
# symbolic projection before any late completion/effect callback can land.
set +e
ANDROID_SERIAL="$serial" ZARA_SOURCE_SHA="$source_sha" \
  ZARA_TREALLA_LIBRARY_ROOT="$trealla_library_root" \
  nix develop ./android -c bash -lc \
  'cd android && gradle :app:connectedDebugAndroidTest --no-daemon \
    -Pandroid.testInstrumentationRunnerArguments.class=ai.zara.app.history.PortableConversationMigrationInstrumentedTest,ai.zara.app.history.PortableConversationV3MigrationInstrumentedTest,ai.zara.app.history.PortableConversationRestartFenceInstrumentedTest' \
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

python android/integration/device_acceptance.py \
  --serial "$serial" \
  --source-sha "$source_sha" \
  --output "$evidence_dir"
