#!/usr/bin/env bash
set -euo pipefail

serial="${1:-emulator-5554}"
source_sha="${2:?Source SHA is required}"
repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

code_apk="android/code-editor/build/outputs/apk/debug/code-editor-debug.apk"
phone_apk="android/app/build/outputs/apk/debug/app-debug.apk"
trealla_library_root="$repo_root/android/app/build/trealla"

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

# Exercise the real Android SQLiteOpenHelper migrations on the same emulator used
# for acceptance. The v2 fixture proves history survives creation of the symbolic
# projection table; the v3 fixture proves existing symbolic rows gain v4 policy
# columns fail-closed and remain durable across helper recreation.
ANDROID_SERIAL="$serial" ZARA_SOURCE_SHA="$source_sha" \
  ZARA_TREALLA_LIBRARY_ROOT="$trealla_library_root" \
  nix develop ./android -c bash -lc \
  'cd android && gradle :app:connectedDebugAndroidTest --no-daemon \
    -Pandroid.testInstrumentationRunnerArguments.class=ai.zara.app.history.PortableConversationMigrationInstrumentedTest,ai.zara.app.history.PortableConversationV3MigrationInstrumentedTest'

python android/integration/device_acceptance.py \
  --serial "$serial" \
  --source-sha "$source_sha" \
  --output android/app/build/reports/device
