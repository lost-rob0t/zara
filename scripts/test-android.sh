#!/usr/bin/env bash
# Zara Android/Wear gate: semantic parity + JVM tests + stock secure-server interop + pinned native build + phone/Code/Termux bridge/Org/Wear debug APKs + secret inspection.
# Run via: nix develop ./android -c bash scripts/test-android.sh
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root/android"

: "${ANDROID_HOME:?ANDROID_HOME must be set by the nix android dev shell}"
: "${ANDROID_NDK_ROOT:?ANDROID_NDK_ROOT must be set by the pinned Android Nix toolchain}"
: "${ZARA_TREALLA_SOURCE_DIR:?ZARA_TREALLA_SOURCE_DIR must be set by the pinned Android Nix toolchain}"

bash "$repo_root/scripts/test-pair-android-qr.sh"
bash "$repo_root/scripts/test-android-semantic-parity.sh"

export ZARA_TREALLA_LIBRARY_ROOT="$PWD/app/build/trealla"
bash ./build-trealla.sh

interop_dir="$(mktemp -d)"
interop_fixture="$interop_dir/fixture.env"
interop_control="$interop_dir/control.fifo"
interop_log="$interop_dir/server.log"
mkfifo "$interop_control"
interop_pid=""
exec 9<>"$interop_control"
cleanup_interop() {
  if [[ -n "$interop_pid" ]] && kill -0 "$interop_pid" 2>/dev/null; then
    printf 'STOP\n' >&9 || true
    wait "$interop_pid" || true
  fi
  exec 9>&- || true
  exec 9<&- || true
  rm -rf "$interop_dir"
}
trap cleanup_interop EXIT

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
    echo "stock ZaraServer Android interop fixture exited before readiness" >&2
    exit 1
  fi
  sleep 0.05
done
if [[ ! -f "$interop_fixture" ]] || ! grep -qx 'READY' "$interop_log"; then
  cat "$interop_log" >&2
  echo "stock ZaraServer Android interop fixture did not become ready" >&2
  exit 1
fi
chmod 600 "$interop_fixture"
export ZARA_STOCK_FIXTURE="$interop_fixture"

recovery_dir="$(mktemp -d)"
recovery_fixture="$recovery_dir/fixture.env"
recovery_log="$recovery_dir/server.log"
recovery_pid=""
cleanup_recovery() {
  if [[ -n "$recovery_pid" ]] && kill -0 "$recovery_pid" 2>/dev/null; then
    kill "$recovery_pid" 2>/dev/null || true
    wait "$recovery_pid" 2>/dev/null || true
  fi
  rm -rf "$recovery_dir"
}
trap 'cleanup_interop; cleanup_recovery' EXIT

nix develop "$repo_root" -c env \
  PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}" \
  python3 "$repo_root/android/integration/remote_recovery_fixture.py" \
  --fixture-file "$recovery_fixture" </dev/null >"$recovery_log" 2>&1 &
recovery_pid=$!

for _ in $(seq 1 1200); do
  if [[ -f "$recovery_fixture" ]] && grep -qx 'READY' "$recovery_log"; then
    break
  fi
  if ! kill -0 "$recovery_pid" 2>/dev/null; then
    cat "$recovery_log" >&2
    echo "remote recovery fixture exited before readiness" >&2
    exit 1
  fi
  sleep 0.05
done
if [[ ! -f "$recovery_fixture" ]] || ! grep -qx 'READY' "$recovery_log"; then
  cat "$recovery_log" >&2
  echo "remote recovery fixture did not become ready" >&2
  exit 1
fi
chmod 600 "$recovery_fixture"
export ZARA_RECOVERY_FIXTURE="$recovery_fixture"

gradle_log="$(mktemp)"
if ! gradle --no-daemon \
  :app:testDebugUnitTest \
  :shared-ui:testDebugUnitTest \
  :editor-core:testDebugUnitTest \
  :code-editor:testDebugUnitTest \
  :termux-bridge:testDebugUnitTest \
  :org-core:testDebugUnitTest \
  :org-storage:testDebugUnitTest \
  :org-sync-core:testDebugUnitTest \
  :org-surfaces:testDebugUnitTest \
  :org-app:testDebugUnitTest \
  :org-editor:testDebugUnitTest \
  :org-todo:testDebugUnitTest \
  :org-reminder:testDebugUnitTest \
  :org-timer:testDebugUnitTest \
  :org-roam:testDebugUnitTest \
  :org-graph:testDebugUnitTest \
  :org-home:testDebugUnitTest \
  :wear-app:testDebugUnitTest \
  :wear-voice:testDebugUnitTest \
  :app:assembleDebug \
  :code-editor:assembleDebug \
  :termux-bridge:assembleDebug \
  :org-app:assembleDebug \
  :org-editor:assembleDebug \
  :org-todo:assembleDebug \
  :org-reminder:assembleDebug \
  :org-timer:assembleDebug \
  :org-roam:assembleDebug \
  :org-graph:assembleDebug \
  :org-home:assembleDebug \
  :wear-app:assembleDebug \
  :wear-voice:assembleDebug 2>&1 | tee "$gradle_log"; then
  diagnostics_dir="app/build/reports/semantic-parity"
  mkdir -p "$diagnostics_dir"
  tail -n 240 "$gradle_log" > "$diagnostics_dir/gradle-failure-tail.log"
  cp "$interop_log" "$diagnostics_dir/stock-zara-server.log"
  cp "$recovery_log" "$diagnostics_dir/remote-recovery-fixture.log" 2>/dev/null || true
  cat "$interop_log" >&2
  echo "stock ZaraServer Android/Wear/Code/Termux/Org interop gate failed" >&2
  exit 1
fi
rm -f "$gradle_log"

printf 'STOP\n' >&9
wait "$interop_pid"
interop_pid=""
unset ZARA_STOCK_FIXTURE

kill "$recovery_pid" 2>/dev/null || true
wait "$recovery_pid" 2>/dev/null || true
recovery_pid=""
unset ZARA_RECOVERY_FIXTURE

phone_apk="app/build/outputs/apk/debug/app-debug.apk"
code_apk="code-editor/build/outputs/apk/debug/code-editor-debug.apk"
termux_bridge_apk="termux-bridge/build/outputs/apk/debug/termux-bridge-debug.apk"
org_org_app_apk="org-app/build/outputs/apk/debug/org-app-debug.apk"
org_org_editor_apk="org-editor/build/outputs/apk/debug/org-editor-debug.apk"
org_org_todo_apk="org-todo/build/outputs/apk/debug/org-todo-debug.apk"
org_org_reminder_apk="org-reminder/build/outputs/apk/debug/org-reminder-debug.apk"
org_org_timer_apk="org-timer/build/outputs/apk/debug/org-timer-debug.apk"
org_org_roam_apk="org-roam/build/outputs/apk/debug/org-roam-debug.apk"
org_org_graph_apk="org-graph/build/outputs/apk/debug/org-graph-debug.apk"
org_org_home_apk="org-home/build/outputs/apk/debug/org-home-debug.apk"
wear_apk="wear-app/build/outputs/apk/debug/wear-app-debug.apk"
voice_apk="wear-voice/build/outputs/apk/debug/wear-voice-debug.apk"
test -f "$phone_apk"
test -f "$code_apk"
test -f "$termux_bridge_apk"
test -f "$org_org_app_apk"
test -f "$org_org_editor_apk"
test -f "$org_org_todo_apk"
test -f "$org_org_reminder_apk"
test -f "$org_org_timer_apk"
test -f "$org_org_roam_apk"
test -f "$org_org_graph_apk"
test -f "$org_org_home_apk"
test -f "$wear_apk"
test -f "$voice_apk"

bash "$repo_root/scripts/check-android-apk-installable.sh" "$phone_apk" "ai.zara.app"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$code_apk" "ai.zara.code.editor"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$termux_bridge_apk" "ai.zara.termux.bridge"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$org_org_app_apk" "ai.zara.org.app"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$org_org_editor_apk" "ai.zara.org.editor"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$org_org_todo_apk" "ai.zara.org.todo"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$org_org_reminder_apk" "ai.zara.org.reminder"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$org_org_timer_apk" "ai.zara.org.timer"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$org_org_roam_apk" "ai.zara.org.roam"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$org_org_graph_apk" "ai.zara.org.graph"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$org_org_home_apk" "ai.zara.org.home"

for apk in "$phone_apk" "$code_apk" "$termux_bridge_apk" \
  "$org_org_app_apk" "$org_org_editor_apk" "$org_org_todo_apk" "$org_org_reminder_apk" \
  "$org_org_timer_apk" "$org_org_roam_apk" "$org_org_graph_apk" "$org_org_home_apk" \
  "$wear_apk" "$voice_apk"; do
  if strings "$apk" | grep -Eq "BEGIN (RSA |EC |DSA |OPENSSH )?PRIVATE KEY|CURVE SECRET KEY|zara-server-secret|ZARA_CLIENT_SECRET"; then
    echo "APK secret-marker inspection FAILED: private/secret material found in $apk" >&2
    exit 1
  fi
done

aapt2="$ANDROID_HOME/build-tools/36.0.0/aapt2"
if [[ ! -x "$aapt2" ]]; then
  echo "Android permission gate FAILED: pinned aapt2 not found at $aapt2" >&2
  exit 1
fi
termux_bridge_permissions="$($aapt2 dump permissions "$termux_bridge_apk")"
if ! grep -Fq "com.termux.permission.RUN_COMMAND" <<<"$termux_bridge_permissions"; then
  echo "Termux bridge permission gate FAILED: RUN_COMMAND permission missing" >&2
  exit 1
fi
if grep -Fq "android.permission.INTERNET" <<<"$termux_bridge_permissions"; then
  echo "Termux bridge permission gate FAILED: foundation APK unexpectedly requests INTERNET" >&2
  exit 1
fi
voice_permissions="$($aapt2 dump permissions "$voice_apk")"
if grep -Fq "android.permission.INTERNET" <<<"$voice_permissions"; then
  echo "Wear Voice permission gate FAILED: focused strict-local APK requests INTERNET" >&2
  exit 1
fi

echo "android/wear/code/termux/org-fleet gate ok: $phone_apk $code_apk $termux_bridge_apk $org_org_app_apk $org_org_editor_apk $org_org_todo_apk $org_org_reminder_apk $org_org_timer_apk $org_org_roam_apk $org_org_graph_apk $org_org_home_apk $wear_apk $voice_apk"
