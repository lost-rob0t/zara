#!/usr/bin/env bash
# Zara Android/Wear gate: semantic parity + JVM tests + stock secure-server interop + pinned native build + phone/Code/Wear debug APKs + secret inspection.
# Run via: nix develop .#android -c bash scripts/test-android.sh
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

gradle_log="$(mktemp)"
if ! gradle --no-daemon \
  :app:testDebugUnitTest \
  :shared-ui:testDebugUnitTest \
  :editor-core:testDebugUnitTest \
  :code-editor:testDebugUnitTest \
  :wear-app:testDebugUnitTest \
  :wear-voice:testDebugUnitTest \
  :app:assembleDebug \
  :code-editor:assembleDebug \
  :wear-app:assembleDebug \
  :wear-voice:assembleDebug 2>&1 | tee "$gradle_log"; then
  diagnostics_dir="app/build/reports/semantic-parity"
  mkdir -p "$diagnostics_dir"
  tail -n 240 "$gradle_log" > "$diagnostics_dir/gradle-failure-tail.log"
  cp "$interop_log" "$diagnostics_dir/stock-zara-server.log"
  cat "$interop_log" >&2
  echo "stock ZaraServer Android/Wear/Code interop gate failed" >&2
  exit 1
fi
rm -f "$gradle_log"

printf 'STOP\n' >&9
wait "$interop_pid"
interop_pid=""
unset ZARA_STOCK_FIXTURE

phone_apk="app/build/outputs/apk/debug/app-debug.apk"
code_apk="code-editor/build/outputs/apk/debug/code-editor-debug.apk"
wear_apk="wear-app/build/outputs/apk/debug/wear-app-debug.apk"
voice_apk="wear-voice/build/outputs/apk/debug/wear-voice-debug.apk"
test -f "$phone_apk"
test -f "$code_apk"
test -f "$wear_apk"
test -f "$voice_apk"

bash "$repo_root/scripts/check-android-apk-installable.sh" "$phone_apk" "ai.zara.app"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$code_apk" "ai.zara.code.editor"

for apk in "$phone_apk" "$code_apk" "$wear_apk" "$voice_apk"; do
  if strings "$apk" | grep -Eq "BEGIN (RSA |EC |DSA |OPENSSH )?PRIVATE KEY|CURVE SECRET KEY|zara-server-secret|ZARA_CLIENT_SECRET"; then
    echo "APK secret-marker inspection FAILED: private/secret material found in $apk" >&2
    exit 1
  fi
done

aapt2="$ANDROID_HOME/build-tools/36.0.0/aapt2"
if [[ ! -x "$aapt2" ]]; then
  echo "Wear Voice permission gate FAILED: pinned aapt2 not found at $aapt2" >&2
  exit 1
fi
voice_permissions="$($aapt2 dump permissions "$voice_apk")"
if grep -Fq "android.permission.INTERNET" <<<"$voice_permissions"; then
  echo "Wear Voice permission gate FAILED: focused strict-local APK requests INTERNET" >&2
  exit 1
fi

echo "android/wear/code gate ok: $phone_apk $code_apk $wear_apk $voice_apk"
