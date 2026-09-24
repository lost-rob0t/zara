#!/usr/bin/env bash
# Zara Android/Wear gate: semantic parity + JVM tests + stock secure-server interop + pinned native build + phone/Code/Termux bridge/Wear debug APKs + secret inspection.
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

# The adversary key is an ephemeral CI fixture, never a product signing secret.
# Its only purpose is to prove Android's signature-level LOCAL_AI boundary rejects
# a differently signed package while the normal app + llm-serve lineage matches.
adversary_key_dir="$(mktemp -d)"
adversary_keystore="$adversary_key_dir/adversary.keystore"
cleanup_adversary_key() {
  rm -rf "$adversary_key_dir"
}
trap 'cleanup_interop; cleanup_recovery; cleanup_adversary_key' EXIT
keytool -genkeypair -noprompt \
  -keystore "$adversary_keystore" \
  -storepass android \
  -keypass android \
  -alias adversary \
  -dname "CN=Zara Local AI Adversary,O=Zara Test,C=US" \
  -keyalg RSA \
  -keysize 2048 \
  -validity 1 >/dev/null 2>&1
chmod 600 "$adversary_keystore"
export ZARA_ANDROID_ADVERSARY_KEYSTORE="$adversary_keystore"

gradle_log="$(mktemp)"
if ! gradle --no-daemon \
  :app:testDebugUnitTest \
  :shared-ui:testDebugUnitTest \
  :editor-core:testDebugUnitTest \
  :code-editor:testDebugUnitTest \
  :termux-bridge:testDebugUnitTest \
  :llm-serve:testDebugUnitTest \
  :wear-app:testDebugUnitTest \
  :wear-voice:testDebugUnitTest \
  :app:assembleDebug \
  :code-editor:assembleDebug \
  :termux-bridge:assembleDebug \
  :llm-serve:assembleDebug \
  :llm-serve:assembleAdversary \
  :wear-app:assembleDebug \
  :wear-voice:assembleDebug 2>&1 | tee "$gradle_log"; then
  diagnostics_dir="app/build/reports/semantic-parity"
  mkdir -p "$diagnostics_dir"
  tail -n 240 "$gradle_log" > "$diagnostics_dir/gradle-failure-tail.log"
  cp "$interop_log" "$diagnostics_dir/stock-zara-server.log"
  cp "$recovery_log" "$diagnostics_dir/remote-recovery-fixture.log" 2>/dev/null || true
  cat "$interop_log" >&2
  echo "stock ZaraServer Android/Wear/Code/Termux interop gate failed" >&2
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
llm_serve_apk="llm-serve/build/outputs/apk/debug/llm-serve-debug.apk"
llm_serve_adversary_apk="llm-serve/build/outputs/apk/adversary/llm-serve-adversary.apk"
wear_apk="wear-app/build/outputs/apk/debug/wear-app-debug.apk"
voice_apk="wear-voice/build/outputs/apk/debug/wear-voice-debug.apk"
test -f "$phone_apk"
test -f "$code_apk"
test -f "$termux_bridge_apk"
test -f "$llm_serve_apk"
test -f "$llm_serve_adversary_apk"
test -f "$wear_apk"
test -f "$voice_apk"

bash "$repo_root/scripts/check-android-apk-installable.sh" "$phone_apk" "ai.zara.app"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$code_apk" "ai.zara.code.editor"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$termux_bridge_apk" "ai.zara.termux.bridge"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$llm_serve_apk" "ai.zara.llmserve"
bash "$repo_root/scripts/check-android-apk-installable.sh" "$llm_serve_adversary_apk" "ai.zara.llmserve.adversary"

for apk in "$phone_apk" "$code_apk" "$termux_bridge_apk" "$llm_serve_apk" "$llm_serve_adversary_apk" "$wear_apk" "$voice_apk"; do
  if strings "$apk" | grep -Eq "BEGIN (RSA |EC |DSA |OPENSSH )?PRIVATE KEY|CURVE SECRET KEY|zara-server-secret|ZARA_CLIENT_SECRET"; then
    echo "APK secret-marker inspection FAILED: private/secret material found in $apk" >&2
    exit 1
  fi
done

aapt2="$ANDROID_HOME/build-tools/36.0.0/aapt2"
apksigner="$ANDROID_HOME/build-tools/36.0.0/apksigner"
if [[ ! -x "$aapt2" ]]; then
  echo "Android permission gate FAILED: pinned aapt2 not found at $aapt2" >&2
  exit 1
fi
if [[ ! -x "$apksigner" ]]; then
  echo "Android signature gate FAILED: pinned apksigner not found at $apksigner" >&2
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

llm_serve_permissions="$($aapt2 dump permissions "$llm_serve_apk")"
if ! grep -Fq "android.permission.INTERNET" <<<"$llm_serve_permissions"; then
  echo "LLM Serve permission gate FAILED: loopback HTTP service requires INTERNET" >&2
  exit 1
fi
if ! grep -Fq "ai.zara.app.permission.LOCAL_AI" <<<"$llm_serve_permissions"; then
  echo "LLM Serve permission gate FAILED: canonical LOCAL_AI permission missing" >&2
  exit 1
fi
adversary_permissions="$($aapt2 dump permissions "$llm_serve_adversary_apk")"
if ! grep -Fq "ai.zara.app.permission.LOCAL_AI" <<<"$adversary_permissions"; then
  echo "LLM Serve adversary gate FAILED: probe must request LOCAL_AI before Android can deny it" >&2
  exit 1
fi

certificate_sha256() {
  "$apksigner" verify --print-certs "$1" \
    | sed -n 's/^Signer #1 certificate SHA-256 digest: //p' \
    | head -n 1
}
phone_signer="$(certificate_sha256 "$phone_apk")"
llm_serve_signer="$(certificate_sha256 "$llm_serve_apk")"
adversary_signer="$(certificate_sha256 "$llm_serve_adversary_apk")"
if [[ -z "$phone_signer" || -z "$llm_serve_signer" || -z "$adversary_signer" ]]; then
  echo "Android signature gate FAILED: signer digest unavailable" >&2
  exit 1
fi
if [[ "$phone_signer" != "$llm_serve_signer" ]]; then
  echo "Android signature gate FAILED: Zara app and LLM Serve are not same-lineage test artifacts" >&2
  exit 1
fi
if [[ "$phone_signer" == "$adversary_signer" ]]; then
  echo "Android signature gate FAILED: adversary unexpectedly shares Zara signer" >&2
  exit 1
fi
signature_evidence="app/build/reports/semantic-parity/local-ai-signature-boundary.txt"
mkdir -p "$(dirname "$signature_evidence")"
{
  printf 'phone_signer_sha256=%s\n' "$phone_signer"
  printf 'llm_serve_signer_sha256=%s\n' "$llm_serve_signer"
  printf 'adversary_signer_sha256=%s\n' "$adversary_signer"
  printf 'same_lineage=true\n'
  printf 'adversary_distinct=true\n'
} > "$signature_evidence"

echo "android/wear/code/termux/llm-serve gate ok: $phone_apk $code_apk $termux_bridge_apk $llm_serve_apk $llm_serve_adversary_apk $wear_apk $voice_apk"
