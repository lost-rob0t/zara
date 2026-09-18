#!/usr/bin/env bash
# Zara Android/Wear gate: semantic parity + JVM tests + stock secure-server interop + pinned native build + phone/Wear/Org debug APKs + secret inspection.
# Run via: nix develop .#android -c bash scripts/test-android.sh
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root/android"

: "${ANDROID_HOME:?ANDROID_HOME must be set by the nix android dev shell}"
: "${ANDROID_NDK_ROOT:?ANDROID_NDK_ROOT must be set by the pinned Android Nix toolchain}"
: "${ZARA_TREALLA_SOURCE_DIR:?ZARA_TREALLA_SOURCE_DIR must be set by the pinned Android Nix toolchain}"

bash "$repo_root/scripts/test-pair-android-qr.sh"
bash "$repo_root/scripts/test-android-semantic-parity.sh"

# One Android Org semantics implementation only. Focused apps may consume it,
# but must not fork the parser/task model under their own source trees.
duplicate_org_semantics="$(
  grep -R -n -E '^[[:space:]]*(object[[:space:]]+OrgParser|data[[:space:]]+class[[:space:]]+OrgTask)'     "$repo_root/android"     --include='*.kt'     --exclude-dir=build     | grep -v '/org-core/' || true
)"
if [[ -n "$duplicate_org_semantics" ]]; then
  echo "duplicate Android Org parser/task semantics found outside :org-core" >&2
  printf '%s\n' "$duplicate_org_semantics" >&2
  exit 1
fi

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

nix develop "$repo_root" -c env   PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"   python3 "$repo_root/android/integration/stock_zara_server_fixture.py"   --fixture-file "$interop_fixture" <&9 >"$interop_log" 2>&1 &
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

if ! gradle --no-daemon   :app:testDebugUnitTest   :shared-ui:testDebugUnitTest   :org-core:testDebugUnitTest   :org-storage:testDebugUnitTest   :org-app:testDebugUnitTest   :org-todo:testDebugUnitTest   :wear-app:testDebugUnitTest   :app:assembleDebug   :org-app:assembleDebug   :org-todo:assembleDebug   :wear-app:assembleDebug; then
  cat "$interop_log" >&2
  echo "stock ZaraServer Android/Wear/Org interop gate failed" >&2
  exit 1
fi

printf 'STOP\n' >&9
wait "$interop_pid"
interop_pid=""
unset ZARA_STOCK_FIXTURE

phone_apk="app/build/outputs/apk/debug/app-debug.apk"
org_apk="org-app/build/outputs/apk/debug/org-app-debug.apk"
org_todo_apk="org-todo/build/outputs/apk/debug/org-todo-debug.apk"
wear_apk="wear-app/build/outputs/apk/debug/wear-app-debug.apk"
test -f "$phone_apk"
test -f "$org_apk"
test -f "$org_todo_apk"
test -f "$wear_apk"

for apk in "$phone_apk" "$org_apk" "$org_todo_apk" "$wear_apk"; do
  if strings "$apk" | grep -Eq "BEGIN (RSA |EC |DSA |OPENSSH )?PRIVATE KEY|CURVE SECRET KEY|zara-server-secret|ZARA_CLIENT_SECRET"; then
    echo "APK secret-marker inspection FAILED: private/secret material found in $apk" >&2
    exit 1
  fi
done

echo "android/wear/org gate ok: $phone_apk $org_apk $org_todo_apk $wear_apk"
