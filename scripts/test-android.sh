#!/usr/bin/env bash
# Zara Android gate: semantic parity + JVM tests + stock secure-server interop + pinned native build + debug APK + secret inspection.
# Run via: nix develop .#android -c bash scripts/test-android.sh
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root/android"

: "${ANDROID_HOME:?ANDROID_HOME must be set by the nix android dev shell}"
: "${ANDROID_NDK_ROOT:?ANDROID_NDK_ROOT must be set by the pinned Android Nix toolchain}"
: "${ZARA_TREALLA_SOURCE_DIR:?ZARA_TREALLA_SOURCE_DIR must be set by the pinned Android Nix toolchain}"

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

nix develop "$repo_root" -c python3 "$repo_root/android/integration/stock_zara_server_fixture.py" \
  --fixture-file "$interop_fixture" <&9 >"$interop_log" 2>&1 &
interop_pid=$!

# The nested root Nix shell may be cold on Actions. This bound is only for
# environment/process readiness; protocol correctness remains event-driven.
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

gradle --no-daemon testDebugUnitTest assembleDebug

printf 'STOP\n' >&9
wait "$interop_pid"
interop_pid=""
unset ZARA_STOCK_FIXTURE

apk="app/build/outputs/apk/debug/app-debug.apk"
test -f "$apk"

if strings "$apk" | grep -Eq "BEGIN (RSA |EC |DSA |OPENSSH )?PRIVATE KEY|CURVE SECRET KEY|zara-server-secret|ZARA_CLIENT_SECRET"; then
  echo "APK secret-marker inspection FAILED: private/secret material found in $apk" >&2
  exit 1
fi

echo "android gate ok: $apk"
