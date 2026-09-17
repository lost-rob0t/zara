#!/usr/bin/env bash
# Zara Android/Wear gate: semantic parity + JVM tests + stock secure-server interop + pinned native build + phone/Wear/watch-face debug APKs + secret inspection.
# Run via: nix develop .#android -c bash scripts/test-android.sh
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root/android"

: "${ANDROID_HOME:?ANDROID_HOME must be set by the nix android dev shell}"
: "${ANDROID_NDK_ROOT:?ANDROID_NDK_ROOT must be set by the pinned Android Nix toolchain}"
: "${ZARA_TREALLA_SOURCE_DIR:?ZARA_TREALLA_SOURCE_DIR must be set by the pinned Android Nix toolchain}"

bash "$repo_root/scripts/test-pair-android-qr.sh"
bash "$repo_root/scripts/test-android-semantic-parity.sh"

watchface_manifest="org-watchface/src/main/AndroidManifest.xml"
watchface_xml="org-watchface/src/main/res/raw/watchface.xml"
grep -q 'android:hasCode="false"' "$watchface_manifest"
grep -q 'com.google.wear.watchface.format.version' "$watchface_manifest"
grep -q '<WatchFace width="450" height="450">' "$watchface_xml"
slot_count="$(grep -c '<ComplicationSlot ' "$watchface_xml")"
if (( slot_count < 1 || slot_count > 8 )); then
  echo "WFF complication slot count must be in [1,8], got $slot_count" >&2
  exit 1
fi
if find org-watchface/src/main -type f \( -name '*.kt' -o -name '*.java' -o -name '*.class' \) -print -quit | grep -q .; then
  echo "WFF package must remain resource-only" >&2
  exit 1
fi
for lane in 1 2 3 4 5 6; do
  grep -q "OrgSchedule${lane}ComplicationService" "$watchface_xml"
done
grep -q 'OrgNextTodoComplicationService' "$watchface_xml"
grep -q '\[COMPLICATION.RANGED_VALUE_MIN\] \* 0.5' "$watchface_xml"
grep -q '\[COMPLICATION.RANGED_VALUE_MAX\] \* 0.5' "$watchface_xml"

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

if ! gradle --no-daemon \
  :app:testDebugUnitTest \
  :shared-ui:testDebugUnitTest \
  :wear-app:testDebugUnitTest \
  :app:assembleDebug \
  :wear-app:assembleDebug \
  :org-watchface:assembleDebug; then
  cat "$interop_log" >&2
  echo "stock ZaraServer Android/Wear/watch-face interop gate failed" >&2
  exit 1
fi

printf 'STOP\n' >&9
wait "$interop_pid"
interop_pid=""
unset ZARA_STOCK_FIXTURE

phone_apk="app/build/outputs/apk/debug/app-debug.apk"
wear_apk="wear-app/build/outputs/apk/debug/wear-app-debug.apk"
watchface_apk="org-watchface/build/outputs/apk/debug/org-watchface-debug.apk"
test -f "$phone_apk"
test -f "$wear_apk"
test -f "$watchface_apk"

for apk in "$phone_apk" "$wear_apk" "$watchface_apk"; do
  if strings "$apk" | grep -Eq "BEGIN (RSA |EC |DSA |OPENSSH )?PRIVATE KEY|CURVE SECRET KEY|zara-server-secret|ZARA_CLIENT_SECRET"; then
    echo "APK secret-marker inspection FAILED: private/secret material found in $apk" >&2
    exit 1
  fi
done

echo "android/wear gate ok: $phone_apk $wear_apk $watchface_apk"
