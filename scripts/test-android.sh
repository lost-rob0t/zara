#!/usr/bin/env bash
# Zara Android gate: semantic parity + JVM tests + pinned native build + debug APK + secret inspection.
# Run via: nix develop .#android -c bash scripts/test-android.sh
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root/android"

: "${ANDROID_HOME:?ANDROID_HOME must be set by the nix android dev shell}"
: "${ANDROID_NDK_ROOT:?ANDROID_NDK_ROOT must be set by the nix android dev shell}"
: "${ZARA_TREALLA_SOURCE_DIR:?ZARA_TREALLA_SOURCE_DIR must be set by the nix android dev shell}"

bash "$repo_root/scripts/test-android-semantic-parity.sh"

export ZARA_TREALLA_LIBRARY_ROOT="$PWD/app/build/trealla"
bash ./build-trealla.sh

gradle --no-daemon testDebugUnitTest assembleDebug

apk="app/build/outputs/apk/debug/app-debug.apk"
test -f "$apk"

if strings "$apk" | grep -Eq "BEGIN (RSA |EC |DSA |OPENSSH )?PRIVATE KEY|CURVE SECRET KEY|zara-server-secret|ZARA_CLIENT_SECRET"; then
  echo "APK secret-marker inspection FAILED: private/secret material found in $apk" >&2
  exit 1
fi

echo "android gate ok: $apk"
