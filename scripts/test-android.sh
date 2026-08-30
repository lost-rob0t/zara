#!/usr/bin/env bash
# Zara Android gate (#171): JVM unit tests + debug APK + secret-marker inspection.
# Run via: nix develop .#android -c bash scripts/test-android.sh
set -euo pipefail

cd "$(dirname "$0")/../android"

: "${ANDROID_HOME:?ANDROID_HOME must be set by the nix android dev shell}"

gradle --no-daemon testDebugUnitTest assembleDebug

apk="app/build/outputs/apk/debug/app-debug.apk"
test -f "$apk"

if strings "$apk" | grep -Eq "BEGIN (RSA |EC |DSA |OPENSSH )?PRIVATE KEY|CURVE SECRET KEY|zara-server-secret|ZARA_CLIENT_SECRET"; then
  echo "APK secret-marker inspection FAILED: private/secret material found in $apk" >&2
  exit 1
fi

echo "android gate ok: $apk"
