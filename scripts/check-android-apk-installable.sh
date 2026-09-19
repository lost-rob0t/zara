#!/usr/bin/env bash
set -euo pipefail

apk="${1:?APK path is required}"
expected_package="${2:?Expected package name is required}"

: "${ANDROID_HOME:?ANDROID_HOME must be set by the Android dev shell}"

build_tools="$ANDROID_HOME/build-tools/36.0.0"
apksigner="$build_tools/apksigner"
aapt2="$build_tools/aapt2"
zipalign="$build_tools/zipalign"

for tool in "$apksigner" "$aapt2" "$zipalign"; do
  if [[ ! -x "$tool" ]]; then
    echo "Android APK installability check missing tool: $tool" >&2
    exit 1
  fi
done

if [[ ! -f "$apk" ]]; then
  echo "Android APK installability check missing APK: $apk" >&2
  exit 1
fi

"$apksigner" verify --verbose --print-certs "$apk" >/dev/null
"$zipalign" -c -P 16 4 "$apk" >/dev/null

badging="$("$aapt2" dump badging "$apk")"
if ! grep -Fq "package: name='$expected_package'" <<<"$badging"; then
  echo "Android APK package mismatch: expected=$expected_package apk=$apk" >&2
  exit 1
fi

if ! grep -Fq "sdkVersion:'" <<<"$badging"; then
  echo "Android APK manifest is missing a parseable minSdk: $apk" >&2
  exit 1
fi

echo "Android APK installability structure/signature ok: $expected_package $apk"
