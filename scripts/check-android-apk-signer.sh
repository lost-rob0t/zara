#!/usr/bin/env bash
set -euo pipefail

apk="${1:?APK path is required}"
: "${ANDROID_HOME:?ANDROID_HOME must be set by the Android dev shell}"
signer="$ANDROID_HOME/build-tools/36.0.0/apksigner"
expected_cert="a4d8d02c3ee7491b6b55aa20817dd207973a8671b8e3ede8a0d71759a680fbf2"
actual_cert="$("$signer" verify --print-certs "$apk" | sed -nE 's/^Signer #1 certificate SHA-256 digest: ([0-9a-f]+)$/\1/p')"

if [[ "$actual_cert" != "$expected_cert" ]]; then
  echo "Android APK signing certificate does not match the update key." >&2
  exit 1
fi

echo "Android APK signer matches the installed Zara debug app."
