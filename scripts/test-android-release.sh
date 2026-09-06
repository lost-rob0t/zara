#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

source_sha="$(git rev-parse HEAD)"
if [[ -n "$(git status --porcelain --untracked-files=no)" ]]; then
  echo "android release gate requires a clean tracked worktree" >&2
  exit 1
fi

bash "$repo_root/scripts/test-android.sh"

apk="$repo_root/android/app/build/outputs/apk/debug/app-debug.apk"
test -f "$apk"

artifact_dir="$repo_root/artifacts/android-release"
mkdir -p "$artifact_dir"
manifest="$artifact_dir/manifest.txt"

apk_sha256="$(sha256sum "$apk" | awk '{print $1}')"
cat >"$manifest" <<EOF
schema=1
source_sha=$source_sha
apk=android/app/build/outputs/apk/debug/app-debug.apk
apk_sha256=$apk_sha256
deterministic.android_gate=PASS
deterministic.semantic_parity=PASS
deterministic.native_trealla=PASS
deterministic.stock_server_interop=PASS
deterministic.jvm_suite=PASS
deterministic.apk_build=PASS
deterministic.apk_secret_scan=PASS
hardware.real_device_install=PENDING
hardware.real_microphone=PENDING
hardware.bluetooth_route=PENDING
hardware.samsung_assistant_role=PENDING
hardware.side_button=PENDING
hardware.revocation_on_device=PENDING
EOF

if ! grep -qx "source_sha=$source_sha" "$manifest"; then
  echo "android release manifest source SHA mismatch" >&2
  exit 1
fi

if grep -Eq '^hardware\..*=PASS$' "$manifest"; then
  echo "hardware-only Android scenarios must remain PENDING in automation" >&2
  exit 1
fi

echo "android release gate ok: $apk"
echo "android release evidence: $manifest"
