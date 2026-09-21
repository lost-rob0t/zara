#!/usr/bin/env bash
set -euo pipefail

root="${ANDROID_BUILD_TOP:-$PWD}"
zara="$root/vendor/zara"
starintel="$root/vendor/starintel-wearos"
out="$zara/android/rom/prebuilts"

[[ -d "$zara/android" ]] || {
    printf 'run from a bootstrapped Lineage tree or set ANDROID_BUILD_TOP\n' >&2
    exit 1
}

mkdir -p "$out"

stage() {
    local src="$1"
    local dst="$2"
    [[ -f "$src" ]] || {
        printf 'missing APK: %s\n' "$src" >&2
        exit 1
    }
    install -m 0644 "$src" "$out/$dst"
}

if [[ -n "${ZARA_SIGNED_APK:-}" ]]; then
    stage "$ZARA_SIGNED_APK" Zara.apk
else
    (
        cd "$zara"
        nix develop ./android --command bash -lc 'cd android && ./gradlew :app:assembleDebug'
    )
    stage "$zara/android/app/build/outputs/apk/debug/app-debug.apk" Zara.apk
fi

if [[ -n "${STARINTEL_COMPANION_SIGNED_APK:-}" && -n "${STARINTEL_QUASAR_SIGNED_APK:-}" ]]; then
    stage "$STARINTEL_COMPANION_SIGNED_APK" StarIntelCompanion.apk
    stage "$STARINTEL_QUASAR_SIGNED_APK" StarIntelQuasar.apk
else
    (
        cd "$starintel"
        ./gradlew :phone-app:assembleDebug :quasar-app:assembleDebug
    )
    stage "$starintel/phone-app/build/outputs/apk/debug/phone-app-debug.apk" StarIntelCompanion.apk
    stage "$starintel/quasar-app/build/outputs/apk/debug/quasar-app-debug.apk" StarIntelQuasar.apk
fi

if command -v apksigner >/dev/null; then
    apksigner verify "$out/Zara.apk"
    apksigner verify "$out/StarIntelCompanion.apk"
    apksigner verify "$out/StarIntelQuasar.apk"
fi

sha256sum \
    "$out/Zara.apk" \
    "$out/StarIntelCompanion.apk" \
    "$out/StarIntelQuasar.apk"
