#!/usr/bin/env bash
set -euo pipefail

die() {
    printf 'error: %s\n' "$*" >&2
    exit 1
}

device="${1:-}"
variant="${2:-userdebug}"
rom_root="${ROM_ROOT:-$HOME/android/zara-rom}"
build_jobs="${BUILD_JOBS:-$(nproc)}"

[[ -n "$device" ]] || die "usage: $0 <device-codename> [user|userdebug|eng]"
[[ "$variant" =~ ^(user|userdebug|eng)$ ]] || die "invalid build variant: $variant"
[[ -d "$rom_root/.repo" ]] || die "not a repo workspace: $rom_root"
[[ -f "$rom_root/vendor/zara-gapps/product.mk" ]] ||
    die "Google services input missing: vendor/zara-gapps/product.mk"

prebuilts="$rom_root/vendor/zara-fork/rom/aosp/prebuilts"
for apk in Zara.apk StarIntelCompanion.apk StarIntelQuasar.apk; do
    [[ -f "$prebuilts/$apk" ]] || die "missing prebuilt: $prebuilts/$apk"
done

bash "$rom_root/vendor/zara-fork/rom/scripts/apply-patches.sh" "$rom_root"

cd "$rom_root"
# shellcheck disable=SC1091
source build/envsetup.sh
breakfast "$device" "$variant"

mka -j"$build_jobs" bacon

out_dir="${OUT:-$rom_root/out/target/product/$device}"
mkdir -p "$out_dir"

repo manifest -r -o "$out_dir/zara-rom-manifest.xml"
sha256sum \
    "$prebuilts/Zara.apk" \
    "$prebuilts/StarIntelCompanion.apk" \
    "$prebuilts/StarIntelQuasar.apk" \
    > "$out_dir/zara-rom-prebuilts.sha256"

zip_path="$(find "$out_dir" -maxdepth 1 -type f -name 'lineage-*.zip' -printf '%T@ %p\n' |
    sort -nr |
    head -n1 |
    cut -d' ' -f2-)"

[[ -n "$zip_path" ]] || die "build finished without a Lineage OTA zip"
printf 'Built: %s\n' "$zip_path"
