#!/usr/bin/env bash
set -euo pipefail

die() {
    printf 'error: %s\n' "$*" >&2
    exit 1
}

need() {
    command -v "$1" >/dev/null 2>&1 || die "missing command: $1"
}

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
source_root="$(git -C "$script_dir/../.." rev-parse --show-toplevel)"

rom_root="${ROM_ROOT:-$HOME/android/zara-rom}"
lineage_branch="${LINEAGE_BRANCH:-lineage-23.2}"
repo_jobs="${REPO_JOBS:-$(nproc)}"

need git
need repo
need rsync
need curl
need python3
need sha256sum

mkdir -p "$rom_root"
cd "$rom_root"

repo init \
    -u https://github.com/LineageOS/android.git \
    -b "$lineage_branch" \
    --git-lfs

mkdir -p .repo/local_manifests
install -m 0644 \
    "$source_root/rom/manifest/zara.xml" \
    .repo/local_manifests/zara.xml

repo sync -c --fail-fast -j"$repo_jobs"

if [[ -n "${GAPPS_VENDOR_DIR:-}" ]]; then
    [[ -f "$GAPPS_VENDOR_DIR/product.mk" ]] ||
        die "GAPPS_VENDOR_DIR must contain product.mk"

    mkdir -p vendor/zara-gapps
    rsync -a --delete --exclude=.git/ \
        "$GAPPS_VENDOR_DIR/" \
        vendor/zara-gapps/
else
    printf '%s\n' \
        'warning: GAPPS_VENDOR_DIR is unset; sync succeeded but ROM build will refuse to run without vendor/zara-gapps/product.mk' >&2
fi

"$rom_root/vendor/zara-fork/rom/scripts/sync-prebuilts.sh" "$rom_root"
"$rom_root/vendor/zara-fork/rom/scripts/apply-patches.sh" "$rom_root"

printf 'ROM workspace ready: %s\n' "$rom_root"
