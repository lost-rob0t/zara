#!/usr/bin/env bash
set -euo pipefail

rom_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dest="${1:-$HOME/android/zara-lineage}"
branch="${LINEAGE_BRANCH:-lineage-23.2}"

command -v repo >/dev/null || {
    printf 'repo tool is required\n' >&2
    exit 1
}

mkdir -p "$dest"
cd "$dest"

if [[ ! -d .repo ]]; then
    repo init \
        -u https://github.com/LineageOS/android.git \
        -b "$branch" \
        --git-lfs \
        --no-clone-bundle
fi

mkdir -p .repo/local_manifests
cp "$rom_dir/local_manifests/zara.xml" .repo/local_manifests/zara.xml

repo sync

vendor/zara/android/rom/scripts/apply-product-hook.sh "$dest"

printf 'LineageOS %s synced at %s\n' "$branch" "$dest"
printf 'Next: vendor/zara/android/rom/scripts/stage-apks.sh\n'
