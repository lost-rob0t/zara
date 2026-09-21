#!/usr/bin/env bash
set -euo pipefail

die() {
    printf 'error: %s\n' "$*" >&2
    exit 1
}

rom_root="${1:-${ROM_ROOT:-$PWD}}"
dest="$rom_root/vendor/zara-fork/rom/aosp/prebuilts"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

mkdir -p "$dest"

curl_args=(
    --fail
    --silent
    --show-error
    --location
    --retry 3
    --retry-all-errors
)

if [[ -n "${GITHUB_TOKEN:-}" ]]; then
    curl_args+=(
        -H "Authorization: Bearer $GITHUB_TOKEN"
        -H "X-GitHub-Api-Version: 2022-11-28"
    )
fi

get() {
    local url="$1"
    local output="$2"
    curl "${curl_args[@]}" "$url" -o "$output"
}

verify() {
    local expected="$1"
    local file="$2"
    printf '%s  %s\n' "$expected" "$file" | sha256sum -c - >/dev/null
}

sync_zara() {
    local base='https://github.com/lost-rob0t/zara/releases/download/android-latest'
    local manifest="$tmp/zara-latest.manifest.txt"
    local apk="$tmp/zara-latest.apk"
    local expected=''
    local attempt

    for attempt in 1 2 3; do
        get "$base/zara-latest.manifest.txt" "$manifest"
        expected="$(sed -n 's/^phone_sha256=//p' "$manifest")"
        [[ "$expected" =~ ^[0-9a-fA-F]{64}$ ]] ||
            die "invalid Zara checksum manifest"

        get "$base/zara-latest.apk" "$apk"
        if verify "$expected" "$apk"; then
            install -m 0644 "$apk" "$dest/Zara.apk"
            printf '%s  Zara.apk\n' "$expected" > "$dest/Zara.apk.sha256"
            cp "$manifest" "$dest/Zara.manifest"
            return 0
        fi

        printf 'Zara rolling release changed during fetch; retry %d/3\n' "$attempt" >&2
    done

    die "unable to fetch a checksum-consistent Zara rolling APK"
}

latest_starintel_tag() {
    if [[ -n "${STARINTEL_TAG:-}" ]]; then
        printf '%s\n' "$STARINTEL_TAG"
        return 0
    fi

    local releases="$tmp/starintel-releases.json"
    get 'https://api.github.com/repos/lost-rob0t/starintel-wearos/releases?per_page=20' "$releases"

    python3 - "$releases" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as handle:
    releases = json.load(handle)

for release in releases:
    if not release.get("draft"):
        tag = release.get("tag_name")
        if tag:
            print(tag)
            raise SystemExit(0)

raise SystemExit("no published StarIntel release found")
PY
}

sync_starintel() {
    local tag
    tag="$(latest_starintel_tag)"
    [[ "$tag" =~ ^v[0-9]+\.[0-9]+\.[0-9]+([-.][0-9A-Za-z.-]+)?$ ]] ||
        die "invalid StarIntel release tag: $tag"

    local base="https://github.com/lost-rob0t/starintel-wearos/releases/download/$tag"
    local sums="$tmp/SHA256SUMS"
    get "$base/SHA256SUMS" "$sums"

    local spec asset target expected file
    for spec in \
        "starintel-phone-$tag.apk:StarIntelCompanion.apk" \
        "quasar-android-$tag.apk:StarIntelQuasar.apk"; do
        asset="${spec%%:*}"
        target="${spec#*:}"
        file="$tmp/$asset"

        expected="$(
            awk -v wanted="$asset" '
                {
                    name=$2
                    sub(/^\.\//, "", name)
                    if (name == wanted) {
                        print $1
                        exit
                    }
                }
            ' "$sums"
        )"

        [[ "$expected" =~ ^[0-9a-fA-F]{64}$ ]] ||
            die "missing checksum for StarIntel asset: $asset"

        get "$base/$asset" "$file"
        verify "$expected" "$file"

        install -m 0644 "$file" "$dest/$target"
        printf '%s  %s\n' "$expected" "$target" > "$dest/$target.sha256"
    done

    printf 'tag=%s\n' "$tag" > "$dest/StarIntel.manifest"
}

sync_zara
sync_starintel

printf 'Synced ROM prebuilts into %s\n' "$dest"
