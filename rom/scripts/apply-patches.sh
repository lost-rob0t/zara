#!/usr/bin/env bash
set -euo pipefail

die() {
    printf 'error: %s\n' "$*" >&2
    exit 1
}

rom_root="${1:-${ROM_ROOT:-$PWD}}"
fork_root="$rom_root/vendor/zara-fork"
series="$fork_root/rom/patches/series"

[[ -f "$series" ]] || die "missing patch series: $series"

while read -r project patch extra; do
    [[ -z "$project" ]] && continue
    [[ "$project" == \#* ]] && continue
    [[ -z "$patch" ]] && die "malformed patch series entry for $project"
    [[ -z "${extra:-}" ]] || die "too many fields in patch series entry: $project $patch $extra"

    project_dir="$rom_root/$project"
    patch_file="$fork_root/rom/$patch"

    [[ -d "$project_dir/.git" || -f "$project_dir/.git" ]] ||
        die "project is not synced: $project"
    [[ -f "$patch_file" ]] ||
        die "patch does not exist: $patch"

    if git -C "$project_dir" apply --reverse --check "$patch_file" >/dev/null 2>&1; then
        printf 'already applied: %s -> %s\n' "$patch" "$project"
        continue
    fi

    printf 'apply: %s -> %s\n' "$patch" "$project"
    if ! git -C "$project_dir" am --3way "$patch_file"; then
        git -C "$project_dir" am --abort || true
        die "patch drift: $patch"
    fi
done < "$series"
