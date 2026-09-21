#!/usr/bin/env bash
set -euo pipefail

root="${1:-$PWD}"
mode="${2:---apply}"
common="$root/vendor/lineage/config/common.mk"
begin="# >>> ZARA_STARINTEL_ROM"
hook='$(call inherit-product-if-exists, vendor/zara/android/rom/product/zara.mk)'
end="# <<< ZARA_STARINTEL_ROM"

[[ -f "$common" ]] || {
    printf 'missing Lineage product config: %s\n' "$common" >&2
    exit 1
}

remove_hook() {
    local tmp
    tmp="$(mktemp)"
    awk -v begin="$begin" -v end="$end" '
        $0 == begin { skip = 1; next }
        $0 == end { skip = 0; next }
        !skip { print }
    ' "$common" > "$tmp"
    cat "$tmp" > "$common"
    rm -f "$tmp"
}

case "$mode" in
    --remove)
        remove_hook
        ;;
    --apply)
        remove_hook
        printf '\n%s\n%s\n%s\n' "$begin" "$hook" "$end" >> "$common"
        ;;
    *)
        printf 'usage: %s [android-root] [--apply|--remove]\n' "$0" >&2
        exit 2
        ;;
esac
