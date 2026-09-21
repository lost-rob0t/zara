#!/usr/bin/env bash
set -euo pipefail

root="${1:-$PWD}"
common="$root/vendor/lineage/config/common.mk"
hook='$(call inherit-product-if-exists, vendor/zara/android/rom/product/zara.mk)'

[[ -f "$common" ]] || {
    printf 'missing Lineage product config: %s\n' "$common" >&2
    exit 1
}

if ! grep -Fqx "$hook" "$common"; then
    printf '\n# Zara / StarIntel ROM overlay\n%s\n' "$hook" >> "$common"
fi
