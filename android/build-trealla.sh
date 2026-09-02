#!/usr/bin/env bash
set -euo pipefail

: "${ANDROID_NDK_ROOT:?ANDROID_NDK_ROOT must point at the pinned Android NDK}"
: "${ZARA_TREALLA_SOURCE_DIR:?ZARA_TREALLA_SOURCE_DIR must point at the pinned Trealla source tree}"

api=29
library_root="${ZARA_TREALLA_LIBRARY_ROOT:-$PWD/app/build/trealla}"
toolchain_root="$ANDROID_NDK_ROOT/toolchains/llvm/prebuilt"

mapfile -t toolchains < <(find "$toolchain_root" -mindepth 1 -maxdepth 1 -type d -print)
if [ "${#toolchains[@]}" -ne 1 ]; then
  echo "expected exactly one Android NDK LLVM prebuilt under $toolchain_root" >&2
  exit 1
fi

toolchain="${toolchains[0]}/bin"
host_cc="${HOST_CC:-cc}"

build_abi() {
  local abi="$1"
  local target="$2"
  local work
  work="$(mktemp -d)"
  trap 'rm -rf "$work"' RETURN

  cp -R "$ZARA_TREALLA_SOURCE_DIR"/. "$work"/
  chmod -R u+w "$work"

  make -C "$work" clean >/dev/null
  make -C "$work" -j"${NIX_BUILD_CORES:-2}" libtrealla.a \
    CC="$toolchain/${target}${api}-clang" \
    AR="$toolchain/llvm-ar" \
    HOST_CC="$host_cc" \
    NOTTY=1 \
    NOSSL=1 \
    NOFFI=1 \
    NONETWORK=1 \
    NOTHREADS=1

  mkdir -p "$library_root/$abi"
  cp "$work/libtrealla.a" "$library_root/$abi/libtrealla.a"
}

rm -rf "$library_root"
build_abi arm64-v8a aarch64-linux-android
build_abi x86_64 x86_64-linux-android

export ZARA_TREALLA_LIBRARY_ROOT="$library_root"
printf '%s\n' "$library_root"
