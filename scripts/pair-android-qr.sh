#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
pair_android_script="$script_dir/pair-android.sh"
qr_timeout="${ZARA_ADB_QR_TIMEOUT:-90}"
connect_discovery_timeout="${ZARA_ADB_CONNECT_TIMEOUT:-10}"

fail() {
  echo "error: $*" >&2
  exit 1
}

require_tool() {
  command -v "$1" >/dev/null 2>&1 || fail "$1 is required for QR pairing"
}

validate_positive_integer() {
  local name="$1"
  local value="$2"
  [[ "$value" =~ ^[1-9][0-9]*$ ]] || fail "$name must be a positive integer, got '$value'"
}

random_hex() {
  local byte_count="$1"
  od -An -N "$byte_count" -tx1 /dev/urandom | tr -d '[:space:]'
}

wait_for_named_pairing_service() {
  local service_name="$1"
  local deadline=$((SECONDS + qr_timeout))
  local endpoint=""

  while (( SECONDS < deadline )); do
    endpoint="$(
      adb mdns services 2>/dev/null |
        awk -v name="$service_name" \
          '$1 == name && $2 == "_adb-tls-pairing._tcp" { print $3; exit }'
    )"
    if [[ -n "$endpoint" ]]; then
      printf '%s\n' "$endpoint"
      return 0
    fi
    sleep 1
  done
  return 1
}

wait_for_connect_service() {
  local host="$1"
  local deadline=$((SECONDS + connect_discovery_timeout))
  local endpoint=""

  while (( SECONDS < deadline )); do
    endpoint="$(
      adb mdns services 2>/dev/null |
        awk -v host="$host" \
          '$2 == "_adb-tls-connect._tcp" && index($3, host ":") == 1 { print $3; exit }'
    )"
    if [[ -n "$endpoint" ]]; then
      printf '%s\n' "$endpoint"
      return 0
    fi
    sleep 1
  done
  return 1
}

require_tool adb
require_tool qrencode
require_tool awk

# Open a PNG QR in the user's image viewer. Returns nonzero if no opener works.
show_qr_png() {
  local png="$1"
  local opener
  for opener in xdg-open gio open; do
    if command -v "$opener" >/dev/null 2>&1; then
      if "$opener" "$png" >/dev/null 2>&1; then
        return 0
      fi
    fi
  done
  return 1
}
validate_positive_integer ZARA_ADB_QR_TIMEOUT "$qr_timeout"
validate_positive_integer ZARA_ADB_CONNECT_TIMEOUT "$connect_discovery_timeout"

adb start-server >/dev/null

service_name="studio-$(random_hex 5)"
password="$(random_hex 12)"
payload="WIFI:T:ADB;S:${service_name};P:${password};;"

cat <<'INSTRUCTIONS'
On the Android device:
  Developer options -> Wireless debugging -> Pair device with QR code

Scan this QR with the scanner that opens from that screen (NOT the camera app).

Note: if the QR does not render in your terminal, a PNG copy is opened in your
image viewer automatically -- scan from there instead.
INSTRUCTIONS

# Terminal QRs render as white blocks on a dark background, which many phone
# scanners reject. Prefer a high-contrast PNG in the image viewer, and fall
# back to an inverted terminal render (dark modules on light background).
png_tmp="$(mktemp /tmp/starintel-adb-qr.XXXXXX.png)"
trap 'rm -f "$png_tmp"' EXIT
qrencode -o "$png_tmp" -t PNG -s 10 -m 4 "$payload" 2>/dev/null || png_ok=0
if [[ -s "$png_tmp" ]] && show_qr_png "$png_tmp"; then
  echo "PNG QR opened in image viewer; scan it with the Wireless debugging scanner."
fi
echo
if ! qrencode -t ANSIUTF8i -m 4 "$payload"; then
  qrencode -t ANSIUTF8 -m 4 "$payload"
fi
echo
echo "Waiting up to ${qr_timeout}s for the device to advertise ${service_name}..."

if ! pair_endpoint="$(wait_for_named_pairing_service "$service_name")"; then
  unset password payload
  fail "timed out waiting for the QR pairing service; keep both devices on the same Wi-Fi network and retry"
fi

echo "Discovered pairing endpoint: $pair_endpoint"
if pair_output="$(printf '%s\n' "$password" | adb pair "$pair_endpoint" 2>&1)"; then
  [[ -n "$pair_output" ]] && printf '%s\n' "$pair_output"
else
  unset password payload
  [[ -n "$pair_output" ]] && printf '%s\n' "$pair_output" >&2
  fail "adb rejected the QR pairing handshake"
fi
unset password payload

echo "Pairing succeeded."
host="${pair_endpoint%:*}"
echo "Discovering the normal ADB endpoint for $host..."

if ! connect_endpoint="$(wait_for_connect_service "$host")"; then
  echo "Pairing is complete, but the normal _adb-tls-connect._tcp endpoint was not discovered." >&2
  echo "Inspect it with: scripts/pair-android.sh --diagnose" >&2
  exit 1
fi

exec bash "$pair_android_script" --connect "$connect_endpoint"
