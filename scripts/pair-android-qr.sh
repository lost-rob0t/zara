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

render_qr() {
  if command -v qrencode >/dev/null 2>&1; then
    qrencode "$@"
    return
  fi

  if command -v nix >/dev/null 2>&1; then
    echo "qrencode is not in PATH; using nixpkgs#qrencode for this pairing attempt" >&2
    nix shell nixpkgs#qrencode --command qrencode "$@"
    return
  fi

  fail "qrencode is required for QR pairing (install it or run on a Nix system)"
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
require_tool awk
validate_positive_integer ZARA_ADB_QR_TIMEOUT "$qr_timeout"
validate_positive_integer ZARA_ADB_CONNECT_TIMEOUT "$connect_discovery_timeout"

adb start-server >/dev/null

service_name="studio-$(random_hex 5)"
password="$(random_hex 12)"
payload="WIFI:T:ADB;S:${service_name};P:${password};;"

cat <<'INSTRUCTIONS'
On the Android device:
  Developer options -> Wireless debugging -> Pair device with QR code

Scan this terminal QR. The generated pairing secret is fresh for this attempt.
INSTRUCTIONS
echo
render_qr -t ANSIUTF8 -m 1 "$payload"
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
  echo "Inspect it with: bash scripts/pair-android.sh --diagnose" >&2
  exit 1
fi

exec bash "$pair_android_script" --connect "$connect_endpoint"
