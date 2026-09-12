#!/usr/bin/env bash
set -euo pipefail

connect_timeout="${ZARA_ADB_CONNECT_TIMEOUT:-10}"

usage() {
  cat <<'EOF'
Usage:
  pair-android --qr
  pair-android HOST:PAIR_PORT [HOST:ADB_PORT]
  pair-android --connect HOST:ADB_PORT
  pair-android --diagnose
  pair-android --reset-adb
  pair-android --help

Examples:
  bash scripts/pair-android.sh --qr
  bash scripts/pair-android.sh 192.168.1.50:37123
  bash scripts/pair-android.sh 192.168.1.50:37123 192.168.1.50:42177
  bash scripts/pair-android.sh --connect 192.168.1.50:42177
  bash scripts/pair-android.sh --diagnose
  bash scripts/pair-android.sh --reset-adb

QR pairing on Android devices that expose a scanner:
  Developer options -> Wireless debugging -> Pair device with QR code

Pairing-code fallback (including watches without a QR scanner):
  Developer options -> Wireless debugging -> Pair device with pairing code

The pairing endpoint and normal ADB endpoint are usually different ports.
If the device has forgotten this workstation, --connect cannot restore trust;
pair it again with --qr or a fresh pairing code.

Recommended Zara environment:
  nix develop .#android
EOF
}

require_adb() {
  if ! command -v adb >/dev/null 2>&1; then
    echo "error: adb is required (use nix develop .#android)" >&2
    exit 1
  fi
}

recovery_hint() {
  cat >&2 <<'EOF'
Recovery:
  1. On the device, turn Wireless debugging off and back on.
  2. If this workstation is missing under Paired devices, pair it again.
  3. Reset the local daemon with: bash scripts/pair-android.sh --reset-adb
  4. QR-capable device: bash scripts/pair-android.sh --qr
  5. Pairing-code fallback: bash scripts/pair-android.sh HOST:PAIR_PORT HOST:ADB_PORT

You can inspect ADB/mDNS state with:
  bash scripts/pair-android.sh --diagnose
EOF
}

verify_online() {
  local endpoint="$1"
  if ! adb devices | awk -v target="$endpoint" '$1 == target && $2 == "device" { found = 1 } END { exit found ? 0 : 1 }'; then
    echo "error: adb did not report $endpoint as an online device" >&2
    echo "Current adb devices:" >&2
    adb devices -l >&2 || true
    recovery_hint
    exit 1
  fi
}

connect_device() {
  local endpoint="$1"
  local output status

  echo "Connecting to $endpoint (timeout ${connect_timeout}s)"
  set +e
  output="$(timeout "${connect_timeout}s" adb connect "$endpoint" 2>&1)"
  status=$?
  set -e

  if [[ -n "$output" ]]; then
    printf '%s\n' "$output"
  fi

  if [[ $status -eq 124 ]]; then
    echo "error: adb connect timed out; the TCP port may be open while pairing trust is stale or forgotten" >&2
    recovery_hint
    exit 1
  fi

  if [[ $status -ne 0 ]]; then
    echo "error: adb connect failed for $endpoint" >&2
    recovery_hint
    exit "$status"
  fi

  verify_online "$endpoint"
}

diagnose() {
  echo "== adb version =="
  adb version || true
  echo
  echo "== adb devices =="
  adb devices -l || true
  echo
  echo "== adb server status =="
  adb server-status 2>&1 || true
  echo
  echo "== adb mDNS services =="
  timeout 5s adb mdns services 2>&1 || true
}

reset_adb() {
  echo "Restarting local ADB daemon"
  adb kill-server >/dev/null 2>&1 || true
  adb start-server
  echo
  adb devices -l || true
  echo
  echo "Local ADB daemon restarted."
  echo "If the device forgot this workstation, pair it again with --qr or a fresh pairing code."
}

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
  usage
  exit 0
fi

require_adb

if [[ "${1:-}" == "--qr" ]]; then
  if [[ $# -ne 1 ]]; then
    usage >&2
    exit 2
  fi
  script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
  exec bash "$script_dir/pair-android-qr.sh"
fi

if [[ "${1:-}" == "--diagnose" ]]; then
  if [[ $# -ne 1 ]]; then
    usage >&2
    exit 2
  fi
  diagnose
  exit 0
fi

if [[ "${1:-}" == "--reset-adb" ]]; then
  if [[ $# -ne 1 ]]; then
    usage >&2
    exit 2
  fi
  reset_adb
  exit 0
fi

if [[ "${1:-}" == "--connect" ]]; then
  if [[ $# -ne 2 || "$2" != *:* ]]; then
    usage >&2
    exit 2
  fi
  endpoint="$2"
  connect_device "$endpoint"
  echo
  echo "Android device connected for Zara development: $endpoint"
  exit 0
fi

if [[ $# -lt 1 || $# -gt 2 ]]; then
  usage >&2
  exit 2
fi

pair_endpoint="$1"
connect_endpoint="${2:-}"

if [[ "$pair_endpoint" != *:* ]]; then
  echo "error: pairing endpoint must look like HOST:PAIR_PORT" >&2
  exit 2
fi

if [[ -n "$connect_endpoint" && "$connect_endpoint" != *:* ]]; then
  echo "error: ADB endpoint must look like HOST:ADB_PORT" >&2
  exit 2
fi

echo "Pairing with $pair_endpoint"
echo "Enter the six-digit pairing code shown on the device when adb asks for it."
adb pair "$pair_endpoint"

echo
echo "Pairing succeeded."

if [[ -z "$connect_endpoint" ]]; then
  echo "Now note the normal Wireless debugging IP address & port and run:"
  echo "  bash scripts/pair-android.sh --connect HOST:ADB_PORT"
  exit 0
fi

connect_device "$connect_endpoint"

echo
echo "Android device paired and connected for Zara development: $connect_endpoint"
