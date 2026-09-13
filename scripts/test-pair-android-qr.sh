#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/bin"

cat > "$tmp/bin/qrencode" <<'QR'
#!/usr/bin/env bash
set -euo pipefail
payload="${@: -1}"
printf '%s\n' "$payload" > "${PAIR_TEST_STATE}/payload"
printf '[mock qr]\n'
QR
chmod +x "$tmp/bin/qrencode"

cat > "$tmp/bin/adb" <<'ADB'
#!/usr/bin/env bash
set -euo pipefail
state="${PAIR_TEST_STATE}"
cmd="${1:-}"
shift || true

case "$cmd" in
  start-server)
    exit 0
    ;;
  mdns)
    [[ "${1:-}" == "services" ]]
    payload="$(cat "$state/payload")"
    service="${payload#*;S:}"
    service="${service%%;P:*}"
    cat <<EOF_MDNS
List of discovered mdns services
${service} _adb-tls-pairing._tcp 10.50.50.69:39625
adb-test-connect _adb-tls-connect._tcp 10.50.50.69:42177
EOF_MDNS
    ;;
  pair)
    endpoint="${1:-}"
    secret="$(cat)"
    payload="$(cat "$state/payload")"
    expected="${payload#*;P:}"
    expected="${expected%;;}"
    [[ "$endpoint" == "10.50.50.69:39625" ]]
    [[ "$secret" == "$expected" ]]
    printf 'Successfully paired to %s\n' "$endpoint"
    ;;
  connect)
    [[ "${1:-}" == "10.50.50.69:42177" ]]
    printf 'connected to %s\n' "$1"
    ;;
  devices)
    printf 'List of devices attached\n10.50.50.69:42177\tdevice product:test model:test device:test\n'
    ;;
  *)
    echo "unexpected adb invocation: $cmd $*" >&2
    exit 99
    ;;
esac
ADB
chmod +x "$tmp/bin/adb"

PAIR_TEST_STATE="$tmp" \
PATH="$tmp/bin:$PATH" \
ZARA_ADB_CONNECT_TIMEOUT=2 \
ZARA_ADB_QR_TIMEOUT=2 \
  bash "$repo_root/scripts/pair-android.sh" --qr > "$tmp/output"

payload="$(cat "$tmp/payload")"
[[ "$payload" =~ ^WIFI:T:ADB\;S:studio-[0-9a-f]{10}\;P:[0-9a-f]{24}\;\;$ ]]
grep -Fq 'Discovered pairing endpoint: 10.50.50.69:39625' "$tmp/output"
grep -Fq 'Android device connected: 10.50.50.69:42177' "$tmp/output"

echo "pair-android QR flow test passed"
