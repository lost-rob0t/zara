#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"
export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"

runtime_dir="${RUNNER_TEMP:-${TMPDIR:-/tmp}}/zara-pulse-${$}"
mkdir -p "$runtime_dir"
chmod 700 "$runtime_dir"

export XDG_RUNTIME_DIR="$runtime_dir"
export PULSE_RUNTIME_PATH="$runtime_dir/pulse"

cleanup() {
  pulseaudio --kill >/dev/null 2>&1 || true
  rm -rf "$runtime_dir"
}
trap cleanup EXIT

pulseaudio \
  --daemonize=yes \
  --exit-idle-time=-1 \
  --log-target="file:$runtime_dir/pulseaudio.log"

for _ in $(seq 1 50); do
  if pactl info >/dev/null 2>&1; then
    break
  fi
  sleep 0.1
done

if ! pactl info >/dev/null 2>&1; then
  cat "$runtime_dir/pulseaudio.log" >&2 || true
  echo "PulseAudio test server did not become ready" >&2
  exit 1
fi

pactl load-module module-null-sink \
  sink_name=zara_ci \
  sink_properties=device.description=ZaraCI >/dev/null
pactl set-default-source zara_ci.monitor

python scripts/verify-shared-audio.py
