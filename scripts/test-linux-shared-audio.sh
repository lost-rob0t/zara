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

tone_pid=""
cleanup() {
  local original_status=$?
  local cleanup_status=0
  local removed=0
  trap - EXIT

  if [[ -n "$tone_pid" ]]; then
    kill "$tone_pid" >/dev/null 2>&1 || true
    wait "$tone_pid" >/dev/null 2>&1 || true
  fi

  pulseaudio --kill >/dev/null 2>&1 || true
  for _ in $(seq 1 100); do
    if ! pulseaudio --check >/dev/null 2>&1; then
      break
    fi
    sleep 0.05
  done
  if pulseaudio --check >/dev/null 2>&1; then
    echo "PulseAudio test server did not stop during cleanup" >&2
    cleanup_status=1
  fi

  for _ in $(seq 1 20); do
    if rm -rf "$runtime_dir" 2>/dev/null; then
      removed=1
      break
    fi
    sleep 0.05
  done
  if [[ "$removed" -ne 1 || -e "$runtime_dir" ]]; then
    echo "PulseAudio runtime directory did not quiesce during cleanup: $runtime_dir" >&2
    cleanup_status=1
  fi

  if [[ "$original_status" -ne 0 ]]; then
    exit "$original_status"
  fi
  exit "$cleanup_status"
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

python - <<'PY' | pacat --playback --raw --format=s16le --rate=48000 --channels=1 --device=zara_ci &
import math
import signal
import struct
import sys
import time

signal.signal(signal.SIGPIPE, signal.SIG_DFL)

rate = 48000
frequency = 440.0
amplitude = 0.2
chunk = 4800
end = time.monotonic() + 8.0
phase = 0
while time.monotonic() < end:
    frames = bytearray()
    for _ in range(chunk):
        sample = int(32767 * amplitude * math.sin(2 * math.pi * frequency * phase / rate))
        frames.extend(struct.pack("<h", sample))
        phase += 1
    sys.stdout.buffer.write(frames)
    sys.stdout.buffer.flush()
PY
tone_pid=$!

python scripts/verify-shared-audio.py --seconds 2 --require-signal
