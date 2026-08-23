#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

python -m pytest -q \
  t/test_voice_protocol.py \
  t/test_voice_output_protocol.py \
  t/test_voice_zmq_transport.py \
  t/test_voice_zmq_negotiation.py \
  t/test_voice_zmq_cancel.py \
  t/test_voice_zmq_cleanup.py \
  t/test_voice_zmq_backpressure.py \
  t/test_voice_zmq_barge_in.py \
  t/test_voice_zmq_client.py \
  t/test_voice_zmq_output_client.py \
  t/test_voice_secure_gateway.py \
  t/test_voice_runtime_ingress.py \
  t/test_voice_runtime_commit.py \
  t/test_streaming_stt_commit.py
