#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# Keep transport deadlocks diagnostic instead of letting the repository gate
# burn until the outer CI timeout. -vv prints the active test, faulthandler
# dumps Python thread stacks, and timeout guarantees a bounded failure.
#
# This focused gate intentionally includes every #129-owned protocol,
# transport, reconnect/idempotency, backpressure/status, and server-endpoint
# test so a narrow run cannot silently omit a later RAGE slice.
timeout 120s python -m pytest \
  -vv \
  -x \
  -o faulthandler_timeout=15 \
  t/test_protocol.py \
  t/test_protocol_runtime.py \
  t/test_zmq_transport.py \
  t/test_zmq_transport_turn_completed.py \
  t/test_zmq_transport_reconnect_idempotency.py \
  t/test_zmq_transport_route_backpressure.py \
  t/test_zmq_transport_status.py \
  t/test_server_zmq_endpoint.py
