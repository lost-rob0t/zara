#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# Keep transport deadlocks diagnostic instead of letting the repository gate
# burn until the outer CI timeout. -vv prints the active test, faulthandler
# dumps Python thread stacks, and timeout guarantees a bounded failure.
# Emit a focused JUnit report when the parent regression gate provides an
# artifact directory, so fail-fast transport regressions stay diagnosable even
# when the full pytest phase is never reached.
junit_args=()
if [ -n "${ARTIFACT_DIR:-}" ]; then
  mkdir -p "$ARTIFACT_DIR"
  junit_args+=(--junitxml="$ARTIFACT_DIR/zara1-junit.xml")
fi

# This focused gate intentionally includes every #129-owned protocol,
# transport, reconnect/idempotency, backpressure/status, and server-endpoint
# test so a narrow run cannot silently omit a later RAGE slice.
timeout 120s python -m pytest \
  -vv \
  -x \
  -o faulthandler_timeout=15 \
  "${junit_args[@]}" \
  t/test_protocol.py \
  t/test_protocol_runtime.py \
  t/test_zmq_transport.py \
  t/test_zmq_hardening.py \
  t/test_zmq_transport_turn_completed.py \
  t/test_zmq_transport_reconnect_idempotency.py \
  t/test_zmq_transport_route_backpressure.py \
  t/test_zmq_transport_status.py \
  t/test_server_zmq_endpoint.py

bash "$repo_root/scripts/test-zara1-voice.sh"
bash "$repo_root/scripts/test-zara1-tool-approvals.sh"
