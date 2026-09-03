#!/usr/bin/env bash
set -euo pipefail

python -m pytest -q t/test_protocol.py t/test_zmq_transport.py t/test_zara1_device_actions.py t/test_daemon_security_gateway.py
