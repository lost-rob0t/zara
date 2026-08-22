#!/usr/bin/env bash
set -euo pipefail

python -m pytest -q \
  t/test_server.py \
  t/test_client.py \
  t/test_runtime_host.py \
  t/test_runtime_host_startup_shutdown.py

python -m zara.server --help | grep -q 'zara-server'
python - <<'PY'
from zara.client import InProcessZaraClient, ZaraClient
from zara.server import PrincipalContext, RuntimeSupervisor, ServerLease, ZaraServer

assert issubclass(InProcessZaraClient, ZaraClient)
assert PrincipalContext.local_owner().principal_id.startswith('uid:')
assert RuntimeSupervisor
assert ServerLease
assert ZaraServer
PY
