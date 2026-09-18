from __future__ import annotations

import os
import stat
from pathlib import Path

from zara.security import Capability
from zara.security_admin import SecurityAdminClient, SecurityAdminServer
from zara.security_state import PersistentSecurityState


def test_security_admin_permission_contract_is_covered(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    registry = state.load_registry()
    admin = SecurityAdminServer(state, capabilities={Capability.SESSION_BASIC})
    admin.bind_registry(registry)
    admin.start()
    try:
        directory_info = os.lstat(state.directory)
        socket_info = os.lstat(state.control_socket_path)
        assert stat.S_IMODE(directory_info.st_mode) == 0o700
        assert stat.S_IMODE(socket_info.st_mode) == 0o600
        assert socket_info.st_uid == os.getuid()
        assert SecurityAdminClient(state.control_socket_path).request("list") == []
    finally:
        admin.close(timeout=1.0)



def test_security_admin_remote_listener_actions_are_explicitly_owner_controlled(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    registry = state.load_registry()
    calls = []
    admin = SecurityAdminServer(
        state,
        capabilities={Capability.SESSION_BASIC},
        ensure_remote_listener=lambda endpoint: calls.append(endpoint) or {
            "active": True,
            "endpoint": endpoint or "tcp://0.0.0.0:7731",
            "server_public_key": state.server_public_key(),
        },
        remote_listener_status=lambda: {
            "active": False,
            "endpoint": None,
            "server_public_key": state.server_public_key(),
        },
    )
    admin.bind_registry(registry)
    admin.start()
    try:
        client = SecurityAdminClient(state.control_socket_path)
        assert client.request("remote_listener.status")["active"] is False
        ensured = client.request("remote_listener.ensure", endpoint="tcp://127.0.0.1:7731")
        assert ensured["active"] is True
        assert calls == ["tcp://127.0.0.1:7731"]
    finally:
        admin.close(timeout=1.0)
