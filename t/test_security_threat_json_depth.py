from __future__ import annotations

import os
import socket
from pathlib import Path

import pytest

from zara.security import Capability
from zara.security_admin import SecurityAdminClient, SecurityAdminError, SecurityAdminServer, _recv_message
from zara.security_state import PersistentSecurityState, SecurityStateError


def _deep_array(depth: int) -> bytes:
    return ("[" * depth + "0" + "]" * depth).encode("ascii")


def test_admin_recursive_json_exhaustion_is_a_closed_admin_error():
    receiver, sender = socket.socketpair()
    try:
        sender.sendall(_deep_array(1500) + b"\n")
        sender.shutdown(socket.SHUT_WR)
        with pytest.raises(SecurityAdminError, match="invalid JSON"):
            _recv_message(receiver, limit=16 * 1024)
    finally:
        receiver.close()
        sender.close()


def test_admin_server_survives_recursive_json_attack_and_accepts_next_request(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    registry = state.load_registry()
    admin = SecurityAdminServer(state, capabilities={Capability.SESSION_BASIC})
    admin.bind_registry(registry)
    admin.start()
    try:
        attacker = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        attacker.settimeout(1.0)
        from zara.security_admin import _connect_socket

        _connect_socket(attacker, state.control_socket_path)
        attacker.sendall(_deep_array(1500) + b"\n")
        attacker.close()

        assert SecurityAdminClient(state.control_socket_path).request("list") == []
    finally:
        admin.close(timeout=1.0)


def test_persisted_recursive_json_exhaustion_is_a_closed_state_error(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    clients_path = state.directory / "clients.json"
    clients_path.write_bytes(_deep_array(1500))
    os.chmod(clients_path, 0o600)
    with pytest.raises(SecurityStateError, match="invalid security state JSON"):
        state.load_registry()
