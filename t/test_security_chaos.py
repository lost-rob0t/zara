from __future__ import annotations

import concurrent.futures
import socket
import threading
from pathlib import Path

import pytest
import zmq

import zara.server as server_module
from zara.principals import PrincipalContext
from zara.security import Capability, KeyNotActive, SecurityRegistry
from zara.security_admin import SecurityAdminClient, SecurityAdminServer
from zara.security_state import PersistentSecurityState, SecurityStateError
from zara.server import ServerLease


def _start_admin(state: PersistentSecurityState):
    registry = state.load_registry()
    admin = SecurityAdminServer(
        state,
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    admin.bind_registry(registry)
    admin.start()
    return admin, registry


def test_live_security_admin_survives_concurrent_enroll_revoke_interleavings(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    admin, registry = _start_admin(state)
    identities = []
    for index in range(8):
        public_key, _secret_key = zmq.curve_keypair()
        identities.append((f"chaos-{index}", public_key.decode("ascii")))

    start = threading.Event()

    def mutate(index: int, device_id: str, public_key: str):
        client = SecurityAdminClient(state.control_socket_path)
        start.wait(timeout=2.0)
        enrolled = client.request(
            "enroll",
            public_key=public_key,
            device_id=device_id,
        )
        assert enrolled["active"] is True
        if index % 2:
            client.request("list")
            revoked = client.request("revoke", device_id=device_id)
            assert revoked == {"device_id": device_id, "active": False}

    try:
        with concurrent.futures.ThreadPoolExecutor(max_workers=len(identities)) as executor:
            futures = [
                executor.submit(mutate, index, device_id, public_key)
                for index, (device_id, public_key) in enumerate(identities)
            ]
            start.set()
            for future in futures:
                future.result(timeout=5.0)

        persisted = {entry["device_id"]: entry for entry in state.list_clients()}
        assert set(persisted) == {device_id for device_id, _key in identities}
        for index, (device_id, public_key) in enumerate(identities):
            expected_active = index % 2 == 0
            assert persisted[device_id]["active"] is expected_active
            if expected_active:
                assert registry.resolve_public_key(public_key).device_id == device_id
            else:
                with pytest.raises(KeyNotActive):
                    registry.resolve_public_key(public_key)
    finally:
        admin.close(timeout=1.0)


def test_partial_admin_request_does_not_poison_next_owner_request(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    admin, registry = _start_admin(state)
    public_key, _secret_key = zmq.curve_keypair()

    try:
        attacker = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        attacker.settimeout(1.0)
        attacker.connect(str(state.control_socket_path))
        attacker.sendall(b'{"version":1,"action":"enroll"')
        attacker.close()

        client = SecurityAdminClient(state.control_socket_path)
        result = client.request(
            "enroll",
            public_key=public_key.decode("ascii"),
            device_id="after-partial",
        )
        assert result["active"] is True
        assert registry.resolve_public_key(public_key).device_id == "after-partial"
    finally:
        admin.close(timeout=1.0)


def test_live_enrollment_divergence_rolls_persistence_back(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    public_key, _secret_key = zmq.curve_keypair()
    live_registry = SecurityRegistry()
    live_registry.enroll(
        public_key,
        principal=PrincipalContext.local_owner(),
        device_id="already-live",
        capabilities={Capability.SESSION_BASIC},
    )

    with pytest.raises(SecurityStateError, match="diverged"):
        state.enroll_client(
            public_key,
            device_id="disk-candidate",
            principal=PrincipalContext.local_owner(),
            capabilities={Capability.SESSION_BASIC},
            live_registry=live_registry,
        )

    assert state.list_clients() == ()
    assert live_registry.resolve_public_key(public_key).device_id == "already-live"


def test_live_revocation_divergence_rolls_persistence_back(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    public_key, _secret_key = zmq.curve_keypair()
    state.enroll_client(
        public_key,
        device_id="persisted-phone",
        principal=PrincipalContext.local_owner(),
        capabilities={Capability.SESSION_BASIC},
    )

    with pytest.raises(SecurityStateError, match="diverged"):
        state.revoke_device(
            "persisted-phone",
            live_registry=SecurityRegistry(),
        )

    persisted = state.list_clients()
    assert len(persisted) == 1
    assert persisted[0]["device_id"] == "persisted-phone"
    assert persisted[0]["active"] is True
    assert state.load_registry().resolve_public_key(public_key).device_id == "persisted-phone"


def test_missing_admin_socket_while_daemon_lease_is_held_fails_closed(tmp_path: Path):
    runtime_dir = tmp_path / "runtime"
    security_dir = tmp_path / "security"
    state = PersistentSecurityState(security_dir)
    state.initialize()
    public_key, _secret_key = zmq.curve_keypair()
    lease = ServerLease(runtime_dir)
    lease.acquire()

    args = server_module._parser().parse_args(
        [
            "--runtime-dir",
            str(runtime_dir),
            "--security-dir",
            str(security_dir),
            "--security-enroll-key",
            public_key.decode("ascii"),
            "--security-device-id",
            "must-not-hit-disk",
        ]
    )
    try:
        with pytest.raises(RuntimeError, match="refusing disk-only security mutation"):
            server_module._run_security_management(args)
        assert state.list_clients() == ()
    finally:
        lease.release()
