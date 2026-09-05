from __future__ import annotations

import concurrent.futures
import os
import socket
import threading
from pathlib import Path

import pytest
import zmq

import zara.security_state as security_state_module
from zara.principals import PrincipalContext
from zara.security import Capability, KeyNotActive, SecurityRegistry
from zara.security_admin import (
    SecurityAdminClient,
    SecurityAdminError,
    SecurityAdminServer,
    _MAX_REQUEST_BYTES,
    _connect_socket,
    _recv_message,
)
from zara.security_state import PersistentSecurityState, SecurityStateError


_CAPABILITIES = {Capability.SESSION_BASIC, Capability.TURN_SUBMIT}


def _start_admin(state: PersistentSecurityState):
    registry = state.load_registry()
    admin = SecurityAdminServer(state, capabilities=_CAPABILITIES)
    admin.bind_registry(registry)
    admin.start()
    return admin, registry


def _recv_payload(payload: bytes, *, limit: int = _MAX_REQUEST_BYTES):
    receiver, sender = socket.socketpair()
    try:
        sender.sendall(payload)
        sender.shutdown(socket.SHUT_WR)
        return _recv_message(receiver, limit=limit)
    finally:
        receiver.close()
        sender.close()


def _new_key() -> str:
    public_key, _secret_key = zmq.curve_keypair()
    return public_key.decode("ascii")


def test_admin_framing_rejects_invalid_utf8_extra_frames_and_oversize():
    with pytest.raises(SecurityAdminError, match="invalid JSON"):
        _recv_payload(b"\xff\n")
    with pytest.raises(SecurityAdminError, match="one request"):
        _recv_payload(b'{"version":1,"action":"list"}\n{}\n')
    with pytest.raises(SecurityAdminError, match="byte limit"):
        _recv_payload(b"x" * (_MAX_REQUEST_BYTES + 1))


def test_admin_protocol_replay_cannot_duplicate_or_resurrect_identity(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    admin, registry = _start_admin(state)
    client = SecurityAdminClient(state.control_socket_path)
    public_key = _new_key()

    try:
        enrolled = client.request("enroll", public_key=public_key, device_id="replay-phone")
        assert enrolled["active"] is True
        with pytest.raises(SecurityAdminError):
            client.request("enroll", public_key=public_key, device_id="replay-phone")

        revoked = client.request("revoke", device_id="replay-phone")
        assert revoked == {"device_id": "replay-phone", "active": False}
        with pytest.raises(SecurityAdminError):
            client.request("revoke", device_id="replay-phone")

        records = state.list_clients()
        assert len(records) == 1
        assert records[0]["active"] is False
        with pytest.raises(KeyNotActive):
            registry.resolve_public_key(public_key)
    finally:
        admin.close(timeout=1.0)


def test_admin_restart_churn_removes_stale_endpoint(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()

    for _ in range(12):
        admin, _registry = _start_admin(state)
        try:
            assert state.control_socket_path.exists()
            assert SecurityAdminClient(state.control_socket_path).request("list") == []
        finally:
            admin.close(timeout=1.0)
        assert not state.control_socket_path.exists()


def test_second_admin_cannot_hijack_active_owner_endpoint(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    first, _registry = _start_admin(state)
    second = SecurityAdminServer(state, capabilities=_CAPABILITIES)
    second.bind_registry(state.load_registry())
    try:
        with pytest.raises(SecurityAdminError, match="already active"):
            second.start()
        assert SecurityAdminClient(state.control_socket_path).request("list") == []
    finally:
        first.close(timeout=1.0)


def test_regular_file_at_admin_endpoint_is_never_unlinked_as_stale_socket(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    state.control_socket_path.write_text("do-not-delete", encoding="utf-8")
    os.chmod(state.control_socket_path, 0o600)
    admin = SecurityAdminServer(state, capabilities=_CAPABILITIES)
    admin.bind_registry(state.load_registry())

    with pytest.raises(SecurityAdminError, match="not a Unix socket"):
        admin.start()
    assert state.control_socket_path.read_text(encoding="utf-8") == "do-not-delete"


def test_client_refuses_permission_tampered_admin_socket_then_recovers(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    admin, _registry = _start_admin(state)
    client = SecurityAdminClient(state.control_socket_path)
    try:
        os.chmod(state.control_socket_path, 0o666)
        with pytest.raises(SecurityAdminError, match="permissions"):
            client.request("list")
        os.chmod(state.control_socket_path, 0o600)
        assert client.request("list") == []
    finally:
        admin.close(timeout=1.0)


def test_malformed_disconnect_storm_does_not_poison_next_owner_request(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    admin, _registry = _start_admin(state)
    try:
        for index in range(8):
            attacker = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            attacker.settimeout(1.0)
            _connect_socket(attacker, state.control_socket_path)
            attacker.sendall(f'{{"garbage":{index}}}'.encode("ascii"))
            attacker.close()
        assert SecurityAdminClient(state.control_socket_path).request("list") == []
    finally:
        admin.close(timeout=1.0)


def test_symlinked_and_world_readable_registry_files_fail_closed(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    clients_path = state.directory / "clients.json"
    backup = state.directory / "clients-backup.json"
    backup.write_bytes(clients_path.read_bytes())
    os.chmod(backup, 0o600)

    clients_path.unlink()
    clients_path.symlink_to(backup)
    with pytest.raises(SecurityStateError, match="unsafe security state file"):
        state.load_registry()

    clients_path.unlink()
    clients_path.write_bytes(backup.read_bytes())
    os.chmod(clients_path, 0o644)
    with pytest.raises(SecurityStateError, match="unsafe security state file"):
        state.load_registry()


def test_oversized_registry_is_rejected_before_json_parsing(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    clients_path = state.directory / "clients.json"
    clients_path.write_bytes(b"{" + b"x" * (1024 * 1024) + b"}")
    os.chmod(clients_path, 0o600)

    with pytest.raises(SecurityStateError, match="invalid size"):
        state.load_registry()


def test_failed_atomic_replace_does_not_mutate_live_authority(tmp_path: Path, monkeypatch):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    live_registry = state.load_registry()
    public_key = _new_key()
    original_replace = security_state_module.os.replace

    def fail_clients_replace(source, destination):
        if Path(destination).name == "clients.json":
            raise OSError("injected replace failure")
        return original_replace(source, destination)

    monkeypatch.setattr(security_state_module.os, "replace", fail_clients_replace)
    with pytest.raises(OSError, match="injected replace failure"):
        state.enroll_client(
            public_key,
            device_id="replace-failure",
            principal=PrincipalContext.local_owner(),
            capabilities=_CAPABILITIES,
            live_registry=live_registry,
        )

    assert state.list_clients() == ()
    with pytest.raises(KeyNotActive):
        live_registry.resolve_public_key(public_key)


def test_unexpected_live_enrollment_exception_rolls_persistence_back(tmp_path: Path):
    class ExplodingRegistry(SecurityRegistry):
        def enroll(self, *args, **kwargs):
            raise RuntimeError("injected live enrollment failure")

    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    public_key = _new_key()

    with pytest.raises(SecurityStateError, match="diverged"):
        state.enroll_client(
            public_key,
            device_id="unexpected-live-failure",
            principal=PrincipalContext.local_owner(),
            capabilities=_CAPABILITIES,
            live_registry=ExplodingRegistry(),
        )
    assert state.list_clients() == ()


def test_unexpected_live_revocation_exception_rolls_persistence_back(tmp_path: Path):
    class ExplodingRevokeRegistry(SecurityRegistry):
        def revoke(self, device_id: str):
            raise RuntimeError("injected live revocation failure")

    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    public_key = _new_key()
    state.enroll_client(
        public_key,
        device_id="unexpected-revoke-failure",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )
    live_registry = ExplodingRevokeRegistry()
    live_registry.enroll(
        public_key,
        device_id="unexpected-revoke-failure",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )

    with pytest.raises(SecurityStateError, match="diverged"):
        state.revoke_device("unexpected-revoke-failure", live_registry=live_registry)
    records = state.list_clients()
    assert len(records) == 1
    assert records[0]["active"] is True
    assert live_registry.resolve_public_key(public_key).device_id == "unexpected-revoke-failure"


class _BarrierLoadState(PersistentSecurityState):
    def __init__(self, directory: Path, barrier: threading.Barrier) -> None:
        super().__init__(directory)
        self._race_barrier = barrier
        self._race_armed = False

    def arm(self) -> None:
        self._race_armed = True

    def _load_client_records(self):
        records = super()._load_client_records()
        if self._race_armed:
            try:
                self._race_barrier.wait(timeout=0.25)
            except threading.BrokenBarrierError:
                pass
        return records


class _BarrierInitializeState(PersistentSecurityState):
    def __init__(self, directory: Path, barrier: threading.Barrier) -> None:
        super().__init__(directory)
        self._race_barrier = barrier

    def _write_private_json(self, path: Path, payload: dict[str, object]) -> None:
        if path.name == "server-curve.json":
            try:
                self._race_barrier.wait(timeout=0.25)
            except threading.BrokenBarrierError:
                pass
        super()._write_private_json(path, payload)


def test_parallel_offline_enrollment_cannot_lose_an_update(tmp_path: Path):
    directory = tmp_path / "security"
    barrier = threading.Barrier(2)
    first = _BarrierLoadState(directory, barrier)
    second = _BarrierLoadState(directory, barrier)
    first.initialize()
    first.arm()
    second.arm()
    identities = [("parallel-a", _new_key()), ("parallel-b", _new_key())]

    def enroll(state: PersistentSecurityState, device_id: str, public_key: str):
        return state.enroll_client(
            public_key,
            device_id=device_id,
            principal=PrincipalContext.local_owner(),
            capabilities=_CAPABILITIES,
        )

    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
        futures = [
            executor.submit(enroll, first, *identities[0]),
            executor.submit(enroll, second, *identities[1]),
        ]
        for future in futures:
            future.result(timeout=3.0)

    records = PersistentSecurityState(directory).list_clients()
    assert {record["device_id"] for record in records} == {"parallel-a", "parallel-b"}


def test_parallel_first_initialization_converges_on_one_daemon_identity(tmp_path: Path):
    directory = tmp_path / "security"
    barrier = threading.Barrier(2)
    first = _BarrierInitializeState(directory, barrier)
    second = _BarrierInitializeState(directory, barrier)

    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
        configs = list(executor.map(lambda state: state.initialize(), (first, second)))

    public_keys = {config.public_key for config in configs}
    assert len(public_keys) == 1
    persisted = PersistentSecurityState(directory).load_server_config()
    assert persisted.public_key in public_keys


def test_symlinked_state_lock_cannot_redirect_security_transaction(tmp_path: Path):
    state = PersistentSecurityState(tmp_path / "security")
    state.initialize()
    lock_path = state.directory / ".state.lock"
    lock_path.unlink()
    victim = tmp_path / "victim"
    victim.write_text("untouched", encoding="utf-8")
    lock_path.symlink_to(victim)

    with pytest.raises(SecurityStateError, match="lock"):
        state.enroll_client(
            _new_key(),
            device_id="lock-symlink",
            principal=PrincipalContext.local_owner(),
            capabilities=_CAPABILITIES,
        )
    assert victim.read_text(encoding="utf-8") == "untouched"
    assert state.list_clients() == ()


def test_device_id_controls_and_resource_bombs_fail_without_enrollment():
    registry = SecurityRegistry()
    principal = PrincipalContext.local_owner()
    invalid_device_ids = [
        "line\nbreak",
        "nul\x00byte",
        "x" * 129,
        "🙂" * 33,
    ]

    for device_id in invalid_device_ids:
        public_key = _new_key()
        with pytest.raises(ValueError):
            registry.enroll(
                public_key,
                device_id=device_id,
                principal=principal,
                capabilities=_CAPABILITIES,
            )
        with pytest.raises(KeyNotActive):
            registry.resolve_public_key(public_key)
