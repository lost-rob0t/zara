from __future__ import annotations

import multiprocessing
import os
import threading
from pathlib import Path

import pytest
import zmq

import zara.security_state as security_state_module
from zara.principals import PrincipalContext
from zara.security import Capability, KeyNotActive, SecurityRegistry
from zara.security_state import PersistentSecurityState


pytestmark = pytest.mark.skipif(
    "fork" not in multiprocessing.get_all_start_methods(),
    reason="security state process-chaos gate requires POSIX fork/flock semantics",
)

_CAPABILITIES = {Capability.SESSION_BASIC, Capability.TURN_SUBMIT}


def _new_key() -> str:
    public_key, _secret_key = zmq.curve_keypair()
    return public_key.decode("ascii")


def _join_clean(processes: list[multiprocessing.Process], *, timeout: float = 8.0) -> None:
    for process in processes:
        process.join(timeout)
    stuck = [process for process in processes if process.is_alive()]
    for process in stuck:
        process.kill()
        process.join(1.0)
    assert not stuck, "security chaos child process failed to terminate"


class _BarrierLoadState(PersistentSecurityState):
    def __init__(self, directory: str, barrier) -> None:
        super().__init__(directory)
        self._race_barrier = barrier

    def _load_client_records(self):
        records = super()._load_client_records()
        try:
            self._race_barrier.wait(timeout=0.5)
        except threading.BrokenBarrierError:
            pass
        return records


class _BarrierInitializeState(PersistentSecurityState):
    def __init__(self, directory: str, barrier) -> None:
        super().__init__(directory)
        self._race_barrier = barrier

    def _write_private_json(self, path: Path, payload: dict[str, object]) -> None:
        if path.name == "server-curve.json":
            try:
                self._race_barrier.wait(timeout=0.5)
            except threading.BrokenBarrierError:
                pass
        super()._write_private_json(path, payload)


class _ExitOnEnrollRegistry(SecurityRegistry):
    def enroll(self, *args, **kwargs):
        os._exit(86)


class _ExitOnRevokeRegistry(SecurityRegistry):
    def revoke(self, device_id: str):
        os._exit(87)


class _ExitBeforeSaveState(PersistentSecurityState):
    def _save_client_records(self, records):
        os._exit(88)


def _offline_enroll_worker(
    directory: str,
    device_id: str,
    public_key: str,
    start,
) -> None:
    start.wait(5.0)
    PersistentSecurityState(directory).enroll_client(
        public_key,
        device_id=device_id,
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )


def _barrier_enroll_worker(
    directory: str,
    device_id: str,
    public_key: str,
    barrier,
) -> None:
    _BarrierLoadState(directory, barrier).enroll_client(
        public_key,
        device_id=device_id,
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )


def _initialize_worker(directory: str, barrier, result_path: str) -> None:
    config = _BarrierInitializeState(directory, barrier).initialize()
    public_key = config.public_key
    if isinstance(public_key, bytes):
        public_key = public_key.decode("ascii")
    Path(result_path).write_text(public_key, encoding="ascii")


def _crash_after_enroll_persist_worker(directory: str, public_key: str) -> None:
    state = PersistentSecurityState(directory)
    state.enroll_client(
        public_key,
        device_id="crash-after-enroll-persist",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
        live_registry=_ExitOnEnrollRegistry(),
    )
    os._exit(99)


def _crash_after_revoke_persist_worker(directory: str, public_key: str) -> None:
    live_registry = _ExitOnRevokeRegistry()
    live_registry.enroll(
        public_key,
        device_id="crash-after-revoke-persist",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )
    PersistentSecurityState(directory).revoke_device(
        "crash-after-revoke-persist",
        live_registry=live_registry,
    )
    os._exit(99)


def _crash_before_enroll_persist_worker(directory: str, public_key: str) -> None:
    _ExitBeforeSaveState(directory).enroll_client(
        public_key,
        device_id="crash-before-enroll-persist",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )
    os._exit(99)


def _crash_before_atomic_replace_worker(directory: str, public_key: str) -> None:
    original_replace = security_state_module.os.replace

    def kill_before_clients_replace(source, destination):
        if Path(destination).name == "clients.json":
            os._exit(89)
        return original_replace(source, destination)

    security_state_module.os.replace = kill_before_clients_replace
    PersistentSecurityState(directory).enroll_client(
        public_key,
        device_id="crash-before-replace",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )
    os._exit(99)


def test_real_process_enrollment_storm_preserves_every_committed_update(tmp_path: Path):
    ctx = multiprocessing.get_context("fork")
    directory = str(tmp_path / "security")
    PersistentSecurityState(directory).initialize()
    start = ctx.Event()
    identities = [(f"process-{index}", _new_key()) for index in range(12)]
    processes = [
        ctx.Process(target=_offline_enroll_worker, args=(directory, device_id, key, start))
        for device_id, key in identities
    ]
    for process in processes:
        process.start()
    start.set()
    _join_clean(processes)
    assert all(process.exitcode == 0 for process in processes)

    records = PersistentSecurityState(directory).list_clients()
    assert {record["device_id"] for record in records} == {
        device_id for device_id, _key in identities
    }


def test_real_process_barrier_forces_lost_update_interleaving_but_lock_serializes_it(tmp_path: Path):
    ctx = multiprocessing.get_context("fork")
    directory = str(tmp_path / "security")
    PersistentSecurityState(directory).initialize()
    barrier = ctx.Barrier(2)
    identities = [("barrier-a", _new_key()), ("barrier-b", _new_key())]
    processes = [
        ctx.Process(target=_barrier_enroll_worker, args=(directory, device_id, key, barrier))
        for device_id, key in identities
    ]
    for process in processes:
        process.start()
    _join_clean(processes)
    assert all(process.exitcode == 0 for process in processes)

    records = PersistentSecurityState(directory).list_clients()
    assert {record["device_id"] for record in records} == {"barrier-a", "barrier-b"}


def test_real_process_first_boot_race_converges_on_one_curve_identity(tmp_path: Path):
    ctx = multiprocessing.get_context("fork")
    directory = str(tmp_path / "security")
    barrier = ctx.Barrier(4)
    result_paths = [str(tmp_path / f"identity-{index}.txt") for index in range(4)]
    processes = [
        ctx.Process(target=_initialize_worker, args=(directory, barrier, result_path))
        for result_path in result_paths
    ]
    for process in processes:
        process.start()
    _join_clean(processes)
    assert all(process.exitcode == 0 for process in processes)

    returned = {Path(path).read_text(encoding="ascii") for path in result_paths}
    assert len(returned) == 1
    assert PersistentSecurityState(directory).server_public_key() in returned


def test_process_death_before_persistence_leaves_no_enrollment(tmp_path: Path):
    ctx = multiprocessing.get_context("fork")
    directory = str(tmp_path / "security")
    state = PersistentSecurityState(directory)
    state.initialize()
    public_key = _new_key()
    process = ctx.Process(target=_crash_before_enroll_persist_worker, args=(directory, public_key))
    process.start()
    _join_clean([process])
    assert process.exitcode == 88
    assert state.list_clients() == ()
    with pytest.raises(KeyNotActive):
        state.load_registry().resolve_public_key(public_key)


def test_process_death_before_atomic_replace_preserves_old_canonical_registry(tmp_path: Path):
    ctx = multiprocessing.get_context("fork")
    directory = str(tmp_path / "security")
    state = PersistentSecurityState(directory)
    state.initialize()
    public_key = _new_key()
    process = ctx.Process(target=_crash_before_atomic_replace_worker, args=(directory, public_key))
    process.start()
    _join_clean([process])
    assert process.exitcode == 89
    assert state.list_clients() == ()
    with pytest.raises(KeyNotActive):
        state.load_registry().resolve_public_key(public_key)


def test_process_death_after_enrollment_durable_commit_recovers_new_authority(tmp_path: Path):
    ctx = multiprocessing.get_context("fork")
    directory = str(tmp_path / "security")
    state = PersistentSecurityState(directory)
    state.initialize()
    public_key = _new_key()
    process = ctx.Process(target=_crash_after_enroll_persist_worker, args=(directory, public_key))
    process.start()
    _join_clean([process])
    assert process.exitcode == 86

    restarted = PersistentSecurityState(directory)
    assert restarted.load_registry().resolve_public_key(public_key).device_id == (
        "crash-after-enroll-persist"
    )
    # A process death must also release the transaction flock.
    restarted.enroll_client(
        _new_key(),
        device_id="post-crash-lock-release",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )


def test_process_death_after_revocation_durable_commit_recovers_revoked_authority(tmp_path: Path):
    ctx = multiprocessing.get_context("fork")
    directory = str(tmp_path / "security")
    state = PersistentSecurityState(directory)
    state.initialize()
    public_key = _new_key()
    state.enroll_client(
        public_key,
        device_id="crash-after-revoke-persist",
        principal=PrincipalContext.local_owner(),
        capabilities=_CAPABILITIES,
    )
    process = ctx.Process(target=_crash_after_revoke_persist_worker, args=(directory, public_key))
    process.start()
    _join_clean([process])
    assert process.exitcode == 87

    restarted = PersistentSecurityState(directory)
    records = restarted.list_clients()
    assert len(records) == 1
    assert records[0]["active"] is False
    with pytest.raises(KeyNotActive):
        restarted.load_registry().resolve_public_key(public_key)
