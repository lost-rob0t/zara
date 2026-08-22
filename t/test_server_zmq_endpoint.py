import concurrent.futures
import os
import stat
from pathlib import Path

import pytest
import zmq

from zara.server import (
    PrincipalContext,
    ServerState,
    ZaraServer,
    _parser,
    default_zmq_endpoint,
)


class RecordingLease:
    def __init__(self, events, path):
        self.events = events
        self.path = Path(path)

    def acquire(self):
        self.events.append("lease.acquire")
        self.path.mkdir(parents=True, exist_ok=True)
        return self.path / "zara-server.lock"

    def release(self):
        self.events.append("lease.release")


class RecordingSupervisor:
    def __init__(self, events):
        self.events = events
        self.state = ServerState.NEW

    def start(self, principal):
        assert isinstance(principal, PrincipalContext)
        self.events.append("supervisor.start")
        self.state = ServerState.READY

    def shutdown(self):
        self.events.append("supervisor.shutdown")
        self.state = ServerState.STOPPED
        return True


class RecordingGateway:
    def __init__(self, events, endpoint, *, fail_start=False):
        self.events = events
        self.endpoint = endpoint
        self.fail_start = fail_start

    def start(self):
        self.events.append("gateway.start")
        future = concurrent.futures.Future()
        if self.fail_start:
            future.set_exception(RuntimeError("bind failed"))
        else:
            future.set_result(True)
        return future

    def close(self, timeout=None):
        self.events.append("gateway.close")


def _ipc_path(endpoint: str) -> str:
    assert endpoint.startswith("ipc://")
    return endpoint.removeprefix("ipc://")


def test_default_zmq_endpoint_is_owner_private_ipc_under_short_runtime_directory():
    runtime_dir = Path("/run/user/1000/zarathushtra")

    endpoint = default_zmq_endpoint(runtime_dir)

    assert endpoint == f"ipc://{runtime_dir / 'zara-server.sock'}"
    assert not endpoint.startswith("tcp://")


def test_default_zmq_endpoint_accepts_string_runtime_directory():
    runtime_dir = "/run/user/1000/zarathushtra"

    endpoint = default_zmq_endpoint(runtime_dir)

    assert endpoint == f"ipc://{Path(runtime_dir) / 'zara-server.sock'}"


def test_long_runtime_directory_uses_deterministic_private_bounded_fallback(tmp_path):
    max_len = getattr(zmq, "IPC_PATH_MAX_LEN", 0)
    if not max_len:
        pytest.skip("platform does not expose an IPC path limit")
    runtime_dir = tmp_path / ("long-segment-" * 10)

    first = default_zmq_endpoint(runtime_dir)
    second = default_zmq_endpoint(runtime_dir)

    path = Path(_ipc_path(first))
    assert first == second
    assert len(os.fsencode(path)) <= max_len
    assert path.name.startswith("zara-server-")
    assert path.name.endswith(".sock")
    assert path.parent != runtime_dir
    assert path.parent.name == f"zarathushtra-{os.getuid()}"
    assert path.parent.is_dir()
    assert os.lstat(path.parent).st_uid == os.getuid()
    assert stat.S_IMODE(os.lstat(path.parent).st_mode) == 0o700


def test_distinct_long_runtime_directories_do_not_collide(tmp_path):
    max_len = getattr(zmq, "IPC_PATH_MAX_LEN", 0)
    if not max_len:
        pytest.skip("platform does not expose an IPC path limit")
    first_dir = tmp_path / ("a" * 120)
    second_dir = tmp_path / ("b" * 120)

    assert default_zmq_endpoint(first_dir) != default_zmq_endpoint(second_dir)


def test_ipc_limit_is_measured_in_filesystem_bytes_not_unicode_characters(tmp_path):
    max_len = getattr(zmq, "IPC_PATH_MAX_LEN", 0)
    if not max_len:
        pytest.skip("platform does not expose an IPC path limit")
    runtime_dir = tmp_path / ("é" * 60)

    endpoint = default_zmq_endpoint(runtime_dir)

    assert len(os.fsencode(_ipc_path(endpoint))) <= max_len


def test_server_starts_gateway_before_ready_and_stops_it_before_runtime_and_lease(tmp_path):
    events = []
    runtime_dir = tmp_path / "runtime"
    lease = RecordingLease(events, runtime_dir)
    supervisor = RecordingSupervisor(events)
    created = []

    def gateway_factory(endpoint, *, supervisor, principal):
        assert endpoint == default_zmq_endpoint(runtime_dir)
        gateway = RecordingGateway(events, endpoint)
        created.append(gateway)
        return gateway

    server = ZaraServer(
        supervisor=supervisor,
        lease=lease,
        runtime_dir=runtime_dir,
        gateway_factory=gateway_factory,
        shutdown_timeout=1.0,
    )

    assert server.start() is ServerState.READY
    assert created
    assert events == ["lease.acquire", "supervisor.start", "gateway.start"]

    assert server.stop() is True
    assert events == [
        "lease.acquire",
        "supervisor.start",
        "gateway.start",
        "gateway.close",
        "supervisor.shutdown",
        "lease.release",
    ]
    assert server.state is ServerState.STOPPED


def test_gateway_start_failure_closes_gateway_rolls_back_runtime_and_releases_lease(tmp_path):
    events = []
    runtime_dir = tmp_path / "runtime"
    lease = RecordingLease(events, runtime_dir)
    supervisor = RecordingSupervisor(events)

    def gateway_factory(endpoint, *, supervisor, principal):
        return RecordingGateway(events, endpoint, fail_start=True)

    server = ZaraServer(
        supervisor=supervisor,
        lease=lease,
        runtime_dir=runtime_dir,
        gateway_factory=gateway_factory,
        shutdown_timeout=1.0,
    )

    with pytest.raises(RuntimeError, match="bind failed"):
        server.start()

    assert events == [
        "lease.acquire",
        "supervisor.start",
        "gateway.start",
        "gateway.close",
        "supervisor.shutdown",
        "lease.release",
    ]
    assert server.state is ServerState.FAILED


def test_server_cli_help_exposes_local_endpoint_and_warns_remote_auth_is_not_available():
    help_text = _parser().format_help()

    assert "--endpoint" in help_text
    assert "local" in help_text.lower()
    assert "#130" in help_text or "authentication" in help_text.lower()


def test_server_rejects_tcp_endpoint_before_authentication_slice(tmp_path):
    with pytest.raises(ValueError, match="TCP|tcp|authentication"):
        ZaraServer(runtime_dir=tmp_path, endpoint="tcp://127.0.0.1:5555")


def test_server_rejects_explicit_overlong_ipc_endpoint_before_bind():
    max_len = getattr(zmq, "IPC_PATH_MAX_LEN", 0)
    if not max_len:
        pytest.skip("platform does not expose an IPC path limit")
    endpoint = "ipc:///" + ("x" * (max_len + 1))

    with pytest.raises(ValueError, match="IPC|ipc|length|long"):
        ZaraServer(endpoint=endpoint)
