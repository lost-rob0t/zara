import concurrent.futures
from pathlib import Path

import pytest

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


def test_default_zmq_endpoint_is_owner_private_ipc_under_runtime_directory(tmp_path):
    runtime_dir = tmp_path / "runtime"

    endpoint = default_zmq_endpoint(runtime_dir)

    assert endpoint == f"ipc://{runtime_dir / 'zara-server.sock'}"
    assert not endpoint.startswith("tcp://")


def test_default_zmq_endpoint_accepts_string_runtime_directory(tmp_path):
    runtime_dir = tmp_path / "runtime"

    endpoint = default_zmq_endpoint(str(runtime_dir))

    assert endpoint == f"ipc://{Path(runtime_dir) / 'zara-server.sock'}"


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


def test_gateway_start_failure_rolls_back_runtime_and_releases_lease(tmp_path):
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
