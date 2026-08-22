import concurrent.futures
from pathlib import Path

from zara.server import PrincipalContext, ServerState, ZaraServer


def completed(value=None):
    future = concurrent.futures.Future()
    future.set_result(value)
    return future


class RecordingLease:
    def __init__(self, runtime_dir: Path, events: list[str]):
        self.path = None
        self.held = False
        self._runtime_dir = runtime_dir
        self._events = events

    def acquire(self):
        self._events.append("lease.acquire")
        self._runtime_dir.mkdir(parents=True, exist_ok=True)
        self.path = self._runtime_dir / "zara-server.lock"
        self.held = True
        return self.path

    def release(self):
        self._events.append("lease.release")
        self.held = False


class RecordingSupervisor:
    def __init__(self, events: list[str]):
        self._events = events
        self.state = ServerState.NEW

    def start(self, principal):
        assert isinstance(principal, PrincipalContext)
        self._events.append("supervisor.start")
        self.state = ServerState.READY

    def shutdown(self):
        self._events.append("supervisor.shutdown")
        self.state = ServerState.STOPPED
        return True


class RecordingGateway:
    def __init__(self, endpoint, supervisor, principal, events):
        self.endpoint = endpoint
        self.supervisor = supervisor
        self.principal = principal
        self._events = events
        self.closed = False

    def start(self):
        self._events.append("gateway.start")
        return completed(True)

    def close(self, timeout=None):
        self._events.append("gateway.close")
        self.closed = True


def test_server_starts_gateway_before_reporting_ready_and_uses_lease_runtime_dir(tmp_path):
    events = []
    runtime_dir = tmp_path / "runtime"
    lease = RecordingLease(runtime_dir, events)
    supervisor = RecordingSupervisor(events)
    created = []

    def gateway_factory(endpoint, *, supervisor, principal):
        gateway = RecordingGateway(endpoint, supervisor, principal, events)
        created.append(gateway)
        return gateway

    server = ZaraServer(
        supervisor=supervisor,
        lease=lease,
        gateway_factory=gateway_factory,
        shutdown_timeout=0.2,
    )

    assert server.start() is ServerState.READY
    assert events == ["lease.acquire", "supervisor.start", "gateway.start"]
    assert len(created) == 1
    assert created[0].endpoint == f"ipc://{runtime_dir / 'zara-server.sock'}"
    assert created[0].principal == server.principal
    assert created[0].supervisor is supervisor

    assert server.stop()


def test_server_closes_gateway_before_runtime_shutdown_and_lease_release(tmp_path):
    events = []
    runtime_dir = tmp_path / "runtime"
    lease = RecordingLease(runtime_dir, events)
    supervisor = RecordingSupervisor(events)

    def gateway_factory(endpoint, *, supervisor, principal):
        return RecordingGateway(endpoint, supervisor, principal, events)

    server = ZaraServer(
        supervisor=supervisor,
        lease=lease,
        gateway_factory=gateway_factory,
        shutdown_timeout=0.2,
    )

    server.start()
    events.clear()

    assert server.stop()
    assert events == ["gateway.close", "supervisor.shutdown", "lease.release"]
    assert server.state is ServerState.STOPPED


def test_gateway_start_failure_shuts_down_runtime_and_releases_lease(tmp_path):
    events = []
    runtime_dir = tmp_path / "runtime"
    lease = RecordingLease(runtime_dir, events)
    supervisor = RecordingSupervisor(events)

    class FailedGateway(RecordingGateway):
        def start(self):
            self._events.append("gateway.start")
            future = concurrent.futures.Future()
            future.set_exception(RuntimeError("bind failed"))
            return future

    def gateway_factory(endpoint, *, supervisor, principal):
        return FailedGateway(endpoint, supervisor, principal, events)

    server = ZaraServer(
        supervisor=supervisor,
        lease=lease,
        gateway_factory=gateway_factory,
        shutdown_timeout=0.2,
    )

    try:
        server.start()
    except RuntimeError as error:
        assert str(error) == "bind failed"
    else:
        raise AssertionError("gateway startup failure must fail server startup")

    assert events == [
        "lease.acquire",
        "supervisor.start",
        "gateway.start",
        "gateway.close",
        "supervisor.shutdown",
        "lease.release",
    ]
    assert server.state is ServerState.FAILED
    assert not lease.held
