from __future__ import annotations

import argparse
import concurrent.futures
import os
import socket
import sys
import tempfile
import threading
from pathlib import Path

import zmq

from zara.principals import PrincipalContext
from zara.runtime import bridge, events
from zara.runtime.commands import CommandReceipt
from zara.security import Capability
from zara.security_state import PersistentSecurityState
from zara.security_transport import CurveClientConfig
from zara.server import ServerState, ZaraServer
from zara.zmq_transport import TransportConfig, ZmqZaraClient


def _trace(phase: str, outcome: str) -> None:
    print(f"STOCK_INTEROP phase={phase} outcome={outcome}", file=sys.stderr, flush=True)


class _AcceptanceBarrier:
    def __init__(self) -> None:
        self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self._socket.bind(("127.0.0.1", 0))
        self._socket.listen(1)
        self.host, self.port = self._socket.getsockname()
        self._started = False

    def publish_after_client_acceptance(self, publish) -> None:
        if self._started:
            raise RuntimeError("acceptance barrier already armed")
        self._started = True
        _trace("turn.accepted", "barrier-armed")

        def wait_and_publish() -> None:
            try:
                connection, _ = self._socket.accept()
                with connection:
                    if connection.recv(1) != b"A":
                        return
                _trace("turn.accepted", "client-observed")
                publish()
            finally:
                self._socket.close()

        threading.Thread(
            target=wait_and_publish,
            name="zara-android-acceptance-barrier",
            daemon=True,
        ).start()

    def close(self) -> None:
        try:
            self._socket.close()
        except OSError:
            pass


class _ReceiptFuture(concurrent.futures.Future):
    def __init__(self, receipt: CommandReceipt, publish, barrier: _AcceptanceBarrier) -> None:
        super().__init__()
        self._publish = publish
        self._barrier = barrier
        self.set_result(receipt)

    def add_done_callback(self, callback, *, context=None) -> None:
        def after_route_registration(done) -> None:
            _trace("turn.accepted", "callback-enter")
            callback(done)
            _trace("turn.accepted", "callback-returned")
            self._barrier.publish_after_client_acceptance(self._publish)

        if context is None:
            super().add_done_callback(after_route_registration)
        else:
            super().add_done_callback(after_route_registration, context=context)


class _Supervisor:
    def __init__(self, barrier: _AcceptanceBarrier) -> None:
        self.state = ServerState.NEW
        self.bus = bridge.RuntimeEventBus()
        self._turn = 0
        self._barrier = barrier

    def start(self, principal: PrincipalContext):
        self.state = ServerState.READY
        return object()

    def open_principal(self, principal: PrincipalContext):
        _trace("principal", "opened")
        return object()

    def subscribe(self, principal: PrincipalContext, *, maxsize: int = 0):
        _trace("subscription", "opened")
        return self.bus.subscribe(maxsize=maxsize)

    def publish(self, principal: PrincipalContext, event):
        return self.bus.publish(event)

    def _publish_traced(self, event: events.RuntimeEvent) -> None:
        envelope = self.bus.publish(event)
        _trace(
            "runtime.event",
            f"published:{event.__class__.__name__}:seq={envelope.sequence}",
        )

    def submit(self, principal: PrincipalContext, command):
        _trace("turn.submit", "received")
        self._turn += 1
        turn_id = f"android-stock-turn-{self._turn}"
        conversation_id = getattr(command, "conversation_id", None)
        receipt = CommandReceipt(request_id=command.request_id, turn_id=turn_id)

        def publish() -> None:
            _trace("runtime.events", "publishing")
            self._publish_traced(
                events.TurnStarted(turn_id=turn_id, conversation_id=conversation_id)
            )
            self._publish_traced(
                events.AssistantStarted(turn_id=turn_id, conversation_id=conversation_id)
            )
            self._publish_traced(
                events.AssistantComplete(
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                    text="stock server response",
                    success=True,
                )
            )
            self._publish_traced(
                events.AgentCompleted(
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                    success=True,
                )
            )

        return _ReceiptFuture(receipt, publish, self._barrier)

    def shutdown(self) -> bool:
        self.state = ServerState.STOPPED
        return True


def _tcp_endpoint() -> str:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return f"tcp://127.0.0.1:{probe.getsockname()[1]}"


def _write_fixture(path: Path, values: dict[str, str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as output:
            for key, value in values.items():
                output.write(f"{key}={value}\n")
            output.flush()
            os.fsync(output.fileno())
    except BaseException:
        try:
            os.close(descriptor)
        except OSError:
            pass
        raise


def _wait_for_transport_ready(
    *,
    endpoint: str,
    server_public: str,
    client_public: str,
    client_secret: str,
    config: TransportConfig,
) -> None:
    """Prove authenticated ZARA/1 hello before exposing the Android fixture."""
    probe = ZmqZaraClient(
        endpoint,
        config=config,
        curve_client=CurveClientConfig(
            public_key=client_public,
            secret_key=client_secret,
            server_public_key=server_public,
        ),
    )
    try:
        probe.start().result(timeout=5.0)
        _trace("transport.probe", "ready")
    finally:
        probe.close()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--fixture-file", required=True)
    args = parser.parse_args()

    fixture_file = Path(args.fixture_file).resolve()
    endpoint = _tcp_endpoint()
    client_public, client_secret = zmq.curve_keypair()
    probe_public, probe_secret = zmq.curve_keypair()
    barrier = _AcceptanceBarrier()

    with tempfile.TemporaryDirectory(prefix="zara-android-stock-") as temporary:
        state = PersistentSecurityState(Path(temporary) / "security")
        server_curve = state.initialize()
        state.enroll_client(
            client_public,
            device_id="android-jvm-fixture",
            principal=PrincipalContext.local_owner(),
            capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
        )
        state.enroll_client(
            probe_public,
            device_id="android-jvm-readiness-probe",
            principal=PrincipalContext.local_owner(),
            capabilities={Capability.SESSION_BASIC},
        )
        transport_config = TransportConfig(
            sndhwm=8,
            rcvhwm=8,
            heartbeat_interval_ms=100,
            heartbeat_timeout_ms=500,
            linger_ms=0,
            request_timeout=2.0,
            poll_interval_ms=5,
            event_queue_size=16,
            pending_request_limit=16,
        )
        server = ZaraServer(
            supervisor=_Supervisor(barrier),
            endpoint=endpoint,
            runtime_dir=Path(temporary) / "runtime",
            security_state=state,
            gateway_transport_config=transport_config,
            shutdown_timeout=1.0,
        )
        server.start()
        _trace("server", "ready")
        try:
            _wait_for_transport_ready(
                endpoint=endpoint,
                server_public=server_curve.public_key.decode("ascii"),
                client_public=probe_public.decode("ascii"),
                client_secret=probe_secret.decode("ascii"),
                config=transport_config,
            )
            _write_fixture(
                fixture_file,
                {
                    "endpoint": endpoint,
                    "server_public": server_curve.public_key.decode("ascii"),
                    "client_public": client_public.decode("ascii"),
                    "client_secret": client_secret.decode("ascii"),
                    "acceptance_host": barrier.host,
                    "acceptance_port": str(barrier.port),
                    "security_admin_path": os.fspath(state.control_socket_path),
                },
            )
            print("READY", flush=True)
            for line in sys.stdin:
                if line.strip() == "STOP":
                    break
        finally:
            barrier.close()
            server.stop()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
