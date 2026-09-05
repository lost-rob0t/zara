from __future__ import annotations

import argparse
import concurrent.futures
import os
import socket
import sys
import tempfile
from pathlib import Path

import zmq

from zara.principals import PrincipalContext
from zara.runtime import bridge, events
from zara.runtime.commands import CommandReceipt
from zara.security import Capability
from zara.security_state import PersistentSecurityState
from zara.server import ServerState, ZaraServer
from zara.zmq_transport import TransportConfig


class _ReceiptFuture(concurrent.futures.Future):
    def __init__(self, receipt: CommandReceipt, publish) -> None:
        super().__init__()
        self._publish = publish
        self.set_result(receipt)

    def add_done_callback(self, callback, *, context=None) -> None:
        def after_route_registration(done) -> None:
            callback(done)
            self._publish()

        if context is None:
            super().add_done_callback(after_route_registration)
        else:
            super().add_done_callback(after_route_registration, context=context)


class _Supervisor:
    def __init__(self) -> None:
        self.state = ServerState.NEW
        self.bus = bridge.RuntimeEventBus()
        self._turn = 0

    def start(self, principal: PrincipalContext):
        self.state = ServerState.READY
        return object()

    def open_principal(self, principal: PrincipalContext):
        return object()

    def subscribe(self, principal: PrincipalContext, *, maxsize: int = 0):
        return self.bus.subscribe(maxsize=maxsize)

    def submit(self, principal: PrincipalContext, command):
        self._turn += 1
        turn_id = f"android-stock-turn-{self._turn}"
        conversation_id = getattr(command, "conversation_id", None)
        receipt = CommandReceipt(request_id=command.request_id, turn_id=turn_id)

        def publish() -> None:
            self.bus.publish(
                events.TurnStarted(turn_id=turn_id, conversation_id=conversation_id)
            )
            self.bus.publish(
                events.AssistantStarted(turn_id=turn_id, conversation_id=conversation_id)
            )
            self.bus.publish(
                events.AssistantComplete(
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                    text="stock server response",
                    success=True,
                )
            )
            self.bus.publish(
                events.AgentCompleted(
                    turn_id=turn_id,
                    conversation_id=conversation_id,
                    success=True,
                )
            )

        return _ReceiptFuture(receipt, publish)

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


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--fixture-file", required=True)
    args = parser.parse_args()

    fixture_file = Path(args.fixture_file).resolve()
    endpoint = _tcp_endpoint()
    client_public, client_secret = zmq.curve_keypair()

    with tempfile.TemporaryDirectory(prefix="zara-android-stock-") as temporary:
        state = PersistentSecurityState(Path(temporary) / "security")
        server_curve = state.initialize()
        state.enroll_client(
            client_public,
            device_id="android-jvm-fixture",
            principal=PrincipalContext.local_owner(),
            capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
        )
        server = ZaraServer(
            supervisor=_Supervisor(),
            endpoint=endpoint,
            security_state=state,
            gateway_transport_config=TransportConfig(
                sndhwm=8,
                rcvhwm=8,
                heartbeat_interval_ms=100,
                heartbeat_timeout_ms=500,
                linger_ms=0,
                request_timeout=2.0,
                poll_interval_ms=5,
                event_queue_size=16,
                pending_request_limit=16,
            ),
            shutdown_timeout=1.0,
        )
        server.start()
        try:
            _write_fixture(
                fixture_file,
                {
                    "endpoint": endpoint,
                    "server_public": server_curve.public_key.decode("ascii"),
                    "client_public": client_public.decode("ascii"),
                    "client_secret": client_secret.decode("ascii"),
                },
            )
            print("READY", flush=True)
            for line in sys.stdin:
                if line.strip() == "STOP":
                    break
        finally:
            server.stop()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
