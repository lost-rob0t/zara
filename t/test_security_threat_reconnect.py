from __future__ import annotations

import concurrent.futures
import queue
import socket as net_socket

import pytest
import zmq

from zara.principals import PrincipalContext
from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.runtime import bridge
from zara.runtime.commands import CommandReceipt
from zara.security import Capability, SecurityLimits, SecurityRegistry
from zara.security_gateway import SecureZaraZmqGateway
from zara.security_transport import CurveClientConfig, CurveServerConfig, configure_curve_client_socket
from zara.server import ServerState
from zara.zmq_transport import TransportConfig, apply_socket_options


class _Supervisor:
    state = ServerState.READY

    def __init__(self) -> None:
        self.bus = bridge.RuntimeEventBus()

    def subscribe(self, principal, *, maxsize=0):
        return self.bus.subscribe(maxsize=maxsize)


class _BlockingSupervisor(_Supervisor):
    def __init__(self) -> None:
        super().__init__()
        self.submitted: queue.Queue[tuple[PrincipalContext, object, concurrent.futures.Future]] = queue.Queue()

    def submit(self, principal, command):
        future = concurrent.futures.Future()
        self.submitted.put((principal, command, future))
        return future


@pytest.fixture
def zmq_context():
    context = zmq.Context()
    try:
        yield context
    finally:
        context.term()


def _keypair() -> tuple[str, str]:
    public, secret = zmq.curve_keypair()
    return public.decode("ascii"), secret.decode("ascii")


def _endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return f"tcp://127.0.0.1:{probe.getsockname()[1]}"


def _config() -> TransportConfig:
    return TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        heartbeat_interval_ms=50,
        heartbeat_timeout_ms=250,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=5,
        event_queue_size=8,
        pending_request_limit=8,
    )


def _dealer(context, endpoint, config, public, secret, server_public, route: bytes):
    dealer = context.socket(zmq.DEALER)
    apply_socket_options(dealer, config, router=False)
    dealer.setsockopt(zmq.ROUTING_ID, route)
    configure_curve_client_socket(
        dealer,
        CurveClientConfig(
            public_key=public,
            secret_key=secret,
            server_public_key=server_public,
        ),
    )
    dealer.connect(endpoint)
    return dealer


def _receive(dealer: zmq.Socket, *, timeout_ms: int = 1500):
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)).get(dealer) == zmq.POLLIN
    return decode_message(dealer.recv_multipart()).message


def _hello(dealer: zmq.Socket, message_id: str):
    dealer.send_multipart(
        encode_message(
            ProtocolMessage(
                type="hello",
                id=message_id,
                timestamp_ns=1,
                payload_count=0,
                body={"versions": [1]},
            )
        )
    )
    return _receive(dealer)


def _submit_turn(dealer: zmq.Socket, *, session_id: str, request_id: str):
    dealer.send_multipart(
        encode_message(
            ProtocolMessage(
                type="turn.submit",
                id=request_id,
                session_id=session_id,
                timestamp_ns=2,
                payload_count=0,
                body={"text": request_id, "context_ids": []},
            )
        )
    )


def test_same_authenticated_device_cannot_exhaust_principal_connection_quota_with_route_ids(
    zmq_context,
):
    endpoint = _endpoint()
    config = _config()
    server_public, server_secret = _keypair()
    client_public, client_secret = _keypair()
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=PrincipalContext("user:alice", kind="authenticated"),
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC},
    )
    gateway = SecureZaraZmqGateway(
        endpoint,
        supervisor=_Supervisor(),
        context=zmq_context,
        config=config,
        security_registry=registry,
        curve_server=CurveServerConfig(public_key=server_public, secret_key=server_secret),
        security_limits=SecurityLimits(max_connections=1),
    )
    gateway.start().result(timeout=1.0)
    first = _dealer(
        zmq_context,
        endpoint,
        config,
        client_public,
        client_secret,
        server_public,
        b"device-route-one",
    )
    second = None
    try:
        first_hello = _hello(first, "first-route")
        assert first_hello.type == "hello.ok"

        # Model process death / fresh DEALER identity: same cryptographic device
        # credential, new ROUTER routing id. One credential must consume at most
        # one principal connection slot; otherwise a single enrolled key can
        # self-DoS its principal simply by reconnecting with fresh route ids.
        first.close(0)
        second = _dealer(
            zmq_context,
            endpoint,
            config,
            client_public,
            client_secret,
            server_public,
            b"device-route-two",
        )
        second_hello = _hello(second, "second-route")
        assert second_hello.type == "hello.ok"
        assert second_hello.session_id != first_hello.session_id
    finally:
        if not first.closed:
            first.close(0)
        if second is not None:
            second.close(0)
        gateway.close(timeout=1.0)


def test_runtime_completion_releases_quota_even_when_original_route_vanished(zmq_context):
    endpoint = _endpoint()
    config = _config()
    server_public, server_secret = _keypair()
    client_public, client_secret = _keypair()
    principal = PrincipalContext("user:alice", kind="authenticated")
    registry = SecurityRegistry()
    registry.enroll(
        client_public,
        principal=principal,
        device_id="alice-phone",
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    supervisor = _BlockingSupervisor()
    gateway = SecureZaraZmqGateway(
        endpoint,
        supervisor=supervisor,
        context=zmq_context,
        config=config,
        security_registry=registry,
        curve_server=CurveServerConfig(public_key=server_public, secret_key=server_secret),
        security_limits=SecurityLimits(
            max_connections=1,
            max_concurrent_requests=1,
            requests_per_window=20,
        ),
    )
    gateway.start().result(timeout=1.0)
    first = _dealer(
        zmq_context,
        endpoint,
        config,
        client_public,
        client_secret,
        server_public,
        b"pending-route-one",
    )
    second = None
    try:
        first_hello = _hello(first, "pending-first-hello")
        _submit_turn(first, session_id=first_hello.session_id, request_id="pending-one")
        _principal, first_command, first_future = supervisor.submitted.get(timeout=1.0)
        assert first_command.request_id == "pending-one"

        first.close(0)
        second = _dealer(
            zmq_context,
            endpoint,
            config,
            client_public,
            client_secret,
            server_public,
            b"pending-route-two",
        )
        second_hello = _hello(second, "pending-second-hello")
        assert second_hello.type == "hello.ok"

        # Completing the old command has no surviving route to enqueue to. The
        # completion itself must still release the principal's concurrent slot.
        first_future.set_result(CommandReceipt(request_id="pending-one", turn_id="turn-one"))

        _submit_turn(second, session_id=second_hello.session_id, request_id="pending-two")
        second_principal, second_command, second_future = supervisor.submitted.get(timeout=1.0)
        assert second_principal == principal
        assert second_command.request_id == "pending-two"
        second_future.set_result(CommandReceipt(request_id="pending-two", turn_id="turn-two"))
        accepted = _receive(second)
        assert accepted.type == "turn.accepted"
        assert accepted.reply_to == "pending-two"
    finally:
        if not first.closed:
            first.close(0)
        if second is not None:
            second.close(0)
        gateway.close(timeout=1.0)
