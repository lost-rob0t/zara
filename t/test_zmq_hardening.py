from __future__ import annotations

import time
from types import SimpleNamespace

import pytest
import zmq

from zara.client import ZaraClientState
from zara.protocol import ProtocolMessage, decode_message
from zara.server import PrincipalContext
from zara.zmq_hardening import (
    ClientRequestTimeout,
    HardenedZaraZmqGateway,
    HardenedZmqZaraClient,
    hardened_transport_config,
)
from zara.zmq_transport import TransportConfig, _RouteState


class _Supervisor:
    def subscribe(self, *_args, **_kwargs):
        raise AssertionError("not used by unit hardening tests")


class _FrameSocket:
    def __init__(self, frames):
        self.frames = frames
        self.sent = []

    def recv_multipart(self, *, copy=False):
        assert copy is False
        return self.frames

    def send_multipart(self, frames, *, flags=0):
        self.sent.append((frames, flags))


class _AgainThenWritableSocket:
    def __init__(self):
        self.calls = 0
        self.sent = []

    def send_multipart(self, frames, *, flags=0):
        assert flags == zmq.NOBLOCK
        self.calls += 1
        if self.calls == 1:
            raise zmq.Again()
        self.sent.append(frames)


def _message(message_id: str) -> ProtocolMessage:
    return ProtocolMessage(
        type="pong",
        id=message_id,
        timestamp_ns=1,
        payload_count=0,
    )


def _ready_client(config: TransportConfig | None = None) -> HardenedZmqZaraClient:
    client = HardenedZmqZaraClient(
        "inproc://hardening-unit",
        config=config or TransportConfig(request_timeout=0.05, poll_interval_ms=1),
    )
    with client._state_lock:
        client._state = ZaraClientState.READY
    client._session_id = "session-test"
    return client


def test_hardened_defaults_match_protocol_frame_cap_and_low_latency_poll():
    config = hardened_transport_config()
    assert config.max_message_bytes == 1024 * 1024
    assert config.poll_interval_ms == 1
    assert config.heartbeat_timeout_ms == 3 * config.heartbeat_interval_ms
    assert config.linger_ms == 0


def test_gateway_rejects_outer_multipart_frame_bomb_without_dying():
    gateway = HardenedZaraZmqGateway(
        "inproc://frame-bomb",
        supervisor=_Supervisor(),
        principal=PrincipalContext("hardening-test"),
    )
    too_many = 1 + 2 + gateway._limits.max_payload_frames + 1
    socket = _FrameSocket([b"route", *([b"x"] * (too_many - 1))])

    gateway._receive(socket)

    assert len(socket.sent) == 1
    frames, flags = socket.sent[0]
    assert flags == zmq.NOBLOCK
    assert frames[0] == b"route"
    error = decode_message(frames[1:]).message
    assert error.type == "protocol.error"
    assert error.body["code"] == "invalid_message"
    gateway.close(timeout=0.0)


def test_gateway_fails_route_closed_instead_of_silently_dropping_oldest_event():
    config = TransportConfig(event_queue_size=1, poll_interval_ms=1)
    gateway = HardenedZaraZmqGateway(
        "inproc://route-overflow",
        supervisor=_Supervisor(),
        principal=PrincipalContext("hardening-test"),
        config=config,
    )
    route = b"route"
    gateway._routes[route] = _RouteState(
        session_id="session",
        principal_id="hardening-test",
        ready=True,
    )

    assert gateway._enqueue_outbound(route, _message("one")) is True
    assert gateway._enqueue_outbound(route, _message("two")) is False
    assert route not in gateway._routes
    assert route not in gateway._route_outbound
    gateway.close(timeout=0.0)


def test_client_request_deadline_completes_future_instead_of_hanging_forever():
    client = _ready_client(TransportConfig(request_timeout=0.01, poll_interval_ms=1))
    future = client.ping()
    time.sleep(0.02)

    client._expire_pending_requests()

    with pytest.raises(ClientRequestTimeout):
        future.result(timeout=0.1)
    assert not client._pending
    client.close(timeout=0.0)


def test_idempotent_start_preserves_inflight_request_deadline():
    client = _ready_client()
    client._thread = SimpleNamespace(is_alive=lambda: True)
    future = client.ping()
    request_id = next(iter(client._pending))
    deadline = client._request_deadlines[request_id]

    started = client.start()

    assert started.result(timeout=0.1) is True
    assert client._request_deadlines[request_id] == deadline
    client._fail_pending(RuntimeError("test cleanup"))
    with pytest.raises(RuntimeError, match="test cleanup"):
        future.result(timeout=0.1)
    client._thread = None
    client.close(timeout=0.0)


def test_client_nonblocking_send_retries_exact_same_request_before_next_fifo_item():
    client = _ready_client()
    first = client.ping()
    second = client.ping()
    expected = list(client._pending)
    socket = _AgainThenWritableSocket()

    client._drain_client_outbound(socket)
    assert socket.sent == []
    assert client._retry_outbound is not None

    client._drain_client_outbound(socket)
    observed = [decode_message(frames).message.id for frames in socket.sent]
    assert observed == expected
    assert client._retry_outbound is None

    error = RuntimeError("test cleanup")
    client._fail_pending(error)
    for future in (first, second):
        with pytest.raises(RuntimeError, match="test cleanup"):
            future.result(timeout=0.1)
    client.close(timeout=0.0)


def test_client_drops_expired_queued_work_before_transport_send():
    client = _ready_client(TransportConfig(request_timeout=0.01, poll_interval_ms=1))
    future = client.ping()
    time.sleep(0.02)
    socket = _AgainThenWritableSocket()
    socket.calls = 1  # make it writable if anything is incorrectly sent

    client._drain_client_outbound(socket)

    with pytest.raises(ClientRequestTimeout):
        future.result(timeout=0.1)
    assert socket.sent == []
    assert client._retry_outbound is None
    client.close(timeout=0.0)
