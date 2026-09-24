from __future__ import annotations

import logging
import threading
from collections import OrderedDict

import zmq

from zara.protocol import (
    AUDIO_OUTPUT_CONTENT_TYPE,
    ProtocolLimits,
    ProtocolMessage,
    decode_message,
)
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, _RouteState


class _BackpressuredThenWritableSocket:
    def __init__(self) -> None:
        self.attempts = 0
        self.sent = []

    def send_multipart(self, frames, *, flags=0):
        assert flags == zmq.NOBLOCK
        self.attempts += 1
        if self.attempts == 1:
            raise zmq.Again()
        self.sent.append(tuple(frames))


def _live_gateway(route: bytes) -> ZaraZmqGateway:
    gateway = object.__new__(ZaraZmqGateway)
    gateway._limits = ProtocolLimits()
    gateway._config = TransportConfig(event_queue_size=4)
    gateway._lock = threading.RLock()
    gateway._route_outbound = OrderedDict()
    gateway._routes = {
        route: _RouteState(
            session_id="session-1",
            principal_id="owner",
            ready=True,
        )
    }
    # Keep the focused transport fixture faithful to the production gateway's
    # acceptance-barrier state without constructing unrelated runtime owners.
    gateway._turn_routes = {}
    gateway._early_turn_events = OrderedDict()
    gateway._turns_awaiting_accept = set()
    gateway._approval_owners = {}
    return gateway


def test_transient_again_preserves_live_route_and_retries_reply(caplog):
    route = b"android-live-route"
    gateway = _live_gateway(route)
    dropped = []
    gateway._drop_route = dropped.append

    message = ProtocolMessage(
        type="turn.accepted",
        id="accepted-1",
        reply_to="request-1",
        session_id="session-1",
        turn_id="turn-1",
        timestamp_ns=1,
        payload_count=0,
    )
    socket = _BackpressuredThenWritableSocket()

    with caplog.at_level(logging.WARNING, logger="zara.zmq_transport"):
        gateway._send(socket, route, message)

    assert dropped == []
    assert route in gateway._route_outbound
    assert "Again" in caplog.text

    gateway._drain_outbound(socket)

    assert socket.attempts == 2
    assert len(socket.sent) == 1
    assert socket.sent[0][0] == route
    delivered = decode_message(socket.sent[0][1:]).message
    assert delivered.type == "turn.accepted"
    assert delivered.id == "accepted-1"
    assert delivered.reply_to == "request-1"
    assert delivered.turn_id == "turn-1"
    assert route not in gateway._route_outbound


def test_transient_again_preserves_payload_and_fifo_order():
    route = b"voice-live-route"
    gateway = _live_gateway(route)
    dropped = []
    gateway._drop_route = dropped.append
    socket = _BackpressuredThenWritableSocket()

    pcm = b"\x01\x00\x02\x00"
    audio = ProtocolMessage(
        type="audio.output.chunk",
        id="audio-1",
        session_id="session-1",
        conversation_id="conversation-1",
        turn_id="turn-1",
        stream_id="stream-1",
        seq=0,
        timestamp_ns=1,
        content_type=AUDIO_OUTPUT_CONTENT_TYPE,
        payload_count=1,
    )
    following = ProtocolMessage(
        type="turn.accepted",
        id="accepted-2",
        reply_to="request-2",
        session_id="session-1",
        turn_id="turn-2",
        timestamp_ns=2,
        payload_count=0,
    )

    gateway._send(socket, route, audio, (pcm,))
    assert gateway._enqueue_outbound(route, following)

    gateway._drain_outbound(socket)

    assert dropped == []
    assert socket.attempts == 3
    assert len(socket.sent) == 2
    first = decode_message(socket.sent[0][1:])
    second = decode_message(socket.sent[1][1:])
    assert first.message.id == "audio-1"
    assert first.payloads == (pcm,)
    assert second.message.id == "accepted-2"
    assert route not in gateway._route_outbound


def test_evicted_tool_waiting_frame_cannot_leave_invisible_approval_owner():
    route = b"approval-live-route"
    gateway = _live_gateway(route)
    gateway._config = TransportConfig(event_queue_size=1)
    owner = type("Owner", (), {"route": route, "session_id": "session-1"})()
    gateway._approval_owners[("owner", "tool-1")] = owner

    waiting = ProtocolMessage(
        type="tool.waiting",
        id="waiting-1",
        session_id="session-1",
        turn_id="turn-1",
        timestamp_ns=1,
        payload_count=0,
        body={
            "tool_run_id": "tool-1",
            "tool_name": "reviewed_effect",
            "kind": "approval",
            "prompt": "Approve reviewed_effect?",
        },
    )
    following = ProtocolMessage(
        type="assistant.delta",
        id="delta-1",
        session_id="session-1",
        turn_id="turn-1",
        seq=2,
        timestamp_ns=2,
        payload_count=0,
        body={"text": "after waiting"},
    )

    assert gateway._enqueue_outbound(route, waiting)
    assert gateway._enqueue_outbound(route, following)

    assert ("owner", "tool-1") not in gateway._approval_owners
    queued = list(gateway._route_outbound[route])
    assert [item.message.type for item in queued] == ["assistant.delta"]
