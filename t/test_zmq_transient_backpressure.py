from __future__ import annotations

import logging
import threading
from collections import OrderedDict

import zmq

from zara.protocol import ProtocolLimits, ProtocolMessage, decode_message
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


def test_transient_again_preserves_live_route_and_retries_reply(caplog):
    gateway = object.__new__(ZaraZmqGateway)
    gateway._limits = ProtocolLimits()
    gateway._config = TransportConfig(event_queue_size=4)
    gateway._lock = threading.RLock()
    gateway._route_outbound = OrderedDict()

    route = b"android-live-route"
    gateway._routes = {
        route: _RouteState(
            session_id="session-1",
            principal_id="owner",
            ready=True,
        )
    }
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
