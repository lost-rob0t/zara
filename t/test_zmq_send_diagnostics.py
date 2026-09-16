from __future__ import annotations

import logging
import threading
from collections import OrderedDict

import zmq

from zara.protocol import ProtocolLimits, ProtocolMessage
from zara.zmq_transport import TransportConfig, ZaraZmqGateway, _RouteState


class _AgainSocket:
    def send_multipart(self, _frames, *, flags=0):
        assert flags == zmq.NOBLOCK
        raise zmq.Again()


class _PermanentFailureSocket:
    def send_multipart(self, _frames, *, flags=0):
        assert flags == zmq.NOBLOCK
        raise zmq.ZMQError(zmq.EHOSTUNREACH)


def _gateway_with_live_route(route: bytes):
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
    return gateway


def test_outbound_again_logs_bounded_failure_without_route_drop(caplog):
    sensitive_route = b"secret-device-route"
    gateway = _gateway_with_live_route(sensitive_route)
    dropped = []
    gateway._drop_route = dropped.append

    with caplog.at_level(logging.WARNING, logger="zara.zmq_transport"):
        gateway._send(
            _AgainSocket(),
            sensitive_route,
            ProtocolMessage(
                type="ping",
                id="send-diagnostic",
                timestamp_ns=1,
                payload_count=0,
            ),
        )

    assert dropped == []
    assert sensitive_route in gateway._route_outbound
    assert "outbound send failed" in caplog.text
    assert "Again" in caplog.text
    assert sensitive_route.decode("ascii") not in caplog.text
    assert "send-diagnostic" not in caplog.text


def test_permanent_outbound_error_remains_fail_closed_and_secret_safe(caplog):
    sensitive_route = b"secret-permanent-route"
    gateway = _gateway_with_live_route(sensitive_route)
    dropped = []
    gateway._drop_route = dropped.append

    with caplog.at_level(logging.WARNING, logger="zara.zmq_transport"):
        gateway._send(
            _PermanentFailureSocket(),
            sensitive_route,
            ProtocolMessage(
                type="ping",
                id="permanent-send-diagnostic",
                timestamp_ns=1,
                payload_count=0,
            ),
        )

    assert dropped == [sensitive_route]
    assert "outbound send failed" in caplog.text
    assert "ZMQError" in caplog.text
    assert sensitive_route.decode("ascii") not in caplog.text
    assert "permanent-send-diagnostic" not in caplog.text


def test_audio_delivery_drop_names_turn_and_stream_without_secrets(caplog):
    sensitive_route = b"secret-audio-route"
    gateway = _gateway_with_live_route(sensitive_route)
    gateway._drop_route = lambda _route: None

    with caplog.at_level(logging.WARNING, logger="zara.zmq_transport"):
        gateway._send(
            _PermanentFailureSocket(),
            sensitive_route,
            ProtocolMessage(
                type="audio.output.chunk",
                id="secret-message-id",
                timestamp_ns=1,
                payload_count=1,
                stream_id="tts-stream-7",
                turn_id="turn-42",
                seq=0,
                content_type="audio/pcm;codec=pcm_s16le",
            ),
            payloads=(b"\x00\x00",),
        )

    assert "audio.output.chunk" in caplog.text
    assert "tts-stream-7" in caplog.text
    assert "turn-42" in caplog.text
    assert sensitive_route.decode("ascii") not in caplog.text
    assert "secret-message-id" not in caplog.text
