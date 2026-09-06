from __future__ import annotations

import logging

import zmq

from zara.protocol import ProtocolLimits, ProtocolMessage
from zara.zmq_transport import ZaraZmqGateway


class _AgainSocket:
    def send_multipart(self, _frames, *, flags=0):
        assert flags == zmq.NOBLOCK
        raise zmq.Again()


def test_outbound_again_logs_bounded_failure_before_route_drop(caplog):
    gateway = object.__new__(ZaraZmqGateway)
    gateway._limits = ProtocolLimits()
    sensitive_route = b"secret-device-route"
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

    assert dropped == [sensitive_route]
    assert "outbound send failed" in caplog.text
    assert "Again" in caplog.text
    assert sensitive_route.decode("ascii") not in caplog.text
    assert "send-diagnostic" not in caplog.text
