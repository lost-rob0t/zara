from __future__ import annotations

from t.test_peer_node_secure_hello import _node_mapping, _send_hello, _start_peer
from zara.node import ZaraNode


def test_secure_hello_publishes_peer_node_before_ack_is_observable(
    zmq_context,
    transport_config,
    monkeypatch,
):
    gateway, dealer, principal, enrolled, _registry = _start_peer(
        zmq_context,
        transport_config,
    )
    node_mapping = _node_mapping(enrolled)
    expected = ZaraNode.from_mapping(node_mapping)
    original_send = gateway._send
    observed = []

    def send_with_publication_probe(socket, route, message, *args, **kwargs):
        if message.type == "hello.ok":
            observed.append(
                gateway.node_for_session(principal.principal_id, message.session_id)
            )
        return original_send(socket, route, message, *args, **kwargs)

    monkeypatch.setattr(gateway, "_send", send_with_publication_probe)
    try:
        hello = _send_hello(
            dealer,
            {"versions": [1], "node": node_mapping},
            "publication-order",
        )
        assert hello.type == "hello.ok"
        assert observed == [expected]
    finally:
        dealer.close(0)
        gateway.close(timeout=1.0)
