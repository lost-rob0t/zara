from __future__ import annotations

import queue
import socket as net_socket
import threading
import time

import pytest
import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.zmq_hardening import ClientRequestTimeout, HardenedZmqZaraClient
from zara.zmq_transport import TransportConfig


def _endpoint() -> str:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return f"tcp://127.0.0.1:{probe.getsockname()[1]}"


def _reply(route: bytes, request: ProtocolMessage, message_type: str, **kwargs):
    values = {
        "type": message_type,
        "id": f"reply-{request.id}",
        "reply_to": request.id,
        "session_id": kwargs.pop("session_id", request.session_id),
        "timestamp_ns": time.time_ns(),
        "payload_count": 0,
    }
    values.update(kwargs)
    return [route, *encode_message(ProtocolMessage(**values))]


def test_actual_tcp_lost_reply_expires_once_and_next_request_stays_live():
    context = zmq.Context()
    endpoint = _endpoint()
    config = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=0.05,
        poll_interval_ms=1,
        event_queue_size=8,
        pending_request_limit=8,
    )
    router = context.socket(zmq.ROUTER)
    router.setsockopt(zmq.LINGER, 0)
    router.bind(endpoint)
    observed: queue.Queue[str] = queue.Queue()
    server_error: queue.Queue[BaseException] = queue.Queue()

    def server() -> None:
        try:
            route, *hello_frames = router.recv_multipart()
            hello = decode_message(hello_frames).message
            assert hello.type == "hello"
            router.send_multipart(
                _reply(
                    route,
                    hello,
                    "hello.ok",
                    session_id="lost-reply-session",
                    body={
                        "version": 1,
                        "audio_output_format": {
                            "codec": "pcm_s16le",
                            "sample_rate": 24000,
                            "channels": 1,
                        },
                    },
                )
            )

            route, *lost_frames = router.recv_multipart()
            lost = decode_message(lost_frames).message
            assert lost.type == "ping"
            observed.put(lost.id)
            # Intentionally lose this reply. The client deadline owns recovery.

            route, *healthy_frames = router.recv_multipart()
            healthy = decode_message(healthy_frames).message
            assert healthy.type == "ping"
            observed.put(healthy.id)
            router.send_multipart(_reply(route, healthy, "pong"))
        except BaseException as error:  # surfaced in the test thread
            server_error.put(error)

    thread = threading.Thread(target=server, name="lost-reply-router", daemon=True)
    thread.start()
    client = HardenedZmqZaraClient(endpoint, context=context, config=config)

    try:
        client.start().result(timeout=1.0)
        lost = client.ping()
        first_id = observed.get(timeout=1.0)

        with pytest.raises(ClientRequestTimeout):
            lost.result(timeout=1.0)

        healthy = client.ping()
        second_id = observed.get(timeout=1.0)
        pong = healthy.result(timeout=1.0)

        assert first_id != second_id
        assert pong.type == "pong"
        assert pong.reply_to == second_id
        assert client.is_alive
        with client._pending_lock:
            assert client._pending == {}
            assert client._request_deadlines == {}
        assert client._retry_outbound is None
        assert server_error.empty()
    finally:
        client.close(timeout=1.0)
        thread.join(timeout=1.0)
        router.close(0)
        context.term()

    assert not thread.is_alive()
    assert server_error.empty()
