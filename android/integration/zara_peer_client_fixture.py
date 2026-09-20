from __future__ import annotations

import argparse
import socket as net_socket
import sys
from pathlib import Path

import zmq

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from zara.protocol import ProtocolMessage, decode_message, encode_message
from zara.security_transport import CurveClientConfig, configure_curve_client_socket
from zara.zmq_transport import TransportConfig, apply_socket_options


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Canonical ZARA/1 CURVE peer client fixture")
    parser.add_argument("--endpoint", required=True)
    parser.add_argument("--server-key", required=True)
    parser.add_argument("--client-key", required=True)
    parser.add_argument("--client-secret", required=True)
    parser.add_argument("--node-id", default=None)
    parser.add_argument("--generation", type=int, default=None)
    parser.add_argument("--node-key", default=None)
    parser.add_argument("--message-id", default="fixture-hello")
    parser.add_argument("--mode", choices=("hello", "raw"), default="hello")
    parser.add_argument("--raw-envelope", default=None)
    parser.add_argument("--timeout-ms", type=int, default=5000)
    return parser.parse_args()


def _node_body(args: argparse.Namespace) -> dict[str, object]:
    body: dict[str, object] = {"versions": [1]}
    if args.node_id is not None:
        body["node"] = {
            "node_id": args.node_id,
            "display_name": "Fixture peer",
            "device_class": "desktop",
            "curve_public_key": args.client_key
            if args.node_key is None
            else args.node_key,
            "endpoints": ["tcp://127.0.0.1:17865"],
            "capabilities": ["open_app"],
            "protocol_versions": ["ZARA/1"],
            "last_seen": 1,
            "enrollment_generation": args.generation or 1,
        }
    return body


def _free_tcp_probe() -> None:
    with net_socket.socket(net_socket.AF_INET, net_socket.SOCK_STREAM):
        pass


def _receive(socket: zmq.Socket, timeout_ms: int) -> ProtocolMessage | None:
    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)
    events = dict(poller.poll(timeout_ms))
    if events.get(socket) != zmq.POLLIN:
        return None
    return decode_message(socket.recv_multipart()).message


def _emit(line: str) -> None:
    print(line, flush=True)


def main() -> int:
    args = _parse_args()
    transport = TransportConfig(
        sndhwm=8,
        rcvhwm=8,
        max_message_bytes=1024 * 1024,
        heartbeat_interval_ms=100,
        heartbeat_timeout_ms=500,
        linger_ms=0,
        request_timeout=1.0,
        poll_interval_ms=5,
        event_queue_size=8,
        pending_request_limit=8,
    )
    context = zmq.Context()
    dealer = context.socket(zmq.DEALER)
    try:
        apply_socket_options(dealer, transport, router=False)
        configure_curve_client_socket(
            dealer,
            CurveClientConfig(
                public_key=args.client_key,
                secret_key=args.client_secret,
                server_public_key=args.server_key,
            ),
        )
        dealer.connect(args.endpoint)

        if args.mode == "raw":
            dealer.send_multipart(
                [b"ZARA/1", args.raw_envelope.encode("utf-8")],
            )
        else:
            dealer.send_multipart(
                encode_message(
                    ProtocolMessage(
                        type="hello",
                        id=args.message_id,
                        timestamp_ns=1,
                        payload_count=0,
                        body=_node_body(args),
                    )
                )
            )

        message = _receive(dealer, args.timeout_ms)
        if message is None:
            _emit("RESULT timeout")
            return 1
        if message.type == "hello.ok":
            _emit(f"RESULT type=hello.ok session_id={message.session_id}")
            return 0
        if message.type == "protocol.error":
            body = message.body
            _emit(
                "RESULT type=protocol.error "
                f"code={body.get('code')} retryable={body.get('retryable')}"
            )
            return 0
        _emit(f"RESULT type={message.type}")
        return 0
    finally:
        dealer.close(0)
        context.term()


if __name__ == "__main__":
    raise SystemExit(main())
