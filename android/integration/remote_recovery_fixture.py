"""Deterministic ZARA/1 CURVE fixture for Android remote recovery acceptance.

The fixture speaks the real wire protocol (zara.protocol) on a CURVE ROUTER
socket and can inject one armed failure mode into the next interaction:

  ARM MALFORMED         send a truncated/malformed multipart mid-turn
  ARM VERSION_MISMATCH  hello.ok reports an unsupported version
  ARM OUT_OF_ORDER      assistant.delta arrives before turn.accepted
  ARM CLOSE             abrupt transport close mid-stream, socket rebinds
  ARM STALE             first post-reconnect frame carries the old session_id

Control: stdin (one command per line) plus the FIFO whose path is published
in the fixture file as ``control_fifo`` (ARM <MODE>, STOP).
Trace lines are written to stderr as ``FIXTURE ...`` for evidence bundles.
"""

from __future__ import annotations

import argparse
import os
import secrets
import socket
import sys
import threading
import time
from pathlib import Path

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message

MARKER = b"ZARA/1"


def _trace(phase: str, outcome: str, **fields) -> None:
    details = " ".join(f"{key}={value}" for key, value in fields.items())
    print(f"FIXTURE phase={phase} outcome={outcome} {details}".rstrip(), file=sys.stderr, flush=True)


def _tcp_endpoint() -> str:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as probe:
        probe.bind(("127.0.0.1", 0))
        return f"tcp://127.0.0.1:{probe.getsockname()[1]}"


def _write_fixture(path: Path, values: dict[str, str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(descriptor, "w", encoding="utf-8") as output:
        for key, value in values.items():
            output.write(f"{key}={value}\n")
        output.flush()
        os.fsync(output.fileno())


VALID_MODES = {"MALFORMED", "VERSION_MISMATCH", "OUT_OF_ORDER", "CLOSE", "STALE"}


class _SessionState:
    def __init__(self) -> None:
        self.session_id: str | None = None
        self.previous_session_id: str | None = None
        self.turn_count = 0
        self.seq = 0

    def next_seq(self) -> int:
        self.seq += 1
        return self.seq


class RecoveryFixture:
    def __init__(self, endpoint: str, server_public: bytes, server_secret: bytes) -> None:
        self.endpoint = endpoint
        self.context = zmq.Context.instance()
        self.server_public = server_public
        self.server_secret = server_secret
        self.socket = self._bind()
        self._armed: str | None = None
        self._lock = threading.Lock()
        self.sessions: dict[bytes, _SessionState] = {}
        self.retired_sessions: list[str] = []
        self.stop = threading.Event()

    def _bind(self) -> zmq.Socket:
        server = self.context.socket(zmq.ROUTER)
        try:
            server.curve_secretkey = self.server_secret
            server.curve_publickey = self.server_public
            server.curve_server = True
            server.set(zmq.SNDHWM, 64)
            server.set(zmq.RCVHWM, 64)
            server.set(zmq.LINGER, 0)
            server.bind(self.endpoint)
        except BaseException:
            server.close(0)
            raise
        return server

    def arm(self, mode: str) -> None:
        with self._lock:
            self._armed = mode
        _trace("arm", mode.lower())

    def take_armed(self) -> str | None:
        with self._lock:
            armed = self._armed
            self._armed = None
            return armed

    def abrupt_close(self) -> None:
        _trace("transport", "close")
        self.socket.close(0)
        deadline = time.monotonic() + 5.0
        while True:
            try:
                self.socket = self._bind()
                return
            except zmq.ZMQError as error:
                if time.monotonic() >= deadline:
                    _trace("transport", "rebind_failed", error=str(error))
                    raise
                time.sleep(0.1)

    def _send(self, identity: bytes, message: ProtocolMessage, payloads=()) -> None:
        frames = encode_message(message, payloads=payloads)
        self.socket.send_multipart([identity, *frames])
        _trace(
            "tx",
            message.type,
            session=message.session_id or "-",
            bytes=len(frames[1]) + sum(len(p) for p in payloads),
        )

    def _message(
        self,
        type_: str,
        *,
        session: str | None,
        reply_to: str | None = None,
        conversation: str | None = None,
        turn: str | None = None,
        stream: str | None = None,
        seq: int | None = None,
        body: dict | None = None,
        content_type: str | None = None,
        payload_count: int = 0,
    ) -> ProtocolMessage:
        return ProtocolMessage(
            type=type_,
            id=secrets.token_hex(8),
            timestamp_ns=time.time_ns(),
            payload_count=payload_count,
            reply_to=reply_to,
            session_id=session,
            conversation_id=conversation,
            content_type=content_type,
            body=body,
            turn_id=turn,
            stream_id=stream,
            seq=seq,
        )

    def serve_forever(self) -> None:
        poller = zmq.Poller()
        watched = None
        _trace("server", "ready")
        while not self.stop.is_set():
            if watched is not self.socket:
                if watched is not None:
                    poller.unregister(watched)
                poller.register(self.socket, zmq.POLLIN)
                watched = self.socket
            for ready, _flags in dict(poller.poll(100)).items():
                if ready is not self.socket:
                    continue
                try:
                    raw = self.socket.recv_multipart()
                except zmq.ZMQError:
                    continue
                if len(raw) < 3:
                    continue
                identity, *frames = raw
                try:
                    decoded = decode_message(frames)
                except Exception as error:  # noqa: BLE001 - fixture must survive bad input
                    _trace("rx", "undecodable", error=type(error).__name__)
                    continue
                message = decoded.message
                _trace("rx", message.type, session=message.session_id or "-", bytes=len(frames[1]))
                state = self.sessions.setdefault(identity, _SessionState())
                try:
                    self.handle(identity, state, message, decoded.payloads)
                except Exception as error:  # noqa: BLE001 - keep serving for evidence
                    _trace("handler", "error", error=f"{type(error).__name__}: {error}")

    def handle(self, identity: bytes, state: _SessionState, message: ProtocolMessage, payloads) -> None:
        if message.type == "hello":
            self.handle_hello(identity, state, message)
        elif message.type == "capability.snapshot":
            self._send(
                identity,
                self._message(
                    "capability.snapshot.ok",
                    session=state.session_id,
                    reply_to=message.id,
                    body={"capabilities": list(message.body.get("capabilities", []))},
                ),
            )
        elif message.type == "turn.submit":
            self.handle_turn(identity, state, message)
        elif message.type == "audio.input.start":
            self._send(
                identity,
                self._message(
                    "audio.input.started",
                    session=state.session_id,
                    reply_to=message.id,
                    stream=message.stream_id,
                ),
            )
        elif message.type == "audio.input.chunk":
            self._send(
                identity,
                self._message(
                    "audio.input.accepted",
                    session=state.session_id,
                    reply_to=message.id,
                    stream=message.stream_id,
                    seq=message.seq,
                ),
            )
        elif message.type == "audio.input.commit":
            self._send(
                identity,
                self._message(
                    "audio.input.committed",
                    session=state.session_id,
                    reply_to=message.id,
                    stream=message.stream_id,
                ),
            )
            self.stream_voice_turn(identity, state, message)
        elif message.type == "audio.input.cancel":
            self._send(
                identity,
                self._message(
                    "audio.input.cancelled",
                    session=state.session_id,
                    reply_to=message.id,
                    stream=message.stream_id,
                ),
            )
        else:
            _trace("handler", "ignored", type=message.type)

    def handle_hello(self, identity: bytes, state: _SessionState, message: ProtocolMessage) -> None:
        mode = self.take_armed() if self._armed_applies_to_hello() else None
        if state.session_id:
            state.previous_session_id = state.session_id
            self.retired_sessions.append(state.session_id)
        state.session_id = f"recovery-session-{secrets.token_hex(6)}"
        self.retired_sessions.append(state.session_id)
        if mode == "VERSION_MISMATCH":
            _trace("inject", "version_mismatch")
            self._send(
                identity,
                self._message(
                    "hello.ok",
                    session=state.session_id,
                    reply_to=message.id,
                    body={
                        "version": 2,
                        "max_payload_frames": 16,
                        "max_payload_frame_bytes": 1048576,
                        "max_payload_bytes": 4194304,
                    },
                ),
            )
            return
        body = {
            "version": 1,
            "max_payload_frames": 16,
            "max_payload_frame_bytes": 1048576,
            "max_payload_bytes": 4194304,
        }
        offered = message.body.get("audio_output_formats") if message.body else None
        if offered:
            body["audio_output_format"] = offered[0]
        self._send(
            identity,
            self._message("hello.ok", session=state.session_id, reply_to=message.id, body=body),
        )

    def _armed_applies_to_hello(self) -> bool:
        with self._lock:
            return self._armed == "VERSION_MISMATCH"

    def handle_turn(self, identity: bytes, state: _SessionState, message: ProtocolMessage) -> None:
        mode = self.take_armed()
        state.turn_count += 1
        turn_id = f"recovery-turn-{state.turn_count}"
        conversation = message.conversation_id
        if mode == "CLOSE":
            _trace("inject", "close_before_reply")
            self.abrupt_close()
            return
        if mode == "OUT_OF_ORDER":
            _trace("inject", "out_of_order")
            self._send(
                identity,
                self._message(
                    "assistant.delta",
                    session=state.session_id,
                    turn=turn_id,
                    conversation=conversation,
                    seq=state.next_seq(),
                    body={"text": "delta before acceptance"},
                ),
            )
            return
        self._send(
            identity,
            self._message(
                "turn.accepted",
                session=state.session_id,
                reply_to=message.id,
                conversation=conversation,
                turn=turn_id,
            ),
        )
        if mode == "MALFORMED":
            _trace("inject", "malformed")
            self.socket.send_multipart([identity, MARKER, b'{"type":"assistant.delta","trunc'])
            return
        self._send(
            identity,
            self._message(
                "turn.started",
                session=state.session_id,
                turn=turn_id,
                conversation=conversation,
                seq=state.next_seq(),
            ),
        )
        self._send(
            identity,
            self._message(
                "assistant.started",
                session=state.session_id,
                turn=turn_id,
                conversation=conversation,
                seq=state.next_seq(),
            ),
        )
        self._send(
            identity,
            self._message(
                "assistant.delta",
                session=state.session_id,
                turn=turn_id,
                conversation=conversation,
                seq=state.next_seq(),
                body={"text": "stock server response"},
            ),
        )
        self._send(
            identity,
            self._message(
                "assistant.completed",
                session=state.session_id,
                turn=turn_id,
                conversation=conversation,
                seq=state.next_seq(),
                body={"text": "stock server response", "success": True},
            ),
        )
        self._send(
            identity,
            self._message(
                "turn.completed",
                session=state.session_id,
                turn=turn_id,
                conversation=conversation,
                seq=state.next_seq(),
                body={"success": True},
            ),
        )
        _trace("turn", "completed", turn=turn_id)

    def stream_voice_turn(self, identity: bytes, state: _SessionState, message: ProtocolMessage) -> None:
        mode = self.take_armed()
        stream = message.stream_id or "mic-fixture"
        conversation = message.conversation_id or "recovery-conversation"
        stale_session = (
            next(
                (candidate for candidate in reversed(self.retired_sessions) if candidate != state.session_id),
                None,
            )
            if mode == "STALE"
            else None
        )
        if mode == "CLOSE":
            _trace("inject", "close_mid_voice")
            self.abrupt_close()
            return
        state.turn_count += 1
        turn_id = f"recovery-voice-turn-{state.turn_count}"
        seq = 0

        def stream_event(type_: str, body: dict) -> None:
            nonlocal seq
            seq += 1
            self._send(
                identity,
                self._message(
                    type_,
                    session=state.session_id,
                    stream=stream,
                    conversation=conversation,
                    seq=seq,
                    body=body,
                ),
            )

        if stale_session:
            _trace("inject", "stale_generation", session=stale_session)
            self._send(
                identity,
                self._message(
                    "voice.speech.started",
                    session=stale_session,
                    stream=stream,
                    conversation=conversation,
                    seq=0,
                    body={"pre_speech_samples": 0},
                ),
            )
        stream_event("voice.speech.started", {"pre_speech_samples": 0})
        stream_event("voice.transcript.partial", {"text": "stock voice"})
        stream_event("voice.speech.ended", {"reason": "commit"})
        stream_event("voice.transcript.final", {"text": "stock voice transcript"})
        self._send(
            identity,
            self._message(
                "audio.output.start",
                session=state.session_id,
                turn=turn_id,
                stream=stream,
                body={"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1},
            ),
        )
        silence = b"\x00\x00" * 4800
        self._send(
            identity,
            self._message(
                "audio.output.chunk",
                session=state.session_id,
                turn=turn_id,
                stream=stream,
                seq=0,
                content_type="audio/pcm;codec=pcm_s16le",
                payload_count=1,
            ),
            payloads=[silence],
        )
        self._send(
            identity,
            self._message("audio.output.done", session=state.session_id, turn=turn_id, stream=stream),
        )
        self._send(
            identity,
            self._message(
                "turn.completed",
                session=state.session_id,
                turn=turn_id,
                conversation=conversation,
                seq=state.next_seq(),
                body={"success": True},
            ),
        )
        _trace("voice", "completed", turn=turn_id)


def _control_fifo(path: Path) -> None:
    os.mkfifo(path, 0o600)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--fixture-file", required=True)
    parser.add_argument("--control-fifo", default=None)
    args = parser.parse_args()

    fixture_path = Path(args.fixture_file).resolve()
    control_path = Path(args.control_fifo) if args.control_fifo else fixture_path.parent / "control.fifo"
    _control_fifo(control_path)

    server_public, server_secret = zmq.curve_keypair()
    client_public, client_secret = zmq.curve_keypair()
    endpoint = _tcp_endpoint()

    fixture = RecoveryFixture(endpoint, server_public, server_secret)

    _write_fixture(
        fixture_path,
        {
            "endpoint": endpoint,
            "server_public": server_public.decode("ascii"),
            "client_public": client_public.decode("ascii"),
            "client_secret": client_secret.decode("ascii"),
            "control_fifo": os.fspath(control_path),
        },
    )
    print("READY", flush=True)

    def handle_command(command: str) -> bool:
        if command == "STOP":
            fixture.stop.set()
            return False
        if command.startswith("ARM "):
            mode = command[4:].strip().upper()
            if mode in VALID_MODES:
                fixture.arm(mode)
            else:
                _trace("arm", "rejected", mode=mode)
        return True

    def control_stdin() -> None:
        for line in sys.stdin:
            if not handle_command(line.strip()):
                return

    def control_fifo() -> None:
        descriptor = os.open(control_path, os.O_RDWR)
        with os.fdopen(descriptor, "r", encoding="utf-8") as stream:
            for line in stream:
                if not handle_command(line.strip()):
                    return

    threading.Thread(target=control_stdin, name="fixture-stdin", daemon=True).start()
    control_thread = threading.Thread(target=control_fifo, name="fixture-control", daemon=True)
    control_thread.start()
    try:
        fixture.serve_forever()
    finally:
        fixture.socket.close(0)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
