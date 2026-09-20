"""Contract tests for the Android remote recovery fixture (issue #1302).

These tests prove the fixture behaves deterministically for good turns and
each injected failure mode. The Android-side interop test (JVM) and the
emulator device acceptance consume the same fixture process.
"""

from __future__ import annotations

import os
import socket
import subprocess
import sys
import tempfile
import time
from pathlib import Path

import pytest

import zmq

from zara.protocol import ProtocolMessage, decode_message, encode_message

REPO_ROOT = Path(__file__).resolve().parents[1]
FIXTURE = REPO_ROOT / "android" / "integration" / "remote_recovery_fixture.py"


class FixtureServer:
    def __init__(self) -> None:
        self.directory = tempfile.TemporaryDirectory(prefix="zara-recovery-fixture-")
        self.fixture_file = Path(self.directory.name) / "fixture.env"
        self.process = subprocess.Popen(
            [sys.executable, str(FIXTURE), "--fixture-file", str(self.fixture_file)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
            cwd=str(REPO_ROOT),
        )
        ready = self.process.stdout.readline().strip()
        assert ready == "READY", f"fixture did not become ready: {ready}"
        values = {}
        for line in self.fixture_file.read_text().splitlines():
            key, _, value = line.partition("=")
            values[key] = value
        self.values = values
        self.endpoint = values["endpoint"]
        self.server_public = values["server_public"].encode("ascii")
        self.client_public = values["client_public"].encode("ascii")
        self.client_secret = values["client_secret"].encode("ascii")
        assert os.stat(self.fixture_file).st_mode & 0o777 == 0o600

    def arm(self, mode: str) -> None:
        fifo = self.values["control_fifo"]
        descriptor = os.open(fifo, os.O_WRONLY)
        try:
            os.write(descriptor, f"ARM {mode}\n".encode())
        finally:
            os.close(descriptor)

    def client(self) -> zmq.Socket:
        dealer = zmq.Context.instance().socket(zmq.DEALER)
        dealer.curve_publickey = self.client_public
        dealer.curve_secretkey = self.client_secret
        dealer.curve_serverkey = self.server_public
        dealer.set(zmq.LINGER, 0)
        dealer.set(zmq.SNDTIMEO, 5000)
        dealer.set(zmq.RCVTIMEO, 5000)
        dealer.connect(self.endpoint)
        return dealer

    def stop(self) -> str:
        try:
            self.process.stdin.write("STOP\n")
            self.process.stdin.flush()
            self.process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.process.kill()
        stderr = self.process.stderr.read()
        self.directory.cleanup()
        return stderr


def _now_ns() -> int:
    return time.time_ns()


def _message(type_: str, **kwargs) -> ProtocolMessage:
    kwargs.setdefault("timestamp_ns", _now_ns())
    return ProtocolMessage(type=type_, id=kwargs.pop("id", os.urandom(8).hex()), payload_count=kwargs.pop("payload_count", 0), **kwargs)


def _send(dealer, message, payloads=()):
    dealer.send_multipart(encode_message(message, payloads=payloads))


def _recv(dealer, timeout_ms: int = 5000) -> ProtocolMessage:
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(timeout_ms)), "fixture did not reply in time"
    frames = dealer.recv_multipart()
    return decode_message(frames).message


def _hello(dealer, *, with_voice: bool = False) -> str:
    body: dict = {"versions": [1]}
    if with_voice:
        body["audio_output_formats"] = [
            {"codec": "pcm_s16le", "sample_rate": 24000, "channels": 1},
        ]
    _send(dealer, _message("hello", body=body))
    hello_ok = _recv(dealer)
    assert hello_ok.type == "hello.ok"
    assert hello_ok.body["version"] == 1
    if with_voice:
        assert hello_ok.body["audio_output_format"] == body["audio_output_formats"][0]
    _send(
        dealer,
        _message(
            "capability.snapshot",
            session_id=hello_ok.session_id,
            body={"capabilities": []},
        ),
    )
    ack = _recv(dealer)
    assert ack.type == "capability.snapshot.ok"
    return hello_ok.session_id


def _complete_text_turn(dealer, session: str, text: str = "hello") -> ProtocolMessage:
    _send(dealer, _message("turn.submit", session_id=session, body={"text": text}))
    accepted = _recv(dealer)
    assert accepted.type == "turn.accepted"
    seen = []
    while True:
        message = _recv(dealer)
        seen.append(message.type)
        if message.type == "turn.completed":
            assert message.body["success"] is True
            return message
        if message.type == "protocol.error":
            raise AssertionError(f"unexpected protocol error: {message.body}")


@pytest.fixture()
def server():
    fixture = FixtureServer()
    yield fixture
    fixture.stop()


def test_good_text_turn_completes_with_stock_response(server):
    dealer = server.client()
    session = _hello(dealer)
    completed = _complete_text_turn(dealer, session)
    assert completed.type == "turn.completed"
    dealer.close(0)


def test_voice_turn_streams_speech_markers_transcript_and_audio(server):
    dealer = server.client()
    session = _hello(dealer, with_voice=True)
    stream = "mic-fixture-1"
    _send(
        dealer,
        _message(
            "audio.input.start",
            session_id=session,
            stream_id=stream,
            body={"codec": "pcm_s16le", "sample_rate": 16000, "channels": 1, "frame_samples": 512},
        ),
    )
    assert _recv(dealer).type == "audio.input.started"
    pcm = b"\x00\x00" * 512
    _send(
        dealer,
        _message(
            "audio.input.chunk",
            session_id=session,
            stream_id=stream,
            seq=0,
            payload_count=1,
            content_type="audio/pcm;codec=pcm_s16le",
        ),
        payloads=[pcm],
    )
    accepted = _recv(dealer)
    assert accepted.type == "audio.input.accepted"
    assert accepted.seq == 0
    _send(dealer, _message("audio.input.commit", session_id=session, stream_id=stream))
    committed = _recv(dealer)
    assert committed.type == "audio.input.committed"

    types = []
    while True:
        message = _recv(dealer)
        types.append(message.type)
        if message.type == "audio.output.chunk":
            continue
        if message.type == "turn.completed":
            break
    for required in (
        "voice.speech.started",
        "voice.transcript.partial",
        "voice.speech.ended",
        "voice.transcript.final",
        "audio.output.start",
        "audio.output.chunk",
        "audio.output.done",
        "turn.completed",
    ):
        assert required in types, f"voice stream missing {required}: {types}"
    dealer.close(0)


def test_armed_malformed_sends_truncated_frame(server):
    dealer = server.client()
    session = _hello(dealer)
    server.arm("MALFORMED")
    _send(dealer, _message("turn.submit", session_id=session, body={"text": "hi"}))
    accepted = _recv(dealer)
    assert accepted.type == "turn.accepted"
    poller = zmq.Poller()
    poller.register(dealer, zmq.POLLIN)
    assert dict(poller.poll(5000)), "no malformed frame arrived"
    frames = dealer.recv_multipart()
    assert frames[0] == b"ZARA/1"
    with pytest.raises(Exception):
        decode_message(frames)
    dealer.close(0)


def test_armed_version_mismatch_replies_unsupported_version(server):
    dealer = server.client()
    server.arm("VERSION_MISMATCH")
    _send(dealer, _message("hello", body={"versions": [1]}))
    hello_ok = _recv(dealer)
    assert hello_ok.type == "hello.ok"
    assert hello_ok.body["version"] != 1
    dealer.close(0)


def test_armed_out_of_order_sends_delta_before_acceptance(server):
    dealer = server.client()
    session = _hello(dealer)
    server.arm("OUT_OF_ORDER")
    _send(dealer, _message("turn.submit", session_id=session, body={"text": "hi"}))
    message = _recv(dealer)
    assert message.type == "assistant.delta"
    dealer.close(0)


def test_armed_close_drops_transport_and_rebinds(server):
    dealer = server.client()
    session = _hello(dealer)
    server.arm("CLOSE")
    _send(dealer, _message("turn.submit", session_id=session, body={"text": "hi"}))
    time.sleep(0.5)
    fresh = server.client()
    fresh_session = _hello(fresh)
    assert fresh_session != session
    _complete_text_turn(fresh, fresh_session)
    fresh.close(0)
    dealer.close(0)


def test_armed_stale_frame_uses_previous_session_after_reconnect(server):
    dealer = server.client()
    first_session = _hello(dealer, with_voice=True)
    _send(
        dealer,
        _message(
            "audio.input.start",
            session_id=first_session,
            stream_id="s1",
            body={"codec": "pcm_s16le", "sample_rate": 16000, "channels": 1, "frame_samples": 512},
        ),
    )
    assert _recv(dealer).type == "audio.input.started"
    _send(dealer, _message("audio.input.commit", session_id=first_session, stream_id="s1"))
    committed = _recv(dealer)
    assert committed.type == "audio.input.committed"
    while True:
        if _recv(dealer).type == "turn.completed":
            break

    dealer.close(0)
    reconnect = server.client()
    second_session = _hello(reconnect, with_voice=True)
    assert second_session != first_session
    server.arm("STALE")
    _send(
        reconnect,
        _message(
            "audio.input.start",
            session_id=second_session,
            stream_id="s2",
            body={"codec": "pcm_s16le", "sample_rate": 16000, "channels": 1, "frame_samples": 512},
        ),
    )
    assert _recv(reconnect).type == "audio.input.started"
    _send(reconnect, _message("audio.input.commit", session_id=second_session, stream_id="s2"))
    assert _recv(reconnect).type == "audio.input.committed"
    types = []
    while True:
        message = _recv(reconnect)
        types.append((message.type, message.session_id))
        if message.type == "turn.completed":
            break
    stale_frames = [entry for entry in types if entry[1] == first_session]
    assert stale_frames, f"expected at least one stale-generation frame: {types}"
    current_frames = [entry for entry in types if entry[1] == second_session]
    assert any(entry[0] == "voice.transcript.final" for entry in current_frames)
    reconnect.close(0)
