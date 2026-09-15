"""Wake daemon voice client: thin ZARA/1 client owning devices only (#244)."""

from __future__ import annotations

import concurrent.futures
import threading

import numpy as np
import pytest

from zara.client import ZaraClientState
from zara.runtime import events
from zara.runtime.bridge import RuntimeEventBus
from zara.wake_daemon import WakeDaemonClient, WakeDaemonUnavailable, utterance_frames


class FakeZaraClient:
    def __init__(self) -> None:
        self.calls: list[tuple] = []
        self.lock = threading.Lock()
        self._bus = RuntimeEventBus()
        self.state = ZaraClientState.READY
        self.audio_output_format = {
            "codec": "pcm_s16le",
            "sample_rate": 24000,
            "channels": 1,
        }

    def _record(self, name: str, **kwargs) -> None:
        with self.lock:
            self.calls.append((name, kwargs))

    def start(self):
        import concurrent.futures

        future = concurrent.futures.Future()
        future.set_result(None)
        return future

    def open_conversation(self, conversation_id=None):
        self._record("open_conversation", conversation_id=conversation_id)
        import concurrent.futures

        future = concurrent.futures.Future()
        future.set_result(None)
        return future

    def subscribe(self, *, maxsize=0):
        self._record("subscribe", maxsize=maxsize)
        return self._bus.subscribe(maxsize=maxsize)

    def start_audio_input(self, stream_id, *, trace_id=None):
        self._record("start_audio_input", stream_id=stream_id, trace_id=trace_id)

    def send_audio_input(self, stream_id, *, seq, pcm, trace_id=None):
        self._record(
            "send_audio_input", stream_id=stream_id, seq=seq, pcm=pcm, trace_id=trace_id
        )

    def commit_audio_input(self, stream_id, *, trace_id=None):
        self._record("commit_audio_input", stream_id=stream_id, trace_id=trace_id)

    def cancel_audio_input(self, stream_id, *, trace_id=None):
        self._record("cancel_audio_input", stream_id=stream_id, trace_id=trace_id)

    def close(self, timeout=0):
        self._record("close")

    def publish(self, event) -> None:
        self._bus.publish(event)


def build_client(fake: FakeZaraClient) -> WakeDaemonClient:
    client = WakeDaemonClient(client=fake)
    client.connect()
    return client


def test_utterance_frames_split_into_exact_protocol_frames():
    audio = np.zeros((1000, 1), dtype=np.float32)

    frames = utterance_frames(audio)

    assert len(frames) == 2
    assert all(len(frame) == 1024 for frame in frames)


def test_utterance_frames_convert_float32_to_s16le():
    audio = np.full((512, 1), 0.5, dtype=np.float32)

    frames = utterance_frames(audio)

    pcm = frames[0]
    values = np.frombuffer(pcm, dtype="<i2")
    assert values.max() == int(0.5 * 32767)


@pytest.mark.asyncio
async def test_stream_utterance_sends_contiguous_frames_and_commits():
    fake = FakeZaraClient()
    client = build_client(fake)
    audio = np.zeros((1000, 1), dtype=np.float32)

    stream_id = await client.stream_utterance(audio, trace_id="trace-abc")

    names = [call[0] for call in fake.calls if call[0].endswith("audio_input")]
    assert names == [
        "start_audio_input",
        "send_audio_input",
        "send_audio_input",
        "commit_audio_input",
    ]
    first_start = next(call for call in fake.calls if call[0] == "start_audio_input")
    assert first_start[1]["trace_id"] == "trace-abc"
    seqs = [call[1]["seq"] for call in fake.calls if call[0] == "send_audio_input"]
    assert seqs == [0, 1]
    commits = [call for call in fake.calls if call[0] == "commit_audio_input"]
    assert commits[0][1]["stream_id"] == stream_id
    assert commits[0][1]["trace_id"] == "trace-abc"


@pytest.mark.asyncio
async def test_stream_utterance_uses_stable_stream_id_for_all_calls():
    fake = FakeZaraClient()
    client = build_client(fake)
    audio = np.zeros((512, 1), dtype=np.float32)

    stream_id = await client.stream_utterance(audio, trace_id="trace-xyz")

    stream_ids = {
        call[1].get("stream_id")
        for call in fake.calls
        if call[0].endswith("audio_input")
    }
    assert stream_ids == {stream_id}


@pytest.mark.asyncio
async def test_dispatch_final_transcript_invokes_handler():
    fake = FakeZaraClient()
    client = build_client(fake)
    received: list[events.RuntimeEvent] = []
    client.on_transcript_final.append(received.append)

    fake.publish(
        events.VoiceTranscriptFinal(
            turn_id="turn-1",
            conversation_id="conv-1",
            stream_id="s-1",
            trace_id="trace-1",
            text="stop listening",
        )
    )

    await client.dispatch_events()
    assert received and received[0].text == "stop listening"


@pytest.mark.asyncio
async def test_dispatch_partial_transcript_invokes_handler():
    fake = FakeZaraClient()
    client = build_client(fake)
    received: list[events.RuntimeEvent] = []
    client.on_transcript_partial.append(received.append)

    fake.publish(
        events.VoiceTranscriptPartial(
            turn_id="turn-1", conversation_id="conv-1", stream_id="s-1", text="hello"
        )
    )

    await client.dispatch_events()
    assert received and received[0].text == "hello"


@pytest.mark.asyncio
async def test_dispatch_completed_turn_signals_listeners():
    fake = FakeZaraClient()
    client = build_client(fake)
    signal = threading.Event()
    client.on_turn_completed.append(lambda event: signal.set())

    fake.publish(
        events.AgentCompleted(turn_id="turn-1", conversation_id="conv-1", success=True)
    )

    await client.dispatch_events()
    assert signal.is_set()


@pytest.mark.asyncio
async def test_dispatch_cancelled_turn_signals_listeners():
    fake = FakeZaraClient()
    client = build_client(fake)
    signal = threading.Event()
    client.on_turn_cancelled.append(lambda event: signal.set())

    fake.publish(
        events.TurnCancelled(turn_id="turn-1", reason="cancel command")
    )

    await client.dispatch_events()
    assert signal.is_set()


def test_connect_failure_raises_unavailable():
    class DeadFactory:
        def __call__(self, *args, **kwargs):
            raise ConnectionError("no daemon")

    client = WakeDaemonClient(client_factory=DeadFactory())

    with pytest.raises(WakeDaemonUnavailable):
        client.connect()


def test_handshake_timeout_raises_unavailable():
    class SlowClient(FakeZaraClient):
        def start(self):
            import concurrent.futures

            future = concurrent.futures.Future()
            future.set_exception(TimeoutError("ZARA/1 handshake timed out"))
            return future

    client = WakeDaemonClient(client=SlowClient())

    with pytest.raises(WakeDaemonUnavailable):
        client.connect()


def test_audio_output_format_delegates_to_client_contract():
    from zara.wake_daemon import WakeDaemonClient

    daemon = WakeDaemonClient.__new__(WakeDaemonClient)
    daemon._client = FakeZaraClient()
    assert daemon.audio_output_format == {
        "codec": "pcm_s16le",
        "sample_rate": 24000,
        "channels": 1,
    }


def test_ensure_connected_ready_client_skips_reconnect():
    fake = FakeZaraClient()

    def reconnect_with_backoff(**kwargs):
        fake._record("reconnect_with_backoff", **kwargs)
        raise AssertionError("READY client must not reconnect")

    fake.reconnect_with_backoff = reconnect_with_backoff
    client = WakeDaemonClient(client=fake)

    client.ensure_connected()

    assert fake.calls == []


def test_ensure_connected_waits_for_backoff_future_before_subscribing():
    fake = FakeZaraClient()
    fake.state = ZaraClientState.STOPPED
    gate = concurrent.futures.Future()

    def reconnect_with_backoff(**kwargs):
        fake._record("reconnect_with_backoff", **kwargs)
        return gate

    fake.reconnect_with_backoff = reconnect_with_backoff
    client = WakeDaemonClient(client=fake)
    finished = threading.Event()

    def call() -> None:
        client.ensure_connected()
        finished.set()

    worker = threading.Thread(target=call, daemon=True)
    worker.start()

    assert not finished.wait(0.2), "ensure_connected must block on the backoff future"
    assert not [call_entry for call_entry in fake.calls if call_entry[0] == "subscribe"]

    fake.state = ZaraClientState.READY
    gate.set_result(True)
    assert finished.wait(5)

    names = [call_entry[0] for call_entry in fake.calls]
    assert "reconnect_with_backoff" in names
    assert names.index("subscribe") > names.index("reconnect_with_backoff")


def test_ensure_connected_surfaces_async_reconnect_failure_as_unavailable():
    fake = FakeZaraClient()
    fake.state = ZaraClientState.STOPPED
    failed = concurrent.futures.Future()
    failed.set_exception(ConnectionError("daemon still down"))

    def reconnect_with_backoff(**kwargs):
        return failed

    fake.reconnect_with_backoff = reconnect_with_backoff
    client = WakeDaemonClient(client=fake)

    with pytest.raises(WakeDaemonUnavailable) as excinfo:
        client.ensure_connected()
    assert "reconnect" in str(excinfo.value)


def test_ensure_connected_times_out_waiting_for_reconnect():
    fake = FakeZaraClient()
    fake.state = ZaraClientState.STOPPED

    def reconnect_with_backoff(**kwargs):
        return concurrent.futures.Future()

    fake.reconnect_with_backoff = reconnect_with_backoff
    client = WakeDaemonClient(client=fake, connect_timeout=0.2)

    with pytest.raises(WakeDaemonUnavailable):
        client.ensure_connected()
