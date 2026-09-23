"""OpenAI-compatible Qwen3-TTS client contract (qwentts.cpp server).

The server exposes POST /v1/audio/speech (response_format "pcm" streams
s16le 24 kHz mono chunked, "wav" returns a one-shot RIFF file),
GET/POST /v1/audio/voices, DELETE /v1/audio/voices/{name}, and
GET /health. The legacy PyTorch endpoints are gone.
"""

from __future__ import annotations

import asyncio
import base64
import json
from pathlib import Path

import pytest

import zara.tts.qwen as qwen_module
from zara.tts.engine import TTSEngine, supports_streaming
from zara.tts.qwen import Qwen3TTSClient


class FakeContent:
    def __init__(self, chunks: list[bytes]):
        self._chunks = chunks

    async def iter_any(self):
        for chunk in self._chunks:
            yield chunk


class FakeResponse:
    def __init__(self, status: int = 200, body: bytes = b"", payload: dict | None = None,
                 chunks: list[bytes] | None = None):
        self.status = status
        self._body = body
        self._payload = payload
        self.content = FakeContent(chunks or [])

    async def read(self) -> bytes:
        return self._body

    async def json(self, **kwargs) -> dict:
        if self._payload is None:
            raise ValueError("no json payload")
        return self._payload

    async def text(self) -> str:
        return self._body.decode(errors="replace")


class FakeRequestContext:
    def __init__(self, response: FakeResponse):
        self._response = response

    async def __aenter__(self):
        return self._response

    async def __aexit__(self, exc_type, exc, traceback):
        return False


class FakeSession:
    def __init__(self):
        self.requests: list[dict] = []
        self.responses: dict[tuple[str, str], FakeResponse] = {}
        self.closed = False

    def _respond(self, method: str, url: str):
        self.requests.append({"method": method, "url": url, "kwargs": None})
        return FakeRequestContext(self._next_response(method, url))

    def _next_response(self, method: str, url: str) -> FakeResponse:
        for (rmethod, rurl), response in self.responses.items():
            if rmethod == method and rurl in url:
                return response
        return FakeResponse(status=404, body=b"not found")

    def get(self, url, **kwargs):
        self.requests.append({"method": "GET", "url": url, "kwargs": kwargs})
        return FakeRequestContext(self._next_response("GET", url))

    def post(self, url, json=None, **kwargs):
        self.requests.append({"method": "POST", "url": url, "kwargs": {"json": json, **kwargs}})
        return FakeRequestContext(self._next_response("POST", url))

    def delete(self, url, **kwargs):
        self.requests.append({"method": "DELETE", "url": url, "kwargs": kwargs})
        return FakeRequestContext(self._next_response("DELETE", url))

    async def close(self):
        self.closed = True


def client_with(session: FakeSession) -> Qwen3TTSClient:
    client = Qwen3TTSClient("http://tts.test")
    client.session = session
    return client


def wav_bytes() -> bytes:
    return b"RIFFxxxxWAVEfmt data"


@pytest.mark.asyncio
async def test_speech_posts_openai_json_body_and_returns_wav():
    session = FakeSession()
    session.responses[("POST", "/v1/audio/speech")] = FakeResponse(body=wav_bytes())
    client = client_with(session)

    audio = await client.synthesize_speech(text="Hello world.", voice="zara")

    assert audio == wav_bytes()
    request = session.requests[0]
    assert request["url"] == "http://tts.test/v1/audio/speech"
    assert request["kwargs"]["json"] == {
        "input": "Hello world.",
        "voice": "zara",
        "response_format": "wav",
    }


@pytest.mark.asyncio
async def test_stream_speech_requests_pcm_and_yields_chunks_in_order():
    session = FakeSession()
    session.responses[("POST", "/v1/audio/speech")] = FakeResponse(
        chunks=[b"aaaa", b"bbbb", b"cccc"]
    )
    client = client_with(session)

    received = [chunk async for chunk in client.stream_speech(text="Hello.", voice="zara")]

    assert received == [b"aaaa", b"bbbb", b"cccc"]
    request = session.requests[0]
    assert request["kwargs"]["json"]["response_format"] == "pcm"


@pytest.mark.asyncio
async def test_stream_speech_does_not_apply_a_total_timeout():
    session = FakeSession()
    session.responses[("POST", "/v1/audio/speech")] = FakeResponse(chunks=[b"aaaa"])
    client = client_with(session)

    _ = [chunk async for chunk in client.stream_speech(text="Hello.", voice="zara")]

    timeout = session.requests[0]["kwargs"]["timeout"]
    assert timeout is None or getattr(timeout, "total", None) is None


@pytest.mark.asyncio
async def test_stream_speech_surfaces_http_errors():
    session = FakeSession()
    session.responses[("POST", "/v1/audio/speech")] = FakeResponse(status=500, body=b"boom")
    client = client_with(session)

    with pytest.raises(RuntimeError, match="500"):
        _ = [chunk async for chunk in client.stream_speech(text="Hello.", voice="zara")]


@pytest.mark.asyncio
async def test_list_voices_parses_the_registry():
    session = FakeSession()
    session.responses[("GET", "/v1/audio/voices")] = FakeResponse(
        payload={"voices": [{"name": "zara", "kind": "registered"},
                            {"name": "vivian", "kind": "speaker"}]}
    )
    client = client_with(session)

    voices = await client.list_voices()

    assert voices == ["zara", "vivian"]
    assert session.requests[0]["url"] == "http://tts.test/v1/audio/voices"


@pytest.mark.asyncio
async def test_register_voice_posts_base64_wav_and_transcript(tmp_path: Path):
    session = FakeSession()
    session.responses[("POST", "/v1/audio/voices")] = FakeResponse(payload={"ok": True})
    audio = tmp_path / "zara.wav"
    audio.write_bytes(wav_bytes())
    client = client_with(session)

    result = await client.register_voice("zara", str(audio), "Exact words spoken.")

    assert result == {"ok": True}
    body = session.requests[0]["kwargs"]["json"]
    assert body["name"] == "zara"
    assert body["ref_text"] == "Exact words spoken."
    assert base64.b64decode(body["wav_b64"]) == wav_bytes()


@pytest.mark.asyncio
async def test_register_voice_missing_file_raises():
    client = client_with(FakeSession())

    with pytest.raises(FileNotFoundError):
        await client.register_voice("zara", "/nonexistent/voice.wav")


@pytest.mark.asyncio
async def test_delete_voice_uses_the_named_route():
    session = FakeSession()
    session.responses[("DELETE", "/v1/audio/voices/zara")] = FakeResponse(payload={"ok": True})
    client = client_with(session)

    await client.delete_voice("zara")

    assert session.requests[0]["method"] == "DELETE"
    assert session.requests[0]["url"].endswith("/v1/audio/voices/zara")


@pytest.mark.asyncio
async def test_health_checks_liveness():
    session = FakeSession()
    session.responses[("GET", "/health")] = FakeResponse(payload={"status": "ok"})
    client = client_with(session)

    assert await client.health() == {"status": "ok"}


def test_legacy_pytorch_endpoints_are_gone():
    source = Path(qwen_module.__file__).read_text(encoding="utf-8")
    for legacy in (
        "synthesize_speech/",
        "base_tts/",
        "upload_audio",
        "change_voice",
        "reference_speaker",
    ):
        assert legacy not in source


@pytest.mark.asyncio
async def test_qwen3_supports_streaming():
    assert supports_streaming("qwen3")


@pytest.mark.asyncio
async def test_engine_streams_qwen3_pcm_chunks(monkeypatch):
    class FakeQwen:
        def __init__(self, url, **kwargs):
            pass

        async def stream_speech(self, text, voice):
            for index, chunk in enumerate([b"aaaa", b"bbbb"]):
                yield chunk

        async def close(self):
            pass

    monkeypatch.setattr("zara.tts.engine.Qwen3TTSClient", FakeQwen)
    engine = TTSEngine("qwen3", {"tts": {"endpoint": "http://qwen.test"}})

    chunks = [chunk async for chunk in engine.synthesize_stream("Hello.")]

    assert [chunk.audio for chunk in chunks] == [b"aaaa", b"bbbb"]
    assert all(chunk.audio_format == "pcm" for chunk in chunks)
    assert chunks[0].first_chunk and not chunks[1].first_chunk
    assert all(chunk.error is None for chunk in chunks)


@pytest.mark.asyncio
async def test_engine_stream_close_on_cancellation(monkeypatch):
    closed = asyncio.Event()

    class FakeQwen:
        def __init__(self, url, **kwargs):
            pass

        async def stream_speech(self, text, voice):
            yield b"aaaa"
            await asyncio.Event().wait()

        async def close(self):
            closed.set()

    monkeypatch.setattr("zara.tts.engine.Qwen3TTSClient", FakeQwen)
    engine = TTSEngine("qwen3")
    chunks = []
    first_chunk = asyncio.Event()

    async def consume():
        async for chunk in engine.synthesize_stream("Hello."):
            chunks.append(chunk)
            first_chunk.set()

    task = asyncio.create_task(consume())
    await asyncio.wait_for(first_chunk.wait(), timeout=2)
    assert chunks and chunks[0].audio == b"aaaa"

    task.cancel()
    with pytest.raises(asyncio.CancelledError):
        await task

    assert closed.is_set()


@pytest.mark.asyncio
async def test_pcm_chunks_pass_through_the_output_bridge_decoder():
    from zara.runtime.tts_output import TtsOutputBridge

    bridge = TtsOutputBridge(subscription=None, publish=lambda event: None,
                             engine_factory=lambda: None, sample_rate=24000)

    pcm = b"\x00\x01\x02\x03"
    assert await asyncio.to_thread(bridge._decode_to_pcm, pcm, "pcm") == pcm


@pytest.mark.asyncio
async def test_same_target_voice_registration_has_one_verified_winner(tmp_path: Path):
    audio = tmp_path / "voice.wav"
    audio.write_bytes(wav_bytes())
    inventory: set[str] = set()
    register_calls = 0

    class DynamicContext:
        def __init__(self, method: str, payload: dict | None = None):
            self.method = method
            self.payload = payload or {}

        async def __aenter__(self):
            nonlocal register_calls
            if self.method == "GET":
                return FakeResponse(
                    payload={"voices": [{"name": name} for name in sorted(inventory)]}
                )
            if self.method == "POST":
                register_calls += 1
                await asyncio.sleep(0.05)
                inventory.add(str(self.payload["name"]))
                return FakeResponse(payload={"ok": True})
            raise AssertionError(self.method)

        async def __aexit__(self, exc_type, exc, traceback):
            return False

    class DynamicSession:
        closed = False

        def get(self, url, **kwargs):
            return DynamicContext("GET")

        def post(self, url, json=None, **kwargs):
            return DynamicContext("POST", json)

        async def close(self):
            self.closed = True

    first = Qwen3TTSClient("http://tts-race.test")
    second = Qwen3TTSClient("http://tts-race.test")
    first.session = DynamicSession()
    second.session = DynamicSession()

    results = await asyncio.gather(
        first.register_voice("same-target", str(audio)),
        second.register_voice("same-target", str(audio)),
        return_exceptions=True,
    )

    successes = [result for result in results if isinstance(result, dict)]
    failures = [result for result in results if isinstance(result, RuntimeError)]
    assert len(successes) == 1
    assert len(failures) == 1
    assert "already exists" in str(failures[0])
    assert register_calls == 1
    assert inventory == {"same-target"}


@pytest.mark.asyncio
async def test_same_target_voice_delete_has_one_verified_winner():
    inventory = {"same-target"}
    delete_calls = 0

    class DynamicContext:
        def __init__(self, method: str):
            self.method = method

        async def __aenter__(self):
            nonlocal delete_calls
            if self.method == "GET":
                return FakeResponse(
                    payload={"voices": [{"name": name} for name in sorted(inventory)]}
                )
            if self.method == "DELETE":
                delete_calls += 1
                await asyncio.sleep(0.05)
                inventory.discard("same-target")
                return FakeResponse(payload={"ok": True})
            raise AssertionError(self.method)

        async def __aexit__(self, exc_type, exc, traceback):
            return False

    class DynamicSession:
        closed = False

        def get(self, url, **kwargs):
            return DynamicContext("GET")

        def delete(self, url, **kwargs):
            return DynamicContext("DELETE")

        async def close(self):
            self.closed = True

    first = Qwen3TTSClient("http://tts-race.test")
    second = Qwen3TTSClient("http://tts-race.test")
    first.session = DynamicSession()
    second.session = DynamicSession()

    results = await asyncio.gather(
        first.delete_voice("same-target"),
        second.delete_voice("same-target"),
        return_exceptions=True,
    )

    successes = [result for result in results if isinstance(result, dict)]
    failures = [result for result in results if isinstance(result, RuntimeError)]
    assert len(successes) == 1
    assert len(failures) == 1
    assert "does not exist" in str(failures[0])
    assert delete_calls == 1
    assert inventory == set()
