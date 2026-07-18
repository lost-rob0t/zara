import asyncio
import io
import subprocess
import sys
import wave
from types import SimpleNamespace
from unittest.mock import AsyncMock

import pytest

import zara.tts.engine as engine_module
import zara.tts.qwen as qwen_module
from zara.tts import SynthesisResult, TTSEngine
from zara.tts.qwen import Qwen3TTSClient
from zara.wake import WakeWordListener


def wav_bytes():
    output = io.BytesIO()
    with wave.open(output, "wb") as audio:
        audio.setnchannels(1)
        audio.setsampwidth(2)
        audio.setframerate(16000)
        audio.writeframes(b"\x00\x00" * 16)
    return output.getvalue()


class FakeProcess:
    def __init__(self, stdout=b"", stderr=b"", returncode=0):
        self.stdout = stdout
        self.stderr = stderr
        self.returncode = returncode
        self.killed = False

    async def communicate(self, input_data=None):
        return self.stdout, self.stderr

    async def wait(self):
        return self.returncode

    def kill(self):
        self.killed = True
        self.returncode = -9


@pytest.mark.asyncio
async def test_piper_returns_wav_container(monkeypatch, tmp_path):
    model = tmp_path / "voice.onnx"
    model.write_bytes(b"model")
    monkeypatch.setattr(
        engine_module.subprocess,
        "run",
        lambda *args, **kwargs: subprocess.CompletedProcess(args, 0),
    )
    process = FakeProcess(stdout=wav_bytes())
    create_process = AsyncMock(return_value=process)
    monkeypatch.setattr(asyncio, "create_subprocess_exec", create_process)
    engine = TTSEngine("local", {"tts": {"model_path": str(model)}})

    result = await engine.synthesize_async("hello")

    assert result.success and result.audio_format == "wav"
    assert "--output_file" in create_process.await_args.args
    assert "--output-raw" not in create_process.await_args.args


@pytest.mark.asyncio
async def test_espeak_returns_wav_container(monkeypatch):
    monkeypatch.setattr(
        engine_module.subprocess,
        "run",
        lambda *args, **kwargs: subprocess.CompletedProcess(args, 1),
    )

    async def create_process(*args, **kwargs):
        output_path = args[2]
        with open(output_path, "wb") as output:
            output.write(wav_bytes())
        return FakeProcess()

    monkeypatch.setattr(asyncio, "create_subprocess_exec", create_process)
    engine = TTSEngine("local")

    result = await engine.synthesize_async("hello")

    assert result.success and result.audio_format == "wav"


@pytest.mark.asyncio
async def test_qwen_returns_wav_and_receives_timeouts(monkeypatch):
    captured = {}

    class FakeQwen:
        def __init__(self, url, **kwargs):
            captured.update(url=url, **kwargs)

        async def synthesize_speech(self, **kwargs):
            return wav_bytes()

        async def close(self):
            captured["closed"] = True

    monkeypatch.setattr(engine_module, "Qwen3TTSClient", FakeQwen)
    engine = TTSEngine(
        "qwen3",
        {
            "tts": {
                "endpoint": "http://qwen.test",
                "connect_timeout": 1,
                "read_timeout": 2,
                "total_timeout": 3,
            }
        },
    )

    result = await engine.synthesize_async("hello")
    await engine.close()

    assert result.success and result.audio_format == "wav"
    assert captured == {
        "url": "http://qwen.test",
        "connect_timeout": 1.0,
        "read_timeout": 2.0,
        "total_timeout": 3.0,
        "closed": True,
    }


@pytest.mark.asyncio
async def test_edge_returns_mp3_container(monkeypatch):
    class Communicate:
        def __init__(self, text, voice):
            pass

        async def save(self, path):
            with open(path, "wb") as output:
                output.write(b"ID3edge")

    monkeypatch.setitem(sys.modules, "edge_tts", SimpleNamespace(Communicate=Communicate))
    engine = TTSEngine("edge")

    result = await engine.synthesize_async("hello")

    assert result.success and result.audio_format == "mp3"


@pytest.mark.asyncio
async def test_elevenlabs_returns_mp3_container(monkeypatch):
    from zara.tts import elevenlabs

    monkeypatch.setattr(elevenlabs, "synthesize", AsyncMock(return_value=b"ID3eleven"))
    close = AsyncMock()
    monkeypatch.setattr(elevenlabs, "close", close)
    engine = TTSEngine(
        "11labs",
        {"tts": {"elevenlabs_api_key": "key", "elevenlabs_voice_id": "voice"}},
    )

    result = await engine.synthesize_async("hello")

    assert result.success and result.audio_format == "mp3"
    close.assert_awaited_once()


@pytest.mark.asyncio
async def test_timeout_returns_actionable_failure(monkeypatch):
    engine = TTSEngine("qwen3", {"tts": {"total_timeout": 0.001}})

    async def slow_provider(text):
        await asyncio.sleep(1)

    monkeypatch.setattr(engine, "_synthesize_provider", slow_provider)

    result = await engine.synthesize_async("hello")

    assert not result.success
    assert "timed out" in result.error


@pytest.mark.asyncio
async def test_cancellation_returns_structured_status(monkeypatch):
    engine = TTSEngine("qwen3")
    started = asyncio.Event()

    async def blocked_provider(text):
        started.set()
        await asyncio.Event().wait()

    monkeypatch.setattr(engine, "_synthesize_provider", blocked_provider)
    task = asyncio.create_task(engine.synthesize_async("hello"))
    await started.wait()
    task.cancel()

    result = await task

    assert not result.success and result.cancelled


@pytest.mark.asyncio
async def test_qwen_client_closes_on_cancellation(monkeypatch):
    started = asyncio.Event()
    closed = asyncio.Event()

    class FakeQwen:
        def __init__(self, *args, **kwargs):
            pass

        async def synthesize_speech(self, **kwargs):
            started.set()
            await asyncio.Event().wait()

        async def close(self):
            closed.set()

    monkeypatch.setattr(engine_module, "Qwen3TTSClient", FakeQwen)
    engine = TTSEngine("qwen3")
    task = asyncio.create_task(engine.synthesize_async("hello"))
    await started.wait()
    task.cancel()

    result = await task

    assert result.cancelled
    assert closed.is_set()


@pytest.mark.asyncio
@pytest.mark.parametrize("audio", [b"", b"not a wav"])
async def test_empty_and_invalid_audio_fail_cleanly(monkeypatch, audio):
    engine = TTSEngine("qwen3")

    async def invalid_provider(text):
        return audio, "wav"

    monkeypatch.setattr(engine, "_synthesize_provider", invalid_provider)

    result = await engine.synthesize_async("hello")

    assert not result.success
    assert "audio" in result.error.lower()


@pytest.mark.asyncio
async def test_qwen_upload_and_change_close_files(monkeypatch, tmp_path):
    audio_path = tmp_path / "sample.wav"
    audio_path.write_bytes(wav_bytes())
    files = []

    class FormData:
        def add_field(self, name, value, **kwargs):
            if name == "file":
                files.append(value)

    class Response:
        status = 200

        async def json(self):
            return {"ok": True}

        async def read(self):
            return wav_bytes()

    class RequestContext:
        async def __aenter__(self):
            return Response()

        async def __aexit__(self, exc_type, exc, traceback):
            return False

    class Session:
        closed = False

        def post(self, url, data):
            return RequestContext()

        async def close(self):
            self.closed = True

    monkeypatch.setattr(qwen_module.aiohttp, "FormData", FormData)
    client = Qwen3TTSClient()
    session = Session()
    client.session = session

    await client.upload_voice(str(audio_path), "voice")
    await client.change_voice(str(audio_path), "voice")
    await client.close()

    assert all(audio_file.closed for audio_file in files)
    assert session.closed and client.session is None


@pytest.mark.asyncio
async def test_replacement_utterances_never_overlap():
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.tts_lock = asyncio.Lock()
    listener.tts_task = None
    listener.tts_stop_event = None
    listener.tts_player_proc = None
    listener.log = lambda message: None
    active = 0
    maximum_active = 0
    started = asyncio.Event()

    async def play(text, stop_event):
        nonlocal active, maximum_active
        active += 1
        maximum_active = max(maximum_active, active)
        started.set()
        try:
            await stop_event.wait()
        finally:
            active -= 1

    listener._synthesize_and_play_task = play

    first = await listener.synthesize_and_play_async("first")
    await started.wait()
    second = await listener.synthesize_and_play_async("second")
    await asyncio.sleep(0)

    assert first.done()
    assert not second.done()
    assert maximum_active == 1
    await listener._stop_tts()


@pytest.mark.asyncio
async def test_synthesis_failure_reaches_wake_status():
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.tts_client = SimpleNamespace(
        synthesize_async=AsyncMock(
            return_value=SynthesisResult(
                provider="qwen3",
                success=False,
                error="invalid audio",
            )
        )
    )
    listener.tts_config = {"provider": "qwen3"}
    listener.last_tts_status = None
    listener.log = lambda message: None

    status = await listener._synthesize_and_play_task("hello", asyncio.Event())

    assert not status.success
    assert status.error == "invalid audio"
    assert listener.last_tts_status == status


@pytest.mark.asyncio
async def test_mp3_playback_returns_structured_status(monkeypatch):
    class Player(FakeProcess):
        def __init__(self):
            super().__init__()
            self.returncode = None

        async def communicate(self, input_data=None):
            self.returncode = 0
            return b"", b""

    player = Player()
    monkeypatch.setattr(
        asyncio,
        "create_subprocess_exec",
        AsyncMock(return_value=player),
    )
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.tts_config = {"provider": "edge"}
    listener.tts_player_proc = None
    listener._apply_conversation_grace = lambda: None

    status = await listener._play_audio_task(
        b"ID3audio",
        "mp3",
        asyncio.Event(),
    )

    assert status.success
    assert listener.tts_player_proc is None
