"""Provider-recovery regressions for daemon streaming TTS (#29)."""

from __future__ import annotations

import io
import wave

import pytest

from zara.runtime import events
from zara.runtime.bridge import RuntimeEventBus
from zara.runtime.tts_output import TtsOutputBridge
from zara.tts.engine import StreamChunk


def _wav_bytes() -> bytes:
    buffer = io.BytesIO()
    with wave.open(buffer, "wb") as handle:
        handle.setnchannels(1)
        handle.setsampwidth(2)
        handle.setframerate(24000)
        handle.writeframes(b"\x00\x01" * 240)
    return buffer.getvalue()


class _Engine:
    def __init__(self) -> None:
        self.calls: list[str] = []

    async def synthesize_stream(self, text: str):
        self.calls.append(text)
        yield StreamChunk(
            provider="fake",
            audio=_wav_bytes(),
            audio_format="wav",
            first_chunk=True,
        )


@pytest.mark.asyncio
async def test_transient_engine_bootstrap_failure_recovers_on_next_turn():
    bus = RuntimeEventBus()
    published: list[events.RuntimeEvent] = []
    engine = _Engine()
    factory_calls = 0

    def factory():
        nonlocal factory_calls
        factory_calls += 1
        if factory_calls == 1:
            raise RuntimeError("temporary provider bootstrap failure")
        return engine

    bridge = TtsOutputBridge(
        subscription=bus.subscribe(),
        publish=published.append,
        engine_factory=factory,
    )

    await bridge.handle_event(
        events.AssistantDelta(
            turn_id="failed-turn",
            conversation_id="conv-29",
            text="First phrase. Second phrase. ",
        )
    )
    await bridge.handle_event(
        events.AssistantComplete(
            turn_id="failed-turn",
            conversation_id="conv-29",
            text="",
        )
    )
    await bridge.wait_for_idle()

    assert factory_calls == 1
    assert engine.calls == []

    await bridge.handle_event(
        events.AssistantDelta(
            turn_id="recovery-turn",
            conversation_id="conv-29",
            text="Recovered audio",
        )
    )
    await bridge.handle_event(
        events.AssistantComplete(
            turn_id="recovery-turn",
            conversation_id="conv-29",
            text="Recovered audio",
        )
    )
    await bridge.wait_for_idle()

    assert factory_calls == 2
    assert engine.calls == ["Recovered audio"]
    assert any(
        isinstance(event, events.AudioOutputChunk)
        and event.turn_id == "recovery-turn"
        for event in published
    )
