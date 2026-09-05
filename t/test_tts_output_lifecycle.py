"""Lifecycle regressions for the daemon TTS output bridge."""

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


class _FakeEngine:
    async def synthesize_stream(self, text: str):
        yield StreamChunk(
            provider="fake",
            audio=_wav_bytes(),
            audio_format="wav",
            first_chunk=True,
        )


def _bridge() -> TtsOutputBridge:
    bus = RuntimeEventBus()
    return TtsOutputBridge(
        subscription=bus.subscribe(),
        publish=lambda _event: None,
        engine_factory=_FakeEngine,
    )


async def _complete_turn(bridge: TtsOutputBridge, turn_id: str) -> None:
    await bridge.handle_event(
        events.AssistantDelta(
            turn_id=turn_id,
            conversation_id="conv-1",
            text="Done. ",
        )
    )
    await bridge.handle_event(
        events.AssistantComplete(
            turn_id=turn_id,
            conversation_id="conv-1",
            text="Done.",
        )
    )
    await bridge.wait_for_idle()
    await bridge._wait_for_tasks(timeout=0.1)


@pytest.mark.asyncio
async def test_successful_turn_releases_completed_state():
    bridge = _bridge()
    await _complete_turn(bridge, "turn-1")
    assert bridge._turns == {}


@pytest.mark.asyncio
async def test_many_successful_turns_do_not_accumulate_state():
    bridge = _bridge()
    for index in range(32):
        await _complete_turn(bridge, f"turn-{index}")
    assert bridge._turns == {}


@pytest.mark.asyncio
async def test_stale_task_cannot_remove_replacement_with_same_turn_id():
    bridge = _bridge()
    event = events.AssistantDelta(
        turn_id="turn-reused",
        conversation_id="conv-1",
        text="ignored",
    )
    stale = bridge._state_for(event)
    bridge._turns.pop(event.turn_id)
    replacement = bridge._state_for(event)

    stale.cancelled = True
    await stale.queue.put("ignored")
    await bridge._synthesize_turn(stale)

    assert bridge._turns[event.turn_id] is replacement
