"""Backend turn services: assistant streaming + memory persistence (#244)."""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock

import pytest

from t.test_daemon_intent_router import FakeProlog, build_router
from zara.agent import stream_events
from zara.runtime.backend import AgentRuntimeBackend
from zara.runtime.events import AssistantComplete, AssistantDelta


class FakeConversationManager:
    def __init__(self, in_conversation: bool = False) -> None:
        self.in_conversation = in_conversation
        self.entered = 0
        self.exited = 0
        self.conversation_history = []

    def enter_conversation(self):
        self.in_conversation = True
        self.entered += 1

    def exit_conversation(self):
        self.in_conversation = False
        self.exited += 1


class FakeManager:
    def __init__(self, memory_manager=None) -> None:
        self.conversation_manager = FakeConversationManager()
        self.process_async = AsyncMock(return_value={"response": "agent reply"})
        if memory_manager is not None:
            self.memory_manager = memory_manager


async def build_backend(manager, prolog=None) -> AgentRuntimeBackend:
    router = build_router(prolog=prolog) if prolog is not None else build_router()
    backend = AgentRuntimeBackend(lambda: manager, router=router)
    await backend.start()
    return backend


@pytest.mark.asyncio
async def test_agent_turn_streams_phrase_events_to_publisher():
    manager = FakeManager()
    publisher = MagicMock()

    async def fake_process(text, **kwargs):
        stream_publisher = kwargs.get("stream_publisher")
        stream_publisher(stream_events.SentenceReady(text="Hello there."))
        stream_publisher(stream_events.Completed(full_text="Hello there."))
        return {"response": "Hello there."}

    manager.process_async = AsyncMock(side_effect=fake_process)
    backend = await build_backend(manager)
    backend.bind_event_publisher(publisher)

    await backend.submit_turn(
        "tell me a story",
        turn_id="turn-0011",
        conversation_id="conv-77",
    )

    delta_calls = [
        call for call in publisher.call_args_list if call.args[0] is not None
    ]
    published = [call.args[0] for call in delta_calls]
    deltas = [event for event in published if isinstance(event, AssistantDelta)]
    completes = [event for event in published if isinstance(event, AssistantComplete)]
    assert [delta.text for delta in deltas] == ["Hello there."]
    assert deltas[0].turn_id == "turn-0011"
    assert deltas[0].conversation_id == "conv-77"
    assert completes and completes[0].text == "Hello there."


@pytest.mark.asyncio
async def test_agent_turn_without_publisher_still_completes():
    manager = FakeManager()
    backend = await build_backend(manager)

    result = await backend.submit_turn("tell me a story", turn_id="turn-0012")

    assert result.response == "agent reply"


@pytest.mark.asyncio
async def test_command_turn_persists_memory():
    memory = MagicMock()
    memory.start_session.return_value = "sess-9"
    manager = FakeManager(memory_manager=memory)
    backend = await build_backend(
        manager, prolog=FakeProlog()
    )

    await backend.submit_turn("open firefox", turn_id="turn-0013")

    persisted = [call.args for call in memory.add_message.call_args_list]
    assert ("sess-9", "user", "open firefox") in persisted
    assert ("sess-9", "assistant", "Executed: open ['firefox']") in persisted


@pytest.mark.asyncio
async def test_end_conversation_summarises_and_rotates_session():
    memory = MagicMock()
    memory.start_session.side_effect = ["sess-1", "sess-2"]
    memory.summarise_session.return_value = "summary"
    manager = FakeManager(memory_manager=memory)
    backend = await build_backend(manager)

    await backend.submit_turn("open firefox", turn_id="turn-0014")
    await backend.submit_turn("goodbye", turn_id="turn-0015")

    memory.summarise_session.assert_called_once()
    assert memory.summarise_session.call_args.args[0] == "sess-1"
    assert memory.start_session.call_count == 2


@pytest.mark.asyncio
async def test_missing_memory_manager_is_tolerated():
    manager = FakeManager(memory_manager=None)
    backend = await build_backend(manager)

    result = await backend.submit_turn("tell me a story", turn_id="turn-0016")

    assert result.response == "agent reply"
