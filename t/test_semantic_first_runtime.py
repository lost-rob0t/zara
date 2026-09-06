"""Semantic-first daemon routing regressions for issue #160."""

from __future__ import annotations

from dataclasses import dataclass

import pytest

from zara.runtime.backend import (
    DETERMINISTIC_COMMAND_FAILED,
    DETERMINISTIC_COMMAND_UNAVAILABLE,
    LangGraphRuntimeBackend,
)
from zara.runtime.intent_router import RouteDecision


class FakeConversationManager:
    def __init__(self) -> None:
        self.in_conversation = False
        self.conversation_history = []

    def enter_conversation(self) -> None:
        self.in_conversation = True

    def exit_conversation(self) -> None:
        self.in_conversation = False

    def update_activity(self) -> None:
        pass


class FakeManager:
    def __init__(self) -> None:
        self.conversation_manager = FakeConversationManager()
        self.memory_manager = None
        self.process_calls: list[str] = []

    async def process_async(self, text: str, **kwargs):
        self.process_calls.append(text)
        return {"response": "model response", "tool_results": []}

    async def shutdown_async(self) -> None:
        pass


@dataclass
class FakeRouter:
    decision: RouteDecision
    calls: list[str]

    async def route(self, text: str, **kwargs):
        self.calls.append(text)
        return self.decision


@pytest.mark.asyncio
async def test_command_never_reaches_llm_when_router_is_unavailable():
    manager = FakeManager()
    backend = LangGraphRuntimeBackend(lambda: manager, router=None)
    await backend.start()

    result = await backend.submit_turn(
        "set a timer for 2 hours",
        turn_id="turn-1",
    )

    assert result.response == DETERMINISTIC_COMMAND_UNAVAILABLE
    assert result.metadata["route"] == "deterministic_unavailable"
    assert manager.process_calls == []


@pytest.mark.asyncio
async def test_command_never_reaches_llm_after_deterministic_failure():
    manager = FakeManager()
    router = FakeRouter(RouteDecision("delegate"), [])
    backend = LangGraphRuntimeBackend(lambda: manager, router=router)
    await backend.start()

    result = await backend.submit_turn(
        "set a timer for 2 hours",
        turn_id="turn-2",
    )

    assert result.response == DETERMINISTIC_COMMAND_FAILED
    assert result.metadata["route"] == "deterministic_failed"
    assert router.calls == ["set a timer for 2 hours"]
    assert manager.process_calls == []


@pytest.mark.asyncio
async def test_successful_timer_route_returns_without_llm():
    manager = FakeManager()
    router = FakeRouter(
        RouteDecision("respond", "Executed: timer [7200, '']"),
        [],
    )
    backend = LangGraphRuntimeBackend(lambda: manager, router=router)
    await backend.start()

    result = await backend.submit_turn(
        "set a timer for 2 hours",
        turn_id="turn-3",
    )

    assert result.response == "Executed: timer [7200, '']"
    assert router.calls == ["set a timer for 2 hours"]
    assert manager.process_calls == []


@pytest.mark.asyncio
async def test_conversational_text_still_uses_llm_fallback():
    manager = FakeManager()
    router = FakeRouter(RouteDecision("delegate"), [])
    backend = LangGraphRuntimeBackend(lambda: manager, router=router)
    await backend.start()

    result = await backend.submit_turn(
        "what do you think about symbolic memory",
        turn_id="turn-4",
    )

    assert result.response == "model response"
    assert router.calls == ["what do you think about symbolic memory"]
    assert manager.process_calls == ["what do you think about symbolic memory"]
