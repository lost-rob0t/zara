"""AgentRuntimeBackend wiring for the daemon Prolog-first router (issue #244)."""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock

import pytest

from t.test_daemon_intent_router import FakeProlog, build_router
from zara.runtime.backend import AgentRuntimeBackend


def build_manager(in_conversation: bool = False) -> MagicMock:
    manager = MagicMock()
    manager.conversation_manager.in_conversation = in_conversation
    manager.process_async = AsyncMock(return_value={"response": "agent reply"})
    return manager


async def build_backend(manager, prolog=None) -> AgentRuntimeBackend:
    router = build_router(prolog=prolog)
    backend = AgentRuntimeBackend(lambda: manager, router=router)
    await backend.start()
    return backend


@pytest.mark.asyncio
async def test_command_turn_responds_without_agent():
    manager = build_manager()
    prolog = FakeProlog()
    backend = await build_backend(manager, prolog)

    result = await backend.submit_turn("open firefox", turn_id="turn-0001")

    assert result.response == "Executed: open ['firefox']"
    manager.process_async.assert_not_awaited()
    assert prolog.executed == [("open", ["firefox"])]


@pytest.mark.asyncio
async def test_agent_fallback_turn_reaches_manager():
    manager = build_manager()
    backend = await build_backend(manager)

    result = await backend.submit_turn("tell me a story", turn_id="turn-0002")

    assert result.response == "agent reply"
    manager.process_async.assert_awaited_once()


@pytest.mark.asyncio
async def test_greeting_enters_conversation():
    manager = build_manager()
    backend = await build_backend(manager)

    result = await backend.submit_turn("zara", turn_id="turn-0003")

    assert result.response
    manager.conversation_manager.enter_conversation.assert_called_once()
    manager.process_async.assert_not_awaited()


@pytest.mark.asyncio
async def test_end_conversation_exits_conversation():
    manager = build_manager(in_conversation=True)
    backend = await build_backend(manager)

    result = await backend.submit_turn("goodbye", turn_id="turn-0004")

    assert result.response
    manager.conversation_manager.exit_conversation.assert_called_once()


@pytest.mark.asyncio
async def test_delegate_passes_latency_trace_to_agent():
    from zara.latency import LatencyTrace

    manager = build_manager()
    backend = await build_backend(manager)
    trace = LatencyTrace(trace_id="trace-voice-1")

    await backend.submit_turn(
        "tell me a story",
        turn_id="turn-0005",
        latency_trace=trace,
    )

    kwargs = manager.process_async.await_args.kwargs
    assert kwargs.get("latency_trace") is trace


@pytest.mark.asyncio
async def test_router_state_follows_conversation_mode():
    seen_states: list[str] = []

    class StatefulProlog(FakeProlog):
        def resolve_intent(self, text: str, state: str = "passive"):
            seen_states.append(state)
            return None

    manager = build_manager(in_conversation=True)
    backend = await build_backend(manager, prolog=StatefulProlog())

    await backend.submit_turn("open firefox", turn_id="turn-0006")

    assert seen_states == ["conversation"]


@pytest.mark.asyncio
async def test_router_skipped_when_absent():
    manager = build_manager()
    backend = AgentRuntimeBackend(lambda: manager)
    await backend.start()

    result = await backend.submit_turn("open firefox", turn_id="turn-0007")

    assert result.response == "agent reply"
    manager.process_async.assert_awaited_once()
