from __future__ import annotations

import asyncio

import pytest

from zara.prolog_engine import IntentResult, TimerLifecycleRecord
from zara.runtime import events
from zara.runtime.backend import LangGraphRuntimeBackend


class FakePrologEngine:
    def __init__(self) -> None:
        self.pending = []
        self.executed = []
        self.resolution = IntentResult("prolog", "timer", [10, "tea"])

    def resolve_intent(self, text):
        return self.resolution

    def execute_intent_with_response(self, intent, args):
        self.executed.append((intent, args))
        self.pending.append(
            TimerLifecycleRecord(
                kind="scheduled",
                timer_id="timer-1",
                name="tea",
                created_at_ns=100,
                due_at_ns=200,
                revision=1,
            )
        )
        return 'Timer "tea" set for 10 seconds.\n'

    def drain_timer_events(self):
        drained = list(self.pending)
        self.pending.clear()
        return drained


class RecoveringPrologEngine(FakePrologEngine):
    def __init__(self) -> None:
        super().__init__()
        self.drain_calls = 0

    def drain_timer_events(self):
        self.drain_calls += 1
        if self.drain_calls == 1:
            raise ValueError("malformed timer event")
        return super().drain_timer_events()


class FakeManager:
    def __init__(self, prolog_engine) -> None:
        self.prolog_engine = prolog_engine
        self.agent_calls = []
        self.stopped = False

    async def process_async(self, text, **kwargs):
        self.agent_calls.append((text, kwargs))
        return {"response": "agent fallback", "tool_results": []}

    def exit_conversation(self):
        self.stopped = True


@pytest.mark.asyncio
async def test_resolved_timer_uses_prolog_without_invoking_llm_and_publishes_schedule():
    prolog = FakePrologEngine()
    manager = FakeManager(prolog)
    published = []
    backend = LangGraphRuntimeBackend(lambda: manager)
    backend.bind_event_publisher(published.append)
    await backend.start()

    try:
        result = await backend.submit_turn(
            "set a timer for 10 seconds",
            turn_id="turn-1",
            conversation_id="conversation-1",
        )
    finally:
        await backend.stop()

    assert result.response == 'Timer "tea" set for 10 seconds.'
    assert prolog.executed == [("timer", [10, "tea"])]
    assert manager.agent_calls == []
    assert published == [
        events.TimerScheduled(
            timer_id="timer-1",
            name="tea",
            created_at_ns=100,
            due_at_ns=200,
            revision=1,
        )
    ]


@pytest.mark.asyncio
async def test_timer_fire_is_published_after_originating_turn_returns():
    prolog = FakePrologEngine()
    manager = FakeManager(prolog)
    published = []
    backend = LangGraphRuntimeBackend(lambda: manager)
    backend.bind_event_publisher(published.append)
    await backend.start()

    try:
        prolog.pending.append(
            TimerLifecycleRecord(
                kind="fired",
                timer_id="timer-1",
                name="tea",
                created_at_ns=100,
                due_at_ns=200,
                fired_at_ns=220,
                revision=2,
                message='Timer "tea" finished.',
            )
        )
        for _ in range(50):
            if published:
                break
            await asyncio.sleep(0.01)
    finally:
        await backend.stop()

    assert published == [
        events.TimerFired(
            timer_id="timer-1",
            name="tea",
            created_at_ns=100,
            due_at_ns=200,
            fired_at_ns=220,
            revision=2,
            message='Timer "tea" finished.',
        )
    ]


@pytest.mark.asyncio
async def test_timer_event_pump_recovers_after_one_bad_drain():
    prolog = RecoveringPrologEngine()
    prolog.pending.append(
        TimerLifecycleRecord(
            kind="fired",
            timer_id="timer-1",
            name="tea",
            created_at_ns=100,
            due_at_ns=200,
            fired_at_ns=220,
            revision=2,
            message='Timer "tea" finished.',
        )
    )
    manager = FakeManager(prolog)
    published = []
    backend = LangGraphRuntimeBackend(lambda: manager)
    backend.bind_event_publisher(published.append)
    await backend.start()

    try:
        for _ in range(50):
            if published:
                break
            await asyncio.sleep(0.01)
    finally:
        await backend.stop()

    assert prolog.drain_calls >= 2
    assert [event.timer_id for event in published] == ["timer-1"]


@pytest.mark.asyncio
async def test_unresolved_text_keeps_conversational_agent_fallback():
    prolog = FakePrologEngine()
    prolog.resolution = None
    manager = FakeManager(prolog)
    backend = LangGraphRuntimeBackend(lambda: manager)
    backend.bind_event_publisher(lambda _event: None)
    await backend.start()

    try:
        result = await backend.submit_turn(
            "what does this mean?",
            turn_id="turn-2",
            conversation_id="conversation-2",
        )
    finally:
        await backend.stop()

    assert result.response == "agent fallback"
    assert manager.agent_calls == [(
        "what does this mean?",
        {"turn_id": "turn-2", "conversation_id": "conversation-2"},
    )]
