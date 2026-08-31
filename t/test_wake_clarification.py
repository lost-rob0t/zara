from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock

import pytest

from zara.prolog_engine import IntentResult
from zara.runtime.clarification import ClarificationCoordinator
from zara.wake import WakeWordListener


class FakeProlog:
    def __init__(self) -> None:
        self.executed: list[tuple[str, list]] = []
        self.results: dict[str, IntentResult] = {
            "open": IntentResult("pending", "open", ["app"]),
            "text": IntentResult("pending", "text", ["contact", "message"]),
            "schedule": IntentResult(
                "pending", "python(schedule_todo)", ["task"]
            ),
            "mystery": IntentResult("pending", "mystery", ["thing"]),
            "run": IntentResult("pending", "mystery", ["thing"]),
            "open gimp": IntentResult("prolog", "open", ["gimp"]),
        }

    def resolve_intent(self, text: str, state: str = "passive"):
        return self.results.get(text.strip().lower())

    def execute_intent(self, name: str, args) -> bool:
        self.executed.append((name, list(args)))
        return True

    def get_app_mapping(self, target: str):
        return None


def build_listener(prolog: FakeProlog) -> WakeWordListener:
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.state = "CONVERSATION"
    listener.prolog = prolog
    listener.agent_manager = MagicMock()
    listener.agent_manager.conversation_manager.in_conversation = True
    listener.agent_manager.process_async = AsyncMock(
        return_value={"response": "agent reply"}
    )
    listener.memory = MagicMock()
    listener.session_id = "sess-1"
    listener.executor = None
    listener.log = lambda _message: None
    listener.current_latency_trace = None
    listener.clarifications = ClarificationCoordinator()
    return listener


@pytest.mark.asyncio
async def test_open_pending_asks_and_fill_completes_intent():
    prolog = FakeProlog()
    listener = build_listener(prolog)

    used_agent, reply = await listener.query_with_fallback_async("open")

    assert (used_agent, reply) == (False, "Which app?")
    session = listener.clarifications.session_for("local", "voice")
    assert session is not None
    assert session.state == "eliciting"
    prolog.executed.clear()

    used_agent, reply = await listener.query_with_fallback_async("firefox")

    assert (used_agent, reply) == (False, "Executed: open ['firefox']")
    assert prolog.executed == [("open", ["firefox"])]
    assert listener.clarifications.session_for("local", "voice").state == "closed"


@pytest.mark.asyncio
async def test_never_mind_cancels_and_late_answer_is_not_consumed():
    prolog = FakeProlog()
    listener = build_listener(prolog)
    await listener.query_with_fallback_async("open")

    used_agent, reply = await listener.query_with_fallback_async("never mind")

    assert (used_agent, reply) == (False, "Cancelled.")
    assert listener.clarifications.session_for("local", "voice").state == "closed"

    used_agent, reply = await listener.query_with_fallback_async("twenty minutes")

    assert used_agent is False
    assert reply == ClarificationCoordinator.STALE_MESSAGE
    assert prolog.executed == []
    listener.agent_manager.process_async.assert_not_awaited()


@pytest.mark.asyncio
async def test_invalid_answer_retries_same_question():
    prolog = FakeProlog()
    listener = build_listener(prolog)
    await listener.query_with_fallback_async("open")

    used_agent, reply = await listener.query_with_fallback_async(
        "the purple spotted bananas"
    )

    assert (used_agent, reply) == (False, "Which app?")
    session = listener.clarifications.session_for("local", "voice")
    assert session.state == "eliciting"
    assert session.attempts == 1

    used_agent, reply = await listener.query_with_fallback_async("gimp")
    assert (used_agent, reply) == (False, "Executed: open ['gimp']")


@pytest.mark.asyncio
async def test_text_dialogue_asks_slots_in_order():
    prolog = FakeProlog()
    listener = build_listener(prolog)

    used_agent, reply = await listener.query_with_fallback_async("text")
    assert reply == "Who should I message?"

    used_agent, reply = await listener.query_with_fallback_async("sarah")
    assert reply == "What should the message say?"

    used_agent, reply = await listener.query_with_fallback_async("hi there")
    assert (used_agent, reply) == (False, "Executed: text ['sarah', 'hi there']")
    assert prolog.executed == [("text", ["sarah", "hi there"])]


@pytest.mark.asyncio
async def test_schedule_todo_skill_executes_on_completion(monkeypatch):
    prolog = FakeProlog()
    listener = build_listener(prolog)
    skill_calls: list[tuple[str, list]] = []

    def fake_execute(skill_name, args):
        skill_calls.append((skill_name, list(args)))
        return "Task scheduled"

    monkeypatch.setattr(
        "zara.wake.python_skills.execute", fake_execute
    )

    used_agent, reply = await listener.query_with_fallback_async("schedule")
    assert reply == "What is the task?"

    used_agent, reply = await listener.query_with_fallback_async("buy milk")
    assert (used_agent, reply) == (False, "Task scheduled")
    assert skill_calls == [("schedule_todo", ["buy milk"])]


@pytest.mark.asyncio
async def test_command_like_utterance_supersedes_open_session():
    prolog = FakeProlog()
    listener = build_listener(prolog)
    await listener.query_with_fallback_async("open")

    used_agent, reply = await listener.query_with_fallback_async("open gimp")

    assert (used_agent, reply) == (False, "Executed: open ['gimp']")
    session = listener.clarifications.session_for("local", "voice")
    assert session is None or session.state == "closed"
    assert session.close_reason == "superseded_by_new_command"


@pytest.mark.asyncio
async def test_non_command_without_session_falls_to_agent():
    prolog = FakeProlog()
    listener = build_listener(prolog)

    used_agent, reply = await listener.query_with_fallback_async(
        "what is the meaning of life"
    )

    assert (used_agent, reply) == (True, "agent reply")
    listener.agent_manager.process_async.assert_awaited_once()
    assert listener.clarifications.session_for("local", "voice") is None


@pytest.mark.asyncio
async def test_unknown_pending_shape_keeps_legacy_prompt():
    prolog = FakeProlog()
    listener = build_listener(prolog)

    used_agent, reply = await listener.query_with_fallback_async("run")

    assert (used_agent, reply) == (False, "Please provide: thing.")
    assert listener.clarifications.session_for("local", "voice") is None


@pytest.mark.asyncio
async def test_execution_failure_after_completion_replies_and_closes():
    prolog = FakeProlog()
    listener = build_listener(prolog)
    prolog.execute_intent = lambda name, args: False
    await listener.query_with_fallback_async("open")

    used_agent, reply = await listener.query_with_fallback_async("firefox")

    assert used_agent is False
    assert reply == "I couldn't complete that."
    assert listener.clarifications.session_for("local", "voice").state == "closed"
    listener.agent_manager.process_async.assert_not_awaited()
