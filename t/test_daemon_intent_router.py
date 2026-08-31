"""Daemon-side deterministic Prolog-first intent router (issue #244)."""

from __future__ import annotations

import pytest

from zara.latency import LatencyTrace
from zara.prolog_engine import IntentResult
from zara.runtime.clarification import ClarificationCoordinator


class FakeProlog:
    def __init__(self) -> None:
        self.executed: list[tuple[str, list]] = []
        self.resolve_calls: list[str] = []
        self.execute_ok = True
        self.results: dict[str, IntentResult] = {
            "open": IntentResult("pending", "open", ["app"]),
            "text": IntentResult("pending", "text", ["contact", "message"]),
            "queue schedule": IntentResult(
                "pending", "python(schedule_todo)", ["task"]
            ),
            "mystery": IntentResult("pending", "mystery", ["thing"]),
            "run": IntentResult("pending", "mystery", ["thing"]),
            "schedule": IntentResult("python", "schedule_todo", ["task"]),
            "goodbye": IntentResult("prolog", "end_conversation", []),
            "open gimp": IntentResult("prolog", "open", ["gimp"]),
            "open firefox": IntentResult("prolog", "open", ["firefox"]),
        }

    def resolve_intent(self, text: str, state: str = "passive"):
        self.resolve_calls.append(text)
        return self.results.get(text.strip().lower())

    def execute_intent(self, name: str, args) -> bool:
        self.executed.append((name, list(args)))
        return self.execute_ok

    def get_app_mapping(self, target: str):
        return None


def build_router(**kwargs):
    from zara.runtime.intent_router import PrologFirstRouter

    prolog = kwargs.pop("prolog", None) or FakeProlog()
    return PrologFirstRouter(
        prolog,
        wake_words=list(kwargs.pop("wake_words", ["zara"])),
        conversation_id=kwargs.pop("conversation_id", "conv-1"),
        **kwargs,
    )


@pytest.mark.asyncio
async def test_non_command_delegates_without_prolog():
    prolog = FakeProlog()
    router = build_router(prolog=prolog)

    decision = await router.route("what is the meaning of life")

    assert decision.action == "delegate"
    assert prolog.resolve_calls == []
    assert prolog.executed == []


@pytest.mark.asyncio
async def test_command_executes_via_prolog():
    prolog = FakeProlog()
    router = build_router(prolog=prolog)

    decision = await router.route("open firefox")

    assert decision.action == "respond"
    assert decision.response == "Executed: open ['firefox']"
    assert prolog.executed == [("open", ["firefox"])]


@pytest.mark.asyncio
async def test_unresolved_command_delegates():
    router = build_router()

    decision = await router.route("play something noisy")

    assert decision.action == "delegate"


@pytest.mark.asyncio
async def test_end_conversation_ends_session():
    router = build_router()

    decision = await router.route("goodbye")

    assert decision.action == "end_conversation"
    assert decision.response


@pytest.mark.asyncio
async def test_ask_intent_delegates():
    class AskProlog(FakeProlog):
        def resolve_intent(self, text: str, state: str = "passive"):
            self.resolve_calls.append(text)
            return IntentResult("prolog", "ask", [])

    router = build_router(prolog=AskProlog())

    decision = await router.route("open firefox")

    assert decision.action == "delegate"


@pytest.mark.asyncio
async def test_python_skill_intent_executes_skill(monkeypatch):
    calls: list[tuple[str, list]] = []

    def fake_execute(name, args):
        calls.append((name, list(args)))
        return "scheduled"

    from zara.runtime import intent_router

    monkeypatch.setattr(intent_router.python_skills, "execute", fake_execute)
    router = build_router()

    decision = await router.route("schedule")

    assert calls == [("schedule_todo", ["task"])]
    assert decision.action == "respond"
    assert decision.response == "scheduled"


@pytest.mark.asyncio
async def test_pending_with_template_opens_clarification():
    router = build_router()

    decision = await router.route("open")

    assert decision.action == "respond"
    assert decision.response == "Which app?"


@pytest.mark.asyncio
async def test_pending_without_template_lists_missing_slots():
    router = build_router()

    decision = await router.route("run")

    assert decision.action == "respond"
    assert decision.response == "Please provide: thing."


@pytest.mark.asyncio
async def test_clarification_follow_up_completes_intent():
    prolog = FakeProlog()
    router = build_router(prolog=prolog)

    await router.route("open")
    decision = await router.route("firefox")

    assert decision.action == "respond"
    assert decision.response == "Executed: open ['firefox']"
    assert prolog.executed == [("open", ["firefox"])]


@pytest.mark.asyncio
async def test_command_supersedes_pending_session():
    prolog = FakeProlog()
    router = build_router(prolog=prolog)

    await router.route("open")
    decision = await router.route("open firefox")

    assert decision.action == "respond"
    assert prolog.executed == [("open", ["firefox"])]
    session = router.clarifications.session_for("local", "conv-1")
    assert session is None or session.state == "closed"


@pytest.mark.asyncio
async def test_stale_follow_up_after_cancel():
    router = build_router()

    await router.route("open")
    cancelled = await router.route("never mind")
    assert cancelled.response == "Cancelled."

    stale = await router.route("twenty minutes")

    assert stale.action == "respond"
    assert stale.response == ClarificationCoordinator.STALE_MESSAGE


@pytest.mark.asyncio
async def test_wake_word_prefix_is_stripped_before_routing():
    prolog = FakeProlog()
    router = build_router(prolog=prolog)

    decision = await router.route("zara open firefox")

    assert decision.action == "respond"
    assert decision.response == "Executed: open ['firefox']"


@pytest.mark.asyncio
async def test_wake_word_only_utterance_greets():
    router = build_router()

    decision = await router.route("zara")

    assert decision.action == "greeting"
    assert decision.response


@pytest.mark.asyncio
async def test_target_only_candidate_recovers_app_mapping():
    class MappingProlog(FakeProlog):
        def get_app_mapping(self, target: str):
            return "firefox" if target == "firefox" else None

    router = build_router(prolog=MappingProlog())

    decision = await router.route("firefox")

    assert decision.action == "respond"
    assert decision.response == "Executed: open ['firefox']"


@pytest.mark.asyncio
async def test_execution_failure_delegates_to_agent():
    prolog = FakeProlog()
    prolog.execute_ok = False
    router = build_router(prolog=prolog)

    decision = await router.route("open firefox")

    assert decision.action == "delegate"
    assert prolog.executed == [("open", ["firefox"])]


@pytest.mark.asyncio
async def test_latency_records_share_router_trace_id():
    trace = LatencyTrace(trace_id="trace-router-1")
    router = build_router()

    await router.route("open firefox", latency_trace=trace)

    recorded = {event.event for event in trace.events}
    assert {"prolog_result", "route_selected"} <= recorded
    assert all(event.trace_id == "trace-router-1" for event in trace.events)


@pytest.mark.asyncio
async def test_clarification_sessions_are_scoped_per_conversation():
    router = build_router()

    await router.route("open")

    assert router.clarifications.session_for("local", "conv-1") is not None
    assert router.clarifications.session_for("local", "conv-2") is None
