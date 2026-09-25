from __future__ import annotations

from types import SimpleNamespace

import pytest

from zara.prolog_engine import IntentResult
from zara.runtime import intent_router
from zara.runtime.clarification import (
    DialogueTemplate,
    SlotSpec,
    SlotType,
)
from zara.runtime.frames import (
    BoolValue,
    DateTimeValue,
    DurationValue,
    NumberValue,
    RefValue,
    TextValue,
)
from zara.runtime.intent_router import PrologFirstRouter


class Prolog:
    def __init__(self) -> None:
        self.result = None
        self.execute_result = True
        self.rows = []
        self.mapping = None

    def resolve_intent(self, text, state="passive"):
        if isinstance(self.result, BaseException):
            raise self.result
        return self.result

    def execute_intent(self, name, args):
        if isinstance(self.execute_result, BaseException):
            raise self.execute_result
        return self.execute_result

    def get_app_mapping(self, target):
        if isinstance(self.mapping, BaseException):
            raise self.mapping
        return self.mapping

    def query_all(self, query, max_solutions):
        if isinstance(self.rows, BaseException):
            raise self.rows
        return self.rows


def router(prolog):
    return PrologFirstRouter(prolog, wake_words=["zara"], conversation_id="conv")


@pytest.mark.asyncio
async def test_resolution_python_and_execution_exceptions_delegate_without_effects(monkeypatch):
    p = Prolog()
    p.result = RuntimeError("resolve")
    assert (await router(p).route("open firefox")).action == "delegate"

    p.result = IntentResult("python", "schedule_todo", ["task"])
    monkeypatch.setattr(
        intent_router.python_skills,
        "execute",
        lambda *_args: (_ for _ in ()).throw(RuntimeError("skill")),
    )
    assert (await router(p).route("schedule")).action == "delegate"

    p.result = IntentResult("prolog", "open", ["firefox"])
    p.execute_result = RuntimeError("execute")
    assert (await router(p).route("open firefox")).action == "delegate"


@pytest.mark.asyncio
async def test_registered_target_discovery_contains_lookup_and_malformed_rows():
    p = Prolog()
    p.mapping = RuntimeError("mapping")
    p.rows = RuntimeError("query")
    match = await router(p)._registered_target_match("firefox")
    assert match.status == "no_match"

    p.mapping = None
    p.rows = [
        None,
        {"Name": b"firefox"},
        {"Name": b"\xff"},
        {"Name": SimpleNamespace(value="chrome")},
        {"Name": SimpleNamespace(value=1)},
        {"Other": "ignored"},
    ]
    match = await router(p)._registered_target_match("firefox")
    assert match.status in {"exact", "rewrite"}

    p.mapping = "mapped"
    exact = await router(p)._registered_target_match("FiReFoX")
    assert exact.status == "exact"
    assert exact.canonical == "firefox"


@pytest.mark.asyncio
async def test_target_only_recovery_rejects_non_target_and_no_match():
    p = Prolog()
    r = router(p)
    assert await r._recover_target_only("this is a whole sentence") is None
    p.rows = []
    assert await r._recover_target_only("firefox") is None


@pytest.mark.asyncio
async def test_clarification_projection_covers_all_typed_slot_values(monkeypatch):
    p = Prolog()
    r = router(p)
    template = DialogueTemplate(
        intent_ns="skill",
        intent_name="edge_skill",
        specs=(
            SlotSpec("text", SlotType.TEXT),
            SlotSpec("ref", SlotType.REF),
            SlotSpec("duration", SlotType.DURATION),
            SlotSpec("number", SlotType.NUMBER),
            SlotSpec("boolean", SlotType.BOOLEAN),
            SlotSpec("datetime", SlotType.DATETIME),
            SlotSpec("optional", SlotType.TEXT, required=False),
        ),
    )
    opened = r.clarifications.open(
        template,
        principal="local",
        conversation_id="conv",
        prefilled={
            "text": TextValue("hello"),
            "ref": RefValue("contact", "alice"),
            "duration": DurationValue(5),
            "number": NumberValue(2.5),
            "boolean": BoolValue(True),
            "datetime": DateTimeValue(2026, 9, 25, 4, 26, 6),
        },
    )
    outcome = SimpleNamespace(session=opened.session, frame=opened.session.frame)
    calls = []

    def skill(name, args):
        calls.append((name, args))
        return "ok"

    monkeypatch.setattr(intent_router.python_skills, "execute", skill)
    assert await r._execute_clarification(outcome, "edge", "conv") == "ok"
    assert calls == [
        (
            "edge_skill",
            [
                "hello",
                "alice",
                5,
                2.5,
                True,
                (2026, 9, 25, 4, 26, 6),
            ],
        )
    ]


@pytest.mark.asyncio
async def test_skill_pending_flow_executes_after_clarification(monkeypatch):
    p = Prolog()
    p.result = IntentResult("pending", "python(schedule_todo)", ["task"])
    r = router(p)
    monkeypatch.setattr(intent_router.python_skills, "execute", lambda name, args: f"{name}:{args[0]}")

    opened = await r.route("queue schedule")
    assert opened.action == "respond"
    completed = await r.route("buy milk")
    assert completed.action == "respond"
    assert completed.response == "schedule_todo:buy milk"


@pytest.mark.asyncio
async def test_clarification_prolog_failure_closes_session():
    p = Prolog()
    r = router(p)
    p.result = IntentResult("pending", "open", ["app"])
    await r.route("open")
    p.execute_result = False
    failed = await r.route("firefox")
    assert failed.response == intent_router.CLARIFICATION_FAILED_RESPONSE
