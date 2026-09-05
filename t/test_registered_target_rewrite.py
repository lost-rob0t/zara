"""Bounded registered-target rewrite acceptance for issue #327."""

from __future__ import annotations

import logging

import pytest

from zara.prolog_engine import IntentResult, PrologEngine
from zara.runtime.intent_router import PrologFirstRouter


class TargetProlog:
    def __init__(self, app_names=()):
        self.app_names = list(app_names)
        self.resolve_calls: list[str] = []
        self.executed: list[tuple[str, list]] = []
        self.app_name_queries = 0

    def get_app_mapping(self, target: str):
        return [target] if target in self.app_names else None

    def get_app_names(self):
        self.app_name_queries += 1
        return list(self.app_names)

    def resolve_intent(self, text: str, state: str = "passive"):
        normalized = text.strip().lower()
        self.resolve_calls.append(normalized)
        if normalized.startswith("open "):
            target = normalized.split(maxsplit=1)[1]
            if target in self.app_names:
                return IntentResult("prolog", "open", [target])
        return None

    def execute_intent(self, name: str, args) -> bool:
        self.executed.append((name, list(args)))
        return True


@pytest.mark.asyncio
async def test_open_target_typo_rewrites_only_to_registered_app():
    prolog = TargetProlog(["4chan", "fourchan", "firefox"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("open 4cham")

    assert decision.action == "respond"
    assert prolog.resolve_calls == ["open 4chan"]
    assert prolog.executed == [("open", ["4chan"])]


@pytest.mark.asyncio
async def test_target_only_typo_uses_same_registered_vocabulary():
    prolog = TargetProlog(["thunderbird", "firefox"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("thunderbrd")

    assert decision.action == "respond"
    assert prolog.resolve_calls == ["open thunderbird"]
    assert prolog.executed == [("open", ["thunderbird"])]


@pytest.mark.asyncio
async def test_exact_registered_target_is_not_semantically_changed():
    prolog = TargetProlog(["4chan", "fourchan"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("open 4chan")

    assert decision.action == "respond"
    assert prolog.resolve_calls == ["open 4chan"]
    assert prolog.executed == [("open", ["4chan"])]
    assert prolog.app_name_queries == 0


@pytest.mark.asyncio
async def test_ambiguous_registered_neighbors_do_not_guess():
    prolog = TargetProlog(["brave", "bravo"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("open brav")

    assert decision.action == "delegate"
    assert prolog.resolve_calls == ["open brav"]
    assert prolog.executed == []


@pytest.mark.asyncio
async def test_unrelated_open_target_is_not_rewritten():
    prolog = TargetProlog(["4chan", "thunderbird", "firefox"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("open astronomy")

    assert decision.action == "delegate"
    assert prolog.resolve_calls == ["open astronomy"]
    assert prolog.executed == []


@pytest.mark.asyncio
async def test_short_target_only_candidate_remains_exact_only():
    prolog = TargetProlog(["tor"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("tol")

    assert decision.action == "delegate"
    assert prolog.resolve_calls == []
    assert prolog.executed == []


def test_prolog_engine_lists_registered_app_targets_through_one_bounded_query():
    engine = PrologEngine.__new__(PrologEngine)
    engine.logger = logging.getLogger(__name__)
    seen = []

    def query_all(goal: str, max_solutions: int = 100):
        seen.append((goal, max_solutions))
        return [
            {"Name": "firefox"},
            {"Name": b"thunderbird"},
            {"Name": "firefox"},
            {"Name": "4chan"},
        ]

    engine.query_all = query_all

    assert engine.get_app_names() == ["firefox", "thunderbird", "4chan"]
    assert len(seen) == 1
    goal, bound = seen[0]
    assert "kb_device_providers:app_mapping(Name" in goal
    assert "kb_device_providers:direct_app(Name)" in goal
    assert bound == 256
