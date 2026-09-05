"""Current-master acceptance for bounded registered app-target recovery."""

from __future__ import annotations

import pytest

from zara import command_gate
from zara.prolog_engine import IntentResult
from zara.runtime.intent_router import PrologFirstRouter


class TargetProlog:
    def __init__(self, app_names=()):
        self.app_names = list(app_names)
        self.resolve_calls: list[str] = []
        self.executed: list[tuple[str, list]] = []
        self.vocabulary_queries: list[tuple[str, int]] = []

    def get_app_mapping(self, target: str):
        return f"mapped:{target}" if target in self.app_names else None

    def query_all(self, goal: str, max_solutions: int = -1):
        self.vocabulary_queries.append((goal, max_solutions))
        return [{"Name": name} for name in self.app_names]

    def resolve_intent(self, text: str, state: str = "passive"):
        normalized = text.strip().casefold()
        self.resolve_calls.append(normalized)
        if normalized.startswith(("open ", "launch ", "run ")):
            target = normalized.split(maxsplit=1)[1]
            if target in self.app_names:
                return IntentResult("prolog", "open", [target])
        return None

    def execute_intent(self, name: str, args) -> bool:
        values = list(args)
        self.executed.append((name, values))
        return name == "open" and values and values[0] in self.app_names


@pytest.mark.parametrize(
    ("candidate", "targets", "status", "canonical", "distance"),
    [
        ("4chan", ["4chan", "fourchan"], "exact", "4chan", 0),
        ("4cham", ["4chan", "fourchan"], "rewrite", "4chan", 1),
        ("thunderbrd", ["thunderbird", "tor"], "rewrite", "thunderbird", 1),
        ("tol", ["tor"], "no_match", None, None),
        ("astronomy", ["firefox", "4chan"], "no_match", None, None),
    ],
)
def test_registered_target_match_is_bounded(candidate, targets, status, canonical, distance):
    match = command_gate.match_registered_target(candidate, targets)
    assert match.status == status
    assert match.canonical == canonical
    assert match.distance == distance


def test_registered_target_tie_fails_closed():
    match = command_gate.match_registered_target("brav", ["brave", "bravo"])
    assert match.status == "ambiguous"
    assert match.canonical is None
    assert match.distance == 1
    assert match.alternatives == ("brave", "bravo")


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        ("open 4cham", ("open", "4cham")),
        ("please launch thunderbrd", ("launch", "thunderbrd")),
        ("run feishn now", ("run", "feishn")),
        ("open https://example.com", None),
        ("open /tmp/file", None),
        ("set a timer", None),
    ],
)
def test_open_target_candidate_stays_narrow(text, expected):
    assert command_gate.open_target_candidate(text) == expected


@pytest.mark.asyncio
async def test_open_typo_rewrites_only_to_registered_target():
    prolog = TargetProlog(["4chan", "fourchan", "firefox"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("open 4cham")

    assert decision.action == "respond"
    assert prolog.resolve_calls == ["open 4chan"]
    assert prolog.executed == [("open", ["4chan"])]
    assert len(prolog.vocabulary_queries) == 1
    goal, bound = prolog.vocabulary_queries[0]
    assert "kb_device_providers:app_mapping(Name, _)" in goal
    assert "kb_device_providers:direct_app(Name)" in goal
    assert bound == 256


@pytest.mark.asyncio
async def test_target_only_typo_uses_same_registered_vocabulary():
    prolog = TargetProlog(["thunderbird", "firefox"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("thunderbrd")

    assert decision.action == "respond"
    assert prolog.resolve_calls == ["open thunderbird"]
    assert prolog.executed == [("open", ["thunderbird"])]
    assert len(prolog.vocabulary_queries) == 1


@pytest.mark.asyncio
async def test_exact_mapping_preserves_fast_path_without_vocabulary_scan():
    prolog = TargetProlog(["4chan", "fourchan"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("open 4chan")

    assert decision.action == "respond"
    assert prolog.resolve_calls == ["open 4chan"]
    assert prolog.executed == [("open", ["4chan"])]
    assert prolog.vocabulary_queries == []


@pytest.mark.asyncio
async def test_ambiguous_target_is_not_guessed():
    prolog = TargetProlog(["brave", "bravo"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("open brav")

    assert decision.action == "delegate"
    assert prolog.resolve_calls == ["open brav"]
    assert prolog.executed == []


@pytest.mark.asyncio
async def test_unrelated_target_is_not_rewritten():
    prolog = TargetProlog(["4chan", "firefox", "thunderbird"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("open astronomy")

    assert decision.action == "delegate"
    assert prolog.resolve_calls == ["open astronomy"]
    assert prolog.executed == []


@pytest.mark.asyncio
async def test_short_target_only_remains_exact_only():
    prolog = TargetProlog(["tor"])
    router = PrologFirstRouter(prolog, wake_words=["zara"])

    decision = await router.route("tol")

    assert decision.action == "delegate"
    assert prolog.resolve_calls == []
    assert prolog.executed == []
