from __future__ import annotations

import pathlib

import pytest

from zara.prolog_engine import PrologEngine
from zara.runtime.frames import (
    DurationValue,
    FilledSlot,
    FrameStatus,
    IntentFrame,
    RefValue,
    SlotOrigin,
)

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent

_ENGINE = None


def get_engine() -> PrologEngine:
    global _ENGINE
    if _ENGINE is not None:
        return _ENGINE
    engine = PrologEngine()
    engine.consult(REPO_ROOT / "kb" / "intents.pl")
    engine.consult(REPO_ROOT / "kb" / "semantic_corpus.pl")
    engine.consult(REPO_ROOT / "modules" / "intent_frames.pl")
    _ENGINE = engine
    return _ENGINE


def corpus_cases():
    engine = get_engine()
    rows = engine.query_all(
        "corpus_case(Id, Utterance, State, Context, ExpectedFrames, Tags)",
        max_solutions=200,
    )
    cases = []
    for row in rows:
        cases.append(
            (
                str(row["Id"]),
                str(row["Utterance"]),
                str(row["State"]),
                [str(tag) for tag in row["Tags"]],
            )
        )
    assert cases, "semantic corpus must not be empty"
    return cases


CASE_IDS = [case[0] for case in corpus_cases()]


def resolve_case(case_id: str) -> list[IntentFrame]:
    engine = get_engine()
    goal = (
        f"corpus_case('{case_id}', U, S, C, _, _), "
        "intent_frames:resolve_frames(U, S, C, Frames)"
    )
    return engine.frames_from_goal(goal)


def expected_case(case_id: str) -> list[IntentFrame]:
    engine = get_engine()
    goal = f"corpus_case('{case_id}', _, _, _, Frames, _)"
    return engine.frames_from_goal(goal)


@pytest.mark.parametrize("case_id", CASE_IDS)
def test_corpus_case(case_id):
    frames = resolve_case(case_id)
    expected = expected_case(case_id)

    assert frames == expected


@pytest.mark.parametrize("case_id", CASE_IDS)
def test_corpus_case_is_deterministic(case_id):
    first = resolve_case(case_id)
    second = resolve_case(case_id)
    assert first == second


def test_corpus_has_no_duplicate_ids():
    assert len(CASE_IDS) == len(set(CASE_IDS))


def test_corpus_covers_issue_minimum_examples():
    joined = {tag for case in corpus_cases() for tag in case[3]}
    for required in (
        "timer",
        "open",
        "text",
        "search",
        "skill",
        "pending",
        "correction",
        "cancel",
        "precedence",
        "boundary",
        "empty",
        "unicode",
        "punctuation",
        "injection",
        "dialogue",
        "ambiguous",
    ):
        assert required in joined, f"corpus missing required tag {required!r}"


def test_open_complete_decodes_to_typed_mirror():
    decoded = expected_case("open_complete")
    assert decoded == [
        IntentFrame(
            intent_ns="app",
            intent_name="open",
            slots=(
                FilledSlot(
                    name="target",
                    value=RefValue(kind="app_alias", id="firefox"),
                    origin=SlotOrigin.UTTERANCE,
                ),
            ),
            status=FrameStatus.COMPLETE,
            missing=(),
        )
    ]


def test_timer_bare_decodes_missing():
    decoded = expected_case("timer_bare_verb")
    assert decoded == [
        IntentFrame(
            intent_ns="device",
            intent_name="timer.set",
            slots=(),
            status=FrameStatus.MISSING,
            missing=("duration",),
        )
    ]


def test_followup_duration_decodes_follow_up_origin():
    decoded = expected_case("timer_followup")
    assert decoded == [
        IntentFrame(
            intent_ns="device",
            intent_name="timer.set",
            slots=(
                FilledSlot(
                    name="duration",
                    value=DurationValue(seconds=120),
                    origin=SlotOrigin.FOLLOW_UP,
                ),
            ),
            status=FrameStatus.COMPLETE,
            missing=(),
        )
    ]


def test_resolved_frames_match_typed_mirrors():
    frames = resolve_case("open_complete")
    assert frames[0].intent_ns == "app"
    assert frames[0].slots[0].value == RefValue(kind="app_alias", id="firefox")
