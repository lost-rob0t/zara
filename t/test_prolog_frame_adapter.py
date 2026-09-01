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
    TextValue,
)

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent

_ENGINE = None


def get_engine() -> PrologEngine:
    global _ENGINE
    if _ENGINE is not None:
        return _ENGINE
    engine = PrologEngine()
    engine.consult(REPO_ROOT / "kb" / "intents.pl")
    engine.consult(REPO_ROOT / "modules" / "intent_resolver.pl")
    engine.consult(REPO_ROOT / "modules" / "intent_frames.pl")
    _ENGINE = engine
    return _ENGINE


def test_resolve_frames_returns_typed_mirror():
    frames = get_engine().resolve_frames("open firefox")
    assert frames == [
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


def test_resolve_frames_bare_timer_missing_duration():
    frames = get_engine().resolve_frames("set a timer")
    assert frames == [
        IntentFrame(
            intent_ns="device",
            intent_name="timer.set",
            slots=(),
            status=FrameStatus.MISSING,
            missing=("duration",),
        )
    ]


def test_resolve_frames_follow_up_completes_open_frame():
    engine = get_engine()
    open_frame = IntentFrame(
        intent_ns="device",
        intent_name="timer.set",
        slots=(),
        status=FrameStatus.MISSING,
        missing=("duration",),
    )

    frames = engine.resolve_frames(
        "2 minutes", context_frame=open_frame, missing=("duration",)
    )

    assert frames == [
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


def test_resolve_frames_correction_replaces_filled_slot():
    engine = get_engine()
    open_frame = IntentFrame(
        intent_ns="device",
        intent_name="timer.set",
        slots=(
            FilledSlot(
                name="duration",
                value=DurationValue(seconds=1200),
                origin=SlotOrigin.UTTERANCE,
            ),
        ),
        status=FrameStatus.COMPLETE,
        missing=(),
    )

    frames = engine.resolve_frames(
        "actually 5 minutes", context_frame=open_frame, missing=()
    )

    assert frames == [
        IntentFrame(
            intent_ns="device",
            intent_name="timer.set",
            slots=(
                FilledSlot(
                    name="duration",
                    value=DurationValue(seconds=300),
                    origin=SlotOrigin.CORRECTION,
                ),
            ),
            status=FrameStatus.COMPLETE,
            missing=(),
        )
    ]


def test_resolve_frames_rejects_unsupported_state():
    with pytest.raises(ValueError):
        get_engine().resolve_frames("timer", state="bogus")


def test_resolve_frames_empty_and_whitespace_return_no_frames():
    engine = get_engine()
    assert engine.resolve_frames("") == []
    assert engine.resolve_frames("   ") == []


def test_resolve_frames_is_deterministic_across_runs():
    engine = get_engine()
    first = engine.resolve_frames("set a timer for 2 minutes called tea")
    second = engine.resolve_frames("set a timer for 2 minutes called tea")
    assert first == second


def test_resolve_frames_text_slot_is_bounded():
    engine = get_engine()
    oversized = "x" * 600
    frames = engine.resolve_frames(f"text alice {oversized}")
    assert frames == []


def test_legacy_intent_fixtures_map_identically():
    """Old valid intent fixtures keep resolving through the legacy path."""
    engine = get_engine()
    result = engine.resolve_intent("open firefox")
    assert result is not None
    assert result.kind == "prolog"
    assert result.name == "open"
    assert result.args == ["firefox"]


def test_resolver_never_builds_dynamic_goals_from_user_strings():
    engine = get_engine()
    injection = 'open x"), throw(injected)), ('
    frames = engine.resolve_frames(injection)
    assert len(frames) == 1
    assert frames[0].slot_value("target") == RefValue(
        kind="app_alias", id="x throwinjected"
    )
    assert engine.resolve_frames("open firefox")[0].status is FrameStatus.COMPLETE
