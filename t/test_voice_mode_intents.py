"""Real Prolog resolution for voice mode intents (game/normal switch)."""

import pathlib

import pytest

from zara.prolog_engine import PrologEngine

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent

_ENGINE = None


def get_engine() -> PrologEngine:
    global _ENGINE
    if _ENGINE is not None:
        return _ENGINE
    engine = PrologEngine()
    engine.consult(REPO_ROOT / "modules" / "normalizer.pl")
    engine.consult(REPO_ROOT / "kb" / "intents.pl")
    engine.consult(REPO_ROOT / "modules" / "intent_resolver.pl")
    _ENGINE = engine
    return _ENGINE


@pytest.mark.parametrize(
    "utterance",
    ["game mode", "game mode please", "hey zara game mode"],
)
def test_game_mode_resolves_to_python_skill(utterance):
    result = get_engine().resolve_intent(utterance)

    assert result.kind == "python"
    assert result.name == "set_game_mode"
    assert list(result.args) == []


def test_normal_mode_resolves_to_python_skill():
    result = get_engine().resolve_intent("normal mode")

    assert result.kind == "python"
    assert result.name == "set_normal_mode"
    assert list(result.args) == []


def test_voice_mode_still_starts_dictation():
    result = get_engine().resolve_intent("voice mode")

    assert result.name == "dictation_start"


def test_stop_still_wins_over_game_mode():
    result = get_engine().resolve_intent("stop game mode")

    assert result.name == "stop"


def test_bare_game_word_does_not_resolve():
    assert get_engine().resolve_intent("game") is None
