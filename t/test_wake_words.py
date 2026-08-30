"""Wake word resolution and edit-distance matching tests."""

from unittest.mock import MagicMock

import pytest

from zara.wake import (
    WAKE_WORDS,
    WakeWordListener,
    edit_distance,
    find_wake_span,
    resolve_wake_words,
    wake_distance_threshold,
)


def test_default_wake_words_include_zarathushtra():
    assert "zarathushtra" in WAKE_WORDS
    assert "zara" in WAKE_WORDS


def test_edit_distance_is_levenshtein():
    assert edit_distance("kitten", "sitting") == 3
    assert edit_distance("zara", "zara") == 0
    assert edit_distance("flaw", "lawn") == 2
    assert edit_distance("", "abc") == 3
    assert edit_distance("abc", "") == 3


def test_wake_distance_threshold_scales_with_length():
    assert wake_distance_threshold("zara") == 1
    assert wake_distance_threshold("hey zara") == 2
    assert wake_distance_threshold("zarathushtra") == 3


def test_find_wake_span_matches_exact_phrase_case_insensitive():
    text = "Hey Zara open Firefox"
    span = find_wake_span(text, WAKE_WORDS)
    assert span is not None
    assert text[span[0]:span[1]].lower() == "hey zara"


def test_find_wake_span_prefers_longest_exact_phrase():
    text = "hey zara what is the weather"
    span = find_wake_span(text, WAKE_WORDS)
    assert span is not None
    assert text[span[0]:span[1]].lower() == "hey zara"


def test_find_wake_span_accepts_close_transcription_variant():
    text = "Zaratustra open Firefox"
    span = find_wake_span(text, WAKE_WORDS)
    assert span is not None
    assert text[span[0]:span[1]].lower() == "zaratustra"


def test_find_wake_span_rejects_unrelated_text():
    assert find_wake_span("open Firefox", WAKE_WORDS) is None
    assert find_wake_span("Sarabande open Firefox", WAKE_WORDS) is None
    assert find_wake_span("", WAKE_WORDS) is None
    assert find_wake_span("Zara", []) is None


def test_find_wake_span_ignores_surrounding_punctuation():
    text = "Zara, open Firefox"
    span = find_wake_span(text, WAKE_WORDS)
    assert span is not None
    assert text[span[0]:span[1]] == "Zara"


def test_wake_command_keeps_known_variants():
    listener = WakeWordListener.__new__(WakeWordListener)
    assert listener._wake_command("Zara") == ""
    assert listener._wake_command("Hey Zara") == ""
    assert listener._wake_command("Sara") == ""
    assert listener._wake_command("Sara open Firefox") == "open Firefox"


def test_wake_command_accepts_zarathushtra_and_fuzzy_variants():
    listener = WakeWordListener.__new__(WakeWordListener)
    assert listener._wake_command("Zarathushtra") == ""
    assert listener._wake_command("Zarathushtra open Firefox") == "open Firefox"
    assert listener._wake_command("Zaratustra open Firefox") == "open Firefox"
    assert listener._wake_command("please Zara open Firefox") == "please open Firefox"


def test_wake_command_returns_none_without_wake_word():
    listener = WakeWordListener.__new__(WakeWordListener)
    assert listener._wake_command("open Firefox") is None
    assert listener._wake_command("") is None
    assert listener._wake_command(None) is None


def test_wake_command_uses_instance_wake_words_when_set():
    listener = WakeWordListener.__new__(WakeWordListener)
    listener.wake_words = ["jarvis"]
    assert listener._wake_command("Jarvis open the pod bay doors") == (
        "open the pod bay doors"
    )
    assert listener._wake_command("Zara open the pod bay doors") is None


def _config_with_wake_section(section):
    config = MagicMock()
    config.get_section.return_value = section
    return config


def test_resolve_wake_words_prefers_config_override():
    config = _config_with_wake_section({"words": ["Jarvis", "zara", "Jarvis"]})
    assert resolve_wake_words(config, None) == ["jarvis", "zara"]


def test_resolve_wake_words_config_single_string_is_accepted():
    config = _config_with_wake_section({"words": "Computer"})
    assert resolve_wake_words(config, None) == ["computer"]


def test_resolve_wake_words_falls_back_to_prolog_facts():
    config = _config_with_wake_section({"words": []})
    prolog = MagicMock()
    prolog.get_wake_words.return_value = ["zarathushtra", "zara"]
    assert resolve_wake_words(config, prolog) == ["zarathushtra", "zara"]


def test_resolve_wake_words_defaults_when_prolog_unavailable():
    config = _config_with_wake_section({})
    assert resolve_wake_words(config, None) == list(WAKE_WORDS)


def test_resolve_wake_words_defaults_when_prolog_fails():
    config = _config_with_wake_section({})
    prolog = MagicMock()
    prolog.get_wake_words.side_effect = RuntimeError("prolog exploded")
    assert resolve_wake_words(config, prolog) == list(WAKE_WORDS)


def test_resolve_wake_words_drops_invalid_config_entries():
    config = _config_with_wake_section({"words": ["", "   ", "Hey Zara"]})
    assert resolve_wake_words(config, None) == ["hey zara"]


def test_prolog_engine_get_wake_words(tmp_path):
    pytest.importorskip("pyswip")
    from zara.prolog_engine import PrologEngine

    fixture = tmp_path / "kb.pl"
    fixture.write_text(
        ":- module(kb_config, [wake_word/1]).\n"
        ":- dynamic wake_word/1.\n"
        'wake_word("jarvis").\n'
        "wake_word(zara).\n",
        encoding="utf-8",
    )
    engine = PrologEngine(fixture)
    assert engine.get_wake_words() == ["jarvis", "zara"]
