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


def test_resolve_wake_words_falls_back_to_explicit_prolog_facts():
    config = _config_with_wake_section({"words": []})
    prolog = MagicMock()
    prolog.get_wake_words.return_value = ["zarathushtra", "zara"]
    assert resolve_wake_words(config, prolog) == ["zarathushtra", "zara"]


def test_resolve_wake_words_derives_defaults_from_project_name_override():
    config = _config_with_wake_section({})
    prolog = MagicMock()
    prolog.get_wake_words.return_value = []
    # config_loader installs local/base overrides with asserta/2, so the
    # effective value is the first clause and the packaged default follows it.
    prolog.query_all.return_value = [{"Name": "Mara"}, {"Name": "Zara"}]
    assert resolve_wake_words(config, prolog) == ["hey mara", "mara"]
    prolog.query_all.assert_called_with(
        "kb_config:project_name(Name)",
        max_solutions=64,
    )


def test_resolve_wake_words_preserves_zara_legacy_aliases():
    config = _config_with_wake_section({})
    prolog = MagicMock()
    prolog.get_wake_words.return_value = []
    prolog.query_all.return_value = [{"Name": "Zara"}]
    assert resolve_wake_words(config, prolog) == list(WAKE_WORDS)


def test_resolve_wake_words_defaults_when_prolog_unavailable():
    config = _config_with_wake_section({})
    assert resolve_wake_words(config, None) == list(WAKE_WORDS)


def test_resolve_wake_words_defaults_when_prolog_fails():
    config = _config_with_wake_section({})
    prolog = MagicMock()
    prolog.get_wake_words.side_effect = RuntimeError("prolog exploded")
    prolog.query_all.side_effect = RuntimeError("prolog exploded")
    assert resolve_wake_words(config, prolog) == list(WAKE_WORDS)


def test_resolve_wake_words_drops_invalid_config_entries():
    config = _config_with_wake_section({"words": ["", "   ", "Hey Zara"]})
    assert resolve_wake_words(config, None) == ["hey zara"]


def test_prolog_engine_get_wake_words_uses_canonical_module_owner():
    pytest.importorskip("pyswip")
    from zara.prolog_engine import PrologEngine, locate_main_pl

    # PySWIP owns one process-wide SWI runtime. A temporary file must not
    # re-declare kb_config after another integration test has loaded Zara's
    # canonical module: SWI rejects that ownership collision and its Python
    # error path is not safe to recover from. Exercise the real module owner.
    config_path = locate_main_pl().parent / "kb" / "config.pl"
    engine = PrologEngine(config_path)

    # Packaged config intentionally has no explicit wake_word/1 facts; normal
    # defaults are derived from project_name/1, while override behavior has
    # dedicated config-loader coverage.
    assert engine.get_wake_words() == []
