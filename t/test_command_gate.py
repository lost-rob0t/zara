"""Tests for the command-verb heuristic gate (zara.command_gate)."""

import pytest

from zara.command_gate import (
    looks_like_command,
    match_registered_target,
    open_target_candidate,
    target_only_candidate,
)


@pytest.mark.parametrize(
    "text",
    [
        "open firefox",
        "OPEN firefox",
        "Open Firefox",
        "play some music",
        "pause",
        "stop",
        "set a timer for 5 minutes called pasta",
        "set 10 minute timer",
        "remind me to call mom at 5pm",
        "remember the wifi password is hunter2",
        "add buy milk to my todo list",
        "search todos milk",
        "find todo groceries",
        "navigate home",
        "launch spotify",
        "run top",
        "lock the screen",
        "goodbye",
        "end conversation",
        "start voice mode",
        "dictate",
        "timer 5 minutes",
        "alarm 7am",
        "weather",
        "forecast for tomorrow",
        "please open firefox",
        "yo open brave",
        "well please open firefox",
        "schedule a meeting tomorrow at 3pm",
        "list todos",
        "show my tasks",
        "edit the third todo",
        "export todos",
        "text mom i will be late",
        "message alice on my way",
    ],
)
def test_looks_like_command_true_for_commands(text):
    assert looks_like_command(text) is True


@pytest.mark.parametrize(
    "text",
    [
        "",
        "hello there",
        "hi zara",
        "hey",
        "good morning",
        "why is the sky blue",
        "what does quantum mechanics mean",
        "how do birds fly",
        "when was rome founded",
        "where is the nearest coffee shop",
        "who wrote thus spoke zarathustra",
        "tell me about stoicism",
        "explain the trolley problem",
        "you are much more responsive now",
        "this is a test of the system",
        "i think therefore i am",
        "the unexamined life is not worth living",
        "what time is it",
        "what's the meaning of life",
        "is there free will",
        "actually that is interesting",
        "yeah it does not look like any audio is being played",
        "thanks",
        "thank you",
        "cool",
        "nice",
        "okay",
        "right",
        "yes",
        "no",
        "maybe",
        "i disagree",
        "tell me a joke",
        "explain again",
        "can you help me",
        "would you say that is fair",
    ],
)
def test_looks_like_command_false_for_conversation(text):
    assert looks_like_command(text) is False


def test_looks_like_command_handles_punctuation():
    assert looks_like_command("Open, firefox") is True
    assert looks_like_command("open. firefox") is True
    assert looks_like_command("open! firefox") is True
    assert looks_like_command("open: firefox") is True
    assert looks_like_command("open  firefox") is True
    assert looks_like_command("open\tfirefox") is True
    assert looks_like_command("open-firefox") is True


def test_looks_like_command_look_words_window():
    assert looks_like_command("please open firefox") is True
    assert looks_like_command("well please open firefox") is True
    assert looks_like_command("yo please open firefox") is True
    assert looks_like_command("a b c d open firefox") is False


def test_looks_like_command_empty_and_whitespace():
    assert looks_like_command("") is False
    assert looks_like_command("   ") is False
    assert looks_like_command(",,,") is False
    assert looks_like_command("\t\n") is False


def test_looks_like_command_case_insensitive():
    assert looks_like_command("OPEN FIREFOX") is True
    assert looks_like_command("Open Firefox") is True
    assert looks_like_command("oPeN fIrEfOx") is True


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        ("YouTube.", "youtube"),
        ("Google Chrome", "google_chrome"),
        ("open YouTube", None),
        ("why YouTube is useful", None),
        ("", None),
    ],
)
def test_target_only_candidate(text, expected):
    assert target_only_candidate(text) == expected


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        ("open 4cham", ("open", "4cham")),
        ("please launch thunderbrd", ("launch", "thunderbrd")),
        ("run feishn now", ("run", "feishn")),
        ("set a timer", None),
        ("open https://example.com", None),
        ("open /tmp/file", None),
        ("open", None),
    ],
)
def test_open_target_candidate_is_narrow(text, expected):
    assert open_target_candidate(text) == expected


def test_registered_target_match_exact_wins():
    match = match_registered_target("4chan", ["fourchan", "4chan", "chan"])

    assert match.status == "exact"
    assert match.canonical == "4chan"
    assert match.distance == 0
    assert match.alternatives == ()


@pytest.mark.parametrize(
    ("candidate", "targets", "expected", "distance"),
    [
        ("4cham", ["4chan", "fourchan"], "4chan", 1),
        ("thunderbrd", ["thunderbird", "tor"], "thunderbird", 1),
        ("feishn", ["feishin", "firefox"], "feishin", 1),
    ],
)
def test_registered_target_match_unique_bounded_rewrite(
    candidate, targets, expected, distance
):
    match = match_registered_target(candidate, targets)

    assert match.status == "rewrite"
    assert match.canonical == expected
    assert match.distance == distance
    assert match.alternatives == ()


def test_registered_target_match_short_tokens_are_exact_only():
    match = match_registered_target("tol", ["tor"])

    assert match.status == "no_match"
    assert match.canonical is None


def test_registered_target_match_tie_is_ambiguous_not_guessed():
    match = match_registered_target("brav", ["brave", "bravo"])

    assert match.status == "ambiguous"
    assert match.canonical is None
    assert match.distance == 1
    assert match.alternatives == ("brave", "bravo")


def test_registered_target_match_unrelated_text_is_no_match():
    match = match_registered_target("astronomy", ["firefox", "thunderbird", "4chan"])

    assert match.status == "no_match"
    assert match.canonical is None
    assert match.alternatives == ()
