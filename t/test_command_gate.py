"""Tests for the command-verb heuristic gate (zara.command_gate)."""

import pytest

from zara.command_gate import looks_like_command


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
