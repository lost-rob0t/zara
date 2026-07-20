"""Heuristic gate for the Prolog-first routing path.

The voice wake loop historically tried Prolog intent resolution on every
user utterance before falling back to the LLM. For conversational turns
(questions, statements, chitchat) this added avoidable latency and let
Prolog intents (e.g. ``ask``) hijack the conversation before the LLM had
a chance to respond.

This module exposes a tiny, dependency-free heuristic: a curated set of
command trigger verbs. If the first content word of the user input is in
this set, Prolog resolution is worthwhile. Otherwise the wake loop skips
Prolog entirely and hands the turn to the LLM, which can still call the
``query_prolog`` tool to execute commands when the user's intent is in
fact a command.
"""

from __future__ import annotations

import re
from typing import FrozenSet


# Verbs that signal a command-style utterance. Sourced from
# kb/intents.pl verb_intent/3 entries that map to executable actions
# (media control, device control, dictation, navigation, search, timers,
# todo skills, conversation stop). Greetings, question words, and
# borderline-conversational verbs (e.g. ``say``) are deliberately
# excluded so they fall through to the LLM, which can still route them
# to the prolog tool when it recognizes a command.
COMMAND_TRIGGER_WORDS: FrozenSet[str] = frozenset(
    {
        "play",
        "pause",
        "resume",
        "next",
        "skip",
        "text",
        "message",
        "sms",
        "open",
        "launch",
        "run",
        "lock",
        "unlock",
        "dictate",
        "dictation",
        "voicemode",
        "micmode",
        "mic",
        "enable",
        "begin",
        "activate",
        "deactivate",
        "stopdictation",
        "stopvoice",
        "quitdictation",
        "navigate",
        "goto",
        "search",
        "find",
        "lookup",
        "todo",
        "todos",
        "task",
        "tasks",
        "add",
        "note",
        "remind",
        "reminder",
        "remember",
        "schedule",
        "sched",
        "plan",
        "set",
        "list",
        "show",
        "edit",
        "update",
        "export",
        "timer",
        "alarm",
        "weather",
        "forecast",
        "bye",
        "goodbye",
        "farewell",
        "quit",
        "start",
        "end",
        "stop",
        "command",
    }
)

_TOKEN_SPLIT = re.compile(r"[^a-z0-9_]+")


def _tokens(text: str) -> list[str]:
    if not text:
        return []
    lowered = text.casefold()
    return [tok for tok in _TOKEN_SPLIT.split(lowered) if tok]


def looks_like_command(text: str, look_words: int = 3) -> bool:
    """Return True if the first ``look_words`` tokens look like a command verb.

    Most commands start with the verb (``open firefox``, ``play music``,
    ``set a timer``). A few prepend a filler (``please open firefox``,
    ``well, please open firefox``); we check the first ``look_words``
    tokens to catch these.
    """
    tokens = _tokens(text)
    if not tokens:
        return False
    head = tokens[:look_words]
    return any(tok in COMMAND_TRIGGER_WORDS for tok in head)
