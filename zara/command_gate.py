"""Heuristic gate for the Prolog-first routing path.

The voice wake loop historically tried Prolog intent resolution on every
user utterance before falling back to the LLM. For conversational turns
(questions, statements, chitchat) this added avoidable latency and let
Prolog intents (e.g. ``ask``) hijack the conversation before the LLM had
a chance to respond.

This module exposes dependency-free routing signals: a curated set of command
trigger verbs, conservative target extractors, and a bounded matcher for
already-registered app targets. The matcher is discovery-only: it never owns
or executes a capability, and ambiguous or unsafe candidates fail closed.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import FrozenSet, Iterable, Optional

from .wake_words import edit_distance


COMMAND_TRIGGER_WORDS: FrozenSet[str] = frozenset(
    {
        "play", "pause", "resume", "next", "skip", "text", "message", "sms",
        "open", "launch", "run", "lock", "unlock", "dictate", "dictation",
        "voicemode", "micmode", "mic", "enable", "begin", "activate",
        "deactivate", "stopdictation", "stopvoice", "quitdictation", "navigate",
        "goto", "search", "find", "lookup", "todo", "todos", "task", "tasks",
        "add", "note", "remind", "reminder", "remember", "schedule", "sched",
        "plan", "set", "list", "show", "edit", "update", "export", "timer",
        "alarm", "weather", "forecast", "bye", "goodbye", "farewell", "quit",
        "start", "end", "stop", "command",
    }
)

# Keep fuzzy recovery narrower than the general command vocabulary. ``start``
# overlaps non-app commands such as ``start voice mode`` and is deliberately
# excluded until a broader typed rewrite stage exists.
OPEN_TARGET_VERBS: FrozenSet[str] = frozenset({"open", "launch", "run"})

_TOKEN_SPLIT = re.compile(r"[^a-z0-9_]+")
_SAFE_TARGET = re.compile(r"^[a-z0-9_]+$")
_MAX_REGISTERED_TARGETS = 256


@dataclass(frozen=True)
class RegisteredTargetMatch:
    """Result of bounded matching against an already-registered vocabulary."""

    status: str
    canonical: Optional[str] = None
    distance: Optional[int] = None
    alternatives: tuple[str, ...] = ()


def _tokens(text: str) -> list[str]:
    if not text:
        return []
    lowered = text.casefold()
    return [tok for tok in _TOKEN_SPLIT.split(lowered) if tok]


def looks_like_command(text: str, look_words: int = 3) -> bool:
    """Return True if the first ``look_words`` tokens look like a command verb."""
    tokens = _tokens(text)
    if not tokens:
        return False
    return any(tok in COMMAND_TRIGGER_WORDS for tok in tokens[:look_words])


def target_only_candidate(text: str, max_words: int = 2) -> Optional[str]:
    """Return a normalized short target that may have lost its command verb."""
    tokens = _tokens(text)
    if not tokens or len(tokens) > max_words:
        return None
    if any(token in COMMAND_TRIGGER_WORDS for token in tokens):
        return None
    return "_".join(tokens)


def open_target_candidate(text: str, *, look_words: int = 3) -> Optional[tuple[str, str]]:
    """Extract one safe app target following an explicit open-style verb.

    URI/path-shaped text is deliberately excluded. This mirrors the narrow
    one-target Prolog open-app boundary rather than doing general fuzzy text.
    """
    raw = text or ""
    if "://" in raw or "/" in raw or "\\" in raw:
        return None
    tokens = _tokens(raw)
    if len(tokens) < 2:
        return None
    for index, token in enumerate(tokens[:look_words]):
        if token not in OPEN_TARGET_VERBS:
            continue
        target_index = index + 1
        if target_index >= len(tokens):
            return None
        target = tokens[target_index]
        if not _SAFE_TARGET.fullmatch(target):
            return None
        return token, target
    return None


def _normalized_targets(registered_targets: Iterable[str]) -> tuple[str, ...]:
    targets: list[str] = []
    seen: set[str] = set()
    for raw in registered_targets:
        if len(targets) >= _MAX_REGISTERED_TARGETS:
            break
        if not isinstance(raw, str):
            continue
        target = raw.strip().casefold()
        if not target or not _SAFE_TARGET.fullmatch(target) or target in seen:
            continue
        seen.add(target)
        targets.append(target)
    return tuple(targets)


def match_registered_target(
    candidate: str,
    registered_targets: Iterable[str],
) -> RegisteredTargetMatch:
    """Match a target only against explicitly registered app names.

    Exact matches win. Fuzzy recovery is disabled below four characters,
    limited to one edit for 4-7 character names and two edits for longer
    names, and succeeds only for a unique closest candidate.
    """
    if not isinstance(candidate, str):
        return RegisteredTargetMatch("no_match")
    normalized = candidate.strip().casefold()
    if not normalized or not _SAFE_TARGET.fullmatch(normalized):
        return RegisteredTargetMatch("no_match")

    targets = _normalized_targets(registered_targets)
    if normalized in targets:
        return RegisteredTargetMatch("exact", normalized, 0)
    if len(normalized) < 4:
        return RegisteredTargetMatch("no_match")

    matches: list[tuple[int, str]] = []
    for target in targets:
        if len(target) < 4:
            continue
        threshold = 1 if max(len(normalized), len(target)) <= 7 else 2
        if abs(len(normalized) - len(target)) > threshold:
            continue
        distance = edit_distance(normalized, target)
        if distance <= threshold:
            matches.append((distance, target))

    if not matches:
        return RegisteredTargetMatch("no_match")

    best_distance = min(distance for distance, _ in matches)
    best = tuple(sorted(target for distance, target in matches if distance == best_distance))
    if len(best) != 1:
        return RegisteredTargetMatch(
            "ambiguous",
            distance=best_distance,
            alternatives=best,
        )
    return RegisteredTargetMatch("rewrite", best[0], best_distance)
