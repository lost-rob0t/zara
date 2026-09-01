"""Wake-word vocabulary resolution and fuzzy span matching.

Shared by the wake listener (client-side spotting) and the daemon intent
router (stripping the wake span from daemon-side transcripts), so both
sides agree on the configured vocabulary.
"""

from __future__ import annotations

import logging
import re
from typing import Optional, Tuple

WAKE_WORDS = ["zarathushtra", "zarathustra", "hey zara", "zara", "sarah", "sara"]
WAKE_TOKEN_STRIP = " \t\r\n,.:;!?-\"'`()[]{}<>…"


def edit_distance(left: str, right: str) -> int:
    """Levenshtein distance (insertions, deletions, substitutions)."""
    if left == right:
        return 0
    if not left:
        return len(right)
    if not right:
        return len(left)
    previous = list(range(len(right) + 1))
    for i, left_char in enumerate(left, 1):
        current = [i]
        for j, right_char in enumerate(right, 1):
            substitution_cost = 0 if left_char == right_char else 1
            current.append(
                min(
                    previous[j] + 1,
                    current[j - 1] + 1,
                    previous[j - 1] + substitution_cost,
                )
            )
        previous = current
    return previous[-1]


def wake_distance_threshold(wake_word: str) -> int:
    """Tolerated edit distance for a wake word (~25% of its length)."""
    return max(1, len(wake_word) // 4)


def _wake_tokens(text: str) -> list:
    tokens = []
    for match in re.finditer(r"\S+", text):
        raw_token = match.group()
        stripped = raw_token.strip(WAKE_TOKEN_STRIP)
        if not stripped:
            continue
        leading = len(raw_token) - len(raw_token.lstrip(WAKE_TOKEN_STRIP))
        start = match.start() + leading
        tokens.append((start, start + len(stripped), stripped.lower()))
    return tokens


def find_wake_span(text: str, wake_words) -> Optional[Tuple[int, int]]:
    """Return the best wake-word match span in ``text`` using edit distance.

    Preference: smallest edit distance, then longest phrase, then earliest
    position. Returns ``None`` when no candidate is within tolerance.
    """
    words = []
    for word in wake_words or []:
        normalized = " ".join(str(word).split()).lower()
        if normalized:
            words.append(normalized)
    if not words:
        return None
    tokens = _wake_tokens(text or "")
    if not tokens:
        return None
    max_tokens = max(len(word.split()) for word in words)
    best = None
    for phrase_length in range(1, max_tokens + 1):
        for index in range(0, len(tokens) - phrase_length + 1):
            candidate = " ".join(
                token for _, _, token in tokens[index:index + phrase_length]
            )
            for word in words:
                if edit_distance(candidate, word) <= wake_distance_threshold(word):
                    key = (edit_distance(candidate, word), -phrase_length, index)
                    if best is None or key < best[0]:
                        best = (
                            key,
                            tokens[index][0],
                            tokens[index + phrase_length - 1][1],
                        )
                    break
    if best is None:
        return None
    return best[1], best[2]


def _normalize_wake_words(raw_words) -> list:
    normalized = []
    seen = set()
    for raw in raw_words or []:
        word = " ".join(str(raw).split()).lower().strip(WAKE_TOKEN_STRIP)
        if word and word not in seen:
            seen.add(word)
            normalized.append(word)
    return normalized


def resolve_wake_words(config=None, prolog_engine=None) -> list:
    """Resolve wake words: config.toml override, Prolog facts, then defaults."""
    words: list = []
    if config is not None:
        try:
            section = config.get_section("wake") or {}
        except Exception:
            section = {}
        if isinstance(section, dict):
            raw_words = section.get("words")
            if isinstance(raw_words, str):
                raw_words = [raw_words]
            if isinstance(raw_words, (list, tuple)):
                words = _normalize_wake_words(raw_words)
    if words:
        return words

    if prolog_engine is not None:
        try:
            words = _normalize_wake_words(prolog_engine.get_wake_words())
        except Exception as error:
            logging.getLogger(__name__).warning(
                "Wake word Prolog query failed, using defaults: %s", error
            )
    if words:
        return words
    return list(WAKE_WORDS)
