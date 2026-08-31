"""Pure sentence/phrase chunker for streaming LLM output (ZARA-027).

Converts a live text stream into speakable sentence-sized chunks. This module
is pure logic: no provider, runtime, or TTS dependencies. Every streaming
producer shares this one implementation.

Guarantees:
- ``"".join(feed outputs) + "".join(final flush)`` equals the exact fed text;
  no character is duplicated or lost at chunk boundaries.
- Emissions are bounded: ``max_chars`` forces a flush at the last word
  boundary, ``max_wait_ms`` forces a flush of stale pending text.

A sentence break is a terminator (``. ! ? … 。 ！ ？``) followed by
whitespace (consumed) or a CJK character (not consumed), guarded against
abbreviations, single-letter initials, decimals, URLs, and backtick code
spans/fenced blocks. A terminator at the very end of the pending buffer is
held back until the next feed reveals what follows it.
"""

from __future__ import annotations

import time
from typing import Callable, List, Optional

TERMINATORS = {".", "!", "?", "…", "。", "！", "？"}
ABBREVIATIONS = {
    "dr",
    "mr",
    "mrs",
    "ms",
    "prof",
    "sr",
    "jr",
    "st",
    "vs",
    "eg",
    "ie",
    "etc",
    "e.g",
    "i.e",
    "ph.d",
    "u.s",
    "inc",
    "ltd",
}
_URL_PREFIXES = ("http://", "https://", "www.")


def _is_cjk_break(char: str) -> bool:
    code = ord(char)
    return (
        0x2E80 <= code <= 0x9FFF
        or 0xAC00 <= code <= 0xD7AF
        or 0xF900 <= code <= 0xFAFF
        or 0xFF00 <= code <= 0xFFEF
    )


class SentenceChunker:
    """Accumulates streamed text and emits sentence-sized chunks."""

    def __init__(
        self,
        *,
        max_chars: int = 180,
        max_wait_ms: float = 500.0,
        clock: Callable[[], float] = time.monotonic,
    ):
        if max_chars < 1:
            raise ValueError("max_chars must be positive")
        if max_wait_ms <= 0:
            raise ValueError("max_wait_ms must be positive")
        self.max_chars = max_chars
        self.max_wait_ms = max_wait_ms
        self._clock = clock
        self._pending = ""
        self._pending_started: Optional[float] = None

    def feed(self, text: str) -> List[str]:
        """Consume a text increment and return completed sentence chunks."""
        if not text:
            return []

        due = self._take_due() if self._pending else []
        self._pending += text
        if self._pending_started is None:
            self._pending_started = self._clock()
        return due + self._emit_ready()

    def flush(self) -> List[str]:
        """Emit everything still pending (end of stream)."""
        return self._drain()

    def take_due(self) -> List[str]:
        """Emit pending text if the max-wait budget has elapsed."""
        return self._take_due() if self._pending else []

    def wait_budget(self) -> Optional[float]:
        """Seconds until the time flush is due, or None when nothing is pending."""
        if not self._pending:
            return None
        elapsed = self._clock() - self._pending_started
        return max(0.0, self.max_wait_ms / 1000.0 - elapsed)

    def _take_due(self) -> List[str]:
        budget = self.wait_budget()
        if budget is None or budget > 0:
            return []
        return self._drain()

    def _drain(self) -> List[str]:
        pending = self._pending
        self._pending = ""
        self._pending_started = None
        return [pending] if pending else []

    def _emit_ready(self) -> List[str]:
        emitted: List[str] = []
        while True:
            cut = self._find_break(self._pending)
            if cut is not None and cut <= self.max_chars:
                emitted.append(self._pending[:cut])
                self._pending = self._pending[cut:]
                self._pending_started = self._clock()
                continue
            if len(self._pending) > self.max_chars:
                emitted.append(self._force_length_flush())
                continue
            break
        return emitted

    def _force_length_flush(self) -> str:
        cut = -1
        for index in range(min(len(self._pending), self.max_chars + 1)):
            if self._pending[index].isspace():
                cut = index + 1
        if cut <= 0:
            chunk = self._pending
            self._pending = ""
        else:
            chunk = self._pending[:cut]
            self._pending = self._pending[cut:]
        if self._pending:
            self._pending_started = self._clock()
        else:
            self._pending_started = None
        return chunk

    def _find_break(self, text: str) -> Optional[int]:
        """Return the length of the next complete sentence chunk, if any."""
        in_inline_code = False
        index = 0
        while index < len(text) - 1:
            char = text[index]
            if char == "`":
                in_inline_code = not in_inline_code
            elif not in_inline_code and char in TERMINATORS:
                next_char = text[index + 1]
                if next_char.isspace():
                    if not self._guarded(text, index):
                        return index + 2
                elif _is_cjk_break(next_char) and not self._guarded(text, index):
                    return index + 1
            index += 1
        return None

    def _guarded(self, text: str, index: int) -> bool:
        if self._inside_url(text, index):
            return True
        if text[index] != ".":
            return False
        if index + 1 < len(text) and text[index + 1].isdigit():
            return True
        word = self._preceding_word(text, index)
        if word in ABBREVIATIONS:
            return True
        return len(word) == 1 and word.isalpha()

    @staticmethod
    def _preceding_word(text: str, index: int) -> str:
        word = []
        cursor = index - 1
        while cursor >= 0 and (text[cursor].isalpha() or text[cursor] == "."):
            word.append(text[cursor])
            cursor -= 1
        return "".join(reversed(word)).lower()

    @staticmethod
    def _inside_url(text: str, index: int) -> bool:
        for prefix in _URL_PREFIXES:
            start = text.rfind(prefix, 0, index + 1)
            if start == -1:
                continue
            segment = text[start:index]
            if not any(char.isspace() for char in segment):
                return True
        return False
