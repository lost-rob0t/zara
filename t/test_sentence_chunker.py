"""Sentence/phrase chunker tests for ZARA-027 streaming (A2).

The chunker is a pure producer-side component: it converts a live token/text
stream into speakable sentence-sized chunks with bounded forced flushes.

Core invariant under test: ``"".join(feed outputs) + "".join(final flush)``
always equals the exact fed text, so no character is ever duplicated or lost
at chunk boundaries.
"""

from __future__ import annotations

import random
import time

import pytest

from zara.agent.sentence_chunker import SentenceChunker


class FakeClock:
    def __init__(self, value: float = 0.0):
        self.value = value

    def __call__(self) -> float:
        return self.value

    def advance(self, seconds: float) -> None:
        self.value += seconds


def collect(text: str, **kwargs) -> list[str]:
    """Feed an entire text in one shot and flush the remainder."""
    chunker = SentenceChunker(**kwargs)
    emitted = chunker.feed(text)
    return emitted + chunker.flush()


def test_basic_punctuation_breaks():
    assert collect("Hello there. How are you? Great! Done.") == [
        "Hello there. ",
        "How are you? ",
        "Great! ",
        "Done.",
    ]


def test_abbreviations_do_not_break():
    text = (
        "Dr. Smith arrived. "
        "e.g. this. "
        "i.e. that. "
        "St. Bernard vs. the fox. "
        "Done."
    )

    assert collect(text) == [
        "Dr. Smith arrived. ",
        "e.g. this. ",
        "i.e. that. ",
        "St. Bernard vs. the fox. ",
        "Done.",
    ]


def test_single_letter_initial_does_not_break():
    assert collect("J. Doe arrived. Done.") == [
        "J. Doe arrived. ",
        "Done.",
    ]


def test_decimals_do_not_break():
    assert collect("Pi is 3.14159 exactly. Cost: 5. Next.") == [
        "Pi is 3.14159 exactly. ",
        "Cost: 5. ",
        "Next.",
    ]


def test_decimal_split_across_feeds_is_held_back():
    chunker = SentenceChunker()

    assert chunker.feed("Pi is 3.") == []
    assert chunker.feed("14159 exactly. ") == ["Pi is 3.14159 exactly. "]


def test_urls_do_not_break():
    assert collect("Visit https://example.com/a.b?x=1 now. Done.") == [
        "Visit https://example.com/a.b?x=1 now. ",
        "Done.",
    ]


def test_www_urls_do_not_break():
    assert collect("See www.example.com/docs today. Done.") == [
        "See www.example.com/docs today. ",
        "Done.",
    ]


def test_code_fence_suppresses_breaks():
    text = "Run this:\n```py\nx = 1.5. y = 2\n```\nDone. Next."
    chunker = SentenceChunker()
    emitted = []
    for line in text.splitlines(keepends=True):
        emitted.extend(chunker.feed(line))
    emitted.extend(chunker.flush())

    assert emitted == [
        "Run this:\n```py\nx = 1.5. y = 2\n```\nDone. ",
        "Next.",
    ]


def test_inline_code_does_not_break():
    assert collect("The `foo.bar` value works. Done.") == [
        "The `foo.bar` value works. ",
        "Done.",
    ]


def test_ellipsis_breaks():
    assert collect("Wait… then go. Done.") == [
        "Wait… ",
        "then go. ",
        "Done.",
    ]


def test_triple_dot_breaks_only_at_the_end():
    assert collect("Wait... then go. Done.") == [
        "Wait... ",
        "then go. ",
        "Done.",
    ]


def test_cjk_punctuation_breaks_without_whitespace():
    assert collect("你好。世界！再见。") == [
        "你好。",
        "世界！",
        "再见。",
    ]


def test_emoji_is_never_split_across_chunks():
    text = "Check 👨‍👩‍👧‍👦 family. Done."
    pieces = [
        text[:10],
        text[10:16],
        text[16:],
    ]
    chunker = SentenceChunker()
    emitted = []
    for piece in pieces:
        emitted.extend(chunker.feed(piece))
    emitted.extend(chunker.flush())

    assert "".join(emitted) == text
    assert "👨‍👩‍👧‍👦" in emitted[0]


def test_streaming_holdback_across_feeds():
    chunker = SentenceChunker()

    assert chunker.feed("Hello there.") == []
    assert chunker.feed(" How") == ["Hello there. "]
    assert chunker.feed(" are you?") == []
    assert chunker.feed(" Fine.") == ["How are you? "]
    assert chunker.flush() == ["Fine."]


def test_punctuationless_text_flushes_at_max_chars():
    chunker = SentenceChunker(max_chars=20, clock=FakeClock())

    assert chunker.feed("aaaa bbbb ") == []
    assert chunker.feed("cccc dddd ") == []
    assert chunker.feed("eeee") == ["aaaa bbbb cccc dddd "]
    assert chunker.flush() == ["eeee"]


def test_length_flush_without_any_whitespace_caps_the_buffer():
    chunker = SentenceChunker(max_chars=20, clock=FakeClock())

    emitted = chunker.feed("x" * 200)

    assert emitted == ["x" * 200]
    assert chunker.flush() == []


def test_time_flush_uses_fake_clock():
    clock = FakeClock()
    chunker = SentenceChunker(max_wait_ms=500, clock=clock)

    assert chunker.feed("hello the") == []
    clock.advance(0.6)
    assert chunker.feed("re") == ["hello the"]
    assert chunker.flush() == ["re"]


def test_time_flush_not_due_before_budget():
    clock = FakeClock()
    chunker = SentenceChunker(max_wait_ms=500, clock=clock)

    assert chunker.feed("hello the") == []
    clock.advance(0.4)
    assert chunker.feed("re") == []
    assert chunker.flush() == ["hello there"]


def test_wait_budget_reports_seconds_until_due():
    clock = FakeClock()
    chunker = SentenceChunker(max_wait_ms=500, clock=clock)

    assert chunker.wait_budget() is None
    chunker.feed("hello")
    assert chunker.wait_budget() == pytest.approx(0.5)
    clock.advance(0.2)
    assert chunker.wait_budget() == pytest.approx(0.3)
    chunker.flush()
    assert chunker.wait_budget() is None


def test_take_due_flushes_only_when_elapsed():
    clock = FakeClock()
    chunker = SentenceChunker(max_wait_ms=500, clock=clock)

    assert chunker.feed("partial words") == []
    assert chunker.take_due() == []
    clock.advance(0.5)
    assert chunker.take_due() == ["partial words"]
    assert chunker.take_due() == []


def test_empty_and_whitespace_input():
    chunker = SentenceChunker(clock=FakeClock())

    assert chunker.feed("") == []
    assert chunker.feed("   ") == []
    assert chunker.flush() == ["   "]


def test_feed_returns_new_list_each_call():
    chunker = SentenceChunker(clock=FakeClock())
    first = chunker.feed("One. Two.")
    first.append("mutated")

    assert chunker.flush() == ["Two."]


@pytest.mark.parametrize(
    "text",
    [
        "One sentence. Another one! A third? The end.",
        "No punctuation at all in this long winded phrase",
        "Mixed.247 values 3.5 and 42. Together now.",
        "URLs https://a.b/c.d and www.e.f/g.h plus text. End.",
        "Dr. Who met J. Smith at St. Mary's. Done.",
        "第一句。第二句！第三句？完。",
        "Emoji 👋 and 🎉 party. Done.",
        "```\ncode.with.periods\n``` after. Done.",
        "Tabs\tand\nnewlines. Next. Done.",
    ],
)
def test_whole_text_concatenation_is_exact(text):
    emitted = collect(text, clock=FakeClock())

    assert emitted and "".join(emitted) == text


def test_streamed_pieces_reconstruct_the_exact_text():
    rng = random.Random(28)
    sentences = [
        "The answer is 42. ",
        "Counting 3.14 decimals now! ",
        "Dr. Smith said hello. ",
        "Visit https://example.com/x.y for more? ",
        "Punctuationless clause keeps flowing without any marks ",
        "你好。世界！",
        "Emoji 👋 here. ",
        "…ellipsis worked. ",
    ]

    for _ in range(50):
        text = "".join(rng.choices(sentences, k=rng.randint(1, 6)))
        chunker = SentenceChunker(clock=FakeClock())
        emitted = []
        position = 0
        while position < len(text):
            size = rng.randint(1, 9)
            emitted.extend(chunker.feed(text[position : position + size]))
            position += size
        emitted.extend(chunker.flush())

        assert "".join(emitted) == text


def test_default_bounds_match_design():
    chunker = SentenceChunker()

    assert chunker.max_chars == 180
    assert chunker.max_wait_ms == 500
    assert chunker._clock is time.monotonic


def test_constructor_rejects_invalid_bounds():
    with pytest.raises(ValueError, match="max_chars"):
        SentenceChunker(max_chars=0)

    with pytest.raises(ValueError, match="max_wait_ms"):
        SentenceChunker(max_wait_ms=0)
