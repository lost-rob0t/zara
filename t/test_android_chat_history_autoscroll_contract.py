from pathlib import Path
import re


SOURCE = (
    Path(__file__).resolve().parents[1]
    / "android/app/src/main/java/ai/zara/app/ui/ZaraApp.kt"
)


def chat_surface_source() -> str:
    source = SOURCE.read_text(encoding="utf-8")
    start = source.index("private fun ChatSurface(")
    end = source.index("@Composable\nprivate fun TurnFailureCard", start)
    return source[start:end]


def test_chat_surface_keeps_new_terminal_turn_visible() -> None:
    """A newly appended assistant reply must become visible without manual scrolling."""
    body = chat_surface_source()

    assert "val historyScroll = rememberScrollState()" in body
    assert ".verticalScroll(historyScroll)" in body
    assert "val latestTurn = conversation?.turns?.lastOrNull()" in body
    assert "LaunchedEffect(" in body
    assert "conversation?.id" in body
    assert "latestTurn?.userText" in body
    assert "latestTurn?.assistantText" in body
    assert re.search(
        r"historyScroll\.(?:scrollTo|animateScrollTo)\(historyScroll\.maxValue\)",
        body,
    )
