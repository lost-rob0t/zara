from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
FACTORY = ROOT / "android/app/src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"


def test_terminal_symbolic_projections_keep_canonical_renderer_provenance() -> None:
    """Every terminal pure-symbolic projection must remain publishable to the edge contract."""
    source = FACTORY.read_text(encoding="utf-8")
    terminal = source.split("private fun terminalProjection(", 1)[1].split(
        "private fun splitDialogueEnvelope", 1
    )[0]

    assert 'rendererProvenance = "symbolic-dcg/v1"' in terminal
    assert "rendererProvenance = if" not in terminal
