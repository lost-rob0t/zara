import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
LOGIC = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "main"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "prolog"
    / "LogicLanguage.kt"
)
FACTORY = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "main"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "prolog"
    / "AndroidPureSymbolicConversationFactory.kt"
)
CANONICAL_TURN = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "main"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "prolog"
    / "CanonicalNaturalExpertTurn.kt"
)


def _selection_fields(source: str) -> str:
    match = re.search(
        r"data class NaturalLanguageExpertSelection\((.*?)\)\s*\n\s*object LocalNaturalLanguageExpertRouter",
        source,
        flags=re.DOTALL,
    )
    assert match is not None, "NaturalLanguageExpertSelection must remain the typed router result"
    return match.group(1)


def test_typed_expert_selection_carries_canonical_invocation_shape_not_a_raw_goal() -> None:
    """Natural routing must preserve identity/operation/input without handing execution a Prolog goal."""
    fields = _selection_fields(LOGIC.read_text(encoding="utf-8"))

    assert "val expertId: String" in fields
    assert "val expertOperation: String" in fields, (
        "Typed expert selection must carry the canonical expert operation needed by expert.invoke."
    )
    assert "val input: Map<String, Any?>" in fields, (
        "Typed expert selection must carry bounded canonical input instead of encoding it in a Prolog goal."
    )
    assert "val query: String" not in fields, (
        "The typed selection path must not expose a raw Prolog query; query(...) is compatibility-only."
    )


def test_conversation_composition_consumes_only_typed_expert_invocation_fields() -> None:
    """Factory -> canonical-turn composition must preserve typed fields without recovering a raw goal."""
    factory = FACTORY.read_text(encoding="utf-8")
    canonical_turn = CANONICAL_TURN.read_text(encoding="utf-8")

    assert "LocalNaturalLanguageExpertRouter.select" in factory
    assert "CanonicalNaturalExpertTurn(" in factory
    assert "selection = selection" in factory, (
        "The factory must hand the exact typed router result to the canonical expert turn boundary."
    )
    for field in ("expertId", "expertOperation", "input"):
        assert f"selection.{field}" in canonical_turn, (
            f"CanonicalNaturalExpertTurn must forward selection.{field} into canonical expert.invoke."
        )
    assert "adapter.invoke(" in canonical_turn
    assert "selection.query" not in factory
    assert "selection.query" not in canonical_turn
    assert "LocalNaturalLanguageExpertRouter.query" not in factory
