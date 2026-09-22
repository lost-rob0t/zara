from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
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


def test_natural_expert_path_uses_the_canonical_zero_model_admission_adapter() -> None:
    """Reject cosmetic expert.invoke strings that still bypass canonical result admission."""
    factory = FACTORY.read_text(encoding="utf-8")

    assert "PureSymbolicExpertAdmission.request(" in factory, (
        "Natural expert turns must build the canonical expert.invoke request from an "
        "already-issued activation; a dead operation string is not authority crossing."
    )
    assert "PureSymbolicExpertAdmission.validateResult(" in factory, (
        "Canonical expert results must be checked for request/activation/generation identity, "
        "shared maxModelCalls=0, canonical evidence, and verified effect postconditions before "
        "conversation projection."
    )
    assert "expertTurnEnvelopeQuery(" not in factory, (
        "The admitted result must be projected after canonical invocation, not by executing a "
        "selected raw Prolog goal inside the conversation factory."
    )
    assert "naturalExpertEvidenceRef(" not in factory, (
        "Conversation evidence must come from the admitted canonical result, not a synthetic "
        "predicate/turn reference."
    )
