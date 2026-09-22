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
EXPERT_ADAPTER = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "main"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "expert"
    / "CanonicalExpertInvocationPort.kt"
)
EXPERT_ADMISSION = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "main"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "expert"
    / "PureSymbolicExpertAdmission.kt"
)


def test_natural_expert_path_uses_the_canonical_zero_model_admission_adapter() -> None:
    """Pin authority crossing at the consumer seam without duplicating admission logic."""
    factory = FACTORY.read_text(encoding="utf-8")
    adapter = EXPERT_ADAPTER.read_text(encoding="utf-8")
    admission = EXPERT_ADMISSION.read_text(encoding="utf-8")

    assert "PureSymbolicExpertInvocationAdapter" in factory, (
        "Natural expert turns must consume the existing canonical invocation adapter; the "
        "conversation factory must not reimplement ZARA-EXPERT/1 admission internals."
    )
    assert "PureSymbolicExpertAdmission.request(" in adapter, (
        "The canonical invocation adapter must build expert.invoke from an already-issued "
        "activation before asking the existing owner to execute it."
    )
    assert "PureSymbolicExpertAdmission.validateResult(" in adapter, (
        "The canonical invocation adapter must validate the returned result against the live "
        "registry/runtime generations before conversation projection."
    )
    assert 'operation = "expert.invoke"' in admission
    assert "expectedRegistryGeneration = activation.registryGeneration" in admission
    assert "expectedRuntimeGeneration = activation.runtimeGeneration" in admission
    assert "limits.maxModelCalls == 0" in admission
    assert 'result.usage["provider_calls"]' in admission
    assert 'result.usage["model_calls"]' in admission
    assert "result.effectReceipts" in admission

    assert "expertTurnEnvelopeQuery(" not in factory, (
        "The admitted result must be projected after canonical invocation, not by executing a "
        "selected raw Prolog goal inside the conversation factory."
    )
    assert "naturalExpertEvidenceRef(" not in factory, (
        "Conversation evidence must come from the admitted canonical result, not a synthetic "
        "predicate/turn reference."
    )
