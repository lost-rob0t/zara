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
LOGIC_LANGUAGE = (
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
EXPERT_CONTRACT = (
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
    / "ExpertContract.kt"
)
EXPERT_INVOCATION_CONTRACT = (
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
    / "ExpertInvocationContract.kt"
)
SYMBOLIC_DIALOGUE = ROOT / "modules" / "symbolic_dialogue.pl"
INSTALLED_ACCEPTANCE = ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"


def test_natural_pure_symbolic_turn_reuses_existing_expert_router_and_response_contract() -> None:
    factory = FACTORY.read_text(encoding="utf-8")
    logic_language = LOGIC_LANGUAGE.read_text(encoding="utf-8")
    symbolic_dialogue = SYMBOLIC_DIALOGUE.read_text(encoding="utf-8")

    assert "object LocalNaturalLanguageExpertRouter" in logic_language
    assert "LocalNaturalLanguageExpertRouter.query" in factory, (
        "Android pure-symbolic natural turns must reuse the existing expert activation router; "
        "do not add a second expert registry or parser"
    )
    assert "symbolic_dialogue:response_act(expert_result(" in factory, (
        "A matched natural expert invocation must be projected through the canonical "
        "symbolic expert_result -> answer(expert, ..., evidence(...)) response contract"
    )
    assert "response_act(expert_result(summary(Summary), evidence(EvidenceRef))" in symbolic_dialogue
    assert "DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX" in factory

    # The pure-symbolic path must stay isolated from the legacy local-model fallback path.
    assert "generateLocalModelTurn" not in factory
    assert "submitLocalText(" not in factory


def test_natural_expert_invocation_must_cross_zara_expert_v1_admission_before_body_execution() -> None:
    factory = FACTORY.read_text(encoding="utf-8")
    contract = EXPERT_CONTRACT.read_text(encoding="utf-8") + EXPERT_INVOCATION_CONTRACT.read_text(encoding="utf-8")

    # Mirror the canonical #1233 request/result envelope on Android instead of inventing a
    # natural-language-only raw Prolog execution lane. These names intentionally match the
    # Python authority so parity is reviewable across runtimes.
    for required in (
        "data class ActivationHandle",
        "data class ExpertRequest",
        "data class ExpertResult",
        "activationId",
        "expertOperation",
        "expectedRegistryGeneration",
        "expectedRuntimeGeneration",
        "maxModelCalls",
        "evidenceRefs",
        "effectReceipts",
    ):
        assert required in contract, f"Android ZARA-EXPERT/1 mirror is missing {required}"

    assert 'operation = "expert.invoke"' in factory, (
        "Natural expert turns must be admitted as canonical expert.invoke requests"
    )
    assert "expectedRegistryGeneration" in factory
    assert "expectedRuntimeGeneration" in factory
    assert "maxModelCalls = 0" in factory, (
        "Pure-symbolic expert admission must carry the shared zero-model budget"
    )
    assert "effectReceipts" in factory, (
        "Successful expert effects must be projected from canonical receipts, never inferred from prose"
    )
    assert "expertTurnEnvelopeQuery(" not in factory, (
        "Natural expert turns must not execute a selected raw Prolog goal before canonical admission"
    )
    assert "naturalExpertEvidenceRef(" not in factory, (
        "Evidence must come from the canonical expert result/receipt, not a fabricated local turn reference"
    )


def test_installed_android_acceptance_proves_expert_evidence_survives_recreation_and_follow_up() -> None:
    source = INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")

    assert "expert_evidence_json" in source, (
        "Installed Android acceptance must inspect durable expert evidence, not only rendered text"
    )
    assert '"expert_answer"' in source, (
        "Installed Android acceptance must prove a natural turn reaches the canonical expert_answer act"
    )
    assert 'send_chat(device, "why?"' in source, (
        "Installed Android acceptance must exercise an evidence-linked follow-up after an expert answer"
    )

    expert_answer = source.index('"expert_answer"')
    why_follow_up = source.index('send_chat(device, "why?"')
    recreate = source.rfind("device.recreate()", expert_answer, why_follow_up)
    assert recreate != -1, (
        "The expert answer must survive Android process recreation before the evidence-linked why? follow-up"
    )

    assert 'stage="expert-answer"' in source
    assert 'stage="expert-follow-up-after-restart"' in source
