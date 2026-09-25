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
SYMBOLIC_DIALOGUE = ROOT / "modules" / "symbolic_dialogue.pl"
INSTALLED_ACCEPTANCE = ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"


def test_natural_pure_symbolic_turn_reuses_existing_expert_router_and_response_contract() -> None:
    factory = FACTORY.read_text(encoding="utf-8")
    logic_language = LOGIC_LANGUAGE.read_text(encoding="utf-8")
    symbolic_dialogue = SYMBOLIC_DIALOGUE.read_text(encoding="utf-8")

    assert "object LocalNaturalLanguageExpertRouter" in logic_language
    assert "LocalNaturalLanguageExpertRouter.select" in factory, (
        "Android pure-symbolic natural turns must reuse the existing expert activation router "
        "and preserve the selected expert identity for canonical admission; do not add a second "
        "expert registry or parser"
    )
    assert "canonicalExpertEnvelopeResult(" in factory, (
        "A matched natural expert invocation must project the admitted canonical expert result "
        "into the ordinary symbolic dialogue envelope rather than re-executing a raw Prolog goal"
    )
    assert "projected.summary" in factory
    assert "projected.evidenceRef" in factory
    assert '"${DIALOGUE_ACT_WIRE_PREFIX}expert_answer"' in factory
    assert "response_act(expert_result(summary(Summary), evidence(EvidenceRef))" in symbolic_dialogue
    assert "DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX" in factory

    # The pure-symbolic path must stay isolated from the legacy local-model fallback path.
    assert "generateLocalModelTurn" not in factory
    assert "submitLocalText(" not in factory


def test_natural_expert_router_preserves_identity_for_canonical_expert_admission() -> None:
    logic_language = LOGIC_LANGUAGE.read_text(encoding="utf-8")
    router = logic_language.split("object LocalNaturalLanguageExpertRouter", 1)[1].split(
        "data class LocalPrologCommand",
        1,
    )[0]

    assert "data class NaturalLanguageExpertSelection" in logic_language, (
        "The deterministic router must return a typed selection instead of dropping expert "
        "identity and returning only a raw Prolog query string"
    )
    assert "fun select(" in router
    assert "expertId" in router
    assert "query" in router
    assert "fun query(" in router, (
        "Keep the existing query compatibility surface while canonical conversation routing "
        "moves to the typed selection contract"
    )


def test_natural_expert_invocation_must_cross_zara_expert_v1_admission_before_body_execution() -> None:
    factory = FACTORY.read_text(encoding="utf-8")
    contract = EXPERT_CONTRACT.read_text(encoding="utf-8") + EXPERT_INVOCATION_CONTRACT.read_text(encoding="utf-8")
    adapter = EXPERT_ADAPTER.read_text(encoding="utf-8")
    admission = EXPERT_ADMISSION.read_text(encoding="utf-8")

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

    assert "PureSymbolicExpertInvocationAdapter" in factory, (
        "Natural expert turns must cross the existing canonical expert invocation adapter, "
        "not duplicate admission logic in the conversation factory."
    )
    assert "PureSymbolicExpertAdmission.request(" in adapter
    assert "PureSymbolicExpertAdmission.validateResult(" in adapter
    assert 'operation = "expert.invoke"' in admission, (
        "Pure-symbolic expert admission must produce canonical expert.invoke requests"
    )
    assert "expectedRegistryGeneration = activation.registryGeneration" in admission
    assert "expectedRuntimeGeneration = activation.runtimeGeneration" in admission
    assert "limits.maxModelCalls == 0" in admission, (
        "Pure-symbolic expert admission must carry the shared zero-model budget"
    )
    assert "result.effectReceipts" in admission, (
        "Successful expert effects must be admitted only from canonical receipts and verified evidence"
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
