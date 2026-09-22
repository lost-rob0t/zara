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
PORT = (
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


def test_conversation_factory_consumes_existing_canonical_expert_owner() -> None:
    """Pin composition at the existing owner boundary; never grow a second expert authority."""
    factory = FACTORY.read_text(encoding="utf-8")
    port = PORT.read_text(encoding="utf-8")

    assert "interface CanonicalExpertInvocationPort" in port
    assert "PureSymbolicExpertInvocationAdapter" in port
    assert "CanonicalExpertInvocationPort" in factory, (
        "Android pure-symbolic conversation must receive the existing canonical expert owner "
        "through the consumer-only port instead of executing expert bodies itself."
    )
    assert "PureSymbolicExpertInvocationAdapter" in factory
    assert "LocalNaturalLanguageExpertRouter.select" in factory

    assert "object : CanonicalExpertInvocationPort" not in factory, (
        "The conversation factory must not fabricate a local CanonicalExpertInvocationPort; "
        "the implementation is supplied by the existing activation/registered-predicate owner."
    )
    assert "class AndroidCanonicalExpertInvocationPort" not in factory, (
        "Do not create a second Android expert owner in the conversation layer."
    )
    assert "expertTurnEnvelopeQuery(" not in factory
    assert "naturalExpertEvidenceRef(" not in factory


def test_canonical_owner_adapter_remains_consumer_only() -> None:
    """Keep registry/lifecycle/execution ownership outside the conversation adapter."""
    port = PORT.read_text(encoding="utf-8")

    for forbidden in (
        "fun register(",
        "fun activate(",
        "fun grantCapability(",
        "queryLocalProlog(",
        "PrologWorkspace(",
        "ExpertRegistry(",
    ):
        assert forbidden not in port, (
            f"CanonicalExpertInvocationPort consumer seam must not acquire owner behavior: {forbidden}"
        )
