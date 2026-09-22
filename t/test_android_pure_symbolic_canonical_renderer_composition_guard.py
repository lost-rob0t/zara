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


def _expert_branch(source: str) -> str:
    marker = "if (selection != null) {"
    assert marker in source, "Factory must keep an explicit typed natural-expert branch"
    branch = source.split(marker, 1)[1].split("} else {", 1)[0]
    return branch


def test_natural_expert_branch_uses_existing_symbolic_response_renderer() -> None:
    """Admitted expert output must cross the canonical symbolic response-act renderer before UI."""
    source = FACTORY.read_text(encoding="utf-8")
    branch = _expert_branch(source)

    assert "CanonicalRenderedNaturalExpertTurn(" in branch, (
        "Natural expert composition must route admitted canonical output through the existing "
        "symbolic_dialogue response-act renderer before creating a terminal conversation envelope."
    )
    assert "CanonicalNaturalExpertTurn(" in branch
    assert "PureSymbolicExpertInvocationAdapter(port)" in branch
    assert "CanonicalExpertSymbolicRendererAdapter(session::queryLocalProlog)" in branch, (
        "Android must reuse the existing local symbolic runtime for deterministic expert rendering; "
        "do not add a second renderer or provider path."
    )
    assert "canonicalRenderedExpertEnvelopeResult(" in branch
    assert "canonicalExpertEnvelopeResult(" not in branch, (
        "The production expert branch must not bypass symbolic response_act/render_response by "
        "projecting the admitted summary directly into the UI envelope."
    )


def test_rendered_expert_envelope_preserves_canonical_evidence_and_runtime_generation() -> None:
    """The terminal envelope must retain renderer text, admitted evidence, and renderer generation."""
    source = FACTORY.read_text(encoding="utf-8")

    marker = "internal fun canonicalRenderedExpertEnvelopeResult("
    assert marker in source, (
        "Factory needs a bounded adapter from CanonicalRenderedExpertAnswer into its existing "
        "four-term terminal dialogue envelope."
    )
    helper = source.split(marker, 1)[1].split("\n    internal fun ", 1)[0]

    assert "CanonicalRenderedExpertAnswer" in helper
    assert "rendered.text" in helper
    assert "rendered.evidenceRef" in helper
    assert "rendered.runtimeGeneration" in helper
    assert "expert_answer" in helper
    assert "DIALOGUE_CONTEXT_WIRE_PREFIX" in helper
    assert "DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX" in helper
