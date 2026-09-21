from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
PARITY = ROOT / "scripts/test-android-semantic-parity.sh"


def test_pinned_trealla_parity_executes_the_production_dialogue_act_and_expert_evidence_wire() -> None:
    source = PARITY.read_text(encoding="utf-8")

    assert "write_term_to_atom(ContextAtom, Context1, [quoted(true)])" in source
    assert "__zara_act__:" in source
    assert "Act = answer(expert, _, evidence(EvidenceRef))" in source
    assert "ActName = expert_answer" in source
    assert "functor(Act, ActName, _)" in source
    assert "string_codes(ActWire, ActWireCodes)" in source
    assert "__zara_expert_evidence__:" in source
    assert "string_codes(EvidenceWire, EvidenceWireCodes)" in source
    assert "Result = ActWire" in source
    assert "Result = EvidenceWire" in source
    assert "Results = [Rendered, ContextWire, ActWire, EvidenceWire]" in source
    assert 'string_codes("__zara_act__:clarify", ExpectedActWireCodes)' in source
    assert "ActWireCodes == ExpectedActWireCodes" in source
    assert "parity_expert_evidence_wire" in source
    assert 'EvidenceWire == "expert:dotfiles:1"' in source
