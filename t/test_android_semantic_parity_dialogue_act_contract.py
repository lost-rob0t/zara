from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
PARITY = ROOT / "scripts/test-android-semantic-parity.sh"


def test_pinned_trealla_parity_executes_the_production_dialogue_act_wire() -> None:
    source = PARITY.read_text(encoding="utf-8")

    assert "__zara_act__:" in source
    assert "Act = answer(expert, _, _) -> ActName = expert_answer" in source
    assert "functor(Act, ActName, _)" in source
    assert "string_codes(ActWire, ActWireCodes)" in source
    assert "Result = ActWire" in source
    assert "Results = [Rendered, ContextWire, ActWire]" in source
    assert 'string_codes("__zara_act__:clarify", ExpectedActWireCodes)' in source
    assert "ActWireCodes == ExpectedActWireCodes" in source
