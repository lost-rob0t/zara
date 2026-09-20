import json
from pathlib import Path

import pytest
from jsonschema import Draft202012Validator


ROOT = Path(__file__).resolve().parents[1]
SCHEMA_PATH = ROOT / "contracts" / "symbolic-dialogue-v1" / "response-act.schema.json"
PROLOG_PATH = ROOT / "modules" / "symbolic_dialogue.pl"


@pytest.fixture(scope="module")
def validator() -> Draft202012Validator:
    schema = json.loads(SCHEMA_PATH.read_text())
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


def envelope(act: str, payload: dict) -> dict:
    return {
        "protocol": "ZARA-SYMBOLIC-DIALOGUE/1",
        "act": act,
        "payload": payload,
        "renderer": "symbolic-dcg/v1",
        "providers_enabled": False,
        "max_model_calls": 0,
        "usage": {"provider_calls": 0, "model_calls": 0},
    }


@pytest.mark.parametrize(
    ("act", "payload"),
    [
        ("greeting", {}),
        ("help", {}),
        ("acknowledgement", {"kind": "thanks"}),
        ("acknowledgement", {"kind": "acknowledged"}),
        ("cancelled", {}),
        ("clarify", {"slot": "duration"}),
        ("clarify", {"reason": "ambiguous_reference"}),
        ("choose", {"choices": ["alpha", "beta"]}),
        ("invalid", {"slot": "duration", "reason": "not_a_duration"}),
        ("dispatch_required", {"frame_ref": "frame:turn-7"}),
        ("verified", {"outcome": "timer started", "evidence_ref": "evidence:timer-7"}),
        ("denied", {"reason": "capability_denied"}),
        ("unavailable", {"reason": "expert_unavailable"}),
        ("error", {"reason": "tool_failed"}),
        ("expert_answer", {"summary": "The project uses SWI-Prolog.", "evidence_ref": "evidence:kb-9"}),
        ("unsupported", {}),
    ],
)
def test_response_act_examples_validate(validator, act, payload):
    validator.validate(envelope(act, payload))


@pytest.mark.parametrize(
    "mutator",
    [
        lambda value: value.update(providers_enabled=True),
        lambda value: value.update(max_model_calls=1),
        lambda value: value["usage"].update(provider_calls=1),
        lambda value: value["usage"].update(model_calls=1),
        lambda value: value.update(protocol="ZARA-SYMBOLIC-DIALOGUE/2"),
        lambda value: value.update(renderer="model-fallback/v1"),
    ],
)
def test_pure_symbolic_envelope_rejects_provider_or_model_fallback(validator, mutator):
    value = envelope("greeting", {})
    mutator(value)
    with pytest.raises(Exception):
        validator.validate(value)


def test_acknowledgement_kind_is_closed(validator):
    validator.validate(envelope("acknowledgement", {"kind": "thanks"}))
    validator.validate(envelope("acknowledgement", {"kind": "acknowledged"}))
    with pytest.raises(Exception):
        validator.validate(envelope("acknowledgement", {"kind": "maybe"}))
    with pytest.raises(Exception):
        validator.validate(envelope("acknowledgement", {}))


def test_verified_act_requires_fresh_evidence_reference(validator):
    value = envelope("verified", {"outcome": "done"})
    with pytest.raises(Exception):
        validator.validate(value)


def test_dispatch_required_cannot_masquerade_as_verified(validator):
    value = envelope("dispatch_required", {"frame_ref": "frame:turn-7", "outcome": "done"})
    with pytest.raises(Exception):
        validator.validate(value)


def test_clarification_is_exactly_one_slot_or_reason(validator):
    with pytest.raises(Exception):
        validator.validate(envelope("clarify", {}))
    with pytest.raises(Exception):
        validator.validate(envelope("clarify", {"slot": "duration", "reason": "ambiguous"}))


def test_choice_bounds_match_symbolic_renderer(validator):
    validator.validate(envelope("choose", {"choices": [str(index) for index in range(8)]}))
    with pytest.raises(Exception):
        validator.validate(envelope("choose", {"choices": [str(index) for index in range(9)]}))


def test_wire_act_vocabulary_covers_prolog_response_acts():
    source = PROLOG_PATH.read_text()
    required_terms = {
        "greeting": "response_codes(greeting)",
        "help": "response_codes(help)",
        "acknowledgement": "response_codes(acknowledgement(",
        "cancelled": "response_codes(cancelled)",
        "clarify": "response_codes(clarify(",
        "choose": "response_codes(choose(",
        "invalid": "response_codes(invalid(",
        "dispatch_required": "response_codes(dispatch_required(",
        "verified": "response_codes(verified(",
        "denied": "response_codes(denied(",
        "unavailable": "response_codes(unavailable(",
        "error": "response_codes(error(",
        "expert_answer": "response_codes(answer(expert,",
        "unsupported": "response_codes(unsupported)",
    }
    schema = json.loads(SCHEMA_PATH.read_text())
    wire_acts = set(schema["properties"]["act"]["enum"])

    assert wire_acts == set(required_terms)
    for term in required_terms.values():
        assert term in source
