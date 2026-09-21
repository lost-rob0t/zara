from dataclasses import replace

import pytest

from zara.desktop.conversation import SymbolicConversationProjection


def _projection(**changes: object) -> SymbolicConversationProjection:
    base = SymbolicConversationProjection(
        conversation_id="conv-pure-validation",
        projection_generation=1,
        runtime_generation=1,
        turn_id="turn-1",
        outcome="pending",
        dialogue_act="clarify",
        renderer_provenance="symbolic-dcg/v1",
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )
    return replace(base, **changes)


def test_pure_symbolic_assertion_validates_full_projection_contract() -> None:
    with pytest.raises(ValueError, match="unsupported symbolic outcome"):
        _projection(outcome="provider_fallback").assert_pure_symbolic()

    with pytest.raises(ValueError, match="dialogue_act"):
        _projection(dialogue_act="Clarify Slot").assert_pure_symbolic()


def test_projection_rejects_model_usage_above_declared_budget() -> None:
    with pytest.raises(ValueError, match="model_calls must not exceed max_model_calls"):
        _projection(max_model_calls=1, model_calls=2).validate()
