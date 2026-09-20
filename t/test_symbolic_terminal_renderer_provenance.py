from __future__ import annotations

from dataclasses import replace

import pytest

from zara.desktop.conversation import SymbolicConversationProjection


def _projection(*, outcome: str, renderer_provenance: str) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id="conv-renderer-fence",
        projection_generation=1,
        runtime_generation=7,
        turn_id="turn-7",
        outcome=outcome,
        dialogue_act="acknowledgement",
        renderer_provenance=renderer_provenance,
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )


def test_successful_pure_symbolic_projection_requires_canonical_renderer_evidence() -> None:
    missing = _projection(outcome="success", renderer_provenance="")

    with pytest.raises(ValueError, match="successful projection requires canonical symbolic renderer"):
        missing.validate()

    with pytest.raises(AssertionError, match="successful projection requires canonical symbolic renderer"):
        missing.assert_pure_symbolic()


def test_prerender_pending_projection_may_leave_renderer_empty() -> None:
    pending = _projection(outcome="pending", renderer_provenance="")

    pending.validate()
    pending.assert_pure_symbolic()

    rendered = replace(pending, renderer_provenance="symbolic-dcg/v1")
    rendered.validate()
    rendered.assert_pure_symbolic()
