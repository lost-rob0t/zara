from __future__ import annotations

import pytest

from zara.desktop.conversation import SymbolicConversationProjection


def _projection(evidence: list[dict[str, object]]) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id="conversation:expert-casefold-fence",
        projection_generation=1,
        runtime_generation=1,
        turn_id="turn:expert-casefold:1",
        outcome="pending",
        project_id="project:zara",
        project_generation=1,
        dialogue_act="expert.answer",
        dialogue_state={"active_project": "project:zara"},
        expert_evidence=evidence,
        renderer_provenance="symbolic-dcg/v1",
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )


@pytest.mark.parametrize(
    "poisoned",
    [
        {
            "expert_id": "zara:expert/python",
            "model_calls": 0,
            "MODEL_CALLS": 7,
        },
        {
            "expert_id": "zara:expert/python",
            "model_calls": 0,
            "explanation": {"MoDeL_CaLlS": 7},
        },
    ],
)
def test_mixed_case_model_calls_metadata_fails_closed(poisoned: dict[str, object]) -> None:
    with pytest.raises((TypeError, ValueError, AssertionError)):
        _projection([poisoned]).assert_pure_symbolic()
