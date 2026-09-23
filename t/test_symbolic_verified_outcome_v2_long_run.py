from __future__ import annotations

from dataclasses import replace

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore, SymbolicConversationProjection

_WINDOW = 64
_SYMBOLIC_RENDERER = "symbolic-dcg/v1"


def _receipt(runtime_generation: int) -> str:
    return (
        "zara.verified-outcome/v2:"
        f"{runtime_generation}:outcome:postcondition/tool-run-{runtime_generation}"
    )


def _projection(
    conversation_id: str,
    *,
    generation: int,
    runtime_generation: int,
    receipts: list[str],
) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=generation,
        runtime_generation=runtime_generation,
        turn_id=f"turn-{runtime_generation}",
        outcome="success",
        dialogue_act="verified",
        dialogue_state={"act": "verified"},
        verified_outcome_refs=receipts,
        renderer_provenance=_SYMBOLIC_RENDERER,
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )


def _next_window(current: list[str], runtime_generation: int) -> list[str]:
    fresh = _receipt(runtime_generation)
    if len(current) < _WINDOW:
        return current + [fresh]
    return current[1:] + [fresh]


def test_eighty_verified_turns_remain_bounded_and_restart_replay_safe(tmp_path):
    database_path = tmp_path / "verified-v2-long-run.db"
    first = ConversationStore(DatabaseManager(database_path))
    conversation = first.create_conversation(
        "Verified v2 long run",
        conversation_id="conv-verified-v2-long-run",
    )

    current = first.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=1,
            runtime_generation=1,
            receipts=[_receipt(1)],
        ),
        expected_generation=0,
    )
    current.assert_pure_symbolic()

    for runtime_generation in range(2, 81):
        receipts = _next_window(current.verified_outcome_refs, runtime_generation)
        current = first.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=runtime_generation,
                runtime_generation=runtime_generation,
                receipts=receipts,
            ),
            expected_generation=current.projection_generation,
        )
        current.assert_pure_symbolic()
        assert len(current.verified_outcome_refs) <= _WINDOW
        assert current.verified_outcome_refs[-1] == _receipt(runtime_generation)
        assert current.max_model_calls == 0
        assert current.provider_calls == 0
        assert current.model_calls == 0

    assert current.projection_generation == 80
    assert current.runtime_generation == 80
    assert len(current.verified_outcome_refs) == _WINDOW
    retired = _receipt(1)
    assert retired not in current.verified_outcome_refs
    first.database.close()

    reopened = ConversationStore(DatabaseManager(database_path))
    recovered = reopened.load_symbolic_projection(conversation.id)
    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered.projection_generation == 80
    assert recovered.runtime_generation == 80
    assert recovered.verified_outcome_refs == current.verified_outcome_refs
    assert recovered.max_model_calls == 0
    assert recovered.provider_calls == 0
    assert recovered.model_calls == 0

    fresh = reopened.save_symbolic_projection(
        _projection(
            conversation.id,
            generation=81,
            runtime_generation=81,
            receipts=_next_window(recovered.verified_outcome_refs, 81),
        ),
        expected_generation=recovered.projection_generation,
    )
    fresh.assert_pure_symbolic()

    with pytest.raises(RuntimeError, match="retired verified outcome replay rejected"):
        reopened.save_symbolic_projection(
            _projection(
                conversation.id,
                generation=82,
                runtime_generation=82,
                receipts=fresh.verified_outcome_refs[1:] + [retired],
            ),
            expected_generation=fresh.projection_generation,
        )

    late = replace(
        fresh,
        projection_generation=81,
        runtime_generation=82,
        turn_id="turn-late",
        verified_outcome_refs=fresh.verified_outcome_refs,
        updated_at="",
    )
    with pytest.raises(RuntimeError, match="stale symbolic projection write"):
        reopened.save_symbolic_projection(late, expected_generation=80)
