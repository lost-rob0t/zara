from __future__ import annotations

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore
from zara.desktop.conversation.symbolic_runtime import PureSymbolicProjectionAdapter
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend


@pytest.mark.asyncio
async def test_pure_symbolic_dialogue_context_survives_backend_restart(tmp_path) -> None:
    store = ConversationStore(DatabaseManager(tmp_path / "symbolic-context.db"))
    conversation = store.create_conversation(
        "Symbolic continuity",
        conversation_id="conv-symbolic-continuity",
    )

    first_backend = PureSymbolicRuntimeBackend(
        projection_adapter=PureSymbolicProjectionAdapter(store),
    )
    await first_backend.start()
    try:
        first = await first_backend.submit_turn(
            "timer",
            turn_id="turn-symbolic-1",
            conversation_id=conversation.id,
        )
        assert first.response == "How long should I set the timer for?"
        first_backend.commit_turn_result(
            first,
            turn_id="turn-symbolic-1",
            conversation_id=conversation.id,
        )
    finally:
        await first_backend.stop()

    first_projection = store.load_symbolic_projection(conversation.id)
    assert first_projection is not None
    first_projection.assert_pure_symbolic()
    assert first_projection.projection_generation == 1
    assert first_projection.runtime_generation == 1
    assert first_projection.dialogue_act == "clarify"
    assert "partial_frame" in first_projection.dialogue_state["prolog_context_term"]

    restarted_backend = PureSymbolicRuntimeBackend(
        projection_adapter=PureSymbolicProjectionAdapter(store),
    )
    await restarted_backend.start()
    try:
        second = await restarted_backend.submit_turn(
            "5 minutes",
            turn_id="turn-symbolic-2",
            conversation_id=conversation.id,
        )
        assert second.response == (
            "That action needs capability-checked execution before I can report success."
        )
        assert second.metadata["response_act"].startswith("dispatch_required(")
        restarted_backend.commit_turn_result(
            second,
            turn_id="turn-symbolic-2",
            conversation_id=conversation.id,
        )
    finally:
        await restarted_backend.stop()

    second_projection = store.load_symbolic_projection(conversation.id)
    assert second_projection is not None
    second_projection.assert_pure_symbolic()
    assert second_projection.projection_generation == 2
    assert second_projection.runtime_generation == 2
    assert second_projection.dialogue_act == "dispatch_required"
    assert "completed_frame" in second_projection.dialogue_state["prolog_context_term"]
    assert second_projection.providers_enabled is False
    assert second_projection.max_model_calls == 0
    assert second_projection.provider_calls == 0
    assert second_projection.model_calls == 0
