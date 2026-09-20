from __future__ import annotations

from dataclasses import replace

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationStore,
    MessageRecord,
    MessageRole,
    MessageStatus,
    SymbolicConversationProjection,
)


def test_restart_recovery_terminalizes_matching_symbolic_projection(tmp_path):
    database_path = tmp_path / "restart-recovery.db"
    first = ConversationStore(DatabaseManager(database_path))
    conversation = first.create_conversation(
        "restart recovery",
        conversation_id="conv-restart-recovery",
    )
    first.save_message(
        MessageRecord(
            id="assistant-pending",
            conversation_id=conversation.id,
            sequence=1,
            role=MessageRole.ASSISTANT,
            content="Working on it",
            status=MessageStatus.STREAMING,
            created_at="2026-09-20T12:00:00.000000",
            updated_at="2026-09-20T12:00:00.000000",
            turn_id="turn-restart-recovery",
        )
    )
    pending = first.save_symbolic_projection(
        SymbolicConversationProjection(
            conversation_id=conversation.id,
            projection_generation=1,
            runtime_generation=23,
            turn_id="turn-restart-recovery",
            outcome="pending",
            project_id="project-restart",
            project_generation=1,
            dialogue_act="clarify",
            dialogue_state={"slot": "target"},
            unresolved_questions=[{"slot": "target"}],
            renderer_provenance="symbolic-dcg/v1",
            providers_enabled=False,
            max_model_calls=0,
            provider_calls=0,
            model_calls=0,
        ),
        expected_generation=0,
    )
    pending.assert_pure_symbolic()
    first.database.close()

    reopened = ConversationStore(DatabaseManager(database_path))
    state = reopened.load_state(conversation.id)
    message = state.messages[0]
    assert message.status is MessageStatus.CANCELLED
    assert message.error == "Interrupted when Zara stopped."

    recovered = reopened.load_symbolic_projection(conversation.id)
    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered.outcome == "interrupted"
    assert recovered.projection_generation == 2
    assert recovered.runtime_generation == 23
    assert recovered.turn_id == "turn-restart-recovery"
    assert recovered.max_model_calls == 0
    assert recovered.provider_calls == 0
    assert recovered.model_calls == 0

    with pytest.raises(RuntimeError, match="terminal turn projection is immutable"):
        reopened.save_symbolic_projection(
            replace(
                recovered,
                projection_generation=3,
                outcome="success",
                verified_facts=[{"fact_id": "late"}],
                updated_at="",
            ),
            expected_generation=2,
        )
