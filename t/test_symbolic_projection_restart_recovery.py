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


def _pending_projection(conversation_id: str, turn_id: str) -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id=conversation_id,
        projection_generation=1,
        runtime_generation=23,
        turn_id=turn_id,
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
    )


def _assert_restart_fenced(
    store: ConversationStore,
    conversation_id: str,
    turn_id: str,
) -> None:
    recovered = store.load_symbolic_projection(conversation_id)
    assert recovered is not None
    recovered.assert_pure_symbolic()
    assert recovered.outcome == "interrupted"
    assert recovered.projection_generation == 2
    assert recovered.runtime_generation == 23
    assert recovered.turn_id == turn_id
    assert recovered.max_model_calls == 0
    assert recovered.provider_calls == 0
    assert recovered.model_calls == 0

    with pytest.raises(RuntimeError, match="terminal turn projection is immutable"):
        store.save_symbolic_projection(
            replace(
                recovered,
                projection_generation=3,
                outcome="success",
                verified_facts=[{"fact_id": "late"}],
                updated_at="",
            ),
            expected_generation=2,
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
        _pending_projection(conversation.id, "turn-restart-recovery"),
        expected_generation=0,
    )
    pending.assert_pure_symbolic()
    first.database.close()

    reopened = ConversationStore(DatabaseManager(database_path))
    state = reopened.load_state(conversation.id)
    message = state.messages[0]
    assert message.status is MessageStatus.CANCELLED
    assert message.error == "Interrupted when Zara stopped."

    _assert_restart_fenced(reopened, conversation.id, "turn-restart-recovery")


def test_restart_recovery_terminalizes_orphan_pending_projection(tmp_path):
    """A crash between projection/message writes must still fence late output."""

    database_path = tmp_path / "restart-orphan-projection.db"
    first = ConversationStore(DatabaseManager(database_path))
    conversation = first.create_conversation(
        "restart orphan projection",
        conversation_id="conv-restart-orphan-projection",
    )
    pending = first.save_symbolic_projection(
        _pending_projection(conversation.id, "turn-restart-orphan"),
        expected_generation=0,
    )
    pending.assert_pure_symbolic()
    assert first.load_messages(conversation.id) == []
    first.database.close()

    reopened = ConversationStore(DatabaseManager(database_path))
    state = reopened.load_state(conversation.id)
    assert state.messages == []

    _assert_restart_fenced(reopened, conversation.id, "turn-restart-orphan")
