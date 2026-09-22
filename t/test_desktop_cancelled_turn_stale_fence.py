from __future__ import annotations

from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationService,
    ConversationStore,
    MessageRole,
    MessageStatus,
)
from zara.runtime import events
from zara.runtime.commands import CommandReceipt


def test_cancelled_turn_fences_late_output_and_effects_after_restart(tmp_path):
    path = tmp_path / "cancelled-turn-stale-fence.db"
    first_db = DatabaseManager(path)
    first = ConversationService(ConversationStore(first_db))
    state = first.create_conversation("Cancelled pure-symbolic turn")
    conversation_id = state.conversation.id
    turn_id = "symbolic-turn-cancelled"

    user, _ = first.add_user_message(
        conversation_id,
        "set a timer",
        request_id="symbolic-request-cancelled",
    )
    first.bind_receipt(
        CommandReceipt(
            request_id="symbolic-request-cancelled",
            turn_id=turn_id,
        )
    )
    assert user.turn_id == turn_id
    assert user.status is MessageStatus.COMPLETE

    update = first.apply_event(
        events.TurnCancelled(
            conversation_id=conversation_id,
            turn_id=turn_id,
            reason="user cancelled",
        )
    )
    assert update is not None
    assert user.status is MessageStatus.CANCELLED
    assert user.error == "user cancelled"
    assert state.active_turn_id is None
    first_db.close()

    second_db = DatabaseManager(path)
    recovered = ConversationService(ConversationStore(second_db))
    recovered_state = recovered.get_state(conversation_id)
    recovered_user = recovered_state.latest_message(
        role=MessageRole.USER,
        turn_id=turn_id,
    )
    assert recovered_user is not None
    assert recovered_user.status is MessageStatus.CANCELLED
    assert recovered_user.error == "user cancelled"
    message_count = len(recovered_state.messages)

    late_events = (
        events.AssistantStarted(
            conversation_id=conversation_id,
            turn_id=turn_id,
        ),
        events.AssistantDelta(
            conversation_id=conversation_id,
            turn_id=turn_id,
            text="late assistant output",
        ),
        events.ResponseText(
            conversation_id=conversation_id,
            turn_id=turn_id,
            text="late buffered output",
        ),
        events.ToolStarted(
            conversation_id=conversation_id,
            turn_id=turn_id,
            tool_run_id="late-tool",
            tool_name="timer",
        ),
        events.ToolCompleted(
            conversation_id=conversation_id,
            turn_id=turn_id,
            tool_run_id="late-tool",
            tool_name="timer",
        ),
        events.RuntimeError(
            conversation_id=conversation_id,
            turn_id=turn_id,
            reason="late runtime error",
        ),
    )
    for event in late_events:
        assert recovered.apply_event(event) is None

    final_state = recovered.store.load_state(conversation_id)
    final_user = final_state.latest_message(role=MessageRole.USER, turn_id=turn_id)
    assert final_user is not None
    assert final_user.status is MessageStatus.CANCELLED
    assert final_user.error == "user cancelled"
    assert len(final_state.messages) == message_count
    assert final_state.latest_message(role=MessageRole.ASSISTANT, turn_id=turn_id) is None
    assert not any(message.tool_run_id == "late-tool" for message in final_state.messages)
    second_db.close()
