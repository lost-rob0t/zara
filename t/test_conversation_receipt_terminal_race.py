from __future__ import annotations

import pytest

from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationService,
    ConversationStore,
    MessageRole,
    MessageStatus,
)
from zara.runtime import events
from zara.runtime.commands import CommandReceipt


def make_service(tmp_path) -> ConversationService:
    return ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "conversation.db"))
    )


def test_late_receipt_does_not_resurrect_already_terminal_turn(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Receipt race")
    user, _ = service.add_user_message(
        state.conversation.id,
        "zzzxqvvv qqqzxvvv",
        request_id="req-1",
    )

    service.apply_event(
        events.TurnStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-1",
        )
    )
    assert user.turn_id == "turn-1"
    assert state.active_turn_id == "turn-1"

    service.apply_event(
        events.ResponseText(
            conversation_id=state.conversation.id,
            turn_id="turn-1",
            text="I don’t know how to handle that symbolically yet.",
        )
    )
    service.apply_event(
        events.AgentCompleted(
            conversation_id=state.conversation.id,
            turn_id="turn-1",
            success=True,
        )
    )
    service.apply_event(
        events.OutputReady(
            conversation_id=state.conversation.id,
            turn_id="turn-1",
        )
    )

    assistant = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-1")
    assert assistant is not None
    assert assistant.status is MessageStatus.COMPLETE
    assert state.active_turn_id is None
    assert service.has_pending_request(state.conversation.id) is True

    update = service.bind_receipt(
        CommandReceipt(request_id="req-1", turn_id="turn-1", detail="turn accepted")
    )

    assert update is not None
    assert update.active_turn_changed is False
    assert state.active_turn_id is None
    assert service.has_pending_request(state.conversation.id) is False
    assert user.turn_id == "turn-1"


def test_conflicting_late_receipt_fails_closed_without_rebinding_turn(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Receipt conflict")
    user, _ = service.add_user_message(
        state.conversation.id,
        "help",
        request_id="req-conflict",
    )

    service.apply_event(
        events.TurnStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-original",
        )
    )
    service.apply_event(
        events.AgentCompleted(
            conversation_id=state.conversation.id,
            turn_id="turn-original",
            success=True,
        )
    )
    assert state.active_turn_id is None

    with pytest.raises(
        RuntimeError,
        match="command receipt turn does not match the already-bound user message",
    ):
        service.bind_receipt(
            CommandReceipt(
                request_id="req-conflict",
                turn_id="turn-conflict",
                detail="turn accepted",
            )
        )

    assert user.turn_id == "turn-original"
    assert state.active_turn_id is None
    assert service.has_pending_request(state.conversation.id) is False
