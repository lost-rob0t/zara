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
    return ConversationService(ConversationStore(DatabaseManager(tmp_path / "conversation.db")))


def test_message_title_request_search_and_rename_boundaries(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation()

    with pytest.raises(ValueError, match="message text must not be empty"):
        service.add_user_message(state.conversation.id, "   ", request_id="empty")

    long_text = "  one   two  " + "x" * 80
    first, update = service.add_user_message(
        state.conversation.id,
        long_text,
        request_id="req-first",
    )
    assert first.content == long_text.strip()
    assert update.metadata_changed is True
    assert state.conversation.title.endswith("…")
    assert len(state.conversation.title) == 58
    assert service.owns_request("req-first") is True
    assert service.has_pending_request(state.conversation.id) is True

    original_title = state.conversation.title
    _, second_update = service.add_user_message(
        state.conversation.id,
        "second message",
        request_id="req-second",
    )
    assert second_update.metadata_changed is False
    assert state.conversation.title == original_title

    renamed = service.rename_conversation(state.conversation.id, "  Renamed chat  ")
    assert renamed.metadata_changed is True
    assert state.conversation.title == "Renamed chat"
    assert service.list_conversations("second")

    with pytest.raises(ValueError, match="conversation title must not be empty"):
        service.rename_conversation(state.conversation.id, "  ")


def test_receipt_missing_empty_matching_and_conflicting_paths(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Receipts")

    assert service.bind_receipt(CommandReceipt(request_id="missing", turn_id="turn-x")) is None

    first, _ = service.add_user_message(
        state.conversation.id,
        "first",
        request_id="req-empty-turn",
    )
    empty_turn_update = service.bind_receipt(
        CommandReceipt(request_id="req-empty-turn", turn_id=None)
    )
    assert empty_turn_update.message_ids == ()
    assert empty_turn_update.active_turn_changed is False
    assert first.turn_id is None

    second, _ = service.add_user_message(
        state.conversation.id,
        "second",
        request_id="req-bound",
    )
    service.apply_event(
        events.TurnStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-bound",
        )
    )
    assert second.turn_id == "turn-bound"
    service.apply_event(
        events.AgentCompleted(
            conversation_id=state.conversation.id,
            turn_id="turn-bound",
        )
    )
    assert state.active_turn_id is None

    matching = service.bind_receipt(
        CommandReceipt(request_id="req-bound", turn_id="turn-bound")
    )
    assert matching.message_ids == ()
    assert matching.active_turn_changed is False
    assert state.active_turn_id is None

    third, _ = service.add_user_message(
        state.conversation.id,
        "third",
        request_id="req-conflict",
    )
    service.apply_event(
        events.TurnStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-real",
        )
    )
    assert third.turn_id == "turn-real"
    with pytest.raises(RuntimeError, match="does not match"):
        service.bind_receipt(
            CommandReceipt(request_id="req-conflict", turn_id="turn-wrong")
        )


def test_events_without_resolvable_conversation_fail_closed(tmp_path):
    service = make_service(tmp_path)

    assert service.apply_event(events.RuntimeIdle()) is None
    assert service.apply_event(events.ProviderChanged(provider="local", model="none")) is None
    assert service.apply_event(
        events.TurnStarted(conversation_id="missing", turn_id="turn-missing")
    ) is None

    state = service.create_conversation("Provider")
    update = service.apply_event(
        events.ProviderChanged(
            conversation_id=state.conversation.id,
            provider="local",
            model="symbolic",
        )
    )
    assert update.metadata_changed is True
    assert state.provider == "local"
    assert state.model == "symbolic"


def test_assistant_completion_tail_failure_and_response_idempotence(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Assistant")

    service.apply_event(
        events.AssistantStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-complete",
        )
    )
    service.apply_event(
        events.AssistantDelta(
            conversation_id=state.conversation.id,
            turn_id="turn-complete",
            text="hello ",
        )
    )
    service.apply_event(
        events.AssistantComplete(
            conversation_id=state.conversation.id,
            turn_id="turn-complete",
            text="world",
        )
    )
    complete = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-complete")
    assert complete.content == "hello world"
    assert complete.status is MessageStatus.COMPLETE

    failed_update = service.apply_event(
        events.AssistantComplete(
            conversation_id=state.conversation.id,
            turn_id="turn-failed-complete",
            success=False,
        )
    )
    failed = state.message_by_id(failed_update.message_ids[0])
    assert failed.status is MessageStatus.ERROR
    assert failed.error == "assistant generation failed"

    service.apply_event(
        events.ResponseText(
            conversation_id=state.conversation.id,
            turn_id="turn-response",
            text="prefix",
        )
    )
    service.apply_event(
        events.ResponseText(
            conversation_id=state.conversation.id,
            turn_id="turn-response",
            text=" tail",
        )
    )
    response = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-response")
    assert response.content == "prefix tail"
    service.apply_event(
        events.ResponseText(
            conversation_id=state.conversation.id,
            turn_id="turn-response",
            text="prefix tail",
        )
    )
    assert response.content == "prefix tail"


def test_failure_events_use_default_reason_and_content(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Failures")

    update = service.apply_event(
        events.AgentFailed(
            conversation_id=state.conversation.id,
            turn_id="turn-agent-failed",
        )
    )
    failed = state.message_by_id(update.message_ids[0])
    assert failed.status is MessageStatus.ERROR
    assert failed.error == "assistant failed"
    assert failed.content == "Zara could not complete this response."

    runtime_update = service.apply_event(
        events.RuntimeError(
            conversation_id=state.conversation.id,
            turn_id="turn-runtime-error",
        )
    )
    runtime_error = state.message_by_id(runtime_update.message_ids[0])
    assert runtime_error.role is MessageRole.SYSTEM
    assert runtime_error.content == "Runtime error"
    assert runtime_error.error == "runtime error"

    assert service.apply_event(
        events.RuntimeError(conversation_id=state.conversation.id, reason="global")
    ) is None


def test_cancellation_is_durable_and_fences_every_late_turn_event(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Cancel")
    user, _ = service.add_user_message(
        state.conversation.id,
        "cancel this",
        request_id="req-cancel",
    )
    service.bind_receipt(CommandReceipt(request_id="req-cancel", turn_id="turn-cancel"))
    assert user.turn_id == "turn-cancel"

    cancelled = service.apply_event(
        events.TurnCancelled(
            conversation_id=state.conversation.id,
            turn_id="turn-cancel",
        )
    )
    assert cancelled.message_ids == (user.id,)
    assert user.status is MessageStatus.CANCELLED
    assert user.error == "turn cancelled"

    assert service.apply_event(
        events.AssistantDelta(
            conversation_id=state.conversation.id,
            turn_id="turn-cancel",
            text="stale text",
        )
    ) is None
    assert service.apply_event(
        events.ToolStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-cancel",
            tool_run_id="stale-tool",
            tool_name="danger",
        )
    ) is None
    assert state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-cancel") is None
    assert not any(message.tool_run_id == "stale-tool" for message in state.messages)

    reloaded = ConversationService(service.store).get_state(state.conversation.id)
    assert reloaded.latest_message(role=MessageRole.USER, turn_id="turn-cancel").status is MessageStatus.CANCELLED


def test_cancellation_only_mutates_live_assistant_and_tool_messages(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Cancel live")

    service.apply_event(
        events.AssistantStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-live",
        )
    )
    service.apply_event(
        events.ToolStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-live",
            tool_run_id="tool-live",
            tool_name="search",
        )
    )
    service.apply_event(
        events.ToolCompleted(
            conversation_id=state.conversation.id,
            turn_id="turn-live",
            tool_run_id="tool-done",
            tool_name="done",
        )
    )

    update = service.apply_event(
        events.TurnCancelled(
            conversation_id=state.conversation.id,
            turn_id="turn-live",
            reason="stop now",
        )
    )
    assistant = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-live")
    live_tool = next(message for message in state.messages if message.tool_run_id == "tool-live")
    done_tool = next(message for message in state.messages if message.tool_run_id == "tool-done")
    assert assistant.status is MessageStatus.CANCELLED
    assert live_tool.status is MessageStatus.CANCELLED
    assert done_tool.status is MessageStatus.COMPLETE
    assert set(update.message_ids) == {assistant.id, live_tool.id}


def test_tool_event_reducer_covers_queue_progress_approval_failure_and_cancel(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Tools")
    common = {
        "conversation_id": state.conversation.id,
        "turn_id": "turn-tool",
        "tool_run_id": "tool-1",
        "tool_name": "search",
    }

    queued = service.apply_event(events.ToolQueued(**common))
    tool = state.message_by_id(queued.message_ids[0])
    assert tool.content == "search: queued"
    assert tool.status is MessageStatus.PENDING

    service.apply_event(events.ToolProgress(**common, message="halfway", progress=0.5))
    assert tool.content == "search: halfway"
    assert tool.status is MessageStatus.STREAMING

    service.apply_event(events.ToolProgress(**common, message=""))
    assert tool.content == "search: running"

    service.apply_event(events.ToolWaitingForUser(**common, prompt="approve"))
    assert tool.content == "search: waiting for approval"
    assert tool.status is MessageStatus.PENDING

    service.apply_event(events.ToolCompleted(**common, success=False))
    assert tool.content == "search: completed"
    assert tool.status is MessageStatus.ERROR

    service.apply_event(events.ToolFailed(**common, reason="boom"))
    assert tool.content == "search: failed"
    assert tool.status is MessageStatus.ERROR
    assert tool.error == "boom"

    service.apply_event(events.ToolCancelled(**common, reason="user stop"))
    assert tool.content == "search: cancelled"
    assert tool.status is MessageStatus.CANCELLED
    assert tool.error == "user stop"

    unnamed = service.apply_event(
        events.ToolStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-unnamed",
        )
    )
    unnamed_tool = state.message_by_id(unnamed.message_ids[0])
    assert unnamed_tool.content == "tool: running"


def test_terminal_completion_only_clears_matching_active_turn(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Terminal")
    state.active_turn_id = "turn-active"

    assert service.apply_event(
        events.OutputReady(
            conversation_id=state.conversation.id,
            turn_id="turn-other",
        )
    ) is None
    assert state.active_turn_id == "turn-active"

    update = service.apply_event(
        events.OutputReady(
            conversation_id=state.conversation.id,
            turn_id="turn-active",
        )
    )
    assert update.active_turn_changed is True
    assert state.active_turn_id is None

    assert service.apply_event(
        events.OutputSeen(
            conversation_id=state.conversation.id,
            turn_id="turn-active",
        )
    ) is None


def test_command_failure_unknown_and_default_reason_paths(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Command failures")

    assert service.mark_command_failed("unknown", "offline") is None

    service.add_user_message(
        state.conversation.id,
        "send me",
        request_id="req-fail-default",
    )
    update = service.mark_command_failed("req-fail-default", "")
    message = state.message_by_id(update.message_ids[0])
    assert message.status is MessageStatus.ERROR
    assert message.error == "runtime rejected the turn"
    assert state.active_turn_id is None
