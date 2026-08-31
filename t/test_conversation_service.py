from __future__ import annotations

import time

from zara.database import DatabaseManager
from zara.desktop.conversation import (
    ConversationService,
    ConversationStore,
    MessageRole,
    MessageStatus,
)
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend, RuntimeTurnResult
from zara.runtime.commands import CommandReceipt, SubmitTurn
from zara.runtime.host import RuntimeHost


def make_service(tmp_path) -> ConversationService:
    return ConversationService(ConversationStore(DatabaseManager(tmp_path / "conversation.db")))


def test_streaming_events_append_incrementally_and_persist_final_text(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation()
    command = SubmitTurn(text="hello", conversation_id=state.conversation.id, request_id="req-1")
    user, _ = service.add_user_message(state.conversation.id, "hello", request_id=command.request_id)

    service.bind_receipt(CommandReceipt(request_id="req-1", turn_id="turn-1"))
    assert user.turn_id == "turn-1"

    service.apply_event(events.AssistantStarted(conversation_id=state.conversation.id, turn_id="turn-1"))
    first = service.apply_event(
        events.AssistantDelta(
            conversation_id=state.conversation.id,
            turn_id="turn-1",
            text="Hello ",
        )
    )
    second = service.apply_event(
        events.AssistantDelta(
            conversation_id=state.conversation.id,
            turn_id="turn-1",
            text="world",
        )
    )
    service.apply_event(
        events.AssistantComplete(
            conversation_id=state.conversation.id,
            turn_id="turn-1",
            text="Hello world",
        )
    )

    assistant = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-1")
    assert assistant is not None
    assert first.message_ids == second.message_ids == (assistant.id,)
    assert assistant.content == "Hello world"
    assert assistant.status is MessageStatus.COMPLETE
    assert state.active_turn_id is None

    reloaded = service.store.load_state(state.conversation.id)
    assert reloaded.latest_message(role=MessageRole.ASSISTANT).content == "Hello world"


def test_buffered_response_is_one_complete_update_not_fake_streaming(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Buffered")

    update = service.apply_event(
        events.ResponseText(
            conversation_id=state.conversation.id,
            turn_id="turn-buffered",
            text="complete buffered answer",
        )
    )
    assistant = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-buffered")

    assert update.message_ids == (assistant.id,)
    assert assistant.content == "complete buffered answer"
    assert assistant.status is MessageStatus.COMPLETE
    assert len([m for m in state.messages if m.role is MessageRole.ASSISTANT]) == 1

    service.apply_event(
        events.ResponseText(
            conversation_id=state.conversation.id,
            turn_id="turn-buffered",
            text="complete buffered answer",
        )
    )
    assert assistant.content == "complete buffered answer"


def test_failures_cancellation_tools_and_provider_are_structured(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Events")

    service.apply_event(
        events.ProviderChanged(
            conversation_id=state.conversation.id,
            provider="openrouter",
            model="test-model",
        )
    )
    assert state.provider == "openrouter"
    assert state.model == "test-model"

    service.apply_event(
        events.AssistantStarted(conversation_id=state.conversation.id, turn_id="turn-fail")
    )
    service.apply_event(
        events.AssistantFailed(
            conversation_id=state.conversation.id,
            turn_id="turn-fail",
            reason="provider unavailable",
        )
    )
    failed = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-fail")
    assert failed.status is MessageStatus.ERROR
    assert failed.error == "provider unavailable"

    service.apply_event(
        events.ToolStarted(
            conversation_id=state.conversation.id,
            turn_id="turn-tool",
            tool_run_id="tool-1",
            tool_name="search",
        )
    )
    service.apply_event(
        events.ToolCompleted(
            conversation_id=state.conversation.id,
            turn_id="turn-tool",
            tool_run_id="tool-1",
            tool_name="search",
        )
    )
    tool = next(message for message in state.messages if message.tool_run_id == "tool-1")
    assert tool.role is MessageRole.TOOL
    assert tool.content == "search: completed"
    assert tool.status is MessageStatus.COMPLETE

    service.apply_event(
        events.AssistantStarted(conversation_id=state.conversation.id, turn_id="turn-cancel")
    )
    service.apply_event(
        events.TurnCancelled(
            conversation_id=state.conversation.id,
            turn_id="turn-cancel",
            reason="user cancelled",
        )
    )
    cancelled = state.latest_message(role=MessageRole.ASSISTANT, turn_id="turn-cancel")
    assert cancelled.status is MessageStatus.CANCELLED
    assert cancelled.error == "user cancelled"


def test_command_failure_becomes_compact_durable_error(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation()
    service.add_user_message(state.conversation.id, "hello", request_id="failed-request")

    update = service.mark_command_failed("failed-request", "runtime offline")
    error = state.message_by_id(update.message_ids[0])

    assert error.role is MessageRole.SYSTEM
    assert error.status is MessageStatus.ERROR
    assert error.content == "Message could not be sent."
    assert error.error == "runtime offline"
    assert service.has_pending_request(state.conversation.id) is False


class BufferedBackend(RuntimeBackend):
    async def submit_turn(self, text, *, turn_id, conversation_id=None, context_ids=(), latency_trace=None):
        return RuntimeTurnResult(response=f"echo: {text}")


def test_runtime_host_buffered_integration_reduces_into_conversation(tmp_path):
    service = make_service(tmp_path)
    state = service.create_conversation("Runtime integration")
    published = []

    def publish(event):
        published.append(event)
        return None

    host = RuntimeHost(lambda: BufferedBackend(), publisher=publish)
    try:
        host.start().result(timeout=2)
        command = SubmitTurn(text="ping", conversation_id=state.conversation.id)
        service.add_user_message(state.conversation.id, "ping", request_id=command.request_id)
        receipt = host.submit(command).result(timeout=2)
        service.bind_receipt(receipt)

        deadline = time.monotonic() + 2
        while time.monotonic() < deadline:
            if any(isinstance(event, events.OutputReady) for event in published):
                break
            time.sleep(0.01)
        assert any(isinstance(event, events.OutputReady) for event in published)

        for event in published:
            service.apply_event(event)

        assistant = state.latest_message(role=MessageRole.ASSISTANT, turn_id=receipt.turn_id)
        assert assistant is not None
        assert assistant.content == "echo: ping"
        assert assistant.status is MessageStatus.COMPLETE
    finally:
        host.shutdown("test complete").result(timeout=2)
        host.join(timeout=2)
