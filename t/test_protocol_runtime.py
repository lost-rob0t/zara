from __future__ import annotations

import pytest

from zara.protocol import ProtocolMessage, encode_message
from zara.protocol_runtime import (
    RuntimeCodecError,
    command_from_message,
    protocol_error_from_exception,
    runtime_event_to_message,
)
from zara.runtime import events
from zara.runtime.bridge import EventEnvelope
from zara.runtime.commands import ApproveTool, CancelTurn, RejectTool, SubmitTurn


def protocol_message(message_type: str, **overrides) -> ProtocolMessage:
    values = {
        "type": message_type,
        "id": "req-123",
        "timestamp_ns": 100,
        "payload_count": 0,
    }
    values.update(overrides)
    return ProtocolMessage(**values)


def envelope(event: events.RuntimeEvent, *, sequence: int = 7) -> EventEnvelope:
    return EventEnvelope(sequence=sequence, occurred_at=12.5, event=event)


def test_turn_submit_maps_to_existing_runtime_command_and_preserves_ids():
    message = protocol_message(
        "turn.submit",
        conversation_id="conv-9",
        trace_id="trace-4",
        flags={"idempotent": True},
        body={"text": "hello", "context_ids": ["ctx-a", "ctx-b"]},
    )

    command = command_from_message(message)

    assert command == SubmitTurn(
        request_id="req-123",
        text="hello",
        conversation_id="conv-9",
        context_ids=("ctx-a", "ctx-b"),
    )


def test_turn_cancel_preserves_canonical_turn_id():
    command = command_from_message(
        protocol_message("turn.cancel", turn_id="turn-canonical")
    )
    assert command == CancelTurn(
        request_id="req-123",
        turn_id="turn-canonical",
    )


@pytest.mark.parametrize(
    ("message_type", "body", "expected"),
    [
        (
            "tool.approve",
            {"tool_run_id": "tool-call-1"},
            ApproveTool(request_id="req-123", tool_run_id="tool-call-1"),
        ),
        (
            "tool.reject",
            {"tool_run_id": "tool-call-1", "reason": "not now"},
            RejectTool(
                request_id="req-123",
                tool_run_id="tool-call-1",
                reason="not now",
            ),
        ),
    ],
)
def test_tool_decisions_map_to_existing_runtime_commands(message_type, body, expected):
    command = command_from_message(
        protocol_message(message_type, session_id="session-1", body=body)
    )

    assert command == expected


@pytest.mark.parametrize(
    "message",
    [
        protocol_message("ping"),
        protocol_message("turn.submit", body=None),
        protocol_message("turn.submit", body={"text": ""}),
        protocol_message("turn.submit", body={"text": "ok", "context_ids": "ctx"}),
        protocol_message("turn.submit", body={"text": "ok", "context_ids": ["ctx", 3]}),
        protocol_message("turn.cancel"),
    ],
)
def test_runtime_command_codec_fails_closed_on_unsupported_or_invalid_messages(message):
    with pytest.raises(RuntimeCodecError):
        command_from_message(message)


@pytest.mark.parametrize(
    ("event", "expected_type", "expected_body"),
    [
        (events.TurnStarted(turn_id="t1", conversation_id="c1"), "turn.started", {}),
        (
            events.TurnCancelled(turn_id="t1", conversation_id="c1", reason="user"),
            "turn.cancelled",
            {"reason": "user"},
        ),
        (
            events.AgentCompleted(turn_id="t1", conversation_id="c1", success=True),
            "turn.completed",
            {"success": True},
        ),
        (events.AssistantStarted(turn_id="t1", conversation_id="c1"), "assistant.started", {}),
        (
            events.AssistantDelta(turn_id="t1", conversation_id="c1", text="delta"),
            "assistant.delta",
            {"text": "delta"},
        ),
        (
            events.AssistantComplete(
                turn_id="t1",
                conversation_id="c1",
                text="done",
                success=True,
            ),
            "assistant.completed",
            {"text": "done", "success": True},
        ),
        (
            events.ResponseText(
                turn_id="t1",
                conversation_id="c1",
                text="answer",
                truncated=False,
            ),
            "assistant.response",
            {"text": "answer", "truncated": False},
        ),
        (
            events.RuntimeError(
                turn_id="t1",
                conversation_id="c1",
                reason="backend unavailable",
                fatal=False,
            ),
            "runtime.error",
            {"reason": "backend unavailable", "fatal": False},
        ),
        (
            events.RuntimeStopped(reason="shutdown"),
            "runtime.stopped",
            {"reason": "shutdown"},
        ),
    ],
)
def test_runtime_events_have_explicit_allowlisted_wire_mapping(event, expected_type, expected_body):
    message = runtime_event_to_message(
        envelope(event),
        message_id="evt-1",
        timestamp_ns=999,
    )

    assert message.type == expected_type
    assert message.id == "evt-1"
    assert message.timestamp_ns == 999
    assert message.payload_count == 0
    assert message.turn_id == event.turn_id
    assert message.conversation_id == event.conversation_id
    assert message.seq == 7
    assert message.body == expected_body

    frames = encode_message(message)
    assert frames[0] == b"ZARA/1"
    assert len(frames) == 2


@pytest.mark.parametrize(
    ("event", "expected_type", "expected_body"),
    [
        (
            events.ToolQueued(
                turn_id="turn-1",
                conversation_id="conversation-1",
                tool_run_id="tool-call-1",
                tool_name="reviewed_effect",
            ),
            "tool.queued",
            {"tool_run_id": "tool-call-1", "tool_name": "reviewed_effect"},
        ),
        (
            events.ToolWaitingForUser(
                turn_id="turn-1",
                conversation_id="conversation-1",
                tool_run_id="tool-call-1",
                tool_name="reviewed_effect",
                prompt="Approve reviewed_effect?",
            ),
            "tool.waiting",
            {
                "tool_run_id": "tool-call-1",
                "tool_name": "reviewed_effect",
                "kind": "approval",
                "prompt": "Approve reviewed_effect?",
            },
        ),
        (
            events.ToolStarted(
                turn_id="turn-1",
                conversation_id="conversation-1",
                tool_run_id="tool-call-1",
                tool_name="reviewed_effect",
            ),
            "tool.started",
            {"tool_run_id": "tool-call-1", "tool_name": "reviewed_effect"},
        ),
        (
            events.ToolCompleted(
                turn_id="turn-1",
                conversation_id="conversation-1",
                tool_run_id="tool-call-1",
                tool_name="reviewed_effect",
                success=True,
            ),
            "tool.completed",
            {
                "tool_run_id": "tool-call-1",
                "tool_name": "reviewed_effect",
                "success": True,
            },
        ),
        (
            events.ToolFailed(
                turn_id="turn-1",
                conversation_id="conversation-1",
                tool_run_id="tool-call-1",
                tool_name="reviewed_effect",
                reason="tool execution failed",
            ),
            "tool.failed",
            {
                "tool_run_id": "tool-call-1",
                "tool_name": "reviewed_effect",
                "reason": "tool execution failed",
            },
        ),
        (
            events.ToolCancelled(
                turn_id="turn-1",
                conversation_id="conversation-1",
                tool_run_id="tool-call-1",
                tool_name="reviewed_effect",
                reason="tool rejected",
            ),
            "tool.cancelled",
            {
                "tool_run_id": "tool-call-1",
                "tool_name": "reviewed_effect",
                "reason": "tool rejected",
            },
        ),
    ],
)
def test_tool_events_have_closed_wire_mapping(event, expected_type, expected_body):
    message = runtime_event_to_message(
        envelope(event, sequence=12),
        message_id="tool-event",
        timestamp_ns=1001,
    )

    assert message.type == expected_type
    assert message.turn_id == "turn-1"
    assert message.conversation_id == "conversation-1"
    assert message.seq == 12
    assert message.body == expected_body
    assert "arguments" not in repr(message)


@pytest.mark.parametrize(
    "event",
    [
        events.ToolWaitingForUser(
            turn_id="turn-1",
            tool_run_id="tool call with spaces",
            tool_name="reviewed_effect",
            prompt="Approve reviewed_effect?",
        ),
        events.ToolWaitingForUser(
            turn_id="turn-1",
            tool_run_id="tool-call-1",
            tool_name="bad tool name",
            prompt="Approve it?",
        ),
        events.ToolWaitingForUser(
            turn_id="turn-1",
            tool_run_id="tool-call-1",
            tool_name="reviewed_effect",
            prompt="unsafe\nprompt",
        ),
        events.ToolCompleted(
            turn_id="turn-1",
            tool_run_id="tool-call-1",
            tool_name="reviewed_effect",
            success="yes",
        ),
        events.ToolFailed(
            turn_id="turn-1",
            tool_run_id="tool-call-1",
            tool_name="reviewed_effect",
            reason="private\nexception",
        ),
    ],
)
def test_invalid_tool_runtime_events_fail_before_transport(event):
    with pytest.raises(RuntimeCodecError):
        runtime_event_to_message(
            envelope(event),
            message_id="tool-event",
            timestamp_ns=1001,
        )


@pytest.mark.parametrize(
    ("event", "expected_type", "expected_body"),
    [
        (
            events.VoiceSpeechStarted(
                conversation_id="voice-conversation",
                stream_id="mic-1",
                trace_id="trace-voice",
                pre_speech_samples=1024,
            ),
            "voice.speech.started",
            {"pre_speech_samples": 1024},
        ),
        (
            events.VoiceTranscriptPartial(
                conversation_id="voice-conversation",
                stream_id="mic-1",
                trace_id="trace-voice",
                text="hello wor",
            ),
            "voice.transcript.partial",
            {"text": "hello wor"},
        ),
        (
            events.VoiceSpeechEnded(
                conversation_id="voice-conversation",
                stream_id="mic-1",
                trace_id="trace-voice",
                reason="silence",
            ),
            "voice.speech.ended",
            {"reason": "silence"},
        ),
        (
            events.VoiceTranscriptFinal(
                conversation_id="voice-conversation",
                stream_id="mic-1",
                trace_id="trace-voice",
                text="hello world",
                provider="provider-secret-must-not-cross-wire",
            ),
            "voice.transcript.final",
            {"text": "hello world"},
        ),
    ],
)
def test_visible_stt_events_have_closed_wire_mapping_without_fake_turn_or_provider(
    event,
    expected_type,
    expected_body,
):
    message = runtime_event_to_message(
        envelope(event, sequence=17),
        message_id="voice-event",
        timestamp_ns=1003,
    )

    assert message.type == expected_type
    assert message.turn_id is None
    assert message.conversation_id == "voice-conversation"
    assert message.stream_id == "mic-1"
    assert message.trace_id == "trace-voice"
    assert message.seq == 17
    assert message.body == expected_body
    assert "provider-secret-must-not-cross-wire" not in repr(message)

    frames = encode_message(message)
    assert frames[0] == b"ZARA/1"
    assert len(frames) == 2


def test_runtime_event_label_and_python_type_are_not_implicitly_serialized():
    event = events.AssistantDelta(
        turn_id="t1",
        conversation_id="c1",
        label="provider-secret-label",
        text="safe",
    )

    message = runtime_event_to_message(
        envelope(event),
        message_id="evt-2",
        timestamp_ns=1000,
    )

    assert message.body == {"text": "safe"}
    assert "provider-secret-label" not in repr(message)
    assert "AssistantDelta" not in repr(message.body)


def test_unsupported_runtime_event_fails_closed_without_reflection_fallback():
    with pytest.raises(RuntimeCodecError, match="unsupported runtime event"):
        runtime_event_to_message(
            envelope(events.RuntimeStarted()),
            message_id="evt-3",
            timestamp_ns=1001,
        )


def test_internal_exception_wire_error_is_sanitized():
    error = RuntimeError(
        "secret token=abc123 route=\\xff\\x00 path=/home/unseen/private/config.toml"
    )

    message = protocol_error_from_exception(
        error,
        message_id="err-1",
        reply_to="req-123",
        timestamp_ns=1002,
    )

    assert message.type == "protocol.error"
    assert message.reply_to == "req-123"
    assert message.body == {
        "code": "internal_error",
        "message": "internal server error",
        "retryable": False,
    }
    serialized = repr(message)
    assert "abc123" not in serialized
    assert "/home/unseen" not in serialized
    assert "\\xff" not in serialized
