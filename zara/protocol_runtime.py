"""Explicit adapters between ZARA/1 messages and Zara runtime contracts.

No reflection-based serialization lives here. Every runtime command/event that
crosses the wire is deliberately allowlisted so provider internals, labels,
paths, exception text, and future dataclass fields do not become protocol by
accident.
"""

from __future__ import annotations

import re
from typing import Any

from zara.protocol import ProtocolMessage
from zara.runtime import events
from zara.runtime.bridge import EventEnvelope
from zara.runtime.commands import (
    ApproveTool,
    CancelTurn,
    RejectTool,
    RuntimeCommand,
    SubmitTurn,
)


class RuntimeCodecError(ValueError):
    """A protocol message/event cannot be mapped to the runtime vocabulary."""


_TOOL_NAME_RE = re.compile(r"^[A-Za-z0-9_.:-]+$")


def _wire_tool_token(name: str, value: Any, *, max_bytes: int) -> str:
    if not isinstance(value, str) or not value:
        raise RuntimeCodecError(f"wire tool event {name} must be a non-empty string")
    try:
        encoded = value.encode("ascii")
    except UnicodeEncodeError as error:
        raise RuntimeCodecError(f"wire tool event {name} must be printable ASCII") from error
    if len(encoded) > max_bytes:
        raise RuntimeCodecError(f"wire tool event {name} exceeds byte limit")
    if any(byte < 0x21 or byte > 0x7E for byte in encoded):
        raise RuntimeCodecError(
            f"wire tool event {name} must be printable ASCII without whitespace"
        )
    return value


def _wire_tool_text(name: str, value: Any) -> str:
    if not isinstance(value, str):
        raise RuntimeCodecError(f"wire tool event {name} must be a string")
    if len(value.encode("utf-8")) > 256:
        raise RuntimeCodecError(f"wire tool event {name} exceeds byte limit")
    if any(ord(character) < 0x20 or ord(character) == 0x7F for character in value):
        raise RuntimeCodecError(f"wire tool event {name} contains control characters")
    return value


def _closed_body(
    message: ProtocolMessage,
    *,
    allowed: frozenset[str],
    required: frozenset[str] = frozenset(),
) -> dict[str, Any]:
    body = message.body
    if body is None:
        if required:
            raise RuntimeCodecError(f"{message.type} requires a body")
        return {}
    if not isinstance(body, dict):
        raise RuntimeCodecError(f"{message.type} body must be an object")
    unknown = set(body) - allowed
    if unknown:
        raise RuntimeCodecError(f"{message.type} body contains unknown fields")
    missing = required - set(body)
    if missing:
        raise RuntimeCodecError(f"{message.type} body is missing required fields")
    return body


def _context_ids(value: Any) -> tuple[str, ...]:
    if value is None:
        return ()
    if not isinstance(value, list):
        raise RuntimeCodecError("turn.submit context_ids must be a list")
    result: list[str] = []
    for item in value:
        if not isinstance(item, str) or not item or item.strip() != item:
            raise RuntimeCodecError("turn.submit context_ids must contain non-empty canonical strings")
        result.append(item)
    return tuple(result)


def command_from_message(message: ProtocolMessage) -> RuntimeCommand:
    """Map one executable ZARA/1 request onto the existing runtime command API."""

    if not isinstance(message, ProtocolMessage):
        raise RuntimeCodecError("runtime command codec requires ProtocolMessage")

    if message.type == "turn.submit":
        body = _closed_body(
            message,
            allowed=frozenset({"text", "context_ids"}),
            required=frozenset({"text"}),
        )
        text = body["text"]
        if not isinstance(text, str) or not text.strip():
            raise RuntimeCodecError("turn.submit text must be a non-empty string")
        return SubmitTurn(
            request_id=message.id,
            text=text,
            conversation_id=message.conversation_id,
            context_ids=_context_ids(body.get("context_ids")),
        )

    if message.type == "turn.cancel":
        _closed_body(message, allowed=frozenset())
        if not isinstance(message.turn_id, str) or not message.turn_id:
            raise RuntimeCodecError("turn.cancel requires turn_id")
        return CancelTurn(request_id=message.id, turn_id=message.turn_id)

    if message.type in {"tool.approve", "tool.reject"}:
        allowed = frozenset({"tool_run_id"})
        if message.type == "tool.reject":
            allowed = frozenset({"tool_run_id", "reason"})
        body = _closed_body(
            message,
            allowed=allowed,
            required=frozenset({"tool_run_id"}),
        )
        tool_run_id = body["tool_run_id"]
        if not isinstance(tool_run_id, str) or not tool_run_id:
            raise RuntimeCodecError(f"{message.type} tool_run_id must be a non-empty string")
        if message.type == "tool.approve":
            return ApproveTool(request_id=message.id, tool_run_id=tool_run_id)
        reason = body.get("reason", "")
        if not isinstance(reason, str):
            raise RuntimeCodecError("tool.reject reason must be a string")
        return RejectTool(
            request_id=message.id,
            tool_run_id=tool_run_id,
            reason=reason,
        )

    raise RuntimeCodecError("unsupported runtime command message")


def runtime_event_to_message(
    envelope: EventEnvelope,
    *,
    message_id: str,
    timestamp_ns: int,
) -> ProtocolMessage:
    """Encode one explicitly supported RuntimeEvent as a ZARA/1 event."""

    if not isinstance(envelope, EventEnvelope):
        raise RuntimeCodecError("runtime event codec requires EventEnvelope")

    event = envelope.event
    message_type: str
    body: dict[str, Any]
    stream_id = None
    trace_id = None

    if type(event) is events.TurnStarted:
        message_type, body = "turn.started", {}
    elif type(event) is events.TurnCancelled:
        message_type, body = "turn.cancelled", {"reason": event.reason}
    elif type(event) is events.AgentCompleted:
        message_type, body = "turn.completed", {"success": event.success}
    elif type(event) is events.AssistantStarted:
        message_type, body = "assistant.started", {}
    elif type(event) is events.AssistantDelta:
        message_type, body = "assistant.delta", {"text": event.text}
    elif type(event) is events.AssistantComplete:
        message_type, body = "assistant.completed", {
            "text": event.text,
            "success": event.success,
        }
    elif type(event) is events.ResponseText:
        message_type, body = "assistant.response", {
            "text": event.text,
            "truncated": event.truncated,
        }
    elif type(event) is events.VoiceSpeechStarted:
        message_type, body = "voice.speech.started", {
            "pre_speech_samples": event.pre_speech_samples,
        }
        stream_id, trace_id = event.stream_id, event.trace_id
    elif type(event) is events.VoiceTranscriptPartial:
        message_type, body = "voice.transcript.partial", {"text": event.text}
        stream_id, trace_id = event.stream_id, event.trace_id
    elif type(event) is events.VoiceSpeechEnded:
        message_type, body = "voice.speech.ended", {"reason": event.reason}
        stream_id, trace_id = event.stream_id, event.trace_id
    elif type(event) is events.VoiceTranscriptFinal:
        message_type, body = "voice.transcript.final", {"text": event.text}
        stream_id, trace_id = event.stream_id, event.trace_id
    elif type(event) in {
        events.ToolQueued,
        events.ToolStarted,
        events.ToolCompleted,
        events.ToolFailed,
        events.ToolCancelled,
        events.ToolWaitingForUser,
    }:
        if not event.turn_id:
            raise RuntimeCodecError("wire tool event requires turn and tool correlation")
        tool_run_id = _wire_tool_token("tool_run_id", event.tool_run_id, max_bytes=256)
        tool_name = _wire_tool_token("tool_name", event.tool_name, max_bytes=128)
        if _TOOL_NAME_RE.fullmatch(tool_name) is None:
            raise RuntimeCodecError("wire tool event tool_name is invalid")
        body = {
            "tool_run_id": tool_run_id,
            "tool_name": tool_name,
        }
        if type(event) is events.ToolQueued:
            message_type = "tool.queued"
        elif type(event) is events.ToolStarted:
            message_type = "tool.started"
        elif type(event) is events.ToolCompleted:
            message_type = "tool.completed"
            if type(event.success) is not bool:
                raise RuntimeCodecError("wire tool event success must be boolean")
            body["success"] = event.success
        elif type(event) is events.ToolFailed:
            message_type = "tool.failed"
            body["reason"] = _wire_tool_text("reason", event.reason)
        elif type(event) is events.ToolCancelled:
            message_type = "tool.cancelled"
            body["reason"] = _wire_tool_text("reason", event.reason)
        else:
            message_type = "tool.waiting"
            if event.kind != "approval":
                raise RuntimeCodecError("wire tool event kind must be approval")
            body.update({"kind": event.kind, "prompt": _wire_tool_text("prompt", event.prompt)})
    elif type(event) is events.RuntimeError:
        message_type, body = "runtime.error", {
            "reason": event.reason,
            "fatal": event.fatal,
        }
    elif type(event) is events.RuntimeStopped:
        message_type, body = "runtime.stopped", {"reason": event.reason}
    else:
        raise RuntimeCodecError("unsupported runtime event")

    return ProtocolMessage(
        type=message_type,
        id=message_id,
        timestamp_ns=timestamp_ns,
        payload_count=0,
        conversation_id=event.conversation_id,
        turn_id=event.turn_id,
        stream_id=stream_id,
        trace_id=trace_id,
        seq=envelope.sequence,
        body=body,
    )


def protocol_error_from_exception(
    error: BaseException,
    *,
    message_id: str,
    timestamp_ns: int,
    reply_to: str | None = None,
) -> ProtocolMessage:
    """Convert an internal exception to a deliberately content-free wire error."""

    if not isinstance(error, BaseException):
        raise TypeError("error must be an exception")
    return ProtocolMessage(
        type="protocol.error",
        id=message_id,
        reply_to=reply_to,
        timestamp_ns=timestamp_ns,
        payload_count=0,
        body={
            "code": "internal_error",
            "message": "internal server error",
            "retryable": False,
        },
    )


__all__ = [
    "RuntimeCodecError",
    "command_from_message",
    "protocol_error_from_exception",
    "runtime_event_to_message",
]
