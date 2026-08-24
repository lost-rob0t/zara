"""Explicit adapters between ZARA/1 messages and Zara runtime contracts.

No reflection-based serialization lives here. Every runtime command/event that
crosses the wire is deliberately allowlisted so provider internals, labels,
paths, exception text, and future dataclass fields do not become protocol by
accident.
"""

from __future__ import annotations

from typing import Any

from zara.protocol import ProtocolMessage
from zara.runtime import events
from zara.runtime.bridge import EventEnvelope
from zara.runtime.commands import CancelTurn, RuntimeCommand, SubmitTurn


class RuntimeCodecError(ValueError):
    """A protocol message/event cannot be mapped to the runtime vocabulary."""


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
