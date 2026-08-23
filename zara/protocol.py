"""Strict, transport-neutral ZARA/1 application framing.

This module deliberately knows nothing about ZeroMQ sockets, RuntimeHost, or
principal authentication. It validates the application multipart frames that
issue #129 places above ZMTP and fails closed on ambiguous input.
"""

from __future__ import annotations

import json
import re
from dataclasses import dataclass, field
from typing import Any, Mapping, Sequence


PROTOCOL_MARKER = b"ZARA/1"
AUDIO_INPUT_CONTENT_TYPE = "audio/pcm;codec=pcm_s16le"
AUDIO_INPUT_CODEC = "pcm_s16le"
AUDIO_INPUT_SAMPLE_RATE = 16000
AUDIO_INPUT_CHANNELS = 1
AUDIO_INPUT_FRAME_SAMPLES = 512
AUDIO_INPUT_FRAME_BYTES = AUDIO_INPUT_FRAME_SAMPLES * 2

CLIENT_MESSAGE_TYPES = frozenset(
    {
        "hello",
        "ping",
        "runtime.status",
        "conversation.open",
        "turn.submit",
        "turn.cancel",
    }
)

SERVER_MESSAGE_TYPES = frozenset(
    {
        "hello.ok",
        "pong",
        "runtime.status.ok",
        "conversation.opened",
        "turn.accepted",
        "turn.cancel.accepted",
        "turn.started",
        "turn.completed",
        "turn.cancelled",
        "assistant.started",
        "assistant.delta",
        "assistant.completed",
        "assistant.response",
        "audio.input.started",
        "audio.input.accepted",
        "audio.input.committed",
        "audio.input.cancelled",
        "runtime.error",
        "runtime.stopped",
        "protocol.error",
    }
)

RESERVED_MESSAGE_TYPES = frozenset(
    {
        "voice.start",
        "audio.input.start",
        "audio.input.chunk",
        "audio.input.commit",
        "audio.input.cancel",
        "audio.output.start",
        "audio.output.chunk",
        "audio.output.done",
    }
)

KNOWN_MESSAGE_TYPES = CLIENT_MESSAGE_TYPES | SERVER_MESSAGE_TYPES | RESERVED_MESSAGE_TYPES

_ALLOWED_ENVELOPE_KEYS = frozenset(
    {
        "type",
        "id",
        "reply_to",
        "session_id",
        "conversation_id",
        "turn_id",
        "stream_id",
        "seq",
        "timestamp_ns",
        "trace_id",
        "content_type",
        "payload_count",
        "flags",
        "body",
    }
)
_ALLOWED_FLAGS = frozenset({"idempotent", "resume"})
_TYPE_RE = re.compile(r"^[a-z][a-z0-9]*(?:\.[a-z0-9]+)*$")
_AUDIO_INPUT_START_BODY = {
    "codec": AUDIO_INPUT_CODEC,
    "sample_rate": AUDIO_INPUT_SAMPLE_RATE,
    "channels": AUDIO_INPUT_CHANNELS,
    "frame_samples": AUDIO_INPUT_FRAME_SAMPLES,
}


class ZaraProtocolError(ValueError):
    """Base class for local ZARA/1 framing failures."""


class ProtocolVersionError(ZaraProtocolError):
    """The application protocol marker is not the supported major version."""


class ProtocolValidationError(ZaraProtocolError):
    """The ZARA/1 envelope or payload violates the closed schema."""


@dataclass(frozen=True)
class ProtocolLimits:
    max_envelope_bytes: int = 64 * 1024
    max_payload_frames: int = 16
    max_payload_frame_bytes: int = 1024 * 1024
    max_payload_bytes: int = 4 * 1024 * 1024
    max_id_bytes: int = 128
    max_type_bytes: int = 64

    def __post_init__(self) -> None:
        integer_fields = (
            "max_envelope_bytes",
            "max_payload_frames",
            "max_payload_frame_bytes",
            "max_payload_bytes",
            "max_id_bytes",
            "max_type_bytes",
        )
        for name in integer_fields:
            value = getattr(self, name)
            if type(value) is not int or value < 0:
                raise ValueError(f"{name} must be a non-negative integer")
        if self.max_envelope_bytes == 0:
            raise ValueError("max_envelope_bytes must be greater than zero")
        if self.max_payload_frame_bytes == 0:
            raise ValueError("max_payload_frame_bytes must be greater than zero")
        if self.max_payload_bytes == 0:
            raise ValueError("max_payload_bytes must be greater than zero")
        if self.max_id_bytes == 0:
            raise ValueError("max_id_bytes must be greater than zero")
        if self.max_type_bytes == 0:
            raise ValueError("max_type_bytes must be greater than zero")


@dataclass(frozen=True)
class ProtocolMessage:
    type: str
    id: str
    timestamp_ns: int
    payload_count: int
    reply_to: str | None = None
    session_id: str | None = None
    conversation_id: str | None = None
    turn_id: str | None = None
    stream_id: str | None = None
    seq: int | None = None
    trace_id: str | None = None
    content_type: str | None = None
    flags: Mapping[str, bool] = field(default_factory=dict)
    body: Mapping[str, Any] | None = None


@dataclass(frozen=True)
class DecodedMessage:
    message: ProtocolMessage
    payloads: tuple[bytes, ...]


def _reject_constant(value: str) -> None:
    raise ProtocolValidationError(f"non-finite JSON constant is not allowed: {value}")


def _strict_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ProtocolValidationError(f"duplicate JSON key: {key}")
        result[key] = value
    return result


def _load_envelope(frame: bytes, limits: ProtocolLimits) -> dict[str, Any]:
    if not isinstance(frame, bytes):
        raise ProtocolValidationError("envelope frame must be bytes")
    if len(frame) > limits.max_envelope_bytes:
        raise ProtocolValidationError("envelope exceeds byte limit")
    try:
        text = frame.decode("utf-8", errors="strict")
    except UnicodeDecodeError as error:
        raise ProtocolValidationError("envelope is not valid UTF-8") from error
    try:
        value = json.loads(
            text,
            object_pairs_hook=_strict_object,
            parse_constant=_reject_constant,
        )
    except ProtocolValidationError:
        raise
    except (TypeError, ValueError, json.JSONDecodeError) as error:
        raise ProtocolValidationError("envelope is not valid strict JSON") from error
    if not isinstance(value, dict):
        raise ProtocolValidationError("envelope must be a JSON object")
    return value


def _validate_ascii_token(
    name: str,
    value: Any,
    *,
    max_bytes: int,
    allow_dot_type: bool = False,
) -> str:
    if not isinstance(value, str) or not value:
        raise ProtocolValidationError(f"{name} must be a non-empty string")
    try:
        encoded = value.encode("ascii")
    except UnicodeEncodeError as error:
        raise ProtocolValidationError(f"{name} must be ASCII") from error
    if len(encoded) > max_bytes:
        raise ProtocolValidationError(f"{name} exceeds byte limit")
    if allow_dot_type and not _TYPE_RE.fullmatch(value):
        raise ProtocolValidationError(f"{name} has invalid format")
    return value


def _validate_optional_ascii_token(
    name: str,
    value: Any,
    *,
    max_bytes: int,
) -> str | None:
    if value is None:
        return None
    return _validate_ascii_token(name, value, max_bytes=max_bytes)


def _validate_non_negative_int(name: str, value: Any) -> int | None:
    if value is None:
        return None
    if type(value) is not int or value < 0:
        raise ProtocolValidationError(f"{name} must be a non-negative integer")
    return value


def _validate_flags(value: Any) -> Mapping[str, bool]:
    if value is None:
        return {}
    if not isinstance(value, dict):
        raise ProtocolValidationError("flags must be a JSON object")
    unknown = set(value) - _ALLOWED_FLAGS
    if unknown:
        raise ProtocolValidationError(f"unknown flags: {sorted(unknown)}")
    for key, enabled in value.items():
        if type(enabled) is not bool:
            raise ProtocolValidationError(f"flag {key} must be boolean")
    return value


def _validate_body(value: Any) -> Mapping[str, Any] | None:
    if value is None:
        return None
    if not isinstance(value, dict):
        raise ProtocolValidationError("body must be a JSON object")
    return value


def _validate_audio_message(message: ProtocolMessage, payloads: Sequence[bytes]) -> None:
    if message.type == "audio.input.start":
        if message.stream_id is None:
            raise ProtocolValidationError("audio.input.start requires stream_id")
        if message.seq is not None:
            raise ProtocolValidationError("audio.input.start must not include seq")
        if message.payload_count != 0 or payloads:
            raise ProtocolValidationError("audio.input.start must not carry payload")
        if dict(message.body or {}) != _AUDIO_INPUT_START_BODY:
            raise ProtocolValidationError("audio.input.start requires baseline PCM geometry")
        return

    if message.type == "audio.input.chunk":
        if message.stream_id is None:
            raise ProtocolValidationError("audio.input.chunk requires stream_id")
        if message.seq is None:
            raise ProtocolValidationError("audio.input.chunk requires seq")
        if message.content_type != AUDIO_INPUT_CONTENT_TYPE:
            raise ProtocolValidationError("audio.input.chunk has unsupported content_type")
        if message.payload_count != 1 or len(payloads) != 1:
            raise ProtocolValidationError("audio.input.chunk requires exactly one payload frame")
        if len(payloads[0]) != AUDIO_INPUT_FRAME_BYTES:
            raise ProtocolValidationError(
                f"audio.input.chunk payload must be exactly {AUDIO_INPUT_FRAME_BYTES} bytes"
            )
        return

    if message.type in {"audio.input.commit", "audio.input.cancel"}:
        if message.stream_id is None:
            raise ProtocolValidationError(f"{message.type} requires stream_id")
        if message.seq is not None:
            raise ProtocolValidationError(f"{message.type} must not include seq")
        if message.payload_count != 0 or payloads:
            raise ProtocolValidationError(f"{message.type} must not carry payload")


def _message_from_mapping(value: Mapping[str, Any], limits: ProtocolLimits) -> ProtocolMessage:
    unknown = set(value) - _ALLOWED_ENVELOPE_KEYS
    if unknown:
        raise ProtocolValidationError(f"unknown envelope keys: {sorted(unknown)}")
    required = {"type", "id", "timestamp_ns", "payload_count"}
    missing = required - set(value)
    if missing:
        raise ProtocolValidationError(f"missing envelope keys: {sorted(missing)}")

    message_type = _validate_ascii_token(
        "type",
        value["type"],
        max_bytes=limits.max_type_bytes,
        allow_dot_type=True,
    )
    if message_type not in KNOWN_MESSAGE_TYPES:
        raise ProtocolValidationError("unknown message type")

    payload_count = _validate_non_negative_int("payload_count", value["payload_count"])
    assert payload_count is not None

    return ProtocolMessage(
        type=message_type,
        id=_validate_ascii_token("id", value["id"], max_bytes=limits.max_id_bytes),
        reply_to=_validate_optional_ascii_token(
            "reply_to", value.get("reply_to"), max_bytes=limits.max_id_bytes
        ),
        session_id=_validate_optional_ascii_token(
            "session_id", value.get("session_id"), max_bytes=limits.max_id_bytes
        ),
        conversation_id=_validate_optional_ascii_token(
            "conversation_id", value.get("conversation_id"), max_bytes=limits.max_id_bytes
        ),
        turn_id=_validate_optional_ascii_token(
            "turn_id", value.get("turn_id"), max_bytes=limits.max_id_bytes
        ),
        stream_id=_validate_optional_ascii_token(
            "stream_id", value.get("stream_id"), max_bytes=limits.max_id_bytes
        ),
        seq=_validate_non_negative_int("seq", value.get("seq")),
        timestamp_ns=_validate_non_negative_int("timestamp_ns", value["timestamp_ns"]),
        trace_id=_validate_optional_ascii_token(
            "trace_id", value.get("trace_id"), max_bytes=limits.max_id_bytes
        ),
        content_type=_validate_optional_ascii_token(
            "content_type", value.get("content_type"), max_bytes=limits.max_type_bytes
        ),
        payload_count=payload_count,
        flags=_validate_flags(value.get("flags")),
        body=_validate_body(value.get("body")),
    )


def _message_to_mapping(message: ProtocolMessage) -> dict[str, Any]:
    mapping: dict[str, Any] = {
        "type": message.type,
        "id": message.id,
        "timestamp_ns": message.timestamp_ns,
        "payload_count": message.payload_count,
    }
    optional_values = {
        "reply_to": message.reply_to,
        "session_id": message.session_id,
        "conversation_id": message.conversation_id,
        "turn_id": message.turn_id,
        "stream_id": message.stream_id,
        "seq": message.seq,
        "trace_id": message.trace_id,
        "content_type": message.content_type,
        "body": message.body,
    }
    for key, value in optional_values.items():
        if value is not None:
            mapping[key] = value
    if message.flags:
        mapping["flags"] = dict(message.flags)
    return mapping


def _validate_decoded(message: ProtocolMessage, payloads: Sequence[bytes], limits: ProtocolLimits) -> None:
    if message.payload_count != len(payloads):
        raise ProtocolValidationError("payload_count does not match multipart frames")
    if len(payloads) > limits.max_payload_frames:
        raise ProtocolValidationError("payload frame count exceeds limit")
    total = 0
    for payload in payloads:
        if not isinstance(payload, bytes):
            raise ProtocolValidationError("payload frames must be bytes")
        if len(payload) > limits.max_payload_frame_bytes:
            raise ProtocolValidationError("payload frame exceeds byte limit")
        total += len(payload)
        if total > limits.max_payload_bytes:
            raise ProtocolValidationError("aggregate payload bytes exceed limit")
    if message.type in RESERVED_MESSAGE_TYPES:
        if message.type.startswith("audio.input."):
            _validate_audio_message(message, payloads)
        elif message.type.startswith("audio.") or message.type.startswith("voice."):
            raise ProtocolValidationError("reserved audio message has no active contract")


def encode_message(
    message: ProtocolMessage,
    *,
    payloads: Sequence[bytes] = (),
    limits: ProtocolLimits | None = None,
) -> list[bytes]:
    limits = limits or ProtocolLimits()
    mapping = _message_to_mapping(message)
    normalized = _message_from_mapping(mapping, limits)
    _validate_decoded(normalized, payloads, limits)
    envelope = json.dumps(
        mapping,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
        allow_nan=False,
    ).encode("ascii")
    if len(envelope) > limits.max_envelope_bytes:
        raise ProtocolValidationError("envelope exceeds byte limit")
    return [PROTOCOL_MARKER, envelope, *payloads]


def decode_message(
    frames: Sequence[bytes],
    *,
    limits: ProtocolLimits | None = None,
) -> DecodedMessage:
    limits = limits or ProtocolLimits()
    if len(frames) < 2:
        raise ProtocolValidationError("message requires protocol marker and envelope")
    if frames[0] != PROTOCOL_MARKER:
        raise ProtocolVersionError("unsupported ZARA protocol marker")
    envelope = _load_envelope(frames[1], limits)
    message = _message_from_mapping(envelope, limits)
    payloads = tuple(frames[2:])
    _validate_decoded(message, payloads, limits)
    return DecodedMessage(message=message, payloads=payloads)


__all__ = [
    "AUDIO_INPUT_CHANNELS",
    "AUDIO_INPUT_CODEC",
    "AUDIO_INPUT_CONTENT_TYPE",
    "AUDIO_INPUT_FRAME_BYTES",
    "AUDIO_INPUT_FRAME_SAMPLES",
    "AUDIO_INPUT_SAMPLE_RATE",
    "CLIENT_MESSAGE_TYPES",
    "DecodedMessage",
    "KNOWN_MESSAGE_TYPES",
    "PROTOCOL_MARKER",
    "ProtocolLimits",
    "ProtocolMessage",
    "ProtocolValidationError",
    "ProtocolVersionError",
    "RESERVED_MESSAGE_TYPES",
    "SERVER_MESSAGE_TYPES",
    "ZaraProtocolError",
    "decode_message",
    "encode_message",
]
