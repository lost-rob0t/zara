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
        raise ProtocolValidationError(f"{name} must be printable ASCII") from error
    if len(encoded) > max_bytes:
        raise ProtocolValidationError(f"{name} exceeds byte limit")
    if allow_dot_type:
        if _TYPE_RE.fullmatch(value) is None:
            raise ProtocolValidationError(f"invalid message type: {value!r}")
    elif any(byte < 0x21 or byte > 0x7E for byte in encoded):
        raise ProtocolValidationError(f"{name} must be printable ASCII without whitespace")
    return value


def _validate_nonnegative_int(name: str, value: Any) -> int:
    if type(value) is not int or value < 0:
        raise ProtocolValidationError(f"{name} must be a non-negative integer")
    return value


def _validate_optional_id(name: str, value: Any, limits: ProtocolLimits) -> str | None:
    if value is None:
        return None
    return _validate_ascii_token(name, value, max_bytes=limits.max_id_bytes)


def _validate_content_type(value: Any) -> str | None:
    if value is None:
        return None
    if not isinstance(value, str) or not value:
        raise ProtocolValidationError("content_type must be a non-empty string")
    try:
        encoded = value.encode("ascii")
    except UnicodeEncodeError as error:
        raise ProtocolValidationError("content_type must be printable ASCII") from error
    if len(encoded) > 128 or any(byte < 0x21 or byte > 0x7E for byte in encoded):
        raise ProtocolValidationError("content_type must be bounded printable ASCII")
    return value


def _validate_flags(value: Any) -> dict[str, bool]:
    if value is None:
        return {}
    if not isinstance(value, dict):
        raise ProtocolValidationError("flags must be a JSON object")
    unknown = set(value) - _ALLOWED_FLAGS
    if unknown:
        raise ProtocolValidationError(f"flags contain unknown keys: {sorted(unknown)!r}")
    result: dict[str, bool] = {}
    for key, item in value.items():
        if type(item) is not bool:
            raise ProtocolValidationError("flags values must be boolean")
        result[key] = item
    return result


def _validate_body(value: Any) -> dict[str, Any] | None:
    if value is None:
        return None
    if not isinstance(value, dict):
        raise ProtocolValidationError("body must be a JSON object")
    return dict(value)


def _validate_message(
    value: Mapping[str, Any],
    *,
    limits: ProtocolLimits,
    expected_payload_count: int | None = None,
) -> ProtocolMessage:
    unknown = set(value) - _ALLOWED_ENVELOPE_KEYS
    if unknown:
        raise ProtocolValidationError(f"unknown envelope fields: {sorted(unknown)!r}")

    message_type = _validate_ascii_token(
        "type",
        value.get("type"),
        max_bytes=limits.max_type_bytes,
        allow_dot_type=True,
    )
    if message_type not in KNOWN_MESSAGE_TYPES:
        raise ProtocolValidationError(f"unknown message type: {message_type}")

    message_id = _validate_ascii_token(
        "id",
        value.get("id"),
        max_bytes=limits.max_id_bytes,
    )
    timestamp_ns = _validate_nonnegative_int("timestamp_ns", value.get("timestamp_ns"))
    payload_count = _validate_nonnegative_int("payload_count", value.get("payload_count"))
    if payload_count > limits.max_payload_frames:
        raise ProtocolValidationError("payload frame count exceeds limit")
    if expected_payload_count is not None and payload_count != expected_payload_count:
        raise ProtocolValidationError("payload_count does not match multipart frame count")

    seq = value.get("seq")
    if seq is not None:
        seq = _validate_nonnegative_int("seq", seq)

    return ProtocolMessage(
        type=message_type,
        id=message_id,
        timestamp_ns=timestamp_ns,
        payload_count=payload_count,
        reply_to=_validate_optional_id("reply_to", value.get("reply_to"), limits),
        session_id=_validate_optional_id("session_id", value.get("session_id"), limits),
        conversation_id=_validate_optional_id("conversation_id", value.get("conversation_id"), limits),
        turn_id=_validate_optional_id("turn_id", value.get("turn_id"), limits),
        stream_id=_validate_optional_id("stream_id", value.get("stream_id"), limits),
        seq=seq,
        trace_id=_validate_optional_id("trace_id", value.get("trace_id"), limits),
        content_type=_validate_content_type(value.get("content_type")),
        flags=_validate_flags(value.get("flags")),
        body=_validate_body(value.get("body")),
    )


def decode_message(
    frames: Sequence[bytes],
    *,
    limits: ProtocolLimits | None = None,
) -> DecodedMessage:
    limits = limits or ProtocolLimits()
    if not isinstance(frames, Sequence) or isinstance(frames, (str, bytes, bytearray)):
        raise ProtocolValidationError("frames must be a sequence of bytes")
    if len(frames) < 2:
        raise ProtocolValidationError("message must contain marker and envelope frames")
    marker = frames[0]
    if marker != PROTOCOL_MARKER:
        raise ProtocolVersionError("unsupported ZARA application protocol marker")
    envelope = _load_envelope(frames[1], limits)
    payloads = tuple(frames[2:])
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
    message = _validate_message(
        envelope,
        limits=limits,
        expected_payload_count=len(payloads),
    )
    return DecodedMessage(message=message, payloads=payloads)


def _message_dict(message: ProtocolMessage) -> dict[str, Any]:
    if not isinstance(message, ProtocolMessage):
        raise ProtocolValidationError("encoder requires ProtocolMessage")
    value: dict[str, Any] = {
        "type": message.type,
        "id": message.id,
        "timestamp_ns": message.timestamp_ns,
        "payload_count": message.payload_count,
    }
    optional = (
        ("reply_to", message.reply_to),
        ("session_id", message.session_id),
        ("conversation_id", message.conversation_id),
        ("turn_id", message.turn_id),
        ("stream_id", message.stream_id),
        ("seq", message.seq),
        ("trace_id", message.trace_id),
        ("content_type", message.content_type),
    )
    for name, item in optional:
        if item is not None:
            value[name] = item
    if message.flags:
        value["flags"] = dict(message.flags)
    if message.body is not None:
        value["body"] = dict(message.body)
    return value


def encode_message(
    message: ProtocolMessage,
    payloads: Sequence[bytes] = (),
    *,
    limits: ProtocolLimits | None = None,
) -> tuple[bytes, ...]:
    limits = limits or ProtocolLimits()
    if not isinstance(payloads, Sequence) or isinstance(payloads, (str, bytes, bytearray)):
        raise ProtocolValidationError("payloads must be a sequence of bytes")
    if message.payload_count != len(payloads):
        raise ProtocolValidationError("payload_count does not match payloads")
    value = _message_dict(message)
    validated = _validate_message(
        value,
        limits=limits,
        expected_payload_count=len(payloads),
    )
    if validated != message:
        raise ProtocolValidationError("message contains non-canonical values")
    payload_tuple = tuple(payloads)
    total = 0
    for payload in payload_tuple:
        if not isinstance(payload, bytes):
            raise ProtocolValidationError("payload frames must be bytes")
        if len(payload) > limits.max_payload_frame_bytes:
            raise ProtocolValidationError("payload frame exceeds byte limit")
        total += len(payload)
        if total > limits.max_payload_bytes:
            raise ProtocolValidationError("aggregate payload bytes exceed limit")
    envelope = json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
        allow_nan=False,
    ).encode("utf-8")
    if len(envelope) > limits.max_envelope_bytes:
        raise ProtocolValidationError("encoded envelope exceeds byte limit")
    return (PROTOCOL_MARKER, envelope, *payload_tuple)


__all__ = [
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
