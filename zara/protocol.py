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
    return value


def _validate_audio_input_envelope(message: ProtocolMessage) -> None:
    if message.type not in {
        "audio.input.start",
        "audio.input.chunk",
        "audio.input.commit",
        "audio.input.cancel",
    }:
        return

    if message.stream_id is None:
        raise ProtocolValidationError(f"{message.type} requires stream_id")

    if message.type == "audio.input.start":
        if message.payload_count != 0:
            raise ProtocolValidationError("audio.input.start does not accept payload frames")
        if message.seq is not None:
            raise ProtocolValidationError("audio.input.start does not accept seq")
        if message.content_type is not None:
            raise ProtocolValidationError("audio.input.start does not accept content_type")
        if dict(message.body or {}) != _AUDIO_INPUT_START_BODY:
            raise ProtocolValidationError(
                "audio.input.start requires pcm_s16le mono 16000 Hz 512-sample geometry"
            )
        return

    if message.type == "audio.input.chunk":
        if message.seq is None:
            raise ProtocolValidationError("audio.input.chunk requires seq")
        if message.content_type != AUDIO_INPUT_CONTENT_TYPE:
            raise ProtocolValidationError(
                f"audio.input.chunk content_type must be {AUDIO_INPUT_CONTENT_TYPE!r}"
            )
        if message.payload_count != 1:
            raise ProtocolValidationError("audio.input.chunk requires exactly one payload frame")
        if message.body is not None:
            raise ProtocolValidationError("audio.input.chunk does not accept body")
        return

    if message.seq is not None:
        raise ProtocolValidationError(f"{message.type} does not accept seq")
    if message.payload_count != 0:
        raise ProtocolValidationError(f"{message.type} does not accept payload frames")
    if message.content_type is not None:
        raise ProtocolValidationError(f"{message.type} does not accept content_type")
    if message.body is not None:
        raise ProtocolValidationError(f"{message.type} does not accept body")


def _validate_audio_input_payloads(
    message: ProtocolMessage,
    payloads: Sequence[bytes],
) -> None:
    if message.type != "audio.input.chunk":
        return
    if len(payloads) != 1 or len(payloads[0]) != AUDIO_INPUT_FRAME_BYTES:
        raise ProtocolValidationError(
            f"audio.input.chunk payload must be exactly {AUDIO_INPUT_FRAME_BYTES} bytes"
        )


def _message_from_mapping(data: Mapping[str, Any], limits: ProtocolLimits) -> ProtocolMessage:
    unknown = set(data) - _ALLOWED_ENVELOPE_KEYS
    if unknown:
        raise ProtocolValidationError(f"unknown envelope keys: {sorted(unknown)!r}")

    missing = {"type", "id", "timestamp_ns", "payload_count"} - set(data)
    if missing:
        raise ProtocolValidationError(f"missing required envelope keys: {sorted(missing)!r}")

    message_type = _validate_ascii_token(
        "type",
        data["type"],
        max_bytes=limits.max_type_bytes,
        allow_dot_type=True,
    )
    if message_type not in KNOWN_MESSAGE_TYPES:
        raise ProtocolValidationError(f"unsupported message type: {message_type!r}")

    request_id = _validate_ascii_token("id", data["id"], max_bytes=limits.max_id_bytes)
    timestamp_ns = _validate_nonnegative_int("timestamp_ns", data["timestamp_ns"])
    payload_count = _validate_nonnegative_int("payload_count", data["payload_count"])
    if payload_count > limits.max_payload_frames:
        raise ProtocolValidationError("payload frame count exceeds limit")

    seq = data.get("seq")
    if seq is not None:
        seq = _validate_nonnegative_int("seq", seq)

    message = ProtocolMessage(
        type=message_type,
        id=request_id,
        timestamp_ns=timestamp_ns,
        payload_count=payload_count,
        reply_to=_validate_optional_id("reply_to", data.get("reply_to"), limits),
        session_id=_validate_optional_id("session_id", data.get("session_id"), limits),
        conversation_id=_validate_optional_id("conversation_id", data.get("conversation_id"), limits),
        turn_id=_validate_optional_id("turn_id", data.get("turn_id"), limits),
        stream_id=_validate_optional_id("stream_id", data.get("stream_id"), limits),
        seq=seq,
        trace_id=_validate_optional_id("trace_id", data.get("trace_id"), limits),
        content_type=_validate_content_type(data.get("content_type")),
        flags=_validate_flags(data.get("flags")),
        body=_validate_body(data.get("body")),
    )
    _validate_audio_input_envelope(message)
    return message


def _validate_payloads(
    payloads: Sequence[bytes],
    *,
    expected_count: int,
    limits: ProtocolLimits,
) -> tuple[bytes, ...]:
    if len(payloads) != expected_count:
        raise ProtocolValidationError(
            f"payload_count mismatch: envelope={expected_count}, frames={len(payloads)}"
        )
    if len(payloads) > limits.max_payload_frames:
        raise ProtocolValidationError("payload frame count exceeds limit")

    normalized: list[bytes] = []
    aggregate = 0
    for frame in payloads:
        if not isinstance(frame, bytes):
            raise ProtocolValidationError("payload frames must be bytes")
        if len(frame) > limits.max_payload_frame_bytes:
            raise ProtocolValidationError("payload frame exceeds byte limit")
        aggregate += len(frame)
        if aggregate > limits.max_payload_bytes:
            raise ProtocolValidationError("aggregate payload exceeds byte limit")
        normalized.append(frame)
    return tuple(normalized)


def _message_to_mapping(message: ProtocolMessage, limits: ProtocolLimits) -> dict[str, Any]:
    data: dict[str, Any] = {
        "type": message.type,
        "id": message.id,
        "timestamp_ns": message.timestamp_ns,
        "payload_count": message.payload_count,
    }
    optional = {
        "reply_to": message.reply_to,
        "session_id": message.session_id,
        "conversation_id": message.conversation_id,
        "turn_id": message.turn_id,
        "stream_id": message.stream_id,
        "seq": message.seq,
        "trace_id": message.trace_id,
        "content_type": message.content_type,
    }
    for key, value in optional.items():
        if value is not None:
            data[key] = value
    if message.flags:
        data["flags"] = dict(message.flags)
    if message.body is not None:
        data["body"] = dict(message.body)

    validated = _message_from_mapping(data, limits)
    if validated != message:
        raise ProtocolValidationError("message contains non-canonical field values")
    return data


def decode_message(
    frames: Sequence[bytes],
    *,
    limits: ProtocolLimits | None = None,
) -> DecodedMessage:
    limits = limits or ProtocolLimits()
    if len(frames) < 2:
        raise ProtocolValidationError("ZARA/1 message requires marker and envelope frames")
    marker = frames[0]
    if marker != PROTOCOL_MARKER:
        raise ProtocolVersionError("unsupported ZARA application protocol marker")

    envelope = _load_envelope(frames[1], limits)
    message = _message_from_mapping(envelope, limits)
    payloads = _validate_payloads(
        frames[2:],
        expected_count=message.payload_count,
        limits=limits,
    )
    _validate_audio_input_payloads(message, payloads)
    return DecodedMessage(message=message, payloads=payloads)


def encode_message(
    message: ProtocolMessage,
    *,
    payloads: Sequence[bytes] = (),
    limits: ProtocolLimits | None = None,
) -> list[bytes]:
    limits = limits or ProtocolLimits()
    if not isinstance(message, ProtocolMessage):
        raise ProtocolValidationError("message must be ProtocolMessage")
    normalized_payloads = _validate_payloads(
        payloads,
        expected_count=message.payload_count,
        limits=limits,
    )
    data = _message_to_mapping(message, limits)
    _validate_audio_input_payloads(message, normalized_payloads)
    try:
        envelope = json.dumps(
            data,
            ensure_ascii=False,
            allow_nan=False,
            sort_keys=True,
            separators=(",", ":"),
        ).encode("utf-8")
    except (TypeError, ValueError) as error:
        raise ProtocolValidationError("message body is not strict JSON") from error
    if len(envelope) > limits.max_envelope_bytes:
        raise ProtocolValidationError("envelope exceeds byte limit")
    return [PROTOCOL_MARKER, envelope, *normalized_payloads]


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
