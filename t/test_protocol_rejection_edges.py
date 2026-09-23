from __future__ import annotations

import json

import pytest

from zara.protocol import (
    PROTOCOL_MARKER,
    ProtocolLimits,
    ProtocolMessage,
    ProtocolValidationError,
    decode_message,
    encode_message,
)


def envelope(**overrides) -> bytes:
    values = {
        "type": "hello",
        "id": "req-1",
        "timestamp_ns": 1,
        "payload_count": 0,
        "body": {"versions": [1]},
    }
    values.update(overrides)
    return json.dumps(values, sort_keys=True, separators=(",", ":")).encode("utf-8")


def reject(message: str, *, payloads: tuple[object, ...] = (), limits=None, **overrides) -> None:
    with pytest.raises(ProtocolValidationError, match=message):
        decode_message(
            [PROTOCOL_MARKER, envelope(**overrides), *payloads],
            limits=limits,
        )


def device_request_body(**overrides):
    body = {
        "action_id": "action-1",
        "action_seq": 1,
        "capability": "open_uri",
        "args": {"uri": "https://example.test"},
        "deadline_ns": 10,
        "idempotency": "at_most_once",
    }
    body.update(overrides)
    return body


def test_ascii_and_content_type_boundaries_fail_closed():
    reject("id must be printable ASCII", id="req-é")
    reject("content_type must be a non-empty string", content_type="")
    reject("content_type must be bounded printable ASCII", content_type="text/plain with-space")


def test_visible_stt_rejects_turn_correlation_before_rendering():
    reject(
        "voice.transcript.final does not accept turn_id",
        type="voice.transcript.final",
        conversation_id="conversation-1",
        turn_id="turn-1",
        stream_id="mic-1",
        seq=1,
        body={"text": "hello"},
    )


def test_tool_common_and_approval_correlation_boundaries_fail_closed():
    reject(
        "tool.approve does not accept payload frames",
        type="tool.approve",
        session_id="session-1",
        payload_count=1,
        body={"tool_run_id": "tool-1"},
    )
    reject(
        "tool.approve does not accept stream payload fields",
        type="tool.approve",
        session_id="session-1",
        stream_id="stream-1",
        body={"tool_run_id": "tool-1"},
    )
    reject(
        "tool.approve does not accept trace or flags",
        type="tool.approve",
        session_id="session-1",
        flags={"resume": True},
        body={"tool_run_id": "tool-1"},
    )
    reject(
        "tool.approve accepts only session correlation",
        type="tool.approve",
        session_id="session-1",
        reply_to="request-0",
        body={"tool_run_id": "tool-1"},
    )
    reject(
        "tool.approve accepts only session correlation",
        type="tool.approve",
        session_id="session-1",
        seq=1,
        body={"tool_run_id": "tool-1"},
    )
    reject(
        "tool_name is invalid",
        type="tool.queued",
        session_id="session-1",
        turn_id="turn-1",
        seq=1,
        body={"tool_run_id": "tool-1", "tool_name": "bad/name"},
    )


def test_capability_and_device_common_boundaries_fail_closed():
    reject(
        "capability snapshot body has invalid fields",
        type="capability.snapshot",
        session_id="session-1",
        body={"capabilities": [], "extra": True},
    )
    reject(
        "device.action.request requires session_id",
        type="device.action.request",
        body=device_request_body(),
    )
    reject(
        "device.action.request does not accept payload frames",
        type="device.action.request",
        session_id="session-1",
        payload_count=1,
        body=device_request_body(),
    )
    reject(
        "device.action.request does not accept flags",
        type="device.action.request",
        session_id="session-1",
        flags={"resume": True},
        body=device_request_body(),
    )
    reject(
        "open_uri args have invalid fields",
        type="device.action.request",
        session_id="session-1",
        body=device_request_body(args={"wrong": "https://example.test"}),
    )


@pytest.mark.parametrize(
    ("message_type", "correlation", "body", "error"),
    [
        (
            "device.action.cancel",
            {"reply_to": "request-0"},
            {"action_id": "action-1"},
            "device.action.cancel has invalid correlation",
        ),
        (
            "device.action.accepted",
            {"reply_to": "request-0"},
            {"action_id": "action-1"},
            "device.action.accepted has invalid correlation",
        ),
        (
            "device.action.result",
            {"trace_id": "trace-1"},
            {"action_id": "action-1", "outcome": "completed"},
            "device.action.result has invalid correlation",
        ),
        (
            "device.action.error",
            {"reply_to": "request-0"},
            {"action_id": "action-1", "code": "failed"},
            "device.action.error has invalid correlation",
        ),
    ],
)
def test_device_terminal_messages_reject_cross_direction_correlation(
    message_type, correlation, body, error
):
    reject(
        error,
        type=message_type,
        session_id="session-1",
        body=body,
        **correlation,
    )


def test_device_terminal_messages_reject_extra_authority_fields():
    reject(
        "device.action.cancel body has invalid fields",
        type="device.action.cancel",
        session_id="session-1",
        body={"action_id": "action-1", "extra": True},
    )
    reject(
        "device.action.error body has invalid fields",
        type="device.action.error",
        session_id="session-1",
        body={"action_id": "action-1", "code": "failed", "extra": True},
    )


def test_missing_required_envelope_key_is_rejected_before_dispatch():
    raw = {
        "type": "hello",
        "timestamp_ns": 1,
        "payload_count": 0,
        "body": {"versions": [1]},
    }
    encoded = json.dumps(raw, sort_keys=True, separators=(",", ":")).encode("utf-8")
    with pytest.raises(ProtocolValidationError, match="missing required envelope keys"):
        decode_message([PROTOCOL_MARKER, encoded])


def test_payload_frame_count_and_type_limits_fail_closed():
    reject(
        "payload frame count exceeds limit",
        payload_count=1,
        payloads=(b"x",),
        limits=ProtocolLimits(max_payload_frames=0),
    )
    reject(
        "payload frames must be bytes",
        payload_count=1,
        payloads=(bytearray(b"x"),),
    )


def test_encoder_rejects_wrong_type_non_json_body_and_oversized_envelope():
    with pytest.raises(ProtocolValidationError, match="message must be ProtocolMessage"):
        encode_message(object())

    non_json = ProtocolMessage(
        type="hello",
        id="req-1",
        timestamp_ns=1,
        payload_count=0,
        body={"versions": {1}},
    )
    with pytest.raises(ProtocolValidationError, match="message body is not strict JSON"):
        encode_message(non_json)

    valid = ProtocolMessage(
        type="hello",
        id="req-1",
        timestamp_ns=1,
        payload_count=0,
        body={"versions": [1]},
    )
    with pytest.raises(ProtocolValidationError, match="envelope exceeds byte limit"):
        encode_message(valid, limits=ProtocolLimits(max_envelope_bytes=1))
