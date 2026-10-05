from __future__ import annotations

import json

import pytest

from zara.protocol import (
    AUDIO_INPUT_CONTENT_TYPE,
    AUDIO_INPUT_FRAME_BYTES,
    AUDIO_OUTPUT_CONTENT_TYPE,
    PROTOCOL_MARKER,
    ProtocolLimits,
    ProtocolValidationError,
    decode_message,
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


def reject(*, payloads: tuple[bytes, ...] = (), **overrides) -> None:
    with pytest.raises(ProtocolValidationError):
        decode_message([PROTOCOL_MARKER, envelope(**overrides), *payloads])


@pytest.mark.parametrize(
    "field",
    [
        "max_envelope_bytes",
        "max_payload_frames",
        "max_payload_frame_bytes",
        "max_payload_bytes",
        "max_id_bytes",
        "max_type_bytes",
    ],
)
def test_protocol_limits_reject_negative_values_for_every_limit(field):
    with pytest.raises(ValueError):
        ProtocolLimits(**{field: -1})


@pytest.mark.parametrize(
    "field",
    [
        "max_envelope_bytes",
        "max_payload_frame_bytes",
        "max_payload_bytes",
        "max_id_bytes",
        "max_type_bytes",
    ],
)
def test_protocol_limits_reject_zero_for_nonzero_limits(field):
    with pytest.raises(ValueError):
        ProtocolLimits(**{field: 0})


def test_protocol_limits_reject_boolean_values_even_though_bool_is_an_int_subclass():
    with pytest.raises(ValueError):
        ProtocolLimits(max_payload_frames=True)


def test_envelope_frame_body_and_content_type_fail_closed_before_dispatch():
    with pytest.raises(ProtocolValidationError, match="envelope frame must be bytes"):
        decode_message([PROTOCOL_MARKER, bytearray(envelope())])

    reject(content_type="text/é")
    reject(body=[])


@pytest.mark.parametrize(
    "overrides",
    [
        {"stream_id": None},
        {"payload_count": 1},
        {"seq": 0},
        {"content_type": "audio/pcm"},
        {
            "body": {
                "codec": "pcm_s16le",
                "sample_rate": 16000,
                "channels": 1,
                "frame_samples": 511,
            }
        },
    ],
)
def test_audio_input_start_rejects_ambiguous_or_noncanonical_geometry(overrides):
    values = {
        "type": "audio.input.start",
        "stream_id": "mic-1",
        "body": {
            "codec": "pcm_s16le",
            "sample_rate": 16000,
            "channels": 1,
            "frame_samples": 512,
        },
    }
    values.update(overrides)
    reject(**values)


@pytest.mark.parametrize(
    "overrides",
    [
        {"seq": None},
        {"content_type": "audio/pcm"},
        {"payload_count": 0},
        {"body": {}},
    ],
)
def test_audio_input_chunk_rejects_missing_or_extra_wire_shape(overrides):
    values = {
        "type": "audio.input.chunk",
        "stream_id": "mic-1",
        "seq": 0,
        "content_type": AUDIO_INPUT_CONTENT_TYPE,
        "payload_count": 1,
        "body": None,
    }
    values.update(overrides)
    reject(**values)


def test_audio_input_chunk_rejects_wrong_pcm_frame_size_after_envelope_validation():
    reject(
        type="audio.input.chunk",
        stream_id="mic-1",
        seq=0,
        content_type=AUDIO_INPUT_CONTENT_TYPE,
        payload_count=1,
        body=None,
        payloads=(b"\x00" * (AUDIO_INPUT_FRAME_BYTES - 2),),
    )


@pytest.mark.parametrize("message_type", ["audio.input.commit", "audio.input.cancel"])
@pytest.mark.parametrize(
    "overrides",
    [
        {"seq": 0},
        {"payload_count": 1},
        {"content_type": AUDIO_INPUT_CONTENT_TYPE},
        {"body": {}},
    ],
)
def test_audio_input_terminal_commands_reject_stream_payload_fields(message_type, overrides):
    values = {"type": message_type, "stream_id": "mic-1"}
    values.update(overrides)
    reject(**values)


@pytest.mark.parametrize(
    "overrides",
    [
        {"turn_id": None},
        {"stream_id": None},
        {"payload_count": 1},
        {"seq": 0},
        {"content_type": AUDIO_OUTPUT_CONTENT_TYPE},
        {"body": {"codec": "pcm_s16le", "sample_rate": 16000}},
        {"body": {"codec": "opus", "sample_rate": 16000, "channels": 1}},
        {"body": {"codec": "pcm_s16le", "sample_rate": 0, "channels": 1}},
        {"body": {"codec": "pcm_s16le", "sample_rate": 16000, "channels": 0}},
    ],
)
def test_audio_output_start_rejects_invalid_transport_geometry(overrides):
    values = {
        "type": "audio.output.start",
        "turn_id": "turn-1",
        "stream_id": "speaker-1",
        "body": {"codec": "pcm_s16le", "sample_rate": 16000, "channels": 1},
    }
    values.update(overrides)
    reject(**values)


@pytest.mark.parametrize(
    "overrides",
    [
        {"seq": None},
        {"content_type": "audio/pcm"},
        {"payload_count": 0},
        {"body": {}},
    ],
)
def test_audio_output_chunk_rejects_noncanonical_wire_shape(overrides):
    values = {
        "type": "audio.output.chunk",
        "turn_id": "turn-1",
        "stream_id": "speaker-1",
        "seq": 0,
        "content_type": AUDIO_OUTPUT_CONTENT_TYPE,
        "payload_count": 1,
        "body": None,
    }
    values.update(overrides)
    reject(**values)


@pytest.mark.parametrize("payload", [b"", b"\x00"])
def test_audio_output_chunk_rejects_empty_or_partial_pcm_samples(payload):
    reject(
        type="audio.output.chunk",
        turn_id="turn-1",
        stream_id="speaker-1",
        seq=0,
        content_type=AUDIO_OUTPUT_CONTENT_TYPE,
        payload_count=1,
        body=None,
        payloads=(payload,),
    )


@pytest.mark.parametrize(
    "overrides",
    [
        {"seq": 0},
        {"payload_count": 1},
        {"content_type": AUDIO_OUTPUT_CONTENT_TYPE},
        {"body": {}},
    ],
)
def test_audio_output_done_rejects_stream_payload_fields(overrides):
    values = {
        "type": "audio.output.done",
        "turn_id": "turn-1",
        "stream_id": "speaker-1",
    }
    values.update(overrides)
    reject(**values)


@pytest.mark.parametrize(
    "overrides",
    [
        {"session_id": None},
        {"reply_to": None},
        {"conversation_id": "conversation-1"},
        {"turn_id": "turn-1"},
        {"seq": 0},
        {"body": {}},
    ],
)
def test_tool_approval_acknowledgements_reject_ambiguous_correlation(overrides):
    values = {
        "type": "tool.approve.accepted",
        "session_id": "session-1",
        "reply_to": "req-0",
        "body": None,
    }
    values.update(overrides)
    reject(**values)


@pytest.mark.parametrize(
    "overrides",
    [
        {"session_id": None},
        {"turn_id": None},
        {"seq": None},
        {"reply_to": "req-0"},
        {"body": {"tool_run_id": "tool-1"}},
        {"body": {"tool_run_id": "tool-1", "tool_name": "bad name"}},
    ],
)
def test_tool_events_reject_missing_correlation_or_noncanonical_body(overrides):
    values = {
        "type": "tool.queued",
        "session_id": "session-1",
        "turn_id": "turn-1",
        "seq": 0,
        "body": {"tool_run_id": "tool-1", "tool_name": "safe.tool"},
    }
    values.update(overrides)
    reject(**values)


def test_tool_waiting_completed_and_failed_events_validate_typed_status_fields():
    reject(
        type="tool.waiting",
        session_id="session-1",
        turn_id="turn-1",
        seq=0,
        body={
            "tool_run_id": "tool-1",
            "tool_name": "safe.tool",
            "kind": "question",
            "prompt": "Continue?",
        },
    )
    reject(
        type="tool.completed",
        session_id="session-1",
        turn_id="turn-1",
        seq=0,
        body={"tool_run_id": "tool-1", "tool_name": "safe.tool", "success": 1},
    )
    reject(
        type="tool.failed",
        session_id="session-1",
        turn_id="turn-1",
        seq=0,
        body={"tool_run_id": "tool-1", "tool_name": "safe.tool", "reason": "bad\nreason"},
    )


@pytest.mark.parametrize(
    "body",
    [
        {"capabilities": "open_uri"},
        {"capabilities": [{"id": "open_uri"}]},
        {"capabilities": [{"id": "unknown", "version": 1}]},
        {"capabilities": [{"id": "open_uri", "version": 2}]},
        {
            "capabilities": [
                {"id": "open_uri", "version": 1},
                {"id": "open_uri", "version": 1},
            ]
        },
    ],
)
def test_capability_snapshot_rejects_malformed_or_duplicated_authority(body):
    reject(type="capability.snapshot", session_id="session-1", body=body)


def test_capability_snapshot_rejects_trace_and_reply_direction_confusion():
    reject(
        type="capability.snapshot",
        session_id="session-1",
        trace_id="trace-1",
        body={"capabilities": []},
    )
    reject(
        type="capability.snapshot",
        session_id="session-1",
        reply_to="req-0",
        body={"capabilities": []},
    )
    reject(
        type="capability.snapshot.ok",
        session_id="session-1",
        body={"capabilities": []},
    )


@pytest.mark.parametrize(
    "body",
    [
        {
            "action_id": "action-1",
            "action_seq": 0,
            "capability": "open_uri",
            "args": {"uri": "https://example.test"},
            "deadline_ns": 10,
            "idempotency": "at_most_once",
        },
        {
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "unknown",
            "args": {},
            "deadline_ns": 10,
            "idempotency": "at_most_once",
        },
        {
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "open_uri",
            "args": "https://example.test",
            "deadline_ns": 10,
            "idempotency": "at_most_once",
        },
        {
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "open_uri",
            "args": {"uri": ""},
            "deadline_ns": 10,
            "idempotency": "at_most_once",
        },
        {
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "open_app",
            "args": {"app": ""},
            "deadline_ns": 10,
            "idempotency": "at_most_once",
        },
        {
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "open_app",
            "args": {"wrong": "app"},
            "deadline_ns": 10,
            "idempotency": "at_most_once",
        },
        {
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "open_uri",
            "args": {"uri": "https://example.test"},
            "deadline_ns": 0,
            "idempotency": "at_most_once",
        },
        {
            "action_id": "action-1",
            "action_seq": 1,
            "capability": "open_uri",
            "args": {"uri": "https://example.test"},
            "deadline_ns": 10,
            "idempotency": "maybe",
        },
    ],
)
def test_device_action_request_rejects_invalid_authority_and_effect_arguments(body):
    reject(type="device.action.request", session_id="session-1", body=body)


def test_device_action_request_rejects_reply_and_invalid_common_correlation():
    body = {
        "action_id": "action-1",
        "action_seq": 1,
        "capability": "open_uri",
        "args": {"uri": "https://example.test"},
        "deadline_ns": 10,
        "idempotency": "at_most_once",
    }
    reject(
        type="device.action.request",
        session_id="session-1",
        reply_to="req-0",
        body=body,
    )
    reject(
        type="device.action.request",
        session_id="session-1",
        turn_id="turn-1",
        body=body,
    )


@pytest.mark.parametrize(
    ("message_type", "body"),
    [
        ("device.action.cancel", {"action_id": "action-1", "reason": "bad\nreason"}),
        ("device.action.accepted", {"action_id": "action-1", "extra": True}),
        ("device.action.result", {"action_id": "action-1", "outcome": "failed"}),
        ("device.action.error", {"action_id": "action-1", "code": "made_up"}),
        (
            "device.action.error",
            {"action_id": "action-1", "code": "failed", "message": "bad\nmessage"},
        ),
    ],
)
def test_device_action_terminal_messages_reject_forged_or_unbounded_outcomes(message_type, body):
    reject(type=message_type, session_id="session-1", body=body)
