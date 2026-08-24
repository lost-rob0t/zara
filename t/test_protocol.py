from __future__ import annotations

import json

import pytest

from zara.protocol import (
    PROTOCOL_MARKER,
    RESERVED_MESSAGE_TYPES,
    ProtocolLimits,
    ProtocolMessage,
    ProtocolValidationError,
    ProtocolVersionError,
    decode_message,
    encode_message,
)


LIMITS = ProtocolLimits(
    max_envelope_bytes=512,
    max_payload_frames=2,
    max_payload_frame_bytes=8,
    max_payload_bytes=12,
    max_id_bytes=16,
    max_type_bytes=32,
)


def message(**overrides) -> ProtocolMessage:
    values = {
        "type": "hello",
        "id": "req-1",
        "timestamp_ns": 123456789,
        "payload_count": 0,
        "body": {"versions": [1]},
    }
    values.update(overrides)
    return ProtocolMessage(**values)


def envelope_frame(**overrides) -> bytes:
    values = {
        "type": "hello",
        "id": "req-1",
        "timestamp_ns": 123456789,
        "payload_count": 0,
        "body": {"versions": [1]},
    }
    values.update(overrides)
    return json.dumps(values, sort_keys=True, separators=(",", ":")).encode("utf-8")


def test_golden_hello_encoding_is_deterministic():
    frames = encode_message(message(), limits=LIMITS)

    assert frames == [
        b"ZARA/1",
        b'{"body":{"versions":[1]},"id":"req-1","payload_count":0,"timestamp_ns":123456789,"type":"hello"}',
    ]


def test_valid_message_round_trips_with_binary_payloads():
    original = message(payload_count=2)
    payloads = (b"12345678", b"abcd")

    frames = encode_message(original, payloads=payloads, limits=LIMITS)
    decoded = decode_message(frames, limits=LIMITS)

    assert decoded.message == original
    assert decoded.payloads == payloads


def test_protocol_marker_is_exact_major_version():
    assert PROTOCOL_MARKER == b"ZARA/1"

    with pytest.raises(ProtocolVersionError):
        decode_message([b"ZARA/2", envelope_frame()], limits=LIMITS)

    with pytest.raises(ProtocolVersionError):
        decode_message([b"ZARA/01", envelope_frame()], limits=LIMITS)

    with pytest.raises(ProtocolVersionError):
        decode_message([b" zara/1", envelope_frame()], limits=LIMITS)


@pytest.mark.parametrize(
    "frame",
    [
        b"\xff",
        b"{",
        b"[]",
        b"null",
        b'"hello"',
    ],
)
def test_invalid_utf8_json_or_non_object_envelope_is_rejected(frame):
    with pytest.raises(ProtocolValidationError):
        decode_message([PROTOCOL_MARKER, frame], limits=LIMITS)


def test_duplicate_json_keys_are_rejected():
    frame = b'{"type":"hello","type":"turn.submit","id":"req-1","timestamp_ns":1,"payload_count":0}'

    with pytest.raises(ProtocolValidationError, match="duplicate"):
        decode_message([PROTOCOL_MARKER, frame], limits=LIMITS)


@pytest.mark.parametrize("constant", [b"NaN", b"Infinity", b"-Infinity"])
def test_non_finite_json_constants_are_rejected(constant):
    frame = (
        b'{"type":"hello","id":"req-1","timestamp_ns":1,"payload_count":0,"body":{"value":'
        + constant
        + b"}}"
    )

    with pytest.raises(ProtocolValidationError):
        decode_message([PROTOCOL_MARKER, frame], limits=LIMITS)


@pytest.mark.parametrize("key", ["wat", "user_id", "principal_id", "routing_id"])
def test_unknown_or_identity_looking_top_level_keys_fail_closed(key):
    frame = envelope_frame(**{key: "attacker-controlled"})

    with pytest.raises(ProtocolValidationError, match="unknown"):
        decode_message([PROTOCOL_MARKER, frame], limits=LIMITS)


def test_unknown_message_type_fails_closed():
    frame = envelope_frame(type="python.import.whatever")

    with pytest.raises(ProtocolValidationError, match="message type"):
        decode_message([PROTOCOL_MARKER, frame], limits=LIMITS)


def test_audio_names_are_reserved_and_fail_closed_without_live_contract():
    assert "audio.input.chunk" in RESERVED_MESSAGE_TYPES

    with pytest.raises(ProtocolValidationError):
        decode_message(
            [
                PROTOCOL_MARKER,
                envelope_frame(
                    type="audio.input.chunk",
                    stream_id="s1",
                    seq=1,
                    content_type="audio/pcm",
                ),
            ],
            limits=LIMITS,
        )


@pytest.mark.parametrize(
    "frames",
    [
        [PROTOCOL_MARKER, envelope_frame(payload_count=1)],
        [PROTOCOL_MARKER, envelope_frame(payload_count=0), b"extra"],
        [PROTOCOL_MARKER, envelope_frame(payload_count=1), b"a", b"extra"],
    ],
)
def test_payload_count_must_exactly_match_remaining_frames(frames):
    with pytest.raises(ProtocolValidationError, match="payload_count"):
        decode_message(frames, limits=LIMITS)


def test_payload_frame_count_limit_is_enforced():
    frame = envelope_frame(payload_count=3)

    with pytest.raises(ProtocolValidationError, match="payload"):
        decode_message([PROTOCOL_MARKER, frame, b"a", b"b", b"c"], limits=LIMITS)


def test_envelope_byte_limit_is_enforced_before_json_semantics():
    tiny = ProtocolLimits(max_envelope_bytes=8)

    with pytest.raises(ProtocolValidationError, match="envelope"):
        decode_message([PROTOCOL_MARKER, envelope_frame()], limits=tiny)


def test_payload_frame_size_limit_accepts_boundary_and_rejects_one_over():
    ok = decode_message(
        [PROTOCOL_MARKER, envelope_frame(payload_count=1), b"12345678"],
        limits=LIMITS,
    )
    assert ok.payloads == (b"12345678",)

    with pytest.raises(ProtocolValidationError, match="payload"):
        decode_message(
            [PROTOCOL_MARKER, envelope_frame(payload_count=1), b"123456789"],
            limits=LIMITS,
        )


def test_aggregate_payload_limit_is_enforced():
    with pytest.raises(ProtocolValidationError, match="aggregate"):
        decode_message(
            [PROTOCOL_MARKER, envelope_frame(payload_count=2), b"12345678", b"12345"],
            limits=LIMITS,
        )


@pytest.mark.parametrize(
    "overrides",
    [
        {"id": ""},
        {"id": "x" * 17},
        {"id": " space"},
        {"type": "HELLO"},
        {"type": "hello "},
        {"timestamp_ns": -1},
        {"timestamp_ns": True},
        {"payload_count": -1},
        {"payload_count": True},
        {"seq": -1},
        {"seq": True},
    ],
)
def test_invalid_identifiers_types_and_non_negative_integers_are_rejected(overrides):
    with pytest.raises(ProtocolValidationError):
        decode_message([PROTOCOL_MARKER, envelope_frame(**overrides)], limits=LIMITS)


@pytest.mark.parametrize(
    "flags",
    [
        {"wat": True},
        {"idempotent": "yes"},
        {"resume": 1},
        [],
    ],
)
def test_flags_are_closed_and_boolean_only(flags):
    with pytest.raises(ProtocolValidationError, match="flags"):
        decode_message(
            [PROTOCOL_MARKER, envelope_frame(flags=flags)],
            limits=LIMITS,
        )


def test_allowed_flags_round_trip():
    original = message(flags={"idempotent": True, "resume": False})
    decoded = decode_message(encode_message(original, limits=LIMITS), limits=LIMITS)

    assert decoded.message.flags == {"idempotent": True, "resume": False}


def test_encoder_rejects_payload_count_mismatch_before_emitting_frames():
    with pytest.raises(ProtocolValidationError, match="payload_count"):
        encode_message(message(payload_count=1), payloads=(), limits=LIMITS)


@pytest.mark.parametrize(
    ("message_type", "body"),
    [
        ("voice.speech.started", {"pre_speech_samples": 512}),
        ("voice.transcript.partial", {"text": "hello wor"}),
        ("voice.speech.ended", {"reason": "silence"}),
        ("voice.transcript.final", {"text": "hello world"}),
    ],
)
def test_visible_stt_event_shapes_round_trip_without_payload_or_turn_id(message_type, body):
    original = message(
        type=message_type,
        conversation_id="conversation-1",
        stream_id="mic-1",
        trace_id="trace-1",
        seq=7,
        body=body,
    )

    decoded = decode_message(encode_message(original, limits=LIMITS), limits=LIMITS)

    assert decoded.message == original
    assert decoded.payloads == ()


@pytest.mark.parametrize(
    "overrides",
    [
        {"stream_id": None},
        {"seq": None},
        {"turn_id": "fake-runtime-turn"},
        {"payload_count": 1},
        {"body": None},
        {"body": {"pre_speech_samples": -1}},
        {"body": {"pre_speech_samples": True}},
        {"body": {"pre_speech_samples": 512, "provider": "secret"}},
    ],
)
def test_visible_speech_started_shape_fails_closed(overrides):
    values = {
        "type": "voice.speech.started",
        "conversation_id": "conversation-1",
        "stream_id": "mic-1",
        "trace_id": "trace-1",
        "seq": 7,
        "body": {"pre_speech_samples": 512},
    }
    values.update(overrides)
    payloads = (b"x",) if values.get("payload_count") == 1 else ()

    with pytest.raises(ProtocolValidationError):
        encode_message(message(**values), payloads=payloads, limits=LIMITS)


@pytest.mark.parametrize(
    ("message_type", "body"),
    [
        ("voice.transcript.partial", {}),
        ("voice.transcript.partial", {"text": 3}),
        ("voice.transcript.partial", {"text": "safe", "provider": "secret"}),
        ("voice.speech.ended", {}),
        ("voice.speech.ended", {"reason": 3}),
        ("voice.transcript.final", {}),
        ("voice.transcript.final", {"text": 3}),
        ("voice.transcript.final", {"text": "safe", "provider": "secret"}),
    ],
)
def test_visible_stt_text_and_end_bodies_fail_closed(message_type, body):
    with pytest.raises(ProtocolValidationError):
        encode_message(
            message(
                type=message_type,
                conversation_id="conversation-1",
                stream_id="mic-1",
                trace_id="trace-1",
                seq=7,
                body=body,
            ),
            limits=LIMITS,
        )
