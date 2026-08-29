from __future__ import annotations

import pytest

from zara.protocol import (
    ProtocolLimits,
    ProtocolMessage,
    ProtocolValidationError,
    decode_message,
    encode_message,
)


LIMITS = ProtocolLimits(
    max_envelope_bytes=2048,
    max_payload_frames=4,
    max_payload_frame_bytes=2048,
    max_payload_bytes=4096,
)
PCM_FRAME = b"\x00\x00" * 512
CONTENT_TYPE = "audio/pcm;codec=pcm_s16le"


def message(message_type: str, **overrides) -> ProtocolMessage:
    values = {
        "type": message_type,
        "id": "voice-req",
        "timestamp_ns": 123,
        "payload_count": 0,
        "stream_id": "mic-1",
    }
    values.update(overrides)
    return ProtocolMessage(**values)


def test_audio_input_start_golden_contract_round_trips_without_payload():
    original = message(
        "audio.input.start",
        body={
            "codec": "pcm_s16le",
            "sample_rate": 16000,
            "channels": 1,
            "frame_samples": 512,
        },
    )

    decoded = decode_message(encode_message(original, limits=LIMITS), limits=LIMITS)

    assert decoded.message == original
    assert decoded.payloads == ()


def test_audio_input_chunk_golden_contract_is_one_exact_vad_frame():
    original = message(
        "audio.input.chunk",
        payload_count=1,
        seq=0,
        content_type=CONTENT_TYPE,
    )

    decoded = decode_message(
        encode_message(original, payloads=(PCM_FRAME,), limits=LIMITS),
        limits=LIMITS,
    )

    assert decoded.message == original
    assert decoded.payloads == (PCM_FRAME,)


@pytest.mark.parametrize(
    "candidate,payloads,match",
    [
        (message("audio.input.chunk", payload_count=1, seq=0, content_type=CONTENT_TYPE, stream_id=None), (PCM_FRAME,), "stream_id"),
        (message("audio.input.chunk", payload_count=1, seq=None, content_type=CONTENT_TYPE), (PCM_FRAME,), "seq"),
        (message("audio.input.chunk", payload_count=1, seq=0, content_type=None), (PCM_FRAME,), "content_type"),
        (message("audio.input.chunk", payload_count=0, seq=0, content_type=CONTENT_TYPE), (), "payload"),
        (message("audio.input.chunk", payload_count=2, seq=0, content_type=CONTENT_TYPE), (PCM_FRAME, PCM_FRAME), "payload"),
        (message("audio.input.chunk", payload_count=1, seq=0, content_type=CONTENT_TYPE), (PCM_FRAME[:-2],), "1024"),
        (message("audio.input.chunk", payload_count=1, seq=0, content_type=CONTENT_TYPE), (PCM_FRAME + b"\x00\x00",), "1024"),
        (message("audio.input.chunk", payload_count=1, seq=0, content_type="audio/pcm"), (PCM_FRAME,), "content_type"),
    ],
)
def test_audio_input_chunk_rejects_ambiguous_or_nonbaseline_shape(candidate, payloads, match):
    with pytest.raises(ProtocolValidationError, match=match):
        encode_message(candidate, payloads=payloads, limits=LIMITS)


@pytest.mark.parametrize(
    "message_type",
    ["audio.input.start", "audio.input.commit", "audio.input.cancel"],
)
def test_audio_input_lifecycle_messages_reject_binary_payloads(message_type):
    candidate = message(message_type, payload_count=1)

    with pytest.raises(ProtocolValidationError, match="payload"):
        encode_message(candidate, payloads=(PCM_FRAME,), limits=LIMITS)


@pytest.mark.parametrize(
    "body",
    [
        None,
        {},
        {"codec": "pcm_s16le", "sample_rate": 8000, "channels": 1, "frame_samples": 512},
        {"codec": "pcm_s16le", "sample_rate": 16000, "channels": 2, "frame_samples": 512},
        {"codec": "pcm_s16le", "sample_rate": 16000, "channels": 1, "frame_samples": 160},
        {"codec": "opus", "sample_rate": 16000, "channels": 1, "frame_samples": 512},
    ],
)
def test_audio_input_start_rejects_nonbaseline_codec_geometry(body):
    candidate = message("audio.input.start", body=body)

    with pytest.raises(ProtocolValidationError, match="audio.input.start"):
        encode_message(candidate, limits=LIMITS)


@pytest.mark.parametrize("message_type", ["audio.input.commit", "audio.input.cancel"])
def test_audio_terminal_messages_require_stream_and_no_sequence(message_type):
    with pytest.raises(ProtocolValidationError, match="stream_id"):
        encode_message(message(message_type, stream_id=None), limits=LIMITS)

    with pytest.raises(ProtocolValidationError, match="seq"):
        encode_message(message(message_type, seq=0), limits=LIMITS)
