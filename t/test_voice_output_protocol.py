from __future__ import annotations

import pytest

from zara.protocol import (
    ProtocolMessage,
    ProtocolValidationError,
    decode_message,
    encode_message,
)


OUTPUT_CONTENT_TYPE = "audio/pcm;codec=pcm_s16le"
OUTPUT_FORMAT = {
    "codec": "pcm_s16le",
    "sample_rate": 24000,
    "channels": 1,
}
PCM_OUTPUT = b"\x01\x00" * 240


def output_start(**overrides) -> ProtocolMessage:
    values = {
        "type": "audio.output.start",
        "id": "output-start",
        "timestamp_ns": 1,
        "payload_count": 0,
        "turn_id": "turn-1",
        "stream_id": "speaker-1",
        "trace_id": "trace-1",
        "body": OUTPUT_FORMAT,
    }
    values.update(overrides)
    return ProtocolMessage(**values)


def output_chunk(**overrides) -> ProtocolMessage:
    values = {
        "type": "audio.output.chunk",
        "id": "output-chunk",
        "timestamp_ns": 2,
        "payload_count": 1,
        "turn_id": "turn-1",
        "stream_id": "speaker-1",
        "seq": 0,
        "trace_id": "trace-1",
        "content_type": OUTPUT_CONTENT_TYPE,
    }
    values.update(overrides)
    return ProtocolMessage(**values)


def output_done(**overrides) -> ProtocolMessage:
    values = {
        "type": "audio.output.done",
        "id": "output-done",
        "timestamp_ns": 3,
        "payload_count": 0,
        "turn_id": "turn-1",
        "stream_id": "speaker-1",
        "trace_id": "trace-1",
    }
    values.update(overrides)
    return ProtocolMessage(**values)


def test_output_pcm_messages_round_trip_as_binary_multipart():
    started = decode_message(encode_message(output_start())).message
    chunk_frames = encode_message(output_chunk(), payloads=(PCM_OUTPUT,))
    chunk = decode_message(chunk_frames)
    done = decode_message(encode_message(output_done())).message

    assert started.body == OUTPUT_FORMAT
    assert chunk.message.seq == 0
    assert chunk.payloads == (PCM_OUTPUT,)
    assert done.stream_id == "speaker-1"


@pytest.mark.parametrize("message_type", ["audio.output.start", "audio.output.chunk", "audio.output.done"])
@pytest.mark.parametrize("missing_field", ["turn_id", "stream_id"])
def test_output_messages_require_turn_and_stream_correlation(message_type, missing_field):
    factory = {
        "audio.output.start": output_start,
        "audio.output.chunk": output_chunk,
        "audio.output.done": output_done,
    }[message_type]
    message = factory(**{missing_field: None})
    payloads = (PCM_OUTPUT,) if message_type == "audio.output.chunk" else ()

    with pytest.raises(ProtocolValidationError):
        encode_message(message, payloads=payloads)


def test_output_start_requires_negotiated_pcm_shape_and_no_payload_fields():
    with pytest.raises(ProtocolValidationError):
        encode_message(output_start(body={"codec": "mp3", "sample_rate": 24000, "channels": 1}))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_start(body={"codec": "pcm_s16le", "sample_rate": 0, "channels": 1}))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_start(body={"codec": "pcm_s16le", "sample_rate": 24000, "channels": 0}))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_start(seq=0))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_start(content_type=OUTPUT_CONTENT_TYPE))


def test_output_chunk_requires_monotonic_seq_pcm_content_type_and_one_binary_payload():
    with pytest.raises(ProtocolValidationError):
        encode_message(output_chunk(seq=None), payloads=(PCM_OUTPUT,))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_chunk(content_type=None), payloads=(PCM_OUTPUT,))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_chunk(content_type="audio/mpeg"), payloads=(PCM_OUTPUT,))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_chunk(payload_count=0))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_chunk(body={"hidden": "metadata"}), payloads=(PCM_OUTPUT,))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_chunk(), payloads=(b"\x00",))


def test_output_done_is_terminal_metadata_only():
    with pytest.raises(ProtocolValidationError):
        encode_message(output_done(seq=1))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_done(content_type=OUTPUT_CONTENT_TYPE))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_done(body={"status": "done"}))
    with pytest.raises(ProtocolValidationError):
        encode_message(output_done(payload_count=1), payloads=(PCM_OUTPUT,))
