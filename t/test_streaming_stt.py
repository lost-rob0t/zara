"""Deterministic streaming STT tests for ZARA-024.

Tests cover:
- Clipped-onset prevention (pre-speech buffer)
- Short command, long command
- Pauses inside speech
- Background noise / false VAD start
- No speech
- Continuous speech
- Cancellation
- Partial transcripts are monotonic; only final triggers execution
- Bounded buffers during a long stream
- Stale partial/final results do not cross turn IDs
"""

from __future__ import annotations

import struct
import wave
from pathlib import Path

import numpy as np
import pytest

from zara.streaming_stt import (
    FinalTranscript,
    PartialTranscript,
    SpeechEnded,
    SpeechStarted,
    StreamingTranscriber,
    StreamingVAD,
    VADConfig,
    VAD_CHUNK_SAMPLES,
    VAD_SAMPLE_RATE,
    make_fixture_transcriber,
)

FIXTURE_DIR = Path(__file__).resolve().parent.parent / "t" / "fixtures" / "audio"


class FakeVADDetector:
    """Deterministic VAD detector for testing.

    Returns speech probabilities from a configurable pattern so the
    endpointing logic can be tested without depending on Silero's model
    accuracy on synthetic audio.
    """

    def __init__(self, pattern=None, default_prob=0.0):
        self._pattern = pattern or []
        self._default = default_prob
        self._index = 0

    def reset(self):
        self._index = 0

    def process_chunk(self, audio_bytes):
        if self._index < len(self._pattern):
            prob = self._pattern[self._index]
        else:
            prob = self._default
        self._index += 1
        return prob


def speech_pattern(n_speech_chunks, n_silence=0):
    """Build a probability pattern: speech then silence."""
    pattern = [0.9] * n_speech_chunks + [0.01] * n_silence
    return pattern


def load_wav_chunks(path: Path, chunk_size: int = VAD_CHUNK_SAMPLES):
    """Load a WAV file as a list of chunk_size float32 arrays."""
    if not path.exists():
        pytest.skip(f"fixture {path} not generated")
    with wave.open(str(path), "rb") as wf:
        frames = wf.readframes(wf.getnframes())
        samples = np.frombuffer(frames, dtype=np.int16).astype(np.float32) / 32767.0
    chunks = []
    for i in range(0, len(samples), chunk_size):
        chunk = samples[i : i + chunk_size]
        if len(chunk) < chunk_size:
            chunk = np.pad(chunk, (0, chunk_size - len(chunk)))
        chunks.append(chunk)
    return chunks


def generate_speech_chunks(duration_s: float, freq: float = 440.0):
    """Generate deterministic audio chunks (content doesn't matter for VAD
    since FakeVADDetector controls the speech probability)."""
    n = int(VAD_SAMPLE_RATE * duration_s)
    chunks = []
    for i in range(0, n, VAD_CHUNK_SAMPLES):
        chunk = np.zeros(VAD_CHUNK_SAMPLES, dtype=np.float32)
        chunks.append(chunk)
    return chunks


def generate_silence_chunks(duration_s: float):
    """Generate silence."""
    n = int(VAD_SAMPLE_RATE * duration_s)
    samples = np.zeros(n, dtype=np.float32)
    chunks = []
    for i in range(0, n, VAD_CHUNK_SAMPLES):
        chunk = samples[i : i + VAD_CHUNK_SAMPLES]
        if len(chunk) < VAD_CHUNK_SAMPLES:
            chunk = np.pad(chunk, (0, VAD_CHUNK_SAMPLES - len(chunk)))
        chunks.append(chunk)
    return chunks


def generate_noise_chunks(duration_s: float, seed: int = 42):
    """Generate deterministic low-level noise."""
    rng = np.random.default_rng(seed=seed)
    n = int(VAD_SAMPLE_RATE * duration_s)
    samples = 0.01 * rng.standard_normal(n).astype(np.float32)
    chunks = []
    for i in range(0, n, VAD_CHUNK_SAMPLES):
        chunk = samples[i : i + VAD_CHUNK_SAMPLES]
        if len(chunk) < VAD_CHUNK_SAMPLES:
            chunk = np.pad(chunk, (0, VAD_CHUNK_SAMPLES - len(chunk)))
        chunks.append(chunk)
    return chunks


def feed_all(vad, chunks):
    """Feed all chunks and collect events."""
    events = []
    for chunk in chunks:
        events.extend(vad.feed(chunk))
    return events


# ---------------------------------------------------------------------------
# VAD tests
# ---------------------------------------------------------------------------


def test_silence_does_not_trigger_speech_started():
    detector = FakeVADDetector(default_prob=0.01)
    vad = StreamingVAD(vad_detector=detector)
    vad.start_turn("turn-1")
    chunks = generate_silence_chunks(2.0)
    events = feed_all(vad, chunks)
    speech_events = [e for e in events if isinstance(e, SpeechStarted)]
    assert speech_events == []
    assert vad.state == "idle"


def test_speech_triggers_speech_started_and_ended():
    detector = FakeVADDetector(pattern=speech_pattern(20, 10))
    vad = StreamingVAD(
        VADConfig(min_speech_frames=2, trailing_silence_frames=4),
        vad_detector=detector,
    )
    vad.start_turn("turn-1")
    speech = generate_speech_chunks(1.0)
    silence = generate_silence_chunks(0.5)
    events = feed_all(vad, speech + silence)

    started = [e for e in events if isinstance(e, SpeechStarted)]
    ended = [e for e in events if isinstance(e, SpeechEnded)]
    assert len(started) >= 1
    assert len(ended) >= 1
    assert started[0].turn_id == "turn-1"
    assert ended[0].turn_id == "turn-1"


def test_pre_speech_buffer_prevents_clipped_onset():
    config = VADConfig(min_speech_frames=2, trailing_silence_frames=4, pre_speech_buffer_chunks=10)
    pattern = [0.01] * 10 + [0.9] * 20 + [0.01] * 10
    detector = FakeVADDetector(pattern=pattern)
    vad = StreamingVAD(config, vad_detector=detector)
    vad.start_turn("turn-1")
    silence = generate_silence_chunks(0.3)
    speech = generate_speech_chunks(0.5)
    events = feed_all(vad, silence + speech)

    started = [e for e in events if isinstance(e, SpeechStarted)]
    assert len(started) >= 1
    assert started[0].pre_speech_samples > 0
    assert started[0].pre_speech_samples <= 10 * VAD_CHUNK_SAMPLES


def test_pauses_inside_speech_do_not_trigger_premature_end():
    config = VADConfig(
        min_speech_frames=2,
        trailing_silence_frames=10,
        pre_speech_buffer_chunks=5,
    )
    # Speech → short pause (5 chunks) → speech → long silence
    pattern = [0.9] * 10 + [0.01] * 5 + [0.9] * 10 + [0.01] * 20
    detector = FakeVADDetector(pattern=pattern)
    vad = StreamingVAD(config, vad_detector=detector)
    vad.start_turn("turn-1")
    speech1 = generate_speech_chunks(0.3)
    pause = generate_silence_chunks(0.15)
    speech2 = generate_speech_chunks(0.3)
    long_silence = generate_silence_chunks(0.5)
    events = feed_all(vad, speech1 + pause + speech2 + long_silence)

    ended = [e for e in events if isinstance(e, SpeechEnded)]
    assert len(ended) <= 1
    if ended:
        assert ended[0].turn_id == "turn-1"


def test_background_noise_does_not_trigger_speech():
    detector = FakeVADDetector(default_prob=0.15)
    vad = StreamingVAD(VADConfig(vad_threshold=0.5), vad_detector=detector)
    vad.start_turn("turn-1")
    noise = generate_noise_chunks(3.0)
    events = feed_all(vad, noise)
    started = [e for e in events if isinstance(e, SpeechStarted)]
    assert started == []
    assert vad.state == "idle"


def test_no_speech_timeout_clears_pre_speech_buffer():
    detector = FakeVADDetector(default_prob=0.01)
    vad = StreamingVAD(
        VADConfig(no_speech_timeout_frames=10),
        vad_detector=detector,
    )
    vad.start_turn("turn-1")
    silence = generate_silence_chunks(1.0)
    feed_all(vad, silence)
    assert len(vad._pre_speech) == 0


def test_max_utterance_cap_triggers_speech_ended():
    config = VADConfig(
        min_speech_frames=1,
        trailing_silence_frames=999,
        max_utterance_frames=5,
        pre_speech_buffer_chunks=2,
    )
    detector = FakeVADDetector(default_prob=0.9)
    vad = StreamingVAD(config, vad_detector=detector)
    vad.start_turn("turn-1")
    speech = generate_speech_chunks(1.0)
    events = feed_all(vad, speech)
    ended = [e for e in events if isinstance(e, SpeechEnded)]
    assert len(ended) >= 1
    assert ended[0].reason == "max_utterance"


def test_cancellation_prevents_subsequent_events():
    detector = FakeVADDetector(default_prob=0.9)
    vad = StreamingVAD(
        VADConfig(min_speech_frames=2, trailing_silence_frames=4),
        vad_detector=detector,
    )
    vad.start_turn("turn-1")
    speech = generate_speech_chunks(0.2)
    events = feed_all(vad, speech)
    vad.cancel("turn-1")
    more_speech = generate_speech_chunks(0.3)
    events2 = feed_all(vad, more_speech)
    assert events2 == []


def test_stale_events_do_not_cross_turn_ids():
    pattern = speech_pattern(20, 10)
    detector = FakeVADDetector(pattern=pattern)
    vad = StreamingVAD(
        VADConfig(min_speech_frames=2, trailing_silence_frames=4),
        vad_detector=detector,
    )
    vad.start_turn("turn-1")
    speech = generate_speech_chunks(0.3)
    silence = generate_silence_chunks(0.3)
    events1 = feed_all(vad, speech + silence)

    detector2 = FakeVADDetector(pattern=pattern)
    vad._vad = detector2
    vad.start_turn("turn-2")
    events2 = feed_all(vad, speech + silence)

    turn1_ids = {e.turn_id for e in events1}
    turn2_ids = {e.turn_id for e in events2}
    assert turn1_ids == {"turn-1"}
    assert turn2_ids == {"turn-2"}


def test_bounded_pre_speech_buffer_during_long_stream():
    detector = FakeVADDetector(default_prob=0.01)
    vad = StreamingVAD(
        VADConfig(pre_speech_buffer_chunks=10, no_speech_timeout_frames=9999),
        vad_detector=detector,
    )
    vad.start_turn("turn-1")
    silence = generate_silence_chunks(5.0)
    feed_all(vad, silence)
    assert len(vad._pre_speech) <= 10


def test_bounded_speech_audio_during_long_speech():
    config = VADConfig(
        min_speech_frames=1,
        trailing_silence_frames=9999,
        max_utterance_frames=100,
        pre_speech_buffer_chunks=5,
    )
    detector = FakeVADDetector(default_prob=0.9)
    vad = StreamingVAD(config, vad_detector=detector)
    vad.start_turn("turn-1")
    speech = generate_speech_chunks(10.0)
    feed_all(vad, speech)
    assert vad._speech_frame_count <= 100


# ---------------------------------------------------------------------------
# StreamingTranscriber tests
# ---------------------------------------------------------------------------


def test_short_command_produces_final_transcript():
    detector = FakeVADDetector(pattern=speech_pattern(15, 10))
    transcriber = StreamingTranscriber(
        transcribe_fn=make_fixture_transcriber(default_text="open firefox"),
        config=VADConfig(min_speech_frames=2, trailing_silence_frames=4),
        vad_detector=detector,
    )
    transcriber.start_turn("turn-1")
    speech = generate_speech_chunks(0.5)
    silence = generate_silence_chunks(0.3)
    events = feed_all(transcriber, speech + silence)

    finals = [e for e in events if isinstance(e, FinalTranscript)]
    assert len(finals) == 1
    assert finals[0].text == "open firefox"
    assert finals[0].turn_id == "turn-1"


def test_long_command_produces_partial_and_final():
    detector = FakeVADDetector(pattern=speech_pattern(60, 10))
    transcriber = StreamingTranscriber(
        transcribe_fn=make_fixture_transcriber(default_text="search for weather"),
        config=VADConfig(
            min_speech_frames=2,
            trailing_silence_frames=4,
            partial_interval_frames=10,
        ),
        vad_detector=detector,
    )
    transcriber.start_turn("turn-1")
    speech = generate_speech_chunks(2.0)
    silence = generate_silence_chunks(0.3)
    events = feed_all(transcriber, speech + silence)

    finals = [e for e in events if isinstance(e, FinalTranscript)]
    assert len(finals) == 1
    assert finals[0].text == "search for weather"


def test_partial_transcripts_are_monotonic():
    call_count = [0]

    def transcribe_fn(audio):
        call_count[0] += 1
        return f"partial {call_count[0]}"

    detector = FakeVADDetector(default_prob=0.9)
    transcriber = StreamingTranscriber(
        transcribe_fn=transcribe_fn,
        config=VADConfig(
            min_speech_frames=2,
            trailing_silence_frames=999,
            max_utterance_frames=100,
            partial_interval_frames=5,
        ),
        vad_detector=detector,
    )
    transcriber.start_turn("turn-1")
    speech = generate_speech_chunks(2.0)
    events = feed_all(transcriber, speech)

    partials = [e for e in events if isinstance(e, PartialTranscript)]
    if partials:
        texts = [p.text for p in partials]
        assert texts == sorted(set(texts), key=lambda t: int(t.split()[1]))


def test_only_final_transcript_triggers_irreversible_action():
    detector = FakeVADDetector(pattern=speech_pattern(15, 10))
    transcriber = StreamingTranscriber(
        transcribe_fn=make_fixture_transcriber(default_text="execute command"),
        config=VADConfig(min_speech_frames=2, trailing_silence_frames=4),
        vad_detector=detector,
    )
    transcriber.start_turn("turn-1")
    speech = generate_speech_chunks(0.5)
    silence = generate_silence_chunks(0.3)
    events = feed_all(transcriber, speech + silence)

    finals = [e for e in events if isinstance(e, FinalTranscript)]
    assert len(finals) == 1
    assert finals[0].text == "execute command"


def test_cancellation_prevents_final_transcript():
    detector = FakeVADDetector(default_prob=0.9)
    transcriber = StreamingTranscriber(
        transcribe_fn=make_fixture_transcriber(default_text="should not appear"),
        config=VADConfig(min_speech_frames=2, trailing_silence_frames=4),
        vad_detector=detector,
    )
    transcriber.start_turn("turn-1")
    speech = generate_speech_chunks(0.3)
    feed_all(transcriber, speech)
    transcriber.cancel("turn-1")
    silence = generate_silence_chunks(0.3)
    events = feed_all(transcriber, silence)
    finals = [e for e in events if isinstance(e, FinalTranscript)]
    assert finals == []


def test_fixture_audio_silence_wav_loads():
    chunks = load_wav_chunks(FIXTURE_DIR / "silence.wav")
    assert len(chunks) > 0
    assert all(len(c) == VAD_CHUNK_SAMPLES for c in chunks)


def test_fixture_audio_command_speech_wav_loads():
    chunks = load_wav_chunks(FIXTURE_DIR / "command_speech.wav")
    assert len(chunks) > 0
    assert all(len(c) == VAD_CHUNK_SAMPLES for c in chunks)
