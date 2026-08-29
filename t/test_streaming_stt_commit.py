from __future__ import annotations

import numpy as np

from zara.streaming_stt import (
    FinalTranscript,
    SpeechEnded,
    StreamingTranscriber,
    VADConfig,
    VAD_CHUNK_SAMPLES,
)


class FakeVADDetector:
    def __init__(self, probabilities) -> None:
        self._probabilities = list(probabilities)
        self._index = 0

    def reset(self) -> None:
        self._index = 0

    def process_chunk(self, _audio_bytes: bytes) -> float:
        if self._index >= len(self._probabilities):
            return 0.01
        probability = self._probabilities[self._index]
        self._index += 1
        return probability


def frame() -> np.ndarray:
    return np.ones(VAD_CHUNK_SAMPLES, dtype=np.float32) * 0.1


def test_commit_finalizes_confirmed_speech_without_waiting_for_trailing_silence():
    transcriber = StreamingTranscriber(
        transcribe_fn=lambda _audio: "push to talk command",
        config=VADConfig(
            min_speech_frames=2,
            trailing_silence_frames=20,
            partial_interval_frames=999,
        ),
        vad_detector=FakeVADDetector([0.9, 0.9, 0.9]),
    )
    transcriber.start_turn("voice-1")
    transcriber.feed(frame())
    transcriber.feed(frame())
    transcriber.feed(frame())

    events = transcriber.commit("voice-1")

    ended = [event for event in events if isinstance(event, SpeechEnded)]
    finals = [event for event in events if isinstance(event, FinalTranscript)]
    assert [(event.turn_id, event.reason) for event in ended] == [("voice-1", "commit")]
    assert [(event.turn_id, event.text) for event in finals] == [
        ("voice-1", "push to talk command")
    ]
    assert transcriber.commit("voice-1") == []


def test_commit_without_confirmed_speech_does_not_emit_final_transcript():
    transcribe_calls = []

    def transcribe(audio):
        transcribe_calls.append(audio.copy())
        return "should never execute"

    transcriber = StreamingTranscriber(
        transcribe_fn=transcribe,
        config=VADConfig(min_speech_frames=3, trailing_silence_frames=20),
        vad_detector=FakeVADDetector([0.01, 0.01]),
    )
    transcriber.start_turn("voice-silent")
    transcriber.feed(frame())
    transcriber.feed(frame())

    assert transcriber.commit("voice-silent") == []
    assert transcribe_calls == []
