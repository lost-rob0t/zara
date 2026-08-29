from __future__ import annotations

import concurrent.futures
import threading

import numpy as np

from zara.runtime.commands import SubmitTurn
from zara.server import PrincipalContext
from zara.streaming_stt import FinalTranscript
from zara.voice_runtime import RuntimeVoiceIngress


PCM_FRAME = (np.arange(512, dtype=np.int16) - 256).astype("<i2").tobytes()


class CommitAwareTranscriber:
    def __init__(self) -> None:
        self.calls = []

    def start_turn(self, turn_id: str) -> None:
        self.calls.append(("start", turn_id))

    def feed(self, chunk: np.ndarray):
        self.calls.append(("feed", chunk.copy()))
        return []

    def commit(self, turn_id: str):
        self.calls.append(("commit", turn_id))
        return [
            FinalTranscript(
                turn_id=turn_id,
                text="committed utterance",
                text_length=len("committed utterance"),
                provider="fixture",
            )
        ]

    def cancel(self, turn_id=None) -> None:
        self.calls.append(("cancel", turn_id))


class RecordingSupervisor:
    def __init__(self) -> None:
        self.submissions = []
        self.submitted = threading.Event()

    def submit(self, principal, command):
        self.submissions.append((principal, command))
        self.submitted.set()
        future = concurrent.futures.Future()
        future.set_result(None)
        return future


def test_commit_flushes_queued_audio_before_submitting_final_transcript():
    transcriber = CommitAwareTranscriber()
    supervisor = RecordingSupervisor()
    principal = PrincipalContext("user:alice")
    ingress = RuntimeVoiceIngress(
        supervisor,
        principal=principal,
        transcriber_factory=lambda **_kwargs: transcriber,
        queue_size=4,
    )
    common = {
        "principal": principal,
        "conversation_id": "conversation-a",
        "stream_id": "mic-1",
        "trace_id": "trace-voice-commit",
    }

    try:
        ingress.start(**common)
        ingress.chunk(PCM_FRAME, **common, seq=0)
        ingress.commit(**common)

        assert supervisor.submitted.wait(1.0)
        assert [call[0] for call in transcriber.calls[:3]] == ["start", "feed", "commit"]
        assert len(supervisor.submissions) == 1
        submitted_principal, command = supervisor.submissions[0]
        assert submitted_principal == principal
        assert isinstance(command, SubmitTurn)
        assert command.text == "committed utterance"
        assert command.conversation_id == "conversation-a"
        assert command.request_id == "trace-voice-commit"
    finally:
        ingress.close(timeout=1.0)
