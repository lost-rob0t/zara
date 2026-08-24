from __future__ import annotations

import concurrent.futures
import threading

import numpy as np

from zara.runtime import events
from zara.runtime.commands import SubmitTurn
from zara.server import PrincipalContext
from zara.streaming_stt import (
    FinalTranscript,
    PartialTranscript,
    SpeechEnded,
    SpeechStarted,
)
from zara.voice_runtime import RuntimeVoiceIngress


PCM_FRAME = np.zeros(512, dtype="<i2").tobytes()


class EventfulTranscriber:
    def __init__(self) -> None:
        self.started = []
        self.cancelled = []

    def start_turn(self, turn_id: str) -> None:
        self.started.append(turn_id)

    def feed(self, _chunk):
        return [
            SpeechStarted(turn_id="mic-1", pre_speech_samples=1024),
            PartialTranscript(turn_id="mic-1", text="hello", text_length=5),
            SpeechEnded(turn_id="mic-1", reason="silence"),
            FinalTranscript(
                turn_id="mic-1",
                text="hello world",
                text_length=11,
                provider="fixture",
            ),
        ]

    def commit(self, _turn_id=None):
        return []

    def cancel(self, turn_id=None) -> None:
        self.cancelled.append(turn_id)


class RecordingSupervisor:
    def __init__(self) -> None:
        self.publications = []
        self.submissions = []
        self.completed = threading.Event()

    def publish(self, principal, event):
        self.publications.append((principal, event))

    def submit(self, principal, command):
        self.submissions.append((principal, command))
        self.completed.set()
        future = concurrent.futures.Future()
        future.set_result(None)
        return future


def test_runtime_voice_ingress_publishes_visible_stt_events_before_submitting_final():
    principal = PrincipalContext("user:alice")
    supervisor = RecordingSupervisor()
    transcriber = EventfulTranscriber()
    ingress = RuntimeVoiceIngress(
        supervisor,
        principal=principal,
        transcriber_factory=lambda **_kwargs: transcriber,
        queue_size=4,
    )

    try:
        ingress.start(
            principal=principal,
            conversation_id="conversation-a",
            stream_id="mic-1",
            trace_id="trace-1",
        )
        ingress.chunk(
            PCM_FRAME,
            principal=principal,
            conversation_id="conversation-a",
            stream_id="mic-1",
            trace_id="trace-1",
            seq=0,
        )

        assert supervisor.completed.wait(1.0)
        assert len(supervisor.publications) == 4
        assert [principal for principal, _event in supervisor.publications] == [
            principal,
            principal,
            principal,
            principal,
        ]

        published = [event for _principal, event in supervisor.publications]
        assert isinstance(published[0], events.VoiceSpeechStarted)
        assert published[0].conversation_id == "conversation-a"
        assert published[0].turn_id is None
        assert published[0].stream_id == "mic-1"
        assert published[0].trace_id == "trace-1"
        assert published[0].pre_speech_samples == 1024

        assert isinstance(published[1], events.VoiceTranscriptPartial)
        assert published[1].text == "hello"
        assert published[1].stream_id == "mic-1"
        assert published[1].trace_id == "trace-1"

        assert isinstance(published[2], events.VoiceSpeechEnded)
        assert published[2].reason == "silence"
        assert published[2].stream_id == "mic-1"

        assert isinstance(published[3], events.VoiceTranscriptFinal)
        assert published[3].text == "hello world"
        assert published[3].provider == "fixture"
        assert published[3].stream_id == "mic-1"
        assert published[3].trace_id == "trace-1"

        assert len(supervisor.submissions) == 1
        submitted_principal, command = supervisor.submissions[0]
        assert submitted_principal == principal
        assert isinstance(command, SubmitTurn)
        assert command.text == "hello world"
        assert command.conversation_id == "conversation-a"
        assert command.request_id == "trace-1"
    finally:
        ingress.close(timeout=1.0)


def test_partial_transcript_is_observational_and_never_submits_a_turn():
    class PartialOnlyTranscriber(EventfulTranscriber):
        def feed(self, _chunk):
            return [
                SpeechStarted(turn_id="mic-1", pre_speech_samples=512),
                PartialTranscript(turn_id="mic-1", text="still speaking", text_length=14),
            ]

    principal = PrincipalContext("user:alice")
    supervisor = RecordingSupervisor()
    ingress = RuntimeVoiceIngress(
        supervisor,
        principal=principal,
        transcriber_factory=lambda **_kwargs: PartialOnlyTranscriber(),
        queue_size=4,
    )

    try:
        ingress.start(
            principal=principal,
            conversation_id="conversation-a",
            stream_id="mic-1",
            trace_id="trace-1",
        )
        ingress.chunk(
            PCM_FRAME,
            principal=principal,
            conversation_id="conversation-a",
            stream_id="mic-1",
            trace_id="trace-1",
            seq=0,
        )

        for _ in range(100):
            if len(supervisor.publications) == 2:
                break
            threading.Event().wait(0.005)

        assert len(supervisor.publications) == 2
        assert supervisor.submissions == []
        assert isinstance(supervisor.publications[1][1], events.VoiceTranscriptPartial)
    finally:
        ingress.close(timeout=1.0)
