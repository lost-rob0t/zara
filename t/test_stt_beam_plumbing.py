"""Daemon-side STT decode parameter plumbing (#882)."""

from unittest.mock import MagicMock, patch

import numpy as np

from zara.streaming_stt import make_faster_whisper_transcriber


class FakeWhisperModel:
    def __init__(self):
        self.calls: list[dict] = []

    def transcribe(self, audio, **kwargs):
        self.calls.append(kwargs)
        return iter([]), None


def test_transcriber_forwards_configured_beam_size():
    model = FakeWhisperModel()

    transcribe = make_faster_whisper_transcriber(model, beam_size=3)
    assert transcribe(np.zeros(512, dtype=np.float32)) == ""

    assert model.calls[0]["beam_size"] == 3


def test_transcriber_beam_size_defaults_to_one():
    model = FakeWhisperModel()

    transcribe = make_faster_whisper_transcriber(model)
    assert transcribe(np.zeros(512, dtype=np.float32)) == ""

    assert model.calls[0]["beam_size"] == 1


def _config_with(settings):
    config = MagicMock()
    config.get_section.return_value = settings
    return config


def test_voice_ingress_factory_reads_stt_beam_size():
    from zara.voice_runtime import RuntimeVoiceIngress

    settings = {
        "provider": "faster-whisper",
        "model": "base.en",
        "device": "cpu",
        "threads": 4,
        "beam_size": 3,
    }
    with (
        patch("zara.config.get_config", return_value=_config_with(settings)),
        patch("zara.transcription.Transcriber") as loader_cls,
        patch(
            "zara.voice_runtime.make_faster_whisper_transcriber"
        ) as make_transcriber,
    ):
        loader_cls.return_value = MagicMock(model=FakeWhisperModel())
        ingress = RuntimeVoiceIngress(supervisor=MagicMock())

        ingress._default_transcriber_factory()

    assert make_transcriber.call_args.kwargs["beam_size"] == 3


def test_voice_ingress_factory_beam_size_default_is_unchanged():
    from zara.voice_runtime import RuntimeVoiceIngress

    settings = {
        "provider": "faster-whisper",
        "model": "small",
        "device": "cpu",
        "threads": 4,
    }
    with (
        patch("zara.config.get_config", return_value=_config_with(settings)),
        patch("zara.transcription.Transcriber") as loader_cls,
        patch(
            "zara.voice_runtime.make_faster_whisper_transcriber"
        ) as make_transcriber,
    ):
        loader_cls.return_value = MagicMock(model=FakeWhisperModel())
        ingress = RuntimeVoiceIngress(supervisor=MagicMock())

        ingress._default_transcriber_factory()

    assert make_transcriber.call_args.kwargs["beam_size"] == 1
