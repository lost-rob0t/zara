from types import SimpleNamespace

from zara.speech_activity import SpeechActivityRegistry
from zara.voice_runtime import RuntimeVoiceIngress


def test_speech_activity_registry_is_nested_and_reversible():
    registry = SpeechActivityRegistry()
    first = registry.begin(source="one")
    second = registry.begin(source="two")

    assert registry.active is True
    assert {item.token for item in registry.snapshot()} == {first.token, second.token}
    assert registry.end(first.token) is True
    assert registry.active is True
    assert registry.end(second.token) is True
    assert registry.active is False


def test_voice_ingress_uses_playback_barge_in_thresholds_and_restores_idle():
    ingress = RuntimeVoiceIngress.__new__(RuntimeVoiceIngress)
    ingress._barge_in_config = SimpleNamespace(
        playback_vad_threshold=0.7,
        playback_min_speech_frames=6,
    )
    config = SimpleNamespace(vad_threshold=0.5, min_speech_frames=4)
    transcriber = SimpleNamespace(_vad=SimpleNamespace(config=config))

    ingress._set_playback_vad(transcriber, True)

    assert config.vad_threshold == 0.7
    assert config.min_speech_frames == 6
    assert transcriber._zara_playback_active is True

    ingress._set_playback_vad(transcriber, False)

    assert config.vad_threshold == 0.5
    assert config.min_speech_frames == 4
    assert transcriber._zara_playback_active is False


def test_voice_ingress_never_lowers_stricter_existing_vad_settings():
    ingress = RuntimeVoiceIngress.__new__(RuntimeVoiceIngress)
    ingress._barge_in_config = SimpleNamespace(
        playback_vad_threshold=0.7,
        playback_min_speech_frames=6,
    )
    config = SimpleNamespace(vad_threshold=0.8, min_speech_frames=8)
    transcriber = SimpleNamespace(_vad=SimpleNamespace(config=config))

    ingress._set_playback_vad(transcriber, True)

    assert config.vad_threshold == 0.8
    assert config.min_speech_frames == 8
