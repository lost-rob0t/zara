from unittest.mock import MagicMock, patch

import zara.audio as audio


def _which(name):
    if name in {"parec", "pactl"}:
        return f"/usr/bin/{name}"
    return None


def _pactl(*args):
    if args == ("info",):
        return "Server Name: PulseAudio (on PipeWire)"
    if args == ("get-default-source",):
        return "alsa_input.usb-test.mono-fallback"
    return None


def test_linux_uses_pulse_server_at_requested_rate():
    audio._restore_portaudio_input_stream()
    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.shutil, "which", side_effect=_which),
        patch.object(audio, "_run_pactl", side_effect=_pactl),
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0)

    try:
        assert sample_rate == 16000.0
        assert audio.sd.InputStream is audio.PulseInputStream
        assert "pulse shared capture via parec" in note.lower()
        assert "alsa_input.usb-test.mono-fallback" in note
        assert "Zarathushtra" in note
    finally:
        audio._restore_portaudio_input_stream()


def test_missing_parec_falls_back_to_portaudio_and_explains_why():
    default = MagicMock()
    default.device = (3, 9)
    device_info = {"default_samplerate": 44100.0}

    def which(name):
        if name == "pactl":
            return "/usr/bin/pactl"
        return None

    def check_input_settings(*, device, samplerate, channels):
        assert channels == 1
        if samplerate == 16000.0:
            raise ValueError("unsupported rate")

    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.shutil, "which", side_effect=which),
        patch.object(audio.sd, "default", default),
        patch.object(audio.sd, "query_devices", return_value=device_info),
        patch.object(audio.sd, "check_input_settings", side_effect=check_input_settings),
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0)

    assert sample_rate == 44100.0
    assert "parec is not available" in note
    assert "44100.0hz" in note.lower()


def test_explicit_device_bypasses_shared_backend():
    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio, "_pulse_backend_status") as pulse_status,
        patch.object(audio.sd, "check_input_settings") as check_input_settings,
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0, device=7)

    assert sample_rate == 16000.0
    assert note is None
    pulse_status.assert_not_called()
    check_input_settings.assert_called_once_with(
        device=7,
        samplerate=16000.0,
        channels=1,
    )


def test_shared_input_preference_can_be_disabled():
    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.dict(audio.os.environ, {"ZARA_PREFER_SHARED_INPUT": "0"}),
        patch.object(audio.sd, "check_input_settings") as check_input_settings,
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0)

    assert sample_rate == 16000.0
    assert note is None
    check_input_settings.assert_called_once_with(
        device=None,
        samplerate=16000.0,
        channels=1,
    )


def test_pulse_stream_is_named_for_pavucontrol():
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda *_args: None,
    )

    env = stream._environment()

    assert env["PULSE_PROP_application.name"] == "Zarathushtra"
    assert env["PULSE_PROP_application.icon_name"] == "audio-input-microphone"
    assert env["PULSE_PROP_media.role"] == "phone"


def test_pulse_stream_targets_selected_source():
    with (
        patch.object(audio.shutil, "which", return_value="/usr/bin/parec"),
        patch.dict(audio.os.environ, {"ZARA_PULSE_SOURCE": "test_source"}),
    ):
        stream = audio.PulseInputStream(
            samplerate=16000,
            channels=1,
            callback=lambda *_args: None,
        )
        command = stream._command()

    assert "--format=s16le" in command
    assert "--rate=16000" in command
    assert "--channels=1" in command
    assert "--device=test_source" in command
