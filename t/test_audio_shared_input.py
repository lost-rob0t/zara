from unittest.mock import MagicMock, patch

import zara.audio as audio


def _device(name, max_input_channels=1, default_samplerate=48000.0):
    return {
        "name": name,
        "max_input_channels": max_input_channels,
        "default_samplerate": default_samplerate,
    }


def test_linux_prefers_pipewire_input_over_hardware_default():
    default = MagicMock()
    default.device = (0, 9)
    devices = [
        _device("USB Audio Device"),
        _device("pulse"),
        _device("pipewire"),
    ]

    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.sd, "default", default),
        patch.object(audio.sd, "query_devices", return_value=devices),
        patch.object(audio.sd, "check_input_settings") as check_input_settings,
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0)

    assert sample_rate == 16000.0
    assert default.device == (2, 9)
    assert "pipewire" in note.lower()
    check_input_settings.assert_called_once_with(
        device=2,
        samplerate=16000.0,
        channels=1,
    )


def test_linux_uses_pulse_when_pipewire_device_is_absent():
    default = MagicMock()
    default.device = (0, 4)
    devices = [
        _device("Built-in Audio Analog Stereo"),
        _device("pulse"),
    ]

    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.sd, "default", default),
        patch.object(audio.sd, "query_devices", return_value=devices),
        patch.object(audio.sd, "check_input_settings"),
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0)

    assert sample_rate == 16000.0
    assert default.device == (1, 4)
    assert "pulse" in note.lower()


def test_explicit_device_is_never_replaced():
    default = MagicMock()
    default.device = (0, 4)

    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.sd, "default", default),
        patch.object(audio.sd, "query_devices") as query_devices,
        patch.object(audio.sd, "check_input_settings") as check_input_settings,
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0, device=7)

    assert sample_rate == 16000.0
    assert note is None
    assert default.device == (0, 4)
    query_devices.assert_not_called()
    check_input_settings.assert_called_once_with(
        device=7,
        samplerate=16000.0,
        channels=1,
    )


def test_shared_input_preference_can_be_disabled():
    default = MagicMock()
    default.device = (5, 4)

    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.dict(audio.os.environ, {"ZARA_PREFER_SHARED_INPUT": "0"}),
        patch.object(audio.sd, "default", default),
        patch.object(audio.sd, "query_devices") as query_devices,
        patch.object(audio.sd, "check_input_settings") as check_input_settings,
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0)

    assert sample_rate == 16000.0
    assert note is None
    assert default.device == (5, 4)
    query_devices.assert_not_called()
    check_input_settings.assert_called_once_with(
        device=None,
        samplerate=16000.0,
        channels=1,
    )


def test_shared_input_keeps_device_when_native_rate_fallback_is_needed():
    default = MagicMock()
    default.device = (0, 8)
    devices = [
        _device("USB Audio Device"),
        _device("pipewire", default_samplerate=44100.0),
    ]

    def query_devices(device=None, kind=None):
        if device is None:
            return devices
        assert device == 1
        assert kind == "input"
        return devices[1]

    def check_input_settings(*, device, samplerate, channels):
        assert device == 1
        assert channels == 1
        if samplerate == 16000.0:
            raise ValueError("unsupported rate")

    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.sd, "default", default),
        patch.object(audio.sd, "query_devices", side_effect=query_devices),
        patch.object(audio.sd, "check_input_settings", side_effect=check_input_settings),
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0)

    assert sample_rate == 44100.0
    assert default.device == (1, 8)
    assert "shared device 'pipewire'" in note.lower()
    assert "44100.0hz" in note.lower()
