import threading

from unittest.mock import MagicMock, patch

import numpy as np

import zara.audio as audio


def setup_function():
    audio._restore_portaudio_input_stream()


def teardown_function():
    audio._restore_portaudio_input_stream()


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
    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.shutil, "which", side_effect=_which),
        patch.object(audio, "_run_pactl", side_effect=_pactl),
    ):
        sample_rate, note = audio.resolve_input_sample_rate(16000.0)

    assert sample_rate == 16000.0
    assert audio.sd.InputStream is audio.PulseInputStream
    assert "pulse shared capture via parec" in note.lower()
    assert "alsa_input.usb-test.mono-fallback" in note
    assert "Zarathushtra" in note


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
    assert stream.source == "test_source"


def test_reader_preserves_partial_pcm_frames():
    samples = np.array([1, -2, 300, -400], dtype="<i2")
    raw = samples.tobytes()
    received = []
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda data, *_args: received.extend(data[:, 0].tolist()),
        diagnostic_callback=lambda _message: None,
    )

    class Reader:
        def __init__(self):
            self.chunks = [raw[:1], raw[1:5], raw[5:]]

        def read(self, _size):
            if self.chunks:
                return self.chunks.pop(0)
            stream.stop_event.set()
            return b""

    process = MagicMock()
    process.stdout = Reader()
    stream.process = process

    stream._reader_loop()

    expected = (samples.astype(np.float32) / 32768.0).tolist()
    assert np.allclose(received, expected)
    assert stream.bytes_received == len(raw)
    assert stream.frames_received == len(samples)


def test_signal_detection_reports_observed_peak_and_rms():
    messages = []
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda *_args: None,
        diagnostic_callback=messages.append,
    )
    stream._started_at = 10.0

    with patch.object(audio.time, "monotonic", return_value=10.1):
        stream._report_signal(np.array([[0.25], [-0.5]], dtype=np.float32))

    assert any("Audio signal detected" in message for message in messages)
    assert stream.peak == 0.5
    assert stream.rms > 0.0


def test_silent_stream_warning_is_throttled_to_once():
    messages = []
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda *_args: None,
        diagnostic_callback=messages.append,
    )
    stream.source = "silent_source"
    stream._started_at = 10.0
    silence = np.zeros((320, 1), dtype=np.float32)

    with patch.object(audio.time, "monotonic", return_value=14.0):
        stream._report_signal(silence)
        stream._report_signal(silence)

    silent_messages = [
        message for message in messages if "Audio input appears silent" in message
    ]
    assert len(silent_messages) == 1
    assert "silent_source" in silent_messages[0]


def test_runtime_parec_failure_surfaces_stderr():
    messages = []
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda *_args: None,
        diagnostic_callback=messages.append,
    )
    process = MagicMock()
    process.stdout.read.return_value = b""
    process.poll.return_value = 7
    stream.process = process
    stream._stderr_lines.append("Connection terminated")

    stream._reader_loop()

    assert stream.last_error == "parec exited with 7: Connection terminated"
    assert messages == [
        "Audio capture failed: parec exited with 7: Connection terminated"
    ]


def test_stop_terminates_parec_process():
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda *_args: None,
        diagnostic_callback=lambda _message: None,
    )
    process = MagicMock()
    process.poll.return_value = None
    stream.process = process

    stream.stop()

    process.terminate.assert_called_once_with()
    process.wait.assert_called_once_with(timeout=1.0)
    assert stream.stop_event.is_set()


class BlockingReader:
    def __init__(self, release: threading.Event):
        self.release = release

    def read(self, _size):
        self.release.wait(5.0)
        return b""


def test_watchdog_flags_stall_and_terminates_process():
    release = threading.Event()
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda *_args: None,
        stall_timeout=0.2,
        diagnostic_callback=lambda _message: None,
    )
    process = MagicMock()
    process.poll.return_value = None
    process.stdout = BlockingReader(release)
    stream.process = process
    stream.last_data_monotonic = audio.time.monotonic() - 10.0

    reader = threading.Thread(target=stream._reader_loop, daemon=True)
    reader.start()
    watchdog = threading.Thread(target=stream._watchdog_loop, daemon=True)
    watchdog.start()
    watchdog.join(timeout=2.0)
    release.set()
    reader.join(timeout=2.0)

    assert not watchdog.is_alive()
    assert stream.last_error is not None
    assert "stalled" in stream.last_error
    process.terminate.assert_called_once()


def test_watchdog_respects_stop_event_without_false_stall():
    messages = []
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda *_args: None,
        stall_timeout=0.1,
        diagnostic_callback=messages.append,
    )
    process = MagicMock()
    process.poll.return_value = None
    stream.process = process
    stream.last_data_monotonic = audio.time.monotonic() - 60.0
    stream.stop_event.set()

    watchdog = threading.Thread(target=stream._watchdog_loop, daemon=True)
    watchdog.start()
    watchdog.join(timeout=1.0)

    assert not watchdog.is_alive()
    assert stream.last_error is None
    process.terminate.assert_not_called()


def test_reader_failure_does_not_overwrite_stall_error():
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda *_args: None,
        diagnostic_callback=lambda _message: None,
    )
    stream.last_error = "capture stalled: no audio bytes for 5.0s"
    process = MagicMock()
    process.poll.return_value = 0

    stream._report_runtime_failure(process)

    assert stream.last_error == "capture stalled: no audio bytes for 5.0s"


def test_receiving_data_refreshes_stall_clock():
    stream = audio.PulseInputStream(
        samplerate=16000,
        channels=1,
        callback=lambda *_args: None,
    )
    stream.process = MagicMock()

    class Burst:
        def __init__(self):
            self.chunks = [b"\x01\x00\x02\x00"]

        def read(self, _size):
            if self.chunks:
                return self.chunks.pop(0)
            stream.stop_event.set()
            return b""

    process = MagicMock()
    process.stdout = Burst()
    stream.process = process
    before = stream.last_data_monotonic
    audio.time.sleep(0.01)

    stream._reader_loop()

    assert stream.last_data_monotonic > before - 1.0
    assert stream.bytes_received == 4
