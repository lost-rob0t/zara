"""PulseOutputStream: bounded Pulse/PipeWire-rated playback sink (#880)."""

from unittest.mock import MagicMock, patch

import pytest

import zara.audio as audio


class StderrReader:
    def __init__(self, lines):
        self._lines = list(lines)

    def readline(self):
        if self._lines:
            return self._lines.pop(0)
        return b""


def _pulse_available(which=None):
    return which or (lambda name: f"/usr/bin/{name}" if name in {"pacat", "pactl", "parec"} else None)


def test_command_uses_pacat_with_s16le_pcm():
    with (
        patch.object(audio.shutil, "which", side_effect=_pulse_available()),
        patch.dict(audio.os.environ, {"ZARA_PULSE_SINK": "test_sink"}),
    ):
        stream = audio.PulseOutputStream(samplerate=24000, channels=1)
        command = stream._command()

    assert command[0] == "/usr/bin/pacat"
    assert "--format=s16le" in command
    assert "--rate=24000" in command
    assert "--channels=1" in command
    assert "--device=test_sink" in command
    assert any(arg.startswith("--client-name=") for arg in command)
    assert any(arg.startswith("--stream-name=") for arg in command)
    assert stream.source == "test_sink"


def test_environment_carries_application_identity():
    stream = audio.PulseOutputStream(samplerate=16000, channels=1)

    env = stream._environment()

    assert env["PULSE_PROP_application.name"] == "Zarathushtra"
    assert env["PULSE_PROP_media.role"] == "phone"


def test_start_raises_with_stderr_detail_when_pacat_exits_early():
    process = MagicMock()
    process.poll.return_value = 1
    process.wait.return_value = 1
    process.stderr = StderrReader([b"Connection refused\n"])

    with (
        patch.object(audio, "_pulse_backend_status", return_value=(True, "ok")),
        patch.object(audio.shutil, "which", side_effect=_pulse_available()),
        patch.object(audio.subprocess, "Popen", return_value=process) as popen,
    ):
        stream = audio.PulseOutputStream(samplerate=16000, channels=1)
        with pytest.raises(RuntimeError, match="Connection refused"):
            stream.start()

    popen.assert_called_once()
    assert stream.process is None


def test_write_goes_to_pacat_stdin_and_records_write_errors():
    stream = audio.PulseOutputStream(samplerate=16000, channels=1)
    process = MagicMock()
    process.stdin = MagicMock()
    stream.process = process

    stream.write(b"\x01\x00\x02\x00")

    process.stdin.write.assert_called_once_with(b"\x01\x00\x02\x00")

    process.stdin.write.side_effect = BrokenPipeError("closed")
    with pytest.raises(BrokenPipeError):
        stream.write(b"\x00\x00")

    assert stream.last_error is not None


def test_write_without_open_stream_raises():
    stream = audio.PulseOutputStream(samplerate=16000, channels=1)

    with pytest.raises(RuntimeError):
        stream.write(b"\x00\x00")


def test_stop_terminates_a_live_process():
    stream = audio.PulseOutputStream(samplerate=16000, channels=1)
    process = MagicMock()
    process.poll.return_value = None
    process.wait.return_value = 0
    stream.process = process

    stream.stop()

    process.terminate.assert_called_once()
    stream.process = None


def test_create_returns_none_without_pacat():
    def which(name):
        return "/usr/bin/pactl" if name == "pactl" else None

    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.shutil, "which", side_effect=which),
    ):
        assert audio.create_pulse_output_stream(24000) is None


def test_create_returns_none_when_disabled():
    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.shutil, "which", side_effect=_pulse_available()),
        patch.dict(audio.os.environ, {"ZARA_PREFER_SHARED_OUTPUT": "0"}),
    ):
        assert audio.create_pulse_output_stream(24000) is None


def test_create_returns_pulse_stream_on_linux_with_pacat():
    with (
        patch.object(audio.sys, "platform", "linux"),
        patch.object(audio.shutil, "which", side_effect=_pulse_available()),
        patch.object(audio, "_pulse_backend_status", return_value=(True, "ok")),
    ):
        stream = audio.create_pulse_output_stream(24000)

    assert isinstance(stream, audio.PulseOutputStream)
    assert stream.samplerate == 24000
