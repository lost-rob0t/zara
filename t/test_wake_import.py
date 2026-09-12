import asyncio
import pathlib
import sys
from unittest.mock import AsyncMock, MagicMock, patch

import pytest


sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))


def test_wake_module_imports_without_opening_audio_hardware():
    with patch("sounddevice.InputStream") as input_stream:
        import zara.wake

    input_stream.assert_not_called()
    assert zara.wake.WakeWordListener is not None


def test_real_construction_owns_no_private_brain(wake_config):
    import zara.wake
    import zara.wake_daemon

    with (
        patch("zara.wake.resolve_input_sample_rate", return_value=(16000.0, None)),
        patch("zara.wake.faster_whisper.WhisperModel") as whisper_model,
        patch.object(zara.wake.WakeWordListener, "log"),
        patch.object(zara.wake.WakeWordListener, "_init_ack_player"),
        patch("zara.wake.AcknowledgementPlayer"),
    ):
        listener = zara.wake.WakeWordListener(enable_tts=False)

    whisper_model.assert_called_once()
    assert not hasattr(listener, "prolog")
    assert not hasattr(listener, "agent_manager")
    assert not hasattr(listener, "memory")
    assert not hasattr(listener, "tts_client")
    assert not hasattr(type(listener), "query_with_fallback_async")
    assert not hasattr(type(listener), "synthesize_and_play_async")
    assert isinstance(listener.daemon, zara.wake_daemon.WakeDaemonClient)
    assert isinstance(listener.speaker, zara.wake_daemon.PcmStreamSpeaker)


def test_active_mode_streams_utterance_to_daemon(wake_config):
    import zara.wake

    with (
        patch("zara.wake.resolve_input_sample_rate", return_value=(16000.0, None)),
        patch("zara.wake.faster_whisper.WhisperModel") as whisper_model,
        patch.object(zara.wake.WakeWordListener, "log"),
        patch("zara.wake.AcknowledgementPlayer"),
        patch("zara.wake.WakeDaemonClient") as daemon_client,
    ):
        daemon = MagicMock()
        daemon.stream_utterance = AsyncMock(return_value="stream-1")
        daemon.ensure_connected = MagicMock()
        daemon.connect = MagicMock()
        daemon.start_pump = MagicMock()
        daemon.audio_output_format = {
            "codec": "pcm_s16le",
            "sample_rate": 24000,
            "channels": 1,
        }
        daemon_client.return_value = daemon

        listener = zara.wake.WakeWordListener(enable_tts=False)
        listener.collect_audio_until_silence = AsyncMock(
            return_value=MagicMock()
        )
        listener._monitor_speech_during_llm = AsyncMock(return_value=False)
        listener._wait_for_turn_completion = AsyncMock(return_value=True)
        listener._play_acknowledgement = MagicMock()
        listener.stop_event = asyncio.Event()

        asyncio.run(listener.active_mode_async())

    daemon.stream_utterance.assert_awaited_once()
    whisper_model.assert_called_once()


def test_real_construction_with_ack_player_needs_no_tts_config(wake_config):
    import zara.wake

    class StubAckPlayer:
        def __init__(self, config=None, tts_engine=None):
            self.config = config
            self.has_audio = False
            self.source = "fixture"

        def initialize(self):
            return None

    with (
        patch("zara.wake.resolve_input_sample_rate", return_value=(16000.0, None)),
        patch("zara.wake.faster_whisper.WhisperModel"),
        patch.object(zara.wake.WakeWordListener, "log"),
        patch("zara.wake.AcknowledgementPlayer", StubAckPlayer),
        patch("zara.wake.TTSEngine"),
        patch("zara.wake.WakeDaemonClient"),
        patch("zara.wake.PcmStreamSpeaker"),
    ):
        listener = zara.wake.WakeWordListener(enable_tts=True)

    assert listener.ack_player.config.enabled is True
    assert listener.ack_player.config.provider


@pytest.fixture
def wake_config(monkeypatch, tmp_path):
    import zara.wake
    from zara.config import ZaraConfig
    from t.test_daemon_client_config import clear_daemon_env

    clear_daemon_env(monkeypatch)
    monkeypatch.setenv("XDG_RUNTIME_DIR", str(tmp_path))
    config = ZaraConfig(config_path=tmp_path / "config.toml")
    config.get_section("daemon").clear()
    with patch("zara.wake.get_config", return_value=config):
        yield config


@pytest.fixture
def configured_wake(wake_config):
    import zara.wake

    with (
        patch("zara.wake.resolve_input_sample_rate", return_value=(16000.0, None)),
        patch("zara.wake.faster_whisper.WhisperModel"),
        patch.object(zara.wake.WakeWordListener, "log"),
        patch.object(zara.wake.WakeWordListener, "_init_ack_player"),
        patch("zara.wake.WakeDaemonClient") as daemon,
    ):
        yield wake_config, daemon


@pytest.mark.parametrize("source", ["environment", "config", "default"])
def test_wake_uses_canonical_endpoint(configured_wake, monkeypatch, source):
    import zara.wake
    from zara.daemon_client import resolve_daemon_endpoint

    config, daemon = configured_wake
    if source != "default":
        config.get_section("daemon")["endpoint"] = " ipc:///configured.sock "
    if source == "environment":
        monkeypatch.setenv("ZARA_DAEMON_ENDPOINT", " ipc:///environment.sock ")

    listener = zara.wake.WakeWordListener(enable_tts=False)
    try:
        assert daemon.call_args.kwargs["endpoint"] == resolve_daemon_endpoint(config)
        assert daemon.call_args.kwargs["curve_client"] is None
        assert daemon.call_args.kwargs["voice_output"] is listener.speaker
    finally:
        listener.speaker.close()
        listener.executor.shutdown(wait=True)


@pytest.mark.parametrize("source", ["environment", "config", "mixed"])
def test_wake_uses_canonical_curve_credentials(configured_wake, monkeypatch, source):
    import zmq
    import zara.wake
    from zara.daemon_client import curve_client_config

    config, daemon = configured_wake
    public, secret = zmq.curve_keypair()
    server_public, _ = zmq.curve_keypair()
    for name, value in (
        ("public_key", public),
        ("secret_key", secret),
        ("server_public_key", server_public),
    ):
        value = " " + value.decode("ascii") + " "
        if source == "environment" or (source == "mixed" and name == "secret_key"):
            config.get_section("daemon")["curve_" + name] = "ignored-config-value"
            monkeypatch.setenv("ZARA_DAEMON_CURVE_" + name.upper(), value)
        else:
            config.get_section("daemon")["curve_" + name] = value

    listener = zara.wake.WakeWordListener(enable_tts=False)
    try:
        assert daemon.call_args.kwargs["curve_client"] == curve_client_config(config)
    finally:
        listener.speaker.close()
        listener.executor.shutdown(wait=True)


@pytest.mark.parametrize("source", ["environment", "config"])
@pytest.mark.parametrize("key", ["public_key", "secret_key", "server_public_key"])
def test_wake_rejects_partial_credentials_before_transport(
    configured_wake, monkeypatch, capsys, source, key
):
    import zara.wake

    config, daemon = configured_wake
    if source == "environment":
        monkeypatch.setenv("ZARA_DAEMON_CURVE_" + key.upper(), "do-not-log-this")
    else:
        config.get_section("daemon")["curve_" + key] = "do-not-log-this"

    with patch.object(zara.wake.WakeWordListener, "run_async", new_callable=AsyncMock):
        assert zara.wake.run_wake_listener(enable_tts=False) == 2
    daemon.assert_not_called()
    error = capsys.readouterr().err
    assert "requires public, secret, and server public keys" in error
    assert "do-not-log-this" not in error
