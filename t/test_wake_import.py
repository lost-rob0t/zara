import asyncio
import pathlib
import sys
from unittest.mock import AsyncMock, MagicMock, patch


sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))


def test_wake_module_imports_without_opening_audio_hardware():
    with patch("sounddevice.InputStream") as input_stream:
        import zara.wake

    input_stream.assert_not_called()
    assert zara.wake.WakeWordListener is not None


def test_real_construction_owns_no_private_brain():
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


def test_active_mode_streams_utterance_to_daemon():
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
