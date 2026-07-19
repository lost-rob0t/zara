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


def test_active_mode_captures_one_command():
    import zara.wake

    with (
        patch("zara.wake.resolve_input_sample_rate", return_value=(16000.0, None)),
        patch("zara.wake.build_memory_manager") as build_memory_manager,
        patch("zara.wake.PrologEngine") as prolog_engine,
        patch("zara.wake.faster_whisper.WhisperModel") as whisper_model,
        patch("zara.wake.TTSEngine") as tts_engine,
        patch("zara.wake.send_notification_async", new_callable=AsyncMock) as notification,
        patch.object(zara.wake.WakeWordListener, "log"),
    ):
        memory = MagicMock()
        memory.start_session.return_value = "test-session"
        build_memory_manager.return_value = memory
        prolog = MagicMock()
        prolog.dictation_active.return_value = False
        prolog.is_conversation_stop.return_value = False
        prolog_engine.return_value = prolog
        listener = zara.wake.WakeWordListener(enable_tts=False)

        listener.collect_audio_until_silence = AsyncMock(return_value=MagicMock())
        listener.transcribe_async = AsyncMock(return_value="open firefox")
        listener.query_with_fallback_async = AsyncMock(return_value=(False, "Opened Firefox"))
        listener.send_response_async = AsyncMock()
        listener.stop_event = asyncio.Event()

        asyncio.run(listener.active_mode_async())

    listener.collect_audio_until_silence.assert_awaited_once_with(5.0)
    listener.transcribe_async.assert_awaited_once()
    listener.query_with_fallback_async.assert_awaited_once_with("open firefox")
    listener.send_response_async.assert_awaited_once_with("Zara", "Opened Firefox")
    whisper_model.assert_called_once()
    tts_engine.assert_not_called()
    notification.assert_not_awaited()
    memory.summarise_session.assert_not_called()
