"""
ElevenLabs TTS - function-based module using official SDK.

Uses the official elevenlabs Python SDK for streaming audio synthesis.
Streaming chunks are piped to the player as they arrive so playback
starts before the full clip has been downloaded.
"""

from __future__ import annotations

from typing import AsyncIterator, Optional
from elevenlabs.client import ElevenLabs
from elevenlabs.types import VoiceSettings
import asyncio

_client: Optional[ElevenLabs] = None


def _ensure_client(api_key: str) -> ElevenLabs:
    """Get or create the ElevenLabs client"""
    global _client
    if _client is None:
        _client = ElevenLabs(api_key=api_key)
    return _client


async def close() -> None:
    global _client
    if _client is not None:
        close_method = getattr(_client, "close", None)
        if callable(close_method):
            result = close_method()
            if asyncio.iscoroutine(result):
                await result
    _client = None


def _get_voice_settings(tts_cfg: dict) -> Optional[VoiceSettings]:
    """Build VoiceSettings from config"""
    # Only create VoiceSettings if at least one setting is specified
    if any(key in tts_cfg for key in [
        "elevenlabs_stability",
        "elevenlabs_similarity_boost",
        "elevenlabs_style",
        "elevenlabs_use_speaker_boost"
    ]):
        return VoiceSettings(
            stability=float(tts_cfg.get("elevenlabs_stability", 0.5)),
            similarity_boost=float(tts_cfg.get("elevenlabs_similarity_boost", 0.75)),
            style=float(tts_cfg.get("elevenlabs_style", 0.0)),
            use_speaker_boost=bool(tts_cfg.get("elevenlabs_use_speaker_boost", True))
        )
    return None


async def synthesize(text: str, tts_cfg: dict) -> bytes:
    """
    Non-streaming synthesis - downloads full audio file before returning.
    Use this for shorter texts or when you need the complete audio at once.
    """
    assert isinstance(text, str) and text, "text must be a non-empty string"
    assert isinstance(tts_cfg, dict), "tts_cfg must be a dict"

    api_key = tts_cfg.get("elevenlabs_api_key")
    voice_id = tts_cfg.get("elevenlabs_voice_id")
    model_id = tts_cfg.get("elevenlabs_model_id", "eleven_turbo_v2_5")
    output_format = tts_cfg.get("elevenlabs_output_format", "mp3_44100_128")

    assert api_key, "tts.elevenlabs_api_key missing in config"
    assert voice_id, "tts.elevenlabs_voice_id missing in config"

    client = _ensure_client(api_key)
    voice_settings = _get_voice_settings(tts_cfg)

    # Run in executor since SDK is sync
    loop = asyncio.get_event_loop()

    def _convert():
        audio_generator = client.text_to_speech.convert(
            voice_id=voice_id,
            text=text,
            model_id=model_id,
            output_format=output_format,
            voice_settings=voice_settings
        )
        # Convert generator to bytes
        return b''.join(audio_generator)

    return await loop.run_in_executor(None, _convert)


async def synthesize_stream(text: str, tts_cfg: dict) -> AsyncIterator[bytes]:
    """
    Streaming synthesis - yields mp3 chunks as they arrive from the API.

    The SDK's ``text_to_speech.stream`` returns a sync ``Iterator[bytes]``;
    we bridge it to an async iterator via a thread + queue so chunks can
    be piped to the player without waiting for the full clip to download.
    """
    assert isinstance(text, str) and text, "text must be a non-empty string"
    assert isinstance(tts_cfg, dict), "tts_cfg must be a dict"

    api_key = tts_cfg.get("elevenlabs_api_key")
    voice_id = tts_cfg.get("elevenlabs_voice_id")
    model_id = tts_cfg.get("elevenlabs_model_id", "eleven_turbo_v2_5")
    output_format = tts_cfg.get("elevenlabs_output_format", "mp3_44100_128")

    assert api_key, "tts.elevenlabs_api_key missing in config"
    assert voice_id, "tts.elevenlabs_voice_id missing in config"

    client = _ensure_client(api_key)
    voice_settings = _get_voice_settings(tts_cfg)

    loop = asyncio.get_running_loop()
    queue: asyncio.Queue = asyncio.Queue(maxsize=64)
    _SENTINEL = object()
    _ERROR_FLAG = object()

    def _produce():
        try:
            for chunk in client.text_to_speech.stream(
                voice_id=voice_id,
                text=text,
                model_id=model_id,
                output_format=output_format,
                voice_settings=voice_settings,
            ):
                if chunk:
                    loop.call_soon_threadsafe(_put, chunk)
        except Exception as error:
            loop.call_soon_threadsafe(_put_error, error)
            return
        loop.call_soon_threadsafe(_put, _SENTINEL)

    def _put(item):
        try:
            queue.put_nowait(item)
        except asyncio.QueueFull:
            pass

    def _put_error(error):
        try:
            queue.put_nowait((_ERROR_FLAG, error))
        except asyncio.QueueFull:
            pass

    loop.run_in_executor(None, _produce)

    while True:
        item = await queue.get()
        if item is _SENTINEL:
            return
        if isinstance(item, tuple) and len(item) == 2 and item[0] is _ERROR_FLAG:
            raise item[1]
        yield item
