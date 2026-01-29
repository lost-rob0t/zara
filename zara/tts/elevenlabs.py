"""
ElevenLabs TTS - function-based module using official SDK.

Uses the official elevenlabs Python SDK for streaming audio synthesis.
"""

from __future__ import annotations

from typing import Optional, AsyncIterator
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
    """Close the client (placeholder for compatibility)"""
    global _client
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


async def synthesize_streaming(text: str, tts_cfg: dict) -> AsyncIterator[bytes]:
    """
    Streaming synthesis - yields audio chunks as they're received.
    This reduces latency as playback can start before the entire audio is generated.

    Usage:
        async for chunk in synthesize_streaming("Hello world", config):
            # Play or process chunk immediately
            pass
    """
    assert isinstance(text, str) and text, "text must be a non-empty string"
    assert isinstance(tts_cfg, dict), "tts_cfg must be a dict"

    api_key = tts_cfg.get("elevenlabs_api_key")
    voice_id = tts_cfg.get("elevenlabs_voice_id")
    model_id = tts_cfg.get("elevenlabs_model_id", "eleven_turbo_v2_5")
    output_format = tts_cfg.get("elevenlabs_output_format", "mp3_44100_128")
    optimize_latency = tts_cfg.get("elevenlabs_optimize_streaming_latency", 3)

    assert api_key, "tts.elevenlabs_api_key missing in config"
    assert voice_id, "tts.elevenlabs_voice_id missing in config"

    client = _ensure_client(api_key)
    voice_settings = _get_voice_settings(tts_cfg)

    # Run the streaming in executor since SDK is sync
    loop = asyncio.get_event_loop()

    def _stream():
        return client.text_to_speech.stream(
            voice_id=voice_id,
            text=text,
            model_id=model_id,
            output_format=output_format,
            voice_settings=voice_settings,
            optimize_streaming_latency=optimize_latency
        )

    # Get the iterator in executor
    audio_stream = await loop.run_in_executor(None, _stream)

    # Yield chunks asynchronously
    def _get_next(iterator):
        try:
            return next(iterator)
        except StopIteration:
            return None

    while True:
        chunk = await loop.run_in_executor(None, _get_next, audio_stream)
        if chunk is None:
            break
        yield chunk
