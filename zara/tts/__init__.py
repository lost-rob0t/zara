"""
TTS subpackage for specialized providers
"""

from .voice_registry import LocalVoiceRegistryQwen3TTSClient as Qwen3TTSClient
from .engine import PlaybackResult, StreamChunk, SynthesisResult, TTSEngine, supports_streaming

__all__ = [
    "Qwen3TTSClient",
    "PlaybackResult",
    "StreamChunk",
    "SynthesisResult",
    "TTSEngine",
    "supports_streaming",
]
