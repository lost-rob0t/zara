"""
TTS subpackage for specialized providers
"""

from .qwen import Qwen3TTSClient
from .engine import PlaybackResult, SynthesisResult, TTSEngine

__all__ = [
    "Qwen3TTSClient",
    "PlaybackResult",
    "SynthesisResult",
    "TTSEngine",
]
