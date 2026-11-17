"""Voice profile switching between normal and game audio output.

Game mode trades the local llamacpp voice for the low-latency Edge
provider; normal mode restores it. A switch persists [tts].provider and
notifies registered consumers (daemon TTS bridge, task announcer) so the
change applies to live output without a restart.
"""

from __future__ import annotations

import logging
from typing import Callable, Optional

from .config import ZaraConfig, get_config

logger = logging.getLogger(__name__)

VOICE_MODE_PROVIDERS = {"normal": "llamacpp", "game": "edge"}

_RESET_CALLBACKS: list[Callable[[], None]] = []


def register_voice_reset(callback: Callable[[], None]) -> None:
    if callback not in _RESET_CALLBACKS:
        _RESET_CALLBACKS.append(callback)


def unregister_voice_reset(callback: Callable[[], None]) -> None:
    try:
        _RESET_CALLBACKS.remove(callback)
    except ValueError:
        pass


def current_voice_mode(config: Optional[ZaraConfig] = None) -> str:
    config = config if config is not None else get_config()
    provider = str((config.get_section("tts") or {}).get("provider", "llamacpp"))
    for mode, mode_provider in VOICE_MODE_PROVIDERS.items():
        if provider == mode_provider:
            return mode
    return "normal"


def set_voice_mode(mode: str, *, config: Optional[ZaraConfig] = None) -> str:
    normalized = (mode or "").strip().lower()
    if normalized not in VOICE_MODE_PROVIDERS:
        raise ValueError(f"unknown voice mode: {mode!r}")
    config = config if config is not None else get_config()
    provider = VOICE_MODE_PROVIDERS[normalized]
    config.set_value("tts", "provider", provider)
    _notify_engine_resets()
    label = "Game mode" if normalized == "game" else "Normal mode"
    return f"{label}: voice on {provider}."


def _notify_engine_resets() -> None:
    for callback in list(_RESET_CALLBACKS):
        try:
            callback()
        except Exception as error:
            logger.warning("Voice engine reset callback failed: %s", error)
