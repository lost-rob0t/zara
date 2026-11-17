"""Quick voice profile switch: zara voice-mode game|normal|status."""

from __future__ import annotations

import argparse
from typing import Any, Optional, Sequence

from .voice_profile import VOICE_MODE_PROVIDERS, current_voice_mode, set_voice_mode


def main(argv: Optional[Sequence[str]] = None, *, config: Any = None) -> int:
    parser = argparse.ArgumentParser(
        prog="zara voice-mode",
        description="Switch or inspect the voice profile (normal <-> game)",
    )
    parser.add_argument(
        "mode",
        nargs="?",
        choices=sorted(VOICE_MODE_PROVIDERS) + ["status"],
        default=None,
        help="voice mode to switch to; default prints the current mode",
    )
    args = parser.parse_args(argv)
    if config is None:
        from .config import get_config

        config = get_config()

    if args.mode is None or args.mode == "status":
        mode = current_voice_mode(config)
        provider = (config.get_section("tts") or {}).get("provider", "llamacpp")
        print(f"Voice mode: {mode} (provider: {provider})")
        return 0

    print(set_voice_mode(args.mode, config=config))
    return 0
