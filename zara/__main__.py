#!/usr/bin/env python3
"""
Zara - Unified CLI Interface
Wraps console (text), voice, and dictate modes
"""

import sys
import argparse
from pathlib import Path
from .config import init_config
from .stt_backends import (
    STT_PROVIDERS,
    backend_compat,
    needs_faster_whisper_files,
    normalize_provider,
    resolve_model_for_provider,
)


STT_DEVICE_ALIASES = {
    "amd": "cuda",
    "hip": "cuda",
    "rocm": "cuda",
}
STT_DEVICE_CHOICES = ["cpu", "cuda", "rocm", "hip", "amd"]
GPU_ERROR_MARKERS = (
    "cuda",
    "cudnn",
    "gpu",
    "hip",
    "rocm",
    "gfx",
)


def normalize_stt_device(device: str) -> str:
    normalized = str(device).strip().lower()
    if normalized in {"cpu", "cuda"}:
        return normalized
    if normalized in STT_DEVICE_ALIASES:
        return STT_DEVICE_ALIASES[normalized]
    choices = ", ".join(STT_DEVICE_CHOICES)
    raise ValueError(f"Unsupported STT device {device!r}; choose one of: {choices}")


def is_gpu_initialization_error(error: Exception) -> bool:
    message = str(error).lower()
    return any(marker in message for marker in GPU_ERROR_MARKERS)


def main():
    config = init_config()
    stt_config = config.get_section("stt") if config is not None else {}
    default_stt_provider = normalize_provider(stt_config.get("provider", "faster-whisper"))
    default_stt_model = stt_config.get("model", "small")
    default_stt_device = stt_config.get("device", "cpu")

    parser = argparse.ArgumentParser(
        prog="zara",
        description="Zarathustra Voice Assistant - Unified Interface",
        epilog="Examples:\n"
               "  zara 'open firefox'           # Execute text command\n"
               "  zara --desktop                # Native desktop / Quick Copilot\n"
               "  zara --console                # Interactive REPL\n"
               "  zara --voice                  # One-shot voice command\n"
               "  zara --dictate                # Continuous dictation mode\n"
               "  zara --wake                   # Wake word listener\n",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    mode_group = parser.add_mutually_exclusive_group()
    mode_group.add_argument(
        "--desktop",
        action="store_true",
        help="Start the native desktop Copilot"
    )
    mode_group.add_argument(
        "--console",
        action="store_true",
        help="Start interactive console (REPL)"
    )
    mode_group.add_argument(
        "--voice",
        action="store_true",
        help="Single voice command mode"
    )
    mode_group.add_argument(
        "--dictate",
        action="store_true",
        help="Continuous dictation mode"
    )
    mode_group.add_argument(
        "--wake",
        action="store_true",
        help="Wake word listener mode"
    )
    mode_group.add_argument(
        "--agent",
        action="store_true",
        help="Direct conversation mode with agent"
    )

    parser.add_argument(
        "--pets",
        action="store_true",
        help="Launch the desktop pet overlay (companion flag; use with --wake)"
    )
    parser.add_argument(
        "--pets-settings",
        action="store_true",
        help="Open the Pets settings dialog"
    )

    parser.add_argument(
        "command",
        nargs="*",
        help="Text command to execute"
    )

    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose logging"
    )

    parser.add_argument(
        "--stt-provider",
        default=default_stt_provider,
        choices=STT_PROVIDERS,
        help=f"Speech-to-text provider (default: {default_stt_provider})"
    )
    parser.add_argument(
        "--model",
        "--mode",
        dest="model",
        default=default_stt_model,
        help=f"STT model name or local model directory (default: {default_stt_model})"
    )
    parser.add_argument(
        "--device",
        default=default_stt_device,
        choices=STT_DEVICE_CHOICES,
        help=(
            f"Device for local transcription (default: {default_stt_device}); "
            "rocm/hip/amd use CUDA-compatible provider APIs"
        )
    )
    parser.add_argument(
        "--threads",
        type=int,
        help="Number of local STT inference threads"
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=2,
        help="Number of parallel transcription workers (default: 2)"
    )
    parser.add_argument(
        "--stop-phrases",
        help='Stop phrases for dictation (comma-separated, e.g. "end voice,stop voice")'
    )

    args = parser.parse_args()
    stt_provider = normalize_provider(args.stt_provider)
    stt_device = normalize_stt_device(args.device)
    stt_model = resolve_model_for_provider(stt_provider, args.model)

    if args.desktop:
        from .desktop.app import main as desktop_main
        sys.exit(desktop_main([sys.argv[0]]))

    elif args.console:
        from .console import main as console_main
        sys.exit(console_main())

    elif args.voice:
        print("Error: Voice mode is not currently implemented.", file=sys.stderr)
        print("Use --dictate for continuous voice input instead.", file=sys.stderr)
        sys.exit(1)

    elif args.dictate:
        from .config import get_config
        if args.stop_phrases:
            stop_phrases = args.stop_phrases.split(",")
        else:
            stop_phrases = get_config().get_section("dictate").get("stop_phrases")

        with backend_compat(stt_provider):
            from .dictate import main as dictate_main
            sys.exit(dictate_main(
                model_name=stt_model,
                device=stt_device,
                threads=args.threads,
                workers=args.workers,
                stop_phrases=stop_phrases
            ))

    elif args.wake:
        if needs_faster_whisper_files(stt_provider):
            from .whisper_loader import resolve_whisper_model_files
            stt_model = resolve_whisper_model_files(stt_model)

        with backend_compat(stt_provider):
            from .wake import main as wake_main
            try:
                exit_code = wake_main(
                    model=stt_model,
                    device=stt_device,
                    with_pets=args.pets,
                )
            except (RuntimeError, ValueError) as error:
                if stt_device != "cuda" or not is_gpu_initialization_error(error):
                    raise
                print(
                    f"GPU transcription unavailable ({error}); falling back to CPU.",
                    file=sys.stderr,
                )
                exit_code = wake_main(
                    model=stt_model,
                    device="cpu",
                    with_pets=args.pets,
                )
        sys.exit(exit_code)

    elif args.agent:
        from .agent_cli import main as agent_main
        sys.exit(agent_main())

    elif args.pets_settings:
        from .pets.cli import main_settings
        sys.exit(main_settings())

    elif args.pets:
        from .pets.cli import main_overlay
        sys.exit(main_overlay())

    elif args.command:
        from .console import ZaraConsole
        command_text = " ".join(args.command)

        try:
            console = ZaraConsole()
            success = console.execute_command(command_text)
            sys.exit(0 if success else 1)
        except Exception as e:
            print(f"Error: {e}", file=sys.stderr)
            sys.exit(1)

    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()
