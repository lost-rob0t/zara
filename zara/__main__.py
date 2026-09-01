#!/usr/bin/env python3
"""Zara unified command-line entry point."""

from __future__ import annotations

import argparse
import sys
from typing import Optional, Sequence

from .config import init_config
from .stt_backends import (
    STT_PROVIDERS,
    backend_compat,
    needs_faster_whisper_files,
    needs_whisper_cpp_files,
    normalize_provider,
    resolve_model_for_provider,
)


STT_DEVICE_ALIASES = {
    "amd": "cuda",
    "hip": "cuda",
    "rocm": "cuda",
}
STT_DEVICE_CHOICES = ["cpu", "cuda", "vulkan", "rocm", "hip", "amd"]
GPU_ERROR_MARKERS = (
    "cuda",
    "cudnn",
    "gpu",
    "hip",
    "rocm",
    "vulkan",
    "vk_",
    "gfx",
)


def normalize_stt_device(device: str, provider: str | None = None) -> str:
    normalized = str(device).strip().lower()
    normalized_provider = normalize_provider(provider) if provider is not None else None

    if normalized_provider == "whisper-cpp":
        if normalized == "cpu":
            return "cpu"
        if normalized in {"vulkan", "amd", "hip", "rocm"}:
            return "vulkan"
        if normalized == "cuda":
            raise ValueError(
                "Zara's whisper.cpp backend uses Vulkan for GPU STT; "
                "choose --device vulkan (or amd/rocm/hip)"
            )

    if normalized in {"cpu", "cuda"}:
        return normalized
    if normalized in STT_DEVICE_ALIASES:
        return STT_DEVICE_ALIASES[normalized]

    choices = ", ".join(STT_DEVICE_CHOICES)
    raise ValueError(f"Unsupported STT device {device!r}; choose one of: {choices}")


def is_gpu_initialization_error(error: Exception) -> bool:
    message = str(error).lower()
    return any(marker in message for marker in GPU_ERROR_MARKERS)


def resolve_local_stt_model(provider: str, model: str) -> str:
    if needs_faster_whisper_files(provider):
        from .whisper_loader import resolve_whisper_model_files

        return resolve_whisper_model_files(model)
    if needs_whisper_cpp_files(provider):
        from .whisper_cpp import resolve_whisper_cpp_model

        return resolve_whisper_cpp_model(model)
    return model


def _parser(config) -> argparse.ArgumentParser:
    stt_config = config.get_section("stt") if config is not None else {}
    default_stt_provider = normalize_provider(stt_config.get("provider", "faster-whisper"))
    default_stt_model = stt_config.get("model", "small")
    default_stt_device = stt_config.get("device", "cpu")

    parser = argparse.ArgumentParser(
        prog="zara",
        description="Zarathustra Voice Assistant - unified Zara runtime",
        epilog=(
            "Examples:\n"
            "  zara                              # Interactive TUI\n"
            "  zara 'open firefox'               # One-shot agent/runtime task\n"
            "  zara --agent 'open firefox'       # Same task path; compatibility alias\n"
            "  zara --connect ipc:///run/user/1000/zara-server.sock\n"
            "  zara --desktop                    # Native desktop / Quick Copilot\n"
            "  zara --dictate                    # Continuous dictation mode\n"
            "  zara --wake                       # Wake word listener\n"
            "  zara mcp status                   # Inspect MCP connections\n"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    mode_group = parser.add_mutually_exclusive_group()
    mode_group.add_argument(
        "--desktop",
        action="store_true",
        help="Start the native desktop Copilot",
    )
    mode_group.add_argument(
        "--console",
        action="store_true",
        help="Compatibility alias for the interactive TUI",
    )
    mode_group.add_argument(
        "--voice",
        action="store_true",
        help="Single voice command mode",
    )
    mode_group.add_argument(
        "--dictate",
        action="store_true",
        help="Continuous dictation mode",
    )
    mode_group.add_argument(
        "--wake",
        action="store_true",
        help="Wake word listener mode",
    )
    mode_group.add_argument(
        "--agent",
        action="store_true",
        help="Compatibility alias for Zara's normal terminal agent path",
    )

    client_group = parser.add_mutually_exclusive_group()
    client_group.add_argument(
        "--connect",
        metavar="ENDPOINT",
        help="Use an existing Zara daemon through the ZaraClient boundary",
    )
    client_group.add_argument(
        "--standalone",
        action="store_true",
        help="Use a private in-process Zara runtime",
    )

    parser.add_argument(
        "--pets",
        action="store_true",
        help="Launch the desktop pet overlay (companion flag; use with --wake)",
    )
    parser.add_argument(
        "--pets-settings",
        action="store_true",
        help="Open the Pets settings dialog",
    )
    parser.add_argument(
        "command",
        nargs="*",
        help="Task to submit to Zara's canonical runtime",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Enable verbose logging",
    )
    parser.add_argument(
        "--stt-provider",
        default=default_stt_provider,
        choices=STT_PROVIDERS,
        help=f"Speech-to-text provider (default: {default_stt_provider})",
    )
    parser.add_argument(
        "--model",
        "--mode",
        dest="model",
        default=default_stt_model,
        help=(
            "STT model name, local model directory, or GGML model file "
            f"(default: {default_stt_model})"
        ),
    )
    parser.add_argument(
        "--device",
        default=default_stt_device,
        choices=STT_DEVICE_CHOICES,
        help=(
            f"Device for local transcription (default: {default_stt_device}); "
            "whisper-cpp uses Vulkan for AMD GPU acceleration"
        ),
    )
    parser.add_argument(
        "--threads",
        type=int,
        help="Number of local STT inference threads",
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=2,
        help="Number of parallel transcription workers (default: 2)",
    )
    parser.add_argument(
        "--stop-phrases",
        help='Stop phrases for dictation (comma-separated, e.g. "end voice,stop voice")',
    )
    return parser


def run(argv: Optional[Sequence[str]] = None) -> int:
    config = init_config()
    args_list = list(sys.argv[1:] if argv is None else argv)

    if args_list and args_list[0] == "mcp":
        from .mcp.cli import main as mcp_main

        return int(mcp_main(args_list[1:], config=config))

    parser = _parser(config)
    if argv is None and not args_list and not (sys.stdin.isatty() and sys.stdout.isatty()):
        parser.print_help()
        return 1

    args = parser.parse_args(args_list)
    stt_provider = normalize_provider(args.stt_provider)
    stt_device = normalize_stt_device(args.device, provider=stt_provider)
    stt_model = resolve_model_for_provider(stt_provider, args.model)

    if args.desktop:
        from .desktop.app import main as desktop_main

        return int(desktop_main(["zara-desktop"]))

    if args.voice:
        print("Error: Voice mode is not currently implemented.", file=sys.stderr)
        print("Use --dictate for continuous voice input instead.", file=sys.stderr)
        return 1

    if args.dictate:
        from .config import get_config

        if args.stop_phrases:
            stop_phrases = args.stop_phrases.split(",")
        else:
            stop_phrases = get_config().get_section("dictate").get("stop_phrases")

        if needs_whisper_cpp_files(stt_provider):
            stt_model = resolve_local_stt_model(stt_provider, stt_model)

        dictate_device = (
            "cuda"
            if stt_provider == "whisper-cpp" and stt_device == "vulkan"
            else stt_device
        )

        with backend_compat(stt_provider):
            from .dictate import main as dictate_main

            return int(
                dictate_main(
                    model_name=stt_model,
                    device=dictate_device,
                    threads=args.threads,
                    workers=args.workers,
                    stop_phrases=stop_phrases,
                )
            )

    if args.wake:
        stt_model = resolve_local_stt_model(stt_provider, stt_model)

        with backend_compat(stt_provider):
            from .wake import main as wake_main

            try:
                return int(
                    wake_main(
                        model=stt_model,
                        device=stt_device,
                        with_pets=args.pets,
                    )
                )
            except (RuntimeError, ValueError) as error:
                if stt_device not in {"cuda", "vulkan"} or not is_gpu_initialization_error(error):
                    raise
                print(
                    f"GPU transcription unavailable ({error}); falling back to CPU.",
                    file=sys.stderr,
                )
                return int(
                    wake_main(
                        model=stt_model,
                        device="cpu",
                        with_pets=args.pets,
                    )
                )

    if args.pets_settings:
        from .pets.cli import main_settings

        return int(main_settings())

    if args.pets:
        from .pets.cli import main_overlay

        return int(main_overlay())

    from .terminal import run_task, run_tui

    endpoint = args.connect
    if args.command:
        return run_task(" ".join(args.command), endpoint=endpoint, config=config)

    return run_tui(endpoint=endpoint, config=config)


def main() -> None:
    raise SystemExit(run())


if __name__ == "__main__":
    main()
