#!/usr/bin/env python3
"""
Zara - Unified CLI Interface
Wraps console (text), voice, and dictate modes
"""

import argparse
import queue
import sys
import time
from pathlib import Path

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
CLI_TURN_TIMEOUT_SECONDS = 30.0


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


def _wait_for_daemon_turn(subscription, turn_id: str) -> str:
    from .runtime import events

    if not turn_id:
        raise RuntimeError("daemon did not assign a turn id")

    deadline = time.monotonic() + CLI_TURN_TIMEOUT_SECONDS
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError("daemon turn timed out")
        try:
            envelope = subscription.get(timeout=remaining)
        except queue.Empty as error:
            raise TimeoutError("daemon turn timed out") from error

        event = envelope.event
        if event.turn_id != turn_id:
            continue
        if isinstance(event, events.ResponseText):
            return event.text
        if isinstance(event, events.AgentCompleted):
            if not event.success:
                raise RuntimeError("agent execution failed")
            return ""
        if isinstance(event, (events.AssistantFailed, events.AgentFailed)):
            raise RuntimeError(event.reason or "daemon turn failed")
        if isinstance(event, events.TurnCancelled):
            raise RuntimeError(event.reason or "daemon turn cancelled")


def _default_daemon_endpoint() -> str:
    from .server import default_server_runtime_dir, default_zmq_endpoint

    return default_zmq_endpoint(default_server_runtime_dir())


def _open_daemon_client(endpoint: str):
    from .zmq_transport import ZmqZaraClient

    client = ZmqZaraClient(endpoint)
    try:
        client.start().result()
        subscription = client.subscribe()
    except BaseException:
        try:
            client.close()
        except Exception:
            pass
        raise
    return client, subscription


def _submit_daemon_text(client, subscription, command_text: str) -> str:
    from .runtime.commands import SubmitTurn

    receipt = client.submit(SubmitTurn(text=command_text)).result()
    return _wait_for_daemon_turn(subscription, receipt.turn_id)


def _print_daemon_error(endpoint: str, error: BaseException) -> None:
    print(f"Error: Zara backend unavailable at {endpoint}: {error}", file=sys.stderr)
    print(
        "Start it with 'zara-server', or use '--standalone' for a private runtime.",
        file=sys.stderr,
    )


def _run_connected_text(endpoint: str, command_text: str) -> int:
    client = None
    subscription = None
    exit_code = 0
    try:
        client, subscription = _open_daemon_client(endpoint)
        response = _submit_daemon_text(client, subscription, command_text)
        if response:
            print(response)
    except Exception as error:
        _print_daemon_error(endpoint, error)
        exit_code = 2
    finally:
        if subscription is not None:
            try:
                subscription.close()
            except Exception as error:
                if exit_code == 0:
                    print(f"Error: {error}", file=sys.stderr)
                    exit_code = 2
        if client is not None:
            try:
                client.close()
            except Exception as error:
                if exit_code == 0:
                    print(f"Error: {error}", file=sys.stderr)
                    exit_code = 2
    return exit_code


def _run_connected_console(endpoint: str) -> int:
    client = None
    subscription = None
    exit_code = 0
    try:
        client, subscription = _open_daemon_client(endpoint)
        print(f"Connected to Zara at {endpoint}. Type 'exit' or 'quit' to leave.")
        while True:
            try:
                command_text = input("zara> ").strip()
            except (EOFError, KeyboardInterrupt):
                print()
                break
            if command_text.lower() in {"exit", "quit"}:
                break
            if not command_text:
                continue
            response = _submit_daemon_text(client, subscription, command_text)
            if response:
                print(response)
    except Exception as error:
        _print_daemon_error(endpoint, error)
        exit_code = 2
    finally:
        if subscription is not None:
            try:
                subscription.close()
            except Exception as error:
                if exit_code == 0:
                    print(f"Error: {error}", file=sys.stderr)
                    exit_code = 2
        if client is not None:
            try:
                client.close()
            except Exception as error:
                if exit_code == 0:
                    print(f"Error: {error}", file=sys.stderr)
                    exit_code = 2
    return exit_code


def main():
    config = init_config()
    stt_config = config.get_section("stt") if config is not None else {}
    default_stt_provider = normalize_provider(stt_config.get("provider", "faster-whisper"))
    default_stt_model = stt_config.get("model", "small")
    default_stt_device = stt_config.get("device", "cpu")

    if len(sys.argv) > 1 and sys.argv[1] == "mcp":
        from .mcp.cli import main as mcp_main
        sys.exit(mcp_main(sys.argv[2:], config=config))

    parser = argparse.ArgumentParser(
        prog="zara",
        description="Zarathustra client for the local Zara backend",
        epilog="Examples:\n"
               "  zara                           # Interactive daemon client\n"
               "  zara 'open firefox'           # Send command to local daemon\n"
               "  zara --standalone 'hello'     # Explicit private local runtime\n"
               "  zara --connect ipc:///run/user/1000/zarathushtra/zara-server.sock 'hello'\n"
               "  zara --desktop                # Native desktop / Quick Copilot\n"
               "  zara --console                # Interactive daemon client\n"
               "  zara --voice                  # One-shot voice command\n"
               "  zara --dictate                # Continuous dictation mode\n"
               "  zara --wake                   # Wake word listener\n"
               "  zara mcp status               # Inspect MCP connections\n",
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
        help="Start the interactive daemon client"
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

    client_group = parser.add_mutually_exclusive_group()
    client_group.add_argument(
        "--connect",
        metavar="ENDPOINT",
        help="Override the owner-local Zara daemon endpoint"
    )
    client_group.add_argument(
        "--standalone",
        action="store_true",
        help="Use a private in-process runtime instead of the Zara daemon"
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
        help=f"STT model name, local model directory, or GGML model file (default: {default_stt_model})"
    )
    parser.add_argument(
        "--device",
        default=default_stt_device,
        choices=STT_DEVICE_CHOICES,
        help=(
            f"Device for local transcription (default: {default_stt_device}); "
            "whisper-cpp uses Vulkan for AMD GPU acceleration"
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
    stt_device = normalize_stt_device(args.device, provider=stt_provider)
    stt_model = resolve_model_for_provider(stt_provider, args.model)

    if args.desktop:
        from .desktop.app import main as desktop_main
        sys.exit(desktop_main([sys.argv[0]]))

    elif args.console:
        if args.standalone:
            from .console import main as console_main
            sys.exit(console_main())
        sys.exit(_run_connected_console(args.connect or _default_daemon_endpoint()))

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

        if needs_whisper_cpp_files(stt_provider):
            stt_model = resolve_local_stt_model(stt_provider, stt_model)

        # dictate.py still exposes the historical faster-whisper device API,
        # where every GPU is represented as `cuda`. The whisper.cpp adapter
        # translates that compatibility token back to Vulkan internally.
        dictate_device = (
            "cuda"
            if stt_provider == "whisper-cpp" and stt_device == "vulkan"
            else stt_device
        )

        with backend_compat(stt_provider):
            from .dictate import main as dictate_main
            sys.exit(dictate_main(
                model_name=stt_model,
                device=dictate_device,
                threads=args.threads,
                workers=args.workers,
                stop_phrases=stop_phrases
            ))

    elif args.wake:
        stt_model = resolve_local_stt_model(stt_provider, stt_model)

        with backend_compat(stt_provider):
            from .wake import main as wake_main
            try:
                exit_code = wake_main(
                    model=stt_model,
                    device=stt_device,
                    with_pets=args.pets,
                )
            except (RuntimeError, ValueError) as error:
                if stt_device not in {"cuda", "vulkan"} or not is_gpu_initialization_error(error):
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
        command_text = " ".join(args.command)

        if not args.standalone:
            sys.exit(
                _run_connected_text(
                    args.connect or _default_daemon_endpoint(),
                    command_text,
                )
            )

        from .console import ZaraConsole

        try:
            console = ZaraConsole()
            success = console.execute_command(command_text)
            sys.exit(0 if success else 1)
        except Exception as error:
            print(f"Error: {error}", file=sys.stderr)
            sys.exit(1)

    else:
        if args.standalone:
            from .console import main as console_main
            sys.exit(console_main())
        sys.exit(_run_connected_console(args.connect or _default_daemon_endpoint()))


if __name__ == "__main__":
    main()
