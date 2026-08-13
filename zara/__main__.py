#!/usr/bin/env python3
"""
Zara - Unified CLI Interface
Wraps console (text), voice, and dictate modes
"""

import sys
import argparse
from pathlib import Path
from .config import init_config


def main():
    # Initialize configuration system
    config = init_config()
    stt_config = config.get_section("stt") if config is not None else {}
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

    # Mode selection (mutually exclusive)
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

    # --pets is a COMPANION flag, not a mode: it can combine with --wake
    # (and --agent) so the pet overlay runs alongside the runtime and
    # reacts to real events over the ZMQ bridge.
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

    # Text command (default mode if no flags)
    parser.add_argument(
        "command",
        nargs="*",
        help="Text command to execute"
    )

    # Common options
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose logging"
    )

    # Dictate/wake STT options
    parser.add_argument(
        "--model",
        default=default_stt_model,
        help=f"Whisper model for dictation/wake (default: {default_stt_model})"
    )
    parser.add_argument(
        "--device",
        default=default_stt_device,
        choices=["cpu", "cuda"],
        help=f"Device for transcription (default: {default_stt_device})"
    )
    parser.add_argument(
        "--threads",
        type=int,
        help="Number of threads for Whisper"
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

    # Determine mode
    if args.desktop:
        from .desktop.app import main as desktop_main
        sys.exit(desktop_main([sys.argv[0]]))

    elif args.console:
        # Interactive console mode
        from .console import main as console_main
        sys.exit(console_main())

    elif args.voice:
        # Single voice command mode - not implemented
        print("Error: Voice mode is not currently implemented.", file=sys.stderr)
        print("Use --dictate for continuous voice input instead.", file=sys.stderr)
        sys.exit(1)

    elif args.dictate:
        from .config import get_config
        from .dictate import main as dictate_main
        if args.stop_phrases:
            stop_phrases = args.stop_phrases.split(",")
        else:
            stop_phrases = get_config().get_section("dictate").get("stop_phrases")
        sys.exit(dictate_main(
            model_name=args.model,
            device=args.device,
            threads=args.threads,
            workers=args.workers,
            stop_phrases=stop_phrases
        ))

    elif args.wake:
        # Wake word listener mode
        from .wake import main as wake_main
        sys.exit(wake_main(
            model=args.model,
            device=args.device,
            with_pets=args.pets,
        ))

    elif args.agent:
        # Direct conversation mode with agent
        from .agent_cli import main as agent_main
        sys.exit(agent_main())

    elif args.pets_settings:
        from .pets.cli import main_settings
        sys.exit(main_settings())

    elif args.pets:
        # --pets with no mode: launch the overlay standalone
        from .pets.cli import main_overlay
        sys.exit(main_overlay())

    elif args.command:
        # Text command mode (default)
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
        # No mode or command specified, show help
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()