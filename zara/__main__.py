#!/usr/bin/env python3
"""
Zara - Unified CLI Interface
Wraps console (text), voice, and dictate modes
"""

import sys
import argparse
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(
        prog="zara",
        description="Zarathustra Voice Assistant - Unified Interface",
        epilog="Examples:\n"
               "  zara 'open firefox'           # Execute text command\n"
               "  zara --console                # Interactive REPL\n"
               "  zara --voice                  # One-shot voice command\n"
               "  zara --dictate                # Continuous dictation mode\n"
               "  zara --wake                   # Wake word listener\n",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    # Mode selection (mutually exclusive)
    mode_group = parser.add_mutually_exclusive_group()
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

    # Dictate-specific options
    parser.add_argument(
        "--model",
        default="small",
        help="Whisper model for dictation (default: small)"
    )
    parser.add_argument(
        "--device",
        default="cpu",
        choices=["cpu", "cuda"],
        help="Device for transcription (default: cpu)"
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
    if args.console:
        # Interactive console mode
        from .console import main as console_main
        sys.exit(console_main())

    elif args.voice:
        # Single voice command mode
        print("Voice mode: Record a command (4 seconds)")
        # Use the zara_voice.pl script
        import subprocess
        project_root = Path(__file__).parent.parent
        voice_script = project_root / "zara_voice.pl"
        result = subprocess.run([str(voice_script)], cwd=project_root)
        sys.exit(result.returncode)

    elif args.dictate:
        # Continuous dictation mode
        from .dictate import main as dictate_main
        stop_phrases = args.stop_phrases.split(",") if args.stop_phrases else None
        sys.exit(dictate_main(
            model_name=args.model,
            device=args.device,
            threads=args.threads,
            workers=args.workers,
            stop_phrases=stop_phrases
        ))

    elif args.wake:
        # Wake word listener mode
        import subprocess
        project_root = Path(__file__).parent.parent
        wake_script = project_root / "scripts" / "zara_wake.py"
        result = subprocess.run([sys.executable, str(wake_script)], cwd=project_root)
        sys.exit(result.returncode)

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
