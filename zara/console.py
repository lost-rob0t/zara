#!/usr/bin/env python3
"""
Zara Console - Text-based command interface
Wraps the Prolog REPL for programmatic text input
"""

import sys
from pathlib import Path
from typing import Optional
import logging

try:
    from .prolog_engine import PrologEngine
except ImportError:
    from prolog_engine import PrologEngine


class ZaraConsole:
    """Text interface to Zarathustra assistant"""

    def __init__(self, main_file: Optional[Path] = None):
        self.logger = logging.getLogger(__name__)

        if main_file is None:
            # Look for main.pl relative to this file
            project_root = Path(__file__).parent.parent
            main_file = project_root / "main.pl"

        if not main_file.exists():
            raise FileNotFoundError(f"main.pl not found at {main_file}")

        self.logger.info(f"Loading Prolog from: {main_file}")
        self.engine = PrologEngine(main_file)

    def execute_command(self, text: str) -> bool:
        """Execute a single command and return success status"""
        return self.engine.execute_command(text)

    def repl(self):
        """Interactive REPL mode"""
        print("Thus spoke Zarathustra...")
        print("Type 'quit' to exit\n")

        while True:
            try:
                text = input("> ").strip()

                if text in ["quit", "exit", "q"]:
                    print("The prophet has spoken.")
                    break

                if not text:
                    continue

                self.execute_command(text)

            except (KeyboardInterrupt, EOFError):
                print("\nThe prophet has spoken.")
                break
            except Exception as e:
                self.logger.error(f"Error: {e}")
                print(f"Error: {e}")


def main():
    """CLI entry point"""
    import argparse

    parser = argparse.ArgumentParser(
        description="Zara Console - Text-based command interface"
    )
    parser.add_argument(
        "command",
        nargs="*",
        help="Command to execute (if empty, starts REPL)"
    )
    parser.add_argument(
        "--main-file",
        type=Path,
        help="Path to main.pl (auto-detected by default)"
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose logging"
    )

    args = parser.parse_args()

    # Setup logging
    logging.basicConfig(
        level=logging.INFO if args.verbose else logging.WARNING,
        format="[%(levelname)s] %(message)s"
    )

    try:
        console = ZaraConsole(main_file=args.main_file)

        if args.command:
            # Execute single command from args
            command_text = " ".join(args.command)
            success = console.execute_command(command_text)
            sys.exit(0 if success else 1)
        else:
            # Start interactive REPL
            console.repl()

    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        logging.exception(e)
        sys.exit(1)


if __name__ == "__main__":
    main()
