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
    from .config import get_config
    from .prolog_engine import PrologEngine
except ImportError:
    from config import get_config
    from prolog_engine import PrologEngine


def find_main_pl() -> Optional[Path]:
    """Locate ``main.pl`` across the supported install surfaces.

    The Nix packages ship Prolog under ``$out/share/zarathushtra/``; pip
    wheels install the same layout under ``<sys.prefix>/share/zarathushtra/``;
    editable checkouts keep ``main.pl`` at the project root. We probe each
    surface in order and return the first match.
    """
    module_path = Path(__file__).resolve()

    candidates = []

    if "/nix/store" in str(module_path):
        nix_package_root = module_path.parents[3]
        candidates.append(nix_package_root / "share" / "zarathushtra" / "main.pl")

    candidates.append(Path(sys.prefix) / "share" / "zarathushtra" / "main.pl")
    candidates.append(Path("/usr/share/zarathushtra/main.pl"))
    candidates.append(module_path.parent.parent / "main.pl")

    for candidate in candidates:
        if candidate.exists():
            return candidate
    return None


class ZaraConsole:
    """Text interface to Zarathustra assistant"""

    def __init__(self, main_file: Optional[Path] = None):
        self.logger = logging.getLogger(__name__)

        if main_file is None:
            main_file = find_main_pl()

        if main_file is None or not main_file.exists():
            raise FileNotFoundError(
                "main.pl not found; tried Nix store, sys.prefix, /usr/share, and project root"
            )

        self.logger.info(f"Loading Prolog from: {main_file}")
        self.engine = PrologEngine(main_file)
        self._apply_todo_surface_config()

    def _apply_todo_surface_config(self) -> None:
        tool_config = get_config().get_tool_config()
        todos_enabled = tool_config.get("todos", True)
        if not isinstance(todos_enabled, bool):
            raise ValueError("tools.todos must be true or false")
        enabled_atom = "true" if todos_enabled else "false"
        self.engine.query_once(
            f"kb_intents:set_todo_intents_enabled({enabled_atom})"
        )
        if not todos_enabled:
            self.logger.info("Built-in todo intents are disabled by tools.todos=false")

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

    logging.basicConfig(
        level=logging.INFO if args.verbose else logging.WARNING,
        format="[%(levelname)s] %(message)s"
    )

    try:
        console = ZaraConsole(main_file=args.main_file)

        if args.command:
            command_text = " ".join(args.command)
            success = console.execute_command(command_text)
            sys.exit(0 if success else 1)
        else:
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
