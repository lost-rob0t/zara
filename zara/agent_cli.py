"""Compatibility entry point for Zara's unified terminal agent surface."""

from __future__ import annotations

import sys
from typing import Optional, Sequence

from . import __main__ as cli


def run(argv: Optional[Sequence[str]] = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    return cli.run(["--agent", *args])


def main() -> int:
    return run()


if __name__ == "__main__":
    raise SystemExit(main())
