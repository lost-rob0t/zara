#!/usr/bin/env python3
"""Compatibility wrapper for the canonical wake listener."""

import pathlib
import sys


sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent.parent))

from zara.wake import main


if __name__ == "__main__":
    raise SystemExit(main())
