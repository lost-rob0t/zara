"""Repository-wide pytest bootstrap.

Qt selects its platform plugin when QApplication is constructed. Default tests
to the headless offscreen plugin before any test module can create an
application, while preserving an explicitly configured developer/CI platform.
"""

from __future__ import annotations

import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
