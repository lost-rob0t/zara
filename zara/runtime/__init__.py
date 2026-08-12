"""Desktop-neutral Zara runtime boundary.

The runtime package contains application/domain contracts that may be
consumed by headless, desktop, pet, and future remote surfaces. It must not
import Qt or any other concrete UI implementation.
"""

from . import bridge, events

__all__ = ["bridge", "events"]
