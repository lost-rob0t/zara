"""Desktop-neutral Zara runtime boundary.

The runtime package contains application/domain contracts that may be
consumed by headless, desktop, pet, and future remote surfaces. It must not
import Qt or any other concrete UI implementation.
"""

from . import bridge, events
from .symbols import (
    ProgrammableSymbolRegistry,
    SymbolDiagnostic,
    SymbolLookupError,
    SymbolRegistration,
    SymbolRegistrationError,
    SymbolSpec,
)

__all__ = [
    "ProgrammableSymbolRegistry",
    "SymbolDiagnostic",
    "SymbolLookupError",
    "SymbolRegistration",
    "SymbolRegistrationError",
    "SymbolSpec",
    "bridge",
    "events",
]
