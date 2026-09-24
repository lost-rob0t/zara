"""Desktop-neutral Zara runtime boundary.

The runtime package contains application/domain contracts that may be
consumed by headless, desktop, pet, and future remote surfaces. It must not
import Qt or any other concrete UI implementation.
"""

from . import bridge, events, registry
from .package_profiles import (
    PACKAGE_PROFILE_SCHEMA,
    AppPackageProfile,
    PackageProfileError,
    activate_profile_package,
)
from .symbols import (
    ProgrammableSymbolRegistry,
    SymbolDiagnostic,
    SymbolLookupError,
    SymbolRegistration,
    SymbolRegistrationError,
    SymbolSpec,
)

__all__ = [
    "PACKAGE_PROFILE_SCHEMA",
    "AppPackageProfile",
    "PackageProfileError",
    "ProgrammableSymbolRegistry",
    "SymbolDiagnostic",
    "SymbolLookupError",
    "SymbolRegistration",
    "SymbolRegistrationError",
    "SymbolSpec",
    "activate_profile_package",
    "bridge",
    "events",
    "registry",
]
