"""Opaque values for Core-owned service-plugin capability composition."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Optional


MAX_CAPABILITY_NAME_LENGTH = 128
MAX_CAPABILITY_ARGUMENT_BYTES = 65536
MAX_CAPABILITY_RESULT_BYTES = 262144
MAX_CAPABILITY_TIMEOUT_SECONDS = 300.0


class PluginCapabilityError(RuntimeError):
    """A Core-owned plugin capability operation could not be completed."""


class PluginCapabilityUnavailable(PluginCapabilityError):
    """A capability is not currently available to the calling plugin."""


@dataclass(frozen=True)
class PluginCapability:
    """Opaque handle to one currently loaded service-plugin tool capability."""

    name: str
    owner: str
    _token: str = field(repr=False, compare=True)


@dataclass(frozen=True)
class PluginCapabilityResult:
    """Bounded structured outcome from one Core-routed capability invocation."""

    success: bool
    value: Any = None
    error: str = ""
    cancelled: bool = False
    tool_run_id: Optional[str] = None


__all__ = [
    "MAX_CAPABILITY_ARGUMENT_BYTES",
    "MAX_CAPABILITY_NAME_LENGTH",
    "MAX_CAPABILITY_RESULT_BYTES",
    "MAX_CAPABILITY_TIMEOUT_SECONDS",
    "PluginCapability",
    "PluginCapabilityError",
    "PluginCapabilityResult",
    "PluginCapabilityUnavailable",
]
