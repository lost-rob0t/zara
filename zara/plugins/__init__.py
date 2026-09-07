"""Public contracts for user-installed Zara plugins."""

from .api import (
    PLUGIN_API_VERSION,
    ManagedWorker,
    PluginMetadata,
    PluginRuntime,
    RuntimeStatus,
    ServicePlugin,
)
from .cancellation import tool_cancellation_requested
from .manager import PluginDiagnostic, PluginManager, PluginState

__all__ = [
    "ManagedWorker",
    "PLUGIN_API_VERSION",
    "PluginDiagnostic",
    "PluginManager",
    "PluginMetadata",
    "PluginRuntime",
    "PluginState",
    "RuntimeStatus",
    "ServicePlugin",
    "tool_cancellation_requested",
]
