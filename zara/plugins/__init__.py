"""Public contracts for user-installed Zara plugins."""

from zara.agent.tool_cancellation import ToolCancellation, current_tool_cancellation

from .api import (
    PLUGIN_API_VERSION,
    ManagedWorker,
    PluginMetadata,
    PluginRuntime,
    RuntimeStatus,
    ServicePlugin,
)
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
    "ToolCancellation",
    "current_tool_cancellation",
]
