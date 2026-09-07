"""Compatibility imports for the generic :mod:`zara.plugins` API."""

from zara.plugins import (
    PLUGIN_API_VERSION,
    ManagedWorker,
    PluginDiagnostic,
    PluginManager,
    PluginMetadata,
    PluginRuntime,
    PluginState,
    RuntimeStatus,
    ServicePlugin,
    tool_cancellation_requested,
)

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
