"""Compatibility wrapper for Zara's generic plugin loader."""

from zara.plugins.loader import (
    _load_module_from_file,
    iter_plugin_files,
    load_plugin_module,
    load_plugins,
)

__all__ = [
    "_load_module_from_file",
    "iter_plugin_files",
    "load_plugin_module",
    "load_plugins",
]
