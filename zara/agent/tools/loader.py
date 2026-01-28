"""
Dynamic plugin loader for user-defined tools.

Loads tool plugins from user directories at runtime.
"""

import os
import sys
import importlib.util
from pathlib import Path
from typing import List, Optional
from .base import BaseTool


def load_plugins(plugin_dir: str, prolog_engine=None) -> List[BaseTool]:
    """
    Load tool plugins from directory.

    Searches for Python files in plugin_dir and imports them.
    Each plugin file should define a register_tools() or register_skills()
    function that returns a list of tool/skill instances.

    Args:
        plugin_dir: Path to plugin directory
        prolog_engine: Optional PrologEngine instance for Prolog-backed tools

    Returns:
        List of loaded tool instances
    """
    plugin_path = Path(plugin_dir).expanduser()

    if not plugin_path.exists():
        return []

    if not plugin_path.is_dir():
        return []

    tools = []

    # Find all .py files in plugin directory
    for file_path in plugin_path.glob("*.py"):
        if file_path.name.startswith("_"):
            # Skip __init__.py and private files
            continue

        try:
            # Load plugin module
            module = _load_module_from_file(file_path)

            # Try to get tools from register_tools() or register_skills()
            if hasattr(module, "register_tools"):
                plugin_tools = module.register_tools(prolog_engine)
                tools.extend(plugin_tools)
            elif hasattr(module, "register_skills"):
                plugin_tools = module.register_skills(prolog_engine)
                tools.extend(plugin_tools)

        except Exception as e:
            # Log error but don't fail entire load
            print(f"Warning: Failed to load plugin {file_path}: {e}")
            continue

    return tools


def _load_module_from_file(file_path: Path):
    """
    Load Python module from file path.

    Args:
        file_path: Path to .py file

    Returns:
        Loaded module object
    """
    module_name = file_path.stem

    spec = importlib.util.spec_from_file_location(module_name, file_path)
    if spec is None or spec.loader is None:
        raise ImportError(f"Cannot load module from {file_path}")

    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)

    return module
