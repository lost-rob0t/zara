"""Dynamic loader shared by legacy tools and service plugins."""

import importlib.util
import logging
import sys
import threading
from pathlib import Path
from typing import Iterable, List

from langchain_core.tools import BaseTool

logger = logging.getLogger(__name__)
_MODULE_CACHE = {}
_MODULE_CACHE_LOCK = threading.RLock()


def iter_plugin_files(paths: Iterable[Path | str]):
    """Yield public Python plugin files in deterministic search-path order."""
    seen = set()
    for raw_path in paths:
        plugin_path = Path(raw_path).expanduser()
        if not plugin_path.is_dir():
            continue
        for file_path in sorted(plugin_path.glob("*.py")):
            if file_path.name.startswith("_"):
                continue
            resolved = file_path.resolve()
            if resolved in seen:
                continue
            seen.add(resolved)
            yield resolved


def load_plugins(plugin_dir: str, prolog_engine=None) -> List[BaseTool]:
    """
    Load tool plugins from directory.

    Searches for Python files in plugin_dir and imports them.
    Each plugin file should define a register_tools() or register_skills()
    function that returns a list of LangChain tool instances.

    Args:
        plugin_dir: Path to plugin directory
        prolog_engine: Optional PrologEngine instance for Prolog-backed tools

    Returns:
        List of loaded tool instances
    """
    tools = []
    for file_path in iter_plugin_files((plugin_dir,)):
        try:
            module = load_plugin_module(file_path)
            if hasattr(module, "register_tools"):
                plugin_tools = tuple(module.register_tools(prolog_engine))
            elif hasattr(module, "register_skills"):
                plugin_tools = tuple(module.register_skills(prolog_engine))
            else:
                continue
            if any(not isinstance(tool, BaseTool) for tool in plugin_tools):
                raise TypeError("legacy plugin entrypoints must return LangChain BaseTool instances")
            tools.extend(plugin_tools)

        except Exception as error:
            logger.warning("Failed to load tool plugin %s: %s", file_path, error)
            continue

    return tools


def load_plugin_module(file_path: Path):
    """Import one plugin file at most once in the current process."""
    resolved = file_path.expanduser().resolve()
    with _MODULE_CACHE_LOCK:
        cached = _MODULE_CACHE.get(resolved)
        if cached is not None:
            return cached

        module_name = resolved.stem
        spec = importlib.util.spec_from_file_location(module_name, resolved)
        if spec is None or spec.loader is None:
            raise ImportError(f"Cannot load module from {resolved}")

        module = importlib.util.module_from_spec(spec)
        previous = sys.modules.get(module_name)
        sys.modules[module_name] = module
        try:
            spec.loader.exec_module(module)
        except Exception:
            if previous is None:
                sys.modules.pop(module_name, None)
            else:
                sys.modules[module_name] = previous
            raise
        _MODULE_CACHE[resolved] = module
        return module


_load_module_from_file = load_plugin_module
