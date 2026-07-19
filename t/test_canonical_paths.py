"""Prevents duplicate independent entrypoint logic (ZARA-020).

These tests enforce the canonical-runtime contract:

* ``scripts/zara_wake.py`` and ``scripts/zara_dictate.py`` must be thin
  delegates that import and call the canonical module — they must NOT
  contain independent wake or dictation logic.
* ``zara/agent/graph.py`` is the sole active agent flow. The deprecated
  ``nodes.py``, ``routing.py``, ``state.py``, and ``tools/base.py`` files
  must not exist.
* ``ToolRegistry`` must use LangChain's ``BaseTool`` — no custom tool
  base class.
* Each console-script entrypoint declared in ``setup.py`` must resolve to
  a callable in the canonical module it claims to delegate to.
"""

from __future__ import annotations

import ast
import importlib
import pathlib
import sys

import pytest

ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPTS = ROOT / "scripts"
AGENT = ROOT / "zara" / "agent"
TOOLS = AGENT / "tools"

DEPRECATED_FILES = [
    AGENT / "nodes.py",
    AGENT / "routing.py",
    AGENT / "state.py",
    TOOLS / "base.py",
]


def test_deprecated_agent_files_do_not_exist():
    for path in DEPRECATED_FILES:
        assert not path.exists(), f"deprecated file should be removed: {path}"


def test_agent_graph_is_the_sole_agent_state_source():
    """graph.py defines AgentState; no other agent module should re-export a
    competing state schema."""
    graph_spec = importlib.util.spec_from_file_location(
        "zara.agent.graph", ROOT / "zara" / "agent" / "graph.py"
    )
    assert graph_spec is not None
    graph_mod = importlib.util.module_from_spec(graph_spec)
    assert hasattr(graph_mod, "__name__")

    # Ensure no other file in zara/agent/ defines a class named AgentState.
    for py_file in AGENT.rglob("*.py"):
        if py_file.name == "__init__.py":
            continue
        if py_file.name == "graph.py":
            continue
        source = py_file.read_text()
        tree = ast.parse(source)
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef) and node.name == "AgentState":
                pytest.fail(
                    f"{py_file} defines AgentState; graph.py is the canonical source"
                )


def test_scripts_zara_wake_is_a_thin_delegate():
    """scripts/zara_wake.py must only import and call zara.wake.main."""
    source = (SCRIPTS / "zara_wake.py").read_text()
    tree = ast.parse(source)

    imports = []
    calls = []
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom):
            imports.append((node.module, [a.name for a in node.names]))
        if isinstance(node, ast.Import):
            imports.append((None, [a.name for a in node.names]))
        if isinstance(node, ast.Call):
            func = node.func
            if isinstance(func, ast.Name):
                calls.append(func.id)
            elif isinstance(func, ast.Attribute):
                calls.append(func.attr)

    assert any(
        mod == "zara.wake" and "main" in names
        for mod, names in imports
    ), "scripts/zara_wake.py must import main from zara.wake"

    assert "main" in calls, "scripts/zara_wake.py must call main()"

    # The file should be short — a thin delegate, not independent logic.
    line_count = len(source.strip().splitlines())
    assert line_count <= 15, (
        f"scripts/zara_wake.py has {line_count} lines; expected a thin delegate (<=15)"
    )


def test_scripts_zara_dictate_is_a_thin_delegate():
    """scripts/zara_dictate.py must only import and call zara.dictate.cli_main."""
    source = (SCRIPTS / "zara_dictate.py").read_text()
    tree = ast.parse(source)

    imports = []
    calls = []
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom):
            imports.append((node.module, [a.name for a in node.names]))
        if isinstance(node, ast.Import):
            imports.append((None, [a.name for a in node.names]))
        if isinstance(node, ast.Call):
            func = node.func
            if isinstance(func, ast.Name):
                calls.append(func.id)
            elif isinstance(func, ast.Attribute):
                calls.append(func.attr)

    assert any(
        mod == "zara.dictate" and "cli_main" in names
        for mod, names in imports
    ), "scripts/zara_dictate.py must import cli_main from zara.dictate"

    assert "cli_main" in calls, "scripts/zara_dictate.py must call cli_main()"

    line_count = len(source.strip().splitlines())
    assert line_count <= 15, (
        f"scripts/zara_dictate.py has {line_count} lines; expected a thin delegate (<=15)"
    )


def test_tool_registry_uses_langchain_base_tool():
    """ToolRegistry must use langchain_core.tools.BaseTool, not a custom base."""
    from zara.agent.tools.registry import ToolRegistry
    from langchain_core.tools import BaseTool as LangChainTool

    # The registry stores tools as LangChainTool instances.
    import inspect
    src = inspect.getsource(ToolRegistry)
    assert "LangChainTool" in src or "BaseTool" in src, (
        "ToolRegistry must reference LangChain BaseTool"
    )
    assert "_tools" in inspect.getsource(ToolRegistry.__init__), (
        "ToolRegistry stores tools in _tools dict"
    )


def test_console_script_entrypoints_resolve():
    """Every console_scripts entrypoint in setup.py must resolve to a callable."""
    setup_path = ROOT / "setup.py"
    source = setup_path.read_text()
    tree = ast.parse(source)

    # Parse the setup() call to find entry_points
    for node in ast.walk(tree):
        if isinstance(node, ast.Call):
            for keyword in node.keywords:
                if keyword.arg == "entry_points":
                    entry_dict = keyword.value
                    if isinstance(entry_dict, ast.Dict):
                        for key, val in zip(entry_dict.keys, entry_dict.values):
                            if isinstance(key, ast.Constant) and key.value == "console_scripts":
                                if isinstance(val, ast.List):
                                    for elt in val.elts:
                                        if isinstance(elt, ast.Constant):
                                            entry_line = elt.value
                                            # Parse "name = module:function"
                                            if "=" in entry_line:
                                                ep_name, ep_target = entry_line.split("=", 1)
                                                ep_name = ep_name.strip()
                                                ep_target = ep_target.strip()
                                                module_path, func_name = ep_target.rsplit(":", 1)
                                                mod = importlib.import_module(module_path)
                                                func = getattr(mod, func_name)
                                                assert callable(func), (
                                                    f"entrypoint {ep_name} -> {ep_target} "
                                                    f"is not callable"
                                                )

def test_agent_init_imports_from_graph_not_nodes_or_routing():
    """zara/agent/__init__.py must import from graph.py, not nodes.py or routing.py."""
    source = (AGENT / "__init__.py").read_text()
    assert "from .graph import" in source or "from .graph " in source, (
        "agent __init__.py must import from graph.py"
    )
    assert "from .nodes import" not in source, (
        "agent __init__.py must not import from deprecated nodes.py"
    )
    assert "from .routing import" not in source, (
        "agent __init__.py must not import from deprecated routing.py"
    )
    assert "from .state import" not in source, (
        "agent __init__.py must not import from deprecated state.py"
    )
