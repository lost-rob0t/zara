"""
Tool registry for agent function calling.

Uses LangChain tools directly. The old custom registry is deprecated.
"""

import logging
from typing import Dict, List, Optional, Any, TYPE_CHECKING

from langchain_core.tools import BaseTool as LangChainTool

from ..approval import valid_tool_name
from ..tool_cancellation import bind_tool_cancellation_transport

logger = logging.getLogger(__name__)

if TYPE_CHECKING:
    from ...config import ZaraConfig


TODO_TOOL_NAMES = frozenset(
    {
        "list_todos",
        "add_todo",
        "edit_todo",
        "complete_todo",
        "reopen_todo",
        "search_todos",
        "schedule_todo",
        "export_todos",
    }
)

_APPROVAL_REQUIRED_METADATA_KEY = "zara_requires_approval"


def _tool_requires_approval(tool: LangChainTool) -> bool:
    metadata = getattr(tool, "metadata", None)
    if metadata is None:
        return False
    marker = metadata.get(_APPROVAL_REQUIRED_METADATA_KEY, False)
    if not isinstance(marker, bool):
        raise ValueError("zara_requires_approval tool metadata must be true or false")
    return marker


class ToolRegistry:
    """Central registry for all agent tools."""

    def __init__(self, prolog_engine=None, config: Optional["ZaraConfig"] = None):
        self.prolog_engine = prolog_engine
        self.config = config
        self._tools: Dict[str, LangChainTool] = {}
        self._mcp_manager: Any = None
        approval_config = config.get_section("tool_approval") if config else {}
        self._configured_approval_required = frozenset(
            str(name) for name in approval_config.get("required_tools", [])
        )
        self._registered_approval_required: set[str] = set()

    def register_tool(self, tool: LangChainTool):
        if not valid_tool_name(tool.name):
            raise ValueError("tool name is invalid")
        if tool.name in self._tools:
            raise ValueError(f"Tool '{tool.name}' already registered")
        requires_approval = _tool_requires_approval(tool)
        bound_tool = bind_tool_cancellation_transport(tool)
        self._tools[bound_tool.name] = bound_tool
        if requires_approval:
            self._registered_approval_required.add(bound_tool.name)

    def unregister_tool(self, name: str) -> Optional[LangChainTool]:
        tool = self._tools.pop(name, None)
        self._registered_approval_required.discard(name)
        return tool

    def register_tools(self, tools: List[LangChainTool]):
        pending = list(tools)
        names = [tool.name for tool in pending]
        if any(not valid_tool_name(name) for name in names):
            raise ValueError("tool name is invalid")
        duplicate_names = sorted({name for name in names if names.count(name) > 1})
        existing_names = sorted(set(names).intersection(self._tools))
        conflicts = duplicate_names + existing_names
        if conflicts:
            raise ValueError(f"Tool '{conflicts[0]}' already registered")

        required_names = [
            tool.name for tool in pending if _tool_requires_approval(tool)
        ]
        bound_tools = [bind_tool_cancellation_transport(tool) for tool in pending]
        self._tools.update((tool.name, tool) for tool in bound_tools)
        self._registered_approval_required.update(required_names)

    def unregister_tools(self, names: List[str]) -> None:
        for name in names:
            self._tools.pop(name, None)
            self._registered_approval_required.discard(name)

    def get_tool(self, name: str) -> Optional[LangChainTool]:
        return self._tools.get(name)

    def list_tools(self) -> List[str]:
        return list(self._tools.keys())

    def to_langchain_tools(self) -> List[LangChainTool]:
        return list(self._tools.values())

    def requires_approval(self, name: str) -> bool:
        return (
            name in self._configured_approval_required
            or name in self._registered_approval_required
        )

    async def prepare_async(self) -> None:
        if self.config is None:
            return
        if self._mcp_manager is None:
            from ...mcp import MCPManager

            self._mcp_manager = MCPManager(self.config, self)
        await self._mcp_manager.ensure_started()

    def dynamic_system_context(self) -> Optional[str]:
        if self._mcp_manager is None:
            return None
        return self._mcp_manager.system_context()

    async def shutdown_async(self) -> None:
        if self._mcp_manager is not None:
            await self._mcp_manager.shutdown()
            self._mcp_manager = None

    def execute_tool(self, name: str, **kwargs) -> str:
        tool = self.get_tool(name)
        if tool is None:
            raise ValueError(f"Tool '{name}' not found")

        try:
            result = tool.invoke(kwargs)
            return str(result)
        except Exception as e:
            raise Exception(f"Tool '{name}' execution failed: {str(e)}") from e

    def load_builtin_tools(self, memory_manager=None):
        """Load built-ins and apply subsystem-level capability toggles."""
        from pathlib import Path

        from .builtin_tools import get_builtin_tools
        from ...python_skills import python_skills

        repo_root = Path(__file__).resolve().parents[3]
        tool_config = self.config.get_tool_config() if self.config else {}
        todo_config = self.config.get_section("todo") if self.config else {}
        file_tool_config = None
        if self.config and tool_config.get("file_tools", False):
            file_tool_config = self.config.get_file_tool_config(repo_root)

        todos_enabled = todo_config.get("enabled", True)
        if not isinstance(todos_enabled, bool):
            raise ValueError("todo.enabled must be true or false")

        python_skills.set_todo_enabled(todos_enabled)
        if self.prolog_engine is not None:
            enabled_atom = "true" if todos_enabled else "false"
            self.prolog_engine.query_once(
                f"kb_intents:set_todo_intents_enabled({enabled_atom})"
            )

        if not todos_enabled:
            logger.info(
                "Built-in todo tools, Python skills, and intents disabled by todo.enabled=false"
            )

        all_tools = get_builtin_tools(
            self.prolog_engine,
            memory_manager=memory_manager,
            file_tool_config=file_tool_config,
        )
        if not todos_enabled:
            all_tools = [tool for tool in all_tools if tool.name not in TODO_TOOL_NAMES]

        if self.config:
            tools_to_register = [
                tool for tool in all_tools
                if tool_config.get(tool.name, True)
            ]
        else:
            tools_to_register = all_tools

        self.register_tools(tools_to_register)

    def load_user_tools(self, plugin_dir: str):
        from .loader import load_plugins

        tools = load_plugins(plugin_dir, self.prolog_engine)
        try:
            self.register_tools(tools)
        except ValueError as error:
            logger.warning("Skipping conflicting tools from %s: %s", plugin_dir, error)
