"""
Tool registry for agent function calling.

Uses LangChain tools directly. The old custom registry is deprecated.
"""

import logging
from typing import Dict, List, Optional, Any, TYPE_CHECKING

from langchain_core.tools import BaseTool as LangChainTool

from ..approval import valid_tool_name

logger = logging.getLogger(__name__)

if TYPE_CHECKING:
    from ...config import ZaraConfig


class ToolRegistry:
    """
    Central registry for all agent tools.

    Manages tool registration, lookup, and execution.
    Converts tools to LangChain format for LLM function calling.
    """

    def __init__(self, prolog_engine=None, config: Optional["ZaraConfig"] = None):
        """
        Initialize tool registry.

        Args:
            prolog_engine: Optional PrologEngine instance for Prolog-backed tools
            config: Optional ZaraConfig instance for tool configuration
        """
        self.prolog_engine = prolog_engine
        self.config = config
        self._tools: Dict[str, LangChainTool] = {}
        self._mcp_manager: Any = None
        approval_config = config.get_section("tool_approval") if config else {}
        self._approval_required = frozenset(
            str(name) for name in approval_config.get("required_tools", [])
        )

    def register_tool(self, tool: LangChainTool):
        """Register one LangChain tool."""
        if not valid_tool_name(tool.name):
            raise ValueError("tool name is invalid")
        if tool.name in self._tools:
            raise ValueError(f"Tool '{tool.name}' already registered")
        self._tools[tool.name] = tool

    def unregister_tool(self, name: str) -> Optional[LangChainTool]:
        """Remove one tool if present and return the previous binding."""
        return self._tools.pop(name, None)

    def register_tools(self, tools: List[LangChainTool]):
        """Register multiple tools atomically with conflict checks."""
        pending = list(tools)
        names = [tool.name for tool in pending]
        if any(not valid_tool_name(name) for name in names):
            raise ValueError("tool name is invalid")
        duplicate_names = sorted({name for name in names if names.count(name) > 1})
        existing_names = sorted(set(names).intersection(self._tools))
        conflicts = duplicate_names + existing_names
        if conflicts:
            raise ValueError(f"Tool '{conflicts[0]}' already registered")
        self._tools.update((tool.name, tool) for tool in pending)

    def unregister_tools(self, names: List[str]) -> None:
        """Remove tools owned by a stopped service plugin."""
        for name in names:
            self._tools.pop(name, None)

    def attach_prolog_engine(self, prolog_engine) -> None:
        """Attach the canonical Prolog service and expose its tool if enabled."""
        if prolog_engine is None:
            raise ValueError("prolog_engine must not be None")
        if self.prolog_engine is not None and self.prolog_engine is not prolog_engine:
            raise RuntimeError("tool registry already owns a different Prolog engine")

        self.prolog_engine = prolog_engine
        tool_config = self.config.get_tool_config() if self.config else {}
        if not tool_config.get("query_prolog", True):
            return
        if self.get_tool("query_prolog") is not None:
            return

        from .builtin_tools import build_prolog_tool

        self.register_tool(build_prolog_tool(prolog_engine))

    def get_tool(self, name: str) -> Optional[LangChainTool]:
        """Return one tool by name."""
        return self._tools.get(name)

    def list_tools(self) -> List[str]:
        """List all registered tool names."""
        return list(self._tools.keys())

    def to_langchain_tools(self) -> List[LangChainTool]:
        """Return the tools already registered in LangChain format."""
        return list(self._tools.values())

    def requires_approval(self, name: str) -> bool:
        """Return the immutable server policy for one registered tool name."""
        return name in self._approval_required

    async def prepare_async(self) -> None:
        """Start/refresh optional dynamic capability providers before a turn."""
        if self.config is None:
            return
        if self._mcp_manager is None:
            from ...mcp import MCPManager

            self._mcp_manager = MCPManager(self.config, self)
        await self._mcp_manager.ensure_started()

    def dynamic_system_context(self) -> Optional[str]:
        """Return dynamic provider routing context for the current turn."""
        if self._mcp_manager is None:
            return None
        return self._mcp_manager.system_context()

    async def shutdown_async(self) -> None:
        """Shut down dynamic capability providers and their child processes."""
        if self._mcp_manager is not None:
            await self._mcp_manager.shutdown()
            self._mcp_manager = None

    def execute_tool(self, name: str, **kwargs) -> str:
        """Execute a registered tool and return its result as text."""
        tool = self.get_tool(name)
        if tool is None:
            raise ValueError(f"Tool '{name}' not found")

        try:
            result = tool.invoke(kwargs)
            return str(result)
        except Exception as error:
            raise Exception(f"Tool '{name}' execution failed: {str(error)}") from error

    def load_builtin_tools(self, memory_manager=None):
        """Load built-in tools while respecting tool configuration."""
        from pathlib import Path

        from .builtin_tools import get_builtin_tools

        repo_root = Path(__file__).resolve().parents[3]
        tool_config = self.config.get_tool_config() if self.config else {}
        file_tool_config = None
        if self.config and tool_config.get("file_tools", False):
            file_tool_config = self.config.get_file_tool_config(repo_root)
        all_tools = get_builtin_tools(
            self.prolog_engine,
            memory_manager=memory_manager,
            file_tool_config=file_tool_config,
        )

        if self.config:
            tools_to_register = [
                tool for tool in all_tools
                if tool_config.get(tool.name, True)
            ]
        else:
            tools_to_register = all_tools

        self.register_tools(tools_to_register)

    def load_user_tools(self, plugin_dir: str):
        """Load user-defined LangChain tools from one plugin directory."""
        from .loader import load_plugins

        tools = load_plugins(plugin_dir, self.prolog_engine)
        try:
            self.register_tools(tools)
        except ValueError as error:
            logger.warning("Skipping conflicting tools from %s: %s", plugin_dir, error)
