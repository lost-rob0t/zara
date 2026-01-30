"""
Tool registry for agent function calling.

Uses LangChain tools directly. The old custom registry is deprecated.
"""

from typing import Dict, List, Optional, Any, TYPE_CHECKING
from langchain_core.tools import BaseTool as LangChainTool




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

    def register_tool(self, tool: LangChainTool):
        """
        Register a single tool.

        Args:
            tool: Tool instance to register

        Raises:
            ValueError: If tool with same name already registered
        """
        if tool.name in self._tools:
            raise ValueError(f"Tool '{tool.name}' already registered")

        self._tools[tool.name] = tool

    def register_tools(self, tools: List[LangChainTool]):
        """
        Register multiple tools.

        Args:
            tools: List of tool instances to register
        """
        for tool in tools:
            self.register_tool(tool)

    def get_tool(self, name: str) -> Optional[LangChainTool]:
        """
        Get tool by name.

        Args:
            name: Tool name

        Returns:
            Tool instance or None if not found
        """
        return self._tools.get(name)

    def list_tools(self) -> List[str]:
        """
        List all registered tool names.

        Returns:
            List of tool names
        """
        return list(self._tools.keys())

    def to_langchain_tools(self) -> List[LangChainTool]:
        """Return the tools already registered in LangChain format."""
        return list(self._tools.values())

    def execute_tool(self, name: str, **kwargs) -> str:
        """
        Execute tool by name with given parameters.

        Args:
            name: Tool name
            **kwargs: Tool parameters

        Returns:
            Tool execution result as string

        Raises:
            ValueError: If tool not found
            Exception: On tool execution failure
        """
        tool = self.get_tool(name)
        if tool is None:
            raise ValueError(f"Tool '{name}' not found")

        try:
            result = tool.invoke(kwargs)
            return str(result)
        except Exception as e:
            raise Exception(f"Tool '{name}' execution failed: {str(e)}") from e

    def load_builtin_tools(self):
        """
        Load built-in example tools.

        Imports and registers standard tools like calculator, time, etc.
        Respects tool enable/disable configuration.
        """
        from pathlib import Path

        from .builtin_tools import get_builtin_tools

        repo_root = Path(__file__).resolve().parents[2]
        all_tools = get_builtin_tools(self.prolog_engine, repo_root=repo_root)

        if self.config:
            tool_config = self.config.get_tool_config()
            tools_to_register = [
                tool for tool in all_tools
                if tool_config.get(tool.name, True)
            ]
        else:
            tools_to_register = all_tools

        self.register_tools(tools_to_register)

    def load_user_tools(self, plugin_dir: str):
        """
        Load user-defined tools from plugin directory.

        Args:
            plugin_dir: Path to directory containing plugin files
        """
        from .loader import load_plugins
        tools = load_plugins(plugin_dir, self.prolog_engine)
        self.register_tools(tools)
