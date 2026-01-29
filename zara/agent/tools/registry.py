"""
Tool registry for agent function calling.

Central registry that manages all available tools, converts them to
LangChain format, and executes them by name.
"""

from typing import Dict, List, Optional, Any, TYPE_CHECKING
from langchain_core.tools import StructuredTool, tool
from pydantic import BaseModel, Field, create_model
from .base import BaseTool

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
        self._tools: Dict[str, BaseTool] = {}

    def register_tool(self, tool: BaseTool):
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

    def register_tools(self, tools: List[BaseTool]):
        """
        Register multiple tools.

        Args:
            tools: List of tool instances to register
        """
        for tool in tools:
            self.register_tool(tool)

    def get_tool(self, name: str) -> Optional[BaseTool]:
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

    def to_langchain_tools(self) -> List[StructuredTool]:
        """
        Convert all registered tools to LangChain StructuredTool format.

        This format is used by LLMs for function calling.

        Returns:
            List of LangChain StructuredTools
        """
        langchain_tools = []

        for tool in self._tools.values():
            # Get tool parameters schema
            params = tool.parameters

            # Create Pydantic model for args_schema if parameters exist
            if params:
                # Convert JSON Schema-style parameters to Pydantic fields
                fields = {}
                for param_name, param_def in params.items():
                    param_type = str  # Default type
                    param_description = param_def.get("description", "")
                    param_default = param_def.get("default", ...)

                    # Map JSON Schema types to Python types
                    if param_def.get("type") == "integer":
                        param_type = int
                    elif param_def.get("type") == "number":
                        param_type = float
                    elif param_def.get("type") == "boolean":
                        param_type = bool

                    fields[param_name] = (
                        param_type,
                        Field(default=param_default, description=param_description)
                    )

                # Create dynamic Pydantic model
                args_schema = create_model(
                    f"{tool.name}Schema",
                    **fields
                )
            else:
                # No parameters - create empty schema
                args_schema = create_model(f"{tool.name}Schema")

            # Create wrapper function that LangChain can call
            # Important: Use default argument to capture tool by value, not reference
            def make_tool_func(t=tool):
                def tool_func(**kwargs):
                    return t.execute(**kwargs)
                return tool_func

            # Create LangChain StructuredTool with proper schema
            lc_tool = StructuredTool(
                name=tool.name,
                description=tool.description,
                func=make_tool_func(),
                args_schema=args_schema,
            )
            langchain_tools.append(lc_tool)

        return langchain_tools

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
            result = tool.execute(**kwargs)
            return str(result)
        except Exception as e:
            raise Exception(f"Tool '{name}' execution failed: {str(e)}") from e

    def load_builtin_tools(self):
        """
        Load built-in example tools.

        Imports and registers standard tools like calculator, time, etc.
        Respects tool enable/disable configuration.
        """
        from .builtin_tools import get_builtin_tools

        # Get all available built-in tools
        all_tools = get_builtin_tools(self.prolog_engine)

        # Filter based on config if available
        if self.config:
            tool_config = self.config.get_tool_config()
            tools_to_register = [
                tool for tool in all_tools
                if tool_config.get(tool.name, True)  # Default to enabled
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
