"""
Built-in agent tools.

Provides minimal example tools for agent function calling:
- calculator: Evaluate mathematical expressions
- get_current_time: Get current time
- query_prolog: Query Prolog knowledge base (optional)
"""

import ast
import operator
from datetime import datetime
from typing import List, Optional
from .base import BaseTool


class CalculatorTool(BaseTool):
    """
    Safe mathematical expression evaluator.

    Evaluates basic math expressions without using eval().
    Supports: +, -, *, /, //, %, **
    """

    # Safe operators for AST evaluation
    OPERATORS = {
        ast.Add: operator.add,
        ast.Sub: operator.sub,
        ast.Mult: operator.mul,
        ast.Div: operator.truediv,
        ast.FloorDiv: operator.floordiv,
        ast.Mod: operator.mod,
        ast.Pow: operator.pow,
        ast.USub: operator.neg,
        ast.UAdd: operator.pos,
    }

    @property
    def name(self) -> str:
        return "calculator"

    @property
    def description(self) -> str:
        return (
            "Evaluate mathematical expressions safely. "
            "Supports: +, -, *, /, //, %, **"
        )

    @property
    def parameters(self):
        return {
            "expression": {
                "type": "string",
                "description": "Mathematical expression to evaluate (e.g. '2+2', '10*5', '2**8')"
            }
        }

    def execute(self, expression: str) -> str:
        """
        Evaluate mathematical expression safely.

        Args:
            expression: Math expression string

        Returns:
            Result as string
        """
        try:
            # Parse expression to AST
            tree = ast.parse(expression, mode='eval')
            result = self._eval_node(tree.body)
            return f"Result: {result}"
        except Exception as e:
            return f"Error: {str(e)}"

    def _eval_node(self, node):
        """Recursively evaluate AST node."""
        if isinstance(node, ast.Constant):
            return node.value
        elif isinstance(node, ast.BinOp):
            op = self.OPERATORS.get(type(node.op))
            if op is None:
                raise ValueError(f"Unsupported operator: {type(node.op).__name__}")
            left = self._eval_node(node.left)
            right = self._eval_node(node.right)
            return op(left, right)
        elif isinstance(node, ast.UnaryOp):
            op = self.OPERATORS.get(type(node.op))
            if op is None:
                raise ValueError(f"Unsupported operator: {type(node.op).__name__}")
            operand = self._eval_node(node.operand)
            return op(operand)
        else:
            raise ValueError(f"Unsupported expression type: {type(node).__name__}")


class CurrentTimeTool(BaseTool):
    """Get current date and time."""

    @property
    def name(self) -> str:
        return "get_current_time"

    @property
    def description(self) -> str:
        return "Get the current date and time. No parameters required."

    def execute(self) -> str:
        """
        Get current time.

        Returns:
            Current date/time as formatted string
        """
        now = datetime.now()
        return now.strftime("%Y-%m-%d %H:%M:%S")


class PrologBridgeTool(BaseTool):
    """
    Optional tool to let LLM query Prolog knowledge base.

    This bridges the gap between agent mode and Prolog commands.
    """

    def __init__(self, prolog_engine):
        """
        Initialize Prolog bridge tool.

        Args:
            prolog_engine: PrologEngine instance
        """
        if prolog_engine is None:
            raise ValueError("PrologBridgeTool requires prolog_engine")
        self.prolog_engine = prolog_engine

    @property
    def name(self) -> str:
        return "query_prolog"

    @property
    def description(self) -> str:
        return (
            "Query the Prolog knowledge base for commands and intents. "
            "Use this to execute commands or check if Prolog can handle something."
        )

    @property
    def parameters(self):
        return {
            "query": {
                "type": "string",
                "description": "Prolog query as string (e.g. 'command_loop:handle_command(\"open firefox\")')"
            }
        }

    def execute(self, query: str) -> str:
        """
        Execute Prolog query.

        Args:
            query: Prolog query string

        Returns:
            Query result as string
        """
        try:
            result = self.prolog_engine.query_once(query)
            if result:
                return f"Success: {result}"
            else:
                return "No results from Prolog query"
        except Exception as e:
            return f"Prolog query error: {str(e)}"


def get_builtin_tools(prolog_engine=None) -> List[BaseTool]:
    """
    Get all built-in tools.

    Args:
        prolog_engine: Optional PrologEngine instance (required for prolog_bridge)

    Returns:
        List of built-in tool instances
    """
    tools = [
        CalculatorTool(),
        CurrentTimeTool(),
    ]

    # Add Prolog bridge if engine provided
    if prolog_engine is not None:
        tools.append(PrologBridgeTool(prolog_engine))

    return tools
