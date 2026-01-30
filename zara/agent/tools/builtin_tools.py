"""
Built-in agent tools.

LangChain tool definitions used by the agent system.
"""

import ast
import operator
from datetime import datetime
from pathlib import Path
from typing import List, Optional

from langchain_core.tools import StructuredTool, tool

from pydantic import BaseModel, Field

from .file_tools import build_file_tools


class RememberArgs(BaseModel):
    text: str = Field(
        ..., description="Fact to store in long-term memory."
    )
    tags: Optional[List[str]] = Field(
        default=None,
        description="Optional tags for categorizing this memory.",
    )


class RecallArgs(BaseModel):
    query: str = Field(
        ..., description="Search query to retrieve relevant memories."
    )
    k: int = Field(
        5,
        description="How many memories to return.",
        ge=1,
        le=20,
    )


class CalculatorArgs(BaseModel):
    expression: str = Field(
        ..., description="Mathematical expression to evaluate (e.g. '2+2', '10*5', '2**8')"
    )


@tool("calculator")
def calculator(expression: str) -> str:
    """
    Evaluate mathematical expressions safely.

    Supports: +, -, *, /, //, %, **
    """
    operators = {
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

    def eval_node(node):
        if isinstance(node, ast.Constant):
            return node.value
        if isinstance(node, ast.BinOp):
            op = operators.get(type(node.op))
            if op is None:
                raise ValueError(f"Unsupported operator: {type(node.op).__name__}")
            return op(eval_node(node.left), eval_node(node.right))
        if isinstance(node, ast.UnaryOp):
            op = operators.get(type(node.op))
            if op is None:
                raise ValueError(f"Unsupported operator: {type(node.op).__name__}")
            return op(eval_node(node.operand))
        raise ValueError(f"Unsupported expression type: {type(node).__name__}")

    try:
        tree = ast.parse(expression, mode="eval")
        result = eval_node(tree.body)
        return f"Result: {result}"
    except Exception as e:
        return f"Error: {str(e)}"


@tool("get_current_time")
def get_current_time() -> str:
    """Get the current date and time."""
    now = datetime.now()
    return now.strftime("%Y-%m-%d %H:%M:%S")


class PrologQueryArgs(BaseModel):
    query: str = Field(
        ...,
        description="Prolog query as string (e.g. 'command_loop:handle_command(\"open firefox\")')",
    )


def build_remember_tool(memory_manager) -> Optional[StructuredTool]:
    if memory_manager is None:
        return None

    @tool("remember", args_schema=RememberArgs)
    def remember(text: str, tags: Optional[List[str]] = None) -> str:
        """Store a specific fact in long-term memory."""
        memory_id = memory_manager.remember_fact(
            text=text,
            tags=tags,
            session_id=getattr(memory_manager, "current_session_id", None),
            source="agent",
        )
        if memory_id:
            return "Stored memory."
        return "No memory stored."

    return remember


def build_recall_tool(memory_manager) -> Optional[StructuredTool]:
    if memory_manager is None:
        return None

    @tool("recall", args_schema=RecallArgs)
    def recall(query: str, k: int = 5) -> str:
        """Retrieve relevant long-term memories."""
        memories = memory_manager.retrieve(query, k=int(k))
        if not memories:
            return "No relevant memories found."

        lines = []
        for entry in memories:
            text = entry.get("text") if isinstance(entry, dict) else str(entry)
            if not text:
                continue
            metadata = entry.get("metadata") if isinstance(entry, dict) else None
            kind = ""
            if isinstance(metadata, dict):
                kind = metadata.get("kind", "")
            prefix = f"[{kind}] " if kind else ""
            lines.append(f"- {prefix}{text}")

        if not lines:
            return "No relevant memories found."

        return "\n".join(lines)

    return recall


def build_prolog_tool(prolog_engine) -> StructuredTool:
    def query_prolog(query: str) -> str:
        try:
            result = prolog_engine.query_once(query)
            if result:
                return f"Success: {result}"
            return "No results from Prolog query"
        except Exception as e:
            return f"Prolog query error: {str(e)}"

    return StructuredTool.from_function(
        func=query_prolog,
        name="query_prolog",
        description=(
            "Query the Prolog knowledge base for commands and intents. "
            "Use this to execute commands or check if Prolog can handle something."
        ),
        args_schema=PrologQueryArgs,
    )


def get_builtin_tools(
    prolog_engine=None,
    repo_root: Path | None = None,
    memory_manager=None,
) -> List[StructuredTool]:
    tools: List[StructuredTool] = [calculator, get_current_time]

    tools.extend(build_file_tools(repo_root))

    remember_tool = build_remember_tool(memory_manager)
    if remember_tool is not None:
        tools.append(remember_tool)

    recall_tool = build_recall_tool(memory_manager)
    if recall_tool is not None:
        tools.append(recall_tool)

    if prolog_engine is not None:
        tools.append(build_prolog_tool(prolog_engine))

    return tools
