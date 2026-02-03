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
from .todo_tools import build_todo_tools
from ...noaa import build_noaa_weather_tool

# TODO Have LLM Save user prefs under "pref" tag
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
        description=(
            "A single valid Prolog goal as a string (no '?-'). "
            "Prefer module-qualified predicates. "
            "Default: run the main command pipeline first via "
            "'command_loop:handle_command(\"<user text>\")'. "
            "For lists, use findall/3 because the tool returns only one solution. "
            "Example: 'command_loop:handle_command(\"open firefox\")' to handle a user command such as open or "
            "'findall(V-I-A, kb_intents:verb_intent(V, I, A), L)' to list all intents when user asks you to list your skills."
        ),
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
            "Zara’s Prolog command router + KB.\n\n"

            "DEFAULT (TRY THIS FIRST):\n"
            "- Run the canonical Prolog command pipeline for the user’s text:\n"
            "  command_loop:handle_command(\"<user text>\")\n"
            "This is the real entry path used by main.pl (handle_input → command_loop).\n\n"

            "FALLBACK LOOKUPS (if command_loop fails / no match / you need structured info):\n"
            "- Resolve intent + args (no execution) if available:\n"
            "  intent_resolver:resolve(\"<user text>\", Intent, Args)\n"
            "- Verb → intent mappings:\n"
            "  kb_intents:verb_intent(Verb, Intent, Arity)\n"
            "- User overrides (config.kb):\n"
            "  kb_user_intents:verb_intent(Verb, Intent, Arity)\n"
            "- Config mappings:\n"
            "  kb_config:app_mapping(App, Cmd)\n"
            "  kb_config:direct_app(App)\n"
            "- TODO context inference:\n"
            "  kb_todo_context:infer_context([tokens...], Tag, Category, Confidence)\n\n"

            "LIST SKILLS / HELP:\n"
            "- If the user asks what Zara can do (skills/help/list commands), enumerate and summarize:\n"
            "  findall(V-I-A, kb_intents:verb_intent(V, I, A), Verbs)\n"
            "  findall(V-S-A, kb_intents:python_skill_intent(V, S, A), PySkills)\n"
            "  findall(App-Cmd, kb_config:app_mapping(App, Cmd), Apps)\n"
            "Then present categories like greet/media/open/search/dictation/timers/todo/config.\n\n"

            "IMPORTANT:\n"
            "- This tool returns ONE solution (query_once). For multiple results, ALWAYS use findall/3.\n"
            "- Side effects: command_loop:handle_command/1 and commands:execute/2 may launch apps or run shell commands. "
            "Only execute side effects when the user explicitly asked to perform the action.\n"
            "- Input must be a valid Prolog goal string; no '?-' prompt.\n"
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
    tools.extend(build_todo_tools())

    remember_tool = build_remember_tool(memory_manager)
    if remember_tool is not None:
        tools.append(remember_tool)

    recall_tool = build_recall_tool(memory_manager)
    if recall_tool is not None:
        tools.append(recall_tool)

    if prolog_engine is not None:
        tools.append(build_prolog_tool(prolog_engine))

    noaa_tool = build_noaa_weather_tool()
    if noaa_tool is not None:
        tools.append(noaa_tool)

    return tools
