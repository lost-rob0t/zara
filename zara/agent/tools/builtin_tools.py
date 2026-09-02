"""
Built-in agent tools.

LangChain tool definitions used by the agent system.
"""

import ast
import operator
from datetime import datetime
from typing import Any, Dict, List, Literal, Optional

from langchain_core.tools import StructuredTool, tool

from pydantic import BaseModel, Field

from .file_tools import build_file_tools
from .todo_tools import build_todo_tools
from ...noaa import build_noaa_weather_tool


TASK_GOAL_MAX_CHARS = 2000

TASK_TOOL_NAMES = (
    "task_create",
    "task_list",
    "task_status",
    "task_cancel",
    "task_resume",
)

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


class ForgetArgs(BaseModel):
    query: Optional[str] = Field(
        default=None,
        description="Text or subject identifying memories to delete.",
    )
    memory_id: Optional[str] = Field(
        default=None,
        description="Exact memory ID to delete.",
    )
    current_session: bool = Field(
        default=False,
        description="Delete memories and buffered messages from the current session.",
    )
    all_memories: bool = Field(
        default=False,
        description="Delete every stored memory and session buffer.",
    )
    confirm: bool = Field(
        default=False,
        description="Must be true when all_memories is true and the user explicitly requested it.",
    )


class MemoryListArgs(BaseModel):
    limit: int = Field(20, ge=1, le=50)
    kind: Optional[Literal["fact", "summary", "transcript"]] = None


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
            memory_id = entry.get("id", "") if isinstance(entry, dict) else ""
            identifier = f"(id: {memory_id}) " if memory_id else ""
            lines.append(f"- {identifier}{prefix}{text}")

        if not lines:
            return "No relevant memories found."

        return "\n".join(lines)

    return recall


def build_memory_list_tool(memory_manager) -> Optional[StructuredTool]:
    if memory_manager is None:
        return None

    @tool("memory_list", args_schema=MemoryListArgs)
    def memory_list(limit: int = 20, kind: Optional[str] = None) -> str:
        """List recent long-term memories so the user can inspect stored data."""
        kinds = [kind] if kind else None
        try:
            memories = memory_manager.list_memories(
                limit=int(limit),
                include_kinds=kinds,
            )
        except Exception as error:
            return f"Memory listing failed: {error}"
        if not memories:
            return "No memories stored."
        return "\n".join(
            f"- (id: {entry.get('id', '')}) "
            f"[{entry.get('metadata', {}).get('kind', '')}] {entry.get('text', '')}"
            for entry in memories
        )

    return memory_list


def build_forget_tool(memory_manager) -> Optional[StructuredTool]:
    if memory_manager is None:
        return None

    @tool("forget", args_schema=ForgetArgs)
    def forget(
        query: Optional[str] = None,
        memory_id: Optional[str] = None,
        current_session: bool = False,
        all_memories: bool = False,
        confirm: bool = False,
    ) -> str:
        """Permanently delete a targeted memory, current session, or all memories."""
        if all_memories and not confirm:
            return "Refusing to delete all memories without explicit confirmation."
        selectors = sum(
            bool(value)
            for value in (memory_id, (query or "").strip(), current_session, all_memories)
        )
        if selectors != 1:
            return "Choose exactly one target: query, memory ID, current session, or all memories."

        session_id = None
        if current_session:
            session_id = getattr(memory_manager, "current_session_id", None)
            if not session_id:
                return "There is no current memory session to forget."

        try:
            deleted = memory_manager.forget(
                memory_id=memory_id,
                query=query,
                session_id=session_id,
                all_memories=all_memories,
            )
        except Exception as error:
            return f"Memory deletion failed: {error}"
        if deleted == 0:
            if current_session:
                return "Current memory session cleared."
            if all_memories:
                return "All memory sessions cleared; no stored memories matched."
            return "No matching memories found."
        noun = "memory" if deleted == 1 else "memories"
        return f"Permanently deleted {deleted} {noun}."

    return forget


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
            "  kb_device_providers:app_mapping(App, Cmd)\n"
            "  kb_device_providers:direct_app(App)\n"
            "- TODO context inference:\n"
            "  kb_todo_context:infer_context([tokens...], Tag, Category, Confidence)\n\n"

            "LIST SKILLS / HELP:\n"
            "- If the user asks what Zara can do (skills/help/list commands), enumerate and summarize:\n"
            "  findall(V-I-A, kb_intents:verb_intent(V, I, A), Verbs)\n"
            "  findall(V-S-A, kb_intents:python_skill_intent(V, S, A), PySkills)\n"
            "  findall(App-Cmd, kb_device_providers:app_mapping(App, Cmd), Apps)\n"
            "Then present categories like greet/media/open/search/dictation/timers/todo/config.\n\n"

            "IMPORTANT:\n"
            "- This tool returns ONE solution (query_once). For multiple results, ALWAYS use findall/3.\n"
            "- Side effects: command_loop:handle_command/1 and commands:execute/2 may launch apps or run shell commands. "
            "Only execute side effects when the user explicitly asked to perform the action.\n"
            "- Input must be a valid Prolog goal string; no '?-' prompt.\n"
        ),
        args_schema=PrologQueryArgs,
    )



class TaskCreateArgs(BaseModel):
    goal: str = Field(
        ...,
        description=(
            "Complete, self-contained goal for a long-horizon task that may need "
            "multiple tool-using steps across separate turns."
        ),
    )
    max_steps: Optional[int] = Field(
        default=None,
        ge=1,
        le=1000,
        description="Optional per-task step budget override.",
    )


class TaskListArgs(BaseModel):
    status: Optional[str] = Field(
        default=None,
        description=(
            "Optional filter: pending, running, waiting_approval, completed, "
            "failed, cancelled, or interrupted."
        ),
    )


class TaskIdArgs(BaseModel):
    task_id: str = Field(
        ..., description="Long-horizon task identifier (e.g. 'task-1a2b3c4d5e6f')."
    )


def _short(text: str, limit: int = 120) -> str:
    clean = " ".join(str(text).split())
    if len(clean) <= limit:
        return clean
    return clean[: limit - 1] + "…"


def build_task_tools(task_service) -> List[StructuredTool]:
    """Build the long-horizon task tools for one bound task service."""

    @tool("task_create", args_schema=TaskCreateArgs)
    async def task_create(goal: str, max_steps: Optional[int] = None) -> str:
        """Start a persistent multi-step task that runs in the background."""

        if len(goal.strip()) > TASK_GOAL_MAX_CHARS:
            return f"Invalid goal: goals are limited to {TASK_GOAL_MAX_CHARS} characters."
        try:
            created = await task_service.create_task(
                goal=goal, max_task_steps=max_steps
            )
        except Exception as error:
            return f"Could not create task: {error}"
        return (
            f"Started task {created.task_id} (status: {created.status.value}, "
            f"steps allowed: {created.max_task_steps}). "
            "Use task_status to follow progress."
        )

    @tool("task_list", args_schema=TaskListArgs)
    async def task_list(status: Optional[str] = None) -> str:
        """List long-horizon tasks and their current state."""

        statuses = [status] if status else None
        try:
            rows = task_service.list_tasks(statuses=statuses)
        except Exception as error:
            return f"Could not list tasks: {error}"
        if not rows:
            return "No long-horizon tasks found."
        lines = [
            f"- {row.task_id} [{row.status.value}] steps {row.steps_completed}/{row.max_task_steps}: "
            f"{_short(row.goal, 80)}"
            for row in rows
        ]
        return "Long-horizon tasks:\n" + "\n".join(lines)

    @tool("task_status", args_schema=TaskIdArgs)
    async def task_status(task_id: str) -> str:
        """Report the state, progress, and last step summary of one task."""

        row = task_service.get_task(task_id)
        if row is None:
            return f"No task found for {task_id}."
        lines = [
            f"Task {row.task_id}: {row.status.value}",
            f"Goal: {_short(row.goal, 160)}",
            f"Steps completed: {row.steps_completed}/{row.max_task_steps}",
        ]
        if row.reason:
            lines.append(f"Reason: {row.reason}")
        return "\n".join(lines)

    @tool("task_cancel", args_schema=TaskIdArgs)
    async def task_cancel(task_id: str) -> str:
        """Cancel a running long-horizon task. Cancelled tasks never resume."""

        try:
            cancelled = await task_service.cancel_task(task_id=task_id)
        except Exception as error:
            return f"Could not cancel task: {error}"
        return f"Task {cancelled.task_id} cancelled."

    @tool("task_resume", args_schema=TaskIdArgs)
    async def task_resume(task_id: str) -> str:
        """Resume an interrupted or pending long-horizon task."""

        try:
            resumed = await task_service.resume_task(task_id=task_id)
        except Exception as error:
            return f"Could not resume task: {error}"
        return f"Task {resumed.task_id} resumed (status: {resumed.status.value})."

    return [task_create, task_list, task_status, task_cancel, task_resume]


def get_builtin_tools(
    prolog_engine=None,
    memory_manager=None,
    file_tool_config: Optional[Dict[str, Any]] = None,
    task_service=None,
) -> List[StructuredTool]:
    tools: List[StructuredTool] = [calculator, get_current_time]

    if file_tool_config is not None:
        tools.extend(build_file_tools(**file_tool_config))
    tools.extend(build_todo_tools())

    if task_service is not None:
        tools.extend(build_task_tools(task_service))

    remember_tool = build_remember_tool(memory_manager)
    if remember_tool is not None:
        tools.append(remember_tool)

    recall_tool = build_recall_tool(memory_manager)
    if recall_tool is not None:
        tools.append(recall_tool)

    memory_list_tool = build_memory_list_tool(memory_manager)
    if memory_list_tool is not None:
        tools.append(memory_list_tool)

    forget_tool = build_forget_tool(memory_manager)
    if forget_tool is not None:
        tools.append(forget_tool)

    if prolog_engine is not None:
        tools.append(build_prolog_tool(prolog_engine))

    noaa_tool = build_noaa_weather_tool()
    if noaa_tool is not None:
        tools.append(noaa_tool)

    return tools
