"""
Built-in agent tools.

LangChain tool definitions used by the agent system.
"""

import ast
import operator
import os
from datetime import datetime
from pathlib import Path
from typing import List, Optional

from orgparse import load as org_load  # type: ignore[import-not-found]

from langchain_core.tools import StructuredTool, tool

from pydantic import BaseModel, Field

from .file_tools import build_file_tools
from ...noaa import build_noaa_weather_tool


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


class TodoListArgs(BaseModel):
    statuses: Optional[List[str]] = Field(
        default=None,
        description="Optional list of TODO states to include (e.g. TODO, NEXT, DONE)",
    )


class TodoAddArgs(BaseModel):
    text: str = Field(
        ..., description="Todo text to capture."
    )
    status: str = Field(
        "TODO",
        description="Todo keyword/state to use (e.g. TODO, NEXT, WAITING).",
    )


class TodoEditArgs(BaseModel):
    match: str = Field(
        ..., description="Text to match for selecting the todo to edit."
    )
    new_text: Optional[str] = Field(
        default=None,
        description="New todo text/heading. If omitted, keep existing text.",
    )
    new_status: Optional[str] = Field(
        default=None,
        description="New TODO keyword/state (e.g. DONE, NEXT).",
    )


class TodoConfigArgs(BaseModel):
    include_format: bool = Field(
        True,
        description="Whether to include the todo file format in the response.",
    )
    include_path: bool = Field(
        True,
        description="Whether to include the todo file path in the response.",
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


def _todo_config_path() -> Path:
    from ...config import get_config

    config = get_config()
    todo_config = config.get_section("todo")
    path = todo_config.get("path", "~/todo.org")
    return Path(os.path.expanduser(os.path.expandvars(str(path))))


def _todo_config_format() -> str:
    from ...config import get_config

    config = get_config()
    todo_config = config.get_section("todo")
    return str(todo_config.get("format", "org")).lower()


def _append_todo_entry(path: Path, text: str, status: str, format_name: str) -> str:
    if format_name == "markdown":
        entry = f"- [ ] {text}\n"
    else:
        entry = f"* {status} {text}\n"

    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as handle:
        handle.write(entry)
    return f"Todo captured to {path}."


def _normalize_status(status: Optional[str]) -> Optional[str]:
    if status is None:
        return None
    normalized = status.strip().upper()
    return normalized or None


def _update_org_todo(path: Path, match: str, new_text: Optional[str], new_status: Optional[str]) -> str:
    lines = path.read_text(encoding="utf-8").splitlines()
    target = match.strip().lower()
    updated_lines = []
    updated = False

    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("*") or updated:
            updated_lines.append(line)
            continue

        content = stripped.lstrip("*").strip()
        if not content:
            updated_lines.append(line)
            continue

        parts = content.split(maxsplit=1)
        if not parts:
            updated_lines.append(line)
            continue

        status = parts[0]
        heading = parts[1] if len(parts) > 1 else ""
        if target not in heading.lower():
            updated_lines.append(line)
            continue

        final_status = new_status or status
        final_heading = new_text if new_text is not None else heading
        updated_lines.append(f"* {final_status} {final_heading}")
        updated = True

    if not updated:
        return "No matching todo found."

    path.write_text("\n".join(updated_lines) + "\n", encoding="utf-8")
    return "Todo updated."


def _update_markdown_todo(path: Path, match: str, new_text: Optional[str], new_status: Optional[str]) -> str:
    lines = path.read_text(encoding="utf-8").splitlines()
    target = match.strip().lower()
    updated_lines = []
    updated = False

    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("- [") or updated:
            updated_lines.append(line)
            continue

        if "]" not in stripped:
            updated_lines.append(line)
            continue

        prefix, rest = stripped.split("]", 1)
        status_token = prefix.replace("- [", "").strip()
        if status_token.lower() == "x":
            status = "DONE"
        elif status_token == " ":
            status = "TODO"
        else:
            status = status_token.upper()

        text = rest.strip()
        if target not in text.lower():
            updated_lines.append(line)
            continue

        final_status = new_status or status
        final_text = new_text if new_text is not None else text

        checkbox = "x" if final_status == "DONE" else " "
        updated_lines.append(f"- [{checkbox}] {final_text}")
        updated = True

    if not updated:
        return "No matching todo found."

    path.write_text("\n".join(updated_lines) + "\n", encoding="utf-8")
    return "Todo updated."


def _list_org_todos(path: Path, statuses: Optional[List[str]]) -> str:
    if org_load is None:
        return "Org parser unavailable."
    root = org_load(str(path))
    nodes = list(root[1:])
    results = []
    for node in nodes:
        status = node.todo
        if not status:
            continue
        status = status.upper()
        if statuses and status not in statuses:
            continue
        heading = node.heading.strip() if node.heading else ""
        if not heading:
            continue
        indent = "  " * max(node.level - 1, 0)
        line = _format_org_line(indent, status, heading, node)
        results.append(line)
    if not results:
        return "No todos found."
    return "\n".join(results)


def _format_org_line(indent: str, status: str, heading: str, node) -> str:
    tags = _format_org_tags(getattr(node, "tags", None))
    priority = _format_org_priority(getattr(node, "priority", None))
    scheduled = _format_org_timestamp(getattr(node, "scheduled", None), "SCHEDULED")
    deadline = _format_org_timestamp(getattr(node, "deadline", None), "DEADLINE")
    return f"{indent}- [{status}] {heading}{tags}{priority}{scheduled}{deadline}"


def _format_org_tags(tags: Optional[List[str]]) -> str:
    if not tags:
        return ""
    tag_list = [tag for tag in tags if tag]
    if not tag_list:
        return ""
    return f" :{':'.join(tag_list)}:"


def _format_org_priority(priority: Optional[object]) -> str:
    if priority is None:
        return ""
    if isinstance(priority, int):
        if 65 <= priority <= 90:
            value = chr(priority)
        else:
            value = str(priority)
    else:
        value = str(priority).strip()
    if not value:
        return ""
    return f" [#{value}]"


def _format_org_timestamp(value: Optional[object], label: str) -> str:
    if not value:
        return ""
    text = str(value)
    if "<" not in text:
        text = f"<{text}>"
    return f" {label}: {text}"


def _list_markdown_todos(path: Path, statuses: Optional[List[str]]) -> str:
    lines = path.read_text(encoding="utf-8").splitlines()
    results = []
    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("-"):
            continue
        if stripped.startswith("- [x]") or stripped.startswith("- [X]"):
            status = "DONE"
        elif stripped.startswith("- [ ]"):
            status = "TODO"
        else:
            continue
        if statuses and status not in statuses:
            continue
        text = stripped.split("]", 1)[-1].strip()
        results.append(f"- [{status}] {text}")
    if not results:
        return "No todos found."
    return "\n".join(results)


def build_todo_list_tool() -> StructuredTool:
    @tool(
        "list_todos",
        args_schema=TodoListArgs,
        description=(
            "List todo items from the configured file with optional status filtering.\n\n"
            "ORG FORMAT OUTPUT (tree aware):\n"
            "- Uses indentation to reflect org heading depth.\n"
            "- Lines like: '  - [TODO] Title :tag1:tag2: [#A] SCHEDULED: <YYYY-MM-DD Day> DEADLINE: <YYYY-MM-DD Day>'\n"
            "- TODO keywords: TODO, NEXT, WAITING, SOMEDAY, DONE, CANCELED\n\n"
            "MARKDOWN FORMAT OUTPUT:\n"
            "- Lines like: '- [TODO] Title' or '- [DONE] Title'\n"
            "- Checkbox mapping: '[ ]' => TODO, '[x]' => DONE"
        ),
    )
    def list_todos(statuses: Optional[List[str]] = None) -> str:
        path = _todo_config_path()
        format_name = _todo_config_format()
        if format_name not in {"org", "markdown"}:
            return f"Unsupported todo format: {format_name}."
        if not path.exists():
            return f"Todo file not found: {path}."
        if format_name == "markdown":
            return _list_markdown_todos(path, statuses)
        return _list_org_todos(path, statuses)

    return list_todos


def build_todo_add_tool() -> StructuredTool:
    @tool(
        "add_todo",
        args_schema=TodoAddArgs,
        description=(
            "Create a new todo entry in the configured file.\n\n"
            "CRITICAL ORG-MODE FORMATTING RULES:\n"
            "1. ALWAYS call get_context() FIRST to know the current date/time, always account for existing task and work around it.\n"
            "2. Use proper org-mode TODO states: TODO, NEXT, WAITING, SOMEDAY, DONE, CANCELED\n"
            "   - TODO: Tasks not yet started\n"
            "   - NEXT: Tasks ready to work on immediately\n"
            "   - WAITING: Tasks blocked or waiting on something\n"
            "   - SOMEDAY: Tasks to do eventually\n"
            "   - DONE: Completed tasks\n"
            "   - CANCELED: Tasks that won't be done\n\n"
            "3. TITLE FORMATTING:\n"
            "   - Keep titles concise and descriptive\n"
            "   - NEVER include temporal information in titles (NO 'tonight', 'tomorrow', 'urgent', 'ASAP', etc.)\n"
            "   - NEVER include time information in titles (NO '7-2pm', '9pm', etc.)\n"
            "   - Good: 'Finish AI todo system', 'Work on Prolog', 'Review code'\n"
            "   - Bad: 'Finish AI todo system - TONIGHT', 'Work on Prolog URGENT', 'Review code ASAP'\n\n"
            "4. TAG USAGE:\n"
            "   - Use ONLY categorical/topic tags: 'work', 'personal', 'project-name', 'coding', 'meeting'\n"
            "   - NEVER use temporal tags: NO 'tonight', 'urgent', 'asap', 'tomorrow', 'today'\n"
            "   - NEVER use time-based tags: NO 'morning', 'evening', '7pm'\n"
            "   - Good tags: 'ai_development', 'prolog_programming', 'work_schedule'\n"
            "   - Bad tags: 'tonight', 'urgent', 'asap', '7-2pm' '-' 'work-schedule'\n\n"
            "5. DATETIME FORMATTING:\n"
            "   - ALWAYS include TIME when the user specifies or implies a specific time\n"
            "   - Format: YYYY-MM-DD HH:MM (24-hour time)\n"
            "   - SCHEDULED: When to START or be REMINDED\n"
            "   - DEADLINE: When it must be DONE BY\n"
            "   - Examples:\n"
            "     * 'tonight at 9pm' → scheduled: '2025-10-28 21:00'\n"
            "     * 'tomorrow morning at 7am' → scheduled: '2025-10-29 07:00'\n"
            "     * 'by Friday 5pm' → deadline: '2025-11-01 17:00'\n"
            "     * '7-2pm shift' → scheduled: '2025-10-30 07:00'\n\n"
            "6. PRIORITY USAGE:\n"
            "   - Use [#A], [#B], [#C] for urgency instead of words\n"
            "   - A = High priority / urgent\n"
            "   - B = Medium priority\n"
            "   - C = Low priority\n\n"
            "CORRECT EXAMPLES:\n"
            "User says: \"Add todo: Finish AI system tonight at 9pm, high priority, tag it ai and development\"\n"
            "You create:\n"
            "  title: 'Finish AI todo system'\n"
            "  tags: 'ai development'\n"
            "  scheduled: '2025-10-28 21:00'\n"
            "  priority: 'A'\n\n"
            "User says: \"Remind me to start work at 7am on Wednesday\"\n"
            "You create:\n"
            "  title: 'Start work'\n"
            "  tags: 'work'\n"
            "  scheduled: '2025-10-30 07:00'\n"
            "  priority: 'A'\n\n"
            "User says: \"I need to finish the report by Friday evening, it's urgent\"\n"
            "You create:\n"
            "  title: 'Finish report'\n"
            "  tags: 'work'\n"
            "  deadline: '2025-11-01 18:00'\n"
            "  priority: 'A'\n\n"
            "User says: \"I need to finish the programming task by Friday evening, it's urgent, i need to add module loading its not so urgent right now \"\n"
            "You create:\n"
            "  title: 'Finish Module loading'\n"
            "  tags: 'work' 'programming' 'module_loading'\n"
            "  deadline: '2025-11-01 18:00'\n"
            "  priority: 'B'\n\n"
            "INCORRECT EXAMPLES (DO NOT DO THIS):\n"
            "❌ title: 'Finish AI system - TONIGHT', tags: 'ai development tonight urgent'\n"
            "❌ title: 'Start work 7-2pm', tags: 'work morning 7am'\n"
            "❌ title: 'Report URGENT', scheduled: '2025-10-28' (missing time)\n\n"
            "Remember: ALL temporal information goes in scheduled/deadline fields WITH TIMES, NOT in titles or tags!\n\n"
            "MARKDOWN RULES:\n"
            "- Uses checkbox syntax: '- [ ] Title' or '- [x] Title'.\n"
            "- Keep titles concise; avoid temporal words in the title.\n"
            "- Example: '- [ ] Finish module loader'"
        ),
    )
    def add_todo(text: str, status: str = "TODO") -> str:
        if not text.strip():
            return "No todo text provided."
        format_name = _todo_config_format()
        if format_name not in {"org", "markdown"}:
            return f"Unsupported todo format: {format_name}."
        path = _todo_config_path()
        normalized_status = status.strip().upper() if status else "TODO"
        return _append_todo_entry(path, text.strip(), normalized_status, format_name)

    return add_todo


def build_todo_edit_tool() -> StructuredTool:
    @tool(
        "edit_todo",
        args_schema=TodoEditArgs,
        description=(
            "Edit the first matching todo entry by text.\n\n"
            "ORG FORMAT:\n"
            "- Matches headings like '* TODO Title'.\n"
            "- Updates TODO keyword and/or title.\n\n"
            "MARKDOWN FORMAT:\n"
            "- Matches lines like '- [ ] Title'.\n"
            "- Updates checkbox and/or title."
        ),
    )
    def edit_todo(match: str, new_text: Optional[str] = None, new_status: Optional[str] = None) -> str:
        if not match.strip():
            return "Match text is required."
        format_name = _todo_config_format()
        if format_name not in {"org", "markdown"}:
            return f"Unsupported todo format: {format_name}."
        path = _todo_config_path()
        if not path.exists():
            return f"Todo file not found: {path}."
        normalized_status = _normalize_status(new_status)
        if format_name == "markdown":
            return _update_markdown_todo(path, match, new_text, normalized_status)
        return _update_org_todo(path, match, new_text, normalized_status)

    return edit_todo


def build_todo_config_tool() -> StructuredTool:
    @tool(
        "todo_config",
        args_schema=TodoConfigArgs,
        description=(
            "Return the configured todo format (org or markdown) and file path."
        ),
    )
    def todo_config(include_format: bool = True, include_path: bool = True) -> str:
        format_name = _todo_config_format()
        path = _todo_config_path()
        parts = []
        if include_format:
            parts.append(f"Todo format: {format_name}")
        if include_path:
            parts.append(f"Todo path: {path}")
        return "\n".join(parts)

    return todo_config


def get_builtin_tools(
    prolog_engine=None,
    repo_root: Path | None = None,
    memory_manager=None,
) -> List[StructuredTool]:
    tools: List[StructuredTool] = [calculator, get_current_time]

    tools.extend(build_file_tools(repo_root))
    tools.append(build_todo_list_tool())
    tools.append(build_todo_add_tool())
    tools.append(build_todo_edit_tool())
    tools.append(build_todo_config_tool())

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
