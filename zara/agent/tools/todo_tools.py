"""Agent tools for todo management."""

from __future__ import annotations

from typing import List, Optional

from langchain_core.tools import StructuredTool, tool
from pydantic import BaseModel, Field

from ...todo_skills import (
    capture_todo,
    complete_todo,
    edit_todo,
    export_todos,
    list_todos,
    open_org_todos,
    reopen_todo,
    schedule_todo,
    search_todos,
    todo_brief,
)


class TodoListArgs(BaseModel):
    statuses: Optional[List[str]] = Field(
        default=None,
        description="Optional Org TODO states to include, such as TODO, STRT, WAIT, LOOP, DONE.",
    )


class TodoAddArgs(BaseModel):
    text: str = Field(..., description="Todo text to capture in the canonical Org store.")


class TodoEditArgs(BaseModel):
    todo_id: str = Field(..., description="Stable Org ID or unique ID prefix to update.")
    new_text: str = Field(..., description="New todo title.")


class TodoIdArgs(BaseModel):
    todo_id: str = Field(..., description="Stable Org ID or unique ID prefix to update.")


class TodoSearchArgs(BaseModel):
    query: str = Field(..., description="Text to search in todos.")


class TodoScheduleArgs(BaseModel):
    todo_id: str = Field(..., description="Stable Org ID or unique ID prefix to schedule.")
    schedule: str = Field(..., description="SCHEDULED placement (YYYY-MM-DD HH:MM).")


class TodoExportArgs(BaseModel):
    format: str = Field("org", description="Export format: org or markdown.")


class TodoBriefArgs(BaseModel):
    limit: int = Field(8, ge=1, le=25, description="Maximum expert-ranked todos to return.")


class OrgEditorArgs(BaseModel):
    todo_id: Optional[str] = Field(
        default=None,
        description="Optional stable Org ID or unique prefix to target.",
    )


@tool("list_todos", args_schema=TodoListArgs)
def list_todos_tool(statuses: Optional[List[str]] = None) -> str:
    """List todos from Zara's canonical Org todo store."""
    return list_todos(statuses or [])


@tool("add_todo", args_schema=TodoAddArgs)
def add_todo_tool(text: str) -> str:
    """Capture a new canonical Org todo item."""
    return capture_todo([text])


@tool("edit_todo", args_schema=TodoEditArgs)
def edit_todo_tool(todo_id: str, new_text: str) -> str:
    """Update an existing todo title by stable Org id or prefix."""
    return edit_todo([todo_id, new_text])


@tool("complete_todo", args_schema=TodoIdArgs)
def complete_todo_tool(todo_id: str) -> str:
    """Mark an existing todo complete."""
    return complete_todo([todo_id])


@tool("reopen_todo", args_schema=TodoIdArgs)
def reopen_todo_tool(todo_id: str) -> str:
    """Reopen a completed todo."""
    return reopen_todo([todo_id])


@tool("search_todos", args_schema=TodoSearchArgs)
def search_todos_tool(query: str) -> str:
    """Search canonical Org todos by text."""
    return search_todos([query])


@tool("schedule_todo", args_schema=TodoScheduleArgs)
def schedule_todo_tool(todo_id: str, schedule: str) -> str:
    """Assign Org SCHEDULED placement to a todo without changing its DEADLINE."""
    return schedule_todo([todo_id, schedule])


@tool("export_todos", args_schema=TodoExportArgs)
def export_todos_tool(format: str = "org") -> str:
    """Export todos in the requested format."""
    return export_todos([format])


@tool("todo_brief", args_schema=TodoBriefArgs)
def todo_brief_tool(limit: int = 8) -> str:
    """Rank active todos through Zara's deterministic Prolog todo expert."""
    return todo_brief([str(limit)])


@tool("open_org_todos", args_schema=OrgEditorArgs)
def open_org_todos_tool(todo_id: Optional[str] = None) -> str:
    """Return the canonical Org editor target, optionally for one task id."""
    return open_org_todos([todo_id] if todo_id else [])


def build_todo_tools() -> list[StructuredTool]:
    return [
        list_todos_tool,
        add_todo_tool,
        edit_todo_tool,
        complete_todo_tool,
        reopen_todo_tool,
        search_todos_tool,
        schedule_todo_tool,
        export_todos_tool,
        todo_brief_tool,
        open_org_todos_tool,
    ]
