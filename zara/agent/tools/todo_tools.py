"""
Agent tools for todo management.
"""

from __future__ import annotations

from typing import List, Optional

from langchain_core.tools import StructuredTool, tool
from pydantic import BaseModel, Field

from ...todo_skills import (
    capture_todo,
    edit_todo,
    export_todos,
    list_todos,
    schedule_todo,
    search_todos,
)


class TodoListArgs(BaseModel):
    statuses: Optional[List[str]] = Field(
        default=None,
        description="Optional list of TODO states to include (TODO, NEXT, WAITING).",
    )


class TodoAddArgs(BaseModel):
    text: str = Field(..., description="Todo text to capture.")


class TodoEditArgs(BaseModel):
    todo_id: int = Field(..., description="Todo id to update.")
    new_text: str = Field(..., description="New todo title.")


class TodoSearchArgs(BaseModel):
    query: str = Field(..., description="Text to search in todos.")


class TodoScheduleArgs(BaseModel):
    todo_id: int = Field(..., description="Todo id to schedule.")
    schedule: str = Field(..., description="Schedule time (YYYY-MM-DD HH:MM).")


class TodoExportArgs(BaseModel):
    format: str = Field("org", description="Export format: org or markdown.")


@tool("list_todos", args_schema=TodoListArgs)
def list_todos_tool(statuses: Optional[List[str]] = None) -> str:
    """List todos with optional status filtering."""
    return list_todos(statuses or [])


@tool("add_todo", args_schema=TodoAddArgs)
def add_todo_tool(text: str) -> str:
    """Capture a new todo item."""
    return capture_todo([text])


@tool("edit_todo", args_schema=TodoEditArgs)
def edit_todo_tool(todo_id: int, new_text: str) -> str:
    """Update an existing todo title by id."""
    return edit_todo([str(todo_id), new_text])


@tool("search_todos", args_schema=TodoSearchArgs)
def search_todos_tool(query: str) -> str:
    """Search todos by text."""
    return search_todos([query])


@tool("schedule_todo", args_schema=TodoScheduleArgs)
def schedule_todo_tool(todo_id: int, schedule: str) -> str:
    """Assign a schedule to a todo."""
    return schedule_todo([str(todo_id), schedule])


@tool("export_todos", args_schema=TodoExportArgs)
def export_todos_tool(format: str = "org") -> str:
    """Export todos in the requested format."""
    return export_todos([format])


def build_todo_tools() -> list[StructuredTool]:
    return [
        list_todos_tool,
        add_todo_tool,
        edit_todo_tool,
        search_todos_tool,
        schedule_todo_tool,
        export_todos_tool,
    ]
