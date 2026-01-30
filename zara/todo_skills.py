"""
Python skills for todo management.
"""

from __future__ import annotations

import logging
from datetime import datetime
from typing import Any, Iterable, List, Optional

from .config import get_config
from .prolog_engine import PrologEngine
from .todo_storage import DEFAULT_STATUSES, TodoRecord, TodoStore

logger = logging.getLogger(__name__)


def capture_todo(args: List[Any]) -> str:
    text = _join_args(args)
    if not text:
        return "No todo content provided."
    store = TodoStore()
    status = _default_status()
    todo_id = store.add_todo(title=text, status=status)
    return f"Todo captured (id {todo_id})."


def list_todos(args: List[Any]) -> str:
    statuses = _normalize_statuses(args)
    store = TodoStore()
    todos = store.list_todos(statuses=statuses)
    return _format_todos(todos, store)


def edit_todo(args: List[Any]) -> str:
    if len(args) < 2:
        return "Provide todo id and update text."
    todo_id = _parse_int(args[0])
    if todo_id is None:
        return "Invalid todo id."
    new_title = _join_args(args[1:])
    store = TodoStore()
    try:
        store.update_todo(todo_id, title=new_title)
    except ValueError:
        return "Todo not found."
    return "Todo updated."


def search_todos(args: List[Any]) -> str:
    query = _join_args(args)
    if not query:
        return "Search query required."
    store = TodoStore()
    todos = store.search_todos(query)
    return _format_todos(todos, store)


def schedule_todo(args: List[Any], prolog: Optional[PrologEngine] = None) -> str:
    if len(args) < 2:
        return "Provide todo id and schedule time."
    todo_id = _parse_int(args[0])
    if todo_id is None:
        return "Invalid todo id."
    schedule_text = _join_args(args[1:])
    if not schedule_text:
        return "Schedule time required."
    schedule_iso = _parse_time(schedule_text)
    if schedule_iso is None:
        return "Could not parse schedule time."
    store = TodoStore()
    existing = [occurrence for _, occurrence in store.list_occurrences()]
    if schedule_iso in existing:
        return "Schedule conflict detected."
    if prolog is not None:
        overlap = _check_overlap(prolog, schedule_iso, existing)
        if overlap:
            return "Schedule conflict detected."
    try:
        store.update_todo(todo_id, scheduled_at=schedule_iso)
    except ValueError:
        return "Todo not found."
    return "Todo scheduled."


def export_todos(args: List[Any]) -> str:
    format_name = _default_export_format(args)
    store = TodoStore()
    todos = store.list_todos(include_done=True)
    if format_name == "markdown":
        return _export_markdown(todos, store)
    return _export_org(todos, store)


def _format_todos(todos: Iterable[TodoRecord], store: TodoStore) -> str:
    lines = []
    for todo in todos:
        tags = store.tags_for(todo.id)
        tag_text = f" :{':'.join(tags)}:" if tags else ""
        schedule = f" SCHEDULED: <{todo.scheduled_at}>" if todo.scheduled_at else ""
        deadline = f" DEADLINE: <{todo.deadline_at}>" if todo.deadline_at else ""
        priority = f" [#{todo.priority}]" if todo.priority else ""
        lines.append(
            f"- [{todo.status}] #{todo.id} {todo.title}{tag_text}{priority}{schedule}{deadline}"
        )
    if not lines:
        return "No todos found."
    return "\n".join(lines)


def _export_org(todos: Iterable[TodoRecord], store: TodoStore) -> str:
    lines = []
    for todo in todos:
        tags = store.tags_for(todo.id)
        tag_block = f" :{':'.join(tags)}:" if tags else ""
        priority = f" [#{todo.priority}]" if todo.priority else ""
        lines.append(f"* {todo.status} {todo.title}{priority}{tag_block}")
        if todo.scheduled_at:
            lines.append(f"  SCHEDULED: <{todo.scheduled_at}>")
        if todo.deadline_at:
            lines.append(f"  DEADLINE: <{todo.deadline_at}>")
    return "\n".join(lines) if lines else "No todos found."


def _export_markdown(todos: Iterable[TodoRecord], store: TodoStore) -> str:
    lines = []
    for todo in todos:
        checkbox = "x" if todo.status == "DONE" else " "
        lines.append(f"- [{checkbox}] {todo.title} (#{todo.id})")
    return "\n".join(lines) if lines else "No todos found."


def _join_args(args: Iterable[Any]) -> str:
    return " ".join(str(arg) for arg in args if arg is not None).strip()


def _default_status() -> str:
    config = get_config()
    todo_config = config.get_section("todo")
    status = str(todo_config.get("default_status", "TODO")).upper()
    return status if status in DEFAULT_STATUSES else "TODO"


def _normalize_statuses(args: Iterable[Any]) -> Optional[List[str]]:
    items = list(args)
    if not items:
        return None
    if len(items) == 1 and isinstance(items[0], str) and "," in items[0]:
        items = [status.strip() for status in items[0].split(",") if status.strip()]
    statuses = [str(status).strip().upper() for status in items if str(status).strip()]
    filtered = [status for status in statuses if status in DEFAULT_STATUSES]
    return filtered or None


def _default_export_format(args: List[Any]) -> str:
    if args:
        value = str(args[0]).strip().lower()
        if value in {"org", "markdown"}:
            return value
    return "org"


def _parse_int(value: Any) -> Optional[int]:
    try:
        cleaned = str(value).strip()
    except Exception:
        return None
    if not cleaned:
        return None
    cleaned = cleaned.lstrip("#")
    if cleaned.lower().startswith("id "):
        cleaned = cleaned[3:].strip()
    try:
        return int(cleaned)
    except (TypeError, ValueError):
        return None


def _parse_time(text: str) -> Optional[str]:
    for fmt in ("%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M"):
        try:
            parsed = datetime.strptime(text, fmt)
            return parsed.isoformat()
        except ValueError:
            continue
    return None


def _check_overlap(
    prolog: PrologEngine,
    schedule_iso: str,
    existing_times: Iterable[str],
) -> bool:
    existing_atoms = [f"'{time}'" for time in existing_times if time]
    schedule_atom = f"'{schedule_iso}'"
    query = f"todo_schedule:no_overlap({schedule_atom}, [{', '.join(existing_atoms)}])"
    result = prolog.query_once(query)
    return result is None
