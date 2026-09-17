"""Python skills for human todo management."""

from __future__ import annotations

from datetime import datetime
from typing import Any, Iterable, List, Optional, Union

from .config import get_config
from .org_todos import ORG_STATUSES, OrgTodoStore, resolve_org_todo_root
from .todo_storage import (
    DEFAULT_DURATION_MINUTES,
    DEFAULT_STATUSES,
    TodoRecord,
    TodoStore,
)

TodoStoreLike = Union[TodoStore, OrgTodoStore]


def capture_todo(args: List[Any]) -> str:
    text = _join_args(args)
    if not text:
        return "No todo content provided."
    store = _todo_store()
    status = _default_status(store)
    todo_id = store.add_todo(title=text, status=status)
    return f"Todo captured (id {_display_id(todo_id)})."


def list_todos(args: List[Any]) -> str:
    store = _todo_store()
    statuses = _normalize_statuses(args, store)
    todos = store.list_todos(statuses=statuses)
    return _format_todos(todos, store)


def edit_todo(args: List[Any]) -> str:
    if len(args) < 2:
        return "Provide todo id and update text."
    store = _todo_store()
    todo_id = _parse_store_id(store, args[0])
    if todo_id is None:
        return "Invalid todo id."
    new_title = _join_args(args[1:])
    try:
        store.update_todo(todo_id, title=new_title)
    except ValueError:
        return "Todo not found."
    return "Todo updated."


def complete_todo(args: List[Any]) -> str:
    return _set_todo_status(args, "DONE", "Todo completed.")


def reopen_todo(args: List[Any]) -> str:
    return _set_todo_status(args, "TODO", "Todo reopened.")


def search_todos(args: List[Any]) -> str:
    query = _join_args(args)
    if not query:
        return "Search query required."
    store = _todo_store()
    todos = store.search_todos(query)
    return _format_todos(todos, store)


def schedule_todo(args: List[Any]) -> str:
    if len(args) < 2:
        return "Provide todo id and schedule time."
    store = _todo_store()
    todo_id = _parse_store_id(store, args[0])
    if todo_id is None:
        return "Invalid todo id."
    schedule_text = _join_args(args[1:])
    if not schedule_text:
        return "Schedule time required."
    schedule_iso = _parse_time(schedule_text)
    if schedule_iso is None:
        return "Could not parse schedule time."
    todo = store.get_todo(todo_id)
    if todo is None:
        return "Todo not found."
    default_duration = _default_duration_minutes()
    duration = todo.duration_minutes or default_duration
    if store.schedule_conflicts(
        schedule_iso,
        duration,
        exclude_todo_id=todo_id,
        default_duration_minutes=default_duration,
    ):
        return "Schedule conflict detected."
    try:
        store.update_todo(
            todo_id,
            scheduled_at=schedule_iso,
            duration_minutes=duration,
        )
    except ValueError:
        return "Todo not found."
    return "Todo scheduled."


def export_todos(args: List[Any]) -> str:
    format_name = _default_export_format(args)
    store = _todo_store()
    todos = store.list_todos(include_done=True)
    if format_name == "markdown":
        return _export_markdown(todos, store)
    return _export_org(todos, store)


def open_org_todos(args: List[Any]) -> str:
    store = _todo_store()
    if not isinstance(store, OrgTodoStore):
        return "Org todo backend is disabled. Set todo backend to org."
    root = store.root
    if args:
        todo_id = _parse_store_id(store, args[0])
        if todo_id is not None:
            todo = store.get_todo(todo_id)
            if todo is not None:
                return f"Org editor target: {root} · #{_display_id(todo.id)} {todo.title}"
    return f"Org editor target: {root}"


def todo_brief(args: List[Any]) -> str:
    store = _todo_store()
    todos = store.list_todos(include_done=False)
    if not todos:
        return "No active todos found."
    now = datetime.now().timestamp()
    rows = []
    engine = _load_todo_expert()
    for todo in todos:
        state, score = _expert_state_score(engine, todo, now)
        rows.append((score, state, todo))
    rows.sort(key=lambda row: (-row[0], row[2].title.casefold()))
    limit = _parse_limit(args, default=8)
    lines = []
    for score, state, todo in rows[:limit]:
        schedule = f" · {todo.scheduled_at}" if todo.scheduled_at else ""
        lines.append(
            f"- {state} · #{_display_id(todo.id)} · {todo.title}{schedule} · score {score:g}"
        )
    return "\n".join(lines)


def _todo_store() -> TodoStoreLike:
    config = get_config().get_section("todo")
    backend = str(config.get("backend", "org")).strip().lower()
    if backend == "sqlite":
        return TodoStore()
    root = resolve_org_todo_root(config)
    default_file = str(config.get("default_file", "inbox.org")).strip() or "inbox.org"
    return OrgTodoStore(root, default_file=default_file)


def _format_todos(todos: Iterable[TodoRecord], store: TodoStoreLike) -> str:
    lines = []
    for todo in todos:
        tags = store.tags_for(todo.id)
        tag_text = f" :{':'.join(tags)}:" if tags else ""
        schedule = f" SCHEDULED: <{todo.scheduled_at}>" if todo.scheduled_at else ""
        deadline = f" DEADLINE: <{todo.deadline_at}>" if todo.deadline_at else ""
        priority = f" [#{todo.priority}]" if todo.priority else ""
        lines.append(
            f"- [{todo.status}] #{_display_id(todo.id)} {todo.title}{tag_text}{priority}{schedule}{deadline}"
        )
    if not lines:
        return "No todos found."
    return "\n".join(lines)


def _export_org(todos: Iterable[TodoRecord], store: TodoStoreLike) -> str:
    lines = []
    for todo in todos:
        tags = store.tags_for(todo.id)
        tag_block = f" :{':'.join(tags)}:" if tags else ""
        priority = f" [#{todo.priority}]" if todo.priority else ""
        lines.append(f"* {todo.status}{priority} {todo.title}{tag_block}")
        lines.append(":PROPERTIES:")
        lines.append(f":ID: {todo.id}")
        if todo.duration_minutes:
            hours, minutes = divmod(todo.duration_minutes, 60)
            lines.append(f":EFFORT: {hours}:{minutes:02d}")
        lines.append(":END:")
        if todo.scheduled_at:
            lines.append(f"SCHEDULED: <{todo.scheduled_at}>")
        if todo.deadline_at:
            lines.append(f"DEADLINE: <{todo.deadline_at}>")
        if todo.notes:
            lines.append(todo.notes)
    return "\n".join(lines) if lines else "No todos found."


def _export_markdown(todos: Iterable[TodoRecord], store: TodoStoreLike) -> str:
    lines = []
    for todo in todos:
        checkbox = "x" if todo.status == "DONE" else " "
        lines.append(f"- [{checkbox}] {todo.title} (#{_display_id(todo.id)})")
    return "\n".join(lines) if lines else "No todos found."


def _join_args(args: Iterable[Any]) -> str:
    return " ".join(str(arg) for arg in args if arg is not None).strip()


def _default_status(store: TodoStoreLike) -> str:
    config = get_config()
    todo_config = config.get_section("todo")
    status = str(todo_config.get("default_status", "TODO")).upper()
    allowed = set(ORG_STATUSES) if isinstance(store, OrgTodoStore) else set(DEFAULT_STATUSES)
    return status if status in allowed else "TODO"


def _default_duration_minutes() -> int:
    value = get_config().get_section("todo").get(
        "default_duration_minutes",
        DEFAULT_DURATION_MINUTES,
    )
    if isinstance(value, bool):
        return DEFAULT_DURATION_MINUTES
    try:
        duration = int(value)
    except (TypeError, ValueError):
        return DEFAULT_DURATION_MINUTES
    return duration if duration > 0 else DEFAULT_DURATION_MINUTES


def _normalize_statuses(
    args: Iterable[Any],
    store: TodoStoreLike,
) -> Optional[List[str]]:
    items = list(args)
    if not items:
        return None
    if len(items) == 1 and isinstance(items[0], str) and "," in items[0]:
        items = [status.strip() for status in items[0].split(",") if status.strip()]
    statuses = [str(status).strip().upper() for status in items if str(status).strip()]
    allowed = set(ORG_STATUSES) if isinstance(store, OrgTodoStore) else set(DEFAULT_STATUSES)
    filtered = [status for status in statuses if status in allowed]
    return filtered or None


def _default_export_format(args: List[Any]) -> str:
    if args:
        value = str(args[0]).strip().lower()
        if value in {"org", "markdown"}:
            return value
    return "org"


def _parse_store_id(store: TodoStoreLike, value: Any) -> Optional[Union[int, str]]:
    try:
        cleaned = str(value).strip()
    except Exception:
        return None
    if not cleaned:
        return None
    cleaned = cleaned.lstrip("#")
    if cleaned.lower().startswith("id "):
        cleaned = cleaned[3:].strip()
    if isinstance(store, TodoStore):
        try:
            return int(cleaned)
        except ValueError:
            return None
    return cleaned


def _display_id(value: Any) -> str:
    text = str(value)
    return text[:8] if len(text) > 8 else text


def _parse_time(text: str) -> Optional[str]:
    for fmt in ("%Y-%m-%d %H:%M", "%Y-%m-%dT%H:%M", "%Y-%m-%dT%H:%M:%S"):
        try:
            parsed = datetime.strptime(text, fmt)
            return parsed.replace(microsecond=0).isoformat()
        except ValueError:
            continue
    return None


def _set_todo_status(args: List[Any], status: str, success: str) -> str:
    if not args:
        return "Provide todo id."
    store = _todo_store()
    todo_id = _parse_store_id(store, args[0])
    if todo_id is None:
        return "Invalid todo id."
    try:
        store.update_todo(todo_id, status=status)
    except ValueError:
        return "Todo not found."
    return success


def _load_todo_expert():
    try:
        from .prolog_engine import PrologEngine, locate_main_pl

        return PrologEngine(locate_main_pl())
    except Exception:
        return None


def _expert_state_score(engine, todo: TodoRecord, now: float) -> tuple[str, float]:
    if engine is None:
        return "unknown", 0.0
    status = _prolog_atom(todo.status.lower())
    priority = _prolog_atom(todo.priority.lower()) if todo.priority else "none"
    scheduled = _epoch_term(todo.scheduled_at)
    deadline = _epoch_term(todo.deadline_at)
    duration = todo.duration_minutes or _default_duration_minutes()
    goal = (
        f"todo_expert:todo_state({status},{scheduled},{duration},{deadline},{int(now)},State),"
        f"todo_expert:todo_score({status},{priority},{scheduled},{duration},{deadline},{int(now)},Score)"
    )
    try:
        result = engine.query_once(goal)
    except Exception:
        return "unknown", 0.0
    if not result:
        return "unknown", 0.0
    return str(result.get("State", "unknown")), float(result.get("Score", 0.0))


def _epoch_term(value: Optional[str]) -> str:
    if value is None:
        return "none"
    try:
        return str(int(datetime.fromisoformat(value).timestamp()))
    except ValueError:
        return "none"


def _prolog_atom(value: str) -> str:
    if value.replace("_", "").isalnum() and value[:1].islower():
        return value
    return "'" + value.replace("'", "''") + "'"


def _parse_limit(args: List[Any], default: int) -> int:
    if not args:
        return default
    try:
        value = int(str(args[0]).strip())
    except (TypeError, ValueError):
        return default
    return max(1, min(value, 25))
