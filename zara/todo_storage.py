"""Todo storage and scheduling logic."""

from __future__ import annotations

import calendar
import re
import sqlite3
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from enum import Enum
from typing import Iterable, Optional, Sequence, TypeVar, Union

from .database import DatabaseManager, get_database


DEFAULT_STATUSES = ["TODO", "NEXT", "WAITING", "SOMEDAY", "DONE", "CANCELED"]
DEFAULT_PRIORITIES = ["A", "B", "C"]
DEFAULT_DURATION_MINUTES = 30
RECURRENCE_SYNTAX = "+Nmin, +Nh, +Nd, +Nw, +Nm, or +Ny"
_REPEATER_PATTERN = re.compile(r"^\+([1-9][0-9]*)(min|h|d|w|m|y)$")


class PatchMode(Enum):
    PRESERVE = "preserve"
    CLEAR = "clear"


PRESERVE = PatchMode.PRESERVE
CLEAR = PatchMode.CLEAR
_PatchValue = TypeVar("_PatchValue")
Patch = Union[_PatchValue, PatchMode]


@dataclass(frozen=True)
class TodoRecord:
    id: int
    title: str
    status: str
    priority: Optional[str]
    scheduled_at: Optional[str]
    deadline_at: Optional[str]
    repeater: Optional[str]
    duration_minutes: Optional[int]
    created_at: str
    updated_at: Optional[str]
    completed_at: Optional[str]
    notes: Optional[str]


@dataclass(frozen=True)
class ScheduledInterval:
    todo_id: int
    start: datetime
    end: datetime


class TodoStore:
    def __init__(self, db: Optional[DatabaseManager] = None) -> None:
        self._db = db or get_database()
        self._ensure_schema()

    def _ensure_schema(self) -> None:
        try:
            self._db.register_migration(
                1,
                [
                    """
                    CREATE TABLE IF NOT EXISTS todos (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        title TEXT NOT NULL,
                        status TEXT NOT NULL,
                        priority TEXT,
                        scheduled_at TEXT,
                        deadline_at TEXT,
                        repeater TEXT,
                        duration_minutes INTEGER,
                        created_at TEXT NOT NULL,
                        updated_at TEXT,
                        completed_at TEXT,
                        notes TEXT
                    )
                    """,
                    """
                    CREATE TABLE IF NOT EXISTS todo_tags (
                        todo_id INTEGER NOT NULL,
                        tag TEXT NOT NULL,
                        FOREIGN KEY(todo_id) REFERENCES todos(id) ON DELETE CASCADE
                    )
                    """,
                    """
                    CREATE TABLE IF NOT EXISTS todo_occurrences (
                        todo_id INTEGER NOT NULL,
                        occurrence_at TEXT NOT NULL,
                        FOREIGN KEY(todo_id) REFERENCES todos(id) ON DELETE CASCADE
                    )
                    """,
                    """
                    CREATE TABLE IF NOT EXISTS todo_reminders (
                        todo_id INTEGER NOT NULL,
                        remind_at TEXT NOT NULL,
                        channel TEXT NOT NULL,
                        sent_at TEXT,
                        FOREIGN KEY(todo_id) REFERENCES todos(id) ON DELETE CASCADE
                    )
                    """,
                    "CREATE INDEX IF NOT EXISTS idx_todos_status ON todos(status)",
                    "CREATE INDEX IF NOT EXISTS idx_todos_scheduled ON todos(scheduled_at)",
                    "CREATE INDEX IF NOT EXISTS idx_todo_tags_tag ON todo_tags(tag)",
                ],
            )
        except ValueError:
            pass
        self._db.connect()

    def add_todo(
        self,
        title: str,
        status: str,
        priority: Optional[str] = None,
        scheduled_at: Optional[str] = None,
        deadline_at: Optional[str] = None,
        repeater: Optional[str] = None,
        duration_minutes: Optional[int] = None,
        tags: Optional[Iterable[str]] = None,
        notes: Optional[str] = None,
    ) -> int:
        values = _validate_todo_values(
            title=title,
            status=status,
            priority=priority,
            scheduled_at=scheduled_at,
            deadline_at=deadline_at,
            repeater=repeater,
            duration_minutes=duration_minutes,
            notes=notes,
        )
        clean_tags = _clean_tags(tags or ())
        occurrences = _occurrences(values["scheduled_at"], values["repeater"])
        now = _now_iso()
        with self._db.transaction() as conn:
            cursor = conn.execute(
                """
                INSERT INTO todos (
                    title, status, priority, scheduled_at, deadline_at, repeater,
                    duration_minutes, created_at, updated_at, completed_at, notes
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    values["title"],
                    values["status"],
                    values["priority"],
                    values["scheduled_at"],
                    values["deadline_at"],
                    values["repeater"],
                    values["duration_minutes"],
                    now,
                    now,
                    now if values["status"] == "DONE" else None,
                    values["notes"],
                ),
            )
            todo_id = int(cursor.lastrowid or 0)
            self._replace_tags(conn, todo_id, clean_tags)
            self._replace_occurrences(conn, todo_id, occurrences)
        return todo_id

    def update_todo(
        self,
        todo_id: int,
        title: Patch[str] = PRESERVE,
        status: Patch[str] = PRESERVE,
        priority: Patch[Optional[str]] = PRESERVE,
        scheduled_at: Patch[Optional[str]] = PRESERVE,
        deadline_at: Patch[Optional[str]] = PRESERVE,
        repeater: Patch[Optional[str]] = PRESERVE,
        duration_minutes: Patch[Optional[int]] = PRESERVE,
        tags: Patch[Iterable[str]] = PRESERVE,
        notes: Patch[Optional[str]] = PRESERVE,
    ) -> None:
        row = self.get_todo(todo_id)
        if row is None:
            raise ValueError("Todo not found")
        values = _validate_todo_values(
            title=_required_patch("title", title, row.title),
            status=_required_patch("status", status, row.status),
            priority=_optional_patch(priority, row.priority),
            scheduled_at=_optional_patch(scheduled_at, row.scheduled_at),
            deadline_at=_optional_patch(deadline_at, row.deadline_at),
            repeater=_optional_patch(repeater, row.repeater),
            duration_minutes=_optional_patch(duration_minutes, row.duration_minutes),
            notes=_optional_patch(notes, row.notes),
        )
        clean_tags = None if tags is PRESERVE else _clean_tags(() if tags is CLEAR else tags)
        occurrences = _occurrences(values["scheduled_at"], values["repeater"])
        completed_at = row.completed_at
        if values["status"] == "DONE" and completed_at is None:
            completed_at = _now_iso()
        elif values["status"] != "DONE":
            completed_at = None
        with self._db.transaction() as conn:
            cursor = conn.execute(
                """
                UPDATE todos SET
                    title = ?, status = ?, priority = ?, scheduled_at = ?,
                    deadline_at = ?, repeater = ?, duration_minutes = ?,
                    updated_at = ?, completed_at = ?, notes = ?
                WHERE id = ?
                """,
                (
                    values["title"],
                    values["status"],
                    values["priority"],
                    values["scheduled_at"],
                    values["deadline_at"],
                    values["repeater"],
                    values["duration_minutes"],
                    _now_iso(),
                    completed_at,
                    values["notes"],
                    todo_id,
                ),
            )
            if cursor.rowcount != 1:
                raise ValueError("Todo not found")
            if clean_tags is not None:
                self._replace_tags(conn, todo_id, clean_tags)
            self._replace_occurrences(conn, todo_id, occurrences)

    def get_todo(self, todo_id: int) -> Optional[TodoRecord]:
        row = self._db.fetch_one("SELECT * FROM todos WHERE id = ?", (todo_id,))
        return _row_to_record(row) if row else None

    def list_todos(
        self,
        statuses: Optional[Iterable[str]] = None,
        tags: Optional[Iterable[str]] = None,
        include_done: bool = False,
    ) -> list[TodoRecord]:
        status_values = list(statuses) if statuses is not None else []
        tag_values = list(tags) if tags is not None else []
        for status in status_values:
            _validate_status(status)
        clauses = []
        params: list[object] = []
        if status_values:
            placeholders = ",".join("?" for _ in status_values)
            clauses.append(f"status IN ({placeholders})")
            params.extend(status_values)
        elif not include_done:
            clauses.append("status != ?")
            params.append("DONE")
        if tag_values:
            clean_tags = _clean_tags(tag_values)
            placeholders = ",".join("?" for _ in clean_tags)
            clauses.append(
                f"id IN (SELECT todo_id FROM todo_tags WHERE tag IN ({placeholders}))"
            )
            params.extend(clean_tags)
        where_clause = " WHERE " + " AND ".join(clauses) if clauses else ""
        rows = self._db.fetch_all(
            f"SELECT * FROM todos{where_clause} ORDER BY created_at DESC",
            params,
        )
        return [_row_to_record(row) for row in rows]

    def search_todos(self, query: str) -> list[TodoRecord]:
        pattern = f"%{query.lower()}%"
        rows = self._db.fetch_all(
            "SELECT * FROM todos WHERE lower(title) LIKE ? OR lower(notes) LIKE ?",
            (pattern, pattern),
        )
        return [_row_to_record(row) for row in rows]

    def tags_for(self, todo_id: int) -> list[str]:
        rows = self._db.fetch_all(
            "SELECT tag FROM todo_tags WHERE todo_id = ? ORDER BY tag",
            (todo_id,),
        )
        return [row["tag"] for row in rows]

    def list_occurrences(self, since: Optional[str] = None) -> list[tuple[int, str]]:
        if since is not None:
            since = _validate_timestamp("since", since)
            rows = self._db.fetch_all(
                "SELECT todo_id, occurrence_at FROM todo_occurrences WHERE occurrence_at >= ?",
                (since,),
            )
        else:
            rows = self._db.fetch_all("SELECT todo_id, occurrence_at FROM todo_occurrences")
        return [(row["todo_id"], row["occurrence_at"]) for row in rows]

    def schedule_conflicts(
        self,
        scheduled_at: str,
        duration_minutes: int,
        exclude_todo_id: Optional[int] = None,
        default_duration_minutes: int = DEFAULT_DURATION_MINUTES,
    ) -> bool:
        start_text = _validate_timestamp("scheduled_at", scheduled_at)
        duration = _validate_duration(duration_minutes)
        default_duration = _validate_duration(default_duration_minutes)
        proposed_start = _comparable_datetime(start_text)
        proposed_end = proposed_start + timedelta(minutes=duration)
        params: list[object] = []
        where = " WHERE todos.status NOT IN ('DONE', 'CANCELED')"
        if exclude_todo_id is not None:
            where += " AND occurrences.todo_id != ?"
            params.append(exclude_todo_id)
        rows = self._db.fetch_all(
            """
            SELECT occurrences.todo_id, occurrences.occurrence_at,
                   COALESCE(todos.duration_minutes, ?) AS duration_minutes
            FROM todo_occurrences AS occurrences
            JOIN todos ON todos.id = occurrences.todo_id
            """ + where,
            [default_duration, *params],
        )
        for row in rows:
            existing_start = _comparable_datetime(row["occurrence_at"])
            existing_end = existing_start + timedelta(minutes=row["duration_minutes"])
            if proposed_start < existing_end and existing_start < proposed_end:
                return True
        return False

    def add_reminder(self, todo_id: int, remind_at: str, channel: str) -> None:
        remind_at = _validate_timestamp("remind_at", remind_at)
        if not isinstance(channel, str) or not channel.strip():
            raise ValueError("channel must not be empty")
        self._db.execute(
            "INSERT INTO todo_reminders (todo_id, remind_at, channel) VALUES (?, ?, ?)",
            (todo_id, remind_at, channel.strip()),
        )

    def pending_reminders(self, now_iso: str) -> list[tuple[int, str, str]]:
        now_iso = _validate_timestamp("now_iso", now_iso)
        rows = self._db.fetch_all(
            """
            SELECT todo_id, remind_at, channel
            FROM todo_reminders
            WHERE sent_at IS NULL AND remind_at <= ?
            """,
            (now_iso,),
        )
        return [(row["todo_id"], row["remind_at"], row["channel"]) for row in rows]

    def mark_reminder_sent(self, todo_id: int, remind_at: str, channel: str) -> None:
        remind_at = _validate_timestamp("remind_at", remind_at)
        self._db.execute(
            """
            UPDATE todo_reminders SET sent_at = ?
            WHERE todo_id = ? AND remind_at = ? AND channel = ?
            """,
            (_now_iso(), todo_id, remind_at, channel),
        )

    @staticmethod
    def _replace_tags(
        conn: sqlite3.Connection,
        todo_id: int,
        tags: Sequence[str],
    ) -> None:
        conn.execute("DELETE FROM todo_tags WHERE todo_id = ?", (todo_id,))
        conn.executemany(
            "INSERT INTO todo_tags (todo_id, tag) VALUES (?, ?)",
            [(todo_id, tag) for tag in tags],
        )

    @staticmethod
    def _replace_occurrences(
        conn: sqlite3.Connection,
        todo_id: int,
        occurrences: Sequence[str],
    ) -> None:
        conn.execute("DELETE FROM todo_occurrences WHERE todo_id = ?", (todo_id,))
        conn.executemany(
            "INSERT INTO todo_occurrences (todo_id, occurrence_at) VALUES (?, ?)",
            [(todo_id, occurrence) for occurrence in occurrences],
        )


def _now_iso() -> str:
    return datetime.now(timezone.utc).replace(tzinfo=None, microsecond=0).isoformat()


def _row_to_record(row: sqlite3.Row) -> TodoRecord:
    return TodoRecord(
        id=int(row["id"]),
        title=row["title"],
        status=row["status"],
        priority=row["priority"],
        scheduled_at=row["scheduled_at"],
        deadline_at=row["deadline_at"],
        repeater=row["repeater"],
        duration_minutes=row["duration_minutes"],
        created_at=row["created_at"],
        updated_at=row["updated_at"],
        completed_at=row["completed_at"],
        notes=row["notes"],
    )


def _validate_todo_values(**values: object) -> dict[str, object]:
    title = values["title"]
    if not isinstance(title, str) or not title.strip():
        raise ValueError("title must not be empty")
    status = _validate_status(values["status"])
    priority = _validate_priority(values["priority"])
    scheduled_at = _validate_optional_timestamp("scheduled_at", values["scheduled_at"])
    deadline_at = _validate_optional_timestamp("deadline_at", values["deadline_at"])
    repeater = _validate_repeater(values["repeater"])
    duration = _validate_optional_duration(values["duration_minutes"])
    notes = values["notes"]
    if notes is not None and not isinstance(notes, str):
        raise ValueError("notes must be a string or cleared")
    if repeater is not None and scheduled_at is None:
        raise ValueError("repeater requires scheduled_at")
    return {
        "title": title.strip(),
        "status": status,
        "priority": priority,
        "scheduled_at": scheduled_at,
        "deadline_at": deadline_at,
        "repeater": repeater,
        "duration_minutes": duration,
        "notes": notes,
    }


def _required_patch(name: str, patch: Patch[str], current: str) -> str:
    if patch is PRESERVE:
        return current
    if patch is CLEAR:
        raise ValueError(f"{name} cannot be cleared")
    return patch


def _optional_patch(patch: Patch[_PatchValue], current: _PatchValue) -> Optional[_PatchValue]:
    if patch is PRESERVE:
        return current
    if patch is CLEAR:
        return None
    return patch


def _validate_status(value: object) -> str:
    if not isinstance(value, str) or value not in DEFAULT_STATUSES:
        raise ValueError(f"status must be one of {', '.join(DEFAULT_STATUSES)}")
    return value


def _validate_priority(value: object) -> Optional[str]:
    if value is None:
        return None
    if not isinstance(value, str) or value not in DEFAULT_PRIORITIES:
        raise ValueError(f"priority must be one of {', '.join(DEFAULT_PRIORITIES)} or cleared")
    return value


def _validate_optional_duration(value: object) -> Optional[int]:
    if value is None:
        return None
    return _validate_duration(value)


def _validate_duration(value: object) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise ValueError("duration_minutes must be a positive integer")
    return value


def _validate_optional_timestamp(name: str, value: object) -> Optional[str]:
    if value is None:
        return None
    return _validate_timestamp(name, value)


def _validate_timestamp(name: str, value: object) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be an ISO-8601 timestamp")
    try:
        parsed = datetime.fromisoformat(value.strip())
    except ValueError as error:
        raise ValueError(f"{name} must be an ISO-8601 timestamp") from error
    return parsed.replace(microsecond=0).isoformat()


def _validate_repeater(value: object) -> Optional[str]:
    if value is None:
        return None
    if not isinstance(value, str) or _REPEATER_PATTERN.fullmatch(value.strip()) is None:
        raise ValueError(f"repeater must use one of: {RECURRENCE_SYNTAX}")
    return value.strip()


def _clean_tags(tags: Iterable[str]) -> list[str]:
    if isinstance(tags, str):
        raise ValueError("tags must be an iterable of strings")
    clean_tags = set()
    for tag in tags:
        if not isinstance(tag, str):
            raise ValueError("tags must be strings")
        if tag.strip():
            clean_tags.add(tag.strip())
    return sorted(clean_tags)


def _occurrences(scheduled_at: object, repeater: object, count: int = 5) -> list[str]:
    if scheduled_at is None:
        return []
    start = str(scheduled_at)
    repeated = _expand_repeater(start, str(repeater), count) if repeater else []
    return [start, *repeated]


def _expand_repeater(start_iso: str, repeater: str, count: int = 5) -> list[str]:
    if count < 0:
        raise ValueError("count must not be negative")
    start = datetime.fromisoformat(_validate_timestamp("start_iso", start_iso))
    match = _REPEATER_PATTERN.fullmatch(_validate_repeater(repeater) or "")
    if match is None:
        raise ValueError(f"repeater must use one of: {RECURRENCE_SYNTAX}")
    value = int(match.group(1))
    unit = match.group(2)
    return [_advance_repeater(start, value, unit, index).isoformat() for index in range(1, count + 1)]


def _advance_repeater(start: datetime, value: int, unit: str, index: int) -> datetime:
    amount = value * index
    if unit == "min":
        return start + timedelta(minutes=amount)
    if unit == "h":
        return start + timedelta(hours=amount)
    if unit == "d":
        return start + timedelta(days=amount)
    if unit == "w":
        return start + timedelta(weeks=amount)
    if unit == "m":
        return _add_months(start, amount)
    return _add_months(start, amount * 12)


def _add_months(start: datetime, months: int) -> datetime:
    month_index = start.year * 12 + start.month - 1 + months
    year, zero_based_month = divmod(month_index, 12)
    month = zero_based_month + 1
    day = min(start.day, calendar.monthrange(year, month)[1])
    return start.replace(year=year, month=month, day=day)


def _comparable_datetime(value: str) -> datetime:
    parsed = datetime.fromisoformat(value)
    if parsed.tzinfo is None:
        return parsed
    return parsed.astimezone(timezone.utc).replace(tzinfo=None)


__all__ = [
    "CLEAR",
    "DEFAULT_DURATION_MINUTES",
    "DEFAULT_PRIORITIES",
    "DEFAULT_STATUSES",
    "PRESERVE",
    "RECURRENCE_SYNTAX",
    "PatchMode",
    "TodoRecord",
    "TodoStore",
]
