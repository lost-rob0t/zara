"""
Todo storage and scheduling logic.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Iterable, Optional, Sequence

from .database import DatabaseManager, get_database

logger = logging.getLogger(__name__)


DEFAULT_STATUSES = ["TODO", "NEXT", "WAITING", "SOMEDAY", "DONE", "CANCELED"]


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
        now = _now_iso()
        cursor = self._db.execute(
            """
            INSERT INTO todos (
                title, status, priority, scheduled_at, deadline_at, repeater,
                duration_minutes, created_at, updated_at, completed_at, notes
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            (
                title,
                status,
                priority,
                scheduled_at,
                deadline_at,
                repeater,
                duration_minutes,
                now,
                now,
                now if status == "DONE" else None,
                notes,
            ),
        )
        todo_id = int(cursor.lastrowid or 0)
        self._replace_tags(todo_id, tags or [])
        self._refresh_occurrences(todo_id, scheduled_at, repeater)
        return todo_id

    def update_todo(
        self,
        todo_id: int,
        title: Optional[str] = None,
        status: Optional[str] = None,
        priority: Optional[str] = None,
        scheduled_at: Optional[str] = None,
        deadline_at: Optional[str] = None,
        repeater: Optional[str] = None,
        duration_minutes: Optional[int] = None,
        tags: Optional[Iterable[str]] = None,
        notes: Optional[str] = None,
    ) -> None:
        row = self.get_todo(todo_id)
        if row is None:
            raise ValueError("Todo not found")
        status_value = status or row.status
        completed_at = row.completed_at
        if status_value == "DONE" and row.completed_at is None:
            completed_at = _now_iso()
        if status_value != "DONE":
            completed_at = None
        updated_at = _now_iso()
        self._db.execute(
            """
            UPDATE todos SET
                title = ?,
                status = ?,
                priority = ?,
                scheduled_at = ?,
                deadline_at = ?,
                repeater = ?,
                duration_minutes = ?,
                updated_at = ?,
                completed_at = ?,
                notes = ?
            WHERE id = ?
            """,
            (
                title if title is not None else row.title,
                status_value,
                priority if priority is not None else row.priority,
                scheduled_at if scheduled_at is not None else row.scheduled_at,
                deadline_at if deadline_at is not None else row.deadline_at,
                repeater if repeater is not None else row.repeater,
                duration_minutes if duration_minutes is not None else row.duration_minutes,
                updated_at,
                completed_at,
                notes if notes is not None else row.notes,
                todo_id,
            ),
        )
        if tags is not None:
            self._replace_tags(todo_id, tags)
        final_scheduled = scheduled_at if scheduled_at is not None else row.scheduled_at
        final_repeater = repeater if repeater is not None else row.repeater
        self._refresh_occurrences(todo_id, final_scheduled, final_repeater)

    def get_todo(self, todo_id: int) -> Optional[TodoRecord]:
        row = self._db.fetch_one("SELECT * FROM todos WHERE id = ?", (todo_id,))
        return _row_to_record(row) if row else None

    def list_todos(
        self,
        statuses: Optional[Iterable[str]] = None,
        tags: Optional[Iterable[str]] = None,
        include_done: bool = False,
    ) -> list[TodoRecord]:
        clauses = []
        params: list[object] = []
        if statuses:
            placeholders = ",".join("?" for _ in statuses)
            clauses.append(f"status IN ({placeholders})")
            params.extend(list(statuses))
        elif not include_done:
            clauses.append("status != ?")
            params.append("DONE")
        if tags:
            placeholders = ",".join("?" for _ in tags)
            clauses.append(
                f"id IN (SELECT todo_id FROM todo_tags WHERE tag IN ({placeholders}))"
            )
            params.extend(list(tags))
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
        if since:
            rows = self._db.fetch_all(
                "SELECT todo_id, occurrence_at FROM todo_occurrences WHERE occurrence_at >= ?",
                (since,),
            )
        else:
            rows = self._db.fetch_all("SELECT todo_id, occurrence_at FROM todo_occurrences")
        return [(row["todo_id"], row["occurrence_at"]) for row in rows]

    def add_reminder(self, todo_id: int, remind_at: str, channel: str) -> None:
        self._db.execute(
            "INSERT INTO todo_reminders (todo_id, remind_at, channel) VALUES (?, ?, ?)",
            (todo_id, remind_at, channel),
        )

    def pending_reminders(self, now_iso: str) -> list[tuple[int, str, str]]:
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
        self._db.execute(
            """
            UPDATE todo_reminders
            SET sent_at = ?
            WHERE todo_id = ? AND remind_at = ? AND channel = ?
            """,
            (_now_iso(), todo_id, remind_at, channel),
        )

    def _replace_tags(self, todo_id: int, tags: Iterable[str]) -> None:
        clean_tags = sorted({tag.strip() for tag in tags if tag.strip()})
        self._db.execute("DELETE FROM todo_tags WHERE todo_id = ?", (todo_id,))
        for tag in clean_tags:
            self._db.execute(
                "INSERT INTO todo_tags (todo_id, tag) VALUES (?, ?)",
                (todo_id, tag),
            )

    def _refresh_occurrences(
        self,
        todo_id: int,
        scheduled_at: Optional[str],
        repeater: Optional[str],
    ) -> None:
        self._db.execute("DELETE FROM todo_occurrences WHERE todo_id = ?", (todo_id,))
        if not scheduled_at:
            return
        self._db.execute(
            "INSERT INTO todo_occurrences (todo_id, occurrence_at) VALUES (?, ?)",
            (todo_id, scheduled_at),
        )
        if repeater:
            try:
                occurrences = _expand_repeater(scheduled_at, repeater, count=5)
            except ValueError:
                logger.warning("Invalid repeater: %s", repeater)
                return
            for occurrence in occurrences:
                self._db.execute(
                    "INSERT INTO todo_occurrences (todo_id, occurrence_at) VALUES (?, ?)",
                    (todo_id, occurrence),
                )


def _now_iso() -> str:
    return datetime.utcnow().replace(microsecond=0).isoformat()


def _row_to_record(row) -> TodoRecord:
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


def _expand_repeater(start_iso: str, repeater: str, count: int = 5) -> list[str]:
    if not repeater:
        return []
    base = datetime.fromisoformat(start_iso)
    step = _parse_repeater_delta(repeater)
    occurrences = []
    current = base
    for _ in range(count):
        current = current + step
        occurrences.append(current.isoformat())
    return occurrences


def _parse_repeater_delta(repeater: str) -> timedelta:
    trimmed = repeater.strip()
    if not trimmed:
        raise ValueError("Empty repeater")
    mode = trimmed[0]
    if mode not in {"+", "-"}:
        raise ValueError("Repeater must start with +")
    interval = trimmed[1:]
    number_text = "".join(ch for ch in interval if ch.isdigit())
    unit = "".join(ch for ch in interval if ch.isalpha())
    if not number_text or not unit:
        raise ValueError("Invalid repeater format")
    value = int(number_text)
    unit = unit.lower()
    if unit.startswith("min"):
        return timedelta(minutes=value)
    if unit.startswith("h"):
        return timedelta(hours=value)
    if unit.startswith("d"):
        return timedelta(days=value)
    if unit.startswith("w"):
        return timedelta(weeks=value)
    if unit.startswith("m"):
        return timedelta(days=value * 30)
    if unit.startswith("y"):
        return timedelta(days=value * 365)
    raise ValueError("Unsupported repeater unit")
