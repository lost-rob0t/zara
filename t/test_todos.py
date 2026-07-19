from __future__ import annotations

import sqlite3

import pytest

from zara.database import DatabaseManager
from zara.todo_storage import CLEAR, PRESERVE, TodoStore, _expand_repeater


@pytest.fixture
def store(tmp_path):
    database = DatabaseManager(tmp_path / "todos.db")
    todo_store = TodoStore(database)
    yield todo_store
    database.close()


def test_create_edit_clear_complete_and_reopen(store):
    todo_id = store.add_todo(
        "  Ship release  ",
        "TODO",
        priority="A",
        scheduled_at="2026-01-31T09:00:00",
        deadline_at="2026-02-01T09:00:00",
        repeater="+1m",
        duration_minutes=45,
        tags=["release", " work "],
        notes="Initial",
    )

    store.update_todo(todo_id, title="Ship stable release", priority=PRESERVE)
    edited = store.get_todo(todo_id)
    assert edited is not None
    assert edited.title == "Ship stable release"
    assert edited.priority == "A"

    store.update_todo(
        todo_id,
        priority=CLEAR,
        scheduled_at=CLEAR,
        deadline_at=CLEAR,
        repeater=CLEAR,
        duration_minutes=CLEAR,
        tags=CLEAR,
        notes=CLEAR,
    )
    cleared = store.get_todo(todo_id)
    assert cleared is not None
    assert cleared.priority is None
    assert cleared.scheduled_at is None
    assert cleared.deadline_at is None
    assert cleared.repeater is None
    assert cleared.duration_minutes is None
    assert cleared.notes is None
    assert store.tags_for(todo_id) == []
    assert store.list_occurrences() == []

    store.update_todo(todo_id, status="DONE")
    completed = store.get_todo(todo_id)
    assert completed is not None
    assert completed.status == "DONE"
    assert completed.completed_at is not None

    store.update_todo(todo_id, status="TODO")
    reopened = store.get_todo(todo_id)
    assert reopened is not None
    assert reopened.status == "TODO"
    assert reopened.completed_at is None


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("status", "BROKEN"),
        ("priority", "urgent"),
        ("duration_minutes", 0),
        ("duration_minutes", -2),
        ("duration_minutes", True),
        ("scheduled_at", "tomorrow"),
        ("deadline_at", "2026-99-01T00:00:00"),
        ("repeater", "-1d"),
        ("repeater", "+0d"),
        ("repeater", "+1fortnight"),
    ],
)
def test_invalid_create_never_writes(store, field, value):
    values = {"title": "Invalid", "status": "TODO"}
    values[field] = value
    if field == "repeater":
        values["scheduled_at"] = "2026-01-01T09:00:00"
    with pytest.raises(ValueError):
        store.add_todo(**values)
    assert store.list_todos(include_done=True) == []


def test_invalid_update_preserves_every_table(store):
    todo_id = store.add_todo(
        "Original",
        "TODO",
        scheduled_at="2026-01-01T09:00:00",
        duration_minutes=30,
        tags=["safe"],
    )
    before = store.get_todo(todo_id)
    occurrences = store.list_occurrences()

    with pytest.raises(ValueError):
        store.update_todo(todo_id, title="Changed", duration_minutes=0, tags=["changed"])

    assert store.get_todo(todo_id) == before
    assert store.tags_for(todo_id) == ["safe"]
    assert store.list_occurrences() == occurrences


def test_calendar_recurrence_preserves_original_month_day():
    assert _expand_repeater("2025-01-31T09:00:00", "+1m", 3) == [
        "2025-02-28T09:00:00",
        "2025-03-31T09:00:00",
        "2025-04-30T09:00:00",
    ]
    assert _expand_repeater("2024-02-29T09:00:00", "+1y", 4) == [
        "2025-02-28T09:00:00",
        "2026-02-28T09:00:00",
        "2027-02-28T09:00:00",
        "2028-02-29T09:00:00",
    ]
    assert _expand_repeater("2025-12-31T23:00:00", "+1h", 2) == [
        "2026-01-01T00:00:00",
        "2026-01-01T01:00:00",
    ]


def test_duplicate_tags_and_single_pass_iterables(store):
    tags = (tag for tag in ["work", "work", " work ", "home"])
    todo_id = store.add_todo("Tagged", "TODO", tags=tags)
    assert store.tags_for(todo_id) == ["home", "work"]

    statuses = (status for status in ["TODO"])
    selected_tags = (tag for tag in ["work"])
    assert [todo.id for todo in store.list_todos(statuses, selected_tags)] == [todo_id]

    with pytest.raises(ValueError):
        store.update_todo(todo_id, tags="not-a-tag-list")
    assert store.tags_for(todo_id) == ["home", "work"]


def test_add_rolls_back_todo_when_related_write_fails(store):
    store._db.execute(
        """
        CREATE TRIGGER reject_tag BEFORE INSERT ON todo_tags
        BEGIN SELECT RAISE(ABORT, 'tag rejected'); END
        """
    )

    with pytest.raises(sqlite3.IntegrityError):
        store.add_todo(
            "Atomic",
            "TODO",
            scheduled_at="2026-01-01T09:00:00",
            tags=["reject"],
        )

    assert store.list_todos(include_done=True) == []
    assert store.list_occurrences() == []


def test_update_rolls_back_todo_and_occurrences_when_tag_write_fails(store):
    todo_id = store.add_todo(
        "Atomic",
        "TODO",
        scheduled_at="2026-01-01T09:00:00",
        tags=["original"],
    )
    store._db.execute(
        """
        CREATE TRIGGER reject_update_tag BEFORE INSERT ON todo_tags
        WHEN NEW.tag = 'reject'
        BEGIN SELECT RAISE(ABORT, 'tag rejected'); END
        """
    )

    with pytest.raises(sqlite3.IntegrityError):
        store.update_todo(
            todo_id,
            title="Changed",
            scheduled_at="2026-01-02T09:00:00",
            tags=["reject"],
        )

    todo = store.get_todo(todo_id)
    assert todo is not None
    assert todo.title == "Atomic"
    assert todo.scheduled_at == "2026-01-01T09:00:00"
    assert store.tags_for(todo_id) == ["original"]
    assert store.list_occurrences() == [(todo_id, "2026-01-01T09:00:00")]


def test_duration_aware_overlap_uses_half_open_intervals(store):
    first_id = store.add_todo(
        "First",
        "TODO",
        scheduled_at="2026-04-01T10:00:00",
        duration_minutes=60,
    )

    assert store.schedule_conflicts("2026-04-01T10:30:00", 15)
    assert not store.schedule_conflicts("2026-04-01T11:00:00", 30)
    assert not store.schedule_conflicts(
        "2026-04-01T10:30:00",
        15,
        exclude_todo_id=first_id,
    )


def test_overlap_uses_supplied_default_for_missing_existing_duration(store):
    store.add_todo(
        "Default duration",
        "TODO",
        scheduled_at="2026-04-01T10:00:00",
    )

    assert not store.schedule_conflicts("2026-04-01T10:40:00", 5)
    assert store.schedule_conflicts(
        "2026-04-01T10:40:00",
        5,
        default_duration_minutes=45,
    )


def test_overlap_includes_generated_recurrences(store):
    store.add_todo(
        "Monthly",
        "TODO",
        scheduled_at="2026-01-31T10:00:00",
        repeater="+1m",
        duration_minutes=60,
    )
    assert store.schedule_conflicts("2026-02-28T10:30:00", 15)
    assert not store.schedule_conflicts("2026-02-28T11:00:00", 15)
