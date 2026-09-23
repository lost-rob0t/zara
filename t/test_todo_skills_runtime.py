from __future__ import annotations

from dataclasses import replace

import pytest

import zara.todo_skills as todo


NOW = "2026-09-23T12:00:00"


def record(todo_id=1, *, title="Ship it", status="TODO", priority=None,
           scheduled_at=None, deadline_at=None, duration_minutes=None, notes=None):
    return todo.TodoRecord(
        id=todo_id,
        title=title,
        status=status,
        priority=priority,
        scheduled_at=scheduled_at,
        deadline_at=deadline_at,
        repeater=None,
        duration_minutes=duration_minutes,
        created_at=NOW,
        updated_at=None,
        completed_at=None,
        notes=notes,
    )


class Config:
    def __init__(self, values=None):
        self.values = values or {}

    def get_section(self, name):
        assert name == "todo"
        return dict(self.values)


class FakeTodoStore(todo.TodoStore):
    def __init__(self, records=()):
        self.records = {item.id: item for item in records}
        self.tags = {}
        self.conflict = False
        self.raise_update = False
        self.calls = []
        self.next_id = 10

    def add_todo(self, *, title, status, **_kwargs):
        self.calls.append(("add", title, status))
        todo_id = self.next_id
        self.records[todo_id] = record(todo_id, title=title, status=status)
        return todo_id

    def list_todos(self, *, statuses=None, include_done=False):
        self.calls.append(("list", statuses, include_done))
        values = list(self.records.values())
        if statuses:
            values = [item for item in values if item.status in statuses]
        if not include_done:
            values = [item for item in values if item.status != "DONE"]
        return values

    def update_todo(self, todo_id, **changes):
        self.calls.append(("update", todo_id, changes))
        if self.raise_update or todo_id not in self.records:
            raise ValueError("missing")
        self.records[todo_id] = replace(self.records[todo_id], **changes)

    def search_todos(self, query):
        self.calls.append(("search", query))
        return [item for item in self.records.values() if query.lower() in item.title.lower()]

    def get_todo(self, todo_id):
        return self.records.get(todo_id)

    def tags_for(self, todo_id):
        return self.tags.get(todo_id, [])

    def schedule_conflicts(self, *args, **kwargs):
        self.calls.append(("conflicts", args, kwargs))
        return self.conflict


class FakeOrgStore(todo.OrgTodoStore):
    def __init__(self, records=()):
        self.root = "/tmp/org"
        self.records = {str(item.id): item for item in records}

    def get_todo(self, todo_id):
        return self.records.get(str(todo_id))

    def tags_for(self, _todo_id):
        return []


@pytest.fixture
def store(monkeypatch):
    value = FakeTodoStore(
        [
            record(1, title="Alpha", priority="A", scheduled_at="2026-09-24T10:00:00",
                   deadline_at="2026-09-25T10:00:00", duration_minutes=45, notes="note"),
            record(2, title="Beta", status="DONE"),
        ]
    )
    value.tags[1] = ["work", "core"]
    monkeypatch.setattr(todo, "_todo_store", lambda: value)
    monkeypatch.setattr(todo, "get_config", lambda: Config())
    return value


def test_capture_list_edit_search_and_status_commands(store):
    assert todo.capture_todo([]) == "No todo content provided."
    assert todo.capture_todo(["new", None, "task"]) == "Todo captured (id 10)."
    assert store.records[10].title == "new task"

    listed = todo.list_todos(["TODO,DONE,bogus"])
    assert "#1 Alpha" in listed
    assert ":work:core:" in listed
    assert "[#A]" in listed
    assert "SCHEDULED:" in listed and "DEADLINE:" in listed

    assert todo.edit_todo([1]) == "Provide todo id and update text."
    assert todo.edit_todo(["wat", "x"]) == "Invalid todo id."
    assert todo.edit_todo([999, "x"]) == "Todo not found."
    assert todo.edit_todo([1, "Renamed", "task"]) == "Todo updated."
    assert store.records[1].title == "Renamed task"

    assert todo.complete_todo([]) == "Provide todo id."
    assert todo.complete_todo(["wat"]) == "Invalid todo id."
    assert todo.complete_todo([999]) == "Todo not found."
    assert todo.complete_todo([1]) == "Todo completed."
    assert store.records[1].status == "DONE"
    assert todo.reopen_todo([1]) == "Todo reopened."
    assert store.records[1].status == "TODO"

    assert todo.search_todos([]) == "Search query required."
    assert "Renamed task" in todo.search_todos(["renamed"])
    assert todo.search_todos(["missing"]) == "No todos found."


def test_schedule_export_and_org_surface(store, monkeypatch):
    assert todo.schedule_todo([1]) == "Provide todo id and schedule time."
    assert todo.schedule_todo(["bad", "2026-09-24 11:00"]) == "Invalid todo id."
    assert todo.schedule_todo([1, "tomorrow"]) == "Could not parse schedule time."
    assert todo.schedule_todo([999, "2026-09-24 11:00"]) == "Todo not found."

    store.conflict = True
    assert todo.schedule_todo([1, "2026-09-24 11:00"]) == "Schedule conflict detected."
    store.conflict = False
    store.raise_update = True
    assert todo.schedule_todo([1, "2026-09-24T11:00"]) == "Todo not found."
    store.raise_update = False
    assert todo.schedule_todo([1, "2026-09-24T11:00:00"]) == "Todo scheduled."

    markdown = todo.export_todos(["markdown"])
    assert "- [ ] Alpha" in markdown
    org = todo.export_todos(["unknown"])
    assert ":PROPERTIES:" in org and ":EFFORT: 0:45" in org and "note" in org

    assert "disabled" in todo.open_org_todos([])
    org_store = FakeOrgStore([record("abcdefghijk", title="Org item")])
    monkeypatch.setattr(todo, "_todo_store", lambda: org_store)
    assert todo.open_org_todos([]) == "Org editor target: /tmp/org"
    assert "#abcdefgh Org item" in todo.open_org_todos(["#abcdefghijk"])
    assert todo.open_org_todos(["missing"]) == "Org editor target: /tmp/org"


def test_brief_and_expert_scoring(store, monkeypatch):
    store.records = {}
    assert todo.todo_brief([]) == "No active todos found."

    store.records = {
        1: record(1, title="Zulu", priority="A", scheduled_at="2026-09-24T10:00:00"),
        2: record(2, title="Alpha", priority="B"),
    }

    class Engine:
        def query_once(self, goal):
            return {"State": "urgent", "Score": 9} if "todo_expert" in goal else None

    monkeypatch.setattr(todo, "_load_todo_expert", lambda: Engine())
    text = todo.todo_brief(["1"])
    assert text.count("\n") == 0
    assert "score" in text

    assert todo._expert_state_score(None, store.records[1], 0) == ("unknown", 0.0)

    class Broken:
        def query_once(self, _goal):
            raise RuntimeError("boom")

    assert todo._expert_state_score(Broken(), store.records[1], 0) == ("unknown", 0.0)

    class Empty:
        def query_once(self, _goal):
            return None

    assert todo._expert_state_score(Empty(), store.records[1], 0) == ("unknown", 0.0)

    class Good:
        def query_once(self, _goal):
            return {"State": "ready", "Score": "3.5"}

    assert todo._expert_state_score(Good(), store.records[1], 0) == ("ready", 3.5)


def test_helpers_cover_config_parsing_and_formatting(monkeypatch):
    fake = FakeTodoStore([record(1, title="A")])

    monkeypatch.setattr(todo, "get_config", lambda: Config({"default_status": "next"}))
    assert todo._default_status(fake) == "NEXT"
    monkeypatch.setattr(todo, "get_config", lambda: Config({"default_status": "invalid"}))
    assert todo._default_status(fake) == "TODO"

    for value, expected in [(True, todo.DEFAULT_DURATION_MINUTES), ("bad", todo.DEFAULT_DURATION_MINUTES),
                            (0, todo.DEFAULT_DURATION_MINUTES), (55, 55)]:
        monkeypatch.setattr(todo, "get_config", lambda value=value: Config({"default_duration_minutes": value}))
        assert todo._default_duration_minutes() == expected

    assert todo._normalize_statuses([], fake) is None
    assert todo._normalize_statuses(["todo, done, nope"], fake) == ["TODO", "DONE"]
    assert todo._normalize_statuses(["nope"], fake) is None
    assert todo._default_export_format(["markdown"]) == "markdown"
    assert todo._default_export_format(["wat"]) == "org"
    assert todo._default_export_format([]) == "org"

    assert todo._parse_store_id(fake, "#12") == 12
    assert todo._parse_store_id(fake, "id 13") == 13
    assert todo._parse_store_id(fake, "") is None
    assert todo._parse_store_id(fake, "wat") is None

    class BadString:
        def __str__(self):
            raise RuntimeError("nope")

    assert todo._parse_store_id(fake, BadString()) is None
    assert todo._display_id("123456789") == "12345678"
    assert todo._display_id(7) == "7"

    assert todo._parse_time("2026-09-24 10:11") == "2026-09-24T10:11:00"
    assert todo._parse_time("2026-09-24T10:11") == "2026-09-24T10:11:00"
    assert todo._parse_time("2026-09-24T10:11:12") == "2026-09-24T10:11:12"
    assert todo._parse_time("bad") is None

    assert todo._export_org([], fake) == "No todos found."
    assert todo._export_markdown([], fake) == "No todos found."
    assert todo._format_todos([], fake) == "No todos found."

    assert todo._epoch_term(None) == "none"
    assert todo._epoch_term("bad") == "none"
    assert todo._epoch_term("2026-09-23T00:00:00").lstrip("-").isdigit()
    assert todo._prolog_atom("simple_atom") == "simple_atom"
    assert todo._prolog_atom("Needs Quote's") == "'Needs Quote''s'"

    assert todo._parse_limit([], 8) == 8
    assert todo._parse_limit(["bad"], 8) == 8
    assert todo._parse_limit(["0"], 8) == 1
    assert todo._parse_limit(["99"], 8) == 25
    assert todo._parse_limit(["4"], 8) == 4


def test_todo_store_factory_selects_configured_backend(monkeypatch):
    sqlite_marker = object()
    org_marker = object()
    monkeypatch.setattr(todo, "get_config", lambda: Config({"backend": "sqlite"}))
    monkeypatch.setattr(todo, "TodoStore", lambda: sqlite_marker)
    assert todo._todo_store() is sqlite_marker

    monkeypatch.setattr(todo, "get_config", lambda: Config({"backend": "org", "default_file": ""}))
    monkeypatch.setattr(todo, "resolve_org_todo_root", lambda _config: "/root")
    monkeypatch.setattr(todo, "OrgTodoStore", lambda root, default_file: (root, default_file, org_marker))
    assert todo._todo_store() == ("/root", "inbox.org", org_marker)
