from __future__ import annotations

from zara import todo_skills as ts
from zara.todo_storage import TodoRecord


def record(
    todo_id=1,
    *,
    title="Task",
    status="TODO",
    priority=None,
    scheduled_at=None,
    deadline_at=None,
    duration_minutes=None,
    notes=None,
):
    return TodoRecord(
        id=todo_id,
        title=title,
        status=status,
        priority=priority,
        scheduled_at=scheduled_at,
        deadline_at=deadline_at,
        repeater=None,
        duration_minutes=duration_minutes,
        created_at="2026-09-25T00:00:00",
        updated_at=None,
        completed_at=None,
        notes=notes,
    )


class FakeStore:
    def __init__(self, todos=None):
        self.todos = list(todos or [])
        self.updated = []
        self.added = []
        self.conflict = False
        self.raise_update = False
        self.tags = {}

    def add_todo(self, *, title, status):
        self.added.append((title, status))
        return 123456789

    def list_todos(self, *, statuses=None, include_done=False):
        self.last_statuses = statuses
        self.include_done = include_done
        return list(self.todos)

    def update_todo(self, todo_id, **changes):
        self.updated.append((todo_id, changes))
        if self.raise_update:
            raise ValueError("missing")

    def search_todos(self, query):
        self.last_query = query
        return list(self.todos)

    def get_todo(self, todo_id):
        self.last_get = todo_id
        return self.todos[0] if self.todos else None

    def schedule_conflicts(
        self,
        schedule_iso,
        duration,
        *,
        exclude_todo_id,
        default_duration_minutes,
    ):
        self.last_conflict = (
            schedule_iso,
            duration,
            exclude_todo_id,
            default_duration_minutes,
        )
        return self.conflict

    def tags_for(self, todo_id):
        return list(self.tags.get(todo_id, []))


class FakeSqliteStore(FakeStore):
    pass


class FakeOrgStore(FakeStore):
    def __init__(self, root="/org", default_file="inbox.org", todos=None):
        super().__init__(todos=todos)
        self.root = root
        self.default_file = default_file


class FakeConfig:
    def __init__(self, todo):
        self.todo = dict(todo)

    def get_section(self, name):
        assert name == "todo"
        return dict(self.todo)


def use_sqlite_type(monkeypatch):
    monkeypatch.setattr(ts, "TodoStore", FakeSqliteStore)
    return FakeSqliteStore


def use_org_type(monkeypatch):
    monkeypatch.setattr(ts, "OrgTodoStore", FakeOrgStore)
    return FakeOrgStore


def test_capture_list_search_and_edit_surfaces(monkeypatch):
    sqlite_type = use_sqlite_type(monkeypatch)
    store = sqlite_type([
        record(
            title="Alpha",
            priority="A",
            scheduled_at="2026-09-25T10:00:00",
            deadline_at="2026-09-26T10:00:00",
        )
    ])
    store.tags[1] = ["home", "fast"]
    monkeypatch.setattr(ts, "_todo_store", lambda: store)
    monkeypatch.setattr(ts, "_default_status", lambda _store: "TODO")

    assert ts.capture_todo([]) == "No todo content provided."
    assert ts.capture_todo(["buy", None, "milk"]) == "Todo captured (id 12345678)."
    assert store.added == [("buy milk", "TODO")]

    listed = ts.list_todos(["todo", "bogus"])
    assert "[TODO] #1 Alpha" in listed
    assert ":home:fast:" in listed
    assert "[#A]" in listed
    assert "SCHEDULED:" in listed
    assert "DEADLINE:" in listed
    assert store.last_statuses == ["TODO"]

    assert ts.search_todos([]) == "Search query required."
    assert "Alpha" in ts.search_todos(["alp", "ha"])
    assert store.last_query == "alp ha"

    assert ts.edit_todo(["1"]) == "Provide todo id and update text."
    assert ts.edit_todo(["bad", "x"]) == "Invalid todo id."
    store.raise_update = True
    assert ts.edit_todo(["1", "new"]) == "Todo not found."
    store.raise_update = False
    assert ts.edit_todo(["1", "new", "title"]) == "Todo updated."
    assert store.updated[-1] == (1, {"title": "new title"})


def test_status_mutations_cover_missing_invalid_not_found_and_success(monkeypatch):
    sqlite_type = use_sqlite_type(monkeypatch)
    store = sqlite_type([record()])
    monkeypatch.setattr(ts, "_todo_store", lambda: store)

    assert ts.complete_todo([]) == "Provide todo id."
    assert ts.complete_todo(["bad"]) == "Invalid todo id."
    store.raise_update = True
    assert ts.complete_todo(["1"]) == "Todo not found."
    store.raise_update = False
    assert ts.complete_todo(["#1"]) == "Todo completed."
    assert store.updated[-1] == (1, {"status": "DONE"})
    assert ts.reopen_todo(["id 1"]) == "Todo reopened."
    assert store.updated[-1] == (1, {"status": "TODO"})


def test_schedule_todo_rejects_bad_inputs_conflicts_and_missing(monkeypatch):
    sqlite_type = use_sqlite_type(monkeypatch)
    store = sqlite_type([record(duration_minutes=45)])
    monkeypatch.setattr(ts, "_todo_store", lambda: store)
    monkeypatch.setattr(ts, "_default_duration_minutes", lambda: 30)

    assert ts.schedule_todo(["1"]) == "Provide todo id and schedule time."
    assert ts.schedule_todo(["bad", "2026-09-25 10:00"]) == "Invalid todo id."
    assert ts.schedule_todo(["1", None]) == "Schedule time required."
    assert ts.schedule_todo(["1", "tomorrowish"]) == "Could not parse schedule time."

    store.todos = []
    assert ts.schedule_todo(["1", "2026-09-25 10:00"]) == "Todo not found."

    store.todos = [record(duration_minutes=45)]
    store.conflict = True
    assert ts.schedule_todo(["1", "2026-09-25 10:00"]) == "Schedule conflict detected."
    assert store.last_conflict == ("2026-09-25T10:00:00", 45, 1, 30)

    store.conflict = False
    store.raise_update = True
    assert ts.schedule_todo(["1", "2026-09-25T10:00"]) == "Todo not found."

    store.raise_update = False
    store.todos = [record(duration_minutes=None)]
    assert ts.schedule_todo(["1", "2026-09-25T10:00:30"]) == "Todo scheduled."
    assert store.updated[-1] == (
        1,
        {"scheduled_at": "2026-09-25T10:00:30", "duration_minutes": 30},
    )


def test_exports_and_org_open_surface(monkeypatch):
    sqlite_type = use_sqlite_type(monkeypatch)
    todo = record(
        title="Ship",
        status="DONE",
        priority="B",
        scheduled_at="2026-09-25T10:00:00",
        deadline_at="2026-09-26T10:00:00",
        duration_minutes=90,
        notes="Details",
    )
    store = sqlite_type([todo])
    store.tags[1] = ["work"]
    monkeypatch.setattr(ts, "_todo_store", lambda: store)

    org = ts.export_todos(["org"])
    assert "* DONE [#B] Ship :work:" in org
    assert ":ID: 1" in org
    assert ":EFFORT: 1:30" in org
    assert "SCHEDULED:" in org
    assert "DEADLINE:" in org
    assert "Details" in org
    assert store.include_done is True

    markdown = ts.export_todos(["markdown"])
    assert markdown == "- [x] Ship (#1)"
    assert ts.export_todos(["weird"]).startswith("* DONE")

    assert ts.open_org_todos([]) == "Org todo backend is disabled. Set todo backend to org."

    org_type = use_org_type(monkeypatch)
    org_store = org_type("/tmp/org", todos=[record("abcdef123456", title="Org task")])
    monkeypatch.setattr(ts, "_todo_store", lambda: org_store)
    assert ts.open_org_todos([]) == "Org editor target: /tmp/org"
    assert (
        ts.open_org_todos(["missing"])
        == "Org editor target: /tmp/org · #abcdef12 Org task"
    )


def test_todo_brief_empty_unknown_scored_sorting_and_limit(monkeypatch):
    sqlite_type = use_sqlite_type(monkeypatch)
    store = sqlite_type([])
    monkeypatch.setattr(ts, "_todo_store", lambda: store)
    assert ts.todo_brief([]) == "No active todos found."

    store.todos = [
        record(1, title="beta", scheduled_at="2026-09-25T11:00:00"),
        record(2, title="Alpha"),
    ]
    monkeypatch.setattr(ts, "_load_todo_expert", lambda: None)
    brief = ts.todo_brief(["1"])
    assert brief.startswith("- unknown · #2 · Alpha")
    assert brief.count("\n") == 0

    class Engine:
        def query_once(self, goal):
            assert "todo_state" in goal
            return {"State": "urgent", "Score": 9.5}

    monkeypatch.setattr(ts, "_load_todo_expert", lambda: Engine())
    brief = ts.todo_brief(["25"])
    assert "urgent" in brief
    assert "score 9.5" in brief
    assert "2026-09-25T11:00:00" in brief


def test_store_factory_selects_sqlite_and_org(monkeypatch):
    class Sqlite(FakeSqliteStore):
        constructed = 0

        def __init__(self):
            super().__init__()
            Sqlite.constructed += 1

    class Org(FakeOrgStore):
        constructed = None

        def __init__(self, root, default_file="inbox.org"):
            super().__init__(root, default_file)
            Org.constructed = (root, default_file)

    monkeypatch.setattr(ts, "TodoStore", Sqlite)
    monkeypatch.setattr(ts, "OrgTodoStore", Org)

    monkeypatch.setattr(ts, "get_config", lambda: FakeConfig({"backend": "sqlite"}))
    assert isinstance(ts._todo_store(), Sqlite)
    assert Sqlite.constructed == 1

    monkeypatch.setattr(
        ts,
        "get_config",
        lambda: FakeConfig({"backend": "org", "default_file": " tasks.org "}),
    )
    monkeypatch.setattr(ts, "resolve_org_todo_root", lambda config: "/resolved")
    store = ts._todo_store()
    assert isinstance(store, Org)
    assert Org.constructed == ("/resolved", "tasks.org")

    monkeypatch.setattr(
        ts,
        "get_config",
        lambda: FakeConfig({"backend": "anything", "default_file": "  "}),
    )
    store = ts._todo_store()
    assert isinstance(store, Org)
    assert store.default_file == "inbox.org"


def test_formatters_cover_empty_optional_and_done_states(monkeypatch):
    sqlite_type = use_sqlite_type(monkeypatch)
    store = sqlite_type([])
    assert ts._format_todos([], store) == "No todos found."
    assert ts._export_org([], store) == "No todos found."
    assert ts._export_markdown([], store) == "No todos found."

    active = record(123456789, title="Plain")
    assert ts._format_todos([active], store) == "- [TODO] #12345678 Plain"
    assert ts._export_markdown([active], store) == "- [ ] Plain (#12345678)"
    org = ts._export_org([active], store)
    assert org == "* TODO Plain\n:PROPERTIES:\n:ID: 123456789\n:END:"


def test_configuration_helpers_validate_status_duration_and_filters(monkeypatch):
    sqlite_type = use_sqlite_type(monkeypatch)
    sqlite = sqlite_type()
    monkeypatch.setattr(ts, "get_config", lambda: FakeConfig({"default_status": "next"}))
    assert ts._default_status(sqlite) == "NEXT"

    monkeypatch.setattr(ts, "get_config", lambda: FakeConfig({"default_status": "invalid"}))
    assert ts._default_status(sqlite) == "TODO"

    org_type = use_org_type(monkeypatch)
    org = org_type()
    monkeypatch.setattr(ts, "get_config", lambda: FakeConfig({"default_status": "done"}))
    assert ts._default_status(org) == "DONE"

    for value in (True, "bad", 0, -2):
        monkeypatch.setattr(
            ts,
            "get_config",
            lambda value=value: FakeConfig({"default_duration_minutes": value}),
        )
        assert ts._default_duration_minutes() == ts.DEFAULT_DURATION_MINUTES

    monkeypatch.setattr(
        ts,
        "get_config",
        lambda: FakeConfig({"default_duration_minutes": "45"}),
    )
    assert ts._default_duration_minutes() == 45

    assert ts._normalize_statuses([], sqlite) is None
    assert ts._normalize_statuses(["todo,next,bogus"], sqlite) == ["TODO", "NEXT"]
    assert ts._normalize_statuses(["", "bogus"], sqlite) is None
    assert ts._normalize_statuses(["done"], org) == ["DONE"]


def test_small_parsers_and_id_helpers_cover_edges(monkeypatch):
    sqlite_type = use_sqlite_type(monkeypatch)
    sqlite = sqlite_type()
    org_type = use_org_type(monkeypatch)
    org = org_type()

    assert ts._default_export_format([]) == "org"
    assert ts._default_export_format(["MARKDOWN"]) == "markdown"
    assert ts._default_export_format(["json"]) == "org"

    class BadString:
        def __str__(self):
            raise RuntimeError("no string")

    assert ts._parse_store_id(sqlite, BadString()) is None
    assert ts._parse_store_id(sqlite, "  ") is None
    assert ts._parse_store_id(sqlite, "#12") == 12
    assert ts._parse_store_id(sqlite, "id 13") == 13
    assert ts._parse_store_id(sqlite, "abc") is None
    assert ts._parse_store_id(org, "#abcdef") == "abcdef"

    assert ts._display_id(123) == "123"
    assert ts._display_id("123456789") == "12345678"

    assert ts._parse_time("2026-09-25 10:11") == "2026-09-25T10:11:00"
    assert ts._parse_time("2026-09-25T10:11") == "2026-09-25T10:11:00"
    assert ts._parse_time("2026-09-25T10:11:12") == "2026-09-25T10:11:12"
    assert ts._parse_time("nope") is None

    assert ts._join_args([1, None, " two "]) == "1  two"
    assert ts._parse_limit([], 8) == 8
    assert ts._parse_limit(["bad"], 8) == 8
    assert ts._parse_limit(["0"], 8) == 1
    assert ts._parse_limit(["99"], 8) == 25
    assert ts._parse_limit(["7"], 8) == 7


def test_expert_helpers_fail_closed_and_escape_terms(monkeypatch):
    todo = record(
        title="X",
        status="TODO",
        priority="A",
        scheduled_at="2026-09-25T10:00:00",
        deadline_at="bad",
        duration_minutes=15,
    )

    assert ts._expert_state_score(None, todo, 10) == ("unknown", 0.0)

    class Raising:
        def query_once(self, goal):
            raise RuntimeError(goal)

    assert ts._expert_state_score(Raising(), todo, 10) == ("unknown", 0.0)

    class Empty:
        def query_once(self, goal):
            return None

    assert ts._expert_state_score(Empty(), todo, 10) == ("unknown", 0.0)

    class Result:
        def __init__(self, value):
            self.value = value
            self.goal = None

        def query_once(self, goal):
            self.goal = goal
            return self.value

    engine = Result({"State": "ready", "Score": "4.25"})
    assert ts._expert_state_score(engine, todo, 10.9) == ("ready", 4.25)
    assert "todo_state(todo" in engine.goal
    assert ",10,State)" in engine.goal

    missing = Result({})
    assert ts._expert_state_score(missing, todo, 10) == ("unknown", 0.0)

    assert ts._epoch_term(None) == "none"
    assert ts._epoch_term("not-a-date") == "none"
    assert ts._epoch_term("1970-01-01T00:00:00") == "0"

    assert ts._prolog_atom("todo_item") == "todo_item"
    assert ts._prolog_atom("UPPER") == "'UPPER'"
    assert ts._prolog_atom("can't") == "'can''t'"


def test_load_todo_expert_fails_closed_when_constructor_raises(monkeypatch):
    import zara.prolog_engine as prolog_engine

    class BrokenEngine:
        def __init__(self, _path):
            raise RuntimeError("boom")

    monkeypatch.setattr(prolog_engine, "PrologEngine", BrokenEngine)
    monkeypatch.setattr(prolog_engine, "locate_main_pl", lambda: "/tmp/main.pl")
    assert ts._load_todo_expert() is None
