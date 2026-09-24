from __future__ import annotations

from pathlib import Path

from zara.org_todos import OrgTodoStore, resolve_org_todo_root
from zara.todo_storage import CLEAR, PRESERVE


def _write_agenda(root: Path) -> Path:
    root.mkdir(parents=True)
    path = root / "zara.org"
    path.write_text(
        """#+title: Zara
#+todo: TODO(t) STRT(s!) WAIT(w@/!) HOLD(h@/!) IDEA(i) LOOP(l!) | DONE(d!) KILL(k@/!)

* Current Work
Project prose must survive rewrites.

** STRT [#A] Build Org watch face :wearos:todo:
:PROPERTIES:
:ID:       5021ae0b-6b2f-4dc2-92c9-cc79b8ed1ed6
:OWNER:    user
:REPO:     lost-rob0t/zara
:EFFORT:   1:30
:END:
SCHEDULED: <2026-09-17 Thu 18:00 +1w>
DEADLINE: <2026-10-01 Thu 23:59>
Keep this note.

** TODO Unrelated task :safe:
:PROPERTIES:
:ID:       9f34af61-9d7d-4b9a-9686-93d616c70e45
:OWNER:    user
:END:
Do not rewrite this block.
""",
        encoding="utf-8",
    )
    return path


def test_parses_gpt_todos_org_schema(tmp_path):
    agenda = tmp_path / "agenda"
    _write_agenda(agenda)

    store = OrgTodoStore(agenda)
    todos = store.list_todos(include_done=True)

    assert len(todos) == 2
    task = todos[0]
    assert task.id == "5021ae0b-6b2f-4dc2-92c9-cc79b8ed1ed6"
    assert task.status == "STRT"
    assert task.title == "Build Org watch face"
    assert task.priority == "A"
    assert task.scheduled_at == "2026-09-17T18:00:00"
    assert task.deadline_at == "2026-10-01T23:59:00"
    assert task.repeater == "+1w"
    assert task.duration_minutes == 90
    assert store.tags_for(task.id) == ["todo", "wearos"]


def test_update_round_trips_without_destroying_gpt_todos_metadata(tmp_path):
    agenda = tmp_path / "agenda"
    path = _write_agenda(agenda)
    store = OrgTodoStore(agenda)
    todo_id = "5021ae0b-6b2f-4dc2-92c9-cc79b8ed1ed6"

    store.update_todo(
        todo_id,
        title="Ship Org watch face",
        priority=PRESERVE,
        scheduled_at="2026-09-18T19:15:00",
        duration_minutes=45,
        tags=["wearos", "org"],
    )

    text = path.read_text(encoding="utf-8")
    assert "Project prose must survive rewrites." in text
    assert ":OWNER:    user" in text
    assert ":REPO:     lost-rob0t/zara" in text
    assert ":ID:       5021ae0b-6b2f-4dc2-92c9-cc79b8ed1ed6" in text
    assert "** STRT [#A] Ship Org watch face :org:wearos:" in text
    assert ":EFFORT:   0:45" in text
    assert "SCHEDULED: <2026-09-18 Fri 19:15 +1w>" in text
    assert "DEADLINE: <2026-10-01 Thu 23:59>" in text
    assert "Keep this note." in text
    assert "** TODO Unrelated task :safe:" in text
    assert "Do not rewrite this block." in text


def test_clear_schedule_does_not_clear_deadline(tmp_path):
    agenda = tmp_path / "agenda"
    path = _write_agenda(agenda)
    store = OrgTodoStore(agenda)
    todo_id = "5021ae0b-6b2f-4dc2-92c9-cc79b8ed1ed6"

    store.update_todo(todo_id, scheduled_at=CLEAR, repeater=CLEAR)

    text = path.read_text(encoding="utf-8")
    assert "SCHEDULED:" not in text
    assert "DEADLINE: <2026-10-01 Thu 23:59>" in text


def test_capture_writes_org_effort_schedule_repeater_and_stable_id(tmp_path):
    agenda = tmp_path / "agenda"
    store = OrgTodoStore(agenda)

    todo_id = store.add_todo(
        "Write docs",
        "TODO",
        priority="B",
        scheduled_at="2026-09-20T19:30:00",
        repeater="+1w",
        duration_minutes=75,
        tags=["docs", "zara"],
        notes="Canonical notes.",
    )

    assert isinstance(todo_id, str)
    assert len(todo_id) == 36
    text = (agenda / "inbox.org").read_text(encoding="utf-8")
    assert f":ID:       {todo_id}" in text
    assert "* TODO [#B] Write docs :docs:zara:" in text
    assert ":EFFORT:   1:15" in text
    assert "SCHEDULED: <2026-09-20 Sun 19:30 +1w>" in text
    assert "DEADLINE:" not in text
    assert "Canonical notes." in text

    reparsed = store.get_todo(todo_id)
    assert reparsed is not None
    assert reparsed.scheduled_at == "2026-09-20T19:30:00"
    assert reparsed.repeater == "+1w"
    assert reparsed.duration_minutes == 75


def test_schedule_conflicts_include_org_repeaters(tmp_path):
    agenda = tmp_path / "agenda"
    store = OrgTodoStore(agenda)
    todo_id = store.add_todo(
        "Weekly block",
        "LOOP",
        scheduled_at="2026-09-20T19:00:00",
        repeater="+1w",
        duration_minutes=60,
    )

    assert store.schedule_conflicts("2026-09-27T19:30:00", 15)
    assert not store.schedule_conflicts("2026-09-27T20:00:00", 15)
    assert not store.schedule_conflicts(
        "2026-09-27T19:30:00",
        15,
        exclude_todo_id=todo_id,
    )


def test_empty_config_does_not_autodiscover_operator_checkout(tmp_path):
    home = tmp_path / "home"
    operator_agenda = home / "Documents" / "gpt-todos" / "agenda"
    operator_agenda.mkdir(parents=True)

    assert resolve_org_todo_root({}, home=home) == (
        home / ".local" / "share" / "zarathushtra" / "todos.org"
    )


def test_explicit_gpt_todos_repo_remains_supported(tmp_path):
    home = tmp_path / "home"
    checkout = tmp_path / "gpt-todos"

    assert resolve_org_todo_root(
        {"gpt_todos_repo": str(checkout)},
        home=home,
    ) == checkout / "agenda"


def test_configured_org_root_beats_compatibility_repo(tmp_path):
    home = tmp_path / "home"
    configured = tmp_path / "custom" / "agenda"
    checkout = tmp_path / "gpt-todos"

    assert resolve_org_todo_root(
        {
            "org_root": str(configured),
            "gpt_todos_repo": str(checkout),
        },
        home=home,
    ) == configured
