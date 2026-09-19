from __future__ import annotations

from zara.org_todos import OrgTodoStore


def test_parent_edit_does_not_consume_child_metadata(tmp_path):
    agenda = tmp_path / "agenda"
    agenda.mkdir()
    path = agenda / "nested.org"
    path.write_text(
        """#+todo: TODO(t) STRT(s!) | DONE(d!)

* TODO Parent
:PROPERTIES:
:ID: parent-task
:END:
SCHEDULED: <2026-09-17 Thu 18:00>
Parent note.

** STRT Child
:PROPERTIES:
:ID: child-task
:EFFORT: 0:45
:END:
SCHEDULED: <2026-09-17 Thu 20:00>
Child note.
""",
        encoding="utf-8",
    )

    store = OrgTodoStore(agenda)
    parent = store.get_todo("parent-task")
    child = store.get_todo("child-task")

    assert parent is not None
    assert child is not None
    assert parent.scheduled_at == "2026-09-17T18:00:00"
    assert child.scheduled_at == "2026-09-17T20:00:00"
    assert child.duration_minutes == 45

    store.update_todo("parent-task", title="Renamed parent")

    text = path.read_text(encoding="utf-8")
    assert "* TODO Renamed parent" in text
    assert "** STRT Child" in text
    assert ":ID: child-task" in text
    assert ":EFFORT: 0:45" in text
    assert "SCHEDULED: <2026-09-17 Thu 20:00>" in text
    assert "Child note." in text

    reparsed_child = store.get_todo("child-task")
    assert reparsed_child is not None
    assert reparsed_child.scheduled_at == "2026-09-17T20:00:00"
    assert reparsed_child.duration_minutes == 45
