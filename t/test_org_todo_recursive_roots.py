from __future__ import annotations

from zara.org_todos import OrgTodoStore


def test_org_todo_store_discovers_and_updates_nested_org_files(tmp_path):
    root = tmp_path / "workspace"
    nested = root / "projects" / "client"
    nested.mkdir(parents=True)
    task_file = nested / "tasks.org"
    task_file.write_text(
        """#+title: Client tasks
#+todo: TODO(t) | DONE(d!)

* TODO Nested task
:PROPERTIES:
:ID: nested-task
:END:
Keep this file in its existing project directory.
""",
        encoding="utf-8",
    )

    store = OrgTodoStore(root)

    listed = store.list_todos()
    assert [todo.id for todo in listed] == ["nested-task"]
    assert store.get_todo("nested-task") is not None

    store.update_todo("nested-task", status="DONE")

    assert task_file.exists()
    assert not (root / "tasks.org").exists()
    text = task_file.read_text(encoding="utf-8")
    assert "* DONE Nested task" in text
    assert "Keep this file in its existing project directory." in text
