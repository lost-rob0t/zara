from __future__ import annotations

import hashlib
import stat
from pathlib import Path

import pytest


def test_desktop_org_has_named_priority_launch_modes():
    from zara.desktop.org_app import OrgLaunchMode

    assert [mode.value for mode in OrgLaunchMode] == [
        "editor",
        "todo",
        "sync",
        "notebook",
    ]


def test_org_root_is_explicit_and_never_guessed(tmp_path: Path):
    from zara.desktop.org_app import resolve_org_root

    assert resolve_org_root(None, environ={}) is None
    assert resolve_org_root(str(tmp_path), environ={}) == tmp_path.resolve()
    assert resolve_org_root(None, environ={"ZARA_ORG_ROOT": str(tmp_path)}) == tmp_path.resolve()


def test_workspace_discovers_only_org_files_under_explicit_root(tmp_path: Path):
    from zara.desktop.org_app import OrgWorkspace

    (tmp_path / "roam").mkdir()
    (tmp_path / "agenda.org").write_text("* TODO one\n", encoding="utf-8")
    (tmp_path / "roam" / "node.org").write_text("* node\n", encoding="utf-8")
    (tmp_path / "ignore.txt").write_text("nope\n", encoding="utf-8")

    workspace = OrgWorkspace(tmp_path)
    assert [doc.document_id for doc in workspace.list_documents()] == [
        "agenda.org",
        "roam/node.org",
    ]


def test_document_save_is_revision_fenced_and_source_preserving(tmp_path: Path):
    from zara.desktop.org_app import OrgWorkspace, RevisionConflict

    source = b"* TODO keep exact bytes\n"
    target = tmp_path / "todo.org"
    target.write_bytes(source)
    workspace = OrgWorkspace(tmp_path)

    snapshot = workspace.open_document("todo.org")
    assert snapshot.text == source.decode("utf-8")
    assert snapshot.revision == hashlib.sha256(source).hexdigest()

    unchanged = workspace.save_document(
        "todo.org",
        expected_revision=snapshot.revision,
        text=snapshot.text,
    )
    assert target.read_bytes() == source
    assert unchanged.revision == snapshot.revision

    target.write_text("* TODO changed elsewhere\n", encoding="utf-8")
    with pytest.raises(RevisionConflict):
        workspace.save_document(
            "todo.org",
            expected_revision=snapshot.revision,
            text="* DONE stale overwrite\n",
        )


def test_document_save_preserves_existing_file_mode(tmp_path: Path):
    from zara.desktop.org_app import OrgWorkspace

    target = tmp_path / "shared.org"
    target.write_text("* TODO before\n", encoding="utf-8")
    target.chmod(0o640)
    workspace = OrgWorkspace(tmp_path)
    snapshot = workspace.open_document("shared.org")

    workspace.save_document(
        "shared.org",
        expected_revision=snapshot.revision,
        text="* DONE after\n",
    )

    assert stat.S_IMODE(target.stat().st_mode) == 0o640


def test_desktop_editor_restores_source_line_ending_convention_before_save():
    from zara.desktop.org_app import restore_source_line_endings

    original_crlf = "* TODO one\r\nbody\r\n"
    edited_from_qt = "* DONE one\nbody\n"
    assert restore_source_line_endings(original_crlf, edited_from_qt) == (
        "* DONE one\r\nbody\r\n"
    )

    original_cr = "* TODO one\rbody\r"
    assert restore_source_line_endings(original_cr, edited_from_qt) == (
        "* DONE one\rbody\r"
    )

    original_lf = "* TODO one\nbody\n"
    assert restore_source_line_endings(original_lf, edited_from_qt) == edited_from_qt


def test_desktop_editor_preserves_mixed_line_endings_when_line_count_is_unchanged():
    from zara.desktop.org_app import restore_source_line_endings

    original = "* TODO one\r\nbody\nlast\r"
    edited_from_qt = "* DONE one\nbody changed\nlast\n"

    assert restore_source_line_endings(original, edited_from_qt) == (
        "* DONE one\r\nbody changed\nlast\r"
    )


def test_workspace_rejects_escape_paths(tmp_path: Path):
    from zara.desktop.org_app import OrgWorkspace

    workspace = OrgWorkspace(tmp_path)
    with pytest.raises(ValueError):
        workspace.open_document("../escape.org")
