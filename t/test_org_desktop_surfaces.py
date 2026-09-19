from __future__ import annotations

import os
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtWidgets import QApplication

from zara.desktop.org_app import (
    OrgDesktopSurface,
    OrgDesktopWindow,
    OrgTodoWidget,
    main,
    main_editor,
    main_todo,
)
from zara.desktop.org_widgets import OrgWorkspaceWidget
from zara.org_browser import OrgBrowserConfig, OrgBrowserHookRegistry, OrgBrowserRuntime
from zara.org_roam import OrgRoamIndex, parse_org_text


WORKSPACE = """#+title: Desktop parity
* TODO Ship desktop Org Todo :org:desktop:
:PROPERTIES:
:ID: desktop-todo
:END:
Keep ordinary Org canonical.

* Notes
:PROPERTIES:
:ID: desktop-notes
:END:
This is not a task.

* NEXT Wire the focused launcher :org:
:PROPERTIES:
:ID: desktop-next
:END:
Reuse the same index.
"""


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    result = instance or QApplication([])
    result.setQuitOnLastWindowClosed(False)
    return result


def index() -> OrgRoamIndex:
    return OrgRoamIndex.from_documents(
        (parse_org_text(WORKSPACE, path="/arbitrary/workspace/project.org"),)
    )


def runtime() -> OrgBrowserRuntime:
    return OrgBrowserRuntime(
        OrgBrowserConfig(roots=("/arbitrary/workspace",)),
        OrgBrowserHookRegistry(),
    )


def test_named_desktop_surfaces_are_stable_and_do_not_claim_unimplemented_apps():
    assert OrgDesktopSurface.EDITOR.value == "editor"
    assert OrgDesktopSurface.TODO.value == "todo"
    assert {surface.value for surface in OrgDesktopSurface} == {"editor", "todo"}


def test_editor_launch_surface_reuses_the_canonical_org_workspace_projection():
    qt_app = app()
    window = OrgDesktopWindow(
        OrgDesktopSurface.EDITOR,
        org_index=index(),
        org_runtime=runtime(),
    )
    try:
        window.show()
        qt_app.processEvents()
        assert window.windowTitle() == "Org Editor"
        assert isinstance(window.centralWidget(), OrgWorkspaceWidget)
        assert window.centralWidget().index.get("desktop-todo") is not None
    finally:
        window.close()
        window.deleteLater()
        qt_app.processEvents()


def test_todo_launch_surface_filters_real_todo_nodes_and_previews_same_document():
    qt_app = app()
    window = OrgDesktopWindow(
        OrgDesktopSurface.TODO,
        org_index=index(),
        org_runtime=runtime(),
    )
    try:
        window.show()
        qt_app.processEvents()
        assert window.windowTitle() == "Org Todo"
        todo = window.centralWidget()
        assert isinstance(todo, OrgTodoWidget)
        assert todo.todo_list.count() == 2
        assert "TODO Ship desktop Org Todo" in todo.todo_list.item(0).text()
        assert "NEXT Wire the focused launcher" in todo.todo_list.item(1).text()

        todo.select_node("desktop-todo")
        qt_app.processEvents()
        assert "* TODO Ship desktop Org Todo" in todo.document_view.toPlainText()

        todo.search_edit.setText("launcher")
        qt_app.processEvents()
        assert todo.todo_list.count() == 1
        assert "desktop-next" == todo.todo_list.item(0).data(todo.NODE_KEY_ROLE)
    finally:
        window.close()
        window.deleteLater()
        qt_app.processEvents()


def test_desktop_surface_module_has_no_operator_specific_org_default():
    source = Path(__file__).resolve().parents[1] / "zara" / "desktop" / "org_app.py"
    text = source.read_text(encoding="utf-8")
    assert "Documents/Notes/org" not in text
    assert "~/Documents" not in text
    assert "org_browser_runtime.ConfiguredOrgRoamWorkspace" not in text


def test_console_entrypoint_targets_are_real_callables():
    assert callable(main)
    assert callable(main_editor)
    assert callable(main_todo)
