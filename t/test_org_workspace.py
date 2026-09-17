from __future__ import annotations

import concurrent.futures
import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationService, ConversationStore
from zara.desktop.org_widgets import OrgDocumentView, OrgWorkspaceWidget
from zara.desktop.windows import CopilotPresentation, CopilotWindow
from zara.org_roam import OrgRoamIndex, parse_org_text


ALPHA = """#+title: Alpha Project
* TODO Build renderer :ui:org:
:PROPERTIES:
:ID: alpha-renderer
:PROJECT: zara
:END:
Keep stars visible.
Link to [[id:beta-memory][memory]].

** NEXT Add backlinks :graph:
:PROPERTIES:
:ID: alpha-backlinks
:END:
Show backlinks.
"""

BETA = """#+title: Memory
* Facts
:PROPERTIES:
:ID: beta-memory
:END:
Back to [[id:alpha-renderer][renderer]].
"""


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    result = instance or QApplication([])
    result.setQuitOnLastWindowClosed(False)
    return result


class FakeBridge(QObject):
    runtime_event = Signal(object)
    command_completed = Signal(object)
    command_failed = Signal(str, str)

    def submit(self, command):
        future: concurrent.futures.Future = concurrent.futures.Future()
        future.set_result(None)
        return future


def index() -> OrgRoamIndex:
    return OrgRoamIndex.from_documents(
        (
            parse_org_text(ALPHA, path="/notes/alpha.org"),
            parse_org_text(BETA, path="/notes/beta.org"),
        )
    )


def test_document_view_renders_literal_stars_with_rich_heading_sizes():
    qt_app = app()
    document = next(document for document in index().documents if document.title == "Alpha Project")
    view = OrgDocumentView(base_font_pt=12.0)
    view.set_document(document)
    qt_app.processEvents()

    plain = view.toPlainText()
    html = view.toHtml()
    assert "* TODO Build renderer" in plain
    assert "** NEXT Add backlinks" in plain
    assert "font-size" in html
    assert "Keep stars visible." in plain

    view.deleteLater()
    qt_app.processEvents()


def test_workspace_search_selection_and_backlinks_use_one_index():
    qt_app = app()
    workspace = OrgWorkspaceWidget(index())
    workspace.resize(520, 640)
    workspace.show()
    qt_app.processEvents()

    workspace.search_edit.setText("Doom renderer")
    qt_app.processEvents()
    assert workspace.node_list.count() == 1
    assert "Build renderer" in workspace.node_list.item(0).text()

    workspace.select_node("alpha-renderer")
    qt_app.processEvents()
    assert workspace.current_node_key == "alpha-renderer"
    assert "Build renderer" in workspace.document_view.toPlainText()
    assert workspace.backlinks_list.count() == 1
    assert "Facts" in workspace.backlinks_list.item(0).text()

    workspace.close()
    workspace.deleteLater()
    qt_app.processEvents()


def test_expanded_copilot_toggles_org_workspace_without_creating_second_chat_renderer(tmp_path):
    qt_app = app()
    service = ConversationService(
        ConversationStore(DatabaseManager(tmp_path / "org-copilot.db"))
    )
    window = CopilotWindow(FakeBridge(), service, org_index=index())  # type: ignore[arg-type]
    identity = id(window)
    try:
        window.set_presentation(CopilotPresentation.EXPANDED)
        window.resize(1180, 760)
        window.show()
        qt_app.processEvents()

        assert window.org_workspace.isHidden()
        window.org_button.click()
        qt_app.processEvents()
        assert not window.org_workspace.isHidden()
        assert id(window) == identity

        window.org_workspace.select_node("alpha-renderer")
        assert "* TODO Build renderer" in window.org_workspace.document_view.toPlainText()

        window.set_presentation(CopilotPresentation.COMPACT)
        qt_app.processEvents()
        assert window.org_workspace.isHidden()
        assert id(window) == identity
    finally:
        window.prepare_for_quit()
        window.close()
        window.deleteLater()
        qt_app.processEvents()
