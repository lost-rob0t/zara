"""Focused native desktop launch surfaces for the canonical Org workspace.

These windows are projections over Zara's existing Org configuration, parser,
index and rendering authority. Ordinary Org files remain canonical; this module
owns no private note/task store and deliberately has no implicit filesystem
root.
"""

from __future__ import annotations

import sys
from enum import Enum
from typing import Optional, Sequence

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QApplication,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QMainWindow,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

from zara.config import ZaraConfig, get_config
from zara.desktop.org_widgets import OrgDocumentView, OrgWorkspaceWidget
from zara.desktop.theme import apply_desktop_theme
from zara.org_browser import OrgBrowserRuntime, build_org_browser_runtime
from zara.org_browser_runtime import ConfiguredOrgRoamWorkspace
from zara.org_roam import OrgNode, OrgRoamIndex


class OrgDesktopSurface(str, Enum):
    """Named desktop products that are implemented by this module."""

    EDITOR = "editor"
    TODO = "todo"


_SURFACE_TITLES = {
    OrgDesktopSurface.EDITOR: "Org Editor",
    OrgDesktopSurface.TODO: "Org Todo",
}


def _configured_org_state(
    config: Optional[ZaraConfig] = None,
) -> tuple[OrgBrowserRuntime, OrgRoamIndex]:
    """Resolve the configured workspace without inventing a fallback root."""
    runtime = build_org_browser_runtime(config or get_config())
    if not runtime.config.enabled or not runtime.config.roots:
        return runtime, OrgRoamIndex.empty()
    workspace = ConfiguredOrgRoamWorkspace(runtime.config, runtime.hooks)
    return runtime, workspace.refresh(force=True).index


class OrgTodoWidget(QWidget):
    """Focused task projection over the same canonical Org index as Editor."""

    NODE_KEY_ROLE = Qt.ItemDataRole.UserRole

    def __init__(
        self,
        index: OrgRoamIndex,
        *,
        runtime: OrgBrowserRuntime,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.setObjectName("zaraOrgTodo")
        self.index = index
        self.runtime = runtime
        self.current_node_key: Optional[str] = None

        self.search_edit = QLineEdit(self)
        self.search_edit.setObjectName("zaraOrgTodoSearch")
        self.search_edit.setPlaceholderText("Search Org tasks")
        self.search_edit.setAccessibleName("Search Org tasks")

        self.todo_list = QListWidget(self)
        self.todo_list.setObjectName("zaraOrgTodoList")
        self.todo_list.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.todo_list.setAccessibleName("Org tasks")

        list_panel = QWidget(self)
        list_layout = QVBoxLayout(list_panel)
        list_layout.setContentsMargins(0, 0, 0, 0)
        list_layout.addWidget(QLabel("Tasks"))
        list_layout.addWidget(self.search_edit)
        list_layout.addWidget(self.todo_list, 1)

        self.document_view = OrgDocumentView(
            config=runtime.config,
            hooks=runtime.hooks,
            parent=self,
        )
        self.document_view.setAccessibleName("Org task source preview")

        splitter = QSplitter(Qt.Orientation.Horizontal, self)
        splitter.addWidget(list_panel)
        splitter.addWidget(self.document_view)
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)
        splitter.setSizes([360, 820])

        layout = QVBoxLayout(self)
        layout.setContentsMargins(12, 12, 12, 12)
        layout.addWidget(splitter, 1)

        self.search_edit.textChanged.connect(self.refresh_nodes)
        self.todo_list.itemActivated.connect(self._activate_item)
        self.todo_list.currentItemChanged.connect(self._current_item_changed)
        self.refresh_nodes()

    def set_index(self, index: OrgRoamIndex) -> None:
        self.index = index
        if self.current_node_key and index.get(self.current_node_key) is None:
            self.current_node_key = None
            self.document_view.clear()
        self.refresh_nodes()

    def _task_nodes(self, query: str) -> tuple[OrgNode, ...]:
        if query.strip():
            rows = self.index.search(
                query,
                limit=self.runtime.config.search_limit,
            )
        else:
            rows = self.index.nodes[: self.runtime.config.search_limit]
        tasks = tuple(node for node in rows if node.todo)
        tasks = self.runtime.hooks.filter_nodes(tasks, self.runtime.config)
        return self.runtime.hooks.sort_nodes(tasks, self.runtime.config)

    def refresh_nodes(self, query: Optional[str] = None) -> None:
        if query is None:
            query = self.search_edit.text()
        rows = self._task_nodes(query)

        self.todo_list.blockSignals(True)
        self.todo_list.clear()
        selected_item: Optional[QListWidgetItem] = None
        for node in rows:
            item = QListWidgetItem(f"{node.todo} {node.title}".strip())
            item.setData(self.NODE_KEY_ROLE, node.key)
            item.setToolTip(node.file_path)
            self.todo_list.addItem(item)
            if node.key == self.current_node_key:
                selected_item = item
        if selected_item is not None:
            self.todo_list.setCurrentItem(selected_item)
        self.todo_list.blockSignals(False)

    def select_node(self, key_or_id: str) -> None:
        node = self.index.get(key_or_id)
        if node is None or not node.todo:
            return
        document = next(
            (
                candidate
                for candidate in self.index.documents
                if candidate.path == node.file_path
            ),
            None,
        )
        if document is None:
            return
        self.current_node_key = node.key
        self.document_view.set_document(document)
        for row in range(self.todo_list.count()):
            item = self.todo_list.item(row)
            if str(item.data(self.NODE_KEY_ROLE)) == node.key:
                self.todo_list.setCurrentItem(item)
                break

    def _activate_item(self, item: QListWidgetItem) -> None:
        key = item.data(self.NODE_KEY_ROLE)
        if key:
            self.select_node(str(key))

    def _current_item_changed(
        self,
        current: Optional[QListWidgetItem],
        _previous: Optional[QListWidgetItem],
    ) -> None:
        if current is None:
            return
        key = current.data(self.NODE_KEY_ROLE)
        if key and str(key) != self.current_node_key:
            self.select_node(str(key))


class OrgDesktopWindow(QMainWindow):
    """First-class native window for one focused Org desktop product."""

    def __init__(
        self,
        surface: OrgDesktopSurface | str,
        *,
        org_index: OrgRoamIndex,
        org_runtime: OrgBrowserRuntime,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.surface = OrgDesktopSurface(surface)
        self.org_index = org_index
        self.org_runtime = org_runtime
        self.setObjectName(f"zaraOrgDesktop-{self.surface.value}")
        self.setWindowTitle(_SURFACE_TITLES[self.surface])
        self.resize(1180, 760)

        if self.surface is OrgDesktopSurface.EDITOR:
            content: QWidget = OrgWorkspaceWidget(
                org_index,
                config=org_runtime.config,
                hooks=org_runtime.hooks,
                parent=self,
            )
        elif self.surface is OrgDesktopSurface.TODO:
            content = OrgTodoWidget(
                org_index,
                runtime=org_runtime,
                parent=self,
            )
        else:  # pragma: no cover - Enum conversion above is exhaustive
            raise ValueError(f"unsupported Org desktop surface: {self.surface}")
        self.setCentralWidget(content)


def create_org_application(
    argv: Optional[Sequence[str]] = None,
    *,
    surface: OrgDesktopSurface | str = OrgDesktopSurface.EDITOR,
    config: Optional[ZaraConfig] = None,
    org_index: Optional[OrgRoamIndex] = None,
    org_runtime: Optional[OrgBrowserRuntime] = None,
) -> tuple[QApplication, OrgDesktopWindow]:
    """Create one focused Org window over the shared configured workspace."""
    if (org_index is None) != (org_runtime is None):
        raise ValueError("org_index and org_runtime must be supplied together")

    instance = QApplication.instance()
    if instance is None:
        app = QApplication(list(argv) if argv is not None else sys.argv)
    elif isinstance(instance, QApplication):
        app = instance
    else:  # pragma: no cover - defensive for non-GUI embedders
        raise RuntimeError("Zara Org Desktop requires QApplication")

    active_config = config or get_config()
    app.setApplicationName(_SURFACE_TITLES[OrgDesktopSurface(surface)])
    app.setOrganizationName("Zara")
    apply_desktop_theme(app, str(active_config.get("desktop", "theme", "signal-cabin")))

    if org_index is None or org_runtime is None:
        org_runtime, org_index = _configured_org_state(active_config)

    window = OrgDesktopWindow(
        surface,
        org_index=org_index,
        org_runtime=org_runtime,
    )
    window.show()
    return app, window


def run_surface(
    surface: OrgDesktopSurface | str,
    argv: Optional[Sequence[str]] = None,
) -> int:
    app, window = create_org_application(argv, surface=surface)
    # Keep an explicit Python reference for embedders and static analyzers; Qt
    # owns the visible object for the duration of the event loop.
    setattr(app, "_zara_org_desktop_window", window)
    return int(app.exec())


def main(argv: Optional[Sequence[str]] = None) -> int:
    """Launch the first-class Org Editor/workbench desktop product."""
    return run_surface(OrgDesktopSurface.EDITOR, argv)


def main_editor(argv: Optional[Sequence[str]] = None) -> int:
    """Launch the named Org Editor desktop product."""
    return run_surface(OrgDesktopSurface.EDITOR, argv)


def main_todo(argv: Optional[Sequence[str]] = None) -> int:
    """Launch the named Org Todo desktop product."""
    return run_surface(OrgDesktopSurface.TODO, argv)


__all__ = [
    "OrgDesktopSurface",
    "OrgDesktopWindow",
    "OrgTodoWidget",
    "create_org_application",
    "main",
    "main_editor",
    "main_todo",
    "run_surface",
]
