"""Native Qt Org/Org-roam views shared by Copilot workspace and Help."""

from __future__ import annotations

from pathlib import Path
from typing import Optional

from PySide6.QtCore import Qt, QUrl, Signal
from PySide6.QtWidgets import (
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QSplitter,
    QTextBrowser,
    QVBoxLayout,
    QWidget,
)

from zara.org_roam import OrgDocument, OrgRoamIndex, parse_org_file, render_org_html


class OrgDocumentView(QTextBrowser):
    """Source-preserving Org renderer with Doom-like heading typography."""

    org_link_activated = Signal(str)

    def __init__(self, *, base_font_pt: float = 12.0, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.base_font_pt = float(base_font_pt)
        self.document_model: Optional[OrgDocument] = None
        self.setObjectName("zaraOrgDocumentView")
        self.setOpenLinks(False)
        self.setOpenExternalLinks(False)
        self.setReadOnly(True)
        self.anchorClicked.connect(self._on_anchor_clicked)

    def set_document(self, document: OrgDocument) -> None:
        self.document_model = document
        self.setHtml(render_org_html(document, base_font_pt=self.base_font_pt))
        self.moveCursor(self.textCursor().MoveOperation.Start)

    def _on_anchor_clicked(self, url: QUrl) -> None:
        self.org_link_activated.emit(url.toString())


class OrgWorkspaceWidget(QWidget):
    """Searchable Org-roam graph projection beside the canonical Copilot chat."""

    node_selected = Signal(str)

    def __init__(self, index: OrgRoamIndex, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setObjectName("zaraOrgWorkspace")
        self.index = index
        self.current_node_key: Optional[str] = None

        self.search_edit = QLineEdit()
        self.search_edit.setObjectName("zaraOrgSearch")
        self.search_edit.setPlaceholderText("Search Org / roam")

        self.node_list = QListWidget()
        self.node_list.setObjectName("zaraOrgNodeList")
        self.node_list.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        left = QWidget()
        left_layout = QVBoxLayout(left)
        left_layout.setContentsMargins(0, 0, 0, 0)
        left_layout.addWidget(QLabel("Org-roam"))
        left_layout.addWidget(self.search_edit)
        left_layout.addWidget(self.node_list, 1)

        self.document_view = OrgDocumentView()
        self.backlinks_list = QListWidget()
        self.backlinks_list.setObjectName("zaraOrgBacklinks")
        self.backlinks_list.setMaximumHeight(150)

        right = QWidget()
        right_layout = QVBoxLayout(right)
        right_layout.setContentsMargins(0, 0, 0, 0)
        right_layout.addWidget(self.document_view, 1)
        right_layout.addWidget(QLabel("Backlinks"))
        right_layout.addWidget(self.backlinks_list)

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(left)
        splitter.addWidget(right)
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)
        splitter.setSizes([260, 700])

        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(splitter)

        self.search_edit.textChanged.connect(self.refresh_nodes)
        self.node_list.itemActivated.connect(self._activate_node_item)
        self.backlinks_list.itemActivated.connect(self._activate_backlink_item)
        self.document_view.org_link_activated.connect(self._activate_org_link)
        self.refresh_nodes()

    def set_index(self, index: OrgRoamIndex) -> None:
        self.index = index
        if self.current_node_key and index.get(self.current_node_key) is None:
            self.current_node_key = None
            self.document_view.clear()
            self.backlinks_list.clear()
        self.refresh_nodes()

    def refresh_nodes(self, query: Optional[str] = None) -> None:
        if query is None:
            query = self.search_edit.text()
        rows = self.index.search(query, limit=200) if query.strip() else self.index.nodes[:200]
        self.node_list.blockSignals(True)
        self.node_list.clear()
        for node in rows:
            stars = "*" * max(1, node.level)
            todo = f"{node.todo} " if node.todo else ""
            item = QListWidgetItem(f"{stars} {todo}{node.title}".strip())
            item.setData(Qt.ItemDataRole.UserRole, node.key)
            item.setToolTip(node.file_path)
            self.node_list.addItem(item)
            if node.key == self.current_node_key:
                self.node_list.setCurrentItem(item)
        self.node_list.blockSignals(False)

    def select_node(self, key_or_id: str) -> None:
        node = self.index.get(key_or_id)
        if node is None:
            return
        self.current_node_key = node.key
        document = next(
            (document for document in self.index.documents if document.path == node.file_path),
            None,
        )
        if document is not None:
            self.document_view.set_document(document)
        self.backlinks_list.clear()
        for backlink in self.index.backlinks(node.key):
            item = QListWidgetItem(backlink.title)
            item.setData(Qt.ItemDataRole.UserRole, backlink.key)
            self.backlinks_list.addItem(item)
        self.refresh_nodes()
        self.node_selected.emit(node.key)

    def _activate_node_item(self, item: QListWidgetItem) -> None:
        key = item.data(Qt.ItemDataRole.UserRole)
        if key:
            self.select_node(str(key))

    def _activate_backlink_item(self, item: QListWidgetItem) -> None:
        key = item.data(Qt.ItemDataRole.UserRole)
        if key:
            self.select_node(str(key))

    def _activate_org_link(self, target: str) -> None:
        if target.startswith("id:"):
            self.select_node(target[3:])


class OrgHelpWindow(QWidget):
    """Render repository-owned Org help sources instead of duplicated UI copy."""

    def __init__(self, *, repo_root: Optional[Path] = None, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setObjectName("zaraOrgHelpWindow")
        self.setWindowTitle("Zara Help — Org")
        self.resize(980, 720)
        self.repo_root = (repo_root or Path(__file__).resolve().parents[2]).resolve()
        self.current_path: Optional[Path] = None

        self.source_list = QListWidget()
        self.source_list.setObjectName("zaraOrgHelpSources")
        self.source_list.setMinimumWidth(240)
        self.source_list.setMaximumWidth(360)
        self.document_view = OrgDocumentView(base_font_pt=12.0)

        layout = QHBoxLayout(self)
        layout.setContentsMargins(12, 12, 12, 12)
        layout.addWidget(self.source_list)
        layout.addWidget(self.document_view, 1)

        self.source_list.itemActivated.connect(self._activate_source_item)
        self.document_view.org_link_activated.connect(self._activate_org_link)
        self._populate_sources()
        if self.source_list.count():
            first = self.source_list.item(0)
            self.open_source(str(first.data(Qt.ItemDataRole.UserRole)))
            self.source_list.setCurrentItem(first)

    def _source_paths(self) -> tuple[Path, ...]:
        rows: list[Path] = []
        readme = self.repo_root / "README.org"
        if readme.is_file():
            rows.append(readme)
        for directory in (self.repo_root / "docs", self.repo_root / "wiki"):
            if directory.is_dir():
                rows.extend(path for path in directory.rglob("*.org") if path.is_file())
        return tuple(sorted(set(rows), key=lambda path: str(path.relative_to(self.repo_root))))

    def _populate_sources(self) -> None:
        self.source_list.clear()
        for path in self._source_paths():
            relative = str(path.relative_to(self.repo_root))
            item = QListWidgetItem(relative)
            item.setData(Qt.ItemDataRole.UserRole, relative)
            self.source_list.addItem(item)

    def open_source(self, relative: str) -> None:
        candidate = (self.repo_root / relative).resolve()
        if self.repo_root not in candidate.parents and candidate != self.repo_root:
            return
        if candidate.suffix.casefold() != ".org" or not candidate.is_file():
            return
        self.current_path = candidate
        self.document_view.set_document(parse_org_file(candidate))

    def _activate_source_item(self, item: QListWidgetItem) -> None:
        relative = item.data(Qt.ItemDataRole.UserRole)
        if relative:
            self.open_source(str(relative))

    def _activate_org_link(self, target: str) -> None:
        if self.current_path is None or not target.startswith("file:"):
            return
        raw = target[5:].split("::", 1)[0]
        candidate = (self.current_path.parent / raw).resolve()
        if self.repo_root not in candidate.parents and candidate != self.repo_root:
            return
        if candidate.suffix.casefold() != ".org" or not candidate.is_file():
            return
        self.open_source(str(candidate.relative_to(self.repo_root)))
        for index in range(self.source_list.count()):
            item = self.source_list.item(index)
            if item.data(Qt.ItemDataRole.UserRole) == str(candidate.relative_to(self.repo_root)):
                self.source_list.setCurrentItem(item)
                break


__all__ = ["OrgDocumentView", "OrgWorkspaceWidget", "OrgHelpWindow"]
