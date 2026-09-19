"""Native Qt launch surfaces for Zara Org desktop products.

Ordinary Org files remain canonical. This module deliberately does not parse
Org or maintain a shadow task/index database: the first desktop slice is a
source-preserving workbench over an explicit user workspace. Structured Todo,
Sync and Notebook projections consume the canonical Org services as those
contracts land; their named launch modes already share this one shell and
workspace authority instead of growing separate desktop stacks.
"""

from __future__ import annotations

import argparse
import hashlib
import os
import stat
import sys
import tempfile
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Mapping, Sequence


class OrgLaunchMode(str, Enum):
    EDITOR = "editor"
    TODO = "todo"
    SYNC = "sync"
    NOTEBOOK = "notebook"


class RevisionConflict(RuntimeError):
    """Raised when a document changed after the caller's snapshot."""


@dataclass(frozen=True)
class OrgDocumentRef:
    document_id: str
    path: Path


@dataclass(frozen=True)
class OrgDocumentSnapshot:
    document_id: str
    text: str
    revision: str


def _revision(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def _source_line_endings(text: str) -> list[str]:
    endings: list[str] = []
    index = 0
    while index < len(text):
        char = text[index]
        if char == "\r":
            if index + 1 < len(text) and text[index + 1] == "\n":
                endings.append("\r\n")
                index += 2
                continue
            endings.append("\r")
        elif char == "\n":
            endings.append("\n")
        index += 1
    return endings


def restore_source_line_endings(original: str, edited: str) -> str:
    """Restore source newline identity after Qt normalizes paragraphs to LF.

    If the edit keeps the same line count, replay each original separator so
    even mixed-newline Org files round-trip without unrelated Git churn. If the
    line count changes, preserve a homogeneous existing convention; mixed files
    then stay normalized because there is no unambiguous separator for new or
    removed lines.
    """

    endings = _source_line_endings(original)
    normalized = edited.replace("\r\n", "\n").replace("\r", "\n")
    parts = normalized.split("\n")
    if len(parts) - 1 == len(endings):
        return "".join(
            part + (endings[index] if index < len(endings) else "")
            for index, part in enumerate(parts)
        )
    if endings and all(ending == endings[0] for ending in endings):
        return normalized.replace("\n", endings[0])
    return normalized


def resolve_org_root(
    value: str | os.PathLike[str] | None,
    *,
    environ: Mapping[str, str] | None = None,
) -> Path | None:
    """Resolve only explicitly configured roots; never guess a user layout."""

    env = os.environ if environ is None else environ
    raw = value if value is not None else env.get("ZARA_ORG_ROOT")
    if raw is None or not str(raw).strip():
        return None
    return Path(raw).expanduser().resolve()


class OrgWorkspace:
    """Filesystem adapter for one explicit desktop Org workspace.

    The adapter owns no Org semantics. It provides stable root-relative logical
    document IDs plus revision-fenced UTF-8 source reads/writes so the Qt shell
    cannot silently overwrite a file modified by Emacs, Git or another Zara
    process.
    """

    def __init__(self, root: str | os.PathLike[str]) -> None:
        resolved = Path(root).expanduser().resolve()
        if not resolved.is_dir():
            raise ValueError(f"Org workspace is not a directory: {resolved}")
        self.root = resolved

    def list_documents(self) -> list[OrgDocumentRef]:
        refs = [
            OrgDocumentRef(path.relative_to(self.root).as_posix(), path)
            for path in self.root.rglob("*")
            if path.is_file() and path.suffix.lower() == ".org"
        ]
        return sorted(refs, key=lambda item: item.document_id.casefold())

    def _path_for(self, document_id: str) -> Path:
        if not document_id or "\\" in document_id:
            raise ValueError("Org document id must be a non-empty POSIX relative path")
        relative = Path(document_id)
        if relative.is_absolute() or any(part in {"", ".", ".."} for part in relative.parts):
            raise ValueError("Org document id must stay inside the configured workspace")
        candidate = (self.root / relative).resolve()
        try:
            candidate.relative_to(self.root)
        except ValueError as exc:
            raise ValueError("Org document id escapes the configured workspace") from exc
        if candidate.suffix.lower() != ".org":
            raise ValueError("Desktop Org workbench only opens .org documents")
        return candidate

    def open_document(self, document_id: str) -> OrgDocumentSnapshot:
        path = self._path_for(document_id)
        payload = path.read_bytes()
        return OrgDocumentSnapshot(
            document_id=document_id,
            text=payload.decode("utf-8"),
            revision=_revision(payload),
        )

    def save_document(
        self,
        document_id: str,
        *,
        expected_revision: str,
        text: str,
    ) -> OrgDocumentSnapshot:
        path = self._path_for(document_id)
        current = path.read_bytes()
        current_revision = _revision(current)
        if current_revision != expected_revision:
            raise RevisionConflict(
                f"{document_id} changed since it was opened; reload before saving"
            )

        payload = text.encode("utf-8")
        if payload == current:
            return OrgDocumentSnapshot(document_id, text, current_revision)

        source_mode = stat.S_IMODE(path.stat().st_mode)
        path.parent.mkdir(parents=True, exist_ok=True)
        fd, temp_name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
        try:
            os.chmod(temp_name, source_mode)
            with os.fdopen(fd, "wb") as handle:
                handle.write(payload)
                handle.flush()
                os.fsync(handle.fileno())
            os.replace(temp_name, path)
        finally:
            try:
                os.unlink(temp_name)
            except FileNotFoundError:
                pass

        return OrgDocumentSnapshot(document_id, text, _revision(payload))


def _parser(default_mode: OrgLaunchMode) -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog=f"zara-org-{default_mode.value}")
    parser.add_argument(
        "--org-root",
        help="explicit Org workspace root; alternatively set ZARA_ORG_ROOT",
    )
    parser.add_argument("--document", help="root-relative .org document to open")
    parser.add_argument(
        "--mode",
        choices=[mode.value for mode in OrgLaunchMode],
        default=default_mode.value,
    )
    return parser


def build_org_window(mode: OrgLaunchMode, root: Path | None, document: str | None):
    """Build the real Desktop Org window without starting the Qt event loop.

    The production launcher and deterministic screenshot harness both use this
    constructor, so evidence exercises the same surface users launch.
    """

    from PySide6.QtCore import Qt
    from PySide6.QtGui import QAction
    from PySide6.QtWidgets import (
        QFileDialog,
        QLabel,
        QListWidget,
        QMainWindow,
        QMessageBox,
        QPlainTextEdit,
        QSplitter,
        QStatusBar,
        QToolBar,
    )

    class OrgDesktopWindow(QMainWindow):
        def __init__(self) -> None:
            super().__init__()
            self.mode = mode
            self.workspace: OrgWorkspace | None = None
            self.snapshot: OrgDocumentSnapshot | None = None
            self.setWindowTitle(f"Org — {mode.value.title()}")
            self.resize(1100, 760)

            self.files = QListWidget()
            self.editor = QPlainTextEdit()
            self.editor.setPlaceholderText(
                "Choose an Org workspace. Zara does not assume a default path."
            )
            splitter = QSplitter(Qt.Orientation.Horizontal)
            splitter.addWidget(self.files)
            splitter.addWidget(self.editor)
            splitter.setStretchFactor(1, 1)
            self.setCentralWidget(splitter)

            status = QStatusBar()
            self.setStatusBar(status)
            self.mode_label = QLabel(f"{mode.value.title()} · ordinary Org source")
            status.addPermanentWidget(self.mode_label)

            toolbar = QToolBar("Org")
            toolbar.setObjectName("org-toolbar")
            self.addToolBar(toolbar)
            choose = QAction("Workspace…", self)
            choose.triggered.connect(self.choose_workspace)
            toolbar.addAction(choose)
            save = QAction("Save", self)
            save.setShortcut("Ctrl+S")
            save.triggered.connect(self.save_current)
            toolbar.addAction(save)

            self.files.currentTextChanged.connect(self.open_document)
            if root is not None:
                self.set_workspace(root)
                if document is not None:
                    matches = self.files.findItems(document, Qt.MatchFlag.MatchExactly)
                    if matches:
                        self.files.setCurrentItem(matches[0])
            else:
                self.statusBar().showMessage(
                    "No Org workspace configured — choose any folder; no path is guessed."
                )

        def choose_workspace(self) -> None:
            chosen = QFileDialog.getExistingDirectory(self, "Choose Org workspace")
            if chosen:
                self.set_workspace(Path(chosen))

        def set_workspace(self, new_root: Path) -> None:
            try:
                workspace = OrgWorkspace(new_root)
            except ValueError as exc:
                QMessageBox.warning(self, "Org workspace", str(exc))
                return
            self.workspace = workspace
            self.snapshot = None
            self.editor.clear()
            self.files.clear()
            self.files.addItems([ref.document_id for ref in workspace.list_documents()])
            self.statusBar().showMessage(str(workspace.root))

        def open_document(self, document_id: str) -> None:
            if not document_id or self.workspace is None:
                return
            try:
                snapshot = self.workspace.open_document(document_id)
            except (OSError, UnicodeError, ValueError) as exc:
                QMessageBox.warning(self, "Open Org document", str(exc))
                return
            self.snapshot = snapshot
            self.editor.setPlainText(snapshot.text)
            self.editor.document().setModified(False)
            self.statusBar().showMessage(
                f"{document_id} · rev {snapshot.revision[:12]}"
            )

        def save_current(self) -> None:
            if self.workspace is None or self.snapshot is None:
                return
            if not self.editor.document().isModified():
                self.statusBar().showMessage(
                    f"{self.snapshot.document_id} · unchanged; source bytes preserved"
                )
                return
            edited = restore_source_line_endings(
                self.snapshot.text,
                self.editor.toPlainText(),
            )
            try:
                saved = self.workspace.save_document(
                    self.snapshot.document_id,
                    expected_revision=self.snapshot.revision,
                    text=edited,
                )
            except RevisionConflict as exc:
                QMessageBox.warning(self, "Stale Org edit", str(exc))
                return
            except OSError as exc:
                QMessageBox.warning(self, "Save Org document", str(exc))
                return
            self.snapshot = saved
            self.editor.document().setModified(False)
            self.statusBar().showMessage(
                f"{saved.document_id} · saved rev {saved.revision[:12]}"
            )

    return OrgDesktopWindow()


def _qt_main(mode: OrgLaunchMode, root: Path | None, document: str | None) -> int:
    from PySide6.QtWidgets import QApplication

    app = QApplication.instance() or QApplication(sys.argv[:1])
    window = build_org_window(mode, root, document)
    window.show()
    return app.exec()


def _run(default_mode: OrgLaunchMode, argv: Sequence[str] | None = None) -> int:
    args = _parser(default_mode).parse_args(list(argv) if argv is not None else None)
    mode = OrgLaunchMode(args.mode)
    root = resolve_org_root(args.org_root)
    if root is not None and not root.is_dir():
        raise SystemExit(f"Org workspace is not a directory: {root}")
    return _qt_main(mode, root, args.document)


def main(argv: Sequence[str] | None = None) -> int:
    return _run(OrgLaunchMode.EDITOR, argv)


def main_editor(argv: Sequence[str] | None = None) -> int:
    return _run(OrgLaunchMode.EDITOR, argv)


def main_todo(argv: Sequence[str] | None = None) -> int:
    return _run(OrgLaunchMode.TODO, argv)


def main_sync(argv: Sequence[str] | None = None) -> int:
    return _run(OrgLaunchMode.SYNC, argv)


def main_notebook(argv: Sequence[str] | None = None) -> int:
    return _run(OrgLaunchMode.NOTEBOOK, argv)


__all__ = [
    "OrgDocumentRef",
    "OrgDocumentSnapshot",
    "OrgLaunchMode",
    "OrgWorkspace",
    "RevisionConflict",
    "build_org_window",
    "main",
    "main_editor",
    "main_notebook",
    "main_sync",
    "main_todo",
    "resolve_org_root",
    "restore_source_line_endings",
]
