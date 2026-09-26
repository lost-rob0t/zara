"""Deterministic screenshot fixtures for the native Desktop Org surfaces."""

from __future__ import annotations

import hashlib
import json
import tempfile
from pathlib import Path

from PySide6.QtGui import QPalette
from PySide6.QtWidgets import QApplication

from zara.desktop.org_app import OrgLaunchMode, build_org_window
from zara.desktop.org_evidence import validate_org_evidence
from zara.desktop.theme import apply_desktop_theme

_THEME = "signal-cabin"
_SIZE = (1100, 760)
_FIXTURES: tuple[tuple[OrgLaunchMode, str, str], ...] = (
    (OrgLaunchMode.EDITOR, "notes.org", "org-editor.png"),
    (OrgLaunchMode.TODO, "agenda.org", "org-todo.png"),
    (OrgLaunchMode.SYNC, "sync.org", "org-sync.png"),
    (OrgLaunchMode.NOTEBOOK, "notebook.org", "org-notebook.png"),
)


def _application() -> QApplication:
    instance = QApplication.instance()
    if instance is not None:
        if not isinstance(instance, QApplication):
            raise RuntimeError("Qt application is not a QApplication")
        return instance
    app = QApplication([])
    app.setQuitOnLastWindowClosed(False)
    return app


def _seed_workspace(root: Path) -> None:
    (root / "notes.org").write_text(
        "#+title: Zara Org Desktop\n\n* Editor\nOrdinary Org text stays canonical.\n",
        encoding="utf-8",
    )
    (root / "agenda.org").write_text(
        "#+TODO: TODO NEXT | DONE\n\n* TODO Ship desktop parity\n* NEXT Verify exact-head evidence\n",
        encoding="utf-8",
    )
    (root / "sync.org").write_text(
        "#+title: Sync workspace\n\n* Git\nThe configured workspace remains the source of truth.\n",
        encoding="utf-8",
    )
    (root / "notebook.org").write_text(
        "#+title: Notebook\n\n* Runnable notes\n#+begin_src python\nprint('zara org')\n#+end_src\n",
        encoding="utf-8",
    )


def _render_one(
    output_dir: Path,
    root: Path,
    mode: OrgLaunchMode,
    document: str,
    filename: str,
    *,
    source_commit: str,
) -> dict[str, object]:
    app = _application()
    window = build_org_window(mode, root, document)
    window.resize(*_SIZE)
    try:
        window.show()
        app.processEvents()
        pixmap = window.grab()
        if pixmap.isNull():
            raise RuntimeError(f"failed to render Org fixture: {mode.value}")
        target = output_dir / filename
        if not pixmap.save(str(target), "PNG"):
            raise RuntimeError(f"failed to save Org fixture: {target}")
        screenshot_sha256 = hashlib.sha256(target.read_bytes()).hexdigest()
        return {
            "mode": mode.value,
            "path": filename,
            "width": pixmap.width(),
            "height": pixmap.height(),
            "theme": _THEME,
            "source_commit": source_commit,
            "sha256": screenshot_sha256,
        }
    finally:
        window.close()
        window.deleteLater()
        app.processEvents()


def render_org_fixtures(output_dir: Path | str, *, source_commit: str) -> dict[str, object]:
    """Render and integrity-check the exact-head Desktop Org evidence set."""

    target = Path(output_dir)
    target.mkdir(parents=True, exist_ok=True)
    app = _application()
    previous_palette = QPalette(app.palette())
    previous_stylesheet = app.styleSheet()
    app.setStyleSheet("")
    previous_style_name = app.style().objectName()
    app.setStyleSheet(previous_stylesheet)
    previous_theme = app.property("zaraTheme")
    apply_desktop_theme(app, _THEME)

    try:
        with tempfile.TemporaryDirectory(prefix="zara-org-fixtures-") as temp_dir:
            root = Path(temp_dir)
            _seed_workspace(root)
            fixtures = [
                _render_one(
                    target,
                    root,
                    mode,
                    document,
                    filename,
                    source_commit=source_commit,
                )
                for mode, document, filename in _FIXTURES
            ]
    finally:
        app.setStyle(previous_style_name)
        app.setPalette(previous_palette)
        app.setStyleSheet(previous_stylesheet)
        app.setProperty("zaraTheme", previous_theme)

    manifest: dict[str, object] = {
        "schema": 1,
        "fixtures": fixtures,
    }
    manifest_path = target / "org-manifest.json"
    manifest_path.write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    validate_org_evidence(target, expected_source_commit=source_commit)
    return manifest


__all__ = ["render_org_fixtures"]
