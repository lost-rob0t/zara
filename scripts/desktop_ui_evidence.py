#!/usr/bin/env python3
"""Render Desktop UI evidence around the existing deterministic fixture renderer."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any, Callable

from PySide6.QtCore import QPoint
from PySide6.QtWidgets import QWidget

from zara.desktop import ui_fixtures as fixtures


def _widget_text(widget: QWidget, method_name: str) -> str:
    method = getattr(widget, method_name, None)
    if not callable(method):
        return ""
    try:
        value = method()
    except (RuntimeError, TypeError):
        return ""
    return value if isinstance(value, str) else ""


def _normalized_widget_semantics(window: QWidget) -> list[dict[str, object]]:
    widgets = [window, *window.findChildren(QWidget)]
    records: list[dict[str, object]] = []
    for widget in widgets:
        if widget is not window and not widget.isVisibleTo(window):
            continue
        point = QPoint(0, 0) if widget is window else widget.mapTo(window, QPoint(0, 0))
        rect = widget.rect()
        strings: list[str] = []
        for method_name in (
            "text",
            "toPlainText",
            "placeholderText",
            "accessibleName",
            "accessibleDescription",
            "windowTitle",
        ):
            value = _widget_text(widget, method_name)
            if value and value not in strings:
                strings.append(value)
        checked_method = getattr(widget, "isChecked", None)
        checked = checked_method() if callable(checked_method) else None
        records.append(
            {
                "class": widget.metaObject().className(),
                "object_name": widget.objectName(),
                "text": strings,
                "enabled": widget.isEnabled(),
                "visible": widget.isVisible(),
                "checked": checked,
                "bounds": [
                    point.x(),
                    point.y(),
                    point.x() + rect.width(),
                    point.y() + rect.height(),
                ],
            }
        )
    records.sort(
        key=lambda record: (
            record["bounds"][1],  # type: ignore[index]
            record["bounds"][0],  # type: ignore[index]
            record["class"],
            record["object_name"],
            json.dumps(record["text"], ensure_ascii=False),
        )
    )
    return records


def _assertion_trace(actions: list[str], assertions: list[dict[str, object]]) -> str:
    lines = [f"ACTION {index} {action}" for index, action in enumerate(actions, 1)]
    lines.extend(
        " ".join(
            (
                "ASSERT",
                "PASS" if assertion["passed"] else "FAIL",
                str(assertion["name"]),
                str(assertion["detail"]),
            )
        ).rstrip()
        for assertion in assertions
    )
    return "\n".join(lines) + ("\n" if lines else "")


def _desktop_text_twin(
    *,
    state: str,
    source_commit: str,
    width: int,
    height: int,
    semantics: list[dict[str, object]],
    trace: str,
) -> str:
    metadata = {
        "state": state,
        "source_commit": source_commit,
        "theme": fixtures._THEME,
        "width": width,
        "height": height,
    }
    lines = [
        "meta=" + json.dumps(metadata, ensure_ascii=False, sort_keys=True, separators=(",", ":"))
    ]
    lines.extend(
        "widget=" + json.dumps(record, ensure_ascii=False, sort_keys=True, separators=(",", ":"))
        for record in semantics
    )
    return "\n".join(lines) + "\n" + trace


class _CaptureFence:
    def __init__(self) -> None:
        self.current_state: str | None = None
        self.semantics: dict[str, list[dict[str, object]]] = {}

    def capture(self, window: QWidget, grab: Callable[[], Any]) -> Any:
        state = self.current_state
        if state is None:
            raise RuntimeError("desktop evidence capture has no active fixture state")
        before = _normalized_widget_semantics(window)
        pixmap = grab()
        after = _normalized_widget_semantics(window)
        if before != after:
            raise RuntimeError(f"desktop UI changed across screenshot capture: {state}")
        if state in self.semantics:
            raise RuntimeError(f"desktop fixture captured more than once: {state}")
        self.semantics[state] = before
        return pixmap


def _render_with_capture_fence(output_dir: Path, *, source_commit: str) -> dict[str, object]:
    fence = _CaptureFence()
    original_window_class = fixtures.CopilotWindow
    original_render_one = fixtures._render_one

    class EvidenceCopilotWindow(original_window_class):
        def grab(self):  # type: ignore[no-untyped-def]
            return fence.capture(self, lambda: super(EvidenceCopilotWindow, self).grab())

    def render_one(
        target: Path,
        state: str,
        filename: str,
        *,
        source_commit: str,
        root: Path,
    ) -> dict[str, object]:
        if fence.current_state is not None:
            raise RuntimeError("desktop fixture capture overlapped another fixture")
        fence.current_state = state
        try:
            return original_render_one(
                target,
                state,
                filename,
                source_commit=source_commit,
                root=root,
            )
        finally:
            fence.current_state = None

    fixtures.CopilotWindow = EvidenceCopilotWindow
    fixtures._render_one = render_one
    try:
        manifest = fixtures.render_copilot_fixtures(output_dir, source_commit=source_commit)
    finally:
        fixtures.CopilotWindow = original_window_class
        fixtures._render_one = original_render_one

    entries = manifest.get("fixtures")
    if not isinstance(entries, list) or not entries:
        raise RuntimeError("desktop fixture renderer returned no fixture entries")

    for raw_entry in entries:
        if not isinstance(raw_entry, dict):
            raise RuntimeError("desktop fixture renderer returned a malformed fixture entry")
        state = raw_entry.get("state")
        if not isinstance(state, str) or not state:
            raise RuntimeError("desktop fixture renderer returned an unnamed state")
        semantics = fence.semantics.pop(state, None)
        if semantics is None:
            raise RuntimeError(f"desktop fixture did not cross screenshot capture fence: {state}")
        width = raw_entry.get("width")
        height = raw_entry.get("height")
        if not isinstance(width, int) or not isinstance(height, int):
            raise RuntimeError(f"desktop fixture dimensions are invalid: {state}")

        actions = [f"render:{state}"]
        assertions: list[dict[str, object]] = [
            {
                "name": "same-state-semantics",
                "passed": True,
                "detail": "widget semantics stable across screenshot capture",
            },
            {
                "name": "screenshot-png",
                "passed": True,
                "detail": "Qt produced PNG screenshot evidence",
            },
        ]
        trace = _assertion_trace(actions, assertions)
        text_path = output_dir / f"{state}.ui.txt"
        text_path.write_text(
            _desktop_text_twin(
                state=state,
                source_commit=source_commit,
                width=width,
                height=height,
                semantics=semantics,
                trace=trace,
            ),
            encoding="utf-8",
        )
        assertions_path = output_dir / f"{state}.assertions.txt"
        assertions_path.write_text(trace, encoding="utf-8")
        raw_entry.update(
            {
                "actions": actions,
                "assertions": assertions,
                "text_evidence": {
                    "file": text_path.name,
                    "sha256": hashlib.sha256(text_path.read_bytes()).hexdigest(),
                },
                "assertion_evidence": {
                    "file": assertions_path.name,
                    "sha256": hashlib.sha256(assertions_path.read_bytes()).hexdigest(),
                },
            }
        )

    if fence.semantics:
        raise RuntimeError(f"unbound desktop capture states remain: {sorted(fence.semantics)}")

    manifest["schema"] = 2
    (output_dir / "manifest.json").write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return manifest


def render_desktop_ui_evidence(
    output_dir: Path | str,
    *,
    source_commit: str,
) -> dict[str, object]:
    target = Path(output_dir)
    target.mkdir(parents=True, exist_ok=True)
    return _render_with_capture_fence(target, source_commit=source_commit)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--source-sha", required=True)
    args = parser.parse_args()
    render_desktop_ui_evidence(args.output, source_commit=args.source_sha)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
