#!/usr/bin/env python3
"""Validate exact-head Android Org screenshot + text evidence."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import re


SHA_RE = re.compile(r"[0-9a-f]{40}")
REQUIRED_STATES = {
    "org-todo",
    "org-drawer",
    "org-roam",
    "org-daily-today",
    "org-daily-previous",
    "org-reminders",
    "org-timers",
    "org-graph",
    "org-editor-pages",
    "org-editor-page",
    "org-editor-block-edit",
    "org-editor-raw",
    "org-editor-config",
    "org-home",
}
EXPECTED_FILES = {
    "screenshot": {state: f"{state}.png" for state in REQUIRED_STATES},
    "text": {state: f"{state}.txt" for state in REQUIRED_STATES},
}


class EvidenceError(RuntimeError):
    pass


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _index(entries: object, kind: str) -> dict[str, dict]:
    if not isinstance(entries, list):
        raise EvidenceError(f"{kind} evidence must be a list")
    indexed: dict[str, dict] = {}
    for entry in entries:
        if not isinstance(entry, dict):
            raise EvidenceError(f"{kind} evidence entry must be an object")
        state = entry.get("state")
        if not isinstance(state, str) or not state:
            raise EvidenceError(f"{kind} evidence entry is missing state")
        if state in indexed:
            raise EvidenceError(f"duplicate {kind} state: {state}")
        indexed[state] = entry
    return indexed


def validate(source_sha: str, manifest_path: Path) -> None:
    if not SHA_RE.fullmatch(source_sha):
        raise EvidenceError(f"invalid expected source SHA: {source_sha!r}")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    if manifest.get("source_sha") != source_sha:
        raise EvidenceError(
            f"Org Android source SHA mismatch: expected {source_sha}, "
            f"got {manifest.get('source_sha')}"
        )
    if manifest.get("passed") is not True:
        raise EvidenceError("Org Android acceptance manifest must record passed=true")
    if manifest.get("corpus_authority") != "ordinary Org files through persisted Android SAF":
        raise EvidenceError("Org acceptance must use the real SAF ordinary-Org corpus authority")
    if manifest.get("fixture_root_is_test_only") is not True:
        raise EvidenceError("Org acceptance must label its arbitrary fixture root as test-only")

    screenshots = _index(manifest.get("screenshots"), "screenshot")
    text_evidence = _index(manifest.get("text_evidence"), "text")
    missing_screens = REQUIRED_STATES - screenshots.keys()
    missing_text = REQUIRED_STATES - text_evidence.keys()
    if missing_screens:
        raise EvidenceError(f"missing Org screenshots: {sorted(missing_screens)}")
    if missing_text:
        raise EvidenceError(f"missing Org text twins: {sorted(missing_text)}")

    root = manifest_path.parent
    for kind, indexed in (("screenshot", screenshots), ("text", text_evidence)):
        for state in REQUIRED_STATES:
            entry = indexed[state]
            filename = entry.get("file")
            digest = entry.get("sha256")
            if not isinstance(filename, str) or not filename:
                raise EvidenceError(f"{kind} {state} is missing file")
            expected_filename = EXPECTED_FILES[kind][state]
            if Path(filename).name != filename:
                raise EvidenceError(
                    f"{kind} {state} must use a safe basename, got {filename!r}"
                )
            if filename != expected_filename:
                raise EvidenceError(
                    f"{kind} {state} filename mismatch: expected {expected_filename!r}, "
                    f"got {filename!r}"
                )
            path = root / filename
            if not path.is_file():
                raise EvidenceError(f"{kind} {state} file is missing: {filename}")
            actual = _sha256(path)
            if digest != actual:
                raise EvidenceError(
                    f"{kind} {state} SHA mismatch: expected {digest}, got {actual}"
                )

    todo_text = (root / text_evidence["org-todo"]["file"]).read_text(encoding="utf-8")
    roam_text = (root / text_evidence["org-roam"]["file"]).read_text(encoding="utf-8")
    today_text = (root / text_evidence["org-daily-today"]["file"]).read_text(encoding="utf-8")
    previous_text = (root / text_evidence["org-daily-previous"]["file"]).read_text(encoding="utf-8")

    for expected in ("Acceptance task", "Today acceptance"):
        if expected not in todo_text:
            raise EvidenceError(f"Todo text twin is missing {expected!r}")
    for expected in ("Search Org-roam nodes", "Acceptance task", "Beta note"):
        if expected not in roam_text:
            raise EvidenceError(f"Roam text twin is missing {expected!r}")
    for expected in ("Today ·", "Ordinary Org text is canonical"):
        if expected not in today_text:
            raise EvidenceError(f"Daily-today text twin is missing {expected!r}")
    if "Separate daily file retained" not in previous_text:
        raise EvidenceError("Daily-previous text twin does not prove a separate prior Org file")

    expected_by_state = {
        "org-drawer": ("Knowledge workspace", "Editor", "Home"),
        "org-reminders": ("Acceptance reminder", "SCHEDULED"),
        "org-timers": ("Acceptance timer", "Start", "Reset"),
        "org-graph": ("3 nodes", "id: links"),
        "org-editor-pages": ("Pages", "tasks.org"),
        "org-editor-page": ("Page", "Raw", "config.pl", "Acceptance Tasks"),
        "org-editor-block-edit": ("Editing block", "Done", "Cancel"),
        "org-editor-raw": ("Raw Org markup", "Acceptance task"),
        "org-editor-config": ("Typed app policy facts", "Validate and save policy"),
        "org-home": ("Org apps", "Recent daily pages"),
    }
    for state, expected_values in expected_by_state.items():
        text = (root / text_evidence[state]["file"]).read_text(encoding="utf-8")
        for expected in expected_values:
            if expected not in text:
                raise EvidenceError(f"{state} text twin is missing {expected!r}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-sha", required=True)
    parser.add_argument("--manifest", type=Path, required=True)
    args = parser.parse_args()
    validate(args.source_sha, args.manifest)


if __name__ == "__main__":
    main()
