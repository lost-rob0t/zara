"""Integrity checks for Desktop Org screenshot evidence.

Evidence is only useful when it proves the reviewed candidate.  Keep validation
independent of Qt so CI/review tooling can reject stale or tampered artifacts
without constructing the UI.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any

_REQUIRED_FIXTURES: tuple[tuple[str, str], ...] = (
    ("editor", "org-editor.png"),
    ("todo", "org-todo.png"),
    ("sync", "org-sync.png"),
    ("notebook", "org-notebook.png"),
)
_PNG_SIGNATURE = b"\x89PNG\r\n\x1a\n"


class OrgEvidenceError(ValueError):
    """Raised when Desktop Org evidence cannot prove the requested candidate."""


@dataclass(frozen=True, slots=True)
class OrgEvidenceValidation:
    source_commit: str
    paths: tuple[str, ...]


def _load_manifest(root: Path) -> dict[str, Any]:
    manifest_path = root / "org-manifest.json"
    if not manifest_path.is_file() or manifest_path.is_symlink():
        raise OrgEvidenceError("missing screenshot manifest: org-manifest.json")
    try:
        payload = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise OrgEvidenceError("invalid screenshot manifest") from exc
    if not isinstance(payload, dict) or payload.get("schema") != 1:
        raise OrgEvidenceError("unsupported screenshot manifest schema")
    return payload


def _entry_map(payload: dict[str, Any]) -> dict[str, dict[str, Any]]:
    entries = payload.get("fixtures")
    if not isinstance(entries, list):
        raise OrgEvidenceError("manifest fixtures must be a list")

    by_mode: dict[str, dict[str, Any]] = {}
    for entry in entries:
        if not isinstance(entry, dict):
            raise OrgEvidenceError("manifest fixture must be an object")
        mode = entry.get("mode")
        if not isinstance(mode, str) or mode in by_mode:
            raise OrgEvidenceError("manifest fixture modes must be unique strings")
        by_mode[mode] = entry

    required_modes = {mode for mode, _ in _REQUIRED_FIXTURES}
    if set(by_mode) != required_modes:
        raise OrgEvidenceError("manifest does not contain the required fixtures")
    return by_mode


def validate_org_evidence(
    evidence_dir: Path | str,
    *,
    expected_source_commit: str,
) -> OrgEvidenceValidation:
    """Validate all priority Org screenshots against one exact source commit.

    The manifest may not redirect validation to arbitrary paths: each mode owns a
    fixed basename and symlinked screenshot files are rejected.  Every file must
    be PNG evidence whose bytes match the recorded SHA-256.
    """

    if not expected_source_commit:
        raise OrgEvidenceError("expected source commit is required")

    root = Path(evidence_dir)
    payload = _load_manifest(root)
    by_mode = _entry_map(payload)
    validated_paths: list[str] = []

    for mode, filename in _REQUIRED_FIXTURES:
        entry = by_mode[mode]
        if entry.get("path") != filename:
            raise OrgEvidenceError(f"unexpected screenshot path for {mode}")
        if entry.get("source_commit") != expected_source_commit:
            raise OrgEvidenceError(f"source commit mismatch for {mode}")

        width = entry.get("width")
        height = entry.get("height")
        if (
            not isinstance(width, int)
            or isinstance(width, bool)
            or width <= 0
            or not isinstance(height, int)
            or isinstance(height, bool)
            or height <= 0
        ):
            raise OrgEvidenceError(f"invalid screenshot dimensions for {mode}")

        screenshot = root / filename
        if not screenshot.is_file() or screenshot.is_symlink():
            raise OrgEvidenceError(f"missing screenshot: {filename}")
        try:
            data = screenshot.read_bytes()
        except OSError as exc:
            raise OrgEvidenceError(f"unreadable screenshot: {filename}") from exc
        if not data.startswith(_PNG_SIGNATURE):
            raise OrgEvidenceError(f"invalid PNG screenshot: {filename}")

        recorded_sha = entry.get("sha256")
        actual_sha = hashlib.sha256(data).hexdigest()
        if not isinstance(recorded_sha, str) or recorded_sha != actual_sha:
            raise OrgEvidenceError(f"sha256 mismatch for {filename}")
        validated_paths.append(filename)

    return OrgEvidenceValidation(
        source_commit=expected_source_commit,
        paths=tuple(validated_paths),
    )


__all__ = [
    "OrgEvidenceError",
    "OrgEvidenceValidation",
    "validate_org_evidence",
]
