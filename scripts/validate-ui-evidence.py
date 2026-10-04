#!/usr/bin/env python3
"""Validate exact-SHA desktop and Android screenshot evidence."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import sys
from typing import Any


_REPO_ROOT = Path(__file__).resolve().parents[1]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from zara.desktop.org_evidence import OrgEvidenceError, validate_org_evidence


PNG_SIGNATURE = b"\x89PNG\r\n\x1a\n"


class EvidenceError(ValueError):
    pass


def _load_manifest(path: Path) -> dict[str, Any]:
    if not path.is_file():
        raise EvidenceError(f"manifest is missing: {path}")
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise EvidenceError(f"manifest is unreadable: {path}: {error}") from error
    if not isinstance(payload, dict):
        raise EvidenceError(f"manifest must contain a JSON object: {path}")
    return payload


def _safe_child(root: Path, name: str) -> Path:
    candidate = (root / name).resolve()
    resolved_root = root.resolve()
    try:
        candidate.relative_to(resolved_root)
    except ValueError as error:
        raise EvidenceError(f"evidence path escapes manifest directory: {name}") from error
    return candidate


def _require_png(path: Path) -> bytes:
    if not path.is_file():
        raise EvidenceError(f"screenshot is missing: {path}")
    data = path.read_bytes()
    if len(data) <= len(PNG_SIGNATURE) or not data.startswith(PNG_SIGNATURE):
        raise EvidenceError(f"screenshot is not a PNG: {path}")
    return data


def _require_sha256(value: object, *, label: str) -> str:
    if not isinstance(value, str) or len(value) != 64:
        raise EvidenceError(f"{label} hash is invalid")
    try:
        int(value, 16)
    except ValueError as error:
        raise EvidenceError(f"{label} hash is invalid") from error
    return value.lower()


def validate_desktop(manifest_path: Path, source_sha: str) -> int:
    manifest = _load_manifest(manifest_path)
    fixtures = manifest.get("fixtures")
    if not isinstance(fixtures, list) or not fixtures:
        raise EvidenceError("desktop manifest must contain at least one fixture")

    seen_states: set[str] = set()
    seen_paths: set[str] = set()
    for entry in fixtures:
        if not isinstance(entry, dict):
            raise EvidenceError("desktop fixture entry must be an object")
        state = entry.get("state")
        path_value = entry.get("path")
        entry_sha = entry.get("source_commit")
        expected_hash = entry.get("sha256")
        width = entry.get("width")
        height = entry.get("height")
        if not isinstance(state, str) or not state:
            raise EvidenceError("desktop fixture state is missing")
        if state in seen_states:
            raise EvidenceError(f"desktop fixture state is duplicated: {state}")
        seen_states.add(state)
        if not isinstance(path_value, str) or not path_value:
            raise EvidenceError(f"desktop fixture path is missing: {state}")
        if path_value in seen_paths:
            raise EvidenceError(f"desktop fixture path is duplicated: {path_value}")
        seen_paths.add(path_value)
        if entry_sha != source_sha:
            raise EvidenceError(
                f"desktop source commit mismatch for {state}: expected {source_sha}, got {entry_sha}"
            )
        if not isinstance(width, int) or width <= 0 or not isinstance(height, int) or height <= 0:
            raise EvidenceError(f"desktop fixture has invalid dimensions: {state}")
        expected_hash = _require_sha256(expected_hash, label=f"desktop screenshot {state}")
        data = _require_png(_safe_child(manifest_path.parent, path_value))
        actual_hash = hashlib.sha256(data).hexdigest()
        if actual_hash != expected_hash:
            raise EvidenceError(
                f"desktop screenshot hash mismatch for {state}: expected {expected_hash}, got {actual_hash}"
            )

    try:
        validate_org_evidence(
            manifest_path.parent,
            expected_source_commit=source_sha,
        )
    except OrgEvidenceError as error:
        raise EvidenceError(f"desktop Org evidence invalid: {error}") from error

    return len(fixtures)


def validate_android(manifest_path: Path, source_sha: str) -> int:
    manifest = _load_manifest(manifest_path)
    if manifest.get("source_sha") != source_sha:
        raise EvidenceError(
            f"android source SHA mismatch: expected {source_sha}, got {manifest.get('source_sha')}"
        )
    if manifest.get("passed") is not True:
        raise EvidenceError("android acceptance manifest must record passed=true")
    screenshots = manifest.get("screenshots")
    if not isinstance(screenshots, list) or not screenshots:
        raise EvidenceError("android manifest must contain at least one screenshot")

    seen_states: set[str] = set()
    seen_files: set[str] = set()
    for entry in screenshots:
        if not isinstance(entry, dict):
            raise EvidenceError("android screenshot entry must be an object")
        state = entry.get("state")
        file_value = entry.get("file")
        expected_hash = entry.get("sha256")
        if not isinstance(state, str) or not state:
            raise EvidenceError("android screenshot state is missing")
        if state in seen_states:
            raise EvidenceError(f"android screenshot state is duplicated: {state}")
        seen_states.add(state)
        if not isinstance(file_value, str) or not file_value:
            raise EvidenceError(f"android screenshot filename is missing: {state}")
        if file_value in seen_files:
            raise EvidenceError(f"android screenshot filename is duplicated: {file_value}")
        seen_files.add(file_value)
        expected_hash = _require_sha256(expected_hash, label=f"android screenshot {state}")
        data = _require_png(_safe_child(manifest_path.parent, file_value))
        actual_hash = hashlib.sha256(data).hexdigest()
        if actual_hash != expected_hash:
            raise EvidenceError(
                f"android screenshot hash mismatch for {state}: expected {expected_hash}, got {actual_hash}"
            )
    return len(screenshots)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-sha", required=True)
    parser.add_argument("--desktop", type=Path, required=True)
    parser.add_argument("--android", type=Path, required=True)
    args = parser.parse_args()

    try:
        desktop_count = validate_desktop(args.desktop, args.source_sha)
        android_count = validate_android(args.android, args.source_sha)
    except EvidenceError as error:
        print(f"UI evidence validation failed: {error}", file=sys.stderr)
        return 1

    print(
        "UI evidence valid: "
        f"desktop={desktop_count} android={android_count} source_sha={args.source_sha}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
