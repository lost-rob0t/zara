from __future__ import annotations

import hashlib
import json
from pathlib import Path

import pytest


SOURCE_COMMIT = "0123456789abcdef0123456789abcdef01234567"
REQUIRED_FIXTURES = (
    ("editor", "org-editor.png"),
    ("todo", "org-todo.png"),
    ("sync", "org-sync.png"),
    ("notebook", "org-notebook.png"),
)


def _write_evidence(root: Path, *, source_commit: str = SOURCE_COMMIT) -> None:
    fixtures = []
    for mode, filename in REQUIRED_FIXTURES:
        data = b"\x89PNG\r\n\x1a\n" + mode.encode("ascii") + b"-fixture"
        (root / filename).write_bytes(data)
        fixtures.append(
            {
                "mode": mode,
                "path": filename,
                "width": 1100,
                "height": 760,
                "theme": "signal-cabin",
                "source_commit": source_commit,
                "sha256": hashlib.sha256(data).hexdigest(),
            }
        )
    (root / "org-manifest.json").write_text(
        json.dumps({"schema": 1, "fixtures": fixtures}),
        encoding="utf-8",
    )


def test_org_desktop_evidence_accepts_exact_head_complete_artifact(tmp_path: Path):
    from zara.desktop.org_evidence import validate_org_evidence

    _write_evidence(tmp_path)

    result = validate_org_evidence(tmp_path, expected_source_commit=SOURCE_COMMIT)

    assert result.source_commit == SOURCE_COMMIT
    assert result.paths == tuple(filename for _, filename in REQUIRED_FIXTURES)


def test_org_desktop_evidence_rejects_stale_source_commit(tmp_path: Path):
    from zara.desktop.org_evidence import OrgEvidenceError, validate_org_evidence

    _write_evidence(tmp_path, source_commit="f" * 40)

    with pytest.raises(OrgEvidenceError, match="source commit"):
        validate_org_evidence(tmp_path, expected_source_commit=SOURCE_COMMIT)


def test_org_desktop_evidence_rejects_missing_required_surface(tmp_path: Path):
    from zara.desktop.org_evidence import OrgEvidenceError, validate_org_evidence

    _write_evidence(tmp_path)
    manifest_path = tmp_path / "org-manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    manifest["fixtures"] = manifest["fixtures"][:-1]
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

    with pytest.raises(OrgEvidenceError, match="required fixtures"):
        validate_org_evidence(tmp_path, expected_source_commit=SOURCE_COMMIT)


def test_org_desktop_evidence_rejects_missing_screenshot_file(tmp_path: Path):
    from zara.desktop.org_evidence import OrgEvidenceError, validate_org_evidence

    _write_evidence(tmp_path)
    (tmp_path / "org-sync.png").unlink()

    with pytest.raises(OrgEvidenceError, match="missing screenshot"):
        validate_org_evidence(tmp_path, expected_source_commit=SOURCE_COMMIT)


def test_org_desktop_evidence_rejects_tampered_screenshot(tmp_path: Path):
    from zara.desktop.org_evidence import OrgEvidenceError, validate_org_evidence

    _write_evidence(tmp_path)
    with (tmp_path / "org-todo.png").open("ab") as handle:
        handle.write(b"tampered")

    with pytest.raises(OrgEvidenceError, match="sha256"):
        validate_org_evidence(tmp_path, expected_source_commit=SOURCE_COMMIT)
