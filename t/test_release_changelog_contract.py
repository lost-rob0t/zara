from __future__ import annotations

import importlib.util
from pathlib import Path
from types import SimpleNamespace

import pytest

from zara.version_context import load_version_context


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "scripts" / "release-changelog.py"


def _module():
    spec = importlib.util.spec_from_file_location("zara_release_changelog_test", SCRIPT)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_release_notes_preserve_canonical_markdown_section() -> None:
    module = _module()
    markdown = """# Zara Changelog

## Unreleased

- future

## 1.2.3

### Added

- one
- two

### Fixed

- three

## 1.2.2

- old
"""

    assert module.extract_version_notes(markdown, "1.2.3") == (
        "### Added\n\n- one\n- two\n\n### Fixed\n\n- three\n"
    )


@pytest.mark.parametrize(
    "markdown",
    (
        "# Zara Changelog\n\n## 1.2.2\n\n- old\n",
        "# Zara Changelog\n\n## 1.2.3\n\n## 1.2.3\n\n- duplicate\n",
        "# Zara Changelog\n\n## 1.2.3\n\n### Added\n\nprose only\n",
    ),
)
def test_release_notes_reject_missing_duplicate_or_entryless_sections(markdown: str) -> None:
    module = _module()

    with pytest.raises(module.ReleaseChangelogError):
        module.extract_version_notes(markdown, "1.2.3")


def test_version_change_rejects_reusing_an_existing_immutable_tag(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    module = _module()
    monkeypatch.setattr(module, "version_context_changed", lambda _base: True)
    monkeypatch.setattr(
        module,
        "load_version_context",
        lambda _path: SimpleNamespace(release_ready=True, version="1.2.3"),
    )
    monkeypatch.setattr(module, "existing_version_tag", lambda _version: "a" * 40)
    monkeypatch.setattr(module, "changelog_changed", lambda _base: True)

    with pytest.raises(module.ReleaseChangelogError, match="already tagged"):
        module.validate(base="base")


def test_current_version_has_publishable_canonical_release_notes() -> None:
    module = _module()
    version = load_version_context(ROOT / "version.properties").version

    notes = module.extract_version_notes(
        (ROOT / "CHANGELOG.md").read_text(encoding="utf-8"),
        version,
    )

    assert notes.strip()
    assert "- " in notes


def test_ordinary_master_change_requires_changelog_update(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    module = _module()
    monkeypatch.setattr(
        module,
        "load_version_context",
        lambda _path: SimpleNamespace(release_ready=False, version="1.2.3"),
    )
    monkeypatch.setattr(module, "version_context_changed", lambda _base: False)
    monkeypatch.setattr(module, "changelog_changed", lambda _base: False)

    with pytest.raises(module.ReleaseChangelogError, match="every master-bound change"):
        module.validate(base="base")


def test_ordinary_master_change_accepts_changelog_update_without_release_promotion(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    module = _module()
    monkeypatch.setattr(
        module,
        "load_version_context",
        lambda _path: SimpleNamespace(release_ready=False, version="1.2.3"),
    )
    monkeypatch.setattr(module, "version_context_changed", lambda _base: False)
    monkeypatch.setattr(module, "changelog_changed", lambda _base: True)

    assert module.validate(base="base") is None


def test_ci_requires_canonical_changelog_for_every_master_bound_change() -> None:
    workflow = (ROOT / ".github/workflows/ci.yml").read_text(encoding="utf-8")

    assert "Validate master changelog and versioned release notes" in workflow
    assert 'python scripts/release-changelog.py --base "$BASE_SHA"' in workflow
    assert "github.event.pull_request.base.sha || github.event.before" in workflow


def test_versioned_release_is_tag_only_and_uses_exact_approved_master_ci() -> None:
    workflow = (ROOT / ".github/workflows/release.yml").read_text(encoding="utf-8")

    assert "android-versioned-release:" in workflow
    assert "github.ref_type == 'tag'" in workflow
    assert "(github.ref == 'refs/heads/master'" not in workflow
    assert "Wait for exact approved source CI" in workflow
    assert "head_sha=${GITHUB_SHA}" in workflow
    assert 'run.get("event") == "push"' in workflow
    assert 'run.get("head_branch") == "master"' in workflow
    assert 'run.get("head_sha") == source_sha' in workflow
    assert 'run.get("conclusion") == "success"' in workflow


def test_existing_immutable_release_is_verified_not_rewritten() -> None:
    workflow = (ROOT / ".github/workflows/release.yml").read_text(encoding="utf-8")

    assert "gh release edit" not in workflow
    assert "already exists; refusing to overwrite it" in workflow
    assert "Verify published release bytes, metadata, signer, and notes" in workflow
    assert 'gh release download "$TAG"' in workflow


def test_versioned_release_uses_canonical_notes_and_exact_downloaded_bytes() -> None:
    workflow = (ROOT / ".github/workflows/release.yml").read_text(encoding="utf-8")

    assert "Validate canonical release notes" in workflow
    assert 'python scripts/release-changelog.py --version "$VERSION" --output "$NOTES"' in workflow
    assert 'gh release create "$TAG"' in workflow
    assert '--notes-file "$NOTES"' in workflow
    assert "Exercise exact release APK before publication" in workflow
    assert 'adb -s "$serial" install -r "$APK"' in workflow
    assert "android/integration/device_acceptance.py" in workflow
    assert "versioned-release-device-${{ github.sha }}" in workflow
    assert "Verify published release bytes, metadata, signer, and notes" in workflow
    assert 'cmp "$APK" "$published_apk"' in workflow


def test_release_notes_are_available_to_github_desktop_and_android() -> None:
    workflow = (ROOT / ".github/workflows/release.yml").read_text(encoding="utf-8")
    desktop = (ROOT / "zara/desktop/app.py").read_text(encoding="utf-8")
    android = (
        ROOT / "android/app/src/main/java/ai/zara/app/update/Changelog.kt"
    ).read_text(encoding="utf-8")
    gradle = (ROOT / "android/app/build.gradle.kts").read_text(encoding="utf-8")

    assert '--notes-file "$NOTES"' in workflow
    assert "current_release_notes" in desktop
    assert 'context.assets.open("CHANGELOG.md")' in android
    assert 'layout.projectDirectory.file("../../CHANGELOG.md")' in gradle
