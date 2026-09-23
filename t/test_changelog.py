from pathlib import Path

from zara.changelog import notes_for_version, parse_changelog, should_show_release_notes


def test_changelog_extracts_only_requested_version():
    markdown = """# Zara Changelog

## Unreleased

- Future work.

## 0.2.2-alpha

### Added

- Projects.
- Three menus.

### Fixed

- Audio focus.

## 0.1.2-alpha

- Old baseline.
"""
    sections = parse_changelog(markdown)
    assert sections["0.2.2-alpha"] == (
        "Added\n"
        "• Projects.\n"
        "• Three menus.\n\n"
        "Fixed\n"
        "• Audio focus."
    )
    assert "Future work." not in sections["0.2.2-alpha"]


def test_notes_for_version_reads_canonical_file(tmp_path: Path):
    path = tmp_path / "CHANGELOG.md"
    path.write_text("## 0.2.2-alpha\n\n- Shipped.\n", encoding="utf-8")
    assert notes_for_version("0.2.2-alpha", path=path) == "• Shipped."
    assert notes_for_version("9.9.9", path=path) is None


def test_release_notes_show_once_per_version():
    settings = {}
    notes = "• New thing"
    assert should_show_release_notes(settings, "0.2.2-alpha", notes)
    settings["desktop/changelog/last-shown-version"] = "0.2.2-alpha"
    assert not should_show_release_notes(settings, "0.2.2-alpha", notes)
    assert should_show_release_notes(settings, "0.2.3-alpha", notes)
    assert not should_show_release_notes({}, "0.2.2-alpha", None)
