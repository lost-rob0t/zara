import os
from pathlib import Path

import pytest

from zara.agent.tools.file_tools import FileTooling, FileToolingConfig
from zara.agent.tools.registry import ToolRegistry
from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig


FILE_TOOL_NAMES = {"read_file", "write_file", "diff_file", "list_dir"}


def build_tooling(root: Path, max_bytes: int = 20000) -> FileTooling:
    return FileTooling(
        FileToolingConfig(
            base_dir=root,
            readable_roots=(root,),
            writable_roots=(root,),
            max_bytes=max_bytes,
        )
    )


def test_parent_traversal_and_absolute_outside_path_are_rejected(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    outside = tmp_path / "outside"
    outside.mkdir()
    sentinel = outside / "sentinel.txt"
    sentinel.write_text("untouched")
    tooling = build_tooling(root)

    results = [
        tooling.read_file("../outside/sentinel.txt"),
        tooling.write_file("../outside/sentinel.txt", "changed", allow_overwrite=True),
        tooling.list_dir("../outside"),
        tooling.read_file(str(sentinel)),
        tooling.write_file(str(sentinel), "changed", allow_overwrite=True),
        tooling.list_dir(str(outside)),
    ]

    assert all("file access policy" in result for result in results)
    assert all(str(outside) not in result for result in results)
    assert sentinel.read_text() == "untouched"


def test_symlink_escape_and_nested_symlink_are_rejected(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    nested = root / "nested"
    nested.mkdir()
    outside = tmp_path / "outside"
    outside.mkdir()
    sentinel = outside / "sentinel.txt"
    sentinel.write_text("untouched")
    (root / "escape").symlink_to(outside, target_is_directory=True)
    (nested / "escape").symlink_to(outside, target_is_directory=True)
    tooling = build_tooling(root)

    results = [
        tooling.read_file("escape/sentinel.txt"),
        tooling.write_file("escape/sentinel.txt", "changed", allow_overwrite=True),
        tooling.list_dir("escape"),
        tooling.read_file("nested/escape/sentinel.txt"),
    ]

    assert all("symlinks are not allowed" in result for result in results)
    assert sentinel.read_text() == "untouched"


def test_broken_symlink_is_rejected(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    (root / "broken").symlink_to(tmp_path / "missing")
    tooling = build_tooling(root)

    assert "symlinks are not allowed" in tooling.read_file("broken")
    assert "symlinks are not allowed" in tooling.write_file("broken", "content")
    assert "symlinks are not allowed" in tooling.list_dir("broken")


def test_directories_and_special_files_are_rejected(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    directory = root / "directory"
    directory.mkdir()
    fifo = root / "pipe"
    os.mkfifo(fifo)
    tooling = FileTooling(
        FileToolingConfig(
            base_dir=root,
            readable_roots=(root, Path("/dev")),
            writable_roots=(root,),
        )
    )

    assert "not a regular file" in tooling.read_file("directory")
    assert "not a regular file" in tooling.diff_file("directory", "content")
    assert "not a regular file" in tooling.read_file("pipe")
    assert "not a regular file" in tooling.write_file("pipe", "content", True)
    assert "not a regular file" in tooling.read_file("/dev/null")


def test_overwrite_requires_opt_in_and_is_atomic(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    target = root / "note.txt"
    target.write_text("old")
    tooling = build_tooling(root)

    assert "allow_overwrite is required" in tooling.write_file("note.txt", "new")
    assert target.read_text() == "old"
    assert tooling.write_file("note.txt", "new", allow_overwrite=True) == "Wrote note.txt"
    assert target.read_text() == "new"


def test_atomic_write_failure_preserves_original(monkeypatch, tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    target = root / "note.txt"
    target.write_text("old")
    tooling = build_tooling(root)

    def fail_replace(source, destination):
        raise OSError("simulated")

    monkeypatch.setattr(os, "replace", fail_replace)

    assert tooling.write_file("note.txt", "new", allow_overwrite=True) == (
        "Error: atomic write failed"
    )
    assert target.read_text() == "old"
    assert not list(root.glob(".zara-write-*"))


def test_size_limits_apply_to_read_write_and_diff(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    (root / "large.txt").write_text("123456789")
    (root / "small.txt").write_text("old")
    tooling = build_tooling(root, max_bytes=8)

    assert "size limit" in tooling.read_file("large.txt")
    assert "size limit" in tooling.write_file("new.txt", "123456789")
    assert "size limit" in tooling.diff_file("small.txt", "123456789")


def test_readable_and_writable_roots_are_independent(tmp_path):
    base = tmp_path / "base"
    readable = tmp_path / "readable"
    writable = tmp_path / "writable"
    base.mkdir()
    readable.mkdir()
    writable.mkdir()
    readable_file = readable / "read.txt"
    readable_file.write_text("readable")
    writable_file = writable / "write.txt"
    tooling = FileTooling(
        FileToolingConfig(
            base_dir=base,
            readable_roots=(readable,),
            writable_roots=(writable,),
        )
    )

    assert tooling.read_file(str(readable_file)) == "readable"
    assert "file access policy" in tooling.write_file(
        str(readable_file), "changed", allow_overwrite=True
    )
    assert tooling.write_file(str(writable_file), "written") == "Wrote write.txt"
    assert "file access policy" in tooling.read_file(str(writable_file))
    assert readable_file.read_text() == "readable"


def test_unicode_paths_and_in_root_operations_work(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    tooling = build_tooling(root)

    assert tooling.write_file("資料/μελέτη.txt", "héllo Ω") == "Wrote 資料/μελέτη.txt"
    assert tooling.read_file("資料/μελέτη.txt") == "héllo Ω"
    assert "+updated" in tooling.diff_file("資料/μελέτη.txt", "updated")
    assert "資料/" in tooling.list_dir(".")


def test_listing_skips_symlinks_hidden_and_special_files(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    (root / "visible.txt").write_text("content")
    (root / ".hidden.txt").write_text("content")
    (root / "link").symlink_to(root / "visible.txt")
    os.mkfifo(root / "pipe")
    tooling = build_tooling(root)

    assert tooling.list_dir(".") == "visible.txt"
    assert tooling.list_dir(".", include_hidden=True) == ".hidden.txt\nvisible.txt"


def test_symlink_root_is_rejected(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    alias = tmp_path / "alias"
    alias.symlink_to(root, target_is_directory=True)

    with pytest.raises(ValueError, match="roots cannot be symlinks"):
        build_tooling(alias)


def test_file_tools_are_disabled_by_default_and_explicitly_enabled(tmp_path):
    config_path = tmp_path / "config.toml"
    config_path.write_text(DEFAULT_CONFIG_TOML)
    config = ZaraConfig(str(config_path))
    registry = ToolRegistry(config=config)
    registry.load_builtin_tools()

    assert FILE_TOOL_NAMES.isdisjoint(registry.list_tools())

    config._config["tools"]["file_tools"] = True
    enabled_registry = ToolRegistry(config=config)
    enabled_registry.load_builtin_tools()

    assert FILE_TOOL_NAMES.issubset(enabled_registry.list_tools())
