import os
from pathlib import Path

import pytest

from zara.agent.tools.file_tools import (
    FileTooling,
    FileToolingConfig,
    build_file_tools,
)
from zara.agent.tools.registry import ToolRegistry
from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig


FILE_TOOL_NAMES = {"read_file", "write_file", "diff_file", "list_dir"}
AGENTIC_TOOL_NAMES = {"edit_file", "find_files", "search_text"}


def build_tooling(root: Path, max_bytes: int = 20000, mode: str = "sandbox") -> FileTooling:
    return FileTooling(
        FileToolingConfig(
            base_dir=root,
            readable_roots=(root,),
            writable_roots=(root,),
            max_bytes=max_bytes,
            mode=mode,
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


def test_readonly_mode_blocks_writes_but_allows_reads(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    target = root / "note.txt"
    target.write_text("original")
    tooling = build_tooling(root, mode="readonly")

    assert "read-only" in tooling.write_file("note.txt", "changed", allow_overwrite=True)
    assert "read-only" in tooling.edit_file("note.txt", "original", "changed")
    assert target.read_text() == "original"
    assert tooling.read_file("note.txt") == "original"
    assert "+changed" in tooling.diff_file("note.txt", "changed")
    assert tooling.list_dir(".") == "note.txt"


def test_full_mode_escapes_root_confinement_but_keeps_file_checks(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    outside = tmp_path / "outside"
    outside.mkdir()
    target = outside / "sentinel.txt"
    target.write_text("untouched")
    tooling = build_tooling(root, mode="full")

    assert tooling.read_file(str(target)) == "untouched"
    result = tooling.write_file(str(target), "rewritten", allow_overwrite=True)
    assert result.startswith("Wrote ") and "sentinel.txt" in result
    assert target.read_text() == "rewritten"
    assert "sentinel.txt" in tooling.list_dir(str(outside))

    os.mkfifo(outside / "pipe")
    assert "not a regular file" in tooling.read_file(str(outside / "pipe"))


def test_edit_file_requires_unique_match_or_replace_all(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    target = root / "code.py"
    target.write_text("alpha\nbeta\nalpha\n")
    tooling = build_tooling(root)

    assert "not found" in tooling.edit_file("code.py", "missing", "x")
    assert "2 places" in tooling.edit_file("code.py", "alpha", "x")
    assert target.read_text() == "alpha\nbeta\nalpha\n"

    assert tooling.edit_file("code.py", "alpha", "x", replace_all=True) == "Edited code.py"
    assert target.read_text() == "x\nbeta\nx\n"

    assert "Edited code.py" in tooling.edit_file("code.py", "beta", "y")
    assert target.read_text() == "x\ny\nx\n"


def test_edit_file_rejects_missing_files_and_oversized_content(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    tooling = build_tooling(root, max_bytes=8)
    (root / "tiny.txt").write_text("abc")

    assert "does not exist" in tooling.edit_file("missing.txt", "a", "b")
    assert "size limit" in tooling.edit_file("tiny.txt", "abc", "123456789")


def test_find_files_matches_within_readable_roots(tmp_path):
    root = tmp_path / "root"
    (root / "nested").mkdir(parents=True)
    (root / "find_me.py").write_text("x")
    (root / "nested" / "find_me.py").write_text("y")
    (root / ".hidden_find_me.py").write_text("z")
    tooling = build_tooling(root)

    found = tooling.find_files("find_me.py").splitlines()
    assert found == ["find_me.py", "nested/find_me.py"]
    hidden = tooling.find_files("*find_me.py", include_hidden=True).splitlines()
    assert ".hidden_find_me.py" in hidden
    assert tooling.find_files("*.md") == ""


def test_search_text_returns_bounded_line_matches(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    (root / "code.py").write_text("x = 1\n# needle here\ny = needle\n")
    (root / ".hidden.txt").write_text("needle\n")
    (root / "blob.bin").write_bytes(b"\x00\xffneedle")
    tooling = build_tooling(root)

    hits = tooling.search_text("needle").splitlines()
    assert any("code.py:2" in hit for hit in hits)
    assert any("code.py:3" in hit for hit in hits)
    assert not any("hidden" in hit for hit in hits)
    assert not any("blob.bin" in hit for hit in hits)

    regex_hits = tooling.search_text(r"y = \w+", regex=True).splitlines()
    assert any("code.py:3" in hit for hit in regex_hits)

    bounded = tooling.search_text("needle", max_results=1).splitlines()
    assert len(bounded) == 1


def test_policy_provider_changes_mode_between_calls(tmp_path):
    from zara.agent.tools.file_access import FileAccessPolicy

    root = tmp_path / "root"
    root.mkdir()
    target = root / "note.txt"
    target.write_text("original")
    policy = FileAccessPolicy(default_mode="sandbox")
    policy.set_mode("chat-1", "readonly", expected_revision=0, actor="test")
    tooling = FileTooling(
        FileToolingConfig(
            base_dir=root,
            readable_roots=(root,),
            writable_roots=(root,),
            mode_provider=lambda: policy.effective_mode("chat-1").value,
        )
    )

    assert "read-only" in tooling.write_file("note.txt", "new", allow_overwrite=True)
    policy.set_mode("chat-1", "sandbox", expected_revision=1, actor="test")
    assert tooling.write_file("note.txt", "new", allow_overwrite=True) == "Wrote note.txt"
    assert target.read_text() == "new"


def test_build_file_tools_registers_agentic_set(tmp_path):
    tools = build_file_tools(base_dir=tmp_path)
    names = {tool.name for tool in tools}
    assert AGENTIC_TOOL_NAMES.issubset(names)
    assert names.issubset(FILE_TOOL_NAMES | AGENTIC_TOOL_NAMES)
