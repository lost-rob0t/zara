"""Per-chat file access policy: modes, CAS overrides, persistence, scope."""

import json
from pathlib import Path

import pytest

from zara.agent.tools.file_access import (
    FileAccessConflict,
    FileAccessMode,
    FileAccessPolicy,
    bind_file_access_scope,
    current_file_access_conversation,
    parse_file_access_mode,
)


def test_parse_mode_accepts_canonical_names_and_rejects_everything_else():
    assert parse_file_access_mode("full") is FileAccessMode.FULL
    assert parse_file_access_mode("sandbox") is FileAccessMode.SANDBOX
    assert parse_file_access_mode("readonly") is FileAccessMode.READONLY
    assert parse_file_access_mode("ask") is FileAccessMode.ASK
    with pytest.raises(ValueError):
        parse_file_access_mode("yolo")
    with pytest.raises(ValueError):
        parse_file_access_mode("")
    with pytest.raises(ValueError):
        parse_file_access_mode(None)


def test_default_mode_is_sandbox_and_chat_override_wins(tmp_path):
    policy = FileAccessPolicy(default_mode="sandbox")
    assert policy.effective_mode("chat-1") is FileAccessMode.SANDBOX

    policy.set_mode("chat-1", "ask", expected_revision=0, actor="desktop")
    assert policy.effective_mode("chat-1") is FileAccessMode.ASK
    assert policy.effective_mode("chat-2") is FileAccessMode.SANDBOX

    policy.clear_mode("chat-1")
    assert policy.effective_mode("chat-1") is FileAccessMode.SANDBOX


def test_stale_revision_cannot_clobber_newer_override(tmp_path):
    policy = FileAccessPolicy(default_mode="sandbox")
    policy.set_mode("chat-1", "ask", expected_revision=0, actor="a")
    revision = policy.revision("chat-1")

    with pytest.raises(FileAccessConflict):
        policy.set_mode("chat-1", "full", expected_revision=0, actor="stale")

    assert policy.effective_mode("chat-1") is FileAccessMode.ASK
    assert policy.revision("chat-1") == revision

    policy.set_mode("chat-1", "full", expected_revision=revision, actor="fresh")
    assert policy.effective_mode("chat-1") is FileAccessMode.FULL


def test_guest_turns_never_exceed_guest_ceiling():
    policy = FileAccessPolicy(default_mode="full", guest_ceiling="ask")
    policy.set_mode("public-chat", "full", expected_revision=0, actor="remote")

    assert policy.effective_mode("public-chat") is FileAccessMode.FULL
    assert policy.effective_mode("public-chat", principal_kind="guest") is FileAccessMode.ASK
    assert policy.effective_mode("public-chat", principal_kind="owner") is FileAccessMode.FULL


def test_persistence_survives_policy_reconstruction(tmp_path):
    store_path = tmp_path / "file_access_modes.json"
    first = FileAccessPolicy(default_mode="sandbox", store_path=store_path)
    first.set_mode("chat-1", "readonly", expected_revision=0, actor="desktop")
    first.set_default("ask")

    second = FileAccessPolicy(default_mode="sandbox", store_path=store_path)
    assert second.default_mode is FileAccessMode.ASK
    assert second.effective_mode("chat-1") is FileAccessMode.READONLY
    assert second.revision("chat-1") == first.revision("chat-1")


def test_corrupt_store_fails_closed_to_defaults(tmp_path):
    store_path = tmp_path / "file_access_modes.json"
    store_path.write_text("{not json")

    policy = FileAccessPolicy(default_mode="sandbox", store_path=store_path)
    assert policy.default_mode is FileAccessMode.SANDBOX
    assert policy.effective_mode("chat-1") is FileAccessMode.SANDBOX


def test_audit_records_mode_changes_without_conversation_content():
    policy = FileAccessPolicy(default_mode="sandbox")
    policy.set_mode("chat-1", "ask", expected_revision=0, actor="desktop")
    policy.set_default("readonly")

    entries = policy.audit_log()
    assert len(entries) == 2
    actor_scoped = entries[0]
    assert actor_scoped["actor"] == "desktop"
    assert actor_scoped["target"] == "chat-1"
    assert actor_scoped["new_mode"] == "ask"
    assert actor_scoped["old_mode"] == "sandbox"
    default_change = entries[1]
    assert default_change["target"] == "default"
    assert default_change["new_mode"] == "readonly"


def test_override_store_is_bounded(tmp_path):
    store_path = tmp_path / "file_access_modes.json"
    policy = FileAccessPolicy(
        default_mode="sandbox", store_path=store_path, max_overrides=8
    )
    for index in range(16):
        policy.set_mode(f"chat-{index}", "ask", expected_revision=0, actor="test")

    assert len(policy._overrides) <= 8
    document = json.loads(store_path.read_text())
    assert len(document["overrides"]) <= 8


def test_scope_binding_exposes_conversation_to_tools():
    assert current_file_access_conversation() is None
    with bind_file_access_scope("conv-42"):
        assert current_file_access_conversation() == "conv-42"
        with bind_file_access_scope("conv-99"):
            assert current_file_access_conversation() == "conv-99"
        assert current_file_access_conversation() == "conv-42"
    assert current_file_access_conversation() is None


def test_default_mode_can_be_changed_and_validated():
    policy = FileAccessPolicy(default_mode="sandbox")
    policy.set_default("readonly")
    assert policy.effective_mode("any-chat") is FileAccessMode.READONLY
    with pytest.raises(ValueError):
        policy.set_default("danger")
