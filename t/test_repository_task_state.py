from __future__ import annotations

import json
import subprocess

import pytest

from zara.tasks.repository_state import RepositoryTaskState
from zara.tasks.store import TaskStatus, TaskTransitionError


def test_code_task_state_is_persisted_inside_selected_repository(tmp_path):
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    state = RepositoryTaskState(tmp_path)
    task = state.create("user:alice", "code", "Fix parser")
    state.start("user:alice", task.task_id)
    state.finish("user:alice", task.task_id, "Parser fixed")
    assert state.get("user:alice", task.task_id).status is TaskStatus.COMPLETED
    assert state.result("user:alice", task.task_id) == "Parser fixed"
    assert RepositoryTaskState(tmp_path).get("user:alice", task.task_id).status is TaskStatus.COMPLETED
    data = json.loads(state.path_for("user:alice").read_text())
    assert data["tasks"][0]["result"] == "Parser fixed"
    assert data["tasks"][0]["repository"] == "code"
    assert (tmp_path / ".zara" / "task-state.sqlite3").is_file()
    with pytest.raises(TaskTransitionError):
        state.start("user:alice", task.task_id)


def test_code_task_state_requires_git_repository(tmp_path):
    with pytest.raises(ValueError, match="Git worktree"):
        RepositoryTaskState(tmp_path)


def test_code_task_cancel_and_restart_recovery(tmp_path):
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    state = RepositoryTaskState(tmp_path)
    pending = state.create("user:alice", "code", "Pending")
    state.cancel("user:alice", pending.task_id)
    assert state.get("user:alice", pending.task_id).status is TaskStatus.CANCELLED
    with pytest.raises(TaskTransitionError):
        state.finish("user:alice", pending.task_id, "late result")
    assert state.result("user:alice", pending.task_id) is None
    running = state.create("user:alice", "code", "Running")
    state.start("user:alice", running.task_id)
    restarted = RepositoryTaskState(tmp_path)
    assert restarted.recover_interrupted("user:alice") == 1
    assert restarted.get("user:alice", running.task_id).status is TaskStatus.INTERRUPTED
    assert restarted.get("user:alice", pending.task_id).status is TaskStatus.CANCELLED


def test_projection_is_principal_scoped(tmp_path):
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    state = RepositoryTaskState(tmp_path)
    alice = state.create("user:alice", "code", "Alice task")
    bob = state.create("user:bob", "code", "Bob task")
    alice_state = json.loads(state.path_for("user:alice").read_text())
    bob_state = json.loads(state.path_for("user:bob").read_text())
    assert [item["task_id"] for item in alice_state["tasks"]] == [alice.task_id]
    assert [item["task_id"] for item in bob_state["tasks"]] == [bob.task_id]
    assert state.path_for("user:alice") != state.path_for("user:bob")


def test_task_state_files_are_git_ignored(tmp_path):
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    state = RepositoryTaskState(tmp_path)
    state.create("user:alice", "code", "Local task")
    result = subprocess.run(
        ["git", "-C", str(tmp_path), "status", "--short", "--untracked-files=all"],
        capture_output=True, text=True, check=True,
    )
    assert result.stdout == ""
