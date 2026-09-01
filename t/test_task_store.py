import sqlite3

import pytest

from zara.database import DatabaseManager
from zara.tasks.store import (
    MAX_GOAL_CHARS,
    AgentTask,
    TaskStatus,
    TaskStore,
    TaskStoreError,
    TaskTransitionError,
)


def build_store(tmp_path, **kwargs) -> TaskStore:
    db = DatabaseManager(tmp_path / "zara.db")
    return TaskStore(db, **kwargs)


def make_task(store, principal="principal-1", goal="research the topic", **kwargs):
    kwargs.setdefault("max_task_steps", 4)
    return store.create_task(principal_id=principal, goal=goal, **kwargs)


class TestCreate:
    def test_create_returns_pending_record_with_identity(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store, max_task_steps=5)

        assert isinstance(task, AgentTask)
        assert task.task_id.startswith("task-")
        assert task.principal_id == "principal-1"
        assert task.goal == "research the topic"
        assert task.status is TaskStatus.PENDING
        assert task.max_task_steps == 5
        assert task.steps_completed == 0
        assert task.reason is None
        assert task.created_at and task.updated_at

    def test_create_persists_and_roundtrips(self, tmp_path):
        store = build_store(tmp_path)
        created = make_task(store, max_task_steps=3)

        fetched = store.get_task(created.task_id, principal_id="principal-1")
        assert fetched == created

    def test_create_strips_goal_whitespace(self, tmp_path):
        store = build_store(tmp_path)
        task = store.create_task(
            principal_id="principal-1", goal="  pad the goal  ", max_task_steps=2
        )
        assert task.goal == "pad the goal"

    @pytest.mark.parametrize("goal", ["", "   "])
    def test_create_rejects_empty_goal(self, tmp_path, goal):
        store = build_store(tmp_path)
        with pytest.raises(ValueError, match="goal"):
            store.create_task(principal_id="p", goal=goal, max_task_steps=1)

    def test_create_rejects_goal_over_bound(self, tmp_path):
        store = build_store(tmp_path)
        with pytest.raises(ValueError, match="goal"):
            store.create_task(
                principal_id="p", goal="x" * (MAX_GOAL_CHARS + 1), max_task_steps=1
            )

    def test_create_accepts_goal_at_bound(self, tmp_path):
        store = build_store(tmp_path)
        task = store.create_task(
            principal_id="p", goal="x" * MAX_GOAL_CHARS, max_task_steps=1
        )
        assert len(task.goal) == MAX_GOAL_CHARS

    @pytest.mark.parametrize("principal", ["", "   "])
    def test_create_rejects_unknown_principal(self, tmp_path, principal):
        store = build_store(tmp_path)
        with pytest.raises(ValueError, match="principal"):
            store.create_task(principal_id=principal, goal="goal", max_task_steps=1)

    @pytest.mark.parametrize("max_steps", [0, -1, True, "3", 1.5, None])
    def test_create_rejects_invalid_step_budget(self, tmp_path, max_steps):
        store = build_store(tmp_path)
        with pytest.raises(ValueError, match="step"):
            store.create_task(
                principal_id="p", goal="goal", max_task_steps=max_steps
            )


SETUP_PATHS = {
    TaskStatus.PENDING: [],
    TaskStatus.RUNNING: [TaskStatus.RUNNING],
    TaskStatus.WAITING_APPROVAL: [TaskStatus.RUNNING, TaskStatus.WAITING_APPROVAL],
    TaskStatus.WAITING_INPUT: [TaskStatus.RUNNING, TaskStatus.WAITING_INPUT],
    TaskStatus.BLOCKED: [TaskStatus.RUNNING, TaskStatus.BLOCKED],
    TaskStatus.COMPLETED: [TaskStatus.RUNNING, TaskStatus.COMPLETED],
    TaskStatus.FAILED: [TaskStatus.RUNNING, TaskStatus.FAILED],
    TaskStatus.CANCELLED: [TaskStatus.CANCELLED],
    TaskStatus.INTERRUPTED: [TaskStatus.RUNNING, TaskStatus.INTERRUPTED],
}


def drive_to(store, task, status):
    for step in SETUP_PATHS[status]:
        store.transition(task.task_id, principal_id="principal-1", status=step)


class TestTransitions:
    LEGAL = [
        (TaskStatus.PENDING, TaskStatus.RUNNING),
        (TaskStatus.PENDING, TaskStatus.CANCELLED),
        (TaskStatus.RUNNING, TaskStatus.WAITING_APPROVAL),
        (TaskStatus.RUNNING, TaskStatus.WAITING_INPUT),
        (TaskStatus.RUNNING, TaskStatus.BLOCKED),
        (TaskStatus.RUNNING, TaskStatus.COMPLETED),
        (TaskStatus.RUNNING, TaskStatus.FAILED),
        (TaskStatus.RUNNING, TaskStatus.CANCELLED),
        (TaskStatus.RUNNING, TaskStatus.INTERRUPTED),
        (TaskStatus.WAITING_APPROVAL, TaskStatus.RUNNING),
        (TaskStatus.WAITING_APPROVAL, TaskStatus.FAILED),
        (TaskStatus.WAITING_APPROVAL, TaskStatus.CANCELLED),
        (TaskStatus.WAITING_APPROVAL, TaskStatus.INTERRUPTED),
        (TaskStatus.WAITING_INPUT, TaskStatus.RUNNING),
        (TaskStatus.BLOCKED, TaskStatus.RUNNING),
        (TaskStatus.INTERRUPTED, TaskStatus.RUNNING),
        (TaskStatus.INTERRUPTED, TaskStatus.CANCELLED),
    ]

    @pytest.mark.parametrize("source,target", LEGAL)
    def test_legal_transition(self, tmp_path, source, target):
        store = build_store(tmp_path)
        task = make_task(store)
        drive_to(store, task, source)
        updated = store.transition(
            task.task_id, principal_id="principal-1", status=target, reason="r-1"
        )
        assert updated.status is target
        assert updated.reason == "r-1"

    ILLEGAL = [
        (TaskStatus.PENDING, TaskStatus.COMPLETED),
        (TaskStatus.PENDING, TaskStatus.WAITING_APPROVAL),
        (TaskStatus.COMPLETED, TaskStatus.RUNNING),
        (TaskStatus.FAILED, TaskStatus.RUNNING),
        (TaskStatus.CANCELLED, TaskStatus.RUNNING),
        (TaskStatus.CANCELLED, TaskStatus.INTERRUPTED),
        (TaskStatus.INTERRUPTED, TaskStatus.COMPLETED),
        (TaskStatus.INTERRUPTED, TaskStatus.PENDING),
    ]

    @pytest.mark.parametrize("source,target", ILLEGAL)
    def test_illegal_transition_rejected(self, tmp_path, source, target):
        store = build_store(tmp_path)
        task = make_task(store)
        drive_to(store, task, source)
        with pytest.raises(TaskTransitionError):
            store.transition(task.task_id, principal_id="principal-1", status=target)

    def test_transition_unknown_task_raises(self, tmp_path):
        store = build_store(tmp_path)
        with pytest.raises(ValueError, match="not found"):
            store.transition(
                "task-does-not-exist", principal_id="p", status=TaskStatus.RUNNING
            )

    def test_transition_accepts_status_strings(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store)
        updated = store.transition(
            task.task_id, principal_id="principal-1", status="running"
        )
        assert updated.status is TaskStatus.RUNNING

    def test_transition_rejects_unknown_status(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store)
        with pytest.raises(ValueError, match="status"):
            store.transition(
                task.task_id, principal_id="principal-1", status="detonated"
            )

    def test_transition_updates_timestamp(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store)
        updated = store.transition(
            task.task_id, principal_id="principal-1", status=TaskStatus.RUNNING
        )
        assert updated.updated_at >= task.updated_at


class TestPrincipalIsolation:
    def test_list_is_scoped_by_principal(self, tmp_path):
        store = build_store(tmp_path)
        mine = make_task(store, principal="principal-1")
        make_task(store, principal="principal-2")

        rows = store.list_tasks(principal_id="principal-1")
        assert [task.task_id for task in rows] == [mine.task_id]

    def test_get_foreign_task_returns_none(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store, principal="principal-1")
        assert store.get_task(task.task_id, principal_id="principal-2") is None

    def test_transition_foreign_task_rejected(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store, principal="principal-1")
        with pytest.raises(ValueError, match="not found"):
            store.transition(
                task.task_id, principal_id="principal-2", status=TaskStatus.RUNNING
            )

    def test_steps_hidden_across_principals(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store, principal="principal-1")
        store.record_step(
            task.task_id,
            principal_id="principal-1",
            step_index=0,
            status="completed",
            summary="step summary",
        )
        with pytest.raises(TaskStoreError, match="not found"):
            store.list_steps(task.task_id, principal_id="principal-2")


class TestListing:
    def test_list_filters_by_statuses(self, tmp_path):
        store = build_store(tmp_path)
        running = make_task(store)
        done = make_task(store)
        store.transition(running.task_id, principal_id="principal-1", status=TaskStatus.RUNNING)
        drive_to(store, done, TaskStatus.COMPLETED)

        rows = store.list_tasks(
            principal_id="principal-1", statuses=[TaskStatus.RUNNING]
        )
        assert [task.task_id for task in rows] == [running.task_id]

        rows = store.list_tasks(
            principal_id="principal-1",
            statuses=[TaskStatus.RUNNING, TaskStatus.COMPLETED],
        )
        assert {task.task_id for task in rows} == {running.task_id, done.task_id}

    def test_list_rejects_unknown_status_filter(self, tmp_path):
        store = build_store(tmp_path)
        with pytest.raises(ValueError, match="status"):
            store.list_tasks(principal_id="p", statuses=["flying"])

    def test_list_empty_store(self, tmp_path):
        store = build_store(tmp_path)
        assert store.list_tasks(principal_id="nobody") == []


class TestStepLog:
    def test_record_and_list_steps(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store)
        store.record_step(
            task.task_id,
            principal_id="principal-1",
            step_index=0,
            status="completed",
            summary="did the first thing",
        )
        steps = store.list_steps(task.task_id, principal_id="principal-1")
        assert len(steps) == 1
        assert steps[0].step_index == 0
        assert steps[0].status == "completed"
        assert steps[0].summary == "did the first thing"

    def test_record_step_increments_completed_counter(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store)
        store.record_step(
            task.task_id,
            principal_id="principal-1",
            step_index=0,
            status="completed",
            summary="done",
        )
        updated = store.get_task(task.task_id, principal_id="principal-1")
        assert updated.steps_completed == 1

    def test_cancelled_step_does_not_increment_counter(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store)
        store.record_step(
            task.task_id,
            principal_id="principal-1",
            step_index=0,
            status="cancelled",
            summary="",
        )
        updated = store.get_task(task.task_id, principal_id="principal-1")
        assert updated.steps_completed == 0

    def test_record_step_rejects_summary_over_bound(self, tmp_path):
        store = build_store(tmp_path, step_log_chars=16)
        task = make_task(store)
        with pytest.raises(ValueError, match="summary"):
            store.record_step(
                task.task_id,
                principal_id="principal-1",
                step_index=0,
                status="completed",
                summary="x" * 17,
            )

    def test_record_step_rejects_duplicate_index(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store)
        store.record_step(
            task.task_id,
            principal_id="principal-1",
            step_index=0,
            status="completed",
            summary="first",
        )
        with pytest.raises(ValueError, match="step"):
            store.record_step(
                task.task_id,
                principal_id="principal-1",
                step_index=0,
                status="completed",
                summary="duplicate",
            )

    def test_record_step_rejects_unknown_task(self, tmp_path):
        store = build_store(tmp_path)
        with pytest.raises(ValueError, match="not found"):
            store.record_step(
                "task-missing",
                principal_id="p",
                step_index=0,
                status="completed",
                summary="s",
            )

    def test_record_step_rejects_unknown_status(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store)
        with pytest.raises(ValueError, match="status"):
            store.record_step(
                task.task_id,
                principal_id="principal-1",
                step_index=0,
                status="vibing",
                summary="s",
            )


class TestPersistenceAndRecovery:
    def test_store_reopen_preserves_tasks_and_steps(self, tmp_path):
        db = DatabaseManager(tmp_path / "zara.db")
        store = TaskStore(db)
        task = make_task(store)
        store.transition(task.task_id, principal_id="principal-1", status=TaskStatus.RUNNING)
        store.record_step(
            task.task_id,
            principal_id="principal-1",
            step_index=0,
            status="completed",
            summary="survives reopen",
        )
        db.close()

        reopened = TaskStore(DatabaseManager(tmp_path / "zara.db"))
        fetched = reopened.get_task(task.task_id, principal_id="principal-1")
        assert fetched is not None
        assert fetched.status is TaskStatus.RUNNING
        assert fetched.steps_completed == 1
        steps = reopened.list_steps(task.task_id, principal_id="principal-1")
        assert [step.summary for step in steps] == ["survives reopen"]

    def test_recover_interrupted_moves_active_states(self, tmp_path):
        store = build_store(tmp_path)
        running = make_task(store)
        waiting = make_task(store)
        pending = make_task(store)
        completed = make_task(store)
        store.transition(running.task_id, principal_id="principal-1", status=TaskStatus.RUNNING)
        store.transition(waiting.task_id, principal_id="principal-1", status=TaskStatus.RUNNING)
        store.transition(waiting.task_id, principal_id="principal-1", status=TaskStatus.WAITING_APPROVAL)
        drive_to(store, completed, TaskStatus.COMPLETED)

        recovered = store.recover_interrupted()
        statuses = {
            task.task_id: store.get_task(task.task_id, principal_id="principal-1").status
            for task in (running, waiting, pending, completed)
        }
        assert statuses[running.task_id] is TaskStatus.INTERRUPTED
        assert statuses[waiting.task_id] is TaskStatus.INTERRUPTED
        assert statuses[pending.task_id] is TaskStatus.PENDING
        assert statuses[completed.task_id] is TaskStatus.COMPLETED
        assert recovered == 2

    def test_recover_interrupted_is_idempotent(self, tmp_path):
        store = build_store(tmp_path)
        task = make_task(store)
        store.transition(task.task_id, principal_id="principal-1", status=TaskStatus.RUNNING)
        assert store.recover_interrupted() == 1
        assert store.recover_interrupted() == 0
