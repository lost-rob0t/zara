"""Tests for the pet task-notifier pure helpers (bubble text, dot pulse)."""

from __future__ import annotations

from zara.pets.qt_overlay import _task_bubble_text, _task_dot_alphas


def test_task_bubble_text_mirrors_announcer_wording():
    assert _task_bubble_text("TaskCompleted", {"label": "Deploy"}) == "Deploy done."
    assert _task_bubble_text("TaskFailed", {"label": "Ship"}) == "Ship failed."
    assert _task_bubble_text("TaskWaitingApproval", {"label": "Run"}) == "Run needs approval."


def test_task_bubble_text_ignores_non_task_events():
    assert _task_bubble_text("ModelStarted", {"label": "llm"}) is None
    assert _task_bubble_text("TaskStarted", {"label": "Deploy"}) is None
    assert _task_bubble_text("TaskStepCompleted", {"label": "Deploy"}) is None
    assert _task_bubble_text("TaskAborted", {"label": "tasks"}) is None


def test_task_bubble_text_falls_back_to_generic_label():
    assert _task_bubble_text("TaskCompleted", {}) == "Task done."
    assert _task_bubble_text("TaskFailed", {"label": "  "}) == "Task failed."


def test_task_dot_alphas_pulse_within_bounds():
    alphas = _task_dot_alphas(0.0, reduced_motion=False)
    assert len(alphas) == 3
    assert all(0.0 <= alpha <= 1.0 for alpha in alphas)
    assert _task_dot_alphas(4.25, reduced_motion=False) != _task_dot_alphas(
        4.75, reduced_motion=False
    )


def test_task_dot_alphas_are_static_under_reduced_motion():
    assert _task_dot_alphas(0.0, reduced_motion=True) == [1.0, 1.0, 1.0]
    assert _task_dot_alphas(9.7, reduced_motion=True) == [1.0, 1.0, 1.0]
