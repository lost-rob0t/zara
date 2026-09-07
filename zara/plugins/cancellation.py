"""Cooperative cancellation helpers for service-plugin tools."""

from __future__ import annotations

import asyncio


def tool_cancellation_requested() -> bool:
    """Return whether the current async tool invocation is being cancelled.

    Zara's canonical tool runner cancels the exact asyncio task executing the
    active tool. This exposes that task-local state to cleanup code without a
    model-visible token, reusable process identifier, or second cancellation
    control plane.
    """

    try:
        task = asyncio.current_task()
    except RuntimeError:
        return False
    return task is not None and task.cancelling() > 0


__all__ = ["tool_cancellation_requested"]
