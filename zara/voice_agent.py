"""Prolog-authoritative conversational control over the canonical task fleet."""

from __future__ import annotations

import asyncio
import logging
from typing import Callable, Optional

from zara.runtime import events
from zara.tasks.store import TaskStatus

logger = logging.getLogger(__name__)
_UNAVAILABLE = "The task router is unavailable. No task action was taken."
_ROUTES = frozenset({
    "chat", "new_task", "status", "speech_only", "cancel_all", "pause_all",
    "resume_all", "cancel_task", "pause_task", "resume_task",
})


class VoiceAgentRouter:
    """Classify finalized input without sending fleet controls through a model."""

    def __init__(self, runner, *, resolver: Optional[Callable] = None, publisher=None):
        self._runner = runner
        self._resolver = resolver or self._resolve
        self._publisher = publisher
        self._resolution = None

    @staticmethod
    def _resolve(text: str):
        from zara.runtime.api_service import get_server_engine
        return get_server_engine().resolve_voice_agent(text)

    async def handle(self, text: str, *, conversation_id: Optional[str] = None) -> Optional[str]:
        if not isinstance(text, str) or not text.strip() or len(text) > 12000:
            return "Voice input is empty or too long. No task action was taken."
        if self._resolution is not None and not self._resolution.done():
            return _UNAVAILABLE
        self._resolution = asyncio.create_task(asyncio.to_thread(self._resolver, text))
        self._resolution.add_done_callback(self._consume_resolution)
        try:
            decision = await asyncio.wait_for(asyncio.shield(self._resolution), timeout=2.0)
        except Exception:
            logger.warning("[VoiceAgent] symbolic routing unavailable")
            return _UNAVAILABLE
        if not isinstance(decision, dict) or decision.get("Route") not in _ROUTES:
            return _UNAVAILABLE
        route = decision["Route"]
        argument = decision.get("Argument", "")
        if not isinstance(argument, str) or len(argument) > 12000:
            return _UNAVAILABLE
        if route == "chat":
            return None
        try:
            return await self._apply(route, argument, conversation_id)
        except ValueError as error:
            return f"Task request not accepted: {error}"
        except RuntimeError as error:
            return f"Task request not accepted: {error}"

    @staticmethod
    def _consume_resolution(task) -> None:
        if not task.cancelled():
            task.exception()

    async def _apply(self, route: str, argument: str, conversation_id: Optional[str]) -> str:
        if route in {"speech_only", "cancel_all", "pause_all"}:
            if self._publisher is not None:
                self._publisher(events.SpeechInterruptRequested(
                    conversation_id=conversation_id, reason="voice_" + route,
                ))
        if route == "speech_only":
            return "Speech stopped. Your agents are still working."
        if route == "new_task":
            task = await self._runner.enqueue_task(goal=argument, conversation_id=conversation_id)
            return f"Task {task.task_id} is {task.status.value}. Keep talking; the fleet will handle it."
        if route == "status":
            tasks = self._runner.list_tasks()
            active = [task for task in tasks if task.status not in {TaskStatus.COMPLETED, TaskStatus.FAILED, TaskStatus.CANCELLED}]
            if not active:
                return "No agents are currently working or waiting."
            rows = [f"{task.task_id}: {task.status.value}; {task.goal[:100]}" for task in active[:32]]
            return f"Fleet: {len(active)} active or retained tasks.\n" + "\n".join(rows)
        if route == "cancel_all":
            tasks = await self._runner.cancel_all(reason="user_stop_all")
            return f"Stopped {len(tasks)} tasks, including queued work and subagents."
        if route == "pause_all":
            tasks = await self._runner.pause_all()
            return f"Paused {len(tasks)} tasks. Work resumes only when you ask."
        if route == "resume_all":
            tasks = await self._runner.resume_all()
            return f"Resumed {len(tasks)} task trees within their original budgets."
        task_id = argument
        if task_id == "current":
            candidates = [
                task for task in self._runner.list_tasks()
                if task.conversation_id == conversation_id and task.parent_task_id is None
                and task.status not in {TaskStatus.COMPLETED, TaskStatus.FAILED, TaskStatus.CANCELLED}
            ]
            if len(candidates) != 1:
                return "Say the task ID; there is not exactly one current task."
            task_id = candidates[0].task_id
        action = {
            "cancel_task": self._runner.cancel_task,
            "pause_task": self._runner.pause_task,
            "resume_task": self._runner.resume_task,
        }[route]
        task = await action(task_id=task_id)
        return f"Task {task.task_id} is {task.status.value}."
