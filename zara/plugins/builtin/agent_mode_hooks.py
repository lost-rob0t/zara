from __future__ import annotations

import hashlib
import threading
from dataclasses import dataclass, field, replace
from typing import Any, Callable, Mapping

from zara.agent.prompting import base_agent_system_prompt
from zara.config import get_config


HOOK_STAGES = frozenset(
    {
        "before_task_create",
        "before_task_run",
        "after_task_result",
        "before_proactive_question",
        "after_proactive_question",
        "before_speak",
        "after_speak",
    }
)


@dataclass(frozen=True)
class AgentModeActionContext:
    stage: str
    prompt: str = ""
    text: str = ""
    task_name: str = ""
    task_id: str = ""
    conversation_id: str = ""
    personality_prompt: str = ""
    personality_fingerprint: str = ""
    metadata: Mapping[str, Any] = field(default_factory=dict)

    def with_updates(self, **changes) -> "AgentModeActionContext":
        return replace(self, **changes)


AgentModeHook = Callable[[AgentModeActionContext], AgentModeActionContext | None]


class AgentModeHookRegistry:
    """Ordered, process-local lifecycle hooks for first-party agent mode actions."""

    def __init__(self) -> None:
        self._lock = threading.RLock()
        self._hooks: dict[str, list[tuple[int, str, AgentModeHook]]] = {
            stage: [] for stage in HOOK_STAGES
        }

    def register(
        self,
        stage: str,
        name: str,
        hook: AgentModeHook,
        *,
        priority: int = 100,
    ) -> None:
        if stage not in HOOK_STAGES:
            raise ValueError(f"unknown agent-mode hook stage {stage!r}")
        name = str(name).strip()
        if not name or len(name) > 128:
            raise ValueError("hook name must contain 1 to 128 characters")
        if not callable(hook):
            raise TypeError("hook must be callable")
        with self._lock:
            hooks = [item for item in self._hooks[stage] if item[1] != name]
            hooks.append((int(priority), name, hook))
            hooks.sort(key=lambda item: (item[0], item[1]))
            self._hooks[stage] = hooks

    def unregister(self, stage: str, name: str) -> bool:
        if stage not in HOOK_STAGES:
            return False
        with self._lock:
            before = self._hooks[stage]
            after = [item for item in before if item[1] != name]
            self._hooks[stage] = after
            return len(before) != len(after)

    def run(self, context: AgentModeActionContext) -> AgentModeActionContext:
        if context.stage not in HOOK_STAGES:
            raise ValueError(f"unknown agent-mode hook stage {context.stage!r}")
        with self._lock:
            hooks = tuple(self._hooks[context.stage])
        current = context
        for _, _, hook in hooks:
            updated = hook(current)
            if updated is None:
                continue
            if not isinstance(updated, AgentModeActionContext):
                raise TypeError("agent-mode hooks must return AgentModeActionContext or None")
            if updated.stage != context.stage:
                raise ValueError("agent-mode hooks may not change lifecycle stage")
            current = updated
        return current

    def clear(self) -> None:
        with self._lock:
            for stage in HOOK_STAGES:
                self._hooks[stage] = []


def build_action_context(stage: str, **values) -> AgentModeActionContext:
    config = get_config()
    personality = base_agent_system_prompt(config)
    fingerprint = hashlib.sha256(personality.encode("utf-8")).hexdigest()[:16]
    return AgentModeActionContext(
        stage=stage,
        personality_prompt=personality,
        personality_fingerprint=fingerprint,
        **values,
    )


agent_mode_hooks = AgentModeHookRegistry()


def register_agent_mode_hook(
    stage: str,
    name: str,
    hook: AgentModeHook,
    *,
    priority: int = 100,
) -> None:
    agent_mode_hooks.register(stage, name, hook, priority=priority)


def unregister_agent_mode_hook(stage: str, name: str) -> bool:
    return agent_mode_hooks.unregister(stage, name)


__all__ = [
    "AgentModeActionContext",
    "AgentModeHook",
    "AgentModeHookRegistry",
    "HOOK_STAGES",
    "agent_mode_hooks",
    "build_action_context",
    "register_agent_mode_hook",
    "unregister_agent_mode_hook",
]
