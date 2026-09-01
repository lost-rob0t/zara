from __future__ import annotations

import os
import stat
import time
from pathlib import Path

from zara.plugins import PluginState
from zara.plugins.builtin.agent_mode import AgentModeStore
from zara.runtime.backend import RuntimeBackend, RuntimeTurnResult
from zara.runtime.host import RuntimeHost, RuntimeHostState


class AgentModeTestConfig:
    def __init__(self, values=None):
        self.values = values or {}

    def get_plugin_runtime_config(self):
        return {
            "lifecycle_timeout": 1.0,
            "event_queue_size": 32,
            "max_managed_workers": 4,
        }

    def get_plugin_config(self, name):
        if name == "agent-mode":
            return dict(self.values)
        return {}


class RecordingBackend(RuntimeBackend):
    def __init__(self):
        self.tools = []
        self.turns = []

    async def submit_turn(
        self,
        text,
        *,
        turn_id,
        conversation_id=None,
        context_ids=(),
    ):
        self.turns.append((text, conversation_id))
        return RuntimeTurnResult(response="background result")

    def register_tools(self, tools):
        self.tools.extend(tools)

    def unregister_tools(self, names):
        names = set(names)
        self.tools = [tool for tool in self.tools if tool.name not in names]


def stop_host(host):
    if host.state not in {RuntimeHostState.NEW, RuntimeHostState.STOPPED}:
        host.shutdown("test cleanup").result(timeout=5)
    host.join(timeout=5)


def test_agent_mode_store_claims_due_tasks_and_persists_private_state(tmp_path):
    path = tmp_path / "agent-mode" / "state.json"
    store = AgentModeStore(path, questions_enabled=True)
    task = store.add_task(
        name="check project",
        prompt="inspect project status",
        interval_seconds=60,
        start_delay_seconds=0,
        now=1000,
    )

    due = store.claim_due(now=1000)

    assert [item.task_id for item in due] == [task.task_id]
    assert store.claim_due(now=1001) == []
    assert AgentModeStore(path).list_tasks()[0].next_run_at == 1060
    assert stat.S_IMODE(path.stat().st_mode) == 0o600
    assert stat.S_IMODE(path.parent.stat().st_mode) == 0o700


def test_agent_mode_is_not_loaded_without_explicit_enable(tmp_path):
    backend = RecordingBackend()
    host = RuntimeHost(
        lambda: backend,
        plugin_paths=(),
        config=AgentModeTestConfig(),
    )
    try:
        host.start().result(timeout=5)
        assert backend.tools == []
        assert host.plugin_diagnostics() == ()
    finally:
        stop_host(host)


def test_agent_mode_registers_tools_when_enabled_and_runs_scheduled_turn(tmp_path):
    backend = RecordingBackend()
    state_path = tmp_path / "state.json"
    host = RuntimeHost(
        lambda: backend,
        plugin_paths=(),
        config=AgentModeTestConfig(
            {
                "enabled": True,
                "random_questions": False,
                "poll_seconds": 0.05,
                "state_path": str(state_path),
            }
        ),
    )

    try:
        host.start().result(timeout=5)
        tool_names = {tool.name for tool in backend.tools}
        assert tool_names == {
            "schedule_recurring_task",
            "list_recurring_tasks",
            "cancel_recurring_task",
            "speak",
            "set_random_questions",
            "agent_mode_status",
        }
        diagnostic = host.plugin_diagnostics()[0]
        assert diagnostic.name == "agent-mode"
        assert diagnostic.state is PluginState.RUNNING

        schedule = next(tool for tool in backend.tools if tool.name == "schedule_recurring_task")
        result = schedule.invoke(
            {
                "name": "heartbeat",
                "prompt": "check the heartbeat",
                "every_minutes": 1,
                "start_delay_minutes": 0,
            }
        )
        assert "Scheduled recurring task" in result

        deadline = time.monotonic() + 2
        while not backend.turns and time.monotonic() < deadline:
            time.sleep(0.02)

        assert backend.turns
        assert backend.turns[0][0] == "check the heartbeat"
        assert backend.turns[0][1].startswith("agent-mode:task:")
    finally:
        stop_host(host)

    assert backend.tools == []


def test_agent_mode_state_path_honors_xdg_state_home(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))
    from zara.plugins.builtin.agent_mode import AgentModePlugin

    assert AgentModePlugin._state_path({}) == (
        Path(tmp_path) / "zarathushtra" / "agent-mode" / "state.json"
    )
