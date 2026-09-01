from __future__ import annotations

import stat
import subprocess
import time
from pathlib import Path
from types import SimpleNamespace

import zara.plugins.builtin.agent_mode as agent_mode_module
from zara.agent import AgentManager
from zara.plugins import PluginState
from zara.plugins.builtin.agent_mode import AgentModePlugin, AgentModeStore
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


def test_default_prompt_routes_agent_mode_tools_before_prolog():
    manager = AgentManager.__new__(AgentManager)
    manager.config = SimpleNamespace(get_agent_system_prompt=lambda: None)

    prompt = manager._build_system_prompt()

    assert "Explicit service-tool capabilities" in prompt
    assert "schedule_recurring_task" in prompt
    assert "use that tool directly instead of `query_prolog`" in prompt
    assert "`speak`" in prompt
    assert "Do not first send them through Prolog" in prompt


def test_speak_tool_uses_output_only_tts_and_mpv(tmp_path, monkeypatch):
    calls = {}

    class FakeConfig:
        def get_section(self, name):
            assert name == "tts"
            return {"provider": "qwen3", "voice": "zara"}

    class FakeEngine:
        def __init__(self, provider, config):
            calls["provider"] = provider
            calls["config"] = config

        async def synthesize_async(self, text):
            calls["text"] = text
            return SimpleNamespace(
                success=True,
                audio=b"RIFF\x00\x00\x00\x00WAVEaudio",
                audio_format="wav",
                error=None,
            )

        async def close(self):
            calls["closed"] = True

    def fake_run(command, **kwargs):
        calls["command"] = command
        calls["run_kwargs"] = kwargs
        assert Path(command[-1]).is_file()
        return subprocess.CompletedProcess(command, 0, "", "")

    monkeypatch.setattr(agent_mode_module, "get_config", lambda: FakeConfig())
    monkeypatch.setattr(agent_mode_module, "TTSEngine", FakeEngine)
    monkeypatch.setattr(agent_mode_module.shutil, "which", lambda name: "/nix/store/mpv/bin/mpv")
    monkeypatch.setattr(agent_mode_module.subprocess, "run", fake_run)

    plugin = AgentModePlugin()
    result = plugin._speak_text("hello from Zara")

    assert result == "Spoken."
    assert calls["provider"] == "qwen3"
    assert calls["text"] == "hello from Zara"
    assert calls["closed"] is True
    assert calls["command"][0] == "mpv"
    assert "sounddevice" not in agent_mode_module.__dict__


def test_agent_mode_state_path_honors_xdg_state_home(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))

    assert AgentModePlugin._state_path({}) == (
        Path(tmp_path) / "zarathushtra" / "agent-mode" / "state.json"
    )
