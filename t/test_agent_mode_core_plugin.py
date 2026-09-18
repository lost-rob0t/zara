from __future__ import annotations

import queue
import stat
import subprocess
import time
from pathlib import Path
from types import SimpleNamespace

import zara.plugins.builtin.agent_mode as agent_mode_module
import zara.plugins.builtin.agent_mode_hooks as hooks_module
from zara.agent import AgentManager
from zara.plugins import PluginState
from zara.plugins.builtin.agent_mode import AgentModePlugin, AgentModeStore
from zara.plugins.builtin.agent_mode_hooks import agent_mode_hooks
from zara.runtime import events
from zara.runtime.backend import RuntimeBackend, RuntimeTurnResult
from zara.runtime.host import RuntimeHost, RuntimeHostState
from zara.speech_activity import speech_activity


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


class HookConfig:
    def __init__(self, personality="You are test Zara."):
        self.personality = personality

    def get_agent_system_prompt(self):
        return self.personality

    def get_section(self, name):
        if name == "tts":
            return {"provider": "qwen3", "voice": "zara"}
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
        latency_trace=None,
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
        personality_fingerprint="deadbeef",
        now=1000,
    )

    due = store.claim_due(now=1000)

    assert [item.task_id for item in due] == [task.task_id]
    assert store.claim_due(now=1001) == []
    persisted = AgentModeStore(path).list_tasks()[0]
    assert persisted.next_run_at == 1060
    assert persisted.created_personality_fingerprint == "deadbeef"
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


def test_agent_mode_registers_tools_when_enabled_and_runs_scheduled_turn(
    tmp_path,
    monkeypatch,
):
    monkeypatch.setattr(hooks_module, "get_config", lambda: HookConfig())
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
        assert "personality context" in result

        deadline = time.monotonic() + 2
        while not backend.turns and time.monotonic() < deadline:
            time.sleep(0.02)

        assert backend.turns
        assert backend.turns[0][0] == "check the heartbeat"
        assert backend.turns[0][1].startswith("agent-mode:task:")
    finally:
        stop_host(host)

    assert backend.tools == []


def test_task_creation_hook_sees_personality_and_can_rewrite_prompt(tmp_path, monkeypatch):
    personality = "You are Mara-ish test personality; never persist this raw prompt."
    monkeypatch.setattr(hooks_module, "get_config", lambda: HookConfig(personality))
    agent_mode_hooks.clear()
    seen = {}

    def rewrite(context):
        seen["personality"] = context.personality_prompt
        seen["fingerprint"] = context.personality_fingerprint
        return context.with_updates(prompt=f"[persona-aware] {context.prompt}")

    agent_mode_hooks.register("before_task_create", "test-rewrite", rewrite)
    try:
        plugin = AgentModePlugin()
        plugin._store = AgentModeStore(tmp_path / "state.json")
        schedule = next(tool for tool in plugin.tools() if tool.name == "schedule_recurring_task")
        schedule.invoke(
            {
                "name": "status",
                "prompt": "check status",
                "every_minutes": 5,
            }
        )
        task = plugin._store.list_tasks()[0]
        raw_state = (tmp_path / "state.json").read_text()

        assert seen["personality"] == personality
        assert len(seen["fingerprint"]) == 16
        assert task.prompt == "[persona-aware] check status"
        assert task.created_personality_fingerprint == seen["fingerprint"]
        assert personality not in raw_state
    finally:
        agent_mode_hooks.clear()


def test_default_prompt_routes_agent_mode_tools_before_prolog():
    manager = AgentManager.__new__(AgentManager)
    manager.config = SimpleNamespace(get_agent_system_prompt=lambda: None)

    prompt = manager._build_system_prompt()

    assert "Agent-mode service actions" in prompt
    assert "schedule_recurring_task" in prompt
    assert "before considering the legacy command router" in prompt
    assert "`speak`" in prompt


def test_speak_tool_uses_output_only_tts_and_marks_playback(monkeypatch):
    calls = {}

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

    class FakePopen:
        def __init__(self, command, **kwargs):
            calls["command"] = command
            calls["popen_kwargs"] = kwargs
            assert Path(command[-1]).is_file()
            calls["playback_active_during_spawn"] = speech_activity.active
            self.returncode = None

        def communicate(self, timeout=None):
            calls["playback_active_during_play"] = speech_activity.active
            self.returncode = 0
            return "", ""

        def poll(self):
            return self.returncode

        def terminate(self):
            self.returncode = -15

        def kill(self):
            self.returncode = -9

    config = HookConfig()
    monkeypatch.setattr(agent_mode_module, "get_config", lambda: config)
    monkeypatch.setattr(hooks_module, "get_config", lambda: config)
    monkeypatch.setattr(agent_mode_module, "TTSEngine", FakeEngine)
    monkeypatch.setattr(agent_mode_module.shutil, "which", lambda name: "/nix/store/mpv/bin/mpv")
    monkeypatch.setattr(agent_mode_module.subprocess, "Popen", FakePopen)
    speech_activity.clear()

    plugin = AgentModePlugin()
    result = plugin._speak_text("hello from Zara")

    assert result == "Spoken."
    assert calls["provider"] == "qwen3"
    assert calls["text"] == "hello from Zara"
    assert calls["closed"] is True
    assert calls["command"][0] == "mpv"
    assert calls["playback_active_during_play"] is True
    assert speech_activity.active is False
    assert "sounddevice" not in agent_mode_module.__dict__


def test_voice_speech_started_barges_in_on_active_tool_tts():
    class FakeProcess:
        returncode = None

        def __init__(self):
            self.terminated = False

        def poll(self):
            return self.returncode

        def terminate(self):
            self.terminated = True
            self.returncode = -15

    class OneShotSubscription:
        closed = False

        def __init__(self):
            self.sent = False

        def get(self, timeout=None):
            if not self.sent:
                self.sent = True
                return SimpleNamespace(
                    event=events.VoiceSpeechStarted(stream_id="voice-1")
                )
            raise RuntimeError("done")

        def close(self):
            self.closed = True

    plugin = AgentModePlugin()
    plugin._configuration = {"barge_in": True}
    process = FakeProcess()
    plugin._speech_process = process
    plugin._subscription = OneShotSubscription()

    plugin._event_loop(SimpleNamespace(is_set=lambda: False))

    assert process.terminated is True


def test_agent_mode_state_path_honors_xdg_state_home(tmp_path, monkeypatch):
    monkeypatch.setenv("XDG_STATE_HOME", str(tmp_path))

    assert AgentModePlugin._state_path({}) == (
        Path(tmp_path) / "zarathushtra" / "agent-mode" / "state.json"
    )
