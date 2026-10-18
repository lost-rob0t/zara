from types import SimpleNamespace

import zara.plugins.builtin.agent_mode as agent_mode_module
import zara.plugins.builtin.agent_mode_hooks as hooks_module
from zara.plugins.builtin.agent_mode import AgentModePlugin
from zara.runtime import events
from t.test_agent_mode_core_plugin import HookConfig


def test_event_loop_never_blocks_on_speech_playback(monkeypatch):
    plugin = AgentModePlugin()
    plugin._configuration = {"speak_questions": True}
    queued = []
    monkeypatch.setattr(plugin, "_queue_speech", queued.append)
    monkeypatch.setattr(plugin, "_speak_text", lambda text: (_ for _ in ()).throw(AssertionError("event thread blocked")))
    sequence = iter([
        events.ResponseText(text="hello", conversation_id="agent-mode:question:one"),
        events.VoiceSpeechStarted(stream_id="voice"),
    ])
    class Subscription:
        def get(self, timeout=None):
            try:
                return SimpleNamespace(event=next(sequence))
            except StopIteration:
                raise RuntimeError("done")
    plugin._subscription = Subscription()
    plugin._event_loop(SimpleNamespace(is_set=lambda: False))
    assert queued == ["hello"]
    assert plugin._speech_generation == 1


def test_barge_in_during_synthesis_suppresses_late_audio(monkeypatch):
    plugin = AgentModePlugin()
    class Engine:
        def __init__(self, provider, config):
            pass
        async def synthesize_async(self, text):
            plugin._interrupt_speech("new user speech")
            return SimpleNamespace(success=True, audio=b"audio", audio_format="wav", error=None)
        async def close(self):
            pass
    config = HookConfig()
    monkeypatch.setattr(agent_mode_module, "get_config", lambda: config)
    monkeypatch.setattr(hooks_module, "get_config", lambda: config)
    monkeypatch.setattr(agent_mode_module, "TTSEngine", Engine)
    monkeypatch.setattr(agent_mode_module.shutil, "which", lambda name: "/bin/mpv")
    monkeypatch.setattr(agent_mode_module.subprocess, "Popen", lambda *a, **k: (_ for _ in ()).throw(AssertionError("stale audio played")))
    assert "interrupted" in plugin._speak_text("stale reply").lower()


def test_barge_in_flushes_bounded_pending_speech():
    plugin = AgentModePlugin()
    for index in range(100):
        plugin._queue_speech(str(index))
    assert plugin._speech_queue.qsize() <= 8
    plugin._interrupt_speech("user speech")
    assert plugin._speech_queue.empty()
