from __future__ import annotations

import json
import os
import signal
from pathlib import Path
from types import SimpleNamespace

import pytest

import zara.plugins.builtin.video_studio as video_module
from zara.plugins.builtin.video_studio import VideoStudioPlugin


class Runtime:
    def __init__(self, configuration):
        self.configuration = configuration


def project_root(tmp_path: Path) -> Path:
    root = tmp_path / "social"
    project = root / "youtube" / "video-source" / "developer-makes-skynet"
    project.mkdir(parents=True)
    (project / "video.json").write_text(
        json.dumps(
            {
                "version": 1,
                "slug": "developer-makes-skynet",
                "title": "Developer Makes Skynet",
                "source": {
                    "recording": "youtube/recordings/developer-makes-skynet/master.mkv",
                    "has_audio": True,
                },
                "variants": [
                    {"name": "youtube", "width": 1920, "height": 1080}
                ],
                "youtube": {"title": "Developer Makes Skynet"},
            }
        ),
        encoding="utf-8",
    )
    return root


def tool(plugin: VideoStudioPlugin, name: str):
    return next(item for item in plugin.tools() if item.name == name)


def test_video_studio_is_disabled_by_default_and_declares_approval_metadata():
    plugin = VideoStudioPlugin()
    tools = {item.name: item for item in plugin.tools()}

    assert plugin.enabled_by_default is False
    assert plugin.metadata.name == "video-studio"
    assert set(tools) == {
        "video_plan",
        "video_record_start",
        "video_record_stop",
        "video_record_status",
        "video_render",
        "video_generate",
        "video_package",
    }
    assert tools["video_plan"].metadata.get("zara_requires_approval") is not True
    assert tools["video_record_status"].metadata.get("zara_requires_approval") is not True
    for name in {
        "video_record_start",
        "video_record_stop",
        "video_render",
        "video_generate",
        "video_package",
    }:
        assert tools[name].metadata["zara_requires_approval"] is True


def test_video_project_slug_is_bounded_to_checked_in_project(tmp_path, monkeypatch):
    root = project_root(tmp_path)
    plugin = VideoStudioPlugin()
    plugin.start(Runtime({"social_root": str(root), "use_nix": False}))

    with pytest.raises(ValueError, match="slug"):
        tool(plugin, "video_plan").invoke({"slug": "../escape"})


def test_video_plan_uses_fixed_cli_argv_and_private_pythonpath(tmp_path, monkeypatch):
    root = project_root(tmp_path)
    plugin = VideoStudioPlugin()
    plugin.start(Runtime({"social_root": str(root), "use_nix": False}))
    calls = {}

    def fake_run(command, **kwargs):
        calls["command"] = command
        calls["kwargs"] = kwargs
        return SimpleNamespace(returncode=0, stdout='{"record":[]}', stderr="")

    monkeypatch.setattr(video_module.subprocess, "run", fake_run)

    result = tool(plugin, "video_plan").invoke({"slug": "developer-makes-skynet"})

    assert result == '{"record":[]}'
    assert calls["command"] == [
        "python3",
        "-m",
        "starintel_video_studio.cli",
        "--root",
        str(root.resolve()),
        "--project",
        str(
            (
                root
                / "youtube"
                / "video-source"
                / "developer-makes-skynet"
                / "video.json"
            ).resolve()
        ),
        "plan",
    ]
    assert calls["kwargs"]["cwd"] == root.resolve()
    python_path = calls["kwargs"]["env"]["PYTHONPATH"].split(os.pathsep)
    assert python_path[0] == str((root / "video_studio" / "src").resolve())


def test_video_record_start_stop_owns_one_process_group(tmp_path, monkeypatch):
    root = project_root(tmp_path)
    plugin = VideoStudioPlugin()
    plugin.start(Runtime({"social_root": str(root), "use_nix": False}))
    calls = {"signals": []}

    class FakeProcess:
        pid = 4242

        def __init__(self):
            self.returncode = None
            self.waits = []

        def poll(self):
            return self.returncode

        def wait(self, timeout=None):
            self.waits.append(timeout)
            self.returncode = 0
            return 0

    process = FakeProcess()

    def fake_popen(command, **kwargs):
        calls["command"] = command
        calls["kwargs"] = kwargs
        return process

    monkeypatch.setattr(video_module.subprocess, "Popen", fake_popen)
    monkeypatch.setattr(
        video_module.os,
        "killpg",
        lambda pid, sig: calls["signals"].append((pid, sig)),
    )

    started = tool(plugin, "video_record_start").invoke(
        {
            "slug": "developer-makes-skynet",
            "geometry": "0,0 1280x720",
            "audio": True,
        }
    )
    status = tool(plugin, "video_record_status").invoke({})
    stopped = tool(plugin, "video_record_stop").invoke({})

    assert "started" in started.lower()
    assert "running" in status.lower()
    assert calls["kwargs"]["start_new_session"] is True
    assert calls["command"][-3:] == ["record", "--geometry", "0,0 1280x720"]
    assert calls["signals"] == [(4242, signal.SIGINT)]
    assert "stopped" in stopped.lower()
    assert plugin._record_process is None


def test_video_record_rejects_second_active_capture(tmp_path, monkeypatch):
    root = project_root(tmp_path)
    plugin = VideoStudioPlugin()
    plugin.start(Runtime({"social_root": str(root), "use_nix": False}))

    class FakeProcess:
        pid = 99
        returncode = None

        def poll(self):
            return None

    monkeypatch.setattr(
        video_module.subprocess,
        "Popen",
        lambda *_args, **_kwargs: FakeProcess(),
    )

    record = tool(plugin, "video_record_start")
    record.invoke({"slug": "developer-makes-skynet"})

    with pytest.raises(RuntimeError, match="already running"):
        record.invoke({"slug": "developer-makes-skynet"})


def test_plugin_stop_terminates_active_capture(tmp_path, monkeypatch):
    root = project_root(tmp_path)
    plugin = VideoStudioPlugin()
    plugin.start(Runtime({"social_root": str(root), "use_nix": False}))
    calls = []

    class FakeProcess:
        pid = 101
        returncode = None

        def poll(self):
            return self.returncode

        def wait(self, timeout=None):
            self.returncode = 0
            return 0

    plugin._record_process = FakeProcess()
    monkeypatch.setattr(video_module.os, "killpg", lambda pid, sig: calls.append((pid, sig)))

    plugin.stop()

    assert calls == [(101, signal.SIGINT)]
    assert plugin._record_process is None


def test_render_generate_and_package_use_bounded_subcommands(tmp_path, monkeypatch):
    root = project_root(tmp_path)
    plugin = VideoStudioPlugin()
    plugin.start(Runtime({"social_root": str(root), "use_nix": False}))
    commands = []

    def fake_run(command, **kwargs):
        commands.append(command)
        return SimpleNamespace(returncode=0, stdout="ok\n", stderr="")

    monkeypatch.setattr(video_module.subprocess, "run", fake_run)

    assert tool(plugin, "video_render").invoke(
        {"slug": "developer-makes-skynet", "variant": "shorts-vertical"}
    ) == "ok"
    assert tool(plugin, "video_generate").invoke(
        {"slug": "developer-makes-skynet", "shot": "boot-stinger"}
    ) == "ok"
    assert tool(plugin, "video_package").invoke(
        {"slug": "developer-makes-skynet"}
    ) == "ok"

    assert commands[0][-3:] == ["render", "--variant", "shorts-vertical"]
    assert commands[1][-3:] == ["generate", "--shot", "boot-stinger"]
    assert commands[2][-1:] == ["package"]


def test_nonzero_video_command_is_bounded_and_actionable(tmp_path, monkeypatch):
    root = project_root(tmp_path)
    plugin = VideoStudioPlugin()
    plugin.start(Runtime({"social_root": str(root), "use_nix": False}))

    monkeypatch.setattr(
        video_module.subprocess,
        "run",
        lambda *_args, **_kwargs: SimpleNamespace(
            returncode=2,
            stdout="",
            stderr="x" * 5000,
        ),
    )

    with pytest.raises(RuntimeError) as error:
        tool(plugin, "video_render").invoke({"slug": "developer-makes-skynet"})

    rendered = str(error.value)
    assert "video command failed" in rendered
    assert len(rendered) < 1500
