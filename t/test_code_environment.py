import json
import shutil
import subprocess
import threading
from pathlib import Path
from types import SimpleNamespace

import pytest

from zara.plugins.builtin.code_environment import CodeEnvironmentPlugin


def configured_plugin(tmp_path, monkeypatch, *, projects=None, max_concurrent=2):
    monkeypatch.setattr("shutil.which", lambda binary: "/usr/bin/pi")
    plugin = CodeEnvironmentPlugin()
    roots = projects or {"alpha": str(tmp_path / "alpha"), "beta": str(tmp_path / "beta")}
    monkeypatch.setattr(plugin, "_resolve_project", lambda alias: Path(roots[alias]))
    plugin.state_events = []

    class FakeState:
        status = "pending"
        result_text = None

        def create(self, principal_id, alias, goal):
            plugin.state_events.append(("create", principal_id, alias, goal))
            return SimpleNamespace(task_id=f"code-{alias}")

        def start(self, principal_id, task_id):
            self.status = "running"
            plugin.state_events.append(("start", principal_id, task_id))

        def finish(self, principal_id, task_id, result, failed=False):
            self.status = "failed" if failed else "completed"
            self.result_text = result
            plugin.state_events.append(("finish", principal_id, task_id, result, failed))

        def list(self, principal_id):
            return []

        def recover_interrupted(self, principal_id):
            return 0

        def cancel(self, principal_id, task_id, reason="cancelled"):
            self.status = "cancelled"

        def interrupt(self, principal_id, task_id, reason="interrupted"):
            self.status = "interrupted"

        def get(self, principal_id, task_id):
            return SimpleNamespace(status=SimpleNamespace(value=self.status), reason=None)

        def result(self, principal_id, task_id):
            return self.result_text

    states = {alias: FakeState() for alias in roots}
    plugin.states = states
    monkeypatch.setattr(plugin, "_state_for_project", lambda alias: states[alias])
    plugin.start(SimpleNamespace(configuration={
        "projects": list(roots),
        "binary": "pi",
        "max_concurrent": max_concurrent,
    }, principal_id="user:test"))
    return plugin


def test_code_tasks_run_in_parallel_and_return_each_result(tmp_path, monkeypatch):
    for name in ("alpha", "beta"):
        (tmp_path / name).mkdir()
    plugin = configured_plugin(tmp_path, monkeypatch)
    barrier = threading.Barrier(2, timeout=2)

    def worker(task, project, mode):
        barrier.wait()
        return f"{project}:{task}:{mode}"

    monkeypatch.setattr(plugin, "_run_worker", worker)
    result = json.loads(plugin.tools()[0].invoke({"tasks": [
        {"task": "first", "project": "alpha"},
        {"task": "second", "project": "beta", "mode": "review"},
    ]}))
    assert [row["task_id"].startswith("code-") for row in result["results"]] == [True, True]
    assert [row["project"] for row in result["results"]] == ["alpha", "beta"]
    assert [row["status"] for row in result["results"]] == ["started", "started"]
    assert [json.loads(plugin.tools()[2].invoke({"project": row["project"], "task_id": row["task_id"], "timeout_seconds": 5}))["result"] for row in result["results"]] == ["alpha:first:implement", "beta:second:review"]
    assert ("finish", "user:test", "code-alpha", "alpha:first:implement", False) in plugin.state_events
    assert ("finish", "user:test", "code-beta", "beta:second:review", False) in plugin.state_events
    plugin.stop()


def test_code_tasks_isolates_worker_failures(tmp_path, monkeypatch):
    for name in ("alpha", "beta"):
        (tmp_path / name).mkdir()
    plugin = configured_plugin(tmp_path, monkeypatch)

    def worker(task, project, mode):
        if project == "alpha":
            raise RuntimeError("worker failed")
        return "done"

    monkeypatch.setattr(plugin, "_run_worker", worker)
    result = json.loads(plugin.tools()[0].invoke({"tasks": [
        {"task": "first", "project": "alpha"},
        {"task": "second", "project": "beta"},
    ]}))
    waited = [json.loads(plugin.tools()[2].invoke({"project": row["project"], "task_id": row["task_id"], "timeout_seconds": 5})) for row in result["results"]]
    assert waited[0]["status"] == "failed"
    assert waited[0]["result"] == "worker failed"
    assert waited[1]["result"] == "done"
    assert ("finish", "user:test", "code-alpha", "worker failed", True) in plugin.state_events
    plugin.stop()


def test_code_tasks_rejects_same_project_concurrency(tmp_path, monkeypatch):
    (tmp_path / "alpha").mkdir()
    plugin = configured_plugin(tmp_path, monkeypatch, projects={"alpha": str(tmp_path / "alpha")})
    with pytest.raises(ValueError, match="separate configured worktrees"):
        plugin.tools()[0].invoke({"tasks": [
            {"task": "first", "project": "alpha"},
            {"task": "second", "project": "alpha"},
        ]})
    plugin.stop()


def test_code_tasks_rejects_unknown_project(tmp_path, monkeypatch):
    (tmp_path / "alpha").mkdir()
    plugin = configured_plugin(tmp_path, monkeypatch, projects={"alpha": str(tmp_path / "alpha")})
    with pytest.raises(ValueError, match="unknown code project"):
        plugin.tools()[0].invoke({"tasks": [{"task": "change", "project": "missing"}]})
    plugin.stop()


def test_review_worker_uses_read_only_tools(tmp_path, monkeypatch):
    (tmp_path / "alpha").mkdir()
    plugin = configured_plugin(tmp_path, monkeypatch, projects={"alpha": str(tmp_path / "alpha")})
    commands = []

    class Process:
        returncode = 0

        def communicate(self, *, input, timeout):
            assert "Review only" in input
            assert timeout == 900
            return "reviewed", ""

    def spawn(command, **kwargs):
        commands.append(command)
        assert kwargs["cwd"] == tmp_path / "alpha"
        assert "HOME" not in kwargs["env"]
        assert "ZARA_DISCORD_TOKEN" not in kwargs["env"]
        return Process()

    monkeypatch.setattr("subprocess.Popen", spawn)
    assert plugin._run_worker("inspect", "alpha", "review") == "reviewed"
    assert commands[0][commands[0].index("--tools") + 1] == "read,grep,find,ls"
    assert "--no-context-files" in commands[0]
    plugin.stop()


def test_code_requires_runtime_principal(tmp_path, monkeypatch):
    (tmp_path / "alpha").mkdir()
    monkeypatch.setattr("shutil.which", lambda binary: "/usr/bin/pi")
    plugin = CodeEnvironmentPlugin()
    monkeypatch.setattr(plugin, "_resolve_project", lambda alias: tmp_path / "alpha")
    with pytest.raises(ValueError, match="runtime principal"):
        plugin.start(SimpleNamespace(configuration={"projects": ["alpha"]}))


def test_sandbox_allows_repo_write_and_denies_outside_and_symlink(tmp_path, monkeypatch):
    sandbox_binary = shutil.which("bwrap")
    if sandbox_binary is None:
        pytest.skip("bubblewrap unavailable")
    root = tmp_path / "alpha"
    root.mkdir()
    (root / ".zara").mkdir()
    (root / ".zara" / "task-state.json").write_text("protected\n")
    (root / "escape").symlink_to("/etc/zara-sandbox-denied")
    plugin = configured_plugin(tmp_path, monkeypatch, projects={"alpha": str(root)})
    plugin._sandbox_binary = sandbox_binary
    command = plugin._sandbox_command("alpha", [
        "/bin/sh", "-c",
        "echo yes > inside && "
        "echo temporary > .zara/task-state.json && "
        "(echo no > /etc/zara-sandbox-denied) 2>/dev/null; "
        "test $? -ne 0 && "
        "(echo no > escape) 2>/dev/null; test $? -ne 0 && "
        "test ! -e /home/unseen/.ssh",
    ])
    result = subprocess.run(command, capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stderr
    assert (root / "inside").read_text() == "yes\n"
    assert (root / ".zara" / "task-state.json").read_text() == "protected\n"
    assert not Path("/etc/zara-sandbox-denied").exists()
    plugin.stop()


def test_second_start_cannot_overlap_same_worktree(tmp_path, monkeypatch):
    (tmp_path / "alpha").mkdir()
    plugin = configured_plugin(tmp_path, monkeypatch, projects={"alpha": str(tmp_path / "alpha")})
    entered = threading.Event()
    release = threading.Event()

    def worker(task, project, mode):
        entered.set()
        assert release.wait(3)
        return "done"

    monkeypatch.setattr(plugin, "_run_worker", worker)
    first = json.loads(plugin.tools()[0].invoke({"tasks": [{"task": "first", "project": "alpha"}]}))
    assert entered.wait(2)
    with pytest.raises(RuntimeError, match="already has a running task"):
        plugin.tools()[0].invoke({"tasks": [{"task": "second", "project": "alpha"}]})
    release.set()
    assert json.loads(plugin.tools()[2].invoke({"project": "alpha", "task_id": first["results"][0]["task_id"], "timeout_seconds": 3}))["status"] == "completed"
    plugin.stop()


def test_cancelled_task_does_not_overwrite_terminal_state(tmp_path, monkeypatch):
    (tmp_path / "alpha").mkdir()
    plugin = configured_plugin(tmp_path, monkeypatch, projects={"alpha": str(tmp_path / "alpha")})
    entered = threading.Event()
    release = threading.Event()

    def worker(task, project, mode):
        entered.set()
        assert release.wait(3)
        return "late result"

    monkeypatch.setattr(plugin, "_run_worker", worker)
    task_id = json.loads(plugin.tools()[0].invoke({"tasks": [{"task": "first", "project": "alpha"}]}))["results"][0]["task_id"]
    assert entered.wait(2)
    assert json.loads(plugin.tools()[3].invoke({"project": "alpha", "task_id": task_id}))["status"] == "cancelled"
    release.set()
    assert json.loads(plugin.tools()[2].invoke({"project": "alpha", "task_id": task_id, "timeout_seconds": 3}))["status"] == "cancelled"
    assert not any(event[0] == "finish" for event in plugin.state_events)
    plugin.stop()


def test_shutdown_interrupts_inflight_task(tmp_path, monkeypatch):
    (tmp_path / "alpha").mkdir()
    plugin = configured_plugin(tmp_path, monkeypatch, projects={"alpha": str(tmp_path / "alpha")})
    entered = threading.Event()
    release = threading.Event()

    def worker(task, project, mode):
        entered.set()
        assert release.wait(3)
        return "late result"

    monkeypatch.setattr(plugin, "_run_worker", worker)
    task_id = json.loads(plugin.tools()[0].invoke({"tasks": [{"task": "first", "project": "alpha"}]}))["results"][0]["task_id"]
    assert entered.wait(2)
    plugin.stop()
    assert plugin.states["alpha"].status == "interrupted"
    release.set()
    plugin._futures[task_id].result(timeout=3)
    assert plugin.states["alpha"].status == "interrupted"


def test_packaged_pi_accepts_worker_flags():
    binary = shutil.which("pi")
    if binary is None:
        pytest.skip("Pi unavailable outside Nix shell")
    result = subprocess.run([binary, "--help"], capture_output=True, text=True, check=True)
    for flag in ("--print", "--no-session", "--tools", "--approve", "--no-approve",
                 "--no-context-files", "--provider", "--model", "--thinking"):
        assert flag in result.stdout


def test_sandboxed_worker_runs_end_to_end(tmp_path, monkeypatch):
    sandbox_binary = shutil.which("bwrap")
    if sandbox_binary is None:
        pytest.skip("Bubblewrap unavailable")
    root = tmp_path / "alpha"
    root.mkdir()
    worker = root / "worker"
    worker.write_text("#!/bin/sh\ncat >/dev/null\nprintf 'worker done'\n")
    worker.chmod(0o755)
    plugin = configured_plugin(tmp_path, monkeypatch, projects={"alpha": str(root)})
    plugin._binary = str(worker)
    plugin._sandbox_binary = sandbox_binary
    assert plugin._run_worker("task", "alpha", "review") == "worker done"
    plugin.stop()


def test_worker_environment_only_passes_named_credential(tmp_path, monkeypatch):
    (tmp_path / "alpha").mkdir()
    plugin = configured_plugin(tmp_path, monkeypatch, projects={"alpha": str(tmp_path / "alpha")})
    plugin._credential_env_vars = ("OPENROUTER_API_KEY",)
    monkeypatch.setenv("OPENROUTER_API_KEY", "selected")
    monkeypatch.setenv("ZARA_DISCORD_TOKEN", "unrelated")
    environment = plugin._worker_environment()
    assert environment["OPENROUTER_API_KEY"] == "selected"
    assert "ZARA_DISCORD_TOKEN" not in environment
    plugin.stop()
