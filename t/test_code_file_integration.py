import json
import subprocess
from types import SimpleNamespace

from zara.plugins.builtin.code_environment import CodeEnvironmentPlugin
from zara.tasks.repository_state import RepositoryTaskState


def test_code_task_uses_prolog_repository_and_returns_persisted_result(tmp_path, monkeypatch):
    repository = tmp_path / "worktree"
    repository.mkdir()
    subprocess.run(["git", "init", "-q", str(repository)], check=True)

    class Prolog:
        def __init__(self, _path):
            pass

        def query_all(self, query):
            assert query == "kb_config:file_repository(Alias, Path)"
            return [{"Alias": "work", "Path": str(repository)}]

    monkeypatch.setattr("zara.prolog_engine.PrologEngine", Prolog)
    monkeypatch.setattr("zara.plugins.builtin.code_environment.shutil.which", lambda _: "/usr/bin/true")
    plugin = CodeEnvironmentPlugin()
    plugin.start(SimpleNamespace(configuration={"projects": ["work"]}, principal_id="user:alice"))
    monkeypatch.setattr(plugin, "_run_worker", lambda task, project, mode: f"{project}: {task}")
    try:
        tools = {tool.name: tool for tool in plugin.tools()}
        started = json.loads(tools["code_tasks"].invoke({"tasks": [{"task": "Fix parser", "project": "work"}]}))
        task_id = started["results"][0]["task_id"]
        completed = json.loads(tools["code_task_wait"].invoke({"project": "work", "task_id": task_id, "timeout_seconds": 5}))

        assert completed["status"] == "completed"
        assert completed["result"] == "work: Fix parser"
        persisted = json.loads(RepositoryTaskState(repository).path_for("user:alice").read_text())
        assert persisted["tasks"][0]["task_id"] == task_id
        assert persisted["tasks"][0]["result"] == "work: Fix parser"
    finally:
        plugin.stop()
