import json
from types import SimpleNamespace

import pytest

from zara.plugins.builtin.pi_coder import PiCoderPlugin


def _fake_pi(tmp_path, *, body=None):
    executable = tmp_path / "pi"
    script_body = body or """
import json
import os
import sys
payload = sys.stdin.read()
print(json.dumps({"argv": sys.argv[1:], "cwd": os.getcwd(), "stdin": payload}))
"""
    executable.write_text(
        "#!/usr/bin/env python3\n" + script_body.lstrip(),
        encoding="utf-8",
    )
    executable.chmod(0o755)
    return executable


def _started_plugin(tmp_path, **overrides):
    project = tmp_path / "project"
    project.mkdir()
    configuration = {
        "binary": str(_fake_pi(tmp_path)),
        "projects": {"zara": str(project)},
        "allow_shell": False,
        "project_trust": False,
        "timeout_seconds": 5.0,
        "max_output_chars": 4000,
    }
    configuration.update(overrides)
    plugin = PiCoderPlugin()
    plugin.start(SimpleNamespace(configuration=configuration))
    return plugin, project


def _tool(plugin, name):
    return next(tool for tool in plugin.tools() if tool.name == name)


def _tool_list(argv):
    index = argv.index("--tools")
    return argv[index + 1].split(",")


def test_coder_tool_requires_canonical_zara_approval():
    coder = _tool(PiCoderPlugin(), "coder")
    assert coder.metadata == {"zara_requires_approval": True}


def test_coder_runs_pi_in_configured_project_and_uses_stdin(tmp_path):
    plugin, project = _started_plugin(tmp_path)

    result = _tool(plugin, "coder").invoke(
        {"task": "Implement the thing", "project": "zara", "mode": "implement"}
    )
    payload = json.loads(result)

    assert payload["cwd"] == str(project.resolve())
    assert "Implement the thing" in payload["stdin"]
    assert "--print" in payload["argv"]
    assert "--no-session" in payload["argv"]
    assert "--no-approve" in payload["argv"]
    assert "edit" in _tool_list(payload["argv"])
    assert "write" in _tool_list(payload["argv"])
    assert "bash" not in _tool_list(payload["argv"])


def test_single_project_is_implicit_default(tmp_path):
    plugin, _ = _started_plugin(tmp_path)

    result = _tool(plugin, "coder").invoke(
        {"task": "Plan the change", "mode": "plan"}
    )
    payload = json.loads(result)

    assert "configured project 'zara'" in payload["stdin"]


def test_review_is_read_only_even_when_shell_is_enabled(tmp_path):
    plugin, _ = _started_plugin(tmp_path, allow_shell=True)

    result = _tool(plugin, "coder").invoke(
        {"task": "Review this project", "project": "zara", "mode": "review"}
    )
    payload = json.loads(result)
    tools = _tool_list(payload["argv"])

    assert tools == ["read", "grep", "find", "ls"]
    assert "Do not modify files" in payload["stdin"]


def test_implement_shell_requires_operator_configuration(tmp_path):
    plugin, _ = _started_plugin(tmp_path, allow_shell=True)

    result = _tool(plugin, "coder").invoke(
        {"task": "Run the tests and fix them", "project": "zara", "mode": "implement"}
    )
    payload = json.loads(result)

    assert "bash" in _tool_list(payload["argv"])


def test_project_trust_is_explicit(tmp_path):
    plugin, _ = _started_plugin(tmp_path, project_trust=True)

    result = _tool(plugin, "coder").invoke(
        {"task": "Inspect project skills", "project": "zara", "mode": "review"}
    )
    payload = json.loads(result)

    assert "--approve" in payload["argv"]
    assert "--no-approve" not in payload["argv"]


def test_unknown_project_fails_without_accepting_a_path(tmp_path):
    plugin, project = _started_plugin(tmp_path)

    with pytest.raises(ValueError, match="unknown coder project"):
        _tool(plugin, "coder").invoke(
            {"task": "Do work", "project": str(project), "mode": "implement"}
        )


def test_project_listing_does_not_expose_host_paths(tmp_path):
    plugin, project = _started_plugin(tmp_path)

    result = _tool(plugin, "coder_projects").invoke({})

    assert "zara (default)" in result
    assert str(project) not in result


def test_start_rejects_missing_project_registry(tmp_path):
    plugin = PiCoderPlugin()
    runtime = SimpleNamespace(
        configuration={
            "binary": str(_fake_pi(tmp_path)),
            "projects": {},
        }
    )

    with pytest.raises(ValueError, match="at least one configured project"):
        plugin.start(runtime)


def test_pi_failure_is_bounded_and_typed(tmp_path):
    project = tmp_path / "project"
    project.mkdir()
    executable = _fake_pi(
        tmp_path,
        body="""
import sys
sys.stderr.write("simulated pi failure")
sys.exit(7)
""",
    )
    plugin = PiCoderPlugin()
    plugin.start(
        SimpleNamespace(
            configuration={
                "binary": str(executable),
                "projects": {"zara": str(project)},
            }
        )
    )

    with pytest.raises(RuntimeError, match="exit code 7: simulated pi failure"):
        _tool(plugin, "coder").invoke(
            {"task": "Do work", "project": "zara", "mode": "implement"}
        )
