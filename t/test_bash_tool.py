from zara.agent.tools.builtin_tools import bash_tool, get_builtin_tools


def test_bash_tool_executes_command():
    result = bash_tool.invoke({"command": "printf 'hello-zara'"})

    assert "Exit code: 0" in result
    assert "hello-zara" in result


def test_bash_tool_reports_stderr_and_exit_code():
    result = bash_tool.invoke(
        {"command": "printf 'nope' >&2; exit 7"}
    )

    assert "Exit code: 7" in result
    assert "stderr:\nnope" in result


def test_bash_tool_honors_working_directory(tmp_path):
    result = bash_tool.invoke(
        {"command": "pwd", "cwd": str(tmp_path)}
    )

    assert "Exit code: 0" in result
    assert str(tmp_path) in result


def test_builtin_tools_include_bash():
    names = {tool.name for tool in get_builtin_tools()}

    assert "bash" in names
