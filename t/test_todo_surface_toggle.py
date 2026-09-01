from types import SimpleNamespace
from unittest.mock import patch

import pytest

from zara.agent.tools.registry import ToolRegistry
from zara.python_skills import TODO_SKILLS, PythonSkillRegistry, python_skills
from zara.prolog_engine import PrologEngine


class FakeConfig:
    def __init__(self, *, todos=True):
        self.todos = todos

    def get_section(self, name):
        if name == "tool_approval":
            return {}
        if name == "todo":
            return {"enabled": self.todos}
        return {}

    def get_tool_config(self):
        return {}


class FakeProlog:
    def __init__(self):
        self.goals = []

    def query_once(self, goal):
        self.goals.append(goal)
        return {}


def fake_builtin_tools(*_args, **_kwargs):
    return [
        SimpleNamespace(name="calculator"),
        SimpleNamespace(name="add_todo"),
        SimpleNamespace(name="list_todos"),
    ]


def test_python_todo_skills_can_be_disabled_as_one_surface():
    registry = PythonSkillRegistry()
    registry.set_todo_enabled(False)
    assert TODO_SKILLS.isdisjoint(registry.list_skills())
    assert "say_hello" in registry.list_skills()
    with pytest.raises(NotImplementedError, match="disabled"):
        registry.execute("capture_todo", ["ship it"])


def test_tool_registry_disables_todo_tools_and_prolog_intents():
    prolog = FakeProlog()
    registry = ToolRegistry(prolog_engine=prolog, config=FakeConfig(todos=False))
    try:
        with patch(
            "zara.agent.tools.builtin_tools.get_builtin_tools",
            side_effect=fake_builtin_tools,
        ):
            registry.load_builtin_tools()

        assert registry.list_tools() == ["calculator"]
        assert prolog.goals == ["kb_intents:set_todo_intents_enabled(false)"]
        assert TODO_SKILLS.isdisjoint(python_skills.list_skills())

        registry.register_tool(SimpleNamespace(name="add_todo"))
        assert registry.get_tool("add_todo") is not None
    finally:
        python_skills.set_todo_enabled(True)


def test_tool_registry_keeps_default_todo_behavior_enabled():
    prolog = FakeProlog()
    registry = ToolRegistry(prolog_engine=prolog, config=FakeConfig(todos=True))
    try:
        with patch(
            "zara.agent.tools.builtin_tools.get_builtin_tools",
            side_effect=fake_builtin_tools,
        ):
            registry.load_builtin_tools()
        assert set(registry.list_tools()) == {"calculator", "add_todo", "list_todos"}
        assert prolog.goals == ["kb_intents:set_todo_intents_enabled(true)"]
        assert TODO_SKILLS.issubset(set(python_skills.list_skills()))
    finally:
        python_skills.set_todo_enabled(True)


def test_invalid_todo_enabled_setting_is_rejected():
    registry = ToolRegistry(config=FakeConfig(todos="false"))
    with patch(
        "zara.agent.tools.builtin_tools.get_builtin_tools",
        side_effect=fake_builtin_tools,
    ), pytest.raises(ValueError, match="todo.enabled"):
        registry.load_builtin_tools()


def test_prolog_resolver_hides_todo_intents_but_not_timers():
    from pathlib import Path

    engine = PrologEngine()
    engine.consult(Path("main.pl"))
    try:
        assert engine.query_once(
            "kb_intents:verb_intent(todo, python(capture_todo), rest)"
        ) is not None
        engine.query_once("kb_intents:set_todo_intents_enabled(false)")
        assert engine.query_once(
            "kb_intents:verb_intent(todo, python(capture_todo), rest)"
        ) is None
        assert engine.query_once(
            'intent_resolver:resolve("search todos bug", python(search_todos), _)'
        ) is None
        assert engine.query_once(
            'intent_resolver:resolve("timer 5 minutes", timer, _)'
        ) is not None
    finally:
        engine.query_once("kb_intents:set_todo_intents_enabled(true)")
