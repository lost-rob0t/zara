"""Mode-gated file tools through the registry and the approval gate."""

import asyncio

import pytest

from langchain_core.messages import AIMessage

from zara.agent.tools.file_access import FileAccessPolicy
from zara.agent.tools.registry import ToolRegistry
from zara.agent.tools.file_tools import FILE_WRITE_TOOL_NAMES


class _Config:
    def __init__(self):
        self._sections = {
            "tools": {"file_tools": True},
            "file_tools": {
                "readable_roots": None,
                "writable_roots": None,
                "max_bytes": 20000,
                "mode": "sandbox",
                "guest_ceiling": "readonly",
            },
            "tool_approval": {
                "required_tools": [],
                "timeout_seconds": 5.0,
                "max_pending": 4,
            },
        }

    def get_section(self, name):
        return self._sections.get(name, {})

    def get_tool_config(self):
        return dict(self._sections["tools"])


def _registry(policy: FileAccessPolicy) -> ToolRegistry:
    registry = ToolRegistry(config=None)
    registry.attach_file_access_policy(policy)
    return registry


def test_ask_mode_requires_approval_for_writes_per_conversation():
    policy = FileAccessPolicy(default_mode="ask")
    registry = _registry(policy)

    assert registry.requires_approval("write_file", conversation_id="c1")
    assert registry.requires_approval("edit_file", conversation_id="c1")
    assert not registry.requires_approval("read_file", conversation_id="c1")
    assert not registry.requires_approval("search_text", conversation_id="c1")


def test_full_and_readonly_never_require_approval():
    policy = FileAccessPolicy(default_mode="sandbox")
    policy.set_mode("c-full", "full", expected_revision=0, actor="test")
    policy.set_mode("c-readonly", "readonly", expected_revision=0, actor="test")
    registry = _registry(policy)

    assert not registry.requires_approval("write_file", conversation_id="c-full")
    assert not registry.requires_approval("edit_file", conversation_id="c-full")
    assert not registry.requires_approval("write_file", conversation_id="c-readonly")
    assert not registry.requires_approval("edit_file", conversation_id="c-readonly")


def test_static_approval_metadata_is_never_weakened_by_file_policy():
    from langchain_core.tools import StructuredTool
    from pydantic import BaseModel

    class _Args(BaseModel):
        x: int = 1

    tool = StructuredTool.from_function(
        func=lambda x=1: "ok",
        name="plugin_tool",
        description="d",
        args_schema=_Args,
        metadata={"zara_requires_approval": True},
    )
    policy = FileAccessPolicy(default_mode="full")
    registry = _registry(policy)
    registry.register_tool(tool)

    assert registry.requires_approval("plugin_tool", conversation_id="c1")


def test_composed_invocation_stays_fail_closed_without_conversation_scope():
    policy = FileAccessPolicy(default_mode="ask")
    registry = _registry(policy)

    with pytest.raises(PermissionError):
        registry.invoke_composed_tool("write_file", {"path": "x.txt", "content": "y"})


def test_graph_approval_gate_is_conversation_aware(monkeypatch):
    import zara.agent.graph as graph

    policy = FileAccessPolicy(default_mode="sandbox")
    policy.set_mode("c-ask", "ask", expected_revision=0, actor="test")
    registry = _registry(policy)
    node = graph.create_approval_node(registry)

    tool_call = {"name": "write_file", "args": {"path": "a.txt"}, "id": "run-1"}
    state = {
        "messages": [AIMessage(content="", tool_calls=[tool_call])],
        "conversation_id": "c-ask",
    }

    captured = {}
    def fake_interrupt(payload):
        captured.update(payload)
        return {"decision": "approve"}

    monkeypatch.setattr(graph, "interrupt", fake_interrupt)
    decisions = node(state)["tool_decisions"]
    assert captured["tool_name"] == "write_file"
    assert decisions["run-1"] == {"decision": "approve"}

    full_state = dict(state)
    full_state["conversation_id"] = "c-sandbox-other"
    assert node(full_state)["tool_decisions"] == {}


def test_agent_turn_binds_conversation_scope_for_tools(monkeypatch):
    from zara.agent import AgentManager
    from zara.agent.tools.file_access import current_file_access_conversation

    class _TurnConfig(_Config):
        def get_llm_config(self):
            return {}

        def get_file_tool_config(self, repo_root):
            return {"mode": "sandbox"}

        def get_file_access_config(self):
            return {"default_mode": "sandbox", "guest_ceiling": "readonly"}

        def get_agent_system_prompt(self):
            return "test"

        def get_module_search_paths(self):
            return []

    class _Memory:
        def retrieve(self, _query, k=5):
            return []

    class _LLM:
        def bind_tools(self, _tools):
            return self

        async def ainvoke(self, messages):
            return AIMessage(content="done")

    seen = {}

    async def fake_backend_callback(llm, registry, state, **_kwargs):
        seen["conversation"] = current_file_access_conversation()
        return {"messages": [], "response": "done", "tool_results": []}

    monkeypatch.setattr(AgentManager, "_create_llm_client", lambda _self, _config: _LLM())
    monkeypatch.setattr("zara.agent.run_conversation_loop", fake_backend_callback)
    manager = AgentManager(config=_TurnConfig(), memory_manager=_Memory())
    result = asyncio.run(
        manager.process_async("hello", conversation_id="conv-turn-bind")
    )

    assert result["response"] == "done"
    assert seen["conversation"] == "conv-turn-bind"


def test_write_tool_names_constant_lists_both_writers():
    assert FILE_WRITE_TOOL_NAMES == ("write_file", "edit_file")
